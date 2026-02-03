# Trace Writer Onboarding Guide

This document provides comprehensive guidance for engineers maintaining the `trace_writer.ml` module, the heart of magic-trace's event processing pipeline.

## Table of Contents

1. [Overview](#1-overview)
2. [Core Algorithm Deep Dive](#2-core-algorithm-deep-dive)
3. [Language-Specific Hacks](#3-language-specific-hacks)
4. [Debugging Guide](#4-debugging-guide)
5. [Extension Guide](#5-extension-guide)
6. [Quick Reference](#6-quick-reference)

---

## 1. Overview

### What trace_writer Does

The trace writer transforms a stream of Intel Processor Trace (PT) events decoded by perf into a Perfetto-compatible trace file (`.fxt.gz`). This transformation involves:

1. **Reconstructing call stacks** from low-level control flow events (calls, returns, jumps)
2. **Distributing timestamps** across events that share the same timestamp
3. **Handling language-specific control flow** (Go goroutines, OCaml exceptions)
4. **Producing duration spans** for the Perfetto visualization UI

### The Core Transformation Pipeline

```
                                                    Perfetto UI
                                                        ^
                                                        |
perf record → perf script → Event.t → trace_writer → .fxt.gz
     |             |            |           |
  Intel PT    Raw text    Structured    Call stacks
  hardware    output      events        + durations
```

### Key Invariant

> **After every operation, the symbol at the top of the callstack must match the symbol at the current program counter (PC).**

This invariant is enforced by `check_current_symbol()` (line 686). When violated, the trace writer synthesizes appropriate call/return events to restore consistency.

### Entry Points

- **`write_event`** (line 985): Main entry point, handles transaction queuing
- **`write_event'`** (line 1024): Actual event processing after transaction handling
- **`end_of_trace`** (line 887): Finalizes all thread stacks at trace end

---

## 2. Core Algorithm Deep Dive

### Data Structures

#### `Mapped_time` (lines 10-33)

Perfetto uses floats for timestamps, which lose precision with large absolute values. `Mapped_time` subtracts a base time (first event) from all timestamps to work with smaller, more precise relative values.

```ocaml
module Mapped_time : sig
  type t = private Time_ns.Span.t
  val start_of_trace : t                                    (* = 0 *)
  val create : Time_ns.Span.t -> base_time:Time_ns.Span.t -> t
  val add : t -> Time_ns.Span.t -> t
  val diff : t -> t -> Time_ns.Span.t
end
```

#### `Pending_event` (lines 35-60)

Represents an event waiting to be written. Events are batched because multiple events can share the same timestamp.

```ocaml
module Pending_event = struct
  module Kind = struct
    type t =
      | Call of { addr : Int64.Hex.t; offset : Int.Hex.t; from_untraced : bool }
      | Ret
      | Ret_from_untraced of { reset_time : Mapped_time.t }
  end

  type t = { symbol : Symbol.t; kind : Kind.t }
end
```

- **`Call`**: Function entry. `from_untraced` is true for frames inferred to exist before tracing started.
- **`Ret`**: Function return from a known stack frame.
- **`Ret_from_untraced`**: Return from a frame that existed before tracing started (we don't know its name).

#### `Callstack` (lines 62-88)

The current known call stack for a thread.

```ocaml
module Callstack = struct
  type t = {
    stack : Event.Location.t Stack.t;
    mutable create_time : Mapped_time.t;  (* When this stack was created/reset *)
  }
end
```

The `create_time` is crucial: when we return from a frame we didn't see the call for, we synthesize a `[unknown]` frame starting at `create_time`.

#### `Thread_info` (lines 98-134)

Per-thread state, the central data structure:

```ocaml
type 'thread t = {
  thread : 'thread;                           (* Handle for trace output *)
  mutable callstack : Callstack.t;            (* Current active stack *)
  inactive_callstacks : Callstack.t Stack.t;  (* Saved stacks (syscalls, interrupts) *)
  mutable last_decode_error_time : Mapped_time.t;
  ocaml_exception_state : ocaml_exception_state;
  mutable pending_events : Pending_event.t list;  (* Batched events *)
  mutable pending_time : Mapped_time.t;           (* Time of pending batch *)
  start_events : (Mapped_time.t * Pending_event.t) Deque.t;  (* Deferred early events *)
  mutable last_event_time : Mapped_time.t;
  track_group_id : int;
  extra_event_tracks : 'thread Hashtbl.M(Collection_mode.Event.Name).t;
}
```

Key fields:
- **`callstack`**: The currently active call stack being built
- **`inactive_callstacks`**: A stack of saved callstacks (used for syscalls, hardware interrupts, and OCaml exception traps)
- **`pending_events`**: Events batched at the current timestamp, waiting to be flushed
- **`start_events`**: Events that happened at the very start of the trace (deferred until we know more)

### Event Processing Flow

#### High-Level Flow

```
write_event()
    |
    v
[Transaction handling: queue if in_transaction, flush on commit, discard on abort]
    |
    v
write_event'()
    |
    +---> Get or create Thread_info
    +---> Calculate event time (with hack_155 adjustment)
    +---> Handle filtered region start/stop
    |
    v
[Dispatch based on event type]
    |
    +---> Trace event: call(), ret(), check_current_symbol(), language hacks
    +---> Power event: write_counter()
    +---> Stacktrace_sample: sync callstack to sample
    +---> Event_sample: write to separate track
```

#### `write_event` (line 985)

Handles Intel TSX transaction support:

```ocaml
let rec write_event (T t) ?events_writer original_event =
  if Env_vars.skip_transaction_handling then
    write_event' (T t) ?events_writer original_event
  else
    (* 1. Transaction abort → clear queue, deliver abort event *)
    (* 2. In transaction → queue event *)
    (* 3. Transaction ends → flush queue, deliver event *)
    (* 4. Not in transaction → deliver directly *)
```

Transaction events are queued because a transaction might abort, in which case all its events should be discarded.

#### `write_event'` (line 1024)

The core dispatch logic:

1. Get or create thread info
2. Calculate mapped time (with hack_155)
3. Handle filtered region transitions
4. Dispatch based on event kind

### Core Operations

#### `call()` (line 561)

Records entering a function:

```ocaml
let call t thread_info ~time ~location =
  let ev = Pending_event.create_call location ~from_untraced:false in
  add_event t thread_info time ev;
  Callstack.push thread_info.callstack location
```

#### `ret()` (line 678)

Records exiting a function:

```ocaml
let ret t (thread_info : _ Thread_info.t) ~time : unit =
  let returned_from = Callstack.top thread_info.callstack |> Option.map ~f:... in
  ret_without_checking_for_go_hacks t thread_info ~time;
  Go_hacks.ret_track_gogo t thread_info ~time ~returned_from
```

The `ret_without_checking_for_go_hacks` (line 567) does the actual work:
- If stack has a frame, emit a `Ret` event
- If stack is empty, emit `Ret_from_untraced` (we're returning from something called before tracing started)

#### `check_current_symbol()` (line 686)

Enforces the key invariant. Called after operations that might cause the stack top to mismatch the current PC:

```ocaml
let check_current_symbol t thread_info ~time location =
  match Callstack.top thread_info.callstack with
  | Some { symbol; _ } when not (Symbol.equal symbol location.symbol) ->
    (* Stack doesn't match PC → synthesize ret + call *)
    ret t thread_info ~time;
    call t thread_info ~time ~location
  | Some _ -> ()  (* Already matches *)
  | None ->
    (* Empty stack → infer a call from trace start *)
    let ev = Pending_event.create_call location ~from_untraced:true in
    write_pending_event t thread_info thread_info.callstack.create_time ev;
    Callstack.push thread_info.callstack location
```

This handles:
- **Tail calls**: Jump from function A to function B appears as A→B without explicit ret/call
- **PLT trampolines**: Indirect jumps through procedure linkage tables
- **Returns past trace start**: Returning from functions called before tracing began

#### `flush()` (line 442)

Distributes timestamps across batched events:

```ocaml
let flush (t : _ inner) ~to_time (thread : _ Thread_info.t) =
  (* Count time-consuming events (calls, not returns) *)
  let count = List.count thread.pending_events ~f:consumes_time in
  let total_ns = Mapped_time.diff to_time thread.pending_time |> ... in
  (* Distribute time evenly among calls *)
  List.iter (List.rev thread.pending_events) ~f:(fun ev ->
    let ns_share = if consumes_time ev then ... else 0 in
    let time = Mapped_time.add thread.pending_time (... !ns_offset) in
    ns_offset := !ns_offset + ns_share;
    write_pending_event t thread time ev);
  thread.pending_time <- to_time;
  thread.pending_events <- []
```

Returns don't consume time (they happen at the same moment as the instruction that follows). This makes traces easier to read.

### Time Distribution

When multiple events share the same timestamp (common with Intel PT), the trace writer distributes time among them:

```
Events at t=100ns: [call A, call B, call C]
Next event at t=200ns

Distribution (100ns / 3 calls ≈ 33ns each):
  - call A at t=100ns
  - call B at t=133ns
  - call C at t=166ns
```

This creates visually distinct spans in Perfetto rather than overlapping zero-width events.

### hack_155 (lines 475-495)

Fixes GitHub issue #155: events at the exact timestamp of a decode error cause stack corruption.

```ocaml
let hack_155 (thread_info : _ Thread_info.t) time =
  let last_decode_error_time = thread_info.last_decode_error_time in
  if Mapped_time.( = ) time last_decode_error_time
     && Mapped_time.( <> ) last_decode_error_time Mapped_time.start_of_trace
  then Mapped_time.add time (Time_ns.Span.of_int_ns 1)
  else time
```

If an event has the exact same timestamp as a previous decode error, slide it forward by 1ns. This preserves event ordering without significantly affecting displayed timestamps (Intel PT is only ~40ns precise anyway).

---

## 3. Language-Specific Hacks

### Go_hacks (lines 621-676)

Go's goroutine scheduler uses `gogo` to jump between coroutines. This completely breaks normal call/return tracking because `gogo` can jump anywhere.

#### The Problem

```
goroutine 1:                goroutine 2:
  foo()                       bar()
    runtime.mcall() ----+       baz()
      park_m()          |
        schedule()      |
          execute()     |
            gogo() -----+---> bar() resumes
```

When `gogo` returns, it doesn't return to `execute()` — it jumps to wherever the target goroutine was suspended.

#### The Solution

Track known "gogo destinations" — functions that goroutines are typically suspended in:
- `runtime.mcall`
- `runtime.morestack.abi0`

When we see a return from `gogo`:
1. If the stack contains a known gogo destination, pop frames until we reach it (then one more)
2. If no known destination, clear the entire stack (we're starting a fresh goroutine)

```ocaml
let ret_track_gogo t thread_info ~time ~returned_from =
  let is_ret_from_gogo = Option.value_map ~f:is_gogo returned_from ~default:false in
  if is_ret_from_gogo then
    if current_stack_contains_known_gogo_destination thread_info then
      pop_until_gogo_destination t thread_info ~time
    else
      end_of_thread t thread_info ~time ~is_kernel_address:false
```

### Ocaml_hacks (lines 727-874)

OCaml exceptions use non-local control flow that breaks stack tracking.

#### Two Modes

**With exception info** (requires compiler support from flambda-backend):
- Compiler provides addresses of `pushtrap` (install handler), `poptrap` (remove handler), and `entertrap` (exception handler entry)
- We track trap stack frames explicitly

**Without exception info** (legacy mode):
- Count calls to `caml_next_frame_descriptor` during unwinding
- On `caml_raise_exn` return, unwind that many frames

#### The Trap Stack Pattern

OCaml's `try...with` creates a "trap" that catches exceptions:

```ocaml
(* Source *)
try risky_operation () with
| Exn -> handler ()

(* Compiled structure *)
pushtrap [handler_addr]    (* Save handler on trap stack *)
  risky_operation ()       (* Protected code *)
poptrap                    (* Remove handler if no exception *)
jmp after_handler
handler_addr:              (* entertrap *)
  handler ()               (* Exception handler *)
after_handler:
```

The trace writer maintains a stack of callstacks (`inactive_callstacks`) to track this:

```ocaml
(* On pushtrap: *)
Stack.push thread_info.inactive_callstacks thread_info.callstack;
thread_info.callstack <- Callstack.create ~create_time:time;
(* Push synthetic frame equal to current top *)

(* On poptrap (normal exit): *)
ignore (Callstack.pop thread_info.callstack);
clear_trap_stack t thread_info ~time;

(* On entertrap (exception raised): *)
clear_trap_stack t thread_info ~time;  (* Pop all frames in try block *)
```

#### `track_executed_pushtraps_and_poptraps_in_range` (line 803)

Scans the instruction range between two events looking for trap operations:

```ocaml
Ocaml_exception_info.iter_pushtraps_and_poptraps_in_range
  ocaml_exception_info
  ~from:last_known_instruction_pointer
  ~to_:src.instruction_pointer
  ~f:(fun (_addr, kind) ->
    match kind with
    | Pushtrap -> (* Save current stack, create new one *)
    | Poptrap -> (* Restore previous stack *))
```

### Inactive Callstacks

The `inactive_callstacks` field is a stack-of-stacks pattern used for:

1. **OCaml traps**: Each `try...with` block gets its own stack
2. **Syscalls** (with kernel tracing): Kernel executes on a separate conceptual stack
3. **Hardware interrupts**: Interrupt handlers have their own stack

```
Active callstack:     [interrupt_handler, ...]
                           |
Inactive callstacks:  [syscall_frame, ...]
                           |
                      [user_main, user_foo, ...]
```

When returning from an interrupt/syscall, we pop from `inactive_callstacks` to restore the previous context.

### Transaction Handling (lines 986-1022)

Intel TSX (Transactional Synchronization Extensions) allows speculative execution. Events during a transaction might be rolled back.

```ocaml
match event with
| Ok { data; in_transaction } ->
  let is_abort = match data with Trace { kind = Some Tx_abort; _ } -> true | _ -> false in
  if is_abort then
    (* Abort: discard queued events *)
    Deque.clear t.transaction_events;
    write_event' (T t) ?events_writer original_event
  else if in_transaction then
    (* In transaction: queue for later *)
    Deque.enqueue_back t.transaction_events original_event
  else
    (* Transaction ended or not in one: flush and deliver *)
    if not (Deque.is_empty t.transaction_events) then (
      Deque.iter' t.transaction_events `front_to_back ~f:(fun ev ->
        write_event' (T t) ?events_writer ev);
      Deque.clear t.transaction_events);
    write_event' (T t) ?events_writer original_event
```

---

## 4. Debugging Guide

### Enabling Debug Mode

```ocaml
Trace_writer.debug := true
```

When enabled, the trace writer prints its state (via `sexp_of_inner`) after every trace event. This shows the current callstack for each thread.

In tests, pass `~debug:true` to `Perf_script.run`:

```ocaml
let%expect_test "my test" =
  let%map () = Perf_script.run ~debug:true ~trace_scope:Userspace "my_file.perf" in
  [%expect {| ... |}]
```

### Common Symptoms and Causes

| Symptom | Likely Cause |
|---------|--------------|
| Inside-out stack (child appears to contain parent) | Missing return event; check for Go `gogo` or OCaml exception |
| Missing frames | Decode error cleared the stack; check for overflow packet warnings |
| Duplicate frames at trace start | Events deferred to `start_events` being written incorrectly |
| `[unknown]` frames everywhere | Returns from functions called before tracing started |
| Stack corruption after specific timestamp | hack_155 not applied; event at decode error time |

### Test-Driven Debugging

1. Capture a minimal `.perf` file that reproduces the issue
2. Add it to `test/` directory
3. Write an expect test:

```ocaml
let%expect_test "my issue" =
  let%map () = Perf_script.run ~trace_scope:Userspace "my_issue.perf" in
  [%expect {| expected output |}]
```

4. Run with debug mode to see state transitions
5. The test will show the actual output; compare with expected

### Key Invariant Violations

Watch for these in debug output:

1. **Stack top doesn't match current symbol**: `check_current_symbol` should fix this
2. **Returning from empty stack**: Should produce `[unknown]` frame
3. **OCaml trap depth mismatch**: Warning printed, may indicate dropped PT data

### Useful Test Files

| File | What it tests |
|------|---------------|
| `test/decode_errors.ml` | Recovery from Intel PT overflow |
| `test/ocaml_exceptions.ml` | Exception unwinding with trap info |
| `test/simple_gogo.ml` | Go goroutine switching |
| `test/hello_world_userspace.ml` | Basic userspace tracing |
| `test/hello_world_with_kernel_tracing.ml` | Kernel + userspace tracing |

---

## 5. Extension Guide

### Adding New Event Kinds

1. Add the new kind to `Event.Kind.t` in `src/event.ml`:

```ocaml
type t =
  | Async | Call | Return | ...
  | MyNewKind  (* Add here *)
```

2. Update `Event.Ok.Data.t` if needed (for new event data variants)

3. Handle the new kind in `write_event'` (line 1024):

```ocaml
| { data = Trace { kind = Some MyNewKind; ... }; ... } ->
  (* Handle your new event kind *)
```

### Adding Language-Specific Hacks

Follow the pattern established by `Go_hacks` and `Ocaml_hacks`:

1. Create a new module within `trace_writer.ml`:

```ocaml
module Rust_hacks : sig
  val handle_async_await : 'a inner -> 'a Thread_info.t -> time:Mapped_time.t -> unit
end = struct
  (* Implementation *)
end
```

2. Hook into the appropriate event processing points:
   - In `ret` for post-return processing
   - In the trace event dispatch for specific event patterns

3. Add detection logic for your language's patterns:

```ocaml
let is_rust_future_poll symbol =
  match symbol with
  | From_perf s -> String.is_substring s ~substring:"poll"
  | _ -> false
```

### Supporting New Trace Output Formats

The trace writer uses the `S_trace` interface (from `trace_writer_intf.ml`):

```ocaml
module type S_trace = sig
  type thread

  val allocate_pid : name:string -> int
  val allocate_thread : pid:int -> name:string -> thread
  val write_duration_begin : args:... -> thread:thread -> name:string -> time:Time_ns.Span.t -> unit
  val write_duration_end : args:... -> thread:thread -> name:string -> time:Time_ns.Span.t -> unit
  val write_duration_complete : args:... -> thread:thread -> name:string -> time:Time_ns.Span.t -> time_end:Time_ns.Span.t -> unit
  val write_duration_instant : args:... -> thread:thread -> name:string -> time:Time_ns.Span.t -> unit
  val write_counter : args:... -> thread:thread -> name:string -> time:Time_ns.Span.t -> unit
end
```

To add a new output format:

1. Create a module implementing `S_trace`
2. Pass it to `Trace_writer.create_expert`

Example (from `test/perf_script.ml`):

```ocaml
let module Trace = struct
  type thread = int
  let allocate_pid ~name:_ = incr next_pid; !next_pid
  let allocate_thread ~pid:_ ~name:_ = incr next_thread; !next_thread
  let write_duration_begin ~args:_ ~thread:_ ~name ~time =
    printf "-> %s BEGIN %s\n" (Time_ns.Span.to_string_hum time) name
  (* ... *)
end in
Trace_writer.create_expert ... (module Trace)
```

### Handling Trace Scope and State Changes

The trace writer supports different trace scopes (defined in `Trace_scope.t`):
- `Userspace`: Only user-mode code
- `Kernel`: Only kernel-mode code
- `Userspace_and_kernel`: Both

State changes (`Trace_state_change.t`):
- `Start`: Tracing resumed (e.g., after page fault)
- `End`: Tracing paused (e.g., entering untraced library)

Handle these in the trace event dispatch (around line 1120):

```ocaml
| Some Syscall, Some End ->
  (* Entering syscall from userspace *)
  assert_trace_scope t outer_event [ Userspace ];
  call t thread_info ~time ~location:Event.Location.syscall

| None, Some Start ->
  (* Tracing resumed *)
  if Trace_scope.equal t.trace_scope Kernel then
    (* In kernel mode: new stack *)
    clear_callstack t thread_info ~time;
    Thread_info.set_callstack_from_addr thread_info ~addr:dst.instruction_pointer ~time
  else if Callstack.is_empty thread_info.callstack then
    (* First start of trace *)
    call t thread_info ~time ~location:dst
  else
    (* Resume from stop *)
    Ocaml_hacks.ret_track_exn_data t thread_info ~time
```

---

## 6. Quick Reference

### Key Types

| Type | Location | Purpose |
|------|----------|---------|
| `Mapped_time.t` | line 10 | Relative timestamp (base subtracted) |
| `Pending_event.t` | line 35 | Queued call/return event |
| `Callstack.t` | line 62 | Thread's current call stack |
| `Thread_info.t` | line 98 | Complete per-thread state |
| `Event.t` | `event.ml:111` | Input event from perf (Result of Ok.t or Decode_error.t) |
| `Event.Kind.t` | `event.ml:3` | Event classification (Call, Return, Jump, etc.) |
| `Event.Location.t` | `event.ml:26` | Code location (address + symbol + offset) |
| `Symbol.t` | `symbol.ml:3` | Function name representation |

### Key Functions

| Function | Line | Purpose |
|----------|------|---------|
| `write_event` | 985 | Entry point, handles transactions |
| `write_event'` | 1024 | Core event processing |
| `call` | 561 | Record function entry |
| `ret` | 678 | Record function exit (with Go hack) |
| `ret_without_checking_for_go_hacks` | 567 | Raw return processing |
| `check_current_symbol` | 686 | Enforce stack/PC invariant |
| `flush` | 442 | Write pending events with distributed times |
| `add_event` | 464 | Queue event for current timestamp |
| `end_of_trace` | 887 | Finalize all thread stacks |
| `end_of_thread` | 602 | Clear one thread's stack |
| `clear_callstack` | 582 | Pop all frames from active stack |
| `clear_all_callstacks` | 593 | Pop all frames from all stacks |
| `hack_155` | 475-495 | Fix decode error timestamp collision |
| `event_time` | 497 | Calculate mapped time for event |
| `create_thread` | 513 | Initialize Thread_info for new thread |

### Language Hack Summary

| Module | Lines | Language | Handles |
|--------|-------|----------|---------|
| `Go_hacks` | 621-676 | Go | Goroutine switching via `gogo` |
| `Ocaml_hacks` | 727-874 | OCaml | Exception unwinding, `try...with` traps |

### Test File Locations

| Test | File |
|------|------|
| Decode errors | `test/decode_errors.ml` |
| OCaml exceptions | `test/ocaml_exceptions.ml` |
| Go goroutines | `test/simple_gogo.ml` |
| Basic userspace | `test/hello_world_userspace.ml` |
| Kernel tracing | `test/hello_world_with_kernel_tracing.ml` |
| Event output format | `test/test_events_output.ml` |
| Test harness | `test/perf_script.ml` |

### Related Files

| File | Purpose |
|------|---------|
| `src/trace_writer.ml` | Core module (this guide's focus) |
| `src/event.ml` | Input event types |
| `src/symbol.ml` | Symbol representation |
| `src/trace_writer_intf.ml` | Output interface (`S_trace`) |
| `src/real_trace.ml` | Tracing library wrapper |
| `src/ocaml_exception_info.mli` | OCaml trap info interface |
| `src/perf_decode.ml` | Event parsing from perf output |
