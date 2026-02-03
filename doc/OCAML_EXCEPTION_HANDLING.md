# OCaml Exception Handling in the Trace Writer

This document explains how `src/trace_writer.ml` uses OCaml exception information from the compiler to produce more accurate traces.

## Overview

The trace writer has two modes for handling OCaml exceptions:

1. **With exception info** (requires compiler support from flambda-backend): Uses precise location data from the `.note.ocaml_eh` ELF section to track exception handler installation/removal and accurately unwind the call stack when exceptions are raised.

2. **Without exception info** (fallback): Uses a heuristic based on counting calls to `caml_next_frame_descriptor` to estimate how many frames to unwind. This mode fails to handle `raise_notrace` exceptions correctly.

## Key Data Structures

### `Ocaml_exception_info.t` (`src/ocaml_exception_info.ml`)

Contains three arrays of instruction addresses:
- **`pushtraps`**: Addresses where exception handlers are installed (OCaml's `try` keyword)
- **`poptraps`**: Addresses where exception handlers are removed when protected blocks return normally
- **`entertraps`**: Addresses that are the start of exception handlers (jumped to when an exception is raised)

### `Thread_info.ocaml_exception_state` (`src/trace_writer.ml:100-106`)

Each thread maintains one of two states:
```ocaml
type ocaml_exception_state =
  | Without_exception_info of { frames_to_unwind : int ref }
  | With_exception_info of
      { ocaml_exception_info : Ocaml_exception_info.t
      ; last_known_instruction_pointer : int64 option ref
      }
```

### `Thread_info.inactive_callstacks` (`src/trace_writer.ml:114`)

A stack of callstacks. When entering a `try` block (pushtrap), the current callstack is pushed here and a new callstack is started. This models how exception handlers operate on a logically separate stack context.

## How It Works (With Exception Info)

### Loading Exception Data

The exception info is extracted from the ELF binary in `src/elf.ml:36-86`:
1. Reads the `.note.ocaml_eh` section (emitted by the patched OCaml compiler)
2. Parses arrays of `entertrap`, `pushtrap`, and `poptrap` addresses
3. Returns an `Ocaml_exception_info.t`

### Tracking Pushtraps and Poptraps

The function `Ocaml_hacks.track_executed_pushtraps_and_poptraps_in_range` (`trace_writer.ml:819-889`) is called on every trace event:

1. **On Pushtrap** (entering a `try` block):
   - Push current callstack to `inactive_callstacks`
   - Create a new callstack for the protected block
   - Push a synthetic frame (copy of current top frame) to avoid incorrectly inferring frames within the `try` block

2. **On Poptrap** (leaving a `try` block normally):
   - If callstack depth is 1 (only the synthetic frame), pop it and restore the previous callstack from `inactive_callstacks`
   - If depth > 1, data was likely dropped (logs a warning and attempts recovery)

### Detecting Exception Raises (Entertrap)

The function `Ocaml_hacks.check_current_symbol_track_entertraps` (`trace_writer.ml:796-817`) detects jumps to entertrap addresses:

When an indirect jump lands on an entertrap address:
1. Remove the synthetic frame from the current callstack
2. Call `clear_trap_stack` to close all frames up to the trap marker
3. Restore the previous callstack context from `inactive_callstacks`

This correctly unwinds the stack regardless of how many frames were built up in the `try` block.

## How It Works (Without Exception Info - Fallback)

In `Ocaml_hacks.ret_track_exn_data` (`trace_writer.ml:771-787`):

1. Counts calls to `caml_next_frame_descriptor` in `frames_to_unwind`
2. When `caml_raise_exn` or `caml_raise_exception` returns:
   - Unwind `frames_to_unwind +/- constant` frames
   - Reset counter to 0

**Limitation**: This heuristic fails for `raise_notrace` because it doesn't call `caml_next_frame_descriptor`.

## Key Files

| File | Purpose |
|------|---------|
| `src/trace_writer.ml` | Main trace processing, contains `Ocaml_hacks` module |
| `src/ocaml_exception_info.ml(i)` | Data structure and queries for exception location data |
| `src/elf.ml` | Loads exception info from `.note.ocaml_eh` ELF section |
| `test/ocaml_exceptions.ml` | Test cases demonstrating exception handling |

## Example: Exception Unwinding

Given this OCaml code:
```ocaml
let () =
  before_try ();
  (try try_body () with _ -> exception_handler ());
  after_try ()
```

The trace writer sees these events:
1. `call before_try` - normal call handling
2. **Pushtrap at 0x13bcd** - save callstack, start new one with synthetic frame
3. `call try_body` - normal call handling
4. **Indirect jump to entertrap at 0x13bf2** (exception raised):
   - Detect jump to entertrap address
   - Close all frames since pushtrap
   - Restore saved callstack
5. `call exception_handler` - normal call handling
6. `call after_try` - normal call handling

Without exception info, steps 2, 4, and the correct unwinding would not occur.

## Assembly-Level Detail

The `.mli` file for `Ocaml_exception_info` contains a detailed example showing how the OCaml compiler generates code for exception handling. Here's a summary:

### Pushtrap (entering `try` block)
```asm
; Push exception handler address onto handler stack
lea    r11,[rip+0x1e]        ; address of exception handler
push   r11
push   QWORD PTR [r14+0x10]  ; previous handler
mov    QWORD PTR [r14+0x10],rsp
```

### Poptrap (leaving `try` block normally)
```asm
; Remove exception handler from stack
pop    QWORD PTR [r14+0x10]
add    rsp,0x8
jmp    <after exception handler>
```

### Entertrap (exception raised)
```asm
; Inside caml_raise_exn / raise function:
mov    rsp,QWORD PTR [r14+0x10]  ; restore stack
pop    QWORD PTR [r14+0x10]      ; restore previous handler
pop    r11                       ; get handler address
jmp    r11                       ; jump to exception handler
```

The indirect `jmp r11` lands at an entertrap address, which magic-trace detects to properly unwind the stack.

## Compiler Requirements

Exception info requires OCaml compiled with:
- [flambda-backend PR #616](https://github.com/ocaml-flambda/flambda-backend/pull/616)

This patch makes the compiler emit the `.note.ocaml_eh` section containing the trap metadata.
