# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is magic-trace?

magic-trace is a high-resolution process tracing tool developed and maintained by Jane Street. It collects and displays detailed traces of what a process is doing at nanosecond resolution. Key features include:

- Collects all control flow (function calls) using Intel Processor Trace (PT)
- Works without application code changes
- Traces every function call with ~40ns resolution
- Renders a timeline of call stacks going back ~10ms (configurable)

The tool is designed for debugging, performance analysis, and understanding production application behavior. It uses perf to drive Intel PT and outputs traces compatible with the Perfetto UI visualization system.

## Build Commands

```bash
dune build          # Build the project
dune build @runtest # Run all tests
```

The built binary is `magic-trace`. Entry point: `bin/magic_trace_bin.ml` which delegates to `Magic_trace_lib.Trace.command`.

## Architecture

### Source Layout

- **src/**: Core library (~60 files) - trace capture, Intel PT decoding, symbol resolution, Perfetto output
- **bin/**: Thin executable wrapper
- **test/**: Test suite with real `.perf` sample files for regression testing
- **lib/magic_trace/**: Public library API for programmatic trace triggering (`Magic_trace.take_snapshot()`)
- **vendor/**: Vendored dependencies (fzf, tracing). You should pay little attention to these.

### Key Modules (src/)

`trace_writer.ml` is arguably **the single most important file in the entire project**,
implementing the core logic for interpreting a stream of events to produce a timeline
of callstacks. To develop an understanding of the codebase, you should begin by reading
this file **in its entirety** first, then work your way out to the various modules it
references (preferably by navigating to them with the OCaml LSP).

- **Trace collection**:  `trace_writer.ml`, `trace.ml`, `real_trace.ml`
- **Intel PT decoding**: `perf_decode.ml` (central decoding logic)
- **Perf integration**: `perf_tool_backend.ml`, `perf_map.ml`, `perf_capabilities.ml`
- **Symbol handling**: `elf.ml`, `symbol.ml`, `demangle_ocaml_symbols.ml`
- **Breakpoints**: `breakpoint.ml`, `symbol_selection.ml`
- **Output**: `tracing_tool_output.ml` (Perfetto format)

### C Stubs

Minimal C code for system interfaces: `boot_time_stubs.c`, `breakpoint_stubs.c`, `ptrace_stubs.c`, `perf_dlfilter.c`

## Testing

Tests use ppx_jane inline expect tests. Test data includes real perf output files (`*.perf`) in `test/` for validating decoding correctness.
