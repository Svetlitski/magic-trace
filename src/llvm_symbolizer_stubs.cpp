#include <cstdint>
#include <cstring>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "llvm/include/llvm/DebugInfo/Symbolize/Symbolize.h"

namespace {
llvm::symbolize::LLVMSymbolizer symbolizer{{.UseSymbolTable = false}};

value ocaml_string_of_cpp_string(const std::string &string) {
  return caml_alloc_initialized_string(string.length(), string.data());
}
} // namespace
  
// Corresponds to [Event.Inlined_frame.t]
struct inlined_frame {
  value demangled_name /* : string */;
  value filename /* : string */;
  value line /* : int */;
  value column /* : int */;
};

extern "C" {
CAMLprim value magic_trace_llvm_symbolize_address(value v_executable_file,
                                                  uintptr_t address) {
  // [symbolizeInlinedCode] takes a [const std::string&] and not a [std::string_view]
  // or similar, so we *have* to copy it from the OCaml string first, which is a little
  // sad. However, as a consequence this means only ever read [v_executable_file] before
  // we ever make any allocations on the OCaml heap, so we can get away with not needing
  // to register it with [CAMLparam*].
  CAMLparam0();
  CAMLlocal4(inlined_frames, filename, demangled_name, inlined_frame);
  std::string executable_file{String_val(v_executable_file),
                              caml_string_length(v_executable_file)};
  llvm::object::SectionedAddress sectioned_address{
      address, llvm::object::SectionedAddress::UndefSection};
  auto result = symbolizer.symbolizeInlinedCode(executable_file, sectioned_address);
  if (auto error = result.takeError()) {
    CAMLreturn(Val_none);
  }
  const auto &frames = result.get();
  const uint32_t num_frames = frames.getNumberOfFrames();
  if (num_frames <= Max_young_wosize) [[likely]] {
    inlined_frames = caml_alloc_small(/*wosize=*/num_frames, /*tag=*/0);
  } else {
    inlined_frames = caml_alloc_shr(/*wosize=*/num_frames, /*tag=*/0);
  }
  memset((uint8_t *)inlined_frames, 0xFF, Bsize_wsize(num_frames));
  for (uint32_t i = 0; i < num_frames; i++) {
    const auto &frame = frames.getFrame(i);
    filename = ocaml_string_of_cpp_string(frame.FileName);
    demangled_name = ocaml_string_of_cpp_string(frame.FunctionName);
    constexpr mlsize_t inlined_frame_wosize = Wsize_bsize(sizeof(struct inlined_frame));
    static_assert(inlined_frame_wosize <= Max_young_wosize);
    auto *inlined_frame =
        (struct inlined_frame *)caml_alloc_small(inlined_frame_wosize, /*tag=*/0);
    *inlined_frame = (struct inlined_frame){.demangled_name = demangled_name,
                                            .filename = filename,
                                            .line = Val_long(frame.Line),
                                            .column = Val_long(frame.Column)};
    caml_modify(&Field(inlined_frames, num_frames - 1 - i), (value)inlined_frame);
  }
  inlined_frames = caml_alloc_some(inlined_frames);
  CAMLreturn(inlined_frames);
}

CAMLprim value magic_trace_llvm_symbolize_address_bytecode(value v_executable_file,
                                                           value v_address) {
  return magic_trace_llvm_symbolize_address(v_executable_file, Field(v_address, 0));
}
}
