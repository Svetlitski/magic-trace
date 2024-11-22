#include <cstdint>
#include "./install/include/llvm/DebugInfo/Symbolize/Symbolize.h"
#define CAML_NAME_SPACE
#include "/usr/local/home/ksvetlitski/application_data/opam/magic-trace/lib/ocaml/caml/alloc.h"
#include "/usr/local/home/ksvetlitski/application_data/opam/magic-trace/lib/ocaml/caml/memory.h"
#include "/usr/local/home/ksvetlitski/application_data/opam/magic-trace/lib/ocaml/caml/mlvalues.h"

llvm::symbolize::LLVMSymbolizer symbolizer{{.UseSymbolTable = false}};
namespace {
value ocaml_string_of_cpp_string(const std::string &string) {
  return caml_alloc_initialized_string(string.length(), string.data());
}
}

struct inlined_frame {
  value demangled_name /* : string */;
  value filename /* : string */;
  value line /* : int */;
  value column /* : column */;
};

extern "C" {
CAMLprim value magic_trace_llvm_symbolize_address(value v_executable_file,
                                                  value v_address) {
  // [v_address] is always an integer, so no need to mention it in [CAMLparam*].
  CAMLparam1(v_executable_file);
  CAMLlocal5(function_names, function_names_option, filename, demangled_name,
             inlined_frame);
  std::string executable_file{String_val(v_executable_file),
                              caml_string_length(v_executable_file)};
  llvm::object::SectionedAddress address{(uintptr_t)(Long_val(v_address)),
                                         llvm::object::SectionedAddress::UndefSection};
  auto result = symbolizer.symbolizeInlinedCode(executable_file, address);
  if (auto error = result.takeError()) {
    CAMLreturn(Val_none);
  }
  const auto &frames = result.get();
  const uint32_t num_frames = frames.getNumberOfFrames();
  if (num_frames < Max_young_wosize) {
    function_names = caml_alloc_small(/*wosize=*/num_frames, /*tag=*/0);
    memset((uint8_t *)function_names, 0xFF, sizeof(value) * num_frames);
  } else {
    function_names = caml_alloc_shr(/*wosize=*/num_frames, /*tag=*/0);
    for (uint32_t i = 0; i < num_frames; i++) {
      caml_initialize(&Field(function_names, i), Val_unit);
    }
  }
  for (uint32_t i = 0; i < num_frames; i++) {
    const auto &frame = frames.getFrame(i);
    filename = ocaml_string_of_cpp_string(frame.FileName);
    demangled_name = ocaml_string_of_cpp_string(frame.FunctionName);
    struct inlined_frame *inlined_frame = (struct inlined_frame *)caml_alloc_small(
        /*wosize=*/Wsize_bsize(sizeof(struct inlined_frame)), /*tag=*/0);
    inlined_frame->demangled_name = demangled_name;
    inlined_frame->filename = filename;
    inlined_frame->line = Val_long(frame.Line);
    inlined_frame->column = Val_long(frame.Column);
    caml_modify(&Field(function_names, num_frames - 1 - i), (value)inlined_frame);
  }
  function_names_option = caml_alloc_some(function_names);
  CAMLreturn(function_names_option);
}
}
