#include <cstdint>
#include <cstring>
#include <cstdlib>
#include <string>
#include <unordered_map>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "llvm/DebugInfo/Symbolize/Symbolize.h"

namespace {
llvm::symbolize::LLVMSymbolizer symbolizer{{.UseSymbolTable = false}};

value ocaml_string_of_cpp_string(const std::string &string) {
  return caml_alloc_initialized_string(string.length(), string.data());
}

value ext_ocaml_string_of_cpp_string(const std::string &string) {
  size_t len = string.length();
  // [wosize] is the size of the string in words, including padding but not
  // including the header.
  mlsize_t wosize = (len + sizeof(value)) / sizeof(value);
  // allocate [(wosize + 1)] words to include the header */
  value *ext_ocaml_string = (value *)malloc(Whsize_wosize(wosize) * sizeof(value));
  ext_ocaml_string[0] = Caml_out_of_heap_header(wosize, String_tag);
  value result = Val_hp(ext_ocaml_string);
  // Initialize the padding at the end of the string
  Field(result, wosize - 1) = 0;
  mlsize_t offset_index = Bsize_wsize(wosize) - 1;
  Byte(result, offset_index) = offset_index - len;
  memcpy(Bytes_val(result), string.data(), len);
  return result;
}

value ext_ocaml_string_of_filename(const std::string &filename) {
  static std::unordered_map<std::string, value> filename_ocaml_string_cache{};
  const auto &result = filename_ocaml_string_cache.find(filename);
  if (result != filename_ocaml_string_cache.end()) {
    return result->second;
  }
  value ext_ocaml_string = ext_ocaml_string_of_cpp_string(filename);
  filename_ocaml_string_cache.insert({filename, ext_ocaml_string});
  return ext_ocaml_string;
}

} // namespace

// Corresponds to [Event.Inlined_frame.t]
struct inlined_frame {
  value line /* : int */;
  value column /* : int */;
  value demangled_name /* : string */;
  value filename /* : string */;
};

// Corresponds to [Perf_decode.Symbolizer.Response.t]
struct response {
  value demangled_name /* : string */;
  value inlined_frames /* Event.Inlined_frame.t array */;
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
  CAMLlocal4(inlined_frames, demangled_name, inlined_frame, response);
  std::string executable_file{String_val(v_executable_file),
                              caml_string_length(v_executable_file)};
  llvm::object::SectionedAddress sectioned_address{
      address, llvm::object::SectionedAddress::UndefSection};
  auto result = symbolizer.symbolizeInlinedCode(executable_file, sectioned_address);
  if (!result || result->getNumberOfFrames() == 0) {
    CAMLreturn(Val_none);
  }
  const auto &frames = result.get();
  const uint32_t num_inlined_frames = frames.getNumberOfFrames() - 1;
  if (num_inlined_frames == 0) {
    inlined_frames = Atom(0);
  } else {
    if (num_inlined_frames <= Max_young_wosize) [[likely]] {
      inlined_frames = caml_alloc_small(/*wosize=*/num_inlined_frames, /*tag=*/0);
    } else {
      inlined_frames = caml_alloc_shr(/*wosize=*/num_inlined_frames, /*tag=*/0);
    }
    memset((uint8_t *)inlined_frames, 0xFF, Bsize_wsize(num_inlined_frames));
    for (uint32_t i = 0; i < num_inlined_frames; i++) {
      const auto &frame = frames.getFrame(i);
      value filename = ext_ocaml_string_of_filename(frame.FileName);
      demangled_name = ocaml_string_of_cpp_string(frame.FunctionName);
      constexpr mlsize_t inlined_frame_wosize = Wsize_bsize(sizeof(struct inlined_frame));
      static_assert(inlined_frame_wosize <= Max_young_wosize);
      inlined_frame = caml_alloc_small(inlined_frame_wosize, /*tag=*/0);
      auto *inlined_frame_ptr = (struct inlined_frame *)inlined_frame;
      *inlined_frame_ptr = (struct inlined_frame){
          .line = Val_long(frame.Line),
          .column = Val_long(frame.Column),
          .demangled_name = demangled_name,
          .filename = filename,
      };
      caml_modify(&Field(inlined_frames, num_inlined_frames - 1 - i),
                  (value)inlined_frame);
    }
  }
  // The last frame corresponds to the function itself, not any of its inlined children.
  demangled_name =
      ocaml_string_of_cpp_string(frames.getFrame(num_inlined_frames).FunctionName);
  response = caml_alloc_small(2, /*tag=*/0);
  auto *response_ptr = (struct response *)response;
  *response_ptr = (struct response){.demangled_name = demangled_name,
                                    .inlined_frames = inlined_frames};
  CAMLreturn(response);
}

CAMLprim value magic_trace_llvm_symbolize_address_bytecode(value v_executable_file,
                                                           value v_address) {
  return magic_trace_llvm_symbolize_address(v_executable_file, Field(v_address, 0));
}
}
