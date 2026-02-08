open! Core

module Request = struct
  type t =
    { addr : Int64.t
    ; executable : Filename.t
    }
  [@@deriving compare, sexp_of, hash]
end

module Inlined_frame = struct
  type t =
    { line : int
    ; column : int
    ; demangled_name : string
    ; filename : string
    }
  [@@deriving sexp, fields, bin_io, compare, equal]

  let display_name { demangled_name; _ } = demangled_name ^ " [inlined]"
end

module Response = struct
  type t =
    | None
    | Some of
        { demangled_name : string
        ; inlined_frames : Inlined_frame.t array
        }
  [@@deriving sexp_of] [@@warning "-37"]
end

external symbolize
  :  executable:Filename.t
  -> addr:(Int64.t[@unboxed])
  -> Response.t
  = "magic_trace_llvm_symbolize_address_bytecode" "magic_trace_llvm_symbolize_address"

let (symbolization_cache : (Request.t, Response.t) Hashtbl.t) =
  Hashtbl.create (module Request)
;;

let symbolize ~executable ~addr =
  (Hashtbl.find_or_add [@inlined hint])
    symbolization_cache
    { addr; executable }
    ~default:(fun () -> symbolize ~executable ~addr)
;;
