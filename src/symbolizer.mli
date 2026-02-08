open! Core

module Inlined_frame : sig
  type t = private
    { line : int
    ; column : int
    ; demangled_name : string
    ; filename : string
    } [@@deriving sexp_of]

  val display_name : t -> string
end

module Response : sig
  type t = private
    | None
    | Some of
        { demangled_name : string
        ; inlined_frames : Inlined_frame.t array
        }
        [@@deriving sexp_of]
end

val symbolize : executable:Filename.t -> addr:Int64.t -> Response.t
