open! Core
open Unboxed

(* TODO Nearly everything about the way this module is implemented is slow, and adds
   measurable overhead. We should do something less naive here. *)

module Request = struct
  type t =
    { addr : I64.t
    ; executable : Interned_string.t
    }
  [@@deriving compare, sexp_of, hash]
end

module Info = struct
  type t = { demangled_name : string }
  [@@unboxed] [@@deriving equal, compare, hash, sexp_of]

  let display_name { demangled_name } = demangled_name ^ " [inlined]"
end

module Response = struct
  (** This is ordered root-to-leaf such that the entry at index 0 is the physical frame,
      and the subsequent entries are the inlined frames. *)
  type t = Info.t iarray [@@deriving sexp_of, equal, hash, compare]

  let physical_frame t = Iarray.unsafe_get t 0
  let inlined_frames t = Slice.create t ~pos:1 ~len:(Iarray.length t - 1)
end

external symbolize
  :  executable:Interned_string.t
  -> addr:i64
  -> Response.t or_null
  = "magic_trace_llvm_symbolize_address_bytecode" "magic_trace_llvm_symbolize_address"

let (symbolization_cache : (Request.t, Response.t Uopt.t) Hashtbl.t) =
  (* Use templated [@kind value value_or_null] once that's available instead of converting to [Uopt.t] *)
  Hashtbl.create (module Request)
;;

let response_cache = Hash_set.create (module Response)

let symbolize ~executable ~addr =
  let addr = I64.of_int64 addr in
  let result =
    Hashtbl.find_or_add
      symbolization_cache
      { addr; executable }
      ~default:(stack_ fun () ->
        match symbolize ~executable ~addr with
        | Null -> Uopt.none
        | This response -> Uopt.some (Hash_set.get_or_add response_cache response))
  in
  Bool.select (Uopt.is_some result) (This (Uopt.unsafe_value result)) Null
;;
