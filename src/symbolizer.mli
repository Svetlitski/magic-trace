open! Core

module Inlined_frame : sig
  type t = private
    { line : int
    ; column : int
    ; demangled_name : string
    ; filename : string
    }
  [@@deriving sexp_of]

  val display_name : t -> string
end

module Response : sig
  type t = private
    | None
    | Some of
        { demangled_name : string
        ; inlined_frames : Inlined_frame.t array
        (*= [inlined_frames] is ordered root-to-leaf, such that the "root" is at index 0,
             and "leaf" is at index [length - 1]. [inlined_frames] does *not* contain the
             enclosing physical (i.e. non-inlined) frame.

             For example, if you had the following pseudocode:

             ```
             function baz(x) {
               return x * 5;
             }

             function bar(x) {
              return baz(x) / 3;
             }

             function foo(x) {
               return bar(x) + 27;
             }
             ```

             If the calls to [bar] and [baz] are both inlined, and you called [symbolize] on an address within [foo],
             the [inlined_frames] you would receive would be:
             ```
             [| "bar"; "baz" |]
             ```
             (but with [Inlined_frame.t] objects instead of the simple strings I've shown for the sake of explanation).
         *)
        }
  [@@deriving sexp_of]
end

(** Symbolizes the given address. Returns [Response.None] if the address is unrecognized,
    or if there are no inlined frames at that address (i.e. the address is within a leaf
    function). *)
val symbolize : executable:Filename.t -> addr:Int64.t -> Response.t
