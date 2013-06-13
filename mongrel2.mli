
(*
  OCaml-Mongrel2-Handler, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

type ('a, 'b, 'c, 'd, 'e) t

val run_main_loop : string -> string -> unit

val fini : ('a, 'b, 'c, 'd, 'e) t -> unit

val run :  ('a, 'b, 'c, 'd, 'e) t -> unit Lwt.t
