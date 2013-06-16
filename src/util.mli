
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

val file_contents : string -> string Lwt.t
val path_join : string list -> string
val html_of_markdown : string -> string
val ext_of_filename : string -> string option
