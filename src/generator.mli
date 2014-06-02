
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)


type handler = 
		Mongrel2.mongrel2_request -> 
		string array -> 
		Mongrel2.mongrel2_response Lwt.t

val serve_file : string -> handler
val serve_md_file : string -> handler
val serve_layout_file : string -> handler
val pg_now : string -> handler

val not_found : 'a -> 'b -> Mongrel2.mongrel2_response Lwt.t

val return_generic_error : Code.status -> Mongrel2.mongrel2_response Lwt.t
