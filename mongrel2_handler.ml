
(*
  OCaml-Mongrel2-Handler, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Mongrel2

let () =
	let context = Mongrel2.init "tcp://127.0.0.1:9999" "tcp://127.0.0.1:9998" in
		Lwt_main.run (Mongrel2.run context);
		Mongrel2.fini context
