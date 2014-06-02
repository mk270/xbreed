
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Lwt
open Mongrel2
open Util

let run inbound_address outbound_address docroot =
	let handlers =  [
		("^/_layouts/", Generator.not_found);
		("^/favicon.ico$", Generator.not_found);
		("^/try-mustache.html", Generator.serve_layout_file docroot);
		("^/pg_now/$", Generator.pg_now docroot);
		("\\.md$", Generator.serve_md_file docroot);
		("^/", Generator.serve_file docroot);
	] in
	let dispatcher = Dispatcher.make handlers Generator.not_found in
	let context = Mongrel2.init inbound_address outbound_address dispatcher
	in
		Lwt_main.run (Mongrel2.run context);
		Mongrel2.fini context

let () =
	let inbound_address = ref "tcp://127.0.0.1:9999" in
	let outbound_address = ref "tcp://127.0.0.1:9998" in
	let docroot = ref "." in
	let speclist = [
		("--in" , Arg.Set_string inbound_address, "TBD");
		("--out", Arg.Set_string outbound_address, "TBD");
		("--docroot", Arg.Set_string docroot, "TBD");
	] in
	Arg.parse speclist (fun s -> assert false) "Usage: TBD";
		run !inbound_address !outbound_address !docroot
