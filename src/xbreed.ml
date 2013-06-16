
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

module Dispatcher : sig
	val make : 
		(string * Generator.handler) list -> 
		Generator.handler ->
        Mongrel2.mongrel2_request -> 
		Mongrel2.mongrel2_response Lwt.t

end = struct

	let dispatch handlers handle_404 request =
		let uri = uri_of_request request in
		let matches pat = Pcre.extract ~pat uri	in

		let guard f =
			try_lwt f ()
			with 
				| Unix.Unix_error (Unix.ENOENT, _, _) ->
					Generator.return_generic_error Code.Not_Found
				| _ -> 
					Generator.return_generic_error Code.Internal_server_error
		in

		let rec handle = function
			| [] -> handle_404 request [||]
			| (url_regexp, handler) :: tl ->
				try_lwt let args = matches url_regexp in
							guard (fun () -> handler request args)
	            with Not_found ->
					handle tl
		in
			Lwt_io.printlf "URI: %s" uri >>= 
				fun () -> handle handlers

	let make handlers not_found =
		dispatch handlers not_found

end

let run inbound_address outbound_address docroot =
	let handlers =  [
		("^/_layouts/", Generator.not_found);
		("^/try-mustache.html", Generator.serve_layout_file docroot);
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
