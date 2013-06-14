
(*
  OCaml-Mongrel2-Handler, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Lwt
open Mongrel2
open Pcre

let generic_response body code status =
	Lwt.return {
		m2resp_body = body;
		m2resp_code = code;
		m2resp_status = status;
		m2resp_headers = [("Content-type", "text/html")];
	}

let serve_from_file filename hreq =
	let headers = [("Content-type", "text/html")] in

	let restructure_thingy text = {
		m2resp_body = text;
		m2resp_code = 200;
		m2resp_status = "OK";
		m2resp_headers = headers;
	} in

	try_lwt let page_text =
				Lwt_io.with_file ~mode:Lwt_io.Input filename Lwt_io.read
			in
				restructure_thingy =|< page_text
	with 
		| Unix.Unix_error (Unix.ENOENT, _, _) ->
			generic_response "File not found" 404 "Not Found"
		| _ -> generic_response "Internal server error" 500 "Internal Server Error"

let respond hreq = serve_from_file "/etc/services" hreq

let normal_document s = generic_response s 200 "OK"

let handler1 request matched_args =	normal_document "MASH"

let handler2 request matched_args =
	respond request

let handler3 request matched_args =
	let uri = List.assoc "URI" request.m2req_headers in
	let filename = "." ^ uri in
		serve_from_file filename request

let not_found request matched_args =
	generic_response "Not found" 404 "Not Found"

let dispatch handlers handle_404 request =
	let matches pat =
		let uri = List.assoc "URI" request.m2req_headers in
			Pcre.extract_all ~pat uri
	in
		
	let rec handle = function
		| [] -> handle_404 request [||]
		| (url_regexp, handler) :: tl ->
			try_lwt let args = matches url_regexp in
						handler request args
			with Not_found ->
				handle tl
	in
		handle handlers

let make_dispatcher handlers not_found =
	dispatch handlers not_found
	
let () =
	let handlers =  [
		("^/person$", handler1);
		("^/ockleon$", handler2);
		("^/", handler3);
	] in
	let dispatcher = make_dispatcher handlers not_found in
	let context = Mongrel2.init
		"tcp://127.0.0.1:9999" "tcp://127.0.0.1:9998" dispatcher
	in
		Lwt_main.run (Mongrel2.run context);
		Mongrel2.fini context
