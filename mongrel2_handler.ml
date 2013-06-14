
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

let count = ref 0

let dump_args args =
	let row (k, v) = "  <tr><td>" ^ k ^ "</td><td>" ^ v ^ "</td></tr>" in
	let rows = List.map row args in
		"<table>\n" ^
			String.concat "\n" rows ^
		"\n</table>\n"

(*
	let page_text = "<html> URI was: " ^
		(dump_args hreq.m2req_headers) ^
		"<br>" ^ (string_of_int !count) ^
		"</html>"
	in *)

let serve_from_file filename hreq =
	let headers = [("Content-type", "text/html")] in

	let page_text =
		Lwt_io.with_file ~mode:Lwt_io.Input filename Lwt_io.read
	in

	let restructure_thingy text = {
		m2resp_body = text;
		m2resp_code = 200;
		m2resp_status = "OK";
		m2resp_headers = headers;
	}
	in
		restructure_thingy =|< page_text

let respond hreq =
	serve_from_file "/etc/services" hreq

let normal_document s =
	Lwt.return {
		m2resp_body = s;
		m2resp_code = 200;
		m2resp_status = "OK";
		m2resp_headers = [("Content-type", "text/html")];
	}

let handler1 request matched_args =	normal_document "MASH"

let handler2 request matched_args =
	respond request

let not_found request matched_args =
	let headers = [("Content-type", "text/html")] in

	Lwt.return {
		m2resp_body = "Not found";
		m2resp_code = 404;
		m2resp_status = "Not Found";
		m2resp_headers = headers;
	}

let dispatch request =
	let matches pat =
		let uri = List.assoc "URI" request.m2req_headers in
			Pcre.extract_all ~pat uri
	in

	let handlers = [
		("^/person$", handler1);
		("^/ockleon$", handler2);
	] in
		
	let rec handle = function
		| [] -> not_found request [||]
		| (url_regexp, handler) :: tl ->
			try_lwt let args = matches url_regexp in
						handler request args
			with Not_found ->
				handle tl
	in
		handle handlers
	
let () =
	let context = Mongrel2.init
		"tcp://127.0.0.1:9999" "tcp://127.0.0.1:9998" dispatch
	in
		Lwt_main.run (Mongrel2.run context);
		Mongrel2.fini context
