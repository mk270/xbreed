
(*
  OCaml-Mongrel2-Handler, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Mongrel2

let count = ref 0

let dump_args args = 
	let row (k, v) = "  <tr><td>" ^ k ^ "</td><td>" ^ v ^ "</td></tr>" in
	let rows = List.map row args in
		"<table>\n" ^
			String.concat "\n" rows ^
		"\n</table>\n"

let respond hreq =
	incr count;
	let page_text = "<html> URI was: " ^
		(dump_args hreq.m2req_headers) ^
		"<br>" ^ (string_of_int !count) ^
		"</html>"
	in
	let headers = [("Content-type", "text/html")] in
		Lwt.return {
			m2resp_body = page_text;
			m2resp_code = 200;
			m2resp_status = "OK";
			m2resp_headers = headers;
		}

let () =
	let context = Mongrel2.init
		"tcp://127.0.0.1:9999" "tcp://127.0.0.1:9998" respond
	in
		Lwt_main.run (Mongrel2.run context);
		Mongrel2.fini context
