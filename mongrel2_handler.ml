
(*
  OCaml-Mongrel2-Handler, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Mongrel2

let respond hreq = 
	let page_text = "<html> URI was: " ^
		(List.assoc "URI" hreq.m2req_headers) ^
		"</html>"
	in
	let headers = [("Content-type", "text/html")] in
		{
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
