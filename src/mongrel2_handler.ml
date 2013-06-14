
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

module Generator : sig 
	val serve_file : Mongrel2.mongrel2_request ->
           'a -> Mongrel2.mongrel2_response Lwt.t

	val not_found : 'a -> 'b -> Mongrel2.mongrel2_response Lwt.t

end = struct
	let generic_response body status =
		{
			m2resp_body = body;
			m2resp_code = Code.int_of_status status;
			m2resp_status = Code.string_of_status status;
			m2resp_headers = [("Content-type", "text/html")];
		}

	let return_generic_response body status =
		Lwt.return (generic_response body status)

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
			return_generic_response "File not found" Code.Not_Found
		| _ -> return_generic_response "Internal server error" Code.Internal_server_error

	let normal_document s = return_generic_response s Code.OK

	let serve_file request matched_args =
		let uri = List.assoc "URI" request.m2req_headers in
		let filename = "." ^ uri in
			serve_from_file filename request

	let not_found request matched_args =
		return_generic_response "Not found" Code.Not_Found

end

module Dispatcher : sig
	val make : (string *
					(Mongrel2.mongrel2_request -> string array -> 'a Lwt.t))
           list ->
           (Mongrel2.mongrel2_request -> 'b array -> 'a Lwt.t) ->
           Mongrel2.mongrel2_request -> 'a Lwt.t


end = struct

(* 	type handler = mongrel2_request -> string array -> 'a Lwt.t *)

	let dispatch handlers handle_404 request =
		let matches pat =
			let uri = List.assoc "URI" request.m2req_headers in
				Pcre.extract ~pat uri
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

	let make handlers not_found =
		dispatch handlers not_found

end

let run inbound_address outbound_address =
	let handlers =  [
		("^/", Generator.serve_file);
	] in
	let dispatcher = Dispatcher.make handlers Generator.not_found in
	let context = Mongrel2.init inbound_address outbound_address dispatcher
	in
		Lwt_main.run (Mongrel2.run context);
		Mongrel2.fini context

let () =
	let inbound_address = ref "tcp://127.0.0.1:9999" in
	let outbound_address = ref "tcp://127.0.0.1:9998" in
	let speclist = [
		("--in" , Arg.Set_string inbound_address, "TBD");
		("--out", Arg.Set_string outbound_address, "TBD");
	] in
	Arg.parse speclist (fun s -> assert false) "Usage: TBD";
		run !inbound_address !outbound_address
