
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Mongrel2
open Util
open Lwt

type handler = 
		Mongrel2.mongrel2_request -> 
		string array -> 
		Mongrel2.mongrel2_response Lwt.t

type mime_type =
	| Text_html
	| Text_plain
	| Javascript
	| Css
	| Mime_other of string

let string_of_mime_type = function
	| Text_html -> "text/html"
	| Text_plain -> "text/plain"
	| Javascript -> "application/javascript"
	| Css -> "text/css"
	| Mime_other s -> s

let generic_response body status mime_type = 
	let mime = string_of_mime_type mime_type in
	let headers = [("Content-type", mime)] in
		{
			m2resp_body = body;
			m2resp_status = status;
			m2resp_headers = headers;
		}
			
let return_generic_response body status mime_type =
	Lwt.return (generic_response body status mime_type)
			
let return_generic_error status =
	let body = Code.body_string_of_status status in
		Lwt.return (generic_response body status Text_html)
				
let unwrap_ok_text mime_type text =
	generic_response text Code.OK mime_type
				
let serve_from_file filename hreq filter mime_type =
	let page_text = file_contents filename in
		page_text >|= filter >|= unwrap_ok_text mime_type
			
let mime_type_of_extn = function
	| Some "txt" -> Text_plain
	| Some "css" -> Css
	| Some "js" -> Javascript
	| Some "html" -> Text_html
	| None -> Text_plain
	| Some s -> Mime_other s
		
let serve_file docroot request matched_args =
	let uri = uri_of_request request in
	let filename = Util.path_join [docroot; uri] in
	let ext = Util.ext_of_filename filename in
	let mime_type = mime_type_of_extn ext in
		serve_from_file filename request (fun i -> i) mime_type

let serve_md_file docroot request matched_args =
	let uri = uri_of_request request in
	let filename = Util.path_join [docroot; uri] in
		serve_from_file filename request (fun i -> html_of_markdown i) Text_html
			
let serve_layout_file docroot request matched_args =
	let uri = uri_of_request request in
	let filename = Util.path_join [docroot; uri] in 
		Layout.make_layout docroot filename >|= unwrap_ok_text Text_html
			
let not_found request matched_args =
	return_generic_response "URL Not found" Code.Not_Found Text_plain
