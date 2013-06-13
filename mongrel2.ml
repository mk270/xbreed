
(*
  OCaml-Mongrel2-Handler, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Printf
open Lwt
open Str
open Yojson.Safe

type mongrel2_request = {
	m2req_uuid : string;
	m2req_conn_id : int;
	m2req_path : string;
	m2req_headers : (string * string) list;
	m2req_body : string;
}

type mongrel2_response = {
	m2resp_body : string;
	m2resp_code : int;
	m2resp_status : string;
	m2resp_headers : (string * string) list;
}

module Netstring = struct
	let by_colon = Str.regexp ":"

	let parse ns =
		let parse_well_formed len_s rest =
			let len = int_of_string len_s in
				assert (rest.[len] = ',');
				(String.sub rest 0 len,
				 String.sub rest (len + 1) (String.length rest - len - 1))
		in
		let words = Str.bounded_split by_colon ns 2 in
			match words with
				| [ len_s; rest; ] -> parse_well_formed len_s rest
				| _ -> assert false
end

module Wire_Format : sig
	val parse_request : string -> mongrel2_request
	val make_response : mongrel2_request -> string -> string
end = struct
	let by_space = Str.regexp " "

	let parse_headers hh =
		let get_string = function
			| key, `String s -> key, s
			| _ -> assert false
		in

		let tmp = Yojson.Safe.from_string hh in
			match tmp with
				| `Assoc kvps -> List.map get_string kvps
				| _ -> assert false

	let parse_request resp =
		let parse_valid_payload uuid conn_id path rest =
			let conn_id = int_of_string conn_id in
			let headers, rest' = Netstring.parse rest in
			let body, _ = Netstring.parse rest' in
			let headers = parse_headers headers in
				{
					m2req_uuid = uuid;
					m2req_conn_id = conn_id;
					m2req_path = path;
					m2req_headers = headers;
					m2req_body = body;
				}
		in
		let words = Str.bounded_split by_space resp 4 in
			match words with
				| [uuid; conn_id; path; rest] ->
					parse_valid_payload uuid conn_id path rest
				| _ -> assert false

	let send uuid conn_id msg =
		let len_s = String.length conn_id in
		let header = Printf.sprintf "%s %d:%s," uuid len_s conn_id in
			header ^ " " ^ msg
				
	let make_response request data =
		let uuid = request.m2req_uuid in
		let idents = [request.m2req_conn_id] in
		let idents = List.map string_of_int idents in
			send uuid (String.concat " " idents) data
end

module HTTP_Response : sig
	val make : mongrel2_response -> string
end = struct
	let make response =
		let body = response.m2resp_body in
		let code = response.m2resp_code in
		let status = response.m2resp_status in
		let headers = response.m2resp_headers in
		let content_length = String.length body in
		let headers = ("Content-Length", string_of_int content_length) :: headers
		in
		let sep_by_colon kv =
			let k, v = kv in
				k ^ ": " ^ v
		in
		let headers' = String.concat "\r\n" (List.map sep_by_colon headers) in
			sprintf "HTTP/1.1 %d %s\r\n%s\r\n\r\n%s" code status headers' body
end


let uncurry f (x, y) = f x y

let handle_reply responder request =
	let compose f g x = f (g x) in

	let hreq = Wire_Format.parse_request request in
	let maker response = response >|= HTTP_Response.make in
	let http_creator = compose maker responder in

	let payload_t = http_creator hreq in
	let pair_with_hreq payload = (hreq, payload) in
		Lwt.map pair_with_hreq payload_t >|=
		uncurry Wire_Format.make_response

let handoff sock hres =
	Lwt_zmq.Socket.send sock hres >>=
		(fun () -> Lwt_io.printlf "resp: [%s]" hres)

let handle_recv sock () =
	Lwt_zmq.Socket.recv sock

let mongrel_handler responder socket socket2 =
	let lwt_socket = Lwt_zmq.Socket.of_socket socket in
	let lwt_socket2 = Lwt_zmq.Socket.of_socket socket2 in
	let loop () =
		Lwt_io.printl "Listening" >>=
		handle_recv lwt_socket >>=
		handle_reply responder >>=
		handoff lwt_socket2
	in
		while_lwt true
			do loop ()
			done

type ('a, 'b, 'c, 'd, 'e) t = {
	mongrel2_zmp_context : ZMQ.context;
	mongrel2_inbound : 'a ZMQ.Socket.t;
	mongrel2_outbound : 'b ZMQ.Socket.t;
	mongrel2_responder : mongrel2_request -> mongrel2_response Lwt.t;
}

let fini context =
	ZMQ.Socket.close context.mongrel2_inbound;
	ZMQ.Socket.close context.mongrel2_outbound;
	ZMQ.term context.mongrel2_zmp_context

let main_loop handler socket socket2 responder =
	Lwt_main.run (handler responder socket socket2)

let _init inbound outbound f_resp =
	let z = ZMQ.init () in
	let socket = ZMQ.Socket.create z ZMQ.Socket.pull in
	let socket2 = ZMQ.Socket.create z ZMQ.Socket.pub in
		{ mongrel2_zmp_context = z;
		  mongrel2_inbound = socket;
		  mongrel2_outbound = socket2;
		  mongrel2_responder = f_resp;
		}

let init inbound outbound f_resp =
	let context = _init inbound outbound f_resp in
		ZMQ.Socket.connect context.mongrel2_inbound inbound;
		ZMQ.Socket.connect context.mongrel2_outbound outbound;
		context

let run_main_loop inbound outbound f_resp =
	let context = init inbound outbound f_resp in
		main_loop mongrel_handler context.mongrel2_inbound context.mongrel2_outbound context.mongrel2_responder;
		fini context

let run context =
	mongrel_handler context.mongrel2_responder context.mongrel2_inbound context.mongrel2_outbound
