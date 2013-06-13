
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

type http_req = {
	hr_uuid : string;
	hr_conn_id : int;
	hr_path : string;
	hr_headers : string;
	hr_body : string;
}

let by_space = Str.regexp " "
let by_colon = Str.regexp ":"

let parse_netstring ns =
	let words = Str.bounded_split by_colon ns 2 in
		match words with
			| [ len_s; rest; ] ->
				let len = int_of_string len_s in
					assert (rest.[len] = ',');
					(String.sub rest 0 len, 
					 String.sub rest (len + 1) (String.length rest - len - 1))		
			| _ -> assert false
		
let parse resp =
	let words = Str.bounded_split by_space resp 4 in
		match words with
			| [uuid; conn_id; path; rest] -> 
				let conn_id = int_of_string conn_id in
				let headers, rest' = parse_netstring rest in
				let body, _ = parse_netstring rest' in
					{
						hr_uuid = uuid;
						hr_conn_id = conn_id;
						hr_path = path;
						hr_headers = headers;
						hr_body = body;
					}
			| _ -> assert false

let http_response body code status headers =
	let content_length = String.length body in
	let headers = ("Content-Length", string_of_int content_length) :: headers in
	let headers' = String.concat "\r\n" (List.map (fun (k,v) ->
		k ^ ": " ^ v
	) headers) in
		Printf.sprintf "HTTP/1.1 %d %s\r\n%s\r\n\r\n%s" code status headers' body

let send uuid conn_id msg =
	let len_s = String.length conn_id in
	let header = Printf.sprintf "%s %d:%s," uuid len_s conn_id in
		header ^ " " ^ msg

let deliver uuid idents data = 
	let idents = List.map string_of_int idents in
		send uuid (String.concat " " idents) data

let handle_reply reply =
    let hreq = parse reply in
(*		Lwt_io.printlf "Received: %s %d" hreq.uuid hreq.conn_id >>= *)
(*			lwt () = Lwt_io.printl "Sending a request" in *)
	let page_text = "<html>oops</html>" in
	let headers = [("Content-type", "text/html")] in
	let payload =
		http_response page_text 200 "OK" headers in
		deliver hreq.hr_uuid [hreq.hr_conn_id] payload

let handoff sock hres =
	Lwt_zmq.Socket.send sock hres >>=
		(fun () -> Lwt_io.printlf "resp: [%s]" hres)

let handle_recv sock () =
	Lwt_zmq.Socket.recv sock

let () =
  let z = ZMQ.init () in
  let socket = ZMQ.Socket.create z ZMQ.Socket.pull in
  let socket2 = ZMQ.Socket.create z ZMQ.Socket.pub in
	  ZMQ.Socket.connect socket  "tcp://127.0.0.1:9999";
	  ZMQ.Socket.connect socket2 "tcp://127.0.0.1:9998";
	  Lwt_main.run (
		  let lwt_socket = Lwt_zmq.Socket.of_socket socket in
		  let lwt_socket2 = Lwt_zmq.Socket.of_socket socket2 in
			  Lwt_io.printl "Listening" >>=
			  handle_recv lwt_socket >|=
			  handle_reply >>=
			  handoff lwt_socket2
	
      );
	  ZMQ.Socket.close socket;
	  ZMQ.term z;
	  ()
