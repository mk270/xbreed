
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

type ('a, 'b, 'c, 'd, 'e) t

type mongrel2_request = {
	m2req_uuid : string;
	m2req_conn_id : int;
	m2req_path : string;
	m2req_headers : (string * string) list;
	m2req_body : string;
}

type mongrel2_response = {
	m2resp_body : string;
	m2resp_status : Code.status;
	m2resp_headers : (string * string) list;
}

type responder = mongrel2_request -> mongrel2_response Lwt.t

val run_main_loop : string -> string -> responder -> unit

val fini : ('a, 'b, 'c, 'd, 'e) t -> unit

val run :  ('a, 'b, 'c, 'd, 'e) t -> unit Lwt.t

val init : string ->
  string -> responder -> ([> `Pull ], [> `Pub ], 'a ZMQ.Socket.t, 'b ZMQ.Socket.t, unit) t
