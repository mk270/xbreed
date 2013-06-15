
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Yojson.Safe
open Lwt

let json_of_s s = `Assoc [ ("content", `String s) ]

let json_string_of_file filename =
	Util.file_contents filename >|=
	json_of_s >|=
	Yojson.Safe.to_string

let interpolate replacement template =
	let needle = "<!-- # MAGIC # -->" in
	let replacement = "params = " ^ replacement ^ ";\n" in
		Str.replace_first (Str.regexp needle) replacement template

let _make_layout template_name docroot filename =
	let template_name = docroot ^ "/_layouts/" ^ template_name in

	let modify_template json_string =
		Util.file_contents template_name >|=
		interpolate json_string
	in
		json_string_of_file filename >>=
		modify_template

let make_layout docroot filename =
	_make_layout "layout.html" docroot filename
		
