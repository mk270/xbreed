
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

open Yojson.Safe
open Lwt
open Pcre

exception Frontmatter_not_found

let json_of_s s = `Assoc [ ("content", `String s) ]

let yaml_frontmatter s =
	let kvp line =
		let rex = Pcre.regexp ": ?" in
			match (Pcre.split ~max:2 ~rex line) with
				| [k; v] -> (k, v)
				| _ -> raise Frontmatter_not_found
	in
	let rex = Pcre.regexp  ~flags:[`MULTILINE] "^---\n" in
	let max = 3 in
	let sections s = Pcre.split ~max ~rex s in
	let yaml_assoc = function
		| [""; yaml; rest] ->
			let lines = Str.split (Str.regexp "\n") yaml in
			let kvps = List.map kvp lines in
				(kvps, rest)
		| _ -> raise Frontmatter_not_found
	in
	let kvps, rest = yaml_assoc (sections s) in
	let layout = List.assoc "layout" kvps in
		(layout, rest)

let json_string_of_file filename =
	Util.file_contents filename >|=
	json_of_s >|=
	Yojson.Safe.to_string

let interpolate replacement template =
	let needle = "<!-- # MAGIC # -->" in
	let replacement = "params = " ^ replacement ^ ";\n" in
		Str.replace_first (Str.regexp needle) replacement template

let _make_layout template_name docroot filename =
	let template_name = Util.path_join [docroot; "_layouts"; template_name] in

	let modify_template json_string =
		Util.file_contents template_name >|=
		interpolate json_string
	in
		json_string_of_file filename >>=
		modify_template

let make_layout docroot filename =
	_make_layout "layout.html" docroot filename

let interpolate_contents contents template =
	let rex = Pcre.regexp "{{ ?contents ?}}" in
		Pcre.replace ~rex ~templ:contents template

let apply_template docroot (layout, body) = 
	let layout_file = layout ^ ".html" in
	let rendered_body = Util.html_of_markdown body in
	let template_name = Util.path_join [docroot; "_layouts"; layout_file] in
		Util.file_contents template_name >|=
			interpolate_contents rendered_body

let make_layout docroot filename =
	Util.file_contents filename >|=
	yaml_frontmatter >>=
	apply_template docroot
				
