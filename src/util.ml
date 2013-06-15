
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

let file_contents filename = 
	Lwt_io.with_file ~mode:Lwt_io.Input filename Lwt_io.read

let path_join elements = String.concat "/" elements

let html_of_markdown s =
	Cow.Html.to_string (Cow.Markdown.to_html (Cow.Markdown.of_string s))
