
(*
  XBreed, a Mongrel2 handler for OCaml

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

(* this code isn't even supposed to be alpha quality - it's just a hack ATM *)

let header = "
<html>
 <head>
  <script src=\"marked.js\" type=\"text/javascript\"></script>
  <script src=\"http://code.jquery.com/jquery-latest.js\" ></script>
 </head>
 <body>
   <div class=\"auto-markdown\">
"

let footer = "
</div>
 </body>
 <script>
   $ = jQuery;
   $().ready(function() {
     $(\".auto-markdown\").each(function() {
        $(this).html(marked($(this).html()));
     });
   });
 </script>
</html>"

let wrap s = 
	header ^ s ^ footer
