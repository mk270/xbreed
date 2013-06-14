
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
   $(\".auto-markdown\").each(function() {
      $(this).html(marked($(this).html()));
   });
 </script>
</html>"

let wrap s = 
	header ^ s ^ footer
