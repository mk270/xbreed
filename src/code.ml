
type meth = GET | POST | HEAD

let string_of_method = function
  | GET -> "GET"
  | POST -> "POST"
  | HEAD -> "HEAD"

let method_of_string = function
  | "GET" -> GET
  | "POST" -> POST
  | "HEAD" -> HEAD
  | _ -> raise Not_found

type status =
  | OK
  | Moved_permanently
  | Found
  | See_other
  | Temporary_redirect
  | Bad_request
  | Unauthorized
  | Forbidden
  | Not_Found
  | Method_not_allowed
  | Request_time_out
  | Internal_server_error
  | Not_implemented
  | Bad_gateway
  | Service_unavailable
  | Gateway_time_out


let status_of_int = function
  | 200 -> OK
  | 301 -> Moved_permanently
  | 302 -> Found
  | 303 -> See_other
  | 307 -> Temporary_redirect
  | 400 -> Bad_request
  | 401 -> Unauthorized
  | 403 -> Forbidden
  | 404 -> Not_Found
  | 405 -> Method_not_allowed
  | 408 -> Request_time_out
  | 500 -> Internal_server_error
  | 501 -> Not_implemented
  | 502 -> Bad_gateway
  | 503 -> Service_unavailable
  | 504 -> Gateway_time_out
  | _ -> raise Not_found

let int_of_status = function
  | OK -> 200
  | Moved_permanently -> 301
  | Found -> 302
  | See_other -> 303
  | Temporary_redirect -> 307
  | Bad_request -> 400
  | Unauthorized -> 401
  | Forbidden -> 403
  | Not_Found -> 404
  | Method_not_allowed -> 405
  | Request_time_out -> 408
  | Internal_server_error -> 500
  | Not_implemented -> 501
  | Bad_gateway -> 502
  | Service_unavailable -> 503
  | Gateway_time_out -> 504

let string_of_status = function
  | OK -> "OK"
  | Moved_permanently -> "Moved Permanently"
  | Found -> "Found"
  | See_other -> "See Other"
  | Temporary_redirect -> "Temporary Redirect"
  | Bad_request -> "Bad Request"
  | Unauthorized -> "Unauthorized"
  | Forbidden -> "Forbidden"
  | Not_Found -> "Not Found"
  | Method_not_allowed -> "Method Not Allowed"
  | Request_time_out -> "Request Time Out"
  | Internal_server_error -> "Internal Server Error"
  | Not_implemented -> "Not Implemented"
  | Bad_gateway -> "Bad Gateway"
  | Service_unavailable -> "Service Unavailable"
  | Gateway_time_out -> "Gateway Timeout"

let body_string_of_status s =
	(string_of_int (int_of_status s)) ^ " " ^ (string_of_status s)

let string_of_int = function
  | 200 -> "OK"
  | 301 -> "Moved permanently"
  | 302 -> "Found"
  | 303 -> "See other"
  | 307 -> "Temporary redirect"
  | 400 -> "Bad request"
  | 401 -> "Unauthorized"
  | 403 -> "Forbidden"
  | 404 -> "Not found"
  | 405 -> "Method not allowed"
  | 408 -> "Request time out"
  | 500 -> "Internal server error"
  | 501 -> "Not implemented"
  | 502 -> "Bad gateway"
  | 503 -> "Service unavailable"
  | 504 -> "Gateway time out"
  | n -> string_of_int n
