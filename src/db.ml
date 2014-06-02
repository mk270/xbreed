
let (>>=) = Lwt.bind

module Lwt_thread = struct
    include Lwt
    include Lwt_chan
end

module Lwt_PGOCaml = PGOCaml_generic.Make (Lwt_thread)

let concat a b = Lwt.return (a ^ b)

let string_of_optional = function
    | None -> "NULL"
    | Some str -> Printf.sprintf "%S" str

let format_row row = 
    List.map string_of_optional row |>
    String.concat "; " |>
    Lwt.return

let run_query () =
    let query = "select * from package;"
    and user = "iatidq"
    and name = "stmt1" (* nonce identifier of prepared statement *)
    in

    lwt dbh = Lwt_PGOCaml.connect ~user () in
        Lwt_PGOCaml.prepare dbh ~query ~name () >>
        Lwt_PGOCaml.execute dbh ~name ~params:[] () >>=
		Lwt_list.map_s format_row >>=	
        Lwt_list.fold_left_s concat "" >>=
        (fun res_string ->
            Lwt_PGOCaml.close dbh >>
            Lwt.return res_string)

