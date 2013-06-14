
let file_contents filename = 
	Lwt_io.with_file ~mode:Lwt_io.Input filename Lwt_io.read
