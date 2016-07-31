(* In order to compile this code run the following:
	ocamlfind ocamlc -linkpkg -thread -package re2  str.cma convert_Bayesian.ml -o convert
*)
module Re2 = Re2.Std.Re2
module AdjList = Map.Make(String)

(* ----------- Reading input file -----------*)
let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

(* Shorten a list of string s.t. the first element starts with edge info. *)
let rec skip_to_edges str_list : string list =
	match str_list with
	| [] -> failwith "This should not be reached. Couldn't find the word 'probability'"
	| h::t -> let words = Str.split (Str.regexp "[ ]+") h in
			  let first = List.hd words in 
				if first = "probability" then
					h::t
				else
					skip_to_edges t

let add_to_map adj_map str = 
(* Update an adjacency list (Map) by adding new edge. If the edge is new, add an empty list. *)
	
	let str_split = Str.split (Str.regexp "[ |,]+") str in 
	if List.length str_split = 1 then 
	(* Only source node *)
	(
		let node = List.hd str_split in 
		try let found = AdjList.find node adj_map in
			adj_map
		with
		| _ -> AdjList.add node [] adj_map
	)
	else
	(

		let srcs = List.tl str_split in
		let dst = List.hd str_split in 
		let rec add_multiple map src_list = 
			match src_list with
			| [] -> map
			| h::t -> 
				(
					try let found = AdjList.find h map in
						let updated = AdjList.add h (found@[dst]) map in
						add_multiple updated t
					with
					| _ -> let updated = AdjList.add h [dst] map in add_multiple updated t

				)
		in
	
		add_multiple adj_map srcs
	)



let rec construct_adj regex str_list adj_map =
	match str_list with
	| [] -> adj_map
	| h::t -> 
		try 
		(let result = Re2.find_submatches_exn regex h in 
			match result.(1) with
			| Some str -> 
				let updated = add_to_map adj_map str in
				construct_adj regex t updated

			| _ -> failwith "This should not be reached! in construct_adj"
		)	
		with | _ -> 
		construct_adj regex t adj_map

		

let rec print_edgelist src dsts = 
	match dsts with
	| [] -> ()
	| h::t -> Printf.printf "%s,%s\n" src h;
			  print_edgelist src t



 let main () =
  	let str_list = Sys.argv.(1) |> read_lines |> skip_to_edges in 
  	let adj_map = AdjList.empty in 
  	let regex = Re2.create_exn "probability \\((.*)\\) {" in
  	let adj = construct_adj regex str_list adj_map in
  	AdjList.iter print_edgelist adj

let () = main ()
