open Core.Std

type t = int String.Map.t
let empty = String.Map.empty
let to_list t = Map.to_alist t 

let read file = In_channel.read_lines file


let to_tupleList lines =
	List.map lines
		~f:(fun s -> 
				String.split_on_chars s ~on:[','] 
			|> fun char_list ->
				(List.nth_exn char_list 0, List.nth_exn char_list 1))


let  make_nodeList tuple_list =
	(* make a llist of nodes from an edge list without duplication*)
	(* create a set of nodes*)
	List.fold ~init:(Set.empty ~comparator:String.comparator)
			  ~f:(fun accum (x,y) ->
				   let accum2 = Set.add accum x in Set.add accum2 y)
	    tuple_list
	|> Set.to_list

let make_map node_list =
	let return_map,_ = 
	List.fold ~init:(empty,0)
			  ~f:(fun (map,index) node ->
			  		let new_map = Map.add map ~key:node ~data:index in 
			  		(new_map, index+1)
			  )
		node_list
	in return_map


let print_nodes map = 
	List.iter (to_list map) 
		~f:(fun (node,index) ->
			print_endline "\tnode";
			print_endline "\t[";
			print_endline ("\t id "^(string_of_int index));
			print_endline ("\t label " ^ "\"" ^ node ^ "\"");
			print_endline "\t]";
			)

let print_edges tuple_list map =
	List.iter tuple_list
		~f:(fun (s,d)->
			let s_id, d_id =  (Map.find_exn map s),(Map.find_exn map d) in
				print_endline "\tedge";
				print_endline "\t[";
				print_endline ("\t source "^(string_of_int s_id));
				print_endline ("\t target "^(string_of_int d_id));
				print_endline "\t]";
		   )


let main () =
	let tuple_list = Sys.argv.(1) |> read |> to_tupleList in
	let map = make_nodeList tuple_list |> make_map in
	print_endline "graph"; print_endline "["; print_endline "\tdirected 0";
	print_nodes map;
	print_edges tuple_list map;
	print_endline "]"


let () = main ()

	
