open Core.Std
module Re2 = Re2.Std.Re2


type t = (int * string) String.Map.t
type t' = (int * string) list String.Map.t
let empty = String.Map.empty
let to_list t = Map.to_alist t 

type accum = 
{
	out_going: t;
	in_going:  t';
	index:      int;
	nodes:		(int * string) list;
}


let read file = In_channel.read_lines file

let python_split x =
	String.split_on_chars ~on:[ ' ' ; '\t' ; '\n' ; '\r' ] x
	|> List.filter ~f:(fun x -> x <> "")

let del_empty_line lines = 
	List.filter ~f:(fun x -> x <> "") lines  

let remove_comment lines =
	List.filter ~f:(fun line -> let split = python_split line in (List.nth_exn split 0) <> "#") lines

(*----------------------------------- print functions -----------------------------------------*)
let rec print_lines lines = 
	match lines with
	| [] -> ()
	| h::t -> print_endline h; print_lines t

let rec print_out_going lines = 
	match lines with
	| [] -> ()
	| (w,(id,gate_type))::t -> 
		print_string w; print_string " ";
		print_int id; print_string " "; print_string gate_type; print_endline ""; 
		print_out_going t

let rec print_ids ids = 
	match ids with
	| [] -> ()
	| (id,gate_type)::t -> 
		print_string "id:"; print_int id; print_string ","; print_string "gate type:";
		print_string gate_type; print_string ","; print_string " "; print_ids t

let rec print_in_going lines =
	match lines with
	| [] -> ()
	| (w,ids)::t ->
		print_string w; print_string " "; print_ids ids;
		print_endline ""; print_in_going t

let rec print_nodes nodes = 
	match nodes with
	| [] -> ()
	| (id,node_type)::t ->
		print_endline "\tnode";
		print_endline "\t[";
		print_endline ("\t id "^(string_of_int id));
		print_endline ("\t node_type " ^ "\"" ^ node_type ^ "\"");
		print_endline "\t]";
		print_nodes t

let rec print_keys keys =
	match keys with
	| [] -> ()
	| h::t-> print_string h; print_endline " "; print_keys t

let rec print_edges_inner source destinations =
	match destinations with
	| [] -> ()
	| (id,node_type)::t ->
		print_endline "\tedge";
		print_endline "\t[";
		print_endline ("\t source "^(string_of_int source));
		print_endline ("\t target "^(string_of_int id));
		print_endline "\t]";
		print_edges_inner source t

let rec print_edges out_map in_map keys =
	match keys with
	| [] -> ()
	| k::t -> 
		let source = begin match Map.find out_map k with
			| Some (id, node_type)-> id 
			| None -> failwith "ERROR @ print_edges #1"
		end
		in
		let destinations = begin match Map.find in_map k with
			| Some nodes_list -> nodes_list
			| None -> failwith "ERROR @ print_edges #2"
			
		end
		in
		print_edges_inner source destinations;
		print_edges out_map in_map t


(*-----------------------------------------------------------------------------------------*)

let input accum split = 
	let wire_list = String.split_on_chars ~on:[')'] (List.nth_exn split 1) in
	let wire = (List.nth_exn wire_list 0) in 
	let out =  Map.add accum.out_going ~key:wire ~data:(accum.index, List.nth_exn split 0) in
	let nodes = (accum.index, "INPUT")::accum.nodes in
		{out_going = out; in_going = accum.in_going; index = accum.index + 1; nodes = nodes}

let output accum split = 
	let wire_list = String.split_on_chars ~on:[')'] (List.nth_exn split 1) in
	let wire = (List.nth_exn wire_list 0) in 
	let id_list = 
		match Map.find accum.in_going wire with
		| None -> [(accum.index, List.nth_exn split 0)]
		| Some l -> (accum.index, List.nth_exn split 0)::l
	in
	let inward =  Map.add accum.in_going ~key:wire ~data:id_list in
	let nodes =  (accum.index, "OUTPUT")::accum.nodes in
		{out_going = accum.out_going; in_going = inward; index = accum.index + 1; nodes = nodes}

let out_going_update out_going wire index node_type =
	match node_type with
	| Some v -> Map.add out_going ~key:wire ~data:(index, v)
	| _ -> failwith "ERROR @ out_going_update."

let rec in_going_update in_going sub_list index node_type = 
	begin match node_type with
	| Some v -> 
		begin match sub_list with
		| [] -> in_going
		| Some h ::t ->  
			let id_list = 
				match Map.find in_going h with
				| None -> [(index, v)]
				| Some l -> (index, v)::l
			in
			let new_in_going = Map.add in_going ~key:h ~data:id_list in 
			in_going_update new_in_going t index node_type
		end
	| _ -> failwith "ERROR @ in_going_update."
	end

let gate_inner accum line re = 
	let result = Re2.find_submatches_exn re line in

	let out_going = match result.(1) with
		| Some wire -> out_going_update accum.out_going wire accum.index (result.(2))
		| _ -> failwith "ERROR @ outgoing." in

	let in_going = 
		let len = Array.length result in  
		let sub_array = Array.sub result 3 (len - 3) in
		in_going_update accum.in_going (Array.to_list sub_array) accum.index (result.(2)) in

	let index =  accum.index + 1 in

	let nodes = match result.(2) with
		| Some node_type -> (accum.index, node_type)::accum.nodes
		| _ -> failwith "ERROR @ nodes of gate_inner."
	in

		{out_going;
		 in_going;
		 index;
		 nodes;
		}

let gate accum line =
	let re_1in = Re2.create_exn "([a-zA-Z0-9_.]+) = ([a-zA-Z0-9_.]+)\\(([a-zA-Z0-9_.]+)\\)" in
	let re_2in = Re2.create_exn "([a-zA-Z0-9_.]+) = ([a-zA-Z0-9_.]+)\\(([a-zA-Z0-9_.]+), ([a-zA-Z0-9_.]+)\\)" in
	let re_3in = Re2.create_exn "([a-zA-Z0-9_.]+) = ([a-zA-Z0-9_.]+)\\(([a-zA-Z0-9_.]+), ([a-zA-Z0-9_.]+), ([a-zA-Z0-9_.]+)\\)" in
	let re_4in = Re2.create_exn "([a-zA-Z0-9_.]+) = ([a-zA-Z0-9_.]+)\\(([a-zA-Z0-9_.]+), ([a-zA-Z0-9_.]+), ([a-zA-Z0-9_.]+), ([a-zA-Z0-9_.]+)\\)" in

	try
		gate_inner accum line re_1in
	with
	| _ ->
		try
			gate_inner accum line re_2in
		with
		| _ ->
			try
				gate_inner accum line re_3in
			with
			| _ ->
				try
					gate_inner accum line re_4in
				with
				| _ -> failwith "Unknown. Here!"


let iterate lines = 
	let accum = {out_going = empty; in_going = empty; index = 0; nodes = []} in
	List.fold ~init:accum ~f:
	(fun accum line ->
		let split = String.split_on_chars ~on:['('] line in
		let first_e = List.nth_exn split 0 in 
			match first_e with
			| "INPUT" -> input accum split
			| "OUTPUT" -> output accum split
			| _ -> gate accum line
	) lines

let accum_print accum =
	let nodes = accum.nodes in 
	let keys_list = Map.keys accum.out_going in
	print_endline "graph"; print_endline "["; print_endline "\tdirected 1";
	print_nodes nodes;
	print_edges accum.out_going accum.in_going keys_list;
	print_endline "]"


let main () =
	Sys.argv.(1)
	|> read
	|> del_empty_line
	|> remove_comment
	|> iterate
	|> accum_print	
	

let () = main ()


