open Core.Std
let read file = In_channel.read_lines file

let python_split x =
	String.split_on_chars ~on:[ ' ' ; '\t' ; '\n' ; '\r' ] x
	|> List.filter ~f:(fun x -> x <> "")

let edge_print (e:string list) =
	print_endline "\tedge";
	print_endline "\t[";
	print_endline ("\t source "^(List.nth_exn e 0));
	print_endline ("\t target "^(List.nth_exn e 1));
	print_endline "\t]"

let node_print (e:string list) =
	print_endline "\tnode";
	print_endline "\t[";
	print_endline ("\t id "^(List.nth_exn e 0));
	print_endline ("\t label "^(List.nth_exn e 1));
	print_endline ("\t longitude "^(List.nth_exn e 2));
	print_endline ("\t latitude "^(List.nth_exn e 3));
	print_endline "\t]"


let rec edges lines = 
	match lines with
	|[] -> ()
	| h::t -> let split = python_split h in
		edge_print split; 
		edges t

let rec vertices lines =
	match lines with
	|[] -> ()
	|h::t ->
		match h with
		|"*Edges" -> edges t; ()
		| _ -> let split = python_split h in 
			node_print split; 
			vertices t

let rec network lines = 
	match lines with
	| [] -> ()
	| h::t ->
		match h with
		| "*Network" -> print_endline "graph"; print_endline "["; print_endline "\tdirected 0" ;network t
		| _ -> vertices t; print_endline "]"

let main () =
	Sys.argv.(1)
	|> read
	|> network
	

let () = main ()