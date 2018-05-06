(* Tree API *)
type 'a tree =
	| EmptyTree
	| Node of int * (int * ('a tree)) * (int * ('a tree))
	| Leave of ('a * int);;

(* Number of occurence in a string *)
let num_occ s c =
	let n = String.length s in
	let ret = ref (if (s.[0] = c) then 1 else 0) in
	for i = 1 to (n-1) do
		if (s.[i] = c) then ret := !ret + 1;
	done;!ret;; 

(* Sort nodes *)
let compare_nodes a b = match (a,b) with
	| (EmptyTree,_) -> -1
	| (_,EmptyTree) -> 1
	| (Leave(_,p1),Leave(_,p2)) -> compare p1 p2
	| (Node(p1,(_,_),(_,_)),Leave(_,p2)) -> compare p1 p2
	| (Leave(_,p1),Node(p2,(_,_),(_,_))) -> compare p1 p2
	| (Node(p1,(_,_),(_,_)),Node(p2,(_,_),(_,_))) -> compare p1 p2;;	

(* Let insert a node keeping the forest (ref) sorted *)
let insert_forest forest t =
	let rec insert_forest_aux l = match l with 
		| [] -> [t]
		| h::q ->
			if ((compare_nodes h t) > 0) then t::l
			else h::(insert_forest_aux q) in
	forest := insert_forest_aux !forest;;

(* Dump and remove first trees of a forest (ref) *)
let dump forest_r = match !forest_r with
	| [] -> failwith "Empty list"
	| _::[] -> failwith "List isn't big enough"
	| a::b::t ->
		forest_r := t;
		(a,b);;

(* Concat strings *)
let concat s1 s2 = (String.concat "" [s1;s2]);;

(* Huffman encodage *)
let get_huffman_tree s =
	(* Make all the nodes *)
	let rec makeNodesList c l =
		if c = 255 then l
		else
			(let cur = (char_of_int c) in
			let occ = (num_occ s cur) in
			if (occ > 0) then (makeNodesList (c+1) (Leave((cur,occ))::l))
			else (makeNodesList (c+1) l);); in
	(* Sort l *)
	let forest = ref (List.fast_sort compare_nodes (makeNodesList 0 [])) in
	(* Build the actual tree *)
	while (List.length !forest > 1) do
		let (a,b) = (dump forest) in
		match (a,b) with
			| (EmptyTree,_) -> failwith "Error empty tree"
			| (_,EmptyTree) -> failwith "Error empty tree"
			| (Leave(_,p1),Leave(_,p2)) ->
				let t = Node((p1+p2),(1,a),(0,b)) in
				insert_forest forest t
			| (Node(p1,(_,_),(_,_)),Leave(_,p2)) ->
				let t = Node((p1+p2),(1,a),(0,b)) in
				insert_forest forest t
			| (Leave(_,p1),Node(p2,(_,_),(_,_))) ->
				let t = Node((p1+p2),(1,a),(0,b)) in
				insert_forest forest t
			| (Node(p1,(_,_),(_,_)),Node(p2,(_,_),(_,_))) ->
				let t = Node((p1+p2),(1,a),(0,b)) in
				insert_forest forest t;
	done;(List.hd !forest);;

(* Read an encoded word *)
let read_huffman tree coded =
	let rec read_huffman_aux t w =
		let n = String.length w in
			match t with
				| EmptyTree -> ()
				| Node(_,(_,g),(_,d)) ->
					if n = 0 then
						(if w.[0] = '0' then read_huffman_aux g ""
						else read_huffman_aux d "")
					else
						(let shorten = (String.sub w 1 (n-1)) in
						if w.[0] = '0' then read_huffman_aux g shorten
						else read_huffman_aux d shorten);
				| Leave(a,_) ->
					print_char a;
					(* Reboot from root *)
					if not (n = 0) then read_huffman_aux tree w;
			in
	read_huffman_aux tree coded;;

(* Get word from huffman tree *)
let get_huffman tree word =
	let letters = Array.make 255 "" in
	(* Find code for all letters *)
	let rec get_huffman_aux t cur = match t with
		| EmptyTree -> ()
		| Node(_,(_,g),(_,d)) ->
			get_huffman_aux g (concat cur "0");
			get_huffman_aux d (concat cur "1");
		| Leave(a,_) -> letters.(int_of_char a)<-cur in
	get_huffman_aux tree "";
	(* Make the actual word *)
	let n = String.length word in
	let ret = ref "" in
	for i = 0 to (n-1) do
		ret := concat !ret letters.(int_of_char word.[i]);
	done;!ret;;
