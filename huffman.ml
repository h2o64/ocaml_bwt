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

(* Get characters code in huffman tree *)
let getHuffmanCode t =
	let l = ref [] in
	let rec getHuffmanCode_aux tree cur =	match tree with
		| EmptyTree -> ()
		| Node(_,(num_g,g),(num_d,d)) ->
			getHuffmanCode_aux g (String.concat "" [cur;string_of_int num_g]);
			getHuffmanCode_aux d (String.concat "" [cur;string_of_int num_d]);
		| Leave(a,_) -> (l := (a,cur)::!l) in
	getHuffmanCode_aux t "";!l;;

(* Find code for a letter *)
let rec getCharacCode l c = match l with
	| [] -> failwith "Character not found!";
	| (a,code)::t -> if a = c then code else getCharacCode t c;;

(* Find caract for the code *)
let rec getCodeCharact l c = match l with
	| [] -> '\255'
	| (a,code)::t -> if code = c then a else getCodeCharact t c;;

(* Concat strings *)
let concat s1 s2 = (String.concat "" [s1;s2]);;

(* Code my string *)
let encode_huffman s =
	let t = get_huffman_tree s in
	let table = getHuffmanCode t in
	let ret = ref "" in
	for i = 0 to ((String.length s)-1) do
		ret := concat !ret (getCharacCode table s.[i]);
	done;(!ret,t);;

(* Decode Huffman code *)
let decode_huffman (code,t) =
	let table = getHuffmanCode t in
	let cur = ref "" in
	let ret = ref "" in
	for i = 0 to ((String.length code)-1) do
		(* Get current code *)
		cur := concat !cur (String.make 1 code.[i]);
		(* Search if it has a code *)
		let cur_car = getCodeCharact table !cur in
		(* Check if code exists *)
		if not (cur_car = '\255') then
			(cur := "";
			ret := concat !ret (String.make 1 cur_car));
	done;!ret;;
