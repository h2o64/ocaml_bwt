(* Circulate a list *)
let circulate l =
	let last = ref (List.hd l) in
	let rec circulate_aux l = match l with
		| [] -> failwith "cirulate: You shouldn't have come here!"
		| [a] -> 
				last := a;
				[]
		| h::t -> h::(circulate_aux t) in
	let tmp = circulate_aux l in
	!last::tmp;;

(* List from a string *)
let list_of_string s =
	let n = String.length s in
	let rec list_of_string_aux i =
		if i = n then []
		else s.[i]::(list_of_string_aux (i+1)) in
	list_of_string_aux 0;;

(* String from list *)
let string_of_list l =
	let ret = String.make (List.length l) 'a' in
	let rec string_of_list_aux q i = match q with
		| [] -> ()
		| h::t ->
				(ret.[i] <- h);
				string_of_list_aux t (i+1) in
	string_of_list_aux l 0;
	ret;;

(* Get cirulating matrix *)
let circulate_matrix s =
	let word = list_of_string s in
	let rec circulate_matrix_aux l m = match l with
		| [] -> failwith "cirulate: You shouldn't have come here!"
		| [a] -> [circulate m]
		| h::t ->
				let n = circulate m in
				n::(circulate_matrix_aux t n) in
	circulate_matrix_aux word word;;

(* Display matrix correctly *)
let display_matrix m =
	(* Display a line *)
	let rec display_line l = match l with
		| [] -> print_string "\n"
		| h::t ->
			print_string " ";
			print_char h;
			display_line t in
	(* Display actual matrix *)
	let rec display_matrix_aux q = match q with
		| [] -> ()
		| h::t ->
			display_line h;
			display_matrix_aux t in
	display_matrix_aux m;;

(* Get sorted matrix *)
let sorted_matrix s =
	let m = circulate_matrix s in
	List.fast_sort compare m;;

(* Get BWT Code *)
let getBWT s =
	let matrix = sorted_matrix (String.concat "" [s;(String.make 1 (char_of_int 255))]) in
	(* Get last element of a list *)
	let rec last_list l = match l with
		| [] -> failwith "Error empty list"
		| [a] -> a
		| h::t -> last_list t in
	let rec getBWT_aux q = match q with
		| [] -> []
		| h::t -> (last_list h)::(getBWT_aux t) in
	string_of_list (getBWT_aux matrix);;

(* Get the first column *)
let first_column key =
	let key_l = list_of_string key in
	List.fast_sort compare key_l;;

(* Get second column *)
let getSecondColumn last first =
	(* Create the previous characters array *)
	let prev = Array.make 255 (Queue.create ()) in
	(* Reset all queues *)
	for i = 0 to 254 do
		prev.(i) <- Queue.create ();
	done;
	(* Fill 'prev' array *)
	let rec fill a b = match (a,b) with
		| ([],[]) -> ()
		| ([],_) -> failwith "Something's wrong here ..."
		| (_,[]) -> failwith "Something's wrong here ..."
		| (h1::t1,h2::t2) ->
			let last_letter = (int_of_char h1) in
			let next_letter = (int_of_char h2) in
			Queue.add next_letter (prev.(last_letter));
			fill t1 t2 in
	fill last first;
	(* Get the new colum *)
	let rec get_new l = match l with
		| [] -> []
		| h::t ->
			let cur_letter = int_of_char h in
			(char_of_int (Queue.pop prev.(cur_letter)))::(get_new t) in
	get_new first;;

(* Find index of a searched string in a sorted string array *)
let getIndex s arr =
	let cur = ref 0 in
	while not (arr.(!cur) = s) do
		cur := !cur + 1;
	done;!cur;;

(* Make a word with columns *)
let makeWord l i =
	let ret = String.make (List.length l) 'a' in
	(* Get i-th element of the list *)
	let rec getIEl p k = match p with
		| [] -> failwith "Empty list";
		| h::t -> if k = i then h else getIEl t (k+1) in
	(* Fill the string *)
	let rec makeWord_aux q n = match q with
		| [] -> ()
		| h::t ->
				(ret.[n] <- (getIEl h 0));
				makeWord_aux t (n+1) in
	makeWord_aux l 0;
	ret;;

(* Fill the first non-empty index of an array *)
let getSmartIndex target arr =
	let n = Array.length arr in
	let i = ref 0 in
	let found = ref false in
	while (not !found) && (!i < n) do
		if arr.(!i) = target then found := true;
		if arr.(!i) = "" then found := true;
		i := !i + 1;
	done;(!i-1);;

(* Find all columns based on previous *)
(* l = [col1,col2,...,col(n-1)] *)
let getColumn l col_n last_col =
	let n = List.length last_col in
	(* Create previous words array *)
	let prev_w = Array.make n "" in
	let prev_c = Array.make n (Queue.create ()) in
	(* Reset all queues *)
	for i = 0 to (n-1) do
		prev_c.(i) <- Queue.create ();
	done;
	(* Fill 'prev' array *)
	let wip = ((last_col)::l) in
	let rec fill cur_last cur = match cur_last with
		| [] -> ()
		| h::t ->			
			(* Get the prefix of the current line *)
			let prefix = makeWord wip cur in
			(* Get the smart index *)
			let ind = getSmartIndex prefix prev_w in
			(* prev.(cur) corresponds to the current prefix *)
			prev_w.(ind) <- prefix;
			(* When this prefix is matched, the next letter should be *)
			Queue.add h prev_c.(ind);
			(* Continue *)
			fill t (cur+1) in
	fill col_n 0;
	(* Create new column *)
	let wip_create = (l@[col_n]) in
	let rec makeCol q cur = match q with
		| [] -> []
		| _::t ->
			(* Get the prefix *)
			let prefix = makeWord wip_create cur in
			(* Identify the matching index *)
			let ind = getIndex prefix prev_w in
			(* Get next character *)
			let car = Queue.pop prev_c.(ind) in
			car::(makeCol t (cur+1)) in
	makeCol col_n 0;;

(* Reverse the BWT *)
let reverseBWT s =
	let last = list_of_string s in
	let c1 = first_column s in
	(* Make it recursive *)
	let rec reverseBWT_aux l last_found cur = match cur with
		| [] -> failwith "Error empty word";
		| [a] -> (l@[last_found])
		| h::t ->
			let new_column = getColumn l last_found last in
			reverseBWT_aux (l@[last_found]) new_column t in
	(* Get last element of a list *)
	let rec last_list l = match l with
		| [] -> failwith "Error empty list"
		| [a] -> a
		| h::t -> last_list t in
	let rec answer q = match q with
		| [] -> []
		| h::t -> (last_list h)::(answer t) in
	let a::b = (reverseBWT_aux [] c1 last) in
	string_of_list (answer b);;

(* Optimised reversing process *)
let reverseBWT_opt s =
	let key = Array.of_list (list_of_string s) in
	let sorted_key = Array.of_list (first_column s) in
	let n = String.length s in
	(* Find first letter *)
	let cur = ref 0 in
	while not (key.(!cur) = '\255') do
		cur := !cur + 1;
	done;
	(* Append the first letter *)
	let ret = ref [sorted_key.(!cur)] in
	for k = 0 to (n-2) do
		let goal_letter = sorted_key.(!cur) in
		(* Get the occurence number of it in key *)
		let occ = ref 0 in
		for i = 0 to !cur do
			if sorted_key.(i) = goal_letter then occ := !occ + 1;
		done;
		(* Search for this letter in the sorted list *)
		let j = ref 0 in
		let found = ref 0 in
		while (not (!found = !occ)) do
			if (key.(!j) = goal_letter) then found := !found + 1;
			j := !j + 1;
		done;
		(* Append the found letter *)
		ret := sorted_key.((!j-1))::!ret;
		(* Set cur to j *)
		cur := (!j-1);
	done;
	(* Reverse a list *)
	let rev_list (h::t) =
		let rec rev_acc acc = function
			| [] -> acc
			| hd::tl -> rev_acc (hd::acc) tl in
	rev_acc [] t in
	string_of_list (rev_list !ret);;

(* Benchmark *)
let time f x =
	let start = Unix.gettimeofday ()
	in let res = f x
	in let stop = Unix.gettimeofday ()
	in let () = Printf.printf "Execution time: %fs\n%!" (stop -. start)
	in res;;

let time_val f x =
	let start = Unix.gettimeofday ()
	in let res = f x
	in let stop = Unix.gettimeofday ()
	in let () = ()
	in res;(stop -. start);;

(* Benchmark a text *)
let benchmark text =
	let a = ref 0. in
	let b = ref 0. in
	let i = ref 10 in
	while ((!b < 12.)) do
		let cur = String.sub text 0 !i in
		let tmp = getBWT cur in
		a := (time_val getBWT cur);
		b := (time_val reverseBWT tmp);
		print_float !a;
		print_string ",";
		print_float !b;
		print_string "\n";
		i := !i + 1
	done;;
