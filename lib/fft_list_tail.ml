let divide l =
	let [@tail_mod_cons] rec aux l1 l2 n = function
		| [] -> l1, l2, n
		| [x] -> x::l1, l2, n + 1
		| x::y::t -> aux (x::l1) (y::l2) (n + 2) t in
	let l1, l2, n = aux [] [] 0 l in List.rev l1, List.rev l2, n
	
let omega inv n j = Complex.polar 1. ((-.2. *. Float.pi *. inv *. float_of_int j) /. float_of_int n)

let zip inv n l1 l2 =
	let [@tail_mod_cons] rec aux j' l =  function
		| [], [] -> l
		| _, [] | [], _ -> failwith "zip"
		| h1::t1, h2::t2 -> aux (j' + 1) ((h1, Complex.mul h2 (omega inv n j'))::l) (t1, t2) in
	aux 0 [] (l1, l2) |> List.rev


let add_c (x, y) = Complex.add x y
let sub_c (x, y) = Complex.sub x y
	
let fusion inv n l1 l2 =
	let l = zip inv n l1 l2 in
	let r1 = l |> List.rev_map add_c |> List.rev in
	let r2 = l |> List.rev_map sub_c |> List.rev in
	r1 @ r2

let rec raw_fft inv = function
	|[] -> []
	|[x] -> [x]
	|l ->
		let l1, l2, n = divide l in
		let r1 = raw_fft inv l1 in
		let r2 = raw_fft inv l2 in
		fusion inv n r1 r2

let fft = raw_fft 1.

let ifft l =
	let n = List.length l in
	List.map (fun x -> Complex.div x {re = float_of_int n; im = 0.}) (raw_fft (-.1.) l)
