open Fifo_poly_tail

let rec divide = function
	|Nil -> Nil, Nil, 0
	|Single x -> Single x, Nil, 1
	|ft ->
		let x, ft_inter1 = extractleft ft in
		let y, ft_inter2 = extractleft ft_inter1 in
		let ft1, ft2, n = divide ft_inter2 in insertleft x ft1, insertleft y ft2, (n+2)
	
let omega inv n j = Complex.polar 1. ((-.2. *. Float.pi *. inv *. float_of_int j) /. float_of_int n)


let rec zip inv n j = function
	|Nil, Nil -> Nil
	|_, Nil | Nil, _ -> failwith "fusion_fifo"
	|ft1, ft2 ->
		let h1, ft_inter1 = extractleft ft1 in
		let h2, ft_inter2 = extractleft ft2 in
		insertleft (h1, Complex.mul h2 (omega inv n j)) (zip inv n (j+1) (ft_inter1, ft_inter2))
		(* On peut mieux faire... Il faut écrire une fonction map (facile) et une fonction zip (délicat) *)

let add_c (x, y) = Complex.add x y
let sub_c (x, y) = Complex.sub x y
	
let fusion inv n l1 l2 =
	let l = zip inv n 0 (l1,l2) in
	let r1 = map add_c l in
	let r2 = map sub_c l in
	concat r1 r2


let rec raw_fft inv = function
	|Nil -> Nil
	|Single x -> Single x
	|l ->
		let l1, l2, n = divide l in
		let r1 = raw_fft inv l1 in
		let r2 = raw_fft inv l2 in
		fusion inv n r1 r2

let fft l = fingertree2list (raw_fft 1. (list2fingertree l))

let ifft l =
	let n = List.length l in
	let ft = map (fun x -> Complex.div x {re = float_of_int n; im = 0.}) (raw_fft (-.1.) (list2fingertree l)) in
	fingertree2list ft
