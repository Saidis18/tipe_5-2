open Fifo_poly_tail

let divide ft =
	let [@tail_mod_cons] rec aux r1 r2 n = function
		| Nil -> (r1, r2, n)
		| Single x -> (insertright x r1, r2, n+1)
		| ft_bis ->
			let x, ft_inter1 = extractleft ft_bis in
			let y, ft_inter2 = extractleft ft_inter1 in
			aux (insertright x r1) (insertright y r2) (n+2) ft_inter2 in
	aux Nil Nil 0 ft

let omega inv n j = Complex.polar 1. ((-.2. *. Float.pi *. inv *. float_of_int j) /. float_of_int n)
	
let fusion inv n ft1 ft2 =
	let f i a = Complex.mul a (omega inv n i) in
	let ft3 = mapi f ft2 in
	let r1 = map2 Complex.add ft1 ft3 in
	let r2 = map2 Complex.sub ft1 ft3 in
	concat r1 r2

let rec raw_fft inv = function
	|Nil -> Nil
	|Single x -> Single x
	|l ->
		let l1, l2, n = divide l in
		let r1 = raw_fft inv l1 in
		let r2 = raw_fft inv l2 in
		fusion inv n r1 r2

let fft = raw_fft 1.

let ifft ft =
	let n = size ft in
	map (fun x -> Complex.div x {re = float_of_int n; im = 0.}) (raw_fft (-.1.) ft)
