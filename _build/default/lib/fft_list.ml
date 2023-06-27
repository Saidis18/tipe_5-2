let rec divide = function
	|[] -> [], [], 0
	|[x] -> [x], [], 1
	|x::y::t -> let l1, l2, n = divide t in x::l1, y::l2, (n+2)
	
let omega inv n j = Complex.polar 1. ((-.2. *. Float.pi *. inv *. float_of_int j) /. float_of_int n)

let rec zip inv n j = function
	|[], [] -> []
	|_, [] | [], _ -> failwith "fusion"
	|h1::t1, h2::t2 -> (h1, Complex.mul h2 (omega inv n j))::zip inv n (j+1) (t1, t2)

let add_c (x, y) = Complex.add x y
let sub_c (x, y) = Complex.sub x y
	
let fusion inv n l1 l2 =
	let l = zip inv n 0 (l1,l2) in
	let r1 = List.map add_c l in
	let r2 = List.map sub_c l in
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
