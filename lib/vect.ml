let ( *.. ) = Complex.mul
let ( /.. ) = Complex.div
let ( -.. ) = Complex.sub
let ( +.. ) = Complex.add

let norm v =
  let s = ref 0. in
  for i = 0 to Array.length v - 1 do
    s := !s +. v.(i) *. v.(i)
  done;
  sqrt !s

let add u v = [|u.(0) +.. v.(0); u.(1) +.. v.(1); u.(2) +.. v.(2)|]

let sub u v = [|u.(0) -.. v.(0); u.(1) -.. v.(1); u.(2) -.. v.(2)|]

let scalar lambda u = [|lambda *.. u.(0); lambda *.. u.(1); lambda *.. u.(2)|]

let cross_prod u v =
  [|
    (u.(1) *.. v.(2)) -.. (u.(2) *.. v.(1));
    (u.(2) *.. v.(0)) -.. (u.(0) *.. v.(2));
    (u.(0) *.. v.(1)) -.. (u.(1) *.. v.(0));
  |]

let gen_op op u v = [|op u.(0) v.(0); op u.(1) v.(1); op u.(2) v.(2)|]
let gen_scalar op lambda u = [|op lambda u.(0); op lambda u.(1); op lambda u.(2)|]
let gen_cross_prod op1 op2 u v = 
  [|
    op2 (op1 u.(1) v.(2)) (op1 u.(2) v.(1));
    op2 (op1 u.(2) v.(0)) (op1 u.(0) v.(2));
    op2 (op1 u.(0) v.(1)) (op1 u.(1) v.(0));
  |]

