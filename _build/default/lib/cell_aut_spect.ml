type field = {
  e : Complex.t array array array array; (* Champ électrique (vectoriel) *)
  b : Complex.t array array array array; (* Champ magnétique (vectoriel) *)
  mutable j : Complex.t array array array array; (* Champ vectoriel de densité de courant *)
  size : int; (* Taille de l'espace cubique *)
  dx : float (* Pas de discrétisation spatiale *)
}

let create n v dx = {
  e = Matrix.create_3D n v;
  b = Matrix.create_3D n v;
  j = Matrix.create_3D n v;
  size = n;
  dx = dx
  }

let ( *.. ) = Complex.mul
let ( /.. ) = Complex.div
let ( -.. ) = Complex.sub
let ( +.. ) = Complex.add
let ( ++ ) = Vect.add
let ( -- ) = Vect.sub
let ( ** ) = Vect.scalar
let ( *^ ) = Vect.cross_prod
let i = Complex.i

let ( *** ) f g = (fun x -> f (g x))

let norm_field s = Array.map (Array.map (Array.map (Vect.norm *** Array.map Complex.norm))) s

let update f0 f1 f2 dt' c mu' = (* f0: field at moment n-1, f1: field at moment n, f2: field at moment n+1,  *)
  let dt = Lib.complex_of_float dt' in
  let dx = Lib.complex_of_float f0.dx in
  let c2 = Lib.complex_of_float c *.. Lib.complex_of_float c in
  let mu = Lib.complex_of_float mu' in
  for x = 0 to f0.size - 1 do
    for y = 0 to f0.size - 1 do
      for z = 0 to f0.size - 1 do
        let k = [|(Lib.complex_of_int (x, 0)) /.. dx ; (Lib.complex_of_int (y, 0)) /.. dx; (Lib.complex_of_int (z, 0)) /.. dx|] in
        f2.e.(x).(y).(z) <- f0.e.(x).(y).(z) ++ (((i *.. dt *.. c2 ) ** k) *^ f1.b.(x).(y).(z)) -- ((mu *.. dt *.. c2 ) ** f1.j.(x).(y).(z));
        f2.b.(x).(y).(z) <- f0.b.(x).(y).(z) -- (((i *.. dt ) ** k) *^ f1.e.(x).(y).(z));
      done;
    done;
  done;;

let show s =
  let s1 = Matrix.map Complex.norm s in
  let s2 = (Array.map (Array.map Vect.norm) s1) in
  Matrix.show_lin s2;
