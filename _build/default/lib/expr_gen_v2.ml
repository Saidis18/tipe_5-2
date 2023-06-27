type scalar_expr = [
  | `Nul
  | `Unk of int (* Inconnues: Fonctions scalaires; Le paramètre est uniquement un identifiant *)
  | `Const of float
  | `Fun of float -> float
  | `D of int * scalar_expr (* Dérivées partielles: Indices: Temps: 0; x: 1; y: 2; z: 3 *)
  | `Sum of scalar_expr * scalar_expr
  | `Diff of scalar_expr * scalar_expr
  | `Prod of scalar_expr * scalar_expr
]

type expr = [
  | `Vect of scalar_expr array (* Indices: x:0; y: 1; z: 2 *)
  (* Il ne faut plus vérifier la cohérence des dimensions (Cf. v1) *)
  | `D of int * expr (* Dérivées partielles: Indices: Temps: 0; x: 1; y: 2; z: 3 *)
  | `Sum of expr * expr
  | `Diff of expr * expr
  | `Prod of scalar_expr * expr
]

exception Dimension

let rec dimension_image e' = match e' with
  | `Nul
  | `Unk _
  | `Const _
  | `Fun _ -> 1
  | `Vect t -> Array.length t
  | `D(_, e) -> dimension_image e
  | `Sum(e1, e2)
  | `Diff(e1, e2) ->
    let d1 = dimension_image e1 in
    let d2 = dimension_image e2 in
    if d1 = d2 then
      d1
    else
      raise Dimension
  | `Prod(_, e) -> dimension_image e

let rec dimension_domain e' = match e' with
  | `Nul
  | `Unk _
  | `Const _
  | `Fun _ -> 1
  | `Vect t -> Array.fold_left (fun d x -> max d (dimension_domain x)) 0 t
  | `D(i, e) -> max (i+1) (dimension_domain e)
  | `Sum(e1, e2)
  | `Diff(e1, e2)
  | `Prod(e1, e2) -> max (dimension_domain e1) (dimension_domain e2)

let rec diff_class = function
  (* What differentiability class should the unknown be for the Schwarz theorem to be applicable (at least) *)
  | `Nul
  | `Unk _
  | `Const _
  | `Fun _ -> 0
  | `Vect t -> Array.fold_left (fun d e -> max d (diff_class e)) 0 t
  | `D (_, e) -> 1 + diff_class e
  | `Sum(e1, e2)
  | `Diff(e1, e2)
  | `Prod(e1, e2)-> max (diff_class e1) (diff_class e2)

let rec max_index_unk = function
  | `Unk i -> i
  | `Nul
  | `Const _
  | `Fun _ -> -1
  | `Vect t -> Array.fold_left (fun m e -> max m (max_index_unk e)) (-1) t
  | `D(_, e) -> max_index_unk e
  | `Sum(e1, e2)
  | `Diff(e1, e2)
  | `Prod(e1, e2) -> max (max_index_unk e1) (max_index_unk e2)

let rec extract_vect: expr -> scalar_expr array = function
  | `Vect t ->  t
  | `D (i, `Vect t) -> Array.map (fun x -> `D(i, x)) t
  | `D(i, e) -> extract_vect (`D(i, `Vect (extract_vect e)))
  | `Sum(`Vect t1, `Vect t2) -> Array.map2 (fun x y -> `Sum(x, y)) t1 t2
  | `Sum (e1, e2) -> extract_vect (`Sum(`Vect (extract_vect e1), `Vect (extract_vect e2)))
  | `Diff(`Vect t1, `Vect t2) -> Array.map2 (fun x y -> `Diff(x, y)) t1 t2
  | `Diff (e1, e2) -> extract_vect (`Diff(`Vect (extract_vect e1), `Vect (extract_vect e2)))
  | `Prod(s, `Vect t) -> Array.map (fun x -> `Prod(s, x)) t
  | `Prod(s, e) -> extract_vect (`Prod(s, `Vect (extract_vect e)))

let scalar (e1: expr) (e2: expr) :scalar_expr =
  let t1 = extract_vect e1 in
  let t2 = extract_vect e2 in
  try Array.fold_left (fun x y -> `Sum(x, y)) `Nul (Array.map2 (fun x y -> `Prod(x, y)) t1 t2) with
    Invalid_argument _ -> raise Dimension

let cross (e1: expr) (e2: expr): expr =
  let t1 = extract_vect e1 in
  let t2 = extract_vect e2 in
  `Vect [|
    `Diff(`Prod(t1.(1), t2.(2)), `Prod(t1.(2), t2.(1)));
    `Diff(`Prod(t1.(2), t2.(0)), `Prod(t1.(0), t2.(2)));
    `Diff(`Prod(t1.(0), t2.(1)), `Prod(t1.(1), t2.(0)))
  |]

let grad (e: scalar_expr) : expr = `Vect [|`D(1, e); `D(2, e); `D(3, e)|]

let rec div = function
  | `Vect t -> `Sum( `Sum ( `D(1, t.(1-1)), `D(2, t.(2-1)) ), `D(3, t.(3-1)))
  | `D(i, e) -> `D(i, div e) (* Schwarz theorem is applicable *)
  | `Sum(e1, e2) -> `Sum(div e1, div e2)
  | `Diff(e1, e2) -> `Diff(div e1, div e2)
  | `Prod(s, e) -> `Sum(`Prod(s, div e), scalar (grad s) e)

let rec rot = function
  | `Vect t -> `Vect [|`Diff (`D(2, t.(3-1)), `D(3, t.(2-1))); `Diff (`D(3, t.(1-1)), `D(1, t.(3-1))); `Diff (`D(1, t.(2-1)), `D(2, t.(1-1)))|]
  | `D(i, e) -> `D(i, rot e) (* Schwarz theorem is applicable *)
  | `Sum(e1, e2) -> `Sum(rot e1, rot e2)
  | `Diff(e1, e2) -> `Diff(rot e1, rot e2)
  | `Prod(s, e) -> `Sum(cross (grad s) e, `Prod(s, rot e))

let laplace_scalar e = div @@ grad @@ e

let laplace_vect e : expr = `Diff (grad @@ div @@ e, rot @@ rot @@ e)

exception Not_lswcc

let lswcc_discretization steps (e: scalar_expr) =
  (* Linear scalar with constant coefficients discretization *)
  let n = 1 + max_index_unk e in
  let m = dimension_domain e in
  let k = 1 + diff_class e in
  let rec aux = function
    | `Unk x1 -> 
      let d = Matrix.create_3D n ~m ~k 0. in
      for x2 = 0 to m - 1 do
      	d.(x1).(x2).(0) <- 1.;
      done;
      d
    | `Nul
    | `Const _
    | `Fun _ -> Matrix.create_3D n ~m ~k 0.
    | `Sum (e1, e2) -> Matrix.map2_3D (+.) (aux e1) (aux e2)
    | `Diff (e1, e2) -> Matrix.map2_3D (-.) (aux e1) (aux e2)
    | `D(i, e) ->
      let d = aux e in
      let d' = Matrix.create_3D n ~m ~k 0. in
      for x1 = 0 to n - 1 do

      	d'.(x1).(i).(0)<- -. d.(x1).(i).(0);
        for x3 = 1 to k - 1 do
          d'.(x1).(i).(x3) <- (d.(x1).(i).(x3 - 1) -. d.(x1).(i).(x3)) /. steps.(i)
        done;

        for x2 = 0 to m - 1 do
          if x2 <> i then begin
            for x3 = 0 to k - 1 do
              d'.(x1).(x2).(x3) <- d.(x1).(x2).(x3)
            done;
          end
        done;

      done;
      d'
    | `Prod(`Nul, _) -> Matrix.create_3D n ~m ~k 0.
    | `Prod(`Const c, e) -> Matrix.map (fun x -> c *. x) (aux e)
    | `Prod _ -> raise Not_lswcc
  in aux e

exception Non_linear

let linear_discretization steps (e: scalar_expr) =
  (* Linear scalar discretization *)
  let n = 1 + max_index_unk e in
  let m = dimension_domain e in
  let k = 1 + diff_class e in
  let rec aux = function
    | `Unk x1 -> 
      let d = Matrix.create_3D n ~m ~k (fun _ -> 0.) in
      for x2 = 0 to m - 1 do
      	d.(x1).(x2).(0) <- (fun _ -> 1.);
      done;
      d
    | `Nul
    | `Const _
    | `Fun _ -> Matrix.create_3D n ~m ~k (fun _ -> 0.)
    | `Sum (e1, e2) -> Matrix.map2_3D (fun f g -> (fun z -> f z +. g z)) (aux e1) (aux e2)
    | `Diff (e1, e2) -> Matrix.map2_3D (fun f g -> (fun z -> f z -. g z)) (aux e1) (aux e2)
    | `D(i, e) ->
      let d = aux e in
      let d' = Matrix.create_3D n ~m ~k (fun _ -> 0.) in
      for x1 = 0 to n - 1 do

      	d'.(x1).(i).(0) <- (fun z -> (* !! Caution: Minus sign !! *) -.  d.(x1).(i).(0) z);
        for x3 = 1 to k - 1 do
          d'.(x1).(i).(x3) <- fun z -> ((d.(x1).(i).(x3 - 1) z) -. d.(x1).(i).(x3) z) /. steps.(i)
        done;

        for x2 = 0 to m - 1 do
          if x2 <> i then begin
            for x3 = 0 to k - 1 do
              d'.(x1).(x2).(x3) <- d.(x1).(x2).(x3)
            done;
          end
        done;

      done;
      d'
    | `Prod(`Nul, _) -> Matrix.create_3D n ~m ~k (fun _ -> 0.)
    | `Prod(`Const c, e) -> Matrix.map (fun f -> (fun x -> c *. f x)) (aux e)
    | `Prod(`Fun f, e) -> Matrix.map (fun g -> (fun x -> (f x) *. g x)) (aux e)
    | `Prod _ -> raise Non_linear
  in aux e

let vect_lswcc_discretization steps e =
  let t = extract_vect e in
  Array.map (lswcc_discretization steps) t