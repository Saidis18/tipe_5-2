type expr =
  | Unk of int (* Inconnues: Fonctions scalaires; Le paramètre est uniquement un identifiant *)
  | Fun of string (* Fonctions scalaires uniquement *)
  | Vect of expr array (* Indices: x: 0; y: 1; z: 2 *)
  | D of int * expr (* Dérivées partielles: Indices: Temps: 0; x: 1; y: 2; z: 3 *)
  | Sum of expr * expr
  | Diff of expr * expr
  | Prod of expr * expr (* ! Au moins l'une des expressions doit être scalaire ! *)

exception Dimension

let rec dimension_image = function (* Verifies coherence of dimensions and retruns the dimension *)
  | Unk _ -> 1
  | Fun _ -> 1
  | Vect t when Array.fold_left (fun b x -> b && dimension_image x = 1) true t -> Array.length t
  | Vect _ -> raise Dimension
  | D(_, e) -> dimension_image e
  | Sum(e1, e2)
  | Diff(e1, e2) ->
    let d1 = dimension_image e1 in
    let d2 = dimension_image e2 in
    if d1 = d2 then
      d1
    else
      raise Dimension
  | Prod(e1, e2) ->
    let d1 = dimension_image e1 in
    let d2 = dimension_image e2 in
    if d1 = 1 then
      d2
    else if d2 = 1 then
      d1
    else
      raise Dimension

let rec dimension_domain = function
  | Unk _ -> 1
  | Fun _ -> 1
  | Vect t -> Array.fold_left (fun d x -> max d (dimension_domain x)) 0 t
  | D(i, e) -> max (i+1) (dimension_domain e)
  | Sum(e1, e2)
  | Diff(e1, e2)
  | Prod(e1, e2) -> max (dimension_domain e1) (dimension_domain e2)

let rec diff_class = function
  (* What differentiability class should the unknown be for the Schwarz theorem to be applicable (at least) *)
  | Unk _ -> 0
  | Fun _ -> 0
  | Vect t -> Array.fold_left (fun d e -> max d (diff_class e)) 0 t
  | D (_, e) -> 1 + diff_class e
  | Sum(e1, e2)
  | Diff(e1, e2)
  | Prod(e1, e2)-> max (diff_class e1) (diff_class e2)

let rec max_index_unk = function
  | Unk i -> i
  | Fun _ -> -1
  | Vect t -> Array.fold_left (fun m e -> max m (max_index_unk e)) (-1) t
  | D(_, e) -> max_index_unk e
  | Sum(e1, e2)
  | Diff(e1, e2)
  | Prod(e1, e2) -> max (max_index_unk e1) (max_index_unk e2)

let rec div = function
  | Vect t -> Sum( Sum ( D(1, t.(1-1)), D(2, t.(2-1)) ), D(3, t.(3-1)))
  | D(i, e) -> D(i, div e) (* Schwarz theorem is applicable *)
  | Sum(e1, e2) -> Sum(div e1, div e2)
  | Diff(e1, e2) -> Diff(div e1, div e2)
  | _ -> raise Dimension

let grad e = Vect [|D(1, e); D(2, e); D(3, e)|]

let rec rot = function
  | Vect t -> Vect [|Diff (D(2, t.(3-1)), D(3, t.(2-1))); Diff (D(3, t.(1-1)), D(1, t.(3-1))); Diff (D(1, t.(2-1)), D(2, t.(1-1)))|]
  | D(i, e) -> D(i, rot e) (* Schwarz theorem is applicable *)
  | Sum(e1, e2) -> Sum(rot e1, rot e2)
  | Diff(e1, e2) -> Diff(rot e1, rot e2)
  | _ -> raise Dimension

let laplace_scalar e = div @@ grad @@ e

let laplace_vect e = Diff (grad @@ div @@ e, rot @@ rot @@ e)

let rec eliminate_1D_vectors = function
  | Unk i -> Unk i
  | Fun f -> Fun f
  | Vect [|e|] -> e
  | Vect t -> Vect t
  | D(i, e) -> D(i, eliminate_1D_vectors e)
  | Sum(e1, e2) -> Sum(eliminate_1D_vectors e1, eliminate_1D_vectors e2)
  | Diff(e1, e2) -> Diff(eliminate_1D_vectors e1, eliminate_1D_vectors e2)
  | Prod(e1, e2) -> Prod(eliminate_1D_vectors e1, eliminate_1D_vectors e2)

let rec extract_vect e' = match eliminate_1D_vectors e' with
  | Unk i -> [|Unk i|]
  | Fun f -> [|Fun f|]
  | e when dimension_image e = 1 -> [|e|]
  | Vect t ->  t
  | D (i, Vect t) -> Array.map (fun x -> D(i, x)) t
  | D(i, e) -> extract_vect (D(i, Vect (extract_vect e)))
  | Sum(Vect t1, Vect t2) -> Array.map2 (fun x y -> Sum(x, y)) t1 t2
  | Sum (e1, e2) -> extract_vect (Sum(Vect (extract_vect e1), Vect (extract_vect e2)))
  | Diff(Vect t1, Vect t2) -> Array.map2 (fun x y -> Diff(x, y)) t1 t2
  | Diff (e1, e2) -> extract_vect (Diff(Vect (extract_vect e1), Vect (extract_vect e2)))
  | Prod(e1, Vect t2) -> Array.map (fun x -> Prod(e1, x)) t2
  | Prod(Vect t1, e2) -> Array.map (fun x -> Prod(e2, x)) t1
  | Prod(e1, e2) -> extract_vect (Prod(Vect (extract_vect e1), Vect (extract_vect e2)))

let discretization_scalar steps e = (* e must be a scalar expression *)
  let n = 1 + max_index_unk e in
  let m = dimension_domain e in
  let k = 1 + diff_class e in
  let rec aux = function
    | Unk x1 -> 
      let d = Matrix.create_3D n ~m ~k 0. in
      for x2 = 0 to m - 1 do
      	d.(x1).(x2).(0) <- 1.;
      done;
      d
    | Fun _ -> Matrix.create_3D n ~m ~k 0.
    | Vect _ -> raise Dimension
    | Sum (e1, e2) -> Matrix.map2_3D (+.) (aux e1) (aux e2)
    | Diff (e1, e2) -> Matrix.map2_3D (-.) (aux e1) (aux e2)
    | D(i, e) ->
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
    | Prod _ -> failwith "Yet to be implemented"
  in aux e
