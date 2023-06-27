open Matrix

type 'a field = {mat: 'a array array array array; size: int; dx: float }

type em_field = {e: float field; b: float field; mutable j: float field; size: int; dx: float}

let ( ++ ) = Vect.gen_op (+.)
let ( -- ) = Vect.gen_op (-.)
let ( ** ) = Vect.gen_scalar ( *. )
let ( *^ ) = Vect.gen_cross_prod ( *. ) ( -. )

let norm_field s = ((Array.map (Array.map (Array.map Vect.norm)) s.mat ): float array array array)

let create_f n v dx = {mat = create_3D n v; size = n; dx}
let create_em_f n v dx = {e = create_f n v dx; b = create_f n v dx; j = create_f n v dx; size = n; dx}

let rot f x y z =
  [|
    (f.mat.(x).(y+1).(z).(2) -. f.mat.(x).(y).(z).(2) -. f.mat.(x).(y).(z+1).(1) +. f.mat.(x).(y).(z).(1)) /. f.dx;
    (f.mat.(x).(y).(z+1).(0) -. f.mat.(x).(y).(z).(0) -. f.mat.(x+1).(y).(z).(2) +. f.mat.(x).(y).(z).(2)) /. f.dx;
    (f.mat.(x+1).(y).(z).(1) -. f.mat.(x).(y).(z).(1) -. f.mat.(x).(y+1).(z).(0) +. f.mat.(x).(y).(z).(0)) /. f.dx
  |]

let update_e e b j x y z dt c mu =
  (c *. c *. dt) ** rot b x y z -- (mu *. c *. c *. dt) ** j.mat.(x).(y).(z) ++ e.mat.(x).(y).(z)

let update_b e b x y z dt =
  b.mat.(x).(y).(z) -- dt ** rot e x y z

  let update f n_f dt c mu =
    let n = f.size in
    for x = 1 to n - 2 do
      for y = 1 to n - 2 do
        for z = 1 to n - 2 do
          n_f.e.mat.(x).(y).(z) <- update_e f.e f.b f.j x y z dt c mu;
          n_f.b.mat.(x).(y).(z) <- update_b f.e f.b x y z dt
        done;
      done;
    done;

open Graphics

let show s (i: int) =
  let aux x = rgb 0 (iof (255. *. (x -. 1.) *. (x -. 1.)) ) (iof (255. *. (1. -. (x -. 1.) *. (x -. 1.))) ) in
  let r = Array.map (Array.map aux) (norm_field s).(i) in
  draw_image (make_image r) 10 10;;


let show_lin s (i: int) =
  let s1 = (Array.map (Array.map Vect.norm) s.mat.(i)) in
  Matrix.show_lin s1;
