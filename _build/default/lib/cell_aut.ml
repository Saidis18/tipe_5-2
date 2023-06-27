open Matrix

type 'a field = {mat: 'a array array array array; size: int}

let norm_field s = ((Array.map (Array.map (Array.map Vect.norm)) s.mat ): float array array array)

let create n v = {mat = create_3D n v; size = n}

let laplace_op (s1: float field) (s2: float field) ht hs c i j k =
  let p x = ht *. ht *. c *. c /. (hs *. hs) *.
    (s2.mat.(i+1).(j).(k).(x) +. s2.mat.(i-1).(j).(k).(x)
    +. s2.mat.(i).(j+1).(k).(x) +. s2.mat.(i).(j-1).(k).(x)
    +. s2.mat.(i).(j).(k+1).(x) +. s2.mat.(i).(j).(k-1).(x)
    -. 6. *. s2.mat.(i).(j).(k).(x)) +. 2. *. s2.mat.(i).(j).(k).(x) -. s1.mat.(i).(j).(k).(x) in
  [|p 0; p 1; p 2|]

let update (s1: float field) (s2: float field) ht hs c =
  let n = s1.size in
  let r = create n s2.mat.(0).(0).(0) in
  for i = 1 to n - 2 do
    for j = 1 to n - 2 do
      for k = 1 to n - 2 do
        r.mat.(i).(j).(k) <- laplace_op s1 s2 ht hs c i j k
      done;
    done;
  done;
  r

open Graphics

let show s (i: int) =
  let aux x = rgb 0 (iof (255. *. (x -. 1.) *. (x -. 1.)) ) (iof (255. *. (1. -. (x -. 1.) *. (x -. 1.))) ) in
  let r = Array.map (Array.map aux) (norm_field s).(i) in
  draw_image (make_image r) 10 10;
