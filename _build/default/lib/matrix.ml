let iof = int_of_float
let foi = float_of_int

let create_3D n ?(m = n) ?(k = n) v =
  let r = Array.make n [||] in
  for i = 0 to n - 1 do
    r.(i) <- Array.make m [||];
    for j = 0 to m - 1 do
      r.(i).(j) <- Array.make k v
    done;
  done;
  r

let create_cylinder n ?(m = n) ?(k = n) y0 z0 r =
  let t = Array.make n [||] in
  for i = 0 to n - 1 do
    t.(i) <- Array.make m [||];
    for j = 0 to m - 1 do
      t.(i).(j) <- Array.make k 0.;
      for l = 0 to k - 1 do
        if (j - y0) * (j - y0) + (l - z0) * (l - z0) <= r * r then
          t.(i).(j).(l) <- 1.
      done;
    done;
  done;
  t

let create_2D n ?(m = n) v =
  let r = Array.make n [||] in
  for i = 0 to n - 1 do
    r.(i) <- Array.make m v;
  done;
  r

let add_2D m1 m2 = Array.map2 (Array.map2 Complex.add) m1 m2

let map2_3D f m1 m2 = Array.map2 (Array.map2 (Array.map2 f)) m1 m2

let transpose_3D t =
  let n = Array.length t in
  let r = create_3D n t.(0).(0).(0) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        r.(i).(j).(k) <- t.(j).(k).(i);
      done;
    done;
  done;
  r

let transpose_2D t =
  let n = Array.length t in
  let r = create_2D n t.(0).(0) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      r.(i).(j) <- t.(j).(i);
    done;
  done;
  r

let map f = Array.map (Array.map (Array.map f))

let zip (r0, r1, r2) =
  let n = Array.length r0 in 
  let r = create_3D n [|r0.(0).(0).(0)|] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        r.(i).(j).(k) <- [|r0.(i).(j).(k); r1.(i).(j).(k); r2.(i).(j).(k)|]
      done;
    done;
  done;
  r

let unzip t =
  let n = Array.length t in
  let r0 = create_3D n t.(0).(0).(0).(0) in
  let r1 = create_3D n t.(0).(0).(0).(0) in
  let r2 = create_3D n t.(0).(0).(0).(0) in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        r0.(i).(j).(k) <- t.(i).(j).(k).(0);
        r1.(i).(j).(k) <- t.(i).(j).(k).(1);
        r2.(i).(j).(k) <- t.(i).(j).(k).(2)
      done;
    done;
  done;
  (r0, r1, r2)

(* THE SIZE OF ALL OF THE MATRICES MUST BE A POWER OF TWO *)
let one_step_scalar_matrix_fft = Array.map (Array.map Fft_array.fft)

let one_step_scalar_matrix_ifft = Array.map (Array.map Fft_array.ifft)

let ossm_fft = one_step_scalar_matrix_fft

let fft_matrix_3D t = t |> ossm_fft |> transpose_3D |> ossm_fft |> transpose_3D |> ossm_fft |> transpose_3D

let ifft_matrix_3D t = t |> one_step_scalar_matrix_ifft |> transpose_3D |> one_step_scalar_matrix_ifft |> transpose_3D |> one_step_scalar_matrix_ifft |> transpose_3D

let fft_matrix_2D t = t |> Array.map Fft_array.fft |> transpose_2D |> Array.map Fft_array.fft |> transpose_2D

let ifft_matrix_2D t = t |> Array.map Fft_array.ifft |> transpose_2D |> Array.map Fft_array.ifft |> transpose_2D

let lines n p =
  let r = create_2D n Complex.zero in
  let flag = ref true in
  for j = 0 to n - 1 do
    if j mod p = 0 then (flag := not !flag);
    for i = 0 to n - 1 do
      if !flag  then
        r.(i).(j) <- Complex.one
    done;
  done;
  r

let sin_mat n (a, b) = (* n: size of the matrix; (a, b): gives the direction of the sine wave *)
  let r = create_2D n Complex.zero in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      r.(i).(j) <- Lib.complex_of_float (sin (2. *. Float.pi *. (foi (a * i + b * j)) /. 200.))
    done;
  done;
  r

let random_mat n =
  let r = create_2D n Complex.zero in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      r.(i).(j) <- Lib.complex_of_float (Random.float 60.)
    done;
  done;
  r

open Graphics

let show s =
  let aux x = rgb 0 (iof (255. *. (x -. 1.) *. (x -. 1.)) ) (iof (255. *. (1. -. (x -. 1.) *. (x-. 1.))) ) in
  let r = Array.map (Array.map aux) s in
  draw_image (make_image r) 10 10

let show_lin s =
  let n = Array.length s in
  let m = ref 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if s.(i).(j) > !m then
        m := s.(i).(j)
    done;
  done;
  let aux x = rgb 0 (iof (255. *. x /. !m) ) (iof (255. *. (1. -. x /. !m)) ) in
  let r = Array.map (Array.map aux) s in
  draw_image (make_image r) 10 10;
