let test_times_fft1 k = (
  let m = Matrix.create_3D 100 0 in

  let () =
  (
  for i = 0 to 99 do
    m.(i).(i).(99 - i) <- 1;
    m.(i).(99-i).(i) <- -1;
    m.(99-i).(i).(i) <- 2
  done;
  ) in

  let ignore _ = () in

  let m' = (m |> Matrix.transpose_3D |> Matrix.transpose_3D |> Matrix.transpose_3D) in

  let () = print_string (string_of_bool (m = m')) in

  (* let complex_of_int (i, j) = ({re = float_of_int i; im = float_of_int j} : Complex.t) *)

  let n = Int.shift_left 1 k in (* Number of samples; must be a power of 2 *)
  let fs = 1200. in  (* Sampling rate: Hertz *)
  let f0 = 20. in (* Harmonic frequency: Hertz *)
  let dt = 1. /. fs in (* Time step: Seconds *)

  let pi = Float.pi in

  let f i =
    let s = ref 0. in
    for j = 0 to 10 do
      s := !s +. sin(2. *. pi  *. (float_of_int (2*i+1)) *. f0 *. (float_of_int j) *. dt) /. float_of_int (2*i+1)
    done;
    !s
  in
  
  let [@tail_mod_cons] rec test l i =
    if i <= 0 then
      l
    else
      test ((f i)::l) (i - 1)
  in

  let l = List.rev_map Lib.complex_of_float (test [] n) |> List.rev in
  let ft = Fifo_poly_tail.list2fingertree l in
  let t = Lib.list2array l in

  let t0 = Sys.time () in
  let () = ignore (Fft_list_tail.fft l); print_string "List"; print_newline () in
  let t1 = Sys.time () in
  let () = ignore (Fft_fifo_v1.fft l); print_string "Fifo_v1"; print_newline () in
  let t2 = Sys.time () in
  let () = ignore (Fft_fifo_v2.fft ft); print_string "Fifo_v2"; print_newline () in
  let t3 = Sys.time () in
  let () = ignore (Fft_array.fft t); print_string "Array"; print_newline () in
  let t4 = Sys.time () in
  let () = ignore (Fft_array_in_place.fft t); print_string "Array_in_place"; print_newline () in
  let t5 = Sys.time () in

  let () =
    print_string "List: ";
    print_float (t1 -. t0);
    print_string "s";
    print_newline ();
    print_string "Fifo v1: ";
    print_float (t2 -. t1);
    print_string "s";
    print_newline();
    print_string "Fifo v2: ";
    print_float (t3 -. t2);
    print_string "s";
    print_newline();
    print_string "Array: ";
    print_float (t4 -. t3);
    print_string "s";
    print_newline();
    print_string "Array_in_place: ";
    print_float (t5 -. t4);
    print_string "s";
    print_newline() in
  ()
)




let test_times_fft2 len = (
  let gen () = (
    let list_time = Array.make len 0. in
    let fifo_v1_time = Array.make len 0. in
    let fifo_v2_time = Array.make len 0. in
    let array_time = Array.make len 0. in

    let update k = (
      (* let complex_of_int (i, j) = ({re = float_of_int i; im = float_of_int j} : Complex.t) *)

      let n = Int.shift_left 1 k in (* Number of samples; must be a power of 2 *)
      let fs = 1200. in  (* Sampling rate: Hertz *)
      let f0 = 20. in (* Harmonic frequency: Hertz *)
      let dt = 1. /. fs in (* Time step: Seconds *)

      let pi = Float.pi in

      let f i =
        let s = ref 0. in
        for j = 0 to 10 do
          s := !s +. sin(2. *. pi  *. (float_of_int (2*i+1)) *. f0 *. (float_of_int j) *. dt) /. float_of_int (2*i+1)
        done;
        !s
      in
      
      let [@tail_mod_cons] rec test l i =
        if i <= 0 then
          l
        else
          test ((f i)::l) (i - 1)
      in

      let l = List.rev_map Lib.complex_of_float (test [] n) |> List.rev in
      let ft = Fifo_poly_tail.list2fingertree l in
      let t = Lib.list2array l in

      let t0 = Sys.time () in
      let () = ignore (Fft_list_tail.fft l); print_string "List"; print_newline () in
      let t1 = Sys.time () in
      let () = ignore (Fft_fifo_v1.fft l); print_string "Fifo_v1"; print_newline () in
      let t2 = Sys.time () in
      let () = ignore (Fft_fifo_v2.fft ft); print_string "Fifo_v2"; print_newline () in
      let t3 = Sys.time () in
      let () = ignore (Fft_array.fft t); print_string "Array"; print_newline () in
      let t4 = Sys.time () in

      list_time.(k - 1) <- t1 -. t0;
      fifo_v1_time.(k - 1) <- t2 -. t1;
      fifo_v2_time.(k - 1) <- t3 -. t2;
      array_time.(k - 1) <- t4 -. t3
    ) in
    for i = 1 to len do
      update i
    done;
    [|list_time; fifo_v1_time; fifo_v2_time; array_time|]
  ) in
  let a = [|"temps_liste = "; "temps_fifo_v1 = "; "temps_fifo_v2 = "; "temps_array = "|] in
  let g i x = print_string a.(i); x in
  let f i x = (x |> Array.map Lib.complex_of_float |> Lib.array2list |> g i |> Lib.print_list); print_newline () in
  Array.iteri f (gen ())
)




let test_correct_fft () = (
  let l1 = List.map Lib.complex_of_int [1,2; 3,1; 3,4; 1,8; 8,1; 5,6; 7,2; 5,2] in

  let () = l1 |> Fft_list_tail.fft |> Lib.print_list |> print_newline in 
  let () = l1 |> Fft_fifo_v1.fft |> Lib.print_list |> print_newline in
  let () = l1 |> Fifo_poly_tail.list2fingertree |> Fft_fifo_v2.fft |> Fifo_poly_tail.fingertree2list |> Lib.print_list |> print_newline in
  let () = l1 |> Array.of_list |> Fft_array.fft |> Lib.array2list |> Lib.print_list |> print_newline in 
  let () = l1 |> Array.of_list |> Fft_array_in_place.fft |> Lib.array2list |> Lib.print_list |> print_newline in
  ()
)




let test_laplace_ca () = (
  let s1 = ref (Cell_aut.create 100 [|0.; 0.; 0.|]) in
  let s2 = ref (Cell_aut.create 100 [|0.; 0.; 0.|]) in

  (* Classic cellular automaton *)
  let () = (
    for i = 0 to 99 do
      for j = 25 to 75 do
        for k = 10 to 30 do
          !s1.mat.(i).(j).(k) <- [|1.; 1.; 1.|];
          !s2.mat.(i).(j).(k) <- [|1.; 1.; 1.|]
        done;
      done;
    done;

    Graphics.open_graph " 120x120";

    for l = 0 to 10 do
      let inter = !s2 in
      s2 := Cell_aut.update !s1 inter 10. 0.1 70.;
      s1 := inter;
      Cell_aut.show !s2 50;
      Unix.sleepf 0.2;
      Graphics.clear_graph ();
      let r = float_of_int (l/10 mod 3) -. 1. in
      for i = 0 to 99 do
        for j = 25 to 75 do
          for k = 10 to 30 do
            !s1.mat.(i).(j).(k) <- [|r; r; 0.|];
            !s2.mat.(i).(j).(k) <- [|r; r; 0.|]
          done;
        done;
      done;
    done;
    Graphics.close_graph()
  ) in
  ()
)




let test_2D_matrix_fft () = (
  (* 2D matrix fft and ifft test *)
  let m = Matrix.sin_mat 512 (2, 1) in (* 512 is a powr of two!! *)

  let f = Matrix.fft_matrix_2D m in
  Graphics.open_graph " 550x550";
  Matrix.show_lin (Array.map (Array.map Complex.norm) m);
  Unix.sleepf 5.;
  Matrix.show_lin (Array.map (Array.map Complex.norm) f);
  Unix.sleepf 5.;
  Matrix.show_lin (Array.map (Array.map Complex.norm) (Matrix.ifft_matrix_2D f));
  Unix.sleepf 5.;
  Graphics.clear_graph();

  let m1 = Matrix.create_2D 512 Complex.zero in
  m1.(20).(20) <- Complex.one;
  m1.(512 - 20).(512 - 20) <- Complex.one;

  let f1 = Matrix.ifft_matrix_2D m1 in
  Matrix.show_lin (Array.map (Array.map Complex.norm) m1);
  Unix.sleepf 5.;
  Graphics.clear_graph();
  Matrix.show_lin (Array.map (Array.map Complex.norm) f1);
  Unix.sleepf 5.;
  Graphics.clear_graph();

  (* 2D atrix ifft test *)
  let m2 = Matrix.create_2D 512 Complex.zero in
  m2.(512 - 1).(1) <- Complex.one;
  m2.(1).(512 - 1) <- Complex.one;
  let f2 = Matrix.ifft_matrix_2D m2 in
  Matrix.show_lin (Array.map (Array.map Complex.norm) m2);
  Unix.sleepf 5.;
  Graphics.clear_graph();
  Matrix.show_lin (Array.map (Array.map Complex.norm) f2);
  Unix.sleepf 5.;
  Graphics.clear_graph();
)




let test_2D_matrix_fft_2 () = (
  (* 2D matrix fft and ifft test *)
  let m = Matrix.sin_mat 512 (2, 1) in (* 512 is a powr of two!! *)

  let f = ref (Matrix.fft_matrix_2D m) in
  Graphics.open_graph " 550x550";
  Matrix.show_lin (Array.map (Array.map Complex.norm) m);
  Unix.sleepf 1.;
  Matrix.show_lin (Array.map (Array.map Complex.norm) !f);
  Unix.sleepf 1.;
  Matrix.show_lin (Array.map (Array.map Complex.norm) (Matrix.ifft_matrix_2D !f));
  Unix.sleepf 1.;
  let f' = ref (Array.map (Array.copy) !f) in
  let (+..) = Complex.add in
  for _ = 0 to 400 do
    for i = 1 to 510 do
      for j = 1 to 510 do
        if (i - j) * (i - j) <= 100000 then
          !f'.(i).(j) <- !f.(i-1).(j) +.. !f.(i).(j-1) +.. !f.(i+1).(j) +.. !f.(i).(j+1) +.. !f.(i).(j);
      done;
    done;
    f := Array.map (Array.copy) !f';
  done;
  Matrix.show_lin (Array.map (Array.map Complex.norm) !f);
  Unix.sleepf 15.;
)




let test_spect_ca_1 () = (
  (* Spectral cellular automaton test (3D vector field) *)
  Graphics.open_graph " 150x150";
  let n = 128 in
  let v = [|Complex.zero; Complex.zero; Complex.zero|] in

  let f0 = Cell_aut_spect.create n v  0.1 in
  let f1 = Cell_aut_spect.create n v 0.1 in
  let f2 = Cell_aut_spect.create n v 0.1 in

  let jx = Matrix.create_3D n Complex.zero in
  let jy = Matrix.create_3D n Complex.zero in
  let jz = Matrix.create_3D n Complex.zero in

  for x = 0 to n - 1 do
    jx.(x) <-  Matrix.sin_mat n (2, 1);
    jy.(x) <-  Matrix.sin_mat n (2, 1);
    jz.(x) <-  Matrix.sin_mat n (2, 1)
  done;

  let j = Matrix.zip (jx, jy, jz) in

  Cell_aut_spect.show j.(0);
  Unix.sleepf 2.;

  let jx_f = Matrix.fft_matrix_3D jx in
  let jy_f = Matrix.fft_matrix_3D jy in 
  let jz_f = Matrix.fft_matrix_3D jz in
  let j_f = Matrix.zip (jx_f, jy_f, jz_f) in

  Cell_aut_spect.show j_f.(0);
  (* Since j is constant along the first axis, its fft is equal to zero at any point outside the first axis *)
  Unix.sleepf 2.;

  f0.j <- j_f;
  f1.j <- j_f; (* Keep in mind it is the same j_f for the three. -> If you modify one, you modify the three *)
  f2.j <- j_f;

  Graphics.clear_graph ();
  Unix.sleepf 0.1 ;
  for _ = 0 to 2 do
    Cell_aut_spect.update f0 f1 f2 0.1 100. 0.00001;
    Cell_aut_spect.update f1 f2 f0 0.1 100. 0.00001;
    Cell_aut_spect.update f2 f0 f1 0.1 100. 0.00001;
    print_int 0;
  done;

  Cell_aut_spect.show f1.e.(0);
  Unix.sleepf 2.;
)




let test_spect_ca_point () = (
  (* Spectral cellular automaton test (3D vector field) *)
  Graphics.open_graph " 150x150";
  let n = 128 in
  let v = [|Complex.zero; Complex.zero; Complex.zero|] in
  let dt = 0.1 in (* 0.1 *)
  let dx = 0.1 in (* 0.1 *)
  let c = 100. in (* 100. *)
  let mu = 0.00001 in (* 0.00001 *)

  let f0 = Cell_aut_spect.create n v  dx in
  let f1 = Cell_aut_spect.create n v dx in
  let f2 = Cell_aut_spect.create n v dx in

  let jx = Matrix.create_3D n Complex.zero in
  let jy = Matrix.create_3D n Complex.zero in
  let jz = Matrix.create_3D n Complex.zero in

  for x = 0 to n - 1 do
    jx.(x).(n/2).(n/2) <- Complex.one
  done;

  let j = Matrix.zip (jx, jy, jz) in

  Cell_aut_spect.show j.(0);
  Unix.sleepf 2.;

  let jx_f = Matrix.fft_matrix_3D jx in
  let jy_f = (* Matrix.fft_matrix_3D *) jy in 
  let jz_f = (* Matrix.fft_matrix_3D *) jz in
  let j_f = Matrix.zip (jx_f, jy_f, jz_f) in

  Cell_aut_spect.show j_f.(0);
  (* Since j is constant along the first axis, its fft is equal to zero at any point outside the first axis *)
  Unix.sleepf 2.;

  f0.j <- j_f;
  f1.j <- j_f; (* Keep in mind it is the same j_f for the three. -> If you modify one, you modify the three *)
  f2.j <- j_f;

  Graphics.clear_graph ();
  Unix.sleepf 0.1 ;
  for _ = 0 to 40 do
    Cell_aut_spect.update f0 f1 f2 dt c mu;
    Cell_aut_spect.update f1 f2 f0 dt c mu;
    Cell_aut_spect.update f2 f0 f1 dt c mu;

    let bx_f, by_f, bz_f = Matrix.unzip f1.b in
    let bx = Matrix.ifft_matrix_3D bx_f in
    let by = Matrix.ifft_matrix_3D by_f in
    let bz = Matrix.ifft_matrix_3D bz_f in
    let b = Matrix.zip (bx, by, bz) in
    Cell_aut_spect.show b.(32);
    Unix.sleepf 0.1;
    print_int 0;
  done;

  let bx_f, by_f, bz_f = Matrix.unzip f1.b in
  let bx = Matrix.ifft_matrix_3D bx_f in
  let by = Matrix.ifft_matrix_3D by_f in
  let bz = Matrix.ifft_matrix_3D bz_f in
  let b = Matrix.zip (bx, by, bz) in
  
  Cell_aut_spect.show b.(32);
  Unix.sleepf 10.;
)




let test_spect_ca_cyl () = (
  (* Spectral cellular automaton test (3D vector field) *)
  Graphics.open_graph " 150x150";
  let n = 128 in
  let v = [|Complex.zero; Complex.zero; Complex.zero|] in
  let dt = 0.1 in (* 0.1 *)
  let dx = 0.1 in (* 0.1 *)
  let c = 100. in (* 100. *)
  let mu = 0.00001 in (* 0.00001 *)

  let f0 = Cell_aut_spect.create n v  dx in
  let f1 = Cell_aut_spect.create n v dx in
  let f2 = Cell_aut_spect.create n v dx in

  let jx = Matrix.map (Lib.complex_of_float) (Matrix.create_cylinder n (n/2) (n/2) 10)in
  let jy = Matrix.create_3D n Complex.zero in
  let jz = Matrix.create_3D n Complex.zero in

  for x = 0 to n - 1 do
    jx.(x).(n/2).(n/2) <- Complex.one
  done;

  let j = Matrix.zip (jx, jy, jz) in

  Cell_aut_spect.show j.(0);
  Unix.sleepf 2.;

  let jx_f = Matrix.fft_matrix_3D jx in
  let jy_f = (* Matrix.fft_matrix_3D *) jy in 
  let jz_f = (* Matrix.fft_matrix_3D *) jz in
  let j_f = Matrix.zip (jx_f, jy_f, jz_f) in

  Cell_aut_spect.show j_f.(0);
  (* Since j is constant along the first axis, its fft is equal to zero at any point outside the first axis *)
  Unix.sleepf 2.;

  f0.j <- j_f;
  f1.j <- j_f; (* Keep in mind it is the same j_f for the three. -> If you modify one, you modify the three *)
  f2.j <- j_f;

  Graphics.clear_graph ();
  Unix.sleepf 0.1 ;
  for _ = 0 to 10 do
    Cell_aut_spect.update f0 f1 f2 dt c mu;
    Cell_aut_spect.update f1 f2 f0 dt c mu;
    Cell_aut_spect.update f2 f0 f1 dt c mu;

    let bx_f, by_f, bz_f = Matrix.unzip f1.b in
    let bx = Matrix.ifft_matrix_3D bx_f in
    let by = Matrix.ifft_matrix_3D by_f in
    let bz = Matrix.ifft_matrix_3D bz_f in
    let b = Matrix.zip (bx, by, bz) in
    Cell_aut_spect.show b.(32);
    Unix.sleepf 0.1;
    print_int 0;
  done;

  let bx_f, by_f, bz_f = Matrix.unzip f1.b in
  let bx = Matrix.ifft_matrix_3D bx_f in
  let by = Matrix.ifft_matrix_3D by_f in
  let bz = Matrix.ifft_matrix_3D bz_f in
  let b = Matrix.zip (bx, by, bz) in
  
  Cell_aut_spect.show b.(32);
  Unix.sleepf 10.;
)




let test_cell_aut_v2_point () = (
  Graphics.open_graph " 150x150";
  let n = 128 in
  let v = [|0.; 0.; 0.|] in
  let dt = 0.1 in (* 0.1 *)
  let dx = 0.1 in (* 0.1 *)
  let c = 100. in (* 100. *)
  let mu = 0.00001 in (* 0.00001 *)

  let f0 = Cell_aut_v2.create_em_f n v  dx in
  let f1 = Cell_aut_v2.create_em_f n v dx in
  
  let jx = Matrix.create_3D n 0. in
  let jy = Matrix.create_3D n 0. in
  let jz = Matrix.create_3D n 0. in

  for x = 0 to n - 1 do
    jx.(x).(n/2).(n/2) <- 1.
  done;

  let j = Matrix.zip (jx, jy, jz) in
  let (j_f : float Cell_aut_v2.field) = {mat = j; size = n; dx} in

  Cell_aut_v2.show_lin j_f 0;
  Unix.sleepf 2.;
  f0.j <- j_f;
  f1.j <- j_f;

  Graphics.clear_graph ();
  Unix.sleepf 0.1 ;
  for _ = 0 to 20 do
    Cell_aut_v2.update f0 f1 dt c mu;
    Cell_aut_v2.update f1 f0 dt c mu;
    Cell_aut_v2.show_lin f1.b 32;
    Unix.sleepf 0.1;
    print_int 0;
  done;

  Cell_aut_v2.show_lin f1.j 32;
  Unix.sleepf 5.;
  Cell_aut_v2.show_lin f1.b 32;
  Unix.sleepf 10.;
)




let test_cell_aut_v2_cyl () = (
  Graphics.open_graph " 150x150";
  let n = 128 in
  let v = [|0.; 0.; 0.|] in
  let dt = 0.1 in (* 0.1 *)
  let dx = 0.1 in (* 0.1 *)
  let c = 100. in (* 100. *)
  let mu = 0.00001 in (* 0.00001 *)

  let f0 = Cell_aut_v2.create_em_f n v  dx in
  let f1 = Cell_aut_v2.create_em_f n v dx in
  
  let jx = Matrix.create_cylinder n (n/2) (n/2) 10 in
  let jy = Matrix.create_3D n 0. in
  let jz = Matrix.create_3D n 0. in

  for x = 0 to n - 1 do
    jx.(x).(n/2).(n/2) <- 1.
  done;

  let j = Matrix.zip (jx, jy, jz) in
  let (j_f : float Cell_aut_v2.field) = {mat = j; size = n; dx} in

  Cell_aut_v2.show_lin j_f 0;
  Unix.sleepf 2.;
  f0.j <- j_f;
  f1.j <- j_f;

  Graphics.clear_graph ();
  Unix.sleepf 0.1 ;
  for _ = 0 to 20 do
    Cell_aut_v2.update f0 f1 dt c mu;
    Cell_aut_v2.update f1 f0 dt c mu;
    Cell_aut_v2.show_lin f1.b 32;
    Unix.sleepf 0.1;
    print_int 0;
  done;

  Cell_aut_v2.show_lin f1.j 32;
  Unix.sleepf 5.;
  Cell_aut_v2.show_lin f1.b 32;
  Unix.sleepf 10.;
)




let test_cell_aut_v3_point () = (
  Graphics.open_graph " 150x150";
  let n = 128 in
  let v = [|0.; 0.; 0.|] in
  let dt = 0.1 in (* 0.1 *)
  let dx = 0.1 in (* 0.1 *)
  let c = 100. in (* 100. *)
  let mu = 0.00001 in (* 0.00001 *)

  let f0 = Cell_aut_v3.create_em_f n v  dx in
  let f1 = Cell_aut_v3.create_em_f n v dx in

  let jx = Matrix.create_3D n 0. in
  let jy = Matrix.create_3D n 0. in
  let jz = Matrix.create_3D n 0. in

  
  for x = 0 to n - 1 do
    jx.(x).(n/2).(n/2) <- 1.
  done;
  

  let j = Matrix.zip (jx, jy, jz) in
  let (j_f : float Cell_aut_v3.field) = {mat = j; size = n; dx} in

  Cell_aut_v3.show_lin j_f 0;
  Unix.sleepf 2.;
  f0.j <- j_f;
  f1.j <- j_f;

  Graphics.clear_graph ();
  Unix.sleepf 0.1 ;
  for _ = 0 to 20 do
    Cell_aut_v3.update f0 f1 dt c mu;
    Cell_aut_v3.update f1 f0 dt c mu;
    Cell_aut_v3.show_lin f1.b 32;
    Unix.sleepf 0.1;
    print_int 0;
  done;

  Cell_aut_v3.show_lin f1.j 32;
  Unix.sleepf 5.;
  Cell_aut_v3.show_lin f1.b 32;
  Unix.sleepf 10.;
)




let test_cell_aut_v3_cyl () = (
  Graphics.open_graph " 150x150";
  let n = 128 in
  let v = [|0.; 0.; 0.|] in
  let dt = 0.1 in (* 0.1 *)
  let dx = 0.1 in (* 0.1 *)
  let c = 100. in (* 100. *)
  let mu = 0.00001 in (* 0.00001 *)

  let f0 = Cell_aut_v3.create_em_f n v  dx in
  let f1 = Cell_aut_v3.create_em_f n v dx in

  let jx = Matrix.create_cylinder n (n/2) (n/2) 10 in
  let jy = Matrix.create_3D n 0. in
  let jz = Matrix.create_3D n 0. in

  (*
  for x = 0 to n - 1 do
    jx.(x).(n/2).(n/2) <- 1.
  done;
  *)

  let j = Matrix.zip (jx, jy, jz) in
  let (j_f : float Cell_aut_v3.field) = {mat = j; size = n; dx} in

  Cell_aut_v3.show_lin j_f 0;
  Unix.sleepf 2.;
  f0.j <- j_f;
  f1.j <- j_f;

  Graphics.clear_graph ();
  Unix.sleepf 0.1 ;
  for _ = 0 to 20 do
    Cell_aut_v3.update f0 f1 dt c mu;
    Cell_aut_v3.update f1 f0 dt c mu;
    Cell_aut_v3.show_lin f1.b 32;
    Unix.sleepf 0.1;
    print_int 0;
  done;

  Cell_aut_v3.show_lin f1.j 32;
  Unix.sleepf 5.;
  Cell_aut_v3.show_lin f1.b 32;
  Unix.sleepf 10.;
)

let test_discretization () = (
  let b = `Vect [|`Unk 0; `Unk 1; `Unk 2|] in
  let e = `Vect [|`Unk 3; `Unk 4; `Unk 5|] in
  let j = `Vect [|`Unk 6; `Unk 7; `Unk 8|] in
  let mu = `Const (4. *. Float.pi *. Float.pow 10. (-.7.)) in
  let inv_c2 = `Const (1. /. (3. *. Float.pow 10. 8.)) in
  let rotb = Expr_gen_v2.rot b in
  let rote = Expr_gen_v2.rot e in
  let faraday = `Diff (rote, `D(0, e)) in
  let ampere = `Diff (rotb, `Sum(`Prod(mu, j),`Prod(inv_c2, `D(0, e)))) in
  let f_d =Expr_gen_v2.vect_lswcc_discretization [|1.; 1.; 1.; 1.|] faraday in 
  let n_line = "                               " in
  let a_d = Expr_gen_v2.vect_lswcc_discretization [|1.; 1.; 1.; 1.|] ampere in
  f_d.(0).(3), n_line, f_d.(0).(4), n_line, f_d.(0).(5), n_line, n_line, n_line,  n_line,
  f_d.(1).(3), n_line, f_d.(1).(4), n_line, f_d.(1).(5), n_line, n_line, n_line, n_line,
  f_d.(2).(3), n_line, f_d.(2).(4), n_line, f_d.(2).(5), n_line, n_line, n_line, n_line,
  n_line, n_line, n_line, n_line, n_line, n_line, n_line, n_line, n_line, n_line, n_line,
  a_d.(0).(0), n_line, a_d.(0).(1), n_line, a_d.(0).(2), n_line, n_line, n_line, n_line,
  a_d.(1).(0), n_line, a_d.(1).(1), n_line, a_d.(1).(2), n_line, n_line, n_line, n_line,
  a_d.(2).(0), n_line, a_d.(2).(1), n_line, a_d.(2).(2)
)