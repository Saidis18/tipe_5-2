open TIPE

let () = Exe_funs.test_times_fft1 7
let () = Exe_funs.test_times_fft2 17
let () = Exe_funs.test_correct_fft ()
let () = Exe_funs.test_laplace_ca ()
let () = Exe_funs.test_2D_matrix_fft_2 ()
let () = Exe_funs.test_spect_ca_1 ()
let () = Exe_funs.test_spect_ca_point ()
let () = Exe_funs.test_spect_ca_cyl ()
let () = Exe_funs.test_cell_aut_v2_point ()
let () = Exe_funs.test_cell_aut_v2_cyl ()
let () = Exe_funs.test_cell_aut_v3_point ()
let () = Exe_funs.test_cell_aut_v3_cyl ()
