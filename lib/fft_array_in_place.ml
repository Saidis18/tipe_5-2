(* THIS PROGRAM DOES NOT WORK *)

let omega inv n j = Complex.polar 1. ((-.2. *. Float.pi *. inv *. float_of_int j) /. float_of_int n)

let fft t =
  let n = Array.length t in
  let r = Array.copy t in
  let rec aux i j =
    if j - i >= 1 then begin
      let k = (i + j) / 2 in
      aux i k;
      aux (k+1) j;
      for l = 0 to (j - i + 1) / 2 - 1 do
        print_int (i + l);
        print_string " ";
        print_int (l + k + 1);
        print_string " | ";
        print_int (j - i + 1);
        print_newline ();
        let a = r.(i + l) in
        let b = Complex.mul (r.(k + l + 1)) (omega 1. (j - i + 1) l) in
        r.(l + i) <- Complex.add a b;
        r.(l + k + 1) <- Complex.sub a b;
      done;
    end 
  in
  aux 0 (n-1);
  r
