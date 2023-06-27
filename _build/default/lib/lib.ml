let complex_of_int (i, j) = ({re = float_of_int i; im = float_of_int j} : Complex.t)
let complex_of_float x = ({re = x; im = 0.} : Complex.t)let ( *.. ) = Complex.mul

let [@tail_mod_cons] rec print_list' = function
  | [] -> ()
  | ({re = a; im = b}: Complex.t)::t ->
    print_float a;
    print_string " + i";
    print_float b;
    print_string "; ";
    print_list' t

let print_list l = print_string "[ "; print_list' l; print_string " ]"

let array2list t =
  let l = ref [] in
  for i = Array.length t - 1 downto 0 do
    l := t.(i)::!l
  done;
  !l

let list2array l =
  let n = List.length l in
  if n = 0 then [||] else begin
  let r = Array.make n (List.hd l) in
  let [@tail_mod_cons] rec aux i = function
    | [] -> ()
    | h::t -> r.(i) <- h; aux (i+1) t
in aux 0 l;
r end
