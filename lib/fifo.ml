type 'a a23 =
  | F of 'a
  | N2 of 'a a23 * 'a a23
  | N3 of 'a a23 * 'a a23 * 'a a23

type 'a digit =
  | One of 'a a23
  | Two of 'a a23 * 'a a23
  | Three of 'a a23 * 'a a23 * 'a a23

type 'a fingertree =
  | Nil
  | Single of 'a a23
  | More of 'a digit * 'a fingertree * 'a digit

let rec size23 = function
  | F _ -> 1
  | N2 (a, b) -> size23 a + size23 b
  | N3 (a, b, c) -> size23 a + size23 b + size23 c

let rec heightcomplete = function
  | F _ -> 1, true
  | N2 (a, b) ->
    let height_a, complete_a = heightcomplete a in
    let height_b, complete_b = heightcomplete b in
    max height_a height_b, complete_a && complete_b && height_a = height_b
  | N3 (a, b, c) ->
    let height_a, complete_a = heightcomplete a in
    let height_b, complete_b = heightcomplete b in
    let height_c, complete_c = heightcomplete c in
    max (max height_a height_b) height_c,
    complete_a && complete_b && complete_c && height_a = height_b && height_a = height_c

let treesize ft =
  let rec aux n = function
    | Nil -> n
    | Single _ -> n + 1
    | More (One _ , a , One _ ) -> aux (n + 2) a
    | More (Two _ , a , One _ )
    | More (One _ , a , Two _ ) -> aux (n + 3) a
    | More (One _ , a , Three _ )
    | More (Two _ , a , Two _ )
    | More (Three _ , a , One _ ) -> aux (n + 4) a
    | More (Two _ , a , Three _ )
    | More (Three _ , a , Two _ ) -> aux (n + 5) a
    | More (Three _ , a , Three _ ) -> aux (n + 6) a
  in aux 0 ft

let size_digit = function
  | One a1 -> size23 a1
  | Two (a1, a2) -> size23 a1 + size23 a2
  | Three (a1, a2, a3) -> size23 a1 + size23 a2 + size23 a3

let size ft =
  let rec aux n = function
    | Nil -> n
    | Single _ -> n + 1
    | More (ld, a, rd) -> aux (size_digit ld + size_digit rd) a
  in aux 0 ft

let height ft =
  let rec aux n = function
    | Nil -> n - 1
    | Single _ -> n
    | More (_,a,_) -> aux (n + 1) a
  in aux 0 ft

let [@tail_mod_cons] rec insertleft a = function
  | Nil -> Single a
  | Single b -> More (One a, Nil, One b)
  | More (One ld1 , b, rd) -> More (Two (a, ld1), b, rd)
  | More (Two (ld1, ld2), b, rd) -> More (Three (a, ld1, ld2), b, rd)
  | More (Three (ld1, ld2, ld3), b, rd) -> More (Two (a, ld1), insertleft (N2(ld2, ld3)) b, rd)

let [@tail_mod_cons] rec insertright a = function
  | Nil -> Single a
  | Single b -> More (One b, Nil, One a)
  | More (ld, b, One rd1) -> More (ld, b, Two (rd1, a))
  | More (ld, b, Two (rd1, rd2)) -> More (ld, b, Three (rd1, rd2, a))
  | More (ld, b, Three (rd1, rd2, rd3)) -> More (ld, insertright (N2(rd1, rd2)) b, Two (rd3, a))

exception Empty_fingertree

let rec extractleft = function
  | Nil -> raise Empty_fingertree
  | Single a -> a, Nil
  | More (Three (ld1, ld2, ld3), ft, rd) -> ld1, More (Two (ld2, ld3), ft, rd)
  | More (Two (ld1, ld2), ft, rd) -> ld1, More (One ld2, ft, rd)
  | More (One ld1, Nil, One rd1) -> ld1, Single rd1
  | More (One ld1, Nil, Two (rd1, rd2)) -> ld1, More (One rd1, Nil, One rd2)
  | More (One ld1, Nil, Three (rd1, rd2, rd3)) -> ld1, More (One rd1, Nil, Two (rd2, rd3))
  | More (One ld1, ft, rd) -> match extractleft ft with
    | F _, _ -> failwith "Structure"
    | N2 (b1, b2), ft_bis -> ld1, More (Two (b1, b2), ft_bis, rd)
    | N3 (b1, b2, b3), ft_bis -> ld1, More (Three (b1, b2, b3), ft_bis, rd)

let rec extractright = function
  | Nil -> raise Empty_fingertree
  | Single a -> a, Nil
  | More (ld, ft, Three (rd1, rd2, rd3)) -> rd3, More (ld, ft, Two (rd1, rd2))
  | More (ld, ft, Two (rd1, rd2)) -> rd2, More (ld, ft, One rd1)
  | More (One ld1, Nil, One rd1) -> rd1, Single ld1
  | More (Two (ld1, ld2), Nil, One rd1) -> rd1, More (One ld1, Nil, One ld2)
  | More (Three (ld1, ld2, ld3), Nil, One rd1) -> rd1, More (Two (ld1, ld2), Nil, One ld3)
  | More (ld, ft, One rd1) -> match extractright ft with
    | F _, _ -> failwith "Structure"
    | N2 (b1, b2), ft_bis -> rd1, More (ld, ft_bis, Two (b1, b2))
    | N3 (b1, b2, b3), ft_bis -> rd1, More (ld, ft_bis, Three (b1, b2, b3))

let complete_digit_of_range n = function
  | One a1 -> let h1, c1 = heightcomplete a1 in c1 && h1 = n
  | Two (a1, a2) ->
    let h1, c1 = heightcomplete a1 in
    let h2, c2 = heightcomplete a2 in
    c1 && c2 && h1 = n && h2 = n
  | Three (a1, a2, a3) ->
    let h1, c1 = heightcomplete a1 in
    let h2, c2 = heightcomplete a2 in
    let h3, c3 = heightcomplete a3 in
    c1 && c2 && c3 && h1 = n && h2 = n && h3 = n

let rec complete_of_range n = function
  | Nil -> true
  | Single a -> let h, c = heightcomplete a in h = n && c
  | More (ld, ft, rd) ->
    complete_digit_of_range n ld && complete_digit_of_range n rd && complete_of_range (n + 1) ft

let cons x dq = insertleft (F x) dq
let snoc x dq = insertright (F x) dq
let tail dq = match extractleft dq with
  | F a, ft -> a, ft
  | _ -> failwith "Structure"
let init dq = match extractright dq with
  | F a, ft -> a, ft
  | _ -> failwith "Structure"

let digit2list = function
  | One a1 -> [a1]
  | Two (a1, a2) -> [a1; a2]
  | Three (a1, a2, a3) -> [a1; a2; a3]

  let rec list2trees = function
  | [] -> []
  | [a;b;c] -> [N3(a, b, c)]
  | a::b::t -> N2(a, b)::list2trees t
  | [_] -> failwith "list2trees"

let rec insert_list_right ft = function
  | [] -> ft
  | h::t -> insertright h (insert_list_right ft t)

let rec insert_list_left ft = function
  | [] -> ft
  | h::t -> insertleft h (insert_list_left ft t)
