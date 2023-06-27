type _ digit =
  | One: 'a -> 'a digit
  | Two: 'a * 'a -> 'a digit
  | Three: 'a * 'a * 'a -> 'a digit

type _ node =
  | N2: 'a * 'a -> 'a node
  | N3: 'a * 'a * 'a -> 'a node

type _ fingertree =
  | Nil: 'a fingertree
  | Single: 'a -> 'a fingertree
  | More: 'a digit * ('a node) fingertree * 'a digit -> 'a fingertree

let rec insertleft: type x. x -> x fingertree -> x fingertree = fun a ft_sup -> match ft_sup with
  | Nil -> Single a
  | Single b -> More (One a, Nil, One b)
  | More (One ld1, ft, rd) -> More (Two (a, ld1), ft, rd)
  | More (Two (ld1, ld2), ft, rd) -> More (Three (a, ld1, ld2), ft, rd)
  | More (Three (ld1, ld2, ld3), ft, rd) -> More (Two (a, ld1), insertleft (N2(ld2, ld3)) ft, rd)

let rec insertright: type x. x -> x fingertree -> x fingertree = fun a ft_sup -> match ft_sup with
  | Nil -> Single a
  | Single b -> More (One b, Nil, One a)
  | More (ld, ft, One rd1) -> More (ld, ft, Two (rd1, a))
  | More (ld, ft, Two (rd1, rd2)) -> More (ld, ft, Three (rd1, rd2, a))
  | More (ld, ft, Three (rd1, rd2, rd3)) -> More (ld, insertright (N2(rd1, rd2)) ft, Two(rd3, a))

exception Empty_fingertree

let rec extractleft: type x. x fingertree -> x * (x fingertree) = function
  | Nil -> raise Empty_fingertree
  | Single a -> a, Nil
  | More (Three (ld1, ld2, ld3), ft, rd) -> ld1, More (Two (ld2, ld3), ft, rd)
  | More (Two (ld1, ld2), ft, rd) -> ld1, More (One ld2, ft, rd)
  | More (One ld1, Nil, One rd1) -> ld1, Single rd1
  | More (One ld1, Nil, Two (rd1, rd2)) -> ld1, More (One rd1, Nil, One rd2)
  | More (One ld1, Nil, Three (rd1, rd2, rd3)) -> ld1, More (One rd1, Nil, Two (rd2, rd3))
  | More (One ld1, ft, rd) -> match extractleft ft with
    | N2 (b1, b2), ft_bis -> ld1, More (Two (b1, b2), ft_bis, rd)
    | N3 (b1, b2, b3), ft_bis -> ld1, More (Three (b1, b2, b3), ft_bis, rd)

let rec extractright: type x. x fingertree -> x * (x fingertree) = function
  | Nil -> raise Empty_fingertree
  | Single a -> a, Nil
  | More (ld, ft, Three (rd1, rd2, rd3)) -> rd3, More (ld, ft, Two (rd1, rd2))
  | More (ld, ft, Two (rd1, rd2)) -> rd2, More (ld, ft, One rd1)
  | More (One ld1, Nil, One rd1) -> rd1, Single ld1
  | More (Two (ld1, ld2), Nil, One rd1) -> rd1, More (One ld1, Nil, One ld2)
  | More (Three (ld1, ld2, ld3), Nil, One rd1) -> rd1, More (Two (ld1, ld2), Nil, One ld3)
  | More (ld, ft, One rd1) -> match extractright ft with
    | N2 (b1, b2), ft_bis -> rd1, More (ld, ft_bis, Two (b1, b2))
    | N3 (b1, b2, b3), ft_bis -> rd1, More (ld, ft_bis, Three (b1, b2, b3))

let digit2list = function
  | One a1 -> [a1]
  | Two (a1, a2) -> [a1; a2]
  | Three (a1, a2, a3) -> [a1; a2; a3]

let rec list2trees = function
  | [] -> []
  | [a;b;c] -> [N3(a, b, c)]
  | a::b::t -> N2(a, b)::list2trees t
  | [_] -> failwith "list2trees_poly"

let rec insert_list_right ft = function
  | [] -> ft
  | h::t -> insertright h (insert_list_right ft t)

let rec insert_list_left ft = function
  | [] -> ft
  | h::t -> insertleft h (insert_list_left ft t)

let rec glue: type x. x fingertree -> x list -> x fingertree -> x fingertree = fun ft1 l ft2 -> match ft1, l, ft2 with
  | Nil, _, _ -> insert_list_left ft2 l
  | _, _, Nil -> insert_list_right ft1 l
  | Single a1, _, _ -> insert_list_left ft2 (a1::l)
  | _, _, Single a2 -> insert_list_right ft1 (l@[a2])
  | More (ld1, ft1_bis, rd1), _, More (ld2, ft2_bis, rd2) ->
    let l_bis = list2trees ((digit2list rd1) @ l @ (digit2list ld2)) in
    More (ld1, glue ft1_bis l_bis ft2_bis, rd2)

let concat ft1 ft2 = glue ft1 [] ft2

let list2fingertree l = glue Nil l Nil

let rec fingertree2list = function
  | Nil -> []
  | ft -> let h, ft_bis = extractleft ft in h::fingertree2list ft_bis

let map_digit f = function
  | One a -> One (f a)
  | Two (a, b) -> Two (f a, f b)
  | Three (a, b, c) -> Three (f a, f b, f c)

let map_node f = function
  | N2 (a, b) -> N2 (f a, f b)
  | N3 (a, b, c) -> N3 (f a, f b, f c)

let rec map: type x y. (x -> y) -> x fingertree -> y fingertree = fun f ft -> match ft with
  | Nil -> Nil
  | Single a -> Single (f a)
  | More (ld, a, rd) -> More (map_digit f ld, map (map_node f) a, map_digit f rd)

  let mapi f ft = fingertree2list ft |> List.mapi f |> list2fingertree
