let divide t =
  let n = Array.length t in
  let r1 = Array.make (n/2) t.(0) in
  let r2 = Array.make (n/2) t.(0) in
  for i = 0 to (n/2) - 1 do
    r1.(i) <- t.(2 * i);
    r2.(i) <- t.(2 * i + 1)
  done;
  r1, r2

let omega inv n j = Complex.polar 1. ((-.2. *. Float.pi *. inv *. float_of_int j) /. float_of_int n)

let rec raw_fft inv t =
  let n = Array.length t in 
  if n <= 1 then
    t
  else begin
  let t1, t2 = divide t in
  let y1, y2 = raw_fft inv t1, raw_fft inv t2 in
  let y = Array.make n Complex.zero in
  for i = 0 to (n/2) - 1 do
      y.(i) <- Complex.add y1.(i) (Complex.mul (omega inv n i) y2.(i));
      y.(i + n/2) <- Complex.sub y1.(i) (Complex.mul (omega inv n i) y2.(i))
  done;
  y end

let fft = raw_fft 1.

let ifft t =
  let n = Array.length t in
  Array.map (fun x -> Complex.div x {re = float_of_int n; im = 0.}) (raw_fft (-.1.) t)

let transpose t =
  let n = Array.length t in
  let p = Array.length t.(0) in
  let r = Array.make p [||] in
  for i = 0 to n do
    for j = 0 to p do
      r.(i).(j) <- t.(j).(i)
    done;
  done;
  r

let fft_vect t = t |> Array.map fft |> transpose |> Array.map fft |> transpose
