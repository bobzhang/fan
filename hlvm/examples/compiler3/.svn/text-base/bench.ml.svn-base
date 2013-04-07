(* Integer Fibonacci *)

let rec fib(n: int) : int =
  if n > 1 then fib(n-2) + fib(n-1) else n;;

fib 40;;


(* Floating-point Fibonacci *)

let rec fib(n: float) : float =
  if n > 1.0 then fib(n - 2.0) + fib(n - 1.0) else n;;

fib 40.0;;


(* Sieve of Eratosthenes *)

let rec loop2((a, i, di) : bool array * int * int) : unit =
  if i >= length a then () else
    ( a.[i] <- false;
      loop2(a, i + di, di) );;

let rec loop1((a, i) : bool array * int) : unit =
  if i >= length a then () else
    ( if a.[i] then loop2(a, 2*i, i) else ();
      loop1(a, i+1) );;

let rec last((a, i) : bool array * int) : unit =
  if a.[i] then print i else last(a, i-1);;

let rec sieve(n: int) : unit =
  let a = create(n, true) in
  loop1(a, 2);
  last(a, length a - 1);;

sieve 1000;;
sieve 100000000;;


(* Mandelbrot set *)

let rec pixel((n, zr, zi, cr, ci) : int * float * float * float * float) : unit =
  if n = 65536 then print " " else
    if zr * zr + zi * zi >= 4.0 then print "X" else
      pixel(n+1, zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci);;

let rec row((i, j, n) : int * int * int) : unit =
  if i <= n then
    begin
      let cr = 2.0 * float_of_int i / float_of_int n - 1.5 in
      let ci = 2.0 * float_of_int j / float_of_int n - 1.0 in
      pixel(0, 0.0, 0.0, cr, ci);
      row(i+1, j, n)
    end;;

let rec col((j, n) : int * int) : unit =
  if j <= n then
    begin
      row(0, j, n);
      print "
";
      col(j+1, n)
    end;;

let rec mandelbrot(n: int) : unit =
  col(0, n);;

mandelbrot 77;;


(* Radix-2 FFT *)

let rec aux1((i, n, a, a1, a2) : int * int * float array * float array * float array) : unit =
  if i < n then
    begin
      a1.(2*i) <- a.(4*i);
      a1.(2*i+1) <- a.(4*i+1);
      a2.(2*i) <- a.(4*i+2);
      a2.(2*i+1) <- a.(4*i+3);
      aux1(i+1, n, a, a1, a2)
    end;;

let rec aux2((k, n, a, a1, a2) : int * int * float array * float array * float array) : unit =
  if k < n then
    begin
      let r1 = a1.(2*k) in
      let i1 = a1.(2*k+1) in
      let r2 = a2.(2*k) in
      let i2 = a2.(2*k+1) in
      let t = 4. *. pi *. float_of_int k /. float_of_int n in
      let tr = cos t in
      let ti = -.sin t in
      a.(2*k) <- r1 +. r2 *. tr -. i2 *. ti;
      a.(2*k+1) <- i1 +. i2 *. tr +. r2 *. ti;
      aux2(k+1, n, a, a1, a2)
    end;;

let rec aux3((k, n, a, a1, a2) : int * int * float array * float array * float array) : unit =
  if k < n then
    begin
      let r1 = a1.(2*k-n) in
      let i1 = a1.(2*k-n+1) in
      let r2 = a2.(2*k-n) in
      let i2 = a2.(2*k-n+1) in
      let t = 4. *. pi *. (float_of_int k /. float_of_int n) in
      let tr = cos t in
      let ti = -.sin t in
      a.(2*k) <- r1 +. r2 *. tr -. i2 *. ti;
      a.(2*k+1) <- i1 +. i2 *. tr +. r2 *. ti;
      aux3(k+1, n, a, a1, a2)
    end;;

let rec fft(a: float array) : float array =
  if length a < 3 then a else
    begin
      let n = length a / 2 in
      let a1 = create(n, 0.) in
      let a2 = create(n, 0.) in
      aux1(0, n/2-1, a, a1, a2);
      let a1 = fft a1 in
      let a2 = fft a2 in
      aux2(0, n/2-1, a, a1, a2);
      aux3(n/2, n-1, a, a1, a2);
      a
    end;;

let rec ignore(a: float array) : unit = ();;

ignore(fft(create(1048576, 0.0)));;
ignore(fft(create(1048576, 0.0)));;

let rec test(n: int) : float array =
  let a = create(n, 0.0) in
  a.(3) <- 1.0;
  fft a;;

test 16;;


(* Radix-2 FFT *)

let rec zadd(((r1, i1), (r2, i2)) : (float * float) * (float * float)) : float * float =
  r1 +. r2, i1 +. i2;;

let rec zmul(((r1, i1), (r2, i2)) : (float * float) * (float * float)) : float * float =
  r1 *. r2 -. i1 *. i2, r1 *. i2 +. i1 *. r2;;

let rec aux1((i, n, a, a1, a2) : int * int * (float * float) array * (float * float) array * (float * float) array) : unit =
  if i < n/2 then
    begin
      a1.(i) <- a.(2*i);
      a2.(i) <- a.(2*i+1);
      aux1(i+1, n, a, a1, a2)
    end;;

let rec aux2((k, n, a, a1, a2) : int * int * (float * float) array * (float * float) array * (float * float) array) : unit =
  if k < n/2 then
    begin
      let t = 4. *. pi *. float_of_int k /. float_of_int n in
      a.(k) <- zadd(a1.(k), zmul(a2.(k), (cos t, -.sin t)));
      aux2(k+1, n, a, a1, a2)
    end;;

let rec aux3((k, n, a, a1, a2) : int * int * (float * float) array * (float * float) array * (float * float) array) : unit =
  if k < n then
    begin
      let t = 4. *. pi *. float_of_int k /. float_of_int n in
      a.(k) <- zadd(a1.(k-n/2), zmul(a2.(k-n/2), (cos t, -.sin t)));
      aux3(k+1, n, a, a1, a2)
    end;;

let rec fft(a: (float * float) array) : (float * float) array =
  if length a = 1 then create(1, a.(0)) else
    begin
      let n = length a in
      let a1 = create(n/2, (0., 0.)) in
      let a2 = create(n/2, (0., 0.)) in
      aux1(0, n, a, a1, a2);
      let a1 = fft a1 in
      let a2 = fft a2 in
      aux2(0, n, a, a1, a2);
      aux3(n/2, n, a, a1, a2);
      a
    end;;

let rec test(n: int) : (float * float) array =
  let a = create(n, (0., 0.)) in
  a.(1) <- 1.0, 0.0;
  fft a;;

test 8;;

let rec ignore(a: (float * float) array) : unit = ();;

ignore(fft(create(524288, (0.0, 0.0))));;
