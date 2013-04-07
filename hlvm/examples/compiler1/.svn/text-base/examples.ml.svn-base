let rec factorial(n: float) : float = if n < 2.0 then n else n * factorial(n - 1.0);;

factorial 10.0;;

let rec swap((x, y): int * float) : float * int = y, x;;

swap(3, 4.5);;

let rec zadd(((r1, i1), (r2, i2)) : (float * float) * (float * float)) : float * float =
  r1 + r2, i1 + i2;;

zadd((1.2, 2.3), (3.4, 4.5));;

let rec zsqr((r, i) : float * float) : float * float =
  r * r - i * i, 2.0 * r * i;;

zsqr(1.2, 2.3);;

let rec pixel((n, zr, zi, cr, ci) : int * float * float * float * float) : unit =
  if n = 65536 then print_char ' ' else
    if zr * zr + zi * zi >= 4.0 then print_char '.' else
      pixel(n+1, zr * zr - zi * zi + cr, 2.0 * zr * zi + ci, cr, ci);;

let rec row((i, j, n) : int * int * int) : unit =
  if i>n then () else
    begin
      let cr = 2.0 * float_of_int i / float_of_int n - 1.5 in
      let ci = 2.0 * float_of_int j / float_of_int n - 1.0 in
      pixel(0, 0.0, 0.0, cr, ci);
      row(i+1, j, n)
    end;;

let rec col((j, n) : int * int) : unit =
  if j>n then () else
    begin
      row(0, j, n);
      print_char '\n';
      col(j+1, n)
    end;;

let rec mandelbrot(n : int) : unit =
  col(0, n);;

mandelbrot 77;;
