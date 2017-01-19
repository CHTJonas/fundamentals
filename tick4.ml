fun nfold f 0 = (fn x => x)
  | nfold f 1 = (fn x => f x)
  | nfold f n = (fn x => f ((nfold f (n-1)) (x))) ;
fun sum m n = (nfold (fn (q) => q+1) n) (m);
fun product m n = (nfold (fn (q) => q+m) (n-1)) (m);
fun power m n = (nfold (fn (q) => q*m) (n-1)) (m);

datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun from k = Cons(k, fn() => from(k+1));

fun nth (Cons(s, fs), 1) = s
  | nth (Cons(s, fs), n) = nth (fs(), n-1);

fun makesquares k = Cons(k*k, fn() => makesquares(k+1));
val squares = makesquares 1;

fun map2 f (Cons(x, fx)) (Cons(y, fy)) = Cons((f x y), fn() => (map2 (f) (fx()) (fy())));