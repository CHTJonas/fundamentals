datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;

fun tcons v Lf = Br (v, Lf, Lf)
  | tcons v (Br (w, t1, t2)) = Br (v, tcons w t2, t1);

fun arrayoflist (xs) =
	let fun worker ([], a) = a
		  | worker (y::ys, a) = worker(ys,tcons y a)
	in
		worker(rev(xs), Lf)
	end;

fun listofarray (Br (v, t1, t2)) =
	let fun sub (Br (b, b1, b2), Lf, k, n, xs) = rev(xs)
		  | sub (Br (b, b1, b2), Br(w, w1,w2), k, n, xs) =
		if k = 1
			then sub (Br (b, b1, b2), Br (b, b1, b2), n+1, n+1, w::xs)
		else if k mod 2 = 0
			then sub (Br (b, b1, b2), w1, k div 2, n, xs)
			else sub (Br (b, b1, b2), w2, k div 2, n, xs)
	in
		sub(Br (v, t1, t2), Br (v, t1, t2), 1, 1, [])
	end;

fun getSubsOfEvens (Br (v, v1, v2)) =
	let
		fun evenSubsInList ([], n, ys) = rev(ys)
		  | evenSubsInList (x::xs, n, ys) =
			if x mod 2 = 0 then evenSubsInList(xs, n+1, n::ys)
			else evenSubsInList(xs, n+1, ys)
	in
		evenSubsInList(listofarray(Br (v, v1, v2)), 1, [])
	end;