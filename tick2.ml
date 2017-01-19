fun last [] = raise Fail "List cannot be empty"
  | last (y::[]) = y
  | last (y::ys) = last(ys);

fun butlast ls =
	let
		fun postAppend (x::[], ys) = ys
		  | postAppend (x::xs, ys) = postAppend (xs, ys @ [x])
	in
	 postAppend (ls, [])
	end;

fun nth (x::xs, n) =
	if n = 0 then x
	else nth(xs, n-1);