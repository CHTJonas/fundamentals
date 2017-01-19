fun evalquad (a:real, b:real, c:real, x:real) = a * x * x + b * x + c;

fun facr(n) =  
	if n < 0 then raise Fail "n cannot be negative"
	else if n = 0 then 1
	else n * facr(n-1);

fun faci(n) =
	let fun factorial(n, total) =
		if n = 0 then total
		else factorial(n-1, n * total)
	in
		if n < 0 then raise Fail "n cannot be negative"
		else factorial(n, 1)
	end;

fun sumt(n) =
	let fun sumSeries(n, prevTerm, total) =
		if n = 0 then total
		else
			let val thisTerm = 0.5 * prevTerm in
			sumSeries(n - 1, thisTerm, total + thisTerm)
			end
	in
		sumSeries(n, 2.0, 0.0)
	end;