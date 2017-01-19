fun eapprox(n) =
	let fun sumSeries(x, n, prevTerm, total) =
		if x > n then total
		else
			let val thisTerm = prevTerm / real(x - 1) in
			sumSeries(x + 1, n, thisTerm, total + thisTerm)
			end
	in
		sumSeries(2, n, 1.0, 1.0)
	end;

fun exp(z, n) =
	let fun sumSeries(x, z, n, prevTerm, total) =
		if x > n then total
		else
			let val thisTerm = z * prevTerm / real(x - 1) in
			sumSeries(x + 1, z, n, thisTerm, total + thisTerm)
			end
	in
		sumSeries(2, z, n, 1.0, 1.0)
	end;

fun gcd(a, b) =
	if a <= 0 orelse b <= 0 then raise Fail "numbers must be greater than zero"
	else
		if a = b then a
		else
			if a mod 2 = 0 then (* a is even *)
				if b mod 2 = 0 then (* b is even *)
					2 * gcd(a div 2, b div 2)
				else (* b is not even *)
					gcd(a div 2, b)
			else  (* a is not even *)
				if b mod 2 = 0 then (* b is even *)
					gcd(a, b div 2)
				else (* b is not even *)
					if a >= b then gcd((a - b) div 2, b)
					else gcd((b - a) div 2, a)
;