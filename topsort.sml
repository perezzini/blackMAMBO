local (* Sort topolo'gico *)
	fun cmp(x, y) = x=y
	fun mem(x, []) = false
	| mem(x, y::l) = cmp(x, y) orelse mem(x, l)
	fun nexts(a, []) = []
	| nexts(a, (x, y)::pairs) =
		if cmp(a, x) then y::nexts(a, pairs) else nexts(a, pairs)
in
	fun topsort graph =
		let	fun sort([], path, visited) = visited
			| sort(x::xs, path, visited) =
				if mem(x, path) then raise Fail "ciclo!"
				else sort(xs, path,
						if mem(x, visited) then visited
						else x::sort(nexts(x, graph), x::path, visited))
			val (starts, _) = ListPair.unzip graph
		in	sort(starts, [], []) end
end
(*
(* 3 *)
(* a esto hay que mejorarlo mucho! *)
fun integraTEnvs(env, env') =
	let	val res = fromTab env
		fun aux(c, v) = tabRInserta(c, v, res)
	in
		tabAplica(aux, env');
		res
	end
(*------------------------------*)

fun muestra(s, t)=
	let	fun aux(NameTy t) = print("NameTy "^t)
		| aux(ArrayTy t) = print("ArrayTy "^t)
		| aux(RecordTy l) =
			let	fun f{name, typ,...} =
					(print(name^" "); aux typ)
			in
				(print "RecordTy "; app f l)
			end
	in
		print s; print "    "; aux t; print "\n"
	end
fun string2Ty(s, t) = (NameTy s, t)
val t = colectaNameTy prueba
val l = List.map string2Ty (tabAList t);
val r = topsort l;
*)
