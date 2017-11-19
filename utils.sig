signature utils = 
sig
	(* Converts list to string *)
	val listToString : 'a list -> ('a -> string) -> string

	(* Maps a set of elements. Only specified when the 2nd parameter
	it is not a set of sets *)
	val setMap : ('a -> 'b) -> 'a Splayset.set -> ('b * 'b -> order) -> 'b Splayset.set

	(* Converts set to string *)
	val setToString : 'a Splayset.set -> ('a -> string) -> string

	val intToString : int -> string
end