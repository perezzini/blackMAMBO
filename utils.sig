signature utils = 
sig
	(* Converts list to string *)
	val listToString : 'a list -> ('a -> string) -> string

	(* Maps a set of elements. Only specified when the 2nd parameter
	it is not a set of sets *)
	val setMap : ('a -> 'b) -> 'a Splayset.set -> ('b * 'b -> order) -> 'b Splayset.set

	(* Converts a Splayset set to string *)
	val setToString : 'a Splayset.set -> ('a -> string) -> string

	(* Converts int to string *)
	val intToString : int -> string

	(* Converts a polymorphic pair to string *)
	val pairToString : ('a * 'b) -> ('a -> string) -> ('b -> string) -> string

	(* Identity function *)
	val id : 'a -> 'a

	(* Converts a Splaymap dict to string *)
	val dictToString : ('a, 'b) Splaymap.dict -> ('a -> string) -> ('b -> string) -> string
end