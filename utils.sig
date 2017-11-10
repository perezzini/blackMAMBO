signature utils = 
sig
	val listToString : 'a list -> ('a -> string) -> string 									(* Converts list to string *)

	val setMap : ('a -> 'b) -> 'a Splayset.set -> ('b * 'b -> order) -> 'b Splayset.set 	(* Maps a set of elements *)

	val setToString : 'a Splayset.set -> ('a -> string) -> string 							(* Converts set to string *)
end