structure utils :> utils = 
struct
	
	fun listToString l toString =
		let
			fun listToString' [] _ = []
				| listToString' ((x :: xs) : 'a list) (toString : 'a -> string) = (toString x) :: (listToString' xs toString)

			val l' = listToString' l toString
		in
			"["^(String.concatWith "," l')^"]"
		end

	fun setMap f set compare =
		let
			val setToList = Splayset.listItems set
			val mappedSetToList = List.map f setToList

			val emptySet = Splayset.empty(compare)
		in
			Splayset.addList(emptySet, mappedSetToList)
		end

	fun setToString s toString =
		let
			val sToList = Splayset.listItems s

			fun setToString' [] _ = []
				| setToString' ((x :: xs) : 'a list) (toString : 'a -> string) = (toString x) :: (setToString' xs toString)
		
			val s' = setToString' sToList toString
		in
			"{"^(String.concatWith "," s')^"}"
		end

end