`capwords` <-
function(x) {
	gsub("_", " ", x)
	special.words <- c("ELA", "EMH", "II", "III", "IV")
	my.split <- function(words, split.character) {
		tmp.split <- unlist(strsplit(words, split=split.character))
		tmp.split.special.words.index <- which(!tmp.split %in% special.words)
		return(list(tmp.split=tmp.split, tmp.split.special.words.index=tmp.split.special.words.index))
	}
	s <- my.split(x, " ")
	s[[1]][s[[2]]] <- paste(toupper(substring(s[[1]][s[[2]]],1,1)), tolower(substring(s[[1]][s[[2]]],2)), sep="")
	s.new <- paste(s[[1]], collapse=" ")
	s.new <- unlist(strsplit(s.new, split="-"))
	if (length(s.new) > 1) s.new <- paste(toupper(substring(s.new,1,1)), substring(s.new,2), sep="", collapse="-")
	s.new <- unlist(strsplit(s.new, split="'"))
	if (length(s.new) > 1) s.new <- paste(toupper(substring(s.new,1,1)), substring(s.new,2), sep="", collapse="'")
	s.new <- unlist(strsplit(s.new, split="[.]"))
	if (length(s.new) > 1) s.new <- paste(toupper(substring(s.new,1,1)), substring(s.new,2), sep="", collapse=".")
	return(s.new)
} ### END capwords
