`capwords` <-
function(x,
	special.words = c("ELA","I", "II", "III", "IV", "CCSD", "CUSD", "CUD", "USD", "PSD", "UD", "ESD", "DCYF", "EMH", "HS", "MS", "ES", "SES", "IEP", "ELL", "MAD", "PARCC", "SBAC", "SD", "SWD", "US", "SGP", "SIMEX", "SS", "SAT", "PSAT", "WIDA", "ACCESS", "WIDA-ACCESS")) {

	if (is.null(x)) return(NULL)
	if (is.na(x)) return(NA)
	if (identical(x, " ")) return(" ")
	x <- gsub("_", " ", x)
	x <- gsub("[.]", " ", x)
	x <- gsub("^ *|(?<= ) | *$", "", x, perl=T)
	x <- gsub("[(]", "( ", x)
	x <- gsub("[)]", " )", x)

	if (identical(x, "")) {
		return("")
	} else {
		my.split <- function(words, split.character) {
			test.numeric <- function(tmp.x) grepl("[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][0-9]+", tmp.x)
			tmp.split <- unlist(strsplit(words, split=split.character))
			tmp.split.special.words.index <- which(!tmp.split %in% special.words)
			if (any(test.numeric(tmp.split))) tmp.split.special.words.index <- setdiff(tmp.split.special.words.index, which(test.numeric(tmp.split))) 
			return(list(tmp.split=tmp.split, tmp.split.special.words.index=tmp.split.special.words.index))
		}
		s <- my.split(x, " ")
		s[[1L]][s[[2L]]] <- paste0(toupper(substring(s[[1L]][s[[2L]]],1L,1L)), tolower(substring(s[[1L]][s[[2L]]],2L)))
		s.new <- paste(s[[1L]], collapse=" ")
		s.new <- unlist(strsplit(s.new, split="-"))
		if (length(s.new) > 1L) s.new <- paste0(toupper(substring(s.new,1L,1L)), substring(s.new,2L), collapse="-")
		if (length(unlist(strsplit(s.new, split="'"))) > 1L & nchar(unlist(strsplit(s.new, split="'"))[2L]) > 1L) {
			s.new <- unlist(strsplit(s.new, split="'"))
			s.new <- paste0(toupper(substring(s.new,1L,1L)), substring(s.new,2L), collapse="'")
		}
		s.new <- unlist(strsplit(s.new, split="[.]"))
		if (length(s.new) > 1L) s.new <- paste0(toupper(substring(s.new,1L,1L)), substring(s.new,2L), collapse=".")
		s.new <- gsub(" [)]", ")", gsub("[(] ", "(", s.new))
		return(s.new)
	}
} ### END capwords
