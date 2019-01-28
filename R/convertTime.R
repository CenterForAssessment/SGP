`convertTime` <-
function(tmp.time) {
	tmp <- tail(c(0, 0, as.numeric(gsub("s", "", unlist(strsplit(head(unlist(strsplit(timetaken(tmp.time), " ")), 1), ":"))))), 3)
	tmp.label <- c("Hour", "Minute", "Second")
	tmp.label[which(tmp!=1)] <- paste0(tmp.label, "s")[which(tmp!=1)]
	return(paste(paste(tmp[tmp!=0], tmp.label[tmp!=0]), collapse=", "))
} ### END convertTime
