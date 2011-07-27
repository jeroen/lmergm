# TODO: Add comment
# 
# Author: jeroen
###############################################################################


extract.frame <- function(...){
	UseMethod("extract.frame")
}

extract.frame.mer <- function(obj){
	return(attributes(obj)$frame);
}