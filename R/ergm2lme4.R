# Basically the goal of this function is to convert an lmergm formula to a proper lme4 formula
# The tricky part is that ergm creates unpredictable variable names (i.e. match('sex') becomes nodematch.sex)
# So therefore we actually simulate an internal part of the ergm process to find out what the name will be.
#
# Author: jeroen
###############################################################################

#very ugly
calltostring <- function(call) {
	capture.output(print(call));
}

#convert the ergm formula to lme4 format
ergm2lme4 <- function(formula){
	
	#predictorpart
	predictorstring <- as.character(formula)[[3]]
	
	#get terms involved in the model
	nobarsformula <- suball(formula);
	datanames <- term.list.formula(nobarsformula[[3]]);
	dataterms <- sapply(datanames, calltostring);	
	
	#note that this does something slightly different for functions. We need this to extract '1' and '0' terms.
	datastrings <- lapply(datanames, as.character);
	datastrings <- sapply(datastrings, paste, collapse=".");
	
	#exclude some stuff from the formula
	exceptions <- datastrings %in% c("sender", "receiver", "0", "1", "edges");
	uniqueterms <- unique(dataterms[!exceptions]);
	
	if(length(uniqueterms) > 0){
		cleanformula <- formula(paste(as.character(formula[[2]]), "~", paste(uniqueterms, collapse="+")));
		
		#dataterms should be converted to coefnames
		tnw <- ergm.getnetwork(cleanformula)
		tmodel <- ergm.getmodel(cleanformula, tnw, drop = FALSE, initialfit = TRUE)
		coefnames <- sapply(tmodel$terms,"[[", "coef.names");
		
		#loop over all the terms
		for(i in 1:length(uniqueterms)){
			predictorstring <- gsub(uniqueterms[i], paste(coefnames[i], collapse="+"), predictorstring, fixed=T);	
		}
	}
	
	#replace the constant terms
	predictorstring <- gsub("edges", 1, predictorstring, fixed=T);
	
	#return
	newformula <- paste("y ~ ", predictorstring);
	return(newformula);
}