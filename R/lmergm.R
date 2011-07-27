# TODO: Add comment
# 
# Author: jeroen
###############################################################################


lmergm <- function(formula, verbose=FALSE, data=NULL, ...){
	
	if(!is.null(data)){
		stop("Data argument has to be NULL. Dataframe is part of the formula. Please read ?ergm")
	}
	
	if(verbose) cat("Building covariates matrix...\n")		
	cmatrix <- buildcmatrix(formula, verbose=verbose);
	
	#create the lme4 formula
	if(verbose) cat("Converting formula...\n")	
	lmeformula <- ergm2lme4(formula);
	if(verbose) cat(lmeformula,"\n")		

	
	#create lme4 model:
	if(verbose) cat("Calling lme4...\n")		
	lmecall <- as.expression(parse(text=paste("glmer(", lmeformula, ", data=cmatrix, family='binomial', verbose=verbose, ...)", sep="")));
	lme.obj <- eval(lmecall);
	
	#return the lme4 obj
	return(lme.obj);
}
