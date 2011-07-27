# TODO: Add comment
# 
# Author: jeroen
###############################################################################


lazega <- function(){
	#download data
	dir.create("Lezaga");
	setwd("Lezaga");
	download.file("http://www.stats.ox.ac.uk/~snijders/siena/LazegaLawyers.zip", "LazegaLawyers.zip")
	unzip("LazegaLawyers.zip")
	
	#Attributes
	ELattr <- read.table("ELattr.dat");
	names(ELattr) <- c("seniority", "status", "gender", "office", "years", "age", "practice", "law_school");
	ELattr$status <- as.character(factor(ELattr$status, levels=c(1,2), labels=c("partner", "associate")));
	ELattr$gender <- as.character(factor(ELattr$gender, levels=c(1,2), labels=c("man", "woman")));
	ELattr$office <- as.character(factor(ELattr$office, levels=c(1,2,3), labels=c("Boston", "Hartford", "Providence")));
	ELattr$practice <- as.character(factor(ELattr$practice, levels=c(1,2), labels=c("ligitation", "corporate")));
	ELattr$law_school <- as.character(factor(ELattr$law_school, levels=c(1,2,3), labels=c("HardvardYale", "ucon", "other")));
	ELattr36 <- subset(ELattr, status=="partner");
	
	#Construct networks
	ELwork <- network(as.matrix(read.table("ELwork.dat")), ELattr, directed=FALSE);
	ELwork36 <- network(as.matrix(read.table("ELwork36.dat")), ELattr36, directed=FALSE);
	
	ELadv <- network(as.matrix(read.table("ELadv.dat")), ELattr, directed=TRUE);
	ELadv36 <- network(as.matrix(read.table("eladv36.dat")), ELattr36, directed=TRUE);
	
	ELfriend <- network(as.matrix(read.table("ELfriend.dat")), ELattr, directed=TRUE);
	ELfriend36 <- network(as.matrix(read.table("elfriend36.dat")), ELattr36, directed=TRUE);	
}
