# TODO: Add comment
# 
# Author: jeroen
###############################################################################

mininet <- network(matrix(c(0,0,1,1,0,1,0,1,0),3),directed=T, vertex.attr=list(sex=c("M","F","M")));
par(mar=c(0,0,0,0))
plot(mininet, edge.lwd=5, vertex.cex=5, arrowhead.cex=3, vertex.col="sex", displaylabels=T, label.cex=2)
buildcmatrix(mininet~match("sex") + transitiveties)