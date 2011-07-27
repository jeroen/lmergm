# TODO: Add comment
# 
# Author: jeroen
###############################################################################


library(lmergm);
data(ELfriend36);
friends <- ELfriend36;
list.vertex.attributes(friends)

#random intercepts model
mymodel1 <- lmergm(friends ~ edges + (edges | sender) + (edges | receiver))

#add fixed and random effect for mutuality
mymodel2 <- lmergm(friends ~ edges + mutual + (edges + mutual | sender) + (edges + mutual | receiver))
anova(mymodel1, mymodel2)

#add fixed and random effects for transitivity
mymodel3 <- lmergm(friends ~ edges + mutual + transitive + (edges + mutual + transitive | sender) + (edges + mutual + transitive | receiver), verbose=T)
anova(mymodel2, mymodel3)

#remove random effect for mutual. Difference is not significant.
mymodel4 <- lmergm(friends ~ edges + mutual + transitive + (edges + transitive | sender) + (edges + transitive | receiver), verbose=T)
anova(mymodel3, mymodel4)

#add some fixed effects that are described in Sneijders et al 2004
mymodel5 <- lmergm(friends ~ edges + mutual + transitive  + match("office") + match("practice") + absdiff("years") + (edges + transitive | sender) + (edges + transitive | receiver), verbose=T)
anova(mymodel4, mymodel5);

#remove covariances. Not significant.
mymodel6 <- lmergm(friends ~ edges + mutual + transitive  + match("office") + match("practice") + absdiff("years") + (edges | sender) + (0 + transitive | sender) + (edges | receiver) + (0 + transitive | receiver), verbose=T)
anova(mymodel5, mymodel6)

#parametric bootstrapping of this model to do significance testing
pboot <- function(m0,m1) {
	s <- simulate(m0);
	L0 <- logLik(refit(m0,s));
	L1 <- logLik(refit(m1,s));
	2*(L1-L0);
}

#observice difference
obsdev <- c(2*(logLik(mymodel5)-logLik(mymodel6)))

#simulate differences
set.seed(1001)
lazega_PB <- replicate(500,pboot(mymodel6, mymodel5))

library(lattice)
qqmath(lazega_PB, distribution=function(p) qchisq(p,df=1),
		type="l",
		prepanel = prepanel.qqmathline,
		panel = function(x, ...) {
			panel.qqmathline(x, ...)
			panel.qqmath(x, ...)
		})

pchisq(obsdev,df=1,lower.tail=FALSE)
mean(lazega_PB>obsdev)

#remove transitive random effect.
mymodel9 <- lmergm(friends ~ edges + mutual + transitive + match("practice") + match("office")  + (edges | sender) + (edges | receiver), verbose=T)
ergm.9 <- ergm(friends ~ edges + mutual + transitive + match("practice") + match("office"), MPLEonly=T)

#try a crosslevel interaction
mymodel11 <- lmergm(friends ~ edges + match("office") + (match("office") + edges | sender) + (match("office") + edges | receiver))

#negative correlation: one becomes popular by not being homophilous
mymodel12 <- lmergm(friends ~ edges + match("office") + (match("office") + edges | receiver))

#transitivity is more important for women than men.
mymodel13 <- lmergm(ELfriend ~ edges + transitive * nodeifactor("gender") + (edges + transitive | receiver) + (edges + transitive | sender));

