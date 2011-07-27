#include
library(lmergm);
data(hansell);

#baseline model.
hansell.0 <- ergm(hansell ~ edges + transitiveties + match('sex'), MPLEonly=T);

#we can explain some of the transitivities by popularity/activity variation.
hansell.1 <- lmergm(hansell ~ edges + transitiveties + match('sex') + (edges | sender) + (edges | receiver));

#lets make homophily random
hansell.2 <- lmergm(hansell ~ edges + transitiveties + match('sex') + (edges + match('sex') | sender) + (edges  + match("sex") | receiver));

#plot some random effects

par(mfrow=c(1,2))
plot(ranef(hansell.2)$sender, main="Sender", xlab="homophily", ylab="activity")
plot(ranef(hansell.2)$receiver, main="Receiver", xlab="homophily", ylab="popularity")

#cross level interaction
mutual.1 <- lmergm(hansell ~ edges + mutual + (edges + mutual | sender) + (edges + mutual | receiver));
mutual.2 <- lmergm(hansell ~ edges + mutual * nodeifactor("sex") + (edges + mutual | sender) + (edges + mutual | receiver))
mutual.3 <- lmergm(hansell ~ edges + mutual * nodeifactor("sex") + mutual * nodeofactor("sex") + (edges + mutual | sender) + (edges + mutual | receiver))

#comparison model 
hansell.ca1 <- ergm(hansell ~ receiver(0));
hansell.ca2 <- ergm(hansell ~ sender(0));
par(mfrow=c(1,2));
hist(coef(hansell.ca2), main="sender");
hist(coef(hansell.ca1), main="receiver");


hansell.cb1 <- lmergm(hansell ~ edges + (1|receiver))
hansell.cb2 <- lmergm(hansell ~ edges + (1|sender))

par(mfrow=c(1,2));
plot(coef(hansell.ca2), coef(hansell.cb2)$sender[[1]], main="sender", xlab="Fixed coefficients", ylab="Random BLUPS", xlim=c(-3,1), ylim=c(-3,1))
abline(0,1, col="red")

plot(coef(hansell.ca1), coef(hansell.cb1)$receiver[[1]], main="receiver", xlab="Fixed coefficients", ylab="Random BLUPS", xlim=c(-3,1), ylim=c(-3,1))
abline(0,1, col="red")

#coefficients table:

allcoef <- data.frame("popularity.fixed"= coef(hansell.ca1), "popularity.random"=coef(hansell.cb1)$receiver[[1]], 
		   "activity.fixed" = coef(hansell.ca2), "activity.random" = coef(hansell.cb2)$sender[[1]]);

#table
allmodels <- list(hansell.0, hansell.1, hansell.2);
mytable <- as.data.frame(cbind(M0=coef(hansell.0),M1=fixef(hansell.1), M2=fixef(hansell.2)));
mytable[c("u0","u2","u12"),] <- NA;
mytable[c("v0","v2","v12"),] <- NA;
mytable["AIC",] <- sapply(allmodels, AIC);
mytable["BIC",] <- sapply(allmodels, BIC);
mytable["Deviance",] <- -2*sapply(allmodels, logLik);

mytable["u0", "M1"] <- VarCorr(hansell.1)$sender[1,1];
mytable["u0", "M2"] <- VarCorr(hansell.2)$sender[1,1];
mytable["v0", "M1"] <- VarCorr(hansell.1)$receiver[1,1];
mytable["v0", "M2"] <- VarCorr(hansell.2)$receiver[1,1];

mytable["u2", "M2"] <- VarCorr(hansell.2)$sender[2,2];
mytable["v2", "M2"] <- VarCorr(hansell.2)$receiver[2,2];
mytable["u12", "M2"] <- VarCorr(hansell.2)$sender[1,2];
mytable["v12", "M2"] <- VarCorr(hansell.2)$receiver[1,2];

#do some plots
plot(ranef(hansell.3))

#data and formula for the logistic regression.
#attributes(mymodel1)$frame
#attributes(mymodel1)$call

#test ANOVA
mymodel2 <- lmergm(hansell ~ edges + mutual + match('sex') + (edges + match("sex") | sender) + (edges + match("sex") | receiver));
anova(mymodel1, mymodel2);

#test for covariates
mymodel3 <- lmergm(hansell ~ edges + match('sex') + (edges + match("sex") | sender) + (edges + match("sex") | receiver));
mymodel4 <- lmergm(hansell ~ edges + match('sex') + (edges | sender) + (0 + match('sex') | sender) + (edges  | receiver) + (0 + match("sex") | receiver));
anova(mymodel3, mymodel4)

#what if we model everything random
mymodel5.3 <- lmergm(hansell ~ edges + transitiveties + (edges + transitiveties | sender) + (edges + transitiveties | receiver));

#add a crosslevel predictor
mymodel5.4 <- lmergm(hansell ~ edges + transitiveties * nodeofactor("sex") + (edges | sender) + (edges | receiver));


