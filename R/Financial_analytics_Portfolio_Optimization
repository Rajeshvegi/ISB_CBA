rm(list = ls()) 

#### Marcowiz

library('quadprog')
library('MASS')
mu         <- matrix(c(0.0008,
                0.0067,
                0.0641,
                0.0408,
                0.0743,
                0.0370,
                0.0480,
                0.0660),8,1)
colnames(mu)<-c("mean")
rownames(mu)<-c("US Bonds","Intnl Bonds","US Large Growth","US Large Value","US Small Growth",
                "US small Value","Intnl Dev Equity","Intnl Emerg. Equity")



cov  <- matrix(c(0.001005,0.001328,-0.000579,-0.000675,0.000121,0.000128,-0.000445,-0.000427,
                  0.001328,0.007277,-0.001307,-0.000610,-0.002237,-0.000989,0.001442,-0.001535,
                  -0.000579,-0.001307,0.059852,0.027588,0.063497,0.023036,0.032967,0.048039,
                  -0.000675,-0.000610,0.027588,0.029609,0.026572,0.021465,0.020697,0.029854,
                  0.000121,-0.002237,0.063497,0.026572,0.102488,0.042744,0.039943,0.065994,
                  0.000128,-0.000989,0.023036,0.021465,0.042744,0.032056,0.019881,0.032235,
                  -0.000445,0.001442,0.032967,0.020697,0.039943,0.019881,0.028355,0.035064,
                  -0.000427,-0.001535,0.048039,0.029854,0.065994,0.032235,0.035064,0.079958),8,8)

colnames(cov)<-c("US Bonds","Intnl Bonds","US Large Growth","US Large Value","US Small Growth",
                 "US small Value","Intnl Dev Equity","Intnl Emerg. Equity")
rownames(cov)<-c("US Bonds","Intnl Bonds","US Large Growth","US Large Value","US Small Growth",
                 "US small Value","Intnl Dev Equity","Intnl Emerg. Equity")
#Define the QP
#
# The quadprog package needs to be installed and loaded before solve.QP is run
# use: install.packages("quadprog") and then library("quadprog")
# 

#R <- 0.05

D    <- 2*cov
d    <- c(0,0,0,0,0,0,0,0)
A    <- matrix( c(1,1,1,1,1,1,1,1,
                  -1,-1,-1,-1,-1,-1,-1,-1,
                  mu ,
                  -mu , 
                  1,0,0,0,0,0,0,0,
                  0,1,0,0,0,0,0,0,
                  0,0,1,0,0,0,0,0,
                  0,0,0,1,0,0,0,0,
                  0,0,0,0,1,0,0,0,
                  0,0,0,0,0,1,0,0,
                  0,0,0,0,0,0,1,0,
                  0,0,0,0,0,0,0,1),8,12)


varP=vector()
MsigmaP=vector()
mw1=vector()
mw2=vector()
mw3=vector()
mw4=vector()
mw5=vector()
mw6=vector()
mw7=vector()
mw8=vector()
mu
#Rs=seq(0.0008,0.066,0.00326)
MRs=seq(min(mu)+0.1^10,max(mu)-0.1^10,length.out=20);



for (i in 1:length(MRs)) {
  R   <- MRs[i]
  b0   <- c(1,-1,R,-R,0,0,0,0,0,0,0,0)
  
      qpSol=solve.QP(D,d,A,b0)
  
  varP[i] = qpSol$value
  MsigmaP[i] = sqrt(varP[i])
  mw1[i]=qpSol$solution[1]
  mw2[i]=qpSol$solution[2]
  mw3[i]=qpSol$solution[3]
  mw4[i]=qpSol$solution[4]
  mw5[i]=qpSol$solution[5]
  mw6[i]=qpSol$solution[6]
  mw7[i]=qpSol$solution[7]
  mw8[i]=qpSol$solution[8]
  
  
}

#Efficient frontier Plot 
plot(MsigmaP,MRs,type = 'l',lty = 1,lwd=3, xlab = 'Risk',ylab = 'Returns', main = 'Markowitz Efficient Frontier',col = 'blue')

#Weights of portfolio assets vs expected returns in one plot
par(mfrow = c(1,1))
plot(MRs,mw1,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = 'Weights of Assets' , main = 'Visualization of Asset Weights vs Returns', col = 'red')
lines(MRs,mw2,"l",lty = 1,lwd=3,col = 'black')
lines(MRs,mw3,"l",lty = 1,lwd=3,col='green')
lines(MRs,mw4,"l",lty = 1,lwd=3,col = 'blue')
lines(MRs,mw5,"l",lty = 1,lwd=3,col='grey')
lines(MRs,mw6,"l",lty = 1,lwd=3,col = 'dark green')
lines(MRs,mw7,"l",lty = 1,lwd=3,col='violet')
lines(MRs,mw8,"l",lty = 1,lwd=3,col='yellow')
legend('topleft', c("US Bonds","Intnl Bonds","US Large Growth","US Large Value",
                    "US Small Growth","US small Value","Intnl Dev Equity","Intnl Emerg. Equity"), pch = 17, 
       col = c('red','black','green','blue','grey','dark green','violet','yellow'), text.col = c('red','black','green','blue','grey','dark green','violet','yellow'), cex = .6)




#Weights of portfolio assets vs expected returns in separate plots
par(mfrow = c(2,4))
plot(MRs,mw1,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'US Bonds', col = 'red')
plot(MRs,mw2,type = 'l', lty = 1,lwd=3,xlab ='' ,ylab = '' , main = "Int'l Bonds", col = 'black')
plot(MRs,mw3,type = 'l', lty = 1, lwd=3,xlab ='' ,ylab = '' , main = "US Large Growth", col = 'green')
plot(MRs,mw4,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = '' , main = "US Large Value", col = 'blue')
plot(MRs,mw5,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = 'Weight' , main = "US Small Growth", col = 'grey')
plot(MRs,mw6,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = '' , main = "US Small Value", col = 'dark green')
plot(MRs,mw7,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = '' , main = "Int'l Dev Equity", col = 'violet')
plot(MRs,mw8,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = '' , main = "Int'l Emerg Equity", col = 'yellow')

##### Black Litterman Model

tscalar <- 0.25
C <- cov
var1 <- ginv(tscalar*C)
  P <- matrix( c(0,0,0,
                 0,0,0,
                 0,0,-1,
                 0,0,0,
                   0,0,1,
                 1,0,0,
                 0,-1,0,
                 0,1,0),3,8)
Q <- c(0.041,0.016,0.008)
Omega <- matrix(c(0.000801,0,0,
                  0,0.009546,0,
                  0,0,0.00084),3,3)
var2 <- t(P) %*% ginv(Omega) %*% P
N1 <- ginv(var1 + var2)
m <- mu
var3 <- ginv(tscalar*C) %*% m
var4 <- t(P) %*% ginv(Omega) %*% Q

N2 <- (var3 + var4)

mhat <- N1 %*% N2
rownames(mhat)<-c("US Bonds","Intnl Bonds","US Large Growth","US Large Value","US Small Growth",
                "US small Value","Intnl Dev Equity","Intnl Emerg. Equity")

#Define the QP
Dmat <- 2*C
dvec <- rep(0,8)
Amat <- matrix(c(mhat,-mhat,rep(1,8),rep(-1,8),diag(length(mhat))),8,12)

# compute efficient frontier for eight stocks
varP=vector()
bsigmaP=vector()
bw1=vector()
bw2=vector()
bw3=vector()
bw4=vector()
bw5=vector()
bw6=vector()
bw7=vector()
bw8=vector()

#Expected Returns 20 values
BRs=seq(min(mhat)+0.1^10,max(mhat)-0.1^10,length.out=20);

for (i in 1:length(BRs)) {
  R=BRs[i]
  bvec <- c(R,-R,1,-1,0,0,0,0,0,0,0,0)
  qpSol=solve.QP(Dmat,dvec,Amat,bvec)
  varP[i]=qpSol$value
  bsigmaP[i]=sqrt(varP[i])
  bw1[i]=qpSol$solution[1];
  bw2[i]=qpSol$solution[2];
  bw3[i]=qpSol$solution[3];
  bw4[i]=qpSol$solution[4];
  bw5[i]=qpSol$solution[5];
  bw6[i]=qpSol$solution[6];
  bw7[i]=qpSol$solution[7];
  bw8[i]=qpSol$solution[8];
  
}

#Efficient frontier Plot 
plot(bsigmaP,BRs,type = 'l',lty = 1,lwd=3, xlab = 'Risk',ylab = 'Returns', main = 'Black Litterman Mean Variance',col = 'red')

#Weights of portfolio assets vs expected returns in one plot
par(mfrow = c(1,1))
plot(BRs,bw1,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = 'Weights of Assets' , main = 'Visualization of Asset Weights vs Returns', col = 'red')
lines(BRs,bw2,"l",lty = 1,lwd=3,col = 'black')
lines(BRs,bw3,"l",lty = 1,lwd=3,col='green')
lines(BRs,bw4,"l",lty = 1,lwd=3,col = 'blue')
lines(BRs,bw5,"l",lty = 1,lwd=3,col='grey')
lines(BRs,bw6,"l",lty = 1,lwd=3,col = 'dark green')
lines(BRs,bw7,"l",lty = 1,lwd=3,col='violet')
lines(BRs,bw8,"l",lty = 1,lwd=3,col='yellow')
legend('topleft', c("US Bonds","Intnl Bonds","US Large Growth","US Large Value",
                    "US Small Growth","US small Value","Intnl Dev Equity","Intnl Emerg. Equity"), pch = 17, 
       col = c('red','black','green','blue','grey','dark green','violet','yellow'), text.col = c('red','black','green','blue','grey','dark green','violet','yellow'), cex = .6)

#Weights of portfolio assets vs expected returns in separate plots
par(mfrow = c(2,4))
plot(BRs,bw1,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'US Bonds', col = 'red')
plot(BRs,bw2,type = 'l', lty = 1,lwd=3,xlab ='' ,ylab = '' , main = "Int'l Bonds", col = 'black')
plot(BRs,bw3,type = 'l', lty = 1, lwd=3,xlab ='' ,ylab = '' , main = "US Large Growth", col = 'green')
plot(BRs,bw4,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = '' , main = "US Large Value", col = 'blue')
plot(BRs,bw5,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = 'Weight' , main = "US Small Growth", col = 'grey')
plot(BRs,bw6,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = '' , main = "US Small Value", col = 'dark green')
plot(BRs,bw7,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = '' , main = "Int'l Dev Equity", col = 'violet')
plot(BRs,bw8,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,ylab = '' , main = "Int'l Emerg Equity", col = 'yellow')


############################################################
## Comparison of Marcowitz and Black Litterman Model
############################################################

## Comparison of efficient frontiers 

plot(MsigmaP,MRs,type = 'l', lty = 1,lwd=3, xlab ='Returns' ,
     ylab = 'Weights of Assets' , 
     main = ' Asset Weights vs Returns', col = 'red')
lines(bsigmaP,BRs,"l",lty = 1,lwd=3,col = 'blue')
legend('bottomright', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
       col = c('red','blue'), text.col = c('red','blue'), cex = .6)


## Comparison of Stocks for each Asset weight
par(mfrow = c(2,2))

plot(BRs,bw1,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'US Bonds', col = 'blue')
lines(MRs,mw1,"l",lty = 1,lwd=3,col = 'red')
 # legend('topright', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
 #        col = c('red','blue'), text.col = c('red','blue'), cex = .6)

plot(BRs,bw2,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'Intnational Bonds', col = 'blue')
lines(MRs,mw2,"l",lty = 1,lwd=3,col = 'red')
# legend('topright', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
#        col = c('red','blue'), text.col = c('red','blue'), cex = .5)

plot(BRs,bw3,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'US Large Growth', col = 'blue')
lines(MRs,mw3,"l",lty = 1,lwd=3,col = 'red')
# legend('topleft', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
#        col = c('red','blue'), text.col = c('red','blue'), cex = .5)

plot(BRs,bw4,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'US Large Value', col = 'blue')
lines(MRs,mw4,"l",lty = 1,lwd=3,col = 'red')
# legend('topleft', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
#        col = c('red','blue'), text.col = c('red','blue'), cex = .5)

plot(BRs,bw5,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'US Small Growth', col = 'blue')
lines(MRs,mw5,"l",lty = 1,lwd=3,col = 'red')
 # legend('topleft', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
 #        col = c('red','blue'), text.col = c('red','blue'), cex = .5)

plot(BRs,bw6,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = 'US Small Value', col = 'blue')
lines(MRs,mw6,"l",lty = 1,lwd=3,col = 'red')
# legend('topleft', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
#        col = c('red','blue'), text.col = c('red','blue'), cex = .5)

plot(BRs,bw7,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = "Int'l Dev Equity", col = 'blue')
lines(MRs,mw7,"l",lty = 1,lwd=3,col = 'red')
# legend('topleft', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
#        col = c('red','blue'), text.col = c('red','blue'), cex = .5)

plot(BRs,bw8,type = 'l', lty = 1,lwd=3, xlab ='' ,ylab = 'Weight' , main = "Int'l Emerg Equity", col = 'blue')
lines(MRs,mw7,"l",lty = 1,lwd=3,col = 'red')
# legend('topleft', c("Marcowitz Efficient Frontier","Black Litterman Efficient frontier"), pch = 17, 
#        col = c('red','blue'), text.col = c('red','blue'), cex = .5)


