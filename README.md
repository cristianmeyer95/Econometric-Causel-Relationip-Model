# Econometric-Causel-Relationip-Model
This is a DIF-in_DIF model coded in python, to evaluate the causal relationship between lagged rainfall on civil disputes in agricultural regions. 
library(haven)
library(memisc)
library(reshape2)

m1.1=lm(rainiv$LaggedGDPGrowth~rainiv$InternalConflict+rainiv$InitialGDP+rainiv$Democracy+rainiv$Mountains+rainiv$EthnicFrac+rainiv$ReligiousFrac)
summary(m1.1)
m1.2=lm(rainiv$LaggedGDPGrowth~rainiv$LaggedRainfallGrowth)
summary(m1.2)
mtable(m1.1,m1.2)

pd0=subset(rainiv, rainiv$year==1981)
out1=subset(rainiv, rainiv$year==1999, select=c(rainiv$InternalConflict,rainiv$LaggedGDPGrowth))
names(out1)[2]=rainiv$LaggedGDPGrowth 
mtable(m1.1,m1.2)
pd0=merge(pd0, out1, by=rainiv$LaggedGDPGrowth)
m1.3=lm(rainiv$LaggedRainfallGrowth~rainiv$LaggedGDPGrowth)
summary(m1.3)
m1.4=lm(rainiv$LaggedGDPGrowth~rainiv$InternalConflict+rainiv$InitialGDP+rainiv$Democracy+rainiv$Mountains+rainiv$EthnicFrac+rainiv$ReligiousFrac+rainiv$year, data = rainiv  [rainiv$year==1981, ])
summary(m1.4)
m1.5=lm(rainiv$LaggedGDPGrowth~rainiv$InternalConflict+rainiv$InitialGDP+rainiv$Democracy+rainiv$Mountains+rainiv$EthnicFrac+rainiv$ReligiousFrac+rainiv$year, data = rainiv  [rainiv$year==1981, ])
summary(m1.5)
griffen=max(rainiv$year)
cristian=min(rainiv$year)
m2=lm(hope$InCollege~hope$Age+hope$After+hope$Age18)
mtable(m2)
sum(hope$AfterGeorgia)
sum(hope$Georgia-hope$AfterGeorgia)
mean(hope$Georgia-hope$AfterGeorgia)

m2.1=lm(hope$InCollege~hope$Georgia+hope$After+hope$AfterGeorgia)
mtable(m2.1)

m2.2=lm(hope$InCollege~hope$Georgia+hope$After+hope$AfterGeorgia-hope$Age-hope$After-hope$Age18)
mtable(m2.2)

