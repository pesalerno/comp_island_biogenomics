#CLEARING THE WORKSPACE
rm(list = ls())

#To change directory press command D



stats<-read.table("islands-matrix-4b.txt", h=T) ##full final matrix excluding the three DIndex outliers (same site, three species)
summary(stats)
stats



##############################################
####           Pi and Island Area         ####
##############################################
Pi_area<-lm(Pi~area, data=stats)## log.Fst and DI
summary(Pi_area)
##############################################
plot(stats$Pi~stats$area, pch=c(1,2,3,4)[as.numeric(stats$species)], col=c(1,2,3,4,5,6,7,8)[as.numeric(stats$island)])
abline(lm(stats$Pi~stats$area))
legend("topleft",
	legend=c("Aphelocoma","Pseudacris", "Urocyon", "Xantusia"),
	pch=c(1,2,3,4))
##############################################
ALL SPECIES AS SEPARATE RELATIONSHIPS
##############################################
library(ggplot2)
p<-qplot(area, Pi, shape=species, color=species, data=stats)
	p + geom_smooth(method="lm", se=FALSE, fullrange=T)
help(qplot)
##############################################



##############################################
####      Pi and Distance to Mainland     ####
##############################################
Pi_distMain<-lm(Pi~dist_main, data=stats)## log.Fst and DI
summary(Pi_distMain)
##############################################
plot(stats$Pi~stats$dist_main, pch=c(1,2,3,4)[as.numeric(stats$species)], col=c(1,2,3,4,5,6,7,8)[as.numeric(stats$island)])
abline(lm(stats$Pi~stats$dist_main))
legend("topright",
	legend=c("Aphelocoma","Pseudacris", "Urocyon", "Xantusia"),
	pch=c(1,2,3,4))
##############################################
##############################################
ALL SPECIES AS SEPARATE RELATIONSHIPS
##############################################
library(ggplot2)
p<-qplot(dist_main, Pi, shape=species, color=species, data=stats)
	p + geom_smooth(method="lm", se=FALSE, fullrange=T)
help(qplot)
##############################################


##############################################
####         Pi and Plant Diversity       ####
##############################################
Pi_plantDivers<-lm(Pi~plant_divers, data=stats)## log.Fst and DI
summary(Pi_plantDivers)
##############################################
plot(stats$Pi~stats$plant_divers, pch=c(1,2,3,4)[as.numeric(stats$species)], col=c(1,2,3,4,5,6,7,8)[as.numeric(stats$island)])
abline(lm(stats$Pi~stats$plant_divers))
legend("topleft",
	legend=c("Aphelocoma","Pseudacris", "Urocyon", "Xantusia"),
	pch=c(1,2,3,4))
##############################################
ALL SPECIES AS SEPARATE RELATIONSHIPS
##############################################
library(ggplot2)
p<-qplot(plant_divers, Pi, shape=species, color=species, data=stats)
	p + geom_smooth(method="lm", se=FALSE, fullrange=T)
help(qplot)
##############################################


##############################################
###   Pi and Distance to Nearest Island    ###
##############################################
Pi_distIsland<-lm(Pi~dist_island, data=stats)## log.Fst and DI
summary(Pi_distIsland)

##############################################
plot(stats$Pi~stats$dist_island, pch=c(1,2,3,4)[as.numeric(stats$species)], col=c(1,2,3,4,5,6,7,8)[as.numeric(stats$island)])
abline(lm(stats$Pi~stats$dist_island))
legend("topright",
	legend=c("Aphelocoma","Pseudacris", "Urocyon", "Xantusia"),
	pch=c(1,2,3,4))
##############################################
ALL SPECIES AS SEPARATE RELATIONSHIPS
##############################################
library(ggplot2)
p<-qplot(dist_island, Pi, shape=species, color=species, data=stats)
	p + geom_smooth(method="lm", se=FALSE, fullrange=T)
help(qplot)
##############################################
##
##
##
##
##
##
##############################################
####           Ar and Island Area         ####
##############################################
Ar_area<-lm(Ar~area, data=stats)## log.Fst and DI
summary(Ar_area)
##############################################
plot(stats$Ar ~stats$area, pch=c(1,2)[as.numeric(stats$species)], col=c(1,2,3,4,5,6)[as.numeric(stats$island)])
abline(lm(stats$Ar ~stats$area))
legend("bottomright",
	legend=c("Pseudacris", "Xantusia"),
	pch=c(1,2))
##############################################



##############################################
####      Ar and Distance to Mainland     ####
##############################################
Ar_distMain<-lm(Ar ~dist_main, data=stats)## log.Fst and DI
summary(Ar_distMain)
##############################################
plot(stats$Ar ~stats$dist_main, pch=c(1,2)[as.numeric(stats$species)], col=c(1,2,3,4,5,6)[as.numeric(stats$island)])
abline(lm(stats$Ar ~stats$dist_main))
legend("bottomright",
	legend=c("Pseudacris", "Xantusia"),
	pch=c(1,2))
##############################################



##############################################
####         Ar and Plant Diversity       ####
##############################################
Ar_plantDivers<-lm(Ar~plant_divers, data=stats)## log.Fst and DI
summary(Ar_plantDivers)
##############################################
plot(stats$Ar~stats$plant_divers, pch=c(1,2)[as.numeric(stats$species)], col=c(1,2,3,4,5,6)[as.numeric(stats$island)])
abline(lm(stats$Ar~stats$plant_divers))
legend("bottomright",
	legend=c("Pseudacris", "Xantusia"),
	pch=c(1,2))
##############################################



##############################################
###   Ar and Distance to Nearest Island    ###
##############################################
Ar_distIsland<-lm(Ar ~dist_island, data=stats)## log.Fst and DI
summary(Ar_distIsland)

##############################################
plot(stats$Ar ~stats$dist_island, pch=c(1,2)[as.numeric(stats$species)], col=c(1,2,3,4,5,6)[as.numeric(stats$island)])
abline(lm(stats$Ar ~stats$dist_island))
legend("topright",
	legend=c("Pseudacris", "Xantusia"),
	pch=c(1,2))
##############################################





###################################
###################################
####    ONE SAMPLE T-TESTS   ######
###################################
###################################

##DID HETEROZYGOSITY AND ALLELIC RICHNESS CHANGE/DECREASE
#OVERALL AS A RESULT OF THE FLOOD? 
help(t.test)

##a. Allelic richness
t.test(stats$scaled.delta.Ar, mu=0)
t.test(stats$scaled.delta.Ar, mu=0, alternative="less")

##b. Heterozygosity
t.test(stats$scaled.Delta.Ho, mu=0)
t.test(stats$scaled.Delta.Ho, mu=0, alternative="less")

##d. For B. bicaudatus only
BbiData<-read.table("Bbi-matrix.txt", h=T)
t.test(BbiData$scaled.delta.Ar, mu=0)
t.test(BbiData$scaled.delta.Ar, mu=0, alternative="less")
t.test(BbiData$scaled.Delta.Ho, mu=0)
t.test(BbiData$scaled.Delta.Ho, mu=0, alternative="less")

##e. For B. tricaudatus only
BtrData<-read.table("Btr-matrix.txt", h=T)
t.test(BtrData$scaled.delta.Ar, mu=0)
t.test(BtrData$scaled.delta.Ar, mu=0, alternative="less")
t.test(BtrData$scaled.delta.Ar, mu=0, alternative="greater")
t.test(BtrData$scaled.Delta.Ho, mu=0)
t.test(BtrData$scaled.Delta.Ho, mu=0, alternative="less")

##f. For D. coloradensis only
DcoData<-read.table("Dco-matrix.txt", h=T)
t.test(DcoData$scaled.delta.Ar, mu=0)
t.test(DcoData$scaled.delta.Ar, mu=0, alternative="less")
t.test(DcoData$scaled.Delta.Ho, mu=0)
t.test(DcoData$scaled.Delta.Ho, mu=0, alternative="less")

##g. For E. longimanus only
EloData<-read.table("Elo-matrix.txt", h=T)
t.test(EloData$scaled.delta.Ar, mu=0)
t.test(EloData$scaled.delta.Ar, mu=0, alternative="less")
t.test(EloData$scaled.Delta.Ho, mu=0)
t.test(EloData$scaled.Delta.Ho, mu=0, alternative="less")

##h. For H. pacifica only
HpaData<-read.table("Hpa-matrix.txt", h=T)
t.test(HpaData$scaled.delta.Ar, mu=0)
t.test(HpaData$scaled.delta.Ar, mu=0, alternative="less")
t.test(HpaData$scaled.Delta.Ho, mu=0)
t.test(HpaData$scaled.Delta.Ho, mu=0, alternative="less")

##h. For M. signata only
MsiData<-read.table("Msi-matrix.txt", h=T)
t.test(MsiData$scaled.delta.Ar, mu=0)
t.test(MsiData$scaled.delta.Ar, mu=0, alternative="less")
t.test(MsiData$scaled.Delta.Ho, mu=0)
t.test(MsiData$scaled.Delta.Ho, mu=0, alternative="less")


###################################
###################################
####   SIGN TEST (non-param) ######
###################################
###################################

library(BSDA)
SIGN.test(stats$scaled.delta.Ar, md=0)
SIGN.test(stats$scaled.delta.Ar, md=0, alternative="less")
SIGN.test(stats$scaled.Delta.Ho, md=0)
SIGN.test(stats$scaled.Delta.Ho, md=0, alternative="less")

help(SIGN.test)





###################################################
###################################################
#####     DISTRIBUTION AND TRANSFORMATIONS     ####
###################################################
###################################################

##checking distribution and transformations for Fst

hist(stats$pop.Fst)  
##stats$arc.Fst<-asin(stats$pop.Fst) ##arcsine transformation (supposed to be ideal for binomial)
##hist(stats$arc.Fst)
##stats$exp.Fst<-exp(stats$pop.Fst)
##hist(stats$exp.Fst)
stats$log.Fst<-log(stats$pop.Fst)
hist(stats$log.Fst)
##stats$log10.Fst<-log10(stats$pop.Fst)
##hist(stats$log10.Fst)


hist(stats$scaled.Delta.Ho) ##normal-ish
hist(stats$scaled.delta.Hs) ##normal-ish
hist(stats$scaled.delta.Ar) ##normal-ish
##stats$log.Ar<-log(stats$scaled.delta.Ar)
##hist(stats$log.Ar) ##makes no sense since I have negative values!!!!!! 
##stats



###################################################
###################################################
####  anovas & pairwise t-tests among species   ###
###################################################
###################################################
##Rhyacophila can't be added to t-tests
stats<-read.table("RAPID-matrix-y.txt", h=T) ##no Rhyacophila
stats
##1.- Fst 
aov2<-lm(pop.Fst~species,data=stats) 
anova(aov2)
FstTtests<-pairwise.t.test(stats$log.Fst, stats$species, paired=FALSE, p.adj="bonf")
FstTtests

##2.- Ho
aov1<-lm(scaled.Delta.Ho~species,data=stats) 
anova(aov1)
HoTtests<-pairwise.t.test(stats$scaled.Delta.Ho, stats$species, paired=FALSE, p.adj="bonf")
HoTtests


##3.- Ar
stats3<-read.table("RAPID-matrix-h-no-DI-outlier.txt", h=T) ##excludes missing Ar value and Rhyacophila
aov3<-lm(scaled.delta.Ar~species,data=stats3) 
anova(aov3)
ArTtests<-pairwise.t.test(stats3$scaled.delta.Ar, stats3$species, paired=FALSE, p.adj="bonf")
ArTtests


################################
################################
#####     CORRELATIONS     ####
################################
################################

cor.test(stats$log.Fst,stats$D.Index) ##0.340
cor.test(stats$log.Fst,stats$Precip) ##0.366
cor.test(stats$log.Fst,stats$taxa.lost) ##0.218
cor.test(stats$log.Fst,stats$Altitude) ##-0.349
#cor.test(stats$log.Fst,stats$mobility) ##0.1395

cor.test(stats$scaled.Delta.Ho,stats$D.Index) ##0.0312
cor.test(stats$scaled.Delta.Ho,stats$Precip) ##-0.0972
cor.test(stats$scaled.Delta.Ho,stats$taxa.lost) ##-0.0894
cor.test(stats$scaled.Delta.Ho,stats$Altitude) ##-0.1894
#cor.test(stats$scaled.Delta.Ho,stats$mobility) ##0.2549

#Ar data needs to exclude the missing data point from E.lo
stats2<-read.table("RAPID-matrix-f-no-DI-outlier.txt", h=T) ##excludes missing Ar values
cor.test(stats$scaled.delta.Ar,stats$D.Index) ##-0.080
cor.test(stats$scaled.delta.Ar,stats$Precip) ##0.1615
cor.test(stats$scaled.delta.Ar,stats$taxa.lost) ##-0.9123
cor.test(stats$scaled.delta.Ar,stats$Altitude) ##0.0607
#cor.test(stats$scaled.delta.Ar,stats$mobility)##0.1639

stats

############################################
############################################
#####     LINEAR REGRESSIONS -- Fst     ####
############################################
############################################

##SIMPLE LINEAR REGRESSIONS##
##modeling log.Fst for each variable separately and with no DI outlier
help(glm)
LogFst1<-lm(stats$log.Fst~stats$D.Index)## log.Fst and DI
summary(LogFst1) #p=0.071, F=3.535
LogFst2<-lm(stats$log.Fst~stats$Precip)## log.Fst and Precip
summary(LogFst2) #p=0.050, F=4.192
LogFst3<-lm(stats$log.Fst~stats$taxa.lost ) ##log.Fst and taxa lost
summary(LogFst3) ##p=0.256, F=1.346
LogFst4<-lm(stats$log.Fst~stats$Altitude) ##log.Fst and Altitude
summary(LogFst4) ##p=0.063, F=3.756
LogFst5<-lm(stats$log.Fst~stats$high.low) ##log.Fst and high.low
summary(LogFst5) #p=0.193
LogFst6<-lm(stats$log.Fst~stats$mobility) ##log.Fst and mobility
summary(LogFst6) #p=0.471
LogFst7<-lm(stats$log.Fst~stats$species) ##log.Fst and species
summary(LogFst7) #p=0.0006, F=6.171
LogFst8<-lm(stats$log.Fst~stats$Vuln) ##log.Fst and Vuln
summary(LogFst8) #p=0.0557, F=3.997

##MULTIPLE LINEAR REGRESSIONS WITH NO INTERACTION##
LogFst9<-lm(log.Fst~D.Index+species, data=stats)
summary(LogFst9)
lme4
LogFst10<-lm(log.Fst~Precip+species, data=stats)
summary(LogFst10)
LogFst11<-lm(log.Fst~Altitude+species, data=stats)
summary(LogFst11)
LogFst12<-lm(log.Fst~Vuln+species, data=stats) ##not sure if this model makes sense conceptually
LogFst17<-lm(log.Fst~taxa.lost+species, data=stats) ##not sure if this model makes sense conceptually
summary(LogFst17)

##MULTIPLE LINEAR REGRESSIONS WITH INTERACTION##
LogFst13<-lm(log.Fst~D.Index*species, data=stats)
summary(LogFst13)
lme4
LogFst14<-lm(log.Fst~Precip*species, data=stats)
summary(LogFst14)
LogFst15<-lm(log.Fst~Altitude*species, data=stats)
summary(LogFst15)
LogFst16<-lm(log.Fst~taxa.lost*species, data=stats)
summary(LogFst16)
###LogFst16<-lm(log.Fst~Vuln*species, data=stats)#conceptually this model makes no sense
###summary(LogFst16)


##############################################
##############################################
##modeling Fst with beta distribution and a GLM
##library(betareg)
##Fst1<-betareg(stats$pop.Fst~stats$D.Index)
##Fst1$coefficients
####forget it. dropping GLMs and weird regression modelling, too complicated to interpret.
#help(betareg)
##############################################
##############################################


############################################
############################################
#####     LINEAR REGRESSIONS -- Ho      ####
############################################
############################################

##modeling scaled.Delta.Ho for each variable separately
Ho1<-lm(stats$scaled.Delta.Ho~stats$D.Index)## 
summary(Ho1)
plot(Ho1)
Ho2<-lm(scaled.Delta.Ho~Precip, data=stats)
summary(Ho2)
Ho3<-lm(scaled.Delta.Ho~Altitude, data=stats)
summary(Ho3)
plot(Ho3)
Ho4<-lm(scaled.Delta.Ho~ taxa.lost, data=stats)
summary(Ho4)
Ho5<-lm(scaled.Delta.Ho~high.low, data=stats)
summary(Ho5)
Ho6<-lm(scaled.Delta.Ho~mobility, data=stats)
summary(Ho6)
Ho7<-lm(scaled.Delta.Ho~ Vuln, data=stats)
summary(Ho7)


############################################
############################################
#####     LINEAR REGRESSIONS -- Ar      ####
############################################
############################################
Ar1<-lm(stats$scaled.delta.Ar~stats$D.Index)## p=0.6855, F=0.1677
summary(Ar1)
plot(Ar1)
Ar2<-lm(scaled.delta.Ar ~Precip, data=stats)##p=0.2513, F=1.37
summary(Ar2)
Ar3<-lm(scaled.delta.Ar ~Altitude, data=stats)##p=0.8981, F=0.0167
summary(Ar3)
plot(Ar3)
Ar4<-lm(scaled.delta.Ar ~ taxa.lost, data=stats)##p=0.6718, F=0.1832
summary(Ar4)
Ar5<-lm(scaled.delta.Ar ~high.low, data=stats2)p=0.7589, F=0.096
summary(Ar5)
Ar6<-lm(scaled.delta.Ar ~mobility, data=stats2)#p=0.6126, F=0.2621
summary(Ar6)
Ar7<-lm(scaled.delta.Ar ~Vuln, data=stats2)#p=0.1043, F=2.007
summary(Ar7)

##MULTIPLE LINEAR REGRESSIONS##
Ar8<-lm(scaled.delta.Ar~taxa.lost+species, data=stats2)
summary(Ar8)
Ar9<-lm(scaled.delta.Ar~taxa.lost*species, data=stats2)
summary(Ar9)
############################################
############################################
#####  LINEAR REGRESSIONS -- NEG Ar     ####
############################################
############################################
stats4<-read.table("RAPID-matrix-Ar-NEG.txt", h=T) ##excludes missing Ar values and positive Ar values

NegAr1<-lm(stats$scaled.delta.Ar~ stats$D.Index)## 
summary(NegAr1)
plot(Ar1)
NegAr2<-lm(scaled.delta.Ar ~Precip, data= stats)##
summary(NegAr2)
NegAr3<-lm(scaled.delta.Ar ~Altitude, data= stats)##
summary(NegAr3)
plot(Ar3)
NegAr4<-lm(scaled.delta.Ar ~ taxa.lost, data= stats)##
summary(NegAr4)
NegAr5<-lm(scaled.delta.Ar ~high.low, data= stats4)
summary(NegAr5)
NegAr6<-lm(scaled.delta.Ar ~mobility, data= stats4)
summary(NegAr6)
NegAr7<-lm(scaled.delta.Ar ~Vuln, data= stats4)
summary(NegAr7)

##MULTIPLE LINEAR REGRESSIONS##
NegAr8<-lm(scaled.delta.Ar~taxa.lost+species, data=stats4)
summary(NegAr8)
NegAr9<-lm(scaled.delta.Ar~taxa.lost*species, data=stats4)
summary(NegAr9)


##############################################
###                                        ###
###       PLOTTING LINEAR REGRESSIONS      ###
###                                        ###
##############################################


#1. ALL SPECIES AS ONE RELATIONSHIP
##############################################
plot(stats$log.Fst~stats$scaled.delta.Ar, pch=c(1,2,3,4,5,6,7)[as.numeric(stats$species)], col=c(1,2)[as.numeric(stats$high.low)])
abline(lm(stats$log.Fst~stats$scaled.delta.Ar))
legend("bottomright",
	legend=c("Bbi", "Btr", "Dco", "Elo", "Hpa", "Msi", "Rbr"),
	pch=c(1,2,3,4,5,6,7))
##############################################


#2. ALL SPECIES AS SEPARATE RELATIONSHIPS
##############################################
library(ggplot2)
p<-qplot(taxa.lost, log.Fst, shape=species, color=species, data=stats)
	p + geom_smooth(method="lm", se=FALSE, fullrange=T)
help(qplot)
##############################################


############################################
############################################
#####      GLMM     ##       Fst        ####
############################################
############################################
library(glmm)
help(glmm)
set.seed(1234)
ptm<-proc.time()

model1<-glmm(pop.Fst~D.Index+species, random=list(~0+Site), varcomps.names=c("site"), data=stats, family.glmm=binomial.glmm, m=10^4, debug=TRUE)
summary(model1)
model2<-glmm(pop.Fst~Precip+species, random=list(~0+Site), varcomps.names=c("site"), data=stats, family.glmm=binomial.glmm, m=10^4, debug=TRUE)
summary(model2)
model3<-glmm(pop.Fst~Altitude+species, random=list(~0+Site), varcomps.names=c("site"), data=stats, family.glmm=binomial.glmm, m=10^4, debug=TRUE)
summary(model3)
model4<-glmm(pop.Fst~species, random=list(~0+Site), varcomps.names=c("site"), data=stats, family.glmm=binomial.glmm, m=10^4, debug=TRUE)
summary(model4)
model5<-glmm(pop.Fst~taxa.lost+species, random=list(~0+Site), varcomps.names=c("site"), data=stats, family.glmm=binomial.glmm, m=10^4, debug=TRUE)
summary(model5)

model6<-glmm(pop.Fst~D.Index, random=list(~0+Site, ~0+species), varcomps.names=c("site", "species"), data=stats, family.glmm=binomial.glmm, m=10^4, debug=TRUE)
summary(model6)


#model2<-glmm(pop.Fst~D.Index+species, random=list(~0+Site), varcomps.names=c("B2252","B3364","P3060","P3166","S2388","S2643","S3249","S3348","B2252","P2212","P3166","S2388","S2643","B2252","P1992","S2189","S2388","S2830","B2252","P2212","P3060","B3364","S3249","S3348","B2252","S2189","S2388","S2830","B2252"), data=stats, family.glmm=binomial.glmm, m=10^4, debug=TRUE)
proc.time()-ptm
??family.glmm




##############################################
##########      MODEL SELECTION     ##########
##############################################
library(MuMIn)
options(na.action="na.fail")
global.model<-lm(scaled.delta.Ar~D.Index+Altitude+species+taxa.lost+mobility, data=stats2)
FstAll<-dredge(global.model, rank="AICc")
FstAll


##############################################
#######    Fst with POS-Ar and NEG-Ar   ######
##############################################

POSArFST<-lm(stats$log.Fst~stats$scaled.delta.Ar)## 
summary(POSArFST)
cor(stats$log.Fst,stats$scaled.delta.Ar)

NEGArFST<-lm(stats$log.Fst~stats$scaled.delta.Ar)## 
summary(NEGArFST)
cor(stats$log.Fst,stats$scaled.delta.Ar)



ALLArFst<-lm(stats$log.Fst~stats$scaled.delta.Ar)
summary(ALLArFst)
cor(stats$log.Fst,stats$scaled.delta.Ar)

##QUADRATIC REGRESSION###
y<-stats$log.Fst
x<-stats$scaled.delta.Ar
x2<-x^2
quadratic<-lm(y ~ x + x2)
summary(quadratic)

xv<-seq(-1,1,0.1)
yv<-predict(quadratic, list(x=xv,x2=xv^2))




plot(y ~ x, pch=c(1,2,3,4,5,6,7)[as.numeric(stats$species)], col=c(1,2)[as.numeric(stats$high.low)])
lines(predict(lm(y~x+(x2))))

lines(xv,yv)
fitted(lm(y ~ x +x2), data=stats)

plot(factor, fitted(lm(data~factor+I(factor^2))), type="l")

plot(fitted(lm(y~x+(x2))))


plot(quadratic)

abline(lm(stats$log.Fst~stats$scaled.delta.Ar))
legend("bottomright",
	legend=c("Bbi", "Btr", "Dco", "Elo", "Hpa", "Msi", "Rbr"),
	pch=c(1,2,3,4,5,6,7))
	

