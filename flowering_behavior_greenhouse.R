
# Run as:
# Rscript filename.R 

###
#         Read big table
###
setwd("/Users/fulgione/Downloads/Wunder_ms/")

###
#     The data
###
gh_noVern_dataSet = "/Users/fulgione/Downloads/Wunder_ms/bigpopdata_af.txt_2011.txt"
  # "/Users/fulgione/Downloads/Wunder_ms/bigpopdata_af.txt"
gh_yesVern_dataSet_noFlowerArrest = "/Users/fulgione/Downloads/Wunder_ms/AfterVern_af.txt"
gh_yesVern_dataSet = "/Volumes/CVI/final/alpina/Wunder_ms/AfterVern_af_withFlowerStop_2.txt"

greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
dat <- subset(greenH_noVern, greenH_noVern$subsets == 1, select = c("region","population","family","individual", "FlowAfterGerm_original", "Flowering_binominal"))

# pops=c("E3", "E4", "F1a", "F1b", "F2", "S1", "S2", "S3", "S4", "S5")
pops=c("E3", "E4", "F1a", "F1b", "F2", "S1", "S2", "S3", "S4", "S5", "Paj", "PEP1")

dat2 <- subset(dat, dat$population %in% pops)
dat2 <- droplevels(dat2)
unique(dat2$population)

# summary(greenH_noVern)
# str(greenH_noVern)
# head(greenH_noVern)

###
#           Section "Flowering behaviour in controlled greenhouse conditions":
#           % flowering plants and flowering time in all populations and controls
###

perc=with(dat2, tapply(dat2$Flowering_binominal, dat2$population, mean, na.rm=TRUE))
sd.perc=with(dat2, tapply(dat2$Flowering_binominal, dat2$population, sd, na.rm=TRUE))
count.perc=with(dat2, tapply(dat2$Flowering_binominal, dat2$population, function(x) length(x[!is.na(x)])))
err.perc = (1.96*sd.perc)/(sqrt(count.perc))

flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, mean, na.rm=TRUE))
sd.flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, sd, na.rm=TRUE))
count.flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, function(x) length(x[!is.na(x)])))
err.flow = (1.96*sd.flTime)/(sqrt(count.flTime))

print("Percentrage flowering plants per population:")
print(perc)
print("Average flowering time per population (NA or NaN for not flowering):")
print(flTime)


###
#         Isolate data on the reference accessions, PEP1 and pajares
###

dat_ref <- subset(dat, dat$population %in% c("Paj", "PEP1"))
dat_ref <- droplevels(dat_ref)

perc_ref=with(dat_ref, tapply(dat_ref$Flowering_binominal, dat_ref$population, mean, na.rm=TRUE))
err.perc_ref=with(dat_ref, tapply(dat_ref$Flowering_binominal, dat_ref$population, sd, na.rm=TRUE))

flTime_ref=with(dat_ref, tapply(dat_ref$FlowAfterGerm_original, dat_ref$population, mean, na.rm=TRUE))
err.flTime_ref=with(dat_ref, tapply(dat_ref$FlowAfterGerm_original, dat_ref$population, sd, na.rm=TRUE))
count.flTime_ref=with(dat_ref, tapply(dat_ref$FlowAfterGerm_original, dat_ref$population, function(x) length(x[!is.na(x)])))
err.flow_ref = (1.96*err.flTime_ref)/(sqrt(count.flTime_ref))



###
#     Now the values organised by family, not by population
###

percByFamily=with(dat2, tapply(dat2$Flowering_binominal, list(dat2$population, dat2$family), mean, na.rm=TRUE))
flTimeByFamily=with(dat2, tapply(dat2$FlowAfterGerm_original, list(dat2$population, dat2$family), mean, na.rm=TRUE))


# Fill in 370 instead of non-flowered (for plotting purposes)

f1b=vector()
f2=vector()
s1=vector()
s2=vector()
s3=vector()
s4=vector()
s5=vector()

for (row in 1:length(percByFamily[,1])) {
  for (col in 1:length(percByFamily[1,])) {
    if (!is.na(percByFamily[row,col]) ) {
      if (percByFamily[row,col] == 0.0) {
        print(paste(rownames(percByFamily)[[row]], colnames(percByFamily)[[col]], percByFamily[row,col], flTimeByFamily[row,col], sep=" "))
        # flTimeByFamily[row,col] = 370
        if (row == 4) {
          f1b = append(f1b, 370)
        }
        if (row == 5) {
          f2 = append(f2, 370)
        }
        if (row == 6) {
          s1 = append(s1, 370)
        }
        if (row == 6) {
          s2 = append(s2, 370)
        }
        if (row == 6) {
          s3 = append(s3, 370)
        }
        if (row == 6) {
          s4 = append(s4, 370)
        }
        if (row == 6) {
          s5 = append(s5, 370)
        }
      }
    }
  }
}



###
#     Check for differences across regions with Fisher-Pitman permutation test
#     Keep in mind, here, the family is considered the lowest level entity, within populations and regions
#     make a new data frame with all families (not individual plants), pop region, fl time, perc flowering
### 

fam=vector()
reg=vector()
pop=vector()
flt=vector()
percentag=vector()

for (row in 1:length(percByFamily[,1])) {
  for (col in 1:length(percByFamily[1,])) {
    if (!is.na(percByFamily[row,col]) ) {
      # if ( strsplit(rownames(percByFamily2)[[row]], "")[[1]]  != "E" && strsplit(rownames(percByFamily2)[[row]], "")[[1]]  != "P") {
        # if ( strsplit(rownames(percByFamily2)[[row]], "")[[1]]  != "S" ) {
        # print(paste(rownames(percByFamily2)[[row]], colnames(percByFamily2)[[col]], percByFamily2[row,col], flTimeByFamily2[row,col], sep=" "))
        
        # region, population and family:
        # 
        population = rownames(percByFamily)[[row]]
        region = ""
        if (strsplit(population, "")[[1]]  == "E" || strsplit(population, "")[[1]]  == "P") {
          region = "E"
        } else {
          if (strsplit(population, "")[[1]]  == "F") {
            region = "F"
          } else {
            region = "S"
          }
        }
        reg = append(reg, region)
        pop = append(pop, population)
        fam = append(fam, paste(population, colnames(percByFamily)[[col]]))
        
        # Flowering time:
        if (percByFamily[row,col] == 0.0) {
          flt = append(flt, 370)
        } else {
          flt = append(flt, flTimeByFamily[row,col])
        }
        
        # Percentage flowering:
        percentag = append(percentag, percByFamily[row,col])
      # }
    }
  }
}
flTimeByRegion<-data.frame("family" = fam, "region" = reg, "population" = pop, "flTime" = flt, "percentage" = percentag)
str(flTimeByRegion)


#
library(coin) 

# France compared to Scandinavia
#
oneway_test(flTimeByRegion$percentage[flTimeByRegion$region %in% c("S","F")] ~ flTimeByRegion$region[flTimeByRegion$region %in% c("S","F")])
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("S", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("S", "F")] (F, S)
# Z = 2.5707, p-value = 0.01015
# alternative hypothesis: true mu is not equal to 0
#

oneway_test(flTimeByRegion$percentage[flTimeByRegion$region %in% c("E","F")] ~ flTimeByRegion$region[flTimeByRegion$region %in% c("E","F")])
#
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "F")] (E, F)
# Z = -7.5341, p-value = 4.918e-14
# alternative hypothesis: true mu is not equal to 0
# 

oneway_test(flTimeByRegion$percentage[flTimeByRegion$region %in% c("E","S")] ~ flTimeByRegion$region[flTimeByRegion$region %in% c("E","S")])
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "S")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "S")] (E, S)
# Z = -7.2113, p-value = 5.542e-13
# alternative hypothesis: true mu is not equal to 0
# 
# pvalue(oneway_test(flTimeByRegion$flTime ~ flTimeByRegion$region))
# pvalue(oneway_test(flTimeByRegion$flTime ~ flTimeByRegion$region, distribution=approximate(nresample=1000000)))






###
#         Plot Fig 2a
###

pdf("./fig2a_2021-04-05.pdf", height=7,width=7)
par(mar=c(5,5,3,3))

symb=c(21, 24, 
       21, 24, 23,
       21, 24, 23, 22, 25)
# colOr=c(rep(rgb(102/255,205/255,170/255,0.2), 2), rep(rgb(178/255, 34/255, 34/255, 0.2), 3), rep(rgb(0, 0, 205/255,0.2), 5))
colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rep(rgb(218/255, 165/255, 32/255, 0.3), 3), rep(rgb(30/255, 144/255, 255/255, 0.3), 5))

col = c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rep(rgb(218/255, 165/255, 32/255, 0.9), 3), rep(rgb(30/255, 144/255, 255/255, 0.9), 5))

# Spaniards?
#flTime[[1]] = 370
#flTime[[2]] = 370


plot(x=perc, y=flTime, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Days to flowering"), cex=1.5), xlab=list(expression("% flowering plants"), cex=1.5), xlim=c(-0.05, 1), ylim=c(0, 380))

# Add families
for (r in 3:length(flTimeByFamily[,1])) {
  points(x=percByFamily[r,], y=flTimeByFamily[r,], bg = colOr[r], pch=symb[r], col="black", lwd=0.1)
}

# Add spaniards and other fixed vern. with jitter
points(x=rep(0, length(f1b)), y=jitter(f1b), bg = colOr[4], pch=symb[4], col="black", lwd=0.1)
points(x=rep(0, length(f2)), y=jitter(f2), bg = colOr[5], pch=symb[5], col="black", lwd=0.1)
points(x=rep(0, length(s1)), y=jitter(s1), bg = colOr[6], pch=symb[6], col="black", lwd=0.1)
points(x=rep(0, length(s2)), y=jitter(s2), bg = colOr[7], pch=symb[7], col="black", lwd=0.1)
points(x=rep(0, length(s3)), y=jitter(s3), bg = colOr[8], pch=symb[8], col="black", lwd=0.1)
points(x=rep(0, length(s4)), y=jitter(s4), bg = colOr[9], pch=symb[9], col="black", lwd=0.1)
points(x=rep(0, length(s5)), y=jitter(s5), bg = colOr[10], pch=symb[10], col="black", lwd=0.1)

for (sp in 1:2) {
  for (ip in 1:length(flTimeByFamily[sp,])) {
    flTimeByFamily[sp,ip] = 370
  }
  points(x=percByFamily[sp,], y=jitter(flTimeByFamily[sp,]), bg = colOr[sp], pch=symb[sp], col="black", lwd=0.1)
}



# Add all points
points(x=perc, y=flTime, bg = col, col="black", pch=symb, lwd=0.3, cex=2)
points(x=perc[1:2], y=jitter(c(370, 370)), bg = rgb(178/255, 34/255, 34/255, 0.9), col="black", pch=symb[1:2], lwd=0.3, cex=2)


# Add references accessions
# Pajares
points(x=perc_ref[1], y=jitter(370), bg = rgb(255/255, 0/255, 0/255, 0.8), col="black", pch=symb[1:2], lwd=0.1, cex=1.5)
# Pep1
points(x=perc_ref[2], y=flTime_ref[2], bg = rgb(255/255, 20/255, 147/255, 0.8), col="black", pch=symb[1], lwd=0.1, cex=1.5)


# Add errors

arrowX.start <- perc + (err.perc)
arrowX.end <- perc - (err.perc)
arrows(arrowX.start, flTime, arrowX.end, flTime, angle=90, code=3, length=0.03) 

arrowY.start <- flTime + (err.flow)
arrowY.end <- flTime - (err.flow)
arrows(perc, arrowY.start, perc, arrowY.end, angle=90, code=3, length=0.03) 

arrowR.start <- flTime_ref + (err.flow_ref)
arrowR.end <- flTime_ref - (err.flow_ref)
arrows(perc_ref, arrowR.start, perc_ref, arrowR.end, angle=90, code=3, length=0.03) 



axis(1, at=c(0.0, 0.5, 1.0), tick=TRUE, labels=T, cex.axis=1.2)
axis(2, at=seq(0, 300, 100), labels=T, tick=TRUE, las=1, cex.axis=1.2)
axis(2, at=c(300, 400), labels=F, tick=T, las=1, cex.axis=1.2)

dev.off()









###
#       Calculate the correlation itself
###


library(Hmisc)
# rcorr(x=perc, y=flTime, type="spearman") # type can be pearson or spearman
cor.test( ~ perc + flTime, 
          data=dat2,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.99)
# 
# 	Spearman's rank correlation rho
#
# data:  perc and flTime
# S = 238, p-value = 4.96e-05
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# -0.9833333 
# 

cor.test( ~ perc + flTime, 
          data=dat2,
          method = "pearson",
          continuity = FALSE,
          conf.level = 0.99)

# 
# 	Pearson's product-moment correlation
# 
# data:  perc and flTime
# t = -10.909, df = 7, p-value = 1.203e-05
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
#   -0.9965176 -0.7904238
# sample estimates:
#   cor 
# -0.9718249 
# 





###
#       And among family means
#       Consider that cor.test dislikes many NAs, 
#       So we need to clean up percByFamily and flTimeByFamily
###

perc_forCorr_families=vector()
flTime_forCorr_families=vector()

for (row in 1:length(percByFamily[,1])) {
  for (r1 in 1:length(rownames(as.data.frame(percByFamily[row,]))) ) {
    nam1 = rownames(as.data.frame(percByFamily[row,]))[[r1]]
    for (r2 in 1:length(rownames(as.data.frame(flTimeByFamily[row,]))) ) {
      nam2 = rownames(as.data.frame(flTimeByFamily[row,]))[[r2]]
      if (nam1 == nam2) {
        # print(paste(rownames(as.data.frame(percByFamily[,1]))[[row]], nam1, nam2, percByFamily[row, r1], flTimeByFamily[row, r2], sep = " "))
        if (!is.na(percByFamily[row, r1]) && !is.na(flTimeByFamily[row, r2])) {
          perc_forCorr_families = append(perc_forCorr_families, percByFamily[row, r1])
          flTime_forCorr_families = append(flTime_forCorr_families, flTimeByFamily[row, r2])
        }
      }
    }
  }
}
length(perc_forCorr_families)
length(flTime_forCorr_families)

plot(perc_forCorr_families, flTime_forCorr_families)

cor.test( ~ perc_forCorr_families + flTime_forCorr_families, 
          data=dat2,
          method = "pearson",
          continuity = FALSE,
          conf.level = 0.95)



# 	Pearson's product-moment correlation
# 
# data:  perc_forCorr_families and flTime_forCorr_families
# t = -32.849, df = 231, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.9278567 -0.8819066
# sample estimates:
#   cor 
# -0.9075615 
# -0.908*-0.908




























######
###
#
#           Experiment 2: with vernalization
#
###
######
# Test for differences in variance
#
# Build a data frame
greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
flow_noVern <- subset(greenH_noVern, greenH_noVern$priority == 1, select = c("FlowAfterGerm_original"))
colnames(flow_noVern)[[1]] = "flow_time"
flow_noVern = subset(flow_noVern, (!is.na(flow_noVern[,1])))
unique(flow_noVern$flow_time)
flow_noVern$vernalization = rep("noV", length(flow_noVern$flow_time))

# Overall Variance
var(flow_noVern$flow_time, na.rm = T)

# Add vern
greenH_vern <- as.data.frame(read.table(gh_yesVern_dataSet, header = T, row.names = NULL))
flow_vern <- subset(greenH_vern, greenH_vern$priority == 1, select = c("FlowAfterGermFlAG"))
colnames(flow_vern)[[1]] = "flow_time"
flow_vern = subset(flow_vern, (!is.na(flow_vern[,1])))
unique(flow_vern$flow_time)
flow_vern$vernalization = rep("yesV", length(flow_vern$flow_time))
str(flow_vern)

# Overall Variance
var(flow_vern$flow_time, na.rm = T)

# Merge
flow_all=c(flow_noVern$flow_time, flow_vern$flow_time)
vern_all=c(flow_noVern$vernalization, flow_vern$vernalization)
flow_variance=data.frame("flow_time" = as.integer(flow_all), "vernalization" = vern_all)
str(flow_variance)

library(onewaytests)
#bf.test(Sepal.Length ~ Species, data = iris)
bf.test(flow_time ~ vernalization, data = flow_variance)

#Brown-Forsythe Test 
#--------------------------------------------------------- 
#  data : flow_time and vernalization 

#statistic  : 491.4583 
#num df     : 1 
#denom df   : 1156.922 
#p.value    : 4.918039e-91 
#
#Result     : Difference is statistically significant. 
#--------------------------------------------------------- 


library(car)
leveneTest(flow_time ~ vernalization, center=mean, data = flow_variance)
leveneTest(flow_time ~ vernalization, center=median, data = flow_variance)
leveneTest(flow_time ~ vernalization, center=median, data = flow_variance, bootstrap=T, num.bootstrap=100000, kruskal.test=T)

#> leveneTest(flow_time ~ vernalization, center=mean, data = flow_variance)
#Levene's Test for Homogeneity of Variance (center = mean)
#        Df F value    Pr(>F)    
#group    1  335.49 < 2.2e-16 ***
#      1293                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#> leveneTest(flow_time ~ vernalization, center=median, data = flow_variance)
#Levene's Test for Homogeneity of Variance (center = median)
#Df F value    Pr(>F)    
#group    1   167.7 < 2.2e-16 ***
#  1293                      
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#> leveneTest(flow_time ~ vernalization, center=median, data = flow_variance, bootstrap=T, num.bootstrap=100000, kruskal.test=T)
#Levene's Test for Homogeneity of Variance (center = median: T)
#        Df F value    Pr(>F)    
#group    1   167.7 < 2.2e-16 ***
#      1293                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1






###
#         looking at flowering duration after vernalization
###

# Pull duration of flowering
#
greenH_vern <- as.data.frame(read.table(gh_yesVern_dataSet, header = T, row.names = NULL))
vernDat <- subset(greenH_vern, greenH_vern$priority == 1, select = c("region","population","family","individual", "FlowAfterGermFlAG", "Flowering_binominal", "duration"))

duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, mean, na.rm=TRUE))
err.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, sd, na.rm=TRUE))
count.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, function(x) length(x[!is.na(x)])))
err.duratV = (1.96*err.duratV)/(sqrt(count.duratV))








###
#       Calculate the correlation between flowering time (no vern.) and duration (after vern.)
###
greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
dat <- subset(greenH_noVern, greenH_noVern$subsets == 1, select = c("region","population","family","individual", "FlowAfterGerm_original", "Flowering_binominal"))
pops=c("F1a", "F1b", "F2", "PEP1", "S1", "S2", "S3", "S4", "S5")
dat2 <- subset(dat, dat$population %in% pops)
dat2 <- droplevels(dat2)

flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, mean, na.rm=TRUE))

duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, mean, na.rm=TRUE))
duratV = duratV[c(3:5, 7:12)]

library(Hmisc)
rcorr(x=duratV, y=flTime, type="spearman") # type can be pearson or spearman
cor.test( x = duratV,
           y = flTime,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.99)

#	Spearman's rank correlation rho
#
#data:  duratV and flTime
#S = 236, p-value = 0.0001653
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#-0.9666667 

cor.test( x = duratV,
          y = flTime,
          method = "pearson",
          continuity = FALSE,
          conf.level = 0.99)
# Pearson's product-moment correlation
#
#data:  duratV and flTime
#t = -15.35, df = 7, p-value = 1.201e-06
#alternative hypothesis: true correlation is not equal to 0
#99 percent confidence interval:
#-0.9982148 -0.8868747
#sample estimates:
#cor 
#-0.9854692 













###
#         Plot Fig 2b
###

greenH_vern <- as.data.frame(read.table(gh_yesVern_dataSet, header = T, row.names = NULL))
summary(greenH_vern)
str(greenH_vern)
head(greenH_vern)

unique(greenH_vern$population)

vernDat <- subset(greenH_vern, greenH_vern$priority == 1, select = c("region","population","family","individual", "FlowAfterGermFlAG", "Flowering_binominal", "duration"))
str(vernDat)

# Pull percentage flowered
#
percV=with(vernDat, tapply(vernDat$Flowering_binominal, vernDat$population, mean, na.rm=TRUE))
err.percV=with(vernDat, tapply(vernDat$Flowering_binominal, vernDat$population, sd, na.rm=TRUE))
count.percV=with(vernDat, tapply(vernDat$Flowering_binominal, vernDat$population, function(x) length(x[!is.na(x)])))
err.percV = (1.96*err.percV)/(sqrt(count.percV))

# Pull time to flower
#
flTimeV=with(vernDat, tapply(vernDat$FlowAfterGermFlAG, vernDat$population, mean, na.rm=TRUE))
err.flTimeV=with(vernDat, tapply(vernDat$FlowAfterGermFlAG, vernDat$population, sd, na.rm=TRUE))
count.flTimeV=with(vernDat, tapply(vernDat$FlowAfterGermFlAG, vernDat$population, function(x) length(x[!is.na(x)])))
err.flowV = (1.96*err.flTimeV)/(sqrt(count.flTimeV))


dfV=data.frame(cbind(percV, flTimeV))
colnames(dfV)=c("perc", "flTime")

str(dfV)


percByFamilyV=with(vernDat, tapply(vernDat$Flowering_binominal, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))
flTimeByFamilyV=with(vernDat, tapply(vernDat$FlowAfterGermFlAG, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))








pdf("/Users/fulgione/Downloads/Wunder_ms/fig2b_2020-12-17.pdf", height=7,width=7)

symb=c(21, 24, 
       21, 24, 23,
       21, 21,
       21, 24, 23, 22, 25)
# colOr=c(rep(rgb(102/255,205/255,170/255,0.2), 2), rep(rgb(178/255, 34/255, 34/255, 0.2), 3), rep(rgb(0, 0, 205/255,0.2), 5))
colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rep(rgb(218/255, 165/255, 32/255, 0.3), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.3), 5))

col = c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rep(rgb(218/255, 165/255, 32/255, 0.9), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.9), 5))

# Spaniards?
#flTime[[1]] = 370
#flTime[[2]] = 370

par(mar=c(5,5,3,3))
plot(x=percV, y=flTimeV, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Days to flowering"), cex=1.5), xlab=list(expression("% flowering plants"), cex=1.5), xlim=c(-0.05, 1), ylim=c(0, 380))

# Add families
for (r in 1:length(flTimeByFamilyV[,1])) {
  points(x=percByFamilyV[r,], y=flTimeByFamilyV[r,], bg = colOr[r], pch=symb[r], col="black", lwd=0.1)
}

points(x=percV, y=flTimeV, bg = col, col="black", pch=symb, lwd=0.3, cex=2)

# Add references accessions
# Pajares
#points(x=perc_ref[1], y=jitter(370), bg = rgb(255/255, 0/255, 0/255, 0.8), col="black", pch=symb[1:2], lwd=0.1, cex=1.5)
# Pep1
#points(x=perc_ref[2], y=flTime_ref[2], bg = rgb(255/255, 20/255, 147/255, 0.8), col="black", pch=symb[1], lwd=0.1, cex=1.5)

# Add errors

arrowX.start <- percV + (err.percV)
arrowX.end <- percV - (err.percV)
arrows(arrowX.start, flTimeV, arrowX.end, flTimeV, angle=90, code=3, length=0.03) 

arrowY.start <- flTimeV + (err.flowV)
arrowY.end <- flTimeV - (err.flowV)
arrows(percV, arrowY.start, percV, arrowY.end, angle=90, code=3, length=0.03) 


polygon(x=c(0, 1, 1, 0), y=c(36, 36, 132, 132), col = "lightblue", border = "lightblue")

axis(1, at=c(0.0, 0.5, 1.0), tick=TRUE, labels=T, cex.axis=1.2)
axis(2, at=seq(0, 300, 100), labels=T, tick=TRUE, las=1, cex.axis=1.2)

dev.off()

























z


###
#         Plot Fig 2c
#         Consider changing the position of the perpetually flowering: they are too close to others.
#         Also, does it make sense to include perpetually flowering plants in the pop means? 
#         Otherwise the relationship is steeper than in reality
###

# Pull flowering time without vernalization

greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
unique(greenH_noVern$population)
dat <- subset(greenH_noVern, greenH_noVern$subsets == 1, select = c("region","population","family","individual", "FlowAfterGerm_original", "Flowering_binominal"))
pops=c("E3", "E4", "F1a", "F1b", "F2", "Paj", "PEP1", "S1", "S2", "S3", "S4", "S5")
dat2 <- subset(dat, dat$population %in% pops)
dat2 <- droplevels(dat2)
unique(dat2$population)

#######
# Clean it up using all flowering times scored
#
for (row in 1:length(dat2$FlowAfterGerm_original)) {
  if (!is.na(dat2$FlowAfterGerm_original[[row]])) {
    dat2$Flowering_binominal[[row]] = 1
    #print(paste(dat2$Flowering_binominal[[row]], dat2$FlowAfterGerm_original[[row]], sep=" "))
  }
}
# dat2$Flowering_binominal[[1]]

# Or not, then comment out till here
#######

flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, mean, na.rm=TRUE))
err.flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, sd, na.rm=TRUE))
count.flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, function(x) length(x[!is.na(x)])))

err.flow = (1.96*err.flTime)/(sqrt(count.flTime))

percByFamily=with(dat2, tapply(dat2$Flowering_binominal, list(dat2$population, dat2$family), mean, na.rm=TRUE))




# Pull duration of flowering
#
greenH_vern <- as.data.frame(read.table(gh_yesVern_dataSet, header = T, row.names = NULL))
vernDat <- subset(greenH_vern, greenH_vern$priority == 1, select = c("region","population","family","individual", "FlowAfterGermFlAG", "Flowering_binominal", "still_flowering_at_end", "duration", "use_for_duration"))

# max(vernDat$duration, na.rm = T)

#######
# Clean it up using experiment length = 272 days
#
for (row in 1:length(vernDat$still_flowering_at_end)) {
  if (!is.na(vernDat$still_flowering_at_end[[row]])) {
    if (vernDat$still_flowering_at_end[[row]] >= 272) {
      vernDat$duration[[row]] = NA
      #print(paste(vernDat$population, vernDat$still_flowering_at_end, vernDat$use_for_duration, sep = " "))
    }
    if (vernDat$use_for_duration[[row]] == 0) {
      vernDat$duration[[row]] = NA
      #print(paste(vernDat$population, vernDat$still_flowering_at_end, vernDat$use_for_duration, sep = " "))
    }
  }
}

# Or not, then comment out till here
#######
str(vernDat)


duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, mean, na.rm=TRUE))
err.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, sd, na.rm=TRUE))
count.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, function(x) length(x[!is.na(x)])))
err.duratV = (1.96*err.duratV)/(sqrt(count.duratV))


dfD=data.frame(cbind(flTime, duratV))
colnames(dfD)=c("flTime", "duration")

str(dfD)


duratByFamilyV=with(vernDat, tapply(vernDat$duration, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))
flTimeByFamily=with(dat2, tapply(dat2$FlowAfterGerm_original, list(dat2$population, dat2$family), mean, na.rm=TRUE))



# Fill in 370 statt non-flowered
for (row in 1:length(percByFamily[,1])) {
  for (col in 1:length(percByFamily[1,])) {
    if (!is.na(percByFamily[row,col]) ) {
      if (percByFamily[row,col] == 0.0) {
        print(paste(rownames(percByFamily)[[row]], colnames(percByFamily)[[col]], percByFamily[row,col], flTimeByFamily[row,col], sep=" "))
        flTimeByFamily[row,col] = 370
      }
    }
  }
}



max(na.omit(duratByFamilyV))





pdf("/Users/fulgione/Downloads/Wunder_ms/fig2c_2020-12-17.pdf", height=7,width=7)
par(mar=c(5,5,3,3))

symb=c(21, 24, 
       21, 24, 23,
       21, 21,
       21, 24, 23, 22, 25)
# colOr=c(rep(rgb(102/255,205/255,170/255,0.2), 2), rep(rgb(178/255, 34/255, 34/255, 0.2), 3), rep(rgb(0, 0, 205/255,0.2), 5))

colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rep(rgb(218/255, 165/255, 32/255, 0.3), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.3), 5))
col = c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rep(rgb(218/255, 165/255, 32/255, 0.9), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.9), 5))

# Spaniards?
flTime[[1]] = 370
flTime[[2]] = 370

# With axis labels
#plot(x=0, y=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Days to flowering (no vernalization)"), cex=1.5), xlab=list(expression("Flowering duration (after vernalization)"), cex=1.5), xlim=c(0, 150), ylim=c(0, 380))
# No axis labels
plot(x=0, y=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Days to flowering (no vernalization)"), cex=1.5), xlab=list(expression("Flowering duration (after vernalization)"), cex=1.5), xlim=c(0, 150), ylim=c(0, 380))


# Work in progress
#
# This may need commenting out
# 
for (sp in 1:2) {
  for (ip in 1:length(flTimeByFamily[sp,])) {
    flTimeByFamily[sp,ip] = 370
  }
  #points(x=jitter(duratByFamilyV[sp,]), y=jitter(flTimeByFamily[sp,]), bg = colOr[sp], pch=symb[sp], col="black", lwd=0.1)
}

e3d=vector()
e3f=vector()
e4d=vector()
e4f=vector()
for (row in 1:length(duratByFamilyV[,1])) {
  for (r1 in 1:length(rownames(as.data.frame(duratByFamilyV[row,]))) ) {
    nam1 = rownames(as.data.frame(duratByFamilyV[row,]))[[r1]]
    for (r2 in 1:length(rownames(as.data.frame(flTimeByFamily[row,]))) ) {
      nam2 = rownames(as.data.frame(flTimeByFamily[row,]))[[r2]]
      if (nam1 == nam2) {
        print(paste(rownames(as.data.frame(duratByFamilyV[,1]))[[row]], nam1, nam2, duratByFamilyV[row, r1], flTimeByFamily[row, r2], sep = " "))
        if (row == 1 ) {
          e3d = append(e3d, duratByFamilyV[row, r1])
          e3f = append(e3f, flTimeByFamily[row, r1])
        } else {
          if ( row == 2) {
            e4d = append(e4d, duratByFamilyV[row, r1])
            e4f = append(e4f, flTimeByFamily[row, r1])
          } else {
            if (rownames(duratByFamilyV)[[row]] != "PEP1" && rownames(duratByFamilyV)[[row]] != "Paj") {
              points(x=duratByFamilyV[row, r1], y=flTimeByFamily[row, r2], bg = colOr[row], pch=symb[row], col="black", lwd=0.1)
            }
          }
        }
      }
    }
  }
}
# Add spaniard families
points(x=e3d, y=rep(370, length(e3d)), bg = colOr[1], pch=symb[1], col="black", lwd=0.1)
points(x=e4d, y=rep(370, length(e3d)), bg = colOr[2], pch=symb[2], col="black", lwd=0.1)


# Add references accessions
# Pajares

# Out pep1 and pajares
dat_ref <- subset(vernDat, vernDat$population %in% c("Paj", "PEP1"))
dat_ref <- droplevels(dat_ref)
str(dat_ref)

durat_ref=with(dat_ref, tapply(dat_ref$duration, dat_ref$population, mean, na.rm=TRUE))
err.durat_ref=with(dat_ref, tapply(dat_ref$duration, dat_ref$population, sd, na.rm=TRUE))

# Paj
points(x=durat_ref[1], y=370, bg = rgb(255/255, 0/255, 0/255, 0.8), col="black", pch=symb[1:2], lwd=0.1, cex=1.5)
# Pep1
#points(x=perc_ref[2], y=flTime_ref[2], bg = rgb(255/255, 20/255, 147/255, 0.8), col="black", pch=symb[1], lwd=0.1, cex=1.5)


# Add the points
points(x=duratV, y=flTime, bg = col, col="black", pch=symb, lwd=0.3, cex=2)


# Add errors
arrowY.start <- flTime + (err.flTime)
arrowY.end <- flTime - (err.flTime)
arrows(duratV, arrowY.start, duratV, arrowY.end, angle=90, code=3, length=0.03) 

arrowX.start <- duratV + (err.duratV)
arrowX.end <- duratV - (err.duratV)
arrows(arrowX.start, flTime, arrowX.end, flTime, angle=90, code=3, length=0.03) 

axis(1, at=c(0.0, 50, 100, 150), labels=c("0", "50", "100", ""), tick=TRUE, las=1, cex.axis=1.2)
axis(2, at=seq(0, 300, 100), labels=T, tick=TRUE, las=1, cex.axis=1.2)
axis(2, at=c(300, 400), labels=F, tick=T, las=1, cex.axis=1.2)








# Now add those that did not stop flowering
#
greenH_vern <- as.data.frame(read.table(gh_yesVern_dataSet, header = T, row.names = NULL))
vernDat <- subset(greenH_vern, greenH_vern$priority == 1, select = c("region","population","family","individual", "FlowAfterGermFlAG", "Flowering_binominal", "still_flowering_at_end", "duration", "use_for_duration"))

#######
# Clean it up using experiment length = 272 days
#
for (row in 1:length(vernDat$still_flowering_at_end)) {
  if (!is.na(vernDat$still_flowering_at_end[[row]])) {
    if (vernDat$still_flowering_at_end[[row]] >= 272) {
      vernDat$duration[[row]] = 150
      #print(paste(vernDat$population, vernDat$still_flowering_at_end, vernDat$use_for_duration, sep = " "))
    }
    #if (vernDat$use_for_duration[[row]] == 0) {
    #  vernDat$duration[[row]] = NA
    #print(paste(vernDat$population, vernDat$still_flowering_at_end, vernDat$use_for_duration, sep = " "))
    #}
  }
}

# Or not, then comment out till here
#######

duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, mean, na.rm=TRUE))
err.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, sd, na.rm=TRUE))
count.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, function(x) length(x[!is.na(x)])))
err.duratV = (1.96*err.duratV)/(sqrt(count.duratV))

duratByFamilyV=with(vernDat, tapply(vernDat$duration, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))



# Now families
for (row in 1:length(duratByFamilyV[,1])) {
  for (r1 in 1:length(rownames(as.data.frame(duratByFamilyV[row,]))) ) {
    nam1 = rownames(as.data.frame(duratByFamilyV[row,]))[[r1]]
    for (r2 in 1:length(rownames(as.data.frame(flTimeByFamily[row,]))) ) {
      nam2 = rownames(as.data.frame(flTimeByFamily[row,]))[[r2]]
      if (nam1 == nam2 && !is.na(duratByFamilyV[row, r1]) && duratByFamilyV[row, r1] == 150) {
        print(paste(rownames(as.data.frame(duratByFamilyV[,1]))[[row]], nam1, nam2, duratByFamilyV[row, r1], flTimeByFamily[row, r2], sep = " "))
        points(x=duratByFamilyV[row, r1], y=flTimeByFamily[row, r2], bg = colOr[row], pch=symb[row], col="black", lwd=0.1)
      }
    }
  }
}

for (p in 1:length(duratV)) {
  if (duratV[[p]] == 150) {
    points(x=duratV[[p]], y=flTime[[p]], bg = col[[p]], col="black", pch=symb[[p]], lwd=0.3, cex=2)
    
    arrowY.start <- flTime[[p]] + (err.flTime[[p]])
    arrowY.end <- flTime[[p]] - (err.flTime[[p]])
    arrows(duratV[[p]], arrowY.start, duratV[[p]], arrowY.end, angle=90, code=3, length=0.03) 
    
    arrowX.start <- duratV[[p]] + (err.duratV[[p]])
    arrowX.end <- duratV[[p]] - (err.duratV[[p]])
    arrows(arrowX.start, flTime[[p]], arrowX.end, flTime[[p]], angle=90, code=3, length=0.03) 
    
  }
}



# Code from Linde:
#for (row in 1:length(duratByFamilyV[,1])) {
#    nam1 = rownames(e(duratByFamilyV[row,]))[[r1]]
#    for (r2 in 1:length(rownames(as.data.frame(flTimeByFamily[row,]))) ) {
#      nam2 = rownames(as.data.frame(flTimeByFamily[row,]))[[r2]]
#      if (nam1 == nam2 && !is.na(duratByFamilyV[row, r2]) && duratByFamilyV[row, r2] == 150) {
#        print(paste(rownames(m,yu8 m,yyhhsfjzclxjfnhyurow, r2], sep = " "))
#        points(x=duratByFamilyV[row, r1], y=flTimeByFamily[row, r2], bg = colOr[row], pch=symb[row], col="black", lwd=0.1)
#      }bvvvvvvvvvh ggyukh ui
#    }bnhghhg
#  }
#}

dev.off()











###
#       Calculate the correlation between flowering time (no vern.) and duration (after vern.)
###

greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
dat <- subset(greenH_noVern, greenH_noVern$subsets == 1, select = c("region","population","family","individual", "FlowAfterGerm_original", "Flowering_binominal"))
pops=c("E3", "E4", "F1a", "F1b", "F2", "PEP1", "S1", "S2", "S3", "S4", "S5")
dat2 <- subset(dat, dat$population %in% pops)
dat2 <- droplevels(dat2)

flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, mean, na.rm=TRUE))
flTime[[1]] = 360
flTime[[2]] = 360

duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, mean, na.rm=TRUE))
duratV[[3]] = 128
duratV[[7]] = 128
duratV = duratV[c(1:5, 7:12)]
# duratV = duratV[c(3:5, 7:12)]

library(Hmisc)
rcorr(x=duratV, y=flTime, type="spearman") # type can be pearson or spearman
cor.test( x = duratV,
          y = flTime,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.99)

###
#   This one with spanish (fl time = 360 days) and perpetually flowering plants (duration = 128 days)
##
# Spearman's rank correlation rho

#  data:  duratV and flTime
#  S = 394.79, p-value = 0.003483
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho 
# -0.7945205 


#	Spearman's rank correlation rho
#
#data:  duratV and flTime
#S = 236, p-value = 0.0001653
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#-0.9666667 

cor.test( x = duratV,
          y = flTime,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.99)
# Pearson's product-moment correlation
#
#data:  duratV and flTime
#t = -15.35, df = 7, p-value = 1.201e-06
#alternative hypothesis: true correlation is not equal to 0
#99 percent confidence interval:
#-0.9982148 -0.8868747
#sample estimates:
#cor 
#-0.9854692 











###
#       Now on families
###

# Run #           Experiment 1: no vernalization


flT_forCorrela=vector()
durat_forCorrela=vector()

# Build indexes for the two weird tails of figure 2c
noFlIndex = vector()
perpFlIndex = vector()


# Now put together non-missing families for both variables
# 
for (row in 1:length(duratByFamilyV[,1])) {
  for (r1 in 1:length(rownames(as.data.frame(duratByFamilyV[row,]))) ) {
    nam1 = rownames(as.data.frame(duratByFamilyV[row,]))[[r1]]
    for (r2 in 1:length(rownames(as.data.frame(flTimeByFamily[row,]))) ) {
      nam2 = rownames(as.data.frame(flTimeByFamily[row,]))[[r2]]
      if (nam1 == nam2 && !is.na(duratByFamilyV[row, r1]) && !is.na(flTimeByFamily[row, r2]) ) {
        flT_forCorrela = append(flT_forCorrela, flTimeByFamily[row, r2])
        durat_forCorrela = append(durat_forCorrela, duratByFamilyV[row, r1])
        # Find vern requiring
        if (flTimeByFamily[row, r2] == 370) {
          noFlIndex = append(noFlIndex, length(flT_forCorrela))
        }
        # Find perpetual flowering
        if (duratByFamilyV[row, r1] == 150) {
          perpFlIndex = append(perpFlIndex, length(durat_forCorrela))
        }
      }
    }
  }
}

plot(durat_forCorrela, flT_forCorrela)


library(Hmisc)
rcorr(x=durat_forCorrela, y=flT_forCorrela, type="spearman") # type can be pearson or spearman
cor.test( x = durat_forCorrela,
          y = flT_forCorrela,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.99)
# Spearman's rank correlation rho

# data:  durat_forCorrela and flT_forCorrela
# S = 263510, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.732541 





### 
#       Now stats on the weird tales
### 

flT_forCorrela[noFlIndex]
durat_forCorrela[perpFlIndex]

meanFl1 = mean(flT_forCorrela[perpFlIndex])
meanFl2 = mean(flT_forCorrela[-perpFlIndex])
sdFl1 = sd(flT_forCorrela[perpFlIndex])
sdFl2 = sd(flT_forCorrela[-perpFlIndex])
ciF1=(1.96*sdFl1)/(sqrt(length(flT_forCorrela[perpFlIndex])))
ciF2=(1.96*sdFl2)/(sqrt(length(flT_forCorrela[-perpFlIndex])))

meanFl1
meanFl2
ciF1
ciF2

# Test wether the 2 groups (by duration: perpetual and non-p) are different in terms of flowering time
# independent 2-group Mann-Whitney U Test
wilcox.test(flT_forCorrela[perpFlIndex], flT_forCorrela[-perpFlIndex])

# Wilcoxon rank sum test with continuity correction

# data:  flT_forCorrela[perpFlIndex] and flT_forCorrela[-perpFlIndex]
# W = 164, p-value = 2.726e-11
# alternative hypothesis: true location shift is not equal to 0




hist(durat_forCorrela[noFlIndex], col="blue3", xlim=c(0, 150))
hist(durat_forCorrela[-noFlIndex], col="red", add=T)

sd(durat_forCorrela[noFlIndex])
mean(durat_forCorrela[-noFlIndex])

durat_forCorrela[perpFlIndex]






meanD1 = mean(durat_forCorrela[noFlIndex])
meanD2 = mean(durat_forCorrela[-noFlIndex])
sdD1 = sd(durat_forCorrela[noFlIndex])
sdD2 = sd(durat_forCorrela[-noFlIndex])
ciD1=(1.96*sdD1)/(sqrt(length(durat_forCorrela[noFlIndex])))
ciD2=(1.96*sdD2)/(sqrt(length(durat_forCorrela[-noFlIndex])))

meanD1
meanD2
ciD1
ciD2













wilcox.test(durat_forCorrela[noFlIndex], durat_forCorrela[-noFlIndex])

# Wilcoxon rank sum test with continuity correction

# data:  durat_forCorrela[noFlIndex] and durat_forCorrela[-noFlIndex]
# W = 295.5, p-value = 3.625e-07
# alternative hypothesis: true location shift is not equal to 0












