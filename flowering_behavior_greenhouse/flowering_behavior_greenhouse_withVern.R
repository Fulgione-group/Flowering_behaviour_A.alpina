
# Run as:
# Rscript filename.R 

###
#         Read big table
###
setwd("/Users/fulgione/git/Flowering_behaviour_A.alpina/")
  # setwd("/Users/fulgione/git/Flowering_behaviour_A.alpina/flowering_behavior_greenhouse")

###
#     The data
###
gh_noVern_dataSet = "./data/bigpopdata_af.txt"
gh_yesVern_dataSet = "./data/AfterVern_af_withFlowerStop_2.txt"



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
#         Plot Fig 2b
###

# greenH_vern <- as.data.frame(read.table(gh_yesVern_dataSet, header = T, row.names = NULL))
vernDat <- subset(greenH_vern, greenH_vern$priority == 1, select = c("region","population","family","individual", "FlowAfterGermFlAG", "Flowering_binominal", "duration"))
# str(vernDat)

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

percByFamilyV=with(vernDat, tapply(vernDat$Flowering_binominal, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))
flTimeByFamilyV=with(vernDat, tapply(vernDat$FlowAfterGermFlAG, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))








pdf("./results/fig2b.pdf", height=7,width=7)
par(mar=c(5,5,3,3))

symb=c(21, 24, 
       21, 24, 23,
       21, 21,
       21, 24, 23, 22, 25)
colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rep(rgb(218/255, 165/255, 32/255, 0.3), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.3), 5))
col = c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rep(rgb(218/255, 165/255, 32/255, 0.9), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.9), 5))

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







