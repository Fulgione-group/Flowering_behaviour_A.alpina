
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

greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
dat <- subset(greenH_noVern, greenH_noVern$priority == 1, select = c("region","population","family","individual", "FlowAfterGerm_original", "Flowering_binominal"))

# Selection of Natural populations and controls
#
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

# Selection of Natural populations and no controls
#
pops=c("E3", "E4", "F1a", "F1b", "F2", "S1", "S2", "S3", "S4", "S5")
dat2 <- subset(dat, dat$population %in% pops)
dat2 <- droplevels(dat2)
unique(dat2$population)


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




###
#     Permutation test to check for differences across regions
###

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

pdf("./results/fig2a.pdf", height=7,width=7)
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



# Add all points from natural populations
#
rownames(perc)
points(x=perc[rownames(perc) %in% c("E3", "E4", "F1a", "F1b", "F2", "S1", "S2", "S3", "S4", "S5")], y=flTime[rownames(perc) %in% c("E3", "E4", "F1a", "F1b", "F2", "S1", "S2", "S3", "S4", "S5")], bg = col, col="black", pch=symb, lwd=0.3, cex=2)
points(x=perc[rownames(perc) %in% c("E3", "E4")], y=jitter(c(370, 370)), bg = rgb(178/255, 34/255, 34/255, 0.9), col="black", pch=symb[1:2], lwd=0.3, cex=2)


# Add references accessions
# Pajares
points(x=perc_ref[rownames(perc_ref) %in% c("Paj")], y=jitter(370), bg = rgb(255/255, 0/255, 0/255, 0.8), col="black", pch=symb[1:2], lwd=0.1, cex=1.5)
# Pep1
points(x=perc_ref[rownames(perc_ref) %in% c("PEP1")], y=flTime_ref[rownames(perc_ref) %in% c("PEP1")], bg = rgb(255/255, 20/255, 147/255, 0.8), col="black", pch=symb[1], lwd=0.1, cex=1.5)


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



# 		Pearson's product-moment correlation
# 
# data:  perc_forCorr_families and flTime_forCorr_families
# t = -32.696, df = 230, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.9275804 -0.8813394
# sample estimates:
#   cor 
# -0.9071614 







