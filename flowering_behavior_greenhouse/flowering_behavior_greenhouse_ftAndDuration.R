
# Run as:
# Rscript filename.R 

###
#         Read big table
###
setwd("/Users/fulgione/git/")
  # setwd("/Users/fulgione/git/Flowering_behaviour_A.alpina/flowering_behavior_greenhouse")

###
#     The data
###
gh_noVern_dataSet = "./data/bigpopdata_af.txt"
gh_yesVern_dataSet_noFlowerArrest = "./data/AfterVern_af.txt"
gh_yesVern_dataSet = "./data/AfterVern_af_withFlowerStop_2.txt"




###
#         Plot Fig 2c
#         Consider changing the position of the perpetually flowering: they are too close to others.
#         Also, does it make sense to include perpetually flowering plants in the pop means? 
#         Otherwise the relationship is steeper than in reality
###

# Pull flowering time without vernalization

greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
unique(greenH_noVern$population)
dat <- subset(greenH_noVern, greenH_noVern$priority == 1, select = c("region","population","family","individual", "FlowAfterGerm_original", "Flowering_binominal"))
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

#######

flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, mean, na.rm=TRUE))
sd.flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, sd, na.rm=TRUE))
count.flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, function(x) length(x[!is.na(x)])))
err.flow = (1.96*sd.flTime)/(sqrt(count.flTime))

percByFamily=with(dat2, tapply(dat2$Flowering_binominal, list(dat2$population, dat2$family), mean, na.rm=TRUE))




# Pull duration of flowering
#
greenH_vern <- as.data.frame(read.table(gh_yesVern_dataSet, header = T, row.names = NULL))
vernDat <- subset(greenH_vern, greenH_vern$priority == 1, select = c("region","population","family","individual", "FlowAfterGermFlAG", "Flowering_binominal", "still_flowering_at_end", "duration", "use_for_duration"))

duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, mean, na.rm=TRUE))
sd.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, sd, na.rm=TRUE))
count.duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, function(x) length(x[!is.na(x)])))
err.duratV = (1.96*sd.duratV)/(sqrt(count.duratV))


#######
# Clean it up using experiment length = 272 days
#
for (row in 1:length(vernDat$still_flowering_at_end)) {
  if (!is.na(vernDat$still_flowering_at_end[[row]])) {
    if (vernDat$still_flowering_at_end[[row]] >= 272) {
      vernDat$duration[[row]] = 160			# NA ?
      #print(paste(vernDat$population, vernDat$still_flowering_at_end, vernDat$use_for_duration, sep = " "))
    }
    if (vernDat$use_for_duration[[row]] == 0) {
      # vernDat$duration[[row]] = NA												# uncomment to cut out perpetually flowering
      #print(paste(vernDat$population, vernDat$still_flowering_at_end, vernDat$use_for_duration, sep = " "))
    }
  }
}

#######

duratByFamilyV=with(vernDat, tapply(vernDat$duration, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))
flTimeByFamily=with(dat2, tapply(dat2$FlowAfterGerm_original, list(dat2$population, dat2$family), mean, na.rm=TRUE))




# Fill in 370 instead of non-flowered - just for plotting 
#
for (row in 1:length(percByFamily[,1])) {
  for (col in 1:length(percByFamily[1,])) {
    if (!is.na(percByFamily[row,col]) ) {
      if (percByFamily[row,col] == 0.0) {
        # print(paste(rownames(percByFamily)[[row]], colnames(percByFamily)[[col]], percByFamily[row,col], flTimeByFamily[row,col], sep=" "))
        flTimeByFamily[row,col] = 370
      }
    }
  }
}






pdf("./results/fig2c.pdf", height=7,width=7)
	# /Users/fulgione/Downloads/Wunder_ms/fig2c_2021-01-04.pdf", height=7,width=7)
par(mar=c(5,5,3,3))

symb=c(21, 24, 
       21, 24, 23,
       21, 21,
       21, 24, 23, 22, 25)
colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rep(rgb(218/255, 165/255, 32/255, 0.3), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.3), 5))
col = c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rep(rgb(218/255, 165/255, 32/255, 0.9), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.9), 5))

# Plot Spanish populations high above
flTime[[1]] = 370
flTime[[2]] = 370

# With axis labels
#plot(x=0, y=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Days to flowering (no vernalization)"), cex=1.5), xlab=list(expression("Flowering duration (after vernalization)"), cex=1.5), xlim=c(0, 150), ylim=c(0, 380))
# No axis labels
plot(x=0, y=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Days to flowering (no vernalization)"), cex=1.5), xlab=list(expression("Flowering duration (after vernalization)"), cex=1.5), xlim=c(0, 160), ylim=c(0, 380))


# Work in progress
#
# This may need commenting out
# 
# for (sp in 1:2) {
#   for (ip in 1:length(flTimeByFamily[sp,])) {
#     flTimeByFamily[sp,ip] = 370
#   }
  #points(x=jitter(duratByFamilyV[sp,]), y=jitter(flTimeByFamily[sp,]), bg = colOr[sp], pch=symb[sp], col="black", lwd=0.1)
# }


###
#     Sort and plot Spanish families
#
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
        # print(paste(rownames(as.data.frame(duratByFamilyV[,1]))[[row]], nam1, nam2, duratByFamilyV[row, r1], flTimeByFamily[row, r2], sep = " "))
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
# Add spanish families
points(x=e3d, y=rep(370, length(e3d)), bg = colOr[1], pch=symb[1], col="black", lwd=0.1)
points(x=e4d, y=rep(370, length(e3d)), bg = colOr[2], pch=symb[2], col="black", lwd=0.1)


# Separate pep1 and pajares

dat_ref <- subset(vernDat, vernDat$population %in% c("Paj", "PEP1"))
dat_ref <- droplevels(dat_ref)

durat_ref=with(dat_ref, tapply(dat_ref$duration, dat_ref$population, mean, na.rm=TRUE))
err.durat_ref=with(dat_ref, tapply(dat_ref$duration, dat_ref$population, sd, na.rm=TRUE))

# Paj
points(x=durat_ref[names(durat_ref) == 'Paj'], y=370, bg = rgb(255/255, 0/255, 0/255, 0.8), col="black", pch=symb[1:2], lwd=0.1, cex=1.5)
# Pep1
#points(x=perc_ref[2], y=flTime_ref[2], bg = rgb(255/255, 20/255, 147/255, 0.8), col="black", pch=symb[1], lwd=0.1, cex=1.5)

# Now add those that did not stop flowering - perpetually flowering
# 

perpF=with(vernDat, tapply(vernDat$still_flowering_at_end, vernDat$population, mean, na.rm=TRUE))

for (i in 1:length(duratV)) {
  if (perpF[[i]] >= 272) {
    duratV[[i]] = 160
    err.duratV[[i]] = 0.0
  }
}

# Add the points
points(x=duratV, y=flTime, bg = col, col="black", pch=symb, lwd=0.3, cex=2)


# Add errors
arrowY.start <- flTime + (err.flTime)
arrowY.end <- flTime - (err.flTime)
arrows(duratV, arrowY.start, duratV, arrowY.end, angle=90, code=3, length=0.03) 

arrowX.start <- duratV + (err.duratV)
arrowX.end <- duratV - (err.duratV)
arrows(arrowX.start, flTime, arrowX.end, flTime, angle=90, code=3, length=0.03) 

axis(1, at=c(0.0, 50, 100, 160), labels=c("0", "50", "100", ""), tick=TRUE, las=1, cex.axis=1.2)
# axis(1, at=c(100, 170), labels=F, tick=T, las=1, cex.axis=1.2)
axis(2, at=c(seq(0, 300, 100), 370), labels=c("0", "100", "200", "300", ""), tick=TRUE, las=1, cex.axis=1.2)
# axis(2, at=c(300, 400), labels=F, tick=T, las=1, cex.axis=1.2)


dev.off()




###
#       And now the legend
#
pdf("./legend_fig2c_2021-01-04.pdf", height=7,width=3)
plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)

pops=c("E3", "E4", "F1a", "F1b", "F2", "Paj", "PEP1", "S1", "S2", "S3", "S4", "S5", "")
# pops[c(6, 7, 1:5, 8:12)]
order=c(c(6, 1, 3, 5, 9, 11, 7, 2, 4, 13, 8, 10, 12))
legend("center", legend = pops[order], cex = 1.2, bty="n", pch=symb[order], pt.bg = col[order], ncol=2, y.intersp=1.5)
dev.off()










###
#       Calculate the correlation between flowering time (no vern.) and duration (after vern.)
###

greenH_noVern <- as.data.frame(read.table(gh_noVern_dataSet, header = T, row.names = NULL))
dat <- subset(greenH_noVern, greenH_noVern$priority == 1, select = c("region","population","family","individual", "FlowAfterGerm_original", "Flowering_binominal"))
pops=c("E3", "E4", "F1a", "F1b", "F2", "Paj", "PEP1", "S1", "S2", "S3", "S4", "S5")
dat2 <- subset(dat, dat$population %in% pops)
dat2 <- droplevels(dat2)

###
#     Plants that never flowered are assigned a fowering time just over the end of the experiment. 
#     Perpetually flowering plants are assigned a fowering duration just over the end of the experiment. 
#     These choices are conservative, in that they assign the least possible extreme values to the most extreme phenotypes.
###
flTime=with(dat2, tapply(dat2$FlowAfterGerm_original, dat2$population, mean, na.rm=TRUE))

flTime[names(flTime) == 'Paj'] = 360
flTime[names(flTime) == 'E3'] = 360
flTime[names(flTime) == 'E4'] = 360

duratV=with(vernDat, tapply(vernDat$duration, vernDat$population, mean, na.rm=TRUE))

duratV[names(duratV) == 'F1GAL'] = 128
duratV[names(duratV) == 'PEP1'] = 128

# duratV = duratV[c(1:5, 7:12)]
# duratV = duratV[c(3:5, 7:12)]

library(Hmisc)
cor.test( x = duratV,
          y = flTime,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.99)

# Spearman's rank correlation rho
# 
# data:  duratV and flTime
# S = 529.13, p-value = 0.0004608
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.8501001 
# 









###
#       Now on families
###
duratByFamilyV=with(vernDat, tapply(vernDat$duration, list(vernDat$population, vernDat$family), mean, na.rm=TRUE))
flTimeByFamily=with(dat2, tapply(dat2$FlowAfterGerm_original, list(dat2$population, dat2$family), mean, na.rm=TRUE))

###
#     Fill in flowering time = 360 instead of non-flowered 
#     Conservative: the most extreme phenotypes are used as least extreme
#
flTimeByFamilyforStat = flTimeByFamily

for (row in 1:length(percByFamily[,1])) {
  for (col in 1:length(percByFamily[1,])) {
    if (!is.na(percByFamily[row,col]) ) {
      if (percByFamily[row,col] == 0.0) {
        # print(paste(rownames(percByFamily)[[row]], colnames(percByFamily)[[col]], percByFamily[row,col], flTimeByFamily[row,col], sep=" "))
        flTimeByFamilyforStat[row,col] = 360
      }
    }
  }
}
###
#     Fill in flowering duration = 128 instead of perpetually flowering 
#     Conservative: the most extreme phenotypes are used as least extreme
#
duratByFamilyVforStat = duratByFamilyV
for (row in 1:length(duratByFamilyV[,1])) {
  for (col in 1:length(duratByFamilyV[1,])) {
    if (!is.na(duratByFamilyV[row,col]) ) {
      if (duratByFamilyV[row,col] == 160) {
        # print(paste(rownames(percByFamily)[[row]], colnames(percByFamily)[[col]], percByFamily[row,col], flTimeByFamily[row,col], sep=" "))
        duratByFamilyVforStat[row,col] = 128
      }
    }
  }
}



#######

flT_forCorrela=vector()
durat_forCorrela=vector()

# Build indexes for the two weird tails of figure 2c
noFlIndex = vector()
perpFlIndex = vector()


# Now put together non-missing families for both variables
# 
for (row in 1:length(duratByFamilyVforStat[,1])) {
  for (r1 in 1:length(rownames(as.data.frame(duratByFamilyVforStat[row,]))) ) {
    nam1 = rownames(as.data.frame(duratByFamilyVforStat[row,]))[[r1]]
    for (r2 in 1:length(rownames(as.data.frame(flTimeByFamilyforStat[row,]))) ) {
      nam2 = rownames(as.data.frame(flTimeByFamilyforStat[row,]))[[r2]]
      if (nam1 == nam2 && !is.na(duratByFamilyVforStat[row, r1]) && !is.na(flTimeByFamilyforStat[row, r2]) ) {
        flT_forCorrela = append(flT_forCorrela, flTimeByFamilyforStat[row, r2])
        durat_forCorrela = append(durat_forCorrela, duratByFamilyVforStat[row, r1])
        # Find vern requiring
        if (flTimeByFamilyforStat[row, r2] == 360) {
          noFlIndex = append(noFlIndex, length(flT_forCorrela))
        }
        # Find perpetual flowering
        if (duratByFamilyVforStat[row, r1] == 128) {
          perpFlIndex = append(perpFlIndex, length(durat_forCorrela))
        }
      }
    }
  }
}


flT_forCorrela[noFlIndex]
durat_forCorrela[perpFlIndex]

# plot(durat_forCorrela, flT_forCorrela)


library(Hmisc)
rcorr(x=durat_forCorrela, y=flT_forCorrela, type="spearman") # type can be pearson or spearman
cor.test( x = durat_forCorrela,
          y = flT_forCorrela,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.99)
# 
# Spearman's rank correlation rho
#
# data:  durat_forCorrela and flT_forCorrela
# S = 253070, p-value = 2.287e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.7163968 
# 





### 
#       Now stats on the weird tales
### 

###
#      Test wether plants that never flowered without vernalization have a different duration of flowering than all others

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
#
# data:  durat_forCorrela[noFlIndex] and durat_forCorrela[-noFlIndex]
# W = 373, p-value = 6.095e-08
# alternative hypothesis: true location shift is not equal to 0
# 



###    Test wether perpetually flowerint and non-perpetually flowering flower at different times
# 
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

# independent 2-group Mann-Whitney U Test
wilcox.test(flT_forCorrela[perpFlIndex], flT_forCorrela[-perpFlIndex])

# Wilcoxon rank sum test with continuity correction
#
# data:  flT_forCorrela[perpFlIndex] and flT_forCorrela[-perpFlIndex]
# W = 164, p-value = 1.827e-12
# alternative hypothesis: true location shift is not equal to 0

