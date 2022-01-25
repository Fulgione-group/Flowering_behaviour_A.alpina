
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
fitness_data="./data/2010plantation-protocols_2010-2012_for-boxplots_05d_forGLMM_2019-05-16_JWcorrected.txt"
  # old table: fitness_data="./data/2010plantation-protocols_2010-2012_for-boxplots_05d_forGLMM_2019-05-16.txt"

fitnessExp <- as.data.frame(read.table(fitness_data, header = T, row.names = NULL))
str(fitnessExp)


#   Get silique number in 2010, 11, 12
# 
sil2010=with(fitnessExp, tapply(fitnessExp$siliques2010all, fitnessExp$population, mean, na.rm=TRUE))
err.sil2010=with(fitnessExp, tapply(fitnessExp$siliques2010all, fitnessExp$population, sd, na.rm=TRUE))
count.sil2010=with(fitnessExp, tapply(fitnessExp$siliques2010all, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.sil2010 = (1.96*err.sil2010)/(sqrt(count.sil2010))
# Now families
siliqByFam2010=with(fitnessExp, tapply(fitnessExp$siliques2010all, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

sil2011=with(fitnessExp, tapply(fitnessExp$siliques2011all, fitnessExp$population, mean, na.rm=TRUE))
err.sil2011=with(fitnessExp, tapply(fitnessExp$siliques2011all, fitnessExp$population, sd, na.rm=TRUE))
count.sil2011=with(fitnessExp, tapply(fitnessExp$siliques2011all, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.sil2011 = (1.96*err.sil2011)/(sqrt(count.sil2011))
# Now families
siliqByFam2011=with(fitnessExp, tapply(fitnessExp$siliques2011all, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

sil2012=with(fitnessExp, tapply(fitnessExp$siliques2012all, fitnessExp$population, mean, na.rm=TRUE))
err.sil2012=with(fitnessExp, tapply(fitnessExp$siliques2012all, fitnessExp$population, sd, na.rm=TRUE))
count.sil2012=with(fitnessExp, tapply(fitnessExp$siliques2012all, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.sil2012 = (1.96*err.sil2012)/(sqrt(count.sil2012))
# Now families
siliqByFam2012=with(fitnessExp, tapply(fitnessExp$siliques2012all, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

silTot=with(fitnessExp, tapply(fitnessExp$siliques20102012sumall, fitnessExp$population, mean, na.rm=TRUE))
err.silTot=with(fitnessExp, tapply(fitnessExp$siliques20102012sumall, fitnessExp$population, sd, na.rm=TRUE))
count.silTot=with(fitnessExp, tapply(fitnessExp$siliques20102012sumall, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.silTot = (1.96*err.silTot)/(sqrt(count.silTot))
# Now families
siliqByFamTot=with(fitnessExp, tapply(fitnessExp$siliques20102012sumall, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

#   Get survival in 2010, 11, 12
# 
surv2010=with(fitnessExp, tapply(fitnessExp$alive2010, fitnessExp$population, mean, na.rm=TRUE))
err.surv2010=with(fitnessExp, tapply(fitnessExp$alive2010, fitnessExp$population, sd, na.rm=TRUE))
count.surv2010=with(fitnessExp, tapply(fitnessExp$alive2010, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.surv2010 = (1.96*err.surv2010)/(sqrt(count.surv2010))
# Now families
survByFam2010=with(fitnessExp, tapply(fitnessExp$alive2010, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

surv2011=with(fitnessExp, tapply(fitnessExp$alive2011, fitnessExp$population, mean, na.rm=TRUE))
err.surv2011=with(fitnessExp, tapply(fitnessExp$alive2011, fitnessExp$population, sd, na.rm=TRUE))
count.surv2011=with(fitnessExp, tapply(fitnessExp$alive2011, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.surv2011 = (1.96*err.surv2011)/(sqrt(count.surv2011))
# Now families
survByFam2011=with(fitnessExp, tapply(fitnessExp$alive2011, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

surv2012=with(fitnessExp, tapply(fitnessExp$alive2012, fitnessExp$population, mean, na.rm=TRUE))
err.surv2012=with(fitnessExp, tapply(fitnessExp$alive2012, fitnessExp$population, sd, na.rm=TRUE))
count.surv2012=with(fitnessExp, tapply(fitnessExp$alive2012, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.surv2012 = (1.96*err.surv2012)/(sqrt(count.surv2012))
# Now families
survByFam2012=with(fitnessExp, tapply(fitnessExp$alive2012, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))


#   Get rosette # in 2010, 11, 12
# 
rose2010=with(fitnessExp, tapply(fitnessExp$rosettes2010, fitnessExp$population, mean, na.rm=TRUE))
err.rose2010=with(fitnessExp, tapply(fitnessExp$rosettes2010, fitnessExp$population, sd, na.rm=TRUE))
count.rose2010=with(fitnessExp, tapply(fitnessExp$rosettes2010, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.rose2010 = (1.96*err.rose2010)/(sqrt(count.rose2010))
# Now families
roseByFam2010=with(fitnessExp, tapply(fitnessExp$rosettes2010, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

rose2011=with(fitnessExp, tapply(fitnessExp$rosettes2011, fitnessExp$population, mean, na.rm=TRUE))
err.rose2011=with(fitnessExp, tapply(fitnessExp$rosettes2011, fitnessExp$population, sd, na.rm=TRUE))
count.rose2011=with(fitnessExp, tapply(fitnessExp$rosettes2011, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.rose2011 = (1.96*err.rose2011)/(sqrt(count.rose2011))
# Now families
roseByFam2011=with(fitnessExp, tapply(fitnessExp$rosettes2011, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))

rose2012=with(fitnessExp, tapply(fitnessExp$rosettes2012, fitnessExp$population, mean, na.rm=TRUE))
err.rose2012=with(fitnessExp, tapply(fitnessExp$rosettes2012, fitnessExp$population, sd, na.rm=TRUE))
count.rose2012=with(fitnessExp, tapply(fitnessExp$rosettes2012, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.rose2012 = (1.96*err.rose2012)/(sqrt(count.rose2012))
# Now families
roseByFam2012=with(fitnessExp, tapply(fitnessExp$rosettes2012, list(fitnessExp$population, fitnessExp$family), mean, na.rm=TRUE))


#   Get proportion of flowering plants in 2010
#
flow2010=with(fitnessExp, tapply(fitnessExp$flowered2010, fitnessExp$population, mean, na.rm=TRUE))
err.flow2010=with(fitnessExp, tapply(fitnessExp$flowered2010, fitnessExp$population, sd, na.rm=TRUE))
count.flow2010=with(fitnessExp, tapply(fitnessExp$flowered2010, fitnessExp$population, function(x) length(x[!is.na(x)])))
stErr.flow2010 = (1.96*err.flow2010)/(sqrt(count.flow2010))



###
#       Now Test for differences between PEP1 and paj
###


# Test for differences in survival between PEP1 and paj in 2010
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$alive2010, fitnessExp[fitnessExp$population == "Paj",]$alive2010)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepSurv and pajSurv
# W = 5114.5, p-value = 0.6132
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in survival between PEP1 and paj in 2011
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$alive2011, fitnessExp[fitnessExp$population == "Paj",]$alive2011)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepSurv and pajSurv
# W = 4212, p-value = 0.004827
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in survival between PEP1 and paj in 2012
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$alive2012, fitnessExp[fitnessExp$population == "Paj",]$alive2012)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepSurv and pajSurv
# W = 4009.5, p-value = 5.459e-06
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in Rosette number between PEP1 and paj in 2010
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$rosettes2010, fitnessExp[fitnessExp$population == "Paj",]$rosettes2010)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepRos and pajRos
# W = 5034.5, p-value = 0.7964
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in Rosette number between PEP1 and paj in 2011
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$rosettes2011, fitnessExp[fitnessExp$population == "Paj",]$rosettes2011)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepRos and pajRos
# W = 4124.5, p-value = 0.01687
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in Rosette number between PEP1 and paj in 2012
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$rosettes2012, fitnessExp[fitnessExp$population == "Paj",]$rosettes2012)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepRos and pajRos
# W = 3977, p-value = 0.0002948
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in Silique number between PEP1 and paj in 2010
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$siliques2010all, fitnessExp[fitnessExp$population == "Paj",]$siliques2010all)
# 
# Wilcoxon rank sum test with continuity correction
# data:  fitnessExp[fitnessExp$population == "PEP1", ]$siliques2010all and fitnessExp[fitnessExp$population == "Paj", ]$siliques2010all
# W = 5350, p-value = 0.003853
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in Silique number between PEP1 and paj in 2011
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$siliques2011all, fitnessExp[fitnessExp$population == "Paj",]$siliques2011all)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepSiliq and pajSiliq
# W = 4159, p-value = 0.002661
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in Silique number between PEP1 and paj in 2012
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$siliques2012all, fitnessExp[fitnessExp$population == "Paj",]$siliques2012all)
# 
# Wilcoxon rank sum test with continuity correction
# data:  pepSiliq and pajSiliq
# W = 4000, p-value = 2.716e-06
# alternative hypothesis: true location shift is not equal to 0

# Test for differences in Silique number between PEP1 and paj Overall
wilcox.test(fitnessExp[fitnessExp$population == "PEP1",]$siliques20102012sumall, fitnessExp[fitnessExp$population == "Paj",]$siliques20102012sumall)
#	
# Wilcoxon rank sum test with continuity correction
# data:  pepSiliq and pajSiliq
# W = 4276, p-value = 0.008309
# alternative hypothesis: true location shift is not equal to 0


# -fold difference in total seed production for PEP1 compared to Pajares, E3, E4
# 
mean(fitnessExp[fitnessExp$population == "Paj",]$siliques20102012sumall)/mean(fitnessExp[fitnessExp$population == "PEP1",]$siliques20102012sumall)
mean(fitnessExp[fitnessExp$population == "E3",]$siliques20102012sumall)/mean(fitnessExp[fitnessExp$population == "PEP1",]$siliques20102012sumall)
mean(fitnessExp[fitnessExp$population == "E4",]$siliques20102012sumall)/mean(fitnessExp[fitnessExp$population == "PEP1",]$siliques20102012sumall)















###
#         Plot Fig 5 - silique number
###
pdf("./fig5c_siliqueNumber_new.pdf", height=6,width=6)
par(mar=c(5,5,3,3))

x=c(1.1, 1.3, 1.5, 1.7)
x1 = jitter(rep(x[[1]], length(sil2010)))
x2 = x1 + 0.2
x3 = x1 + 0.4
x4 = x1 + 0.6
symb=c(21, 24, 23, 22)

colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rgb(255/255, 0/255, 0/255, 0.3), rgb(255/255, 20/255, 147/255, 0.3))
col=c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rgb(255/255, 0/255, 0/255, 0.9), rgb(255/255, 20/255, 147/255, 0.9))

plot(y=0, x=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Silique number"), cex=1.5), xlab=list(expression("Years"), cex=1.5), xlim=c(1.0, 1.8), ylim=c(0, 421))

# Plot families
for (r in 1:2) {
  xx=jitter(rep(x[[1]], length(siliqByFam2010[r,])))
  points(x=xx, y=siliqByFam2010[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  xx=xx+0.2
  points(x=xx, y=siliqByFam2011[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  xx=xx+0.2
  points(x=xx, y=siliqByFam2012[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  xx=xx+0.2
  points(x=xx, y=siliqByFamTot[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
}

# Plot population means
points(x=x1, y=sil2010, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x1, (sil2010 + (stErr.sil2010)), x1, (sil2010 - (stErr.sil2010)), angle=90, code=3, length=0.03) 

points(x=x2, y=sil2011, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x2, (sil2011 + (stErr.sil2011)), x2, (sil2011 - (stErr.sil2011)), angle=90, code=3, length=0.03) 

points(x=x3, y=sil2012, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x3, (sil2012 + (stErr.sil2012)), x3, (sil2012 - (stErr.sil2012)), angle=90, code=3, length=0.03) 

points(x=x4, y=silTot, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x4, (silTot + (stErr.silTot)), x4, (silTot - (stErr.silTot)), angle=90, code=3, length=0.03) 

# Connect the means across years
# for (i in 1:4) {
#   points(x=c(x1[[i]], x2[[i]], x3[[i]]), y=c(sil2010[[i]], sil2011[[i]], sil2012[[i]]), col = colOr[[i]], type="l", lwd=1.)
# }

axis(1, at=c(1.0, 1.2, 1.4, 1.6, 1.8), tick=TRUE, labels=F, cex.axis=1.2)
axis(1, at=c(1.1, 1.3, 1.5, 1.7), tick=F, labels=c(expression(paste("1"^"st")), expression(paste("2"^"nd")), expression(paste("3"^"rd")), expression(paste("All"))), cex.axis=1.2)
axis(2, at=seq(0, 400, 100), labels=T, tick=TRUE, las=1, cex.axis=1.2)

dev.off()






###
#         Plot Fig 5 - survival
###
pdf("./fig5c_survival_new.pdf", height=6,width=6)
par(mar=c(5,5,3,3))

x=c(1.1, 1.3, 1.5)
x1 = jitter(rep(x[[1]], length(sil2010)))
x2 = x1 + 0.2
x3 = x1 + 0.4
symb=c(21, 24, 23, 22)

colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rgb(255/255, 0/255, 0/255, 0.3), rgb(255/255, 20/255, 147/255, 0.3))
col=c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rgb(255/255, 0/255, 0/255, 0.9), rgb(255/255, 20/255, 147/255, 0.9))

plot(y=0, x=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Survival %"), cex=1.5), xlab=list(expression("Years"), cex=1.5), xlim=c(1.0, 1.6), ylim=c(0, 0.81))

# Plot families
for (r in 1:2) {
  print(max(na.omit(survByFam2010[r,])))
  xx=jitter(rep(x[[1]], length(survByFam2010[r,])))
  points(x=xx, y=survByFam2010[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  xx=xx+0.2
  points(x=xx, y=survByFam2011[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  xx=xx+0.2
  points(x=xx, y=survByFam2012[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
}

# Plot population means
points(x=x1, y=surv2010, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x1, (surv2010 + (stErr.surv2010)), x1, (surv2010 - (stErr.surv2010)), angle=90, code=3, length=0.03) 

points(x=x2, y=surv2011, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x2, (surv2011 + (stErr.surv2011)), x2, (surv2011 - (stErr.surv2011)), angle=90, code=3, length=0.03) 

points(x=x3, y=surv2012, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x3, (surv2012 + (stErr.surv2012)), x3, (surv2012 - (stErr.surv2012)), angle=90, code=3, length=0.03) 

# Connect the means across years
# for (i in 1:4) {
#   points(x=c(x1[[i]], x2[[i]], x3[[i]]), y=c(sil2010[[i]], sil2011[[i]], sil2012[[i]]), col = colOr[[i]], type="l", lwd=1.)
# }

axis(1, at=c(1.0, 1.2, 1.4, 1.6), tick=TRUE, labels=F, cex.axis=1.2)
axis(1, at=c(1.1, 1.3, 1.5), tick=F, labels=c(expression(paste("1"^"st")), expression(paste("2"^"nd")), expression(paste("3"^"rd"))), cex.axis=1.2)
axis(2, at=seq(0, 0.8, 0.4), labels=seq(0, 80, 40), tick=TRUE, las=1, cex.axis=1.2)

dev.off()





###
#         Plot Fig 5 - rosette number
###
pdf("./fig5c_rosettes_new.pdf", height=6,width=6)
par(mar=c(5,5,3,3))

x=c(1.1, 1.3, 1.5)
x1 = jitter(rep(x[[1]], length(rose2010)))
x2 = x1 + 0.2
x3 = x1 + 0.4
symb=c(21, 24, 23, 22)

colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.3), 2), rgb(255/255, 0/255, 0/255, 0.3), rgb(255/255, 20/255, 147/255, 0.3))
col=c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rgb(255/255, 0/255, 0/255, 0.9), rgb(255/255, 20/255, 147/255, 0.9))

plot(y=0, x=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("Rosette number"), cex=1.5), xlab=list(expression("Years"), cex=1.5), xlim=c(1.0, 1.6), ylim=c(0, 7.5))

# Plot families
for (r in 1:2) {
  print(max(na.omit(roseByFam2012[r,])))
  xx=jitter(rep(x[[1]], length(roseByFam2010[r,])))
  points(x=xx, y=roseByFam2010[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  xx=xx+0.2
  points(x=xx, y=roseByFam2011[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  xx=xx+0.2
  points(x=xx, y=roseByFam2012[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
}

# Plot population means
points(x=x1, y=rose2010, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x1, (rose2010 + (stErr.rose2010)), x1, (rose2010 - (stErr.rose2010)), angle=90, code=3, length=0.03) 

points(x=x2, y=rose2011, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x2, (rose2011 + (stErr.rose2011)), x2, (rose2011 - (stErr.rose2011)), angle=90, code=3, length=0.03) 

points(x=x3, y=rose2012, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
arrows( x3, (rose2012 + (stErr.rose2012)), x3, (rose2012 - (stErr.rose2012)), angle=90, code=3, length=0.03) 

# Connect the means across years
# for (i in 1:4) {
#   points(x=c(x1[[i]], x2[[i]], x3[[i]]), y=c(sil2010[[i]], sil2011[[i]], sil2012[[i]]), col = colOr[[i]], type="l", lwd=1.)
# }

axis(1, at=c(1.0, 1.2, 1.4, 1.6), tick=TRUE, labels=F, cex.axis=1.2)
axis(1, at=c(1.1, 1.3, 1.5), tick=F, labels=c(expression(paste("1"^"st")), expression(paste("2"^"nd")), expression(paste("3"^"rd"))), cex.axis=1.2)
axis(2, at=seq(0, 7.5, 2.5), labels=T, tick=TRUE, las=1, cex.axis=1.2)

dev.off()







###
#       And now the legend
#
pdf("./legend_figFitness.pdf", height=3, width=7)
plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)

pops=c("E3", "E4", "Paj", "PEP1")
order=c(3, 4, 1, 2)
legend("center", legend = pops[order], cex = 1.2, bty="n", pch=symb[order], pt.bg = col[order], ncol=4, y.intersp=1.5)
dev.off()





