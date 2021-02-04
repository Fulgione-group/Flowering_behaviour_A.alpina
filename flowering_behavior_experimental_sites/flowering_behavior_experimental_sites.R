
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
FB_expSites_data <- as.data.frame(read.table("./data/on-sitedata_E-S-F-D_2019-11-20_simplified.txt", header = T, row.names = NULL))
# str(FB_expSites_data)







#######
#####
##
#           Flowering behavior at field sites,fig 3
#           A total of 4 sites
#           LOOK FOR CORRELATION BETWEEN PERCENTAGE SURVIVED TO NEXT GENERATION AND FLOWERING BEHAVIOR!!!
#           Think about Grouped boxplots
##
#####
########

sites=c("Angliru", "Geargevaggi", "Lautaret", "MPIPZ")
# site=c("Angliru")
for (site in sites) {
  print(site)
  expSitDat <- subset(FB_expSites_data, FB_expSites_data$experimental_site %in% site)
  # Do not frop levels to keep NAs
  # expSitDat <- droplevels(expSitDat)
  # unique(expSitDat$experimental_site)
  
  #   Get Flowering_binominal_2011 and 2012
  # 
  # Eliminate plants that died already the first year
  # 
  expSitDat_sub=subset(expSitDat, expSitDat$alive_2011 == 1)
  expSitDat = expSitDat_sub
  
  flBin2011=with(expSitDat, tapply(expSitDat$Flowering_binominal_2011, expSitDat$population, mean, na.rm=TRUE))
  # flBin2011_sub=with(expSitDat_sub, tapply(expSitDat_sub$Flowering_binominal_2011, expSitDat_sub$population, mean, na.rm=TRUE))
  sd.flBin2011=with(expSitDat, tapply(expSitDat$Flowering_binominal_2011, expSitDat$population, sd, na.rm=TRUE))
  count.flBin2011=with(expSitDat, tapply(expSitDat$Flowering_binominal_2011, expSitDat$population, function(x) length(x[!is.na(x)])))
  stErr.flBin2011 = (1.96*sd.flBin2011)/(sqrt(count.flBin2011))
  
  flBin2012=with(expSitDat, tapply(expSitDat$Flowering_binominal_2012, expSitDat$population, mean, na.rm=TRUE))
  sd.flBin2012=with(expSitDat, tapply(expSitDat$Flowering_binominal_2012, expSitDat$population, sd, na.rm=TRUE))
  count.flBin2012=with(expSitDat, tapply(expSitDat$Flowering_binominal_2012, expSitDat$population, function(x) length(x[!is.na(x)])))
  stErr.flBin2012 = (1.96*sd.flBin2012)/(sqrt(count.flBin2012))
  
  survive2012=with(expSitDat, tapply(expSitDat$alive_2012, expSitDat$population, mean, na.rm=TRUE))
  sd.surv2012=with(expSitDat, tapply(expSitDat$alive_2012, expSitDat$population, sd, na.rm=TRUE))
  count.surv2012=with(expSitDat, tapply(expSitDat$alive_2012, expSitDat$population, function(x) length(x[!is.na(x)])))
  stErr.surv2012 = (1.96*sd.surv2012)/(sqrt(count.surv2012))
  
  
  # And by families
  
  percByFamily_2011=with(expSitDat, tapply(expSitDat$Flowering_binominal_2011, list(expSitDat$population, expSitDat$family), mean, na.rm=TRUE))
  survByFamily_2012=with(expSitDat, tapply(expSitDat$alive_2012, list(expSitDat$population, expSitDat$family), mean, na.rm=TRUE))
  
  
  symb=c(21, 24, 
         21, 24, 23,
         21, 21,
         21, 24, 23, 22, 25)
  colOr=c(rep(rgb(178/255, 34/255, 34/255, 0.1), 2), rep(rgb(218/255, 165/255, 32/255, 0.1), 3), rgb(255/255, 0/255, 0/255, 0.1), rgb(255/255, 20/255, 147/255, 0.1), rep(rgb(30/255, 144/255, 255/255, 0.1), 5))
  col = c(rep(rgb(178/255, 34/255, 34/255, 0.9), 2), rep(rgb(218/255, 165/255, 32/255, 0.9), 3), rgb(255/255, 0/255, 0/255, 0.8), rgb(255/255, 20/255, 147/255, 0.8), rep(rgb(30/255, 144/255, 255/255, 0.9), 5))
  
  
  
  ###
  #         Plot Fig 3
  ###
  
  pdf(paste("./results/fig3_2021-01-25_2_", site, ".pdf", sep=""), height=6,width=6)
  par(mar=c(5,5,3,3))
  
  x=c(1.2)
  x1 = jitter(rep(x[[1]], length(flBin2011)))
  x2 = x1 + 0.2
  min(x1)
  max(x2)
  
  x_more=c(1.18, 1.19, 1.2, 1.21)
  x1_1=jitter(rep(x_more[[1]], 1))
  x1_2=jitter(rep(x_more[[2]], 3))
  x1_3=jitter(rep(x_more[[3]], 2))
  x1_4=jitter(rep(x_more[[4]], 4
  ))
  
  plot(y=0, x=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("% flowering plants"), cex=1.5), xlab=list(expression("Year"), cex=1.5), xlim=c(1.1, 1.5), ylim=c(0, 1))
  
  
  # Plot families
  for (r in 1:length(percByFamily_2011[,1])) {
     xx=jitter(rep(x[[1]], length(percByFamily_2011[r,])))
     points(x=xx, y=percByFamily_2011[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
     xx=xx+0.2
     points(x=xx, y=percByFamily_2012[r,], bg = colOr[r], pch=symb[r], col=col[r], lwd=0.1)
  }
  
  
  
  # Plot population means
  points(x=x1, y=flBin2011, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
  yzero=(flBin2011 + (stErr.flBin2011))
  for (i in 1:length(yzero)) {
    if ( !is.na(yzero[[i]])) {
      if ( yzero[[i]] > 1.0) {
        yzero[[i]] = 1.0
      }
    }
  }
  yone=(flBin2011 - (stErr.flBin2011))
  for (i in 1:length(yone)) {
    if ( !is.na(yone[[i]])) {
      if ( yone[[i]] < 0.0) {
        yone[[i]] = 0.0
      }
    }
  }
  arrows( x1, yzero, x1, yone, angle=90, code=3, length=0.03) 
  points(x=x2, y=flBin2012, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
  
  yzero=(flBin2012 + (stErr.flBin2012))
  for (i in 1:length(yzero)) {
    if ( !is.na(yzero[[i]])) {
      if ( yzero[[i]] > 1.0) {
        yzero[[i]] = 1.0
      }
    }
  }
  yone=(flBin2012 - (stErr.flBin2012))
  for (i in 1:length(yone)) {
    if ( !is.na(yone[[i]])) {
      if ( yone[[i]] < 0.0) {
        yone[[i]] = 0.0
      }
    }
  }
  arrows( x2, yzero, x2, yone, angle=90, code=3, length=0.03) 
  
  
  # Connect the means across years
  # for (i in 1:10) {
    # points(x=c(x1[[i]], x2[[i]]), y=c(flBin2011[[i]], flBin2012[[i]]), col = colOr[[i]], type="l", lwd=1.)
  # }
  
  axis(1, at=c(1.1, 1.3, 1.5), tick=TRUE, labels=F, cex.axis=1.2)
  axis(1, at=c(1.2, 1.4), tick=F, labels=c(expression(paste("1"^"st")), expression(paste("2"^"nd"))), cex.axis=1.2)
  axis(2, at=seq(0, 1), labels=T, tick=TRUE, las=1, cex.axis=1.2)
  
  dev.off()
  
  
  
  
  
  
  ###
  #         Plot Fig 4 - new: Flowering behavior vs survival fitness
  ###
  
  pdf(paste("./results/fig3surv_2021-01-25_2011alive_", site, ".pdf", sep=""), height=6,width=6)
  par(mar=c(5,5,3,3))
  
  x=c(1.2)
  x1 = jitter(rep(x[[1]], length(flBin2011)))
  x2 = x1 + 0.2
  min(x1)
  max(x2)
  
  x_more=c(1.18, 1.19, 1.2, 1.21)
  x1_1=jitter(rep(x_more[[1]], 1))
  x1_2=jitter(rep(x_more[[2]], 3))
  x1_3=jitter(rep(x_more[[3]], 2))
  x1_4=jitter(rep(x_more[[4]], 4
  ))
  
  plot(y=0, x=0, bg = col, col="black", type="n", lwd=0.3, cex=2, axes=F, ylab=list(expression("% surviving"), cex=1.5), xlab=list(expression("% flowering 1st year"), cex=1.5), xlim=c(0, 1), ylim=c(0, 1))
  
  ###
  #     Sort and plot families
  #
  for (row in 1:length(percByFamily_2011[,1])) {
    for (r1 in 1:length(rownames(as.data.frame(percByFamily_2011[row,]))) ) {
      nam1 = rownames(as.data.frame(percByFamily_2011[row,]))[[r1]]
      for (r2 in 1:length(rownames(as.data.frame(survByFamily_2012[row,]))) ) {
        nam2 = rownames(as.data.frame(survByFamily_2012[row,]))[[r2]]
        if (nam1 == nam2) {
          # if (!is.na(percByFamily_2011[row, r1]) && !is.na(survByFamily_2012[row, r2]) && (rownames(percByFamily_2011)[row] == "E3" || rownames(percByFamily_2011)[row] == "E4") ) {
            # print(paste(nam1, percByFamily_2011[row, r1], survByFamily_2012[row, r2], sep=" "))
          # }
          points(x=percByFamily_2011[row, r1], y=survByFamily_2012[row, r2], bg = colOr[row], pch=symb[row], col=col[r], lwd=0.1)
        }
      }
    }
  }
  ####
  
  
  # Plot population means
  
  points(x=flBin2011, y=survive2012, bg = col, pch=symb, col="black", lwd=0.1, cex=1.5)
  xzero=(flBin2011 + (stErr.flBin2011))
  for (i in 1:length(xzero)) {
    if ( !is.na(xzero[[i]])) {
      if ( xzero[[i]] > 1.0) {
        xzero[[i]] = 1.0
      }
    }
  }
  xone=(flBin2011 - (stErr.flBin2011))
  for (i in 1:length(xone)) {
    if ( !is.na(xone[[i]])) {
      if ( xone[[i]] < 0.0) {
        xone[[i]] = 0.0
      }
    }
  }
  arrows( xzero, survive2012, xone, survive2012, angle=90, code=3, length=0.03) 
  
  yzero=(survive2012 + (stErr.surv2012))
  for (i in 1:length(yzero)) {
    if ( !is.na(yzero[[i]])) {
      if ( yzero[[i]] > 1.0) {
        yzero[[i]] = 1.0
      }
    }
  }
  yone=(survive2012 - (stErr.surv2012))
  for (i in 1:length(yone)) {
    if ( !is.na(yone[[i]])) {
      if ( yone[[i]] < 0.0) {
        yone[[i]] = 0.0
      }
    }
  }
  arrows( flBin2011, yzero, flBin2011, yone, angle=90, code=3, length=0.03) 
  
  axis(2, at=seq(0, 1), labels=T, tick=TRUE, las=1, cex.axis=1.2)
  axis(1, at=seq(0, 1), labels=T, tick=TRUE, las=1, cex.axis=1.2)
  
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###
  #       STATS now!
  #       Calculate the correlation 
  ###
  
  print("Correlation on populations")
  
  library(Hmisc)
  # rcorr(x=perc, y=flTime, type="spearman") # type can be pearson or spearman
  print(cor.test( ~ flBin2011 + survive2012, 
            data=expSitDat,
            method = "spearman",
            continuity = FALSE,
            conf.level = 0.99))
  
  
  print(cor.test( ~ flBin2011 + survive2012, 
            data=expSitDat,
            method = "pearson",
            continuity = FALSE,
            conf.level = 0.99))
  
  
  print("Without Pep1 (outlier and not a natural accession)")
  
  print(cor.test( ~ subset(flBin2011, rownames(flBin2011) != "pep1") + subset(survive2012, rownames(survive2012) != "pep1"), 
                  data=expSitDat,
                  method = "pearson",
                  continuity = FALSE,
                  conf.level = 0.99))
  
  
  ###
  #       And among family means
  #       Consider that cor.test dislikes many NAs, 
  #       So we need to clean up percByFamily and flTimeByFamily
  ###
  
  percFl_forCorr_families=vector()
  surv_forCorr_families=vector()
  
  for (row in 1:length(percByFamily_2011[,1])) {
    for (r1 in 1:length(rownames(as.data.frame(percByFamily_2011[row,]))) ) {
      nam1 = rownames(as.data.frame(percByFamily_2011[row,]))[[r1]]
      for (r2 in 1:length(rownames(as.data.frame(survByFamily_2012[row,]))) ) {
        nam2 = rownames(as.data.frame(survByFamily_2012[row,]))[[r2]]
        if (nam1 == nam2 && nam1 != "Paj" && nam1 != "pep1") {
          # print(paste(rownames(as.data.frame(percByFamily[,1]))[[row]], nam1, nam2, percByFamily[row, r1], flTimeByFamily[row, r2], sep = " "))
          if (!is.na(percByFamily_2011[row, r1]) && !is.na(survByFamily_2012[row, r2])) {
            percFl_forCorr_families = append(percFl_forCorr_families, percByFamily_2011[row, r1])
            surv_forCorr_families = append(surv_forCorr_families, survByFamily_2012[row, r2])
          }
        }
      }
    }
  }
  length(percFl_forCorr_families)
  length(surv_forCorr_families)
  
  # plot(perc_forCorr_families, flTime_forCorr_families)
  
  print("Correlation on families")
  
  print(cor.test( ~ percFl_forCorr_families + surv_forCorr_families, 
            data=expSitDat,
            method = "pearson",
            continuity = FALSE,
            conf.level = 0.95))
  
  
  
  
  
  
  ###
  #     Check for differences across regions with Fisher-Pitman permutation test
  #     Keep in mind, here, the family is considered the lowest level entity, within populations and regions
  #     make a new data frame with all families (not individual plants), pop region, fl time, perc flowering
  ### 
  
  fam=vector()
  reg=vector()
  pop=vector()
  percentag=vector()
  
  for (row in 1:length(percByFamily_2011[,1])) {
    for (col in 1:length(percByFamily_2011[1,])) {
      if (!is.na(percByFamily_2011[row,col]) ) {
        #
        # region, population and family:
        # 
        population = rownames(percByFamily_2011)[[row]]
        region = ""
        if (strsplit(population, "")[[1]]  == "E" || strsplit(population, "")[[1]]  == "P") {
          region = "E"
        } else {
          if (strsplit(population, "")[[1]]  == "F") {
            region = "F"
          } else {
            if (strsplit(population, "")[[1]]  == "S") {
              region = "S"
            }
          }
        }
        if (strsplit(population, "")[[1]]  != "p") {
          reg = append(reg, region)
          pop = append(pop, population)
          fam = append(fam, paste(population, colnames(percByFamily_2011)[[col]]))
          
          # Percentage flowering:
          percentag = append(percentag, percByFamily_2011[row,col])
        }
      }
    }
  }
  flTimeByRegion<-data.frame("family" = fam, "region" = reg, "population" = pop, "percentage" = percentag)
  
  with(flTimeByRegion, tapply(flTimeByRegion$percentage, flTimeByRegion$population, mean, na.rm=TRUE))
  
  
  ###
  #     Permutation test to check for differences across regions
  ###
  
  #
  print("Permutation test to compare families from different regions")
  
  library(coin) 
  
  # x=seq(0.0, 1.0, 0.01)
  # hist(flTimeByRegion$percentage[flTimeByRegion$region %in% c("S")], col="blue3", breaks = x)
  # hist(flTimeByRegion$percentage[flTimeByRegion$region %in% c("F")], col="red", add=T, breaks = x)
  
  print(oneway_test(flTimeByRegion$percentage[flTimeByRegion$region %in% c("S","F")] ~ flTimeByRegion$region[flTimeByRegion$region %in% c("S","F")]))
  print(oneway_test(flTimeByRegion$percentage[flTimeByRegion$region %in% c("E","F")] ~ flTimeByRegion$region[flTimeByRegion$region %in% c("E","F")]))
  print(oneway_test(flTimeByRegion$percentage[flTimeByRegion$region %in% c("E","S")] ~ flTimeByRegion$region[flTimeByRegion$region %in% c("E","S")]))
  
}



# [1] "Angliru"
# [1] "Correlation on populations"
# 
# Spearman's rank correlation rho
# 
# data:  flBin2011 and survive2012
# S = 289.52, p-value = 0.01164
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.754658 
# 
# 
# Pearson's product-moment correlation
# 
# data:  flBin2011 and survive2012
# t = -4.4193, df = 8, p-value = 0.002228
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
#   -0.9758624 -0.2499418
# sample estimates:
#   cor 
# -0.8422666 
# 
# [1] "Without Pep1 (outlier and not a natural accession)"
# 
# Pearson's product-moment correlation
# 
# data:  subset(flBin2011, rownames(flBin2011) != "pep1") and subset(survive2012, rownames(survive2012) != "pep1")
# t = -2.0535, df = 7, p-value = 0.07912
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
# -0.9431159  0.3253688
# sample estimates:
# cor 
# -0.6131403 
# 
# [1] "Correlation on families"
# 
# Pearson's product-moment correlation
# 
# data:  percFl_forCorr_families and surv_forCorr_families
# t = -2.4886, df = 143, p-value = 0.01397
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.3549655 -0.0421357
# sample estimates:
#   cor 
# -0.2037455 
# 
# [1] "Permutation test to compare families from different regions"
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("S", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("S", "F")] (F, S)
# Z = 4.9981, p-value = 5.789e-07
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "F")] (E, F)
# Z = -5.6987, p-value = 1.207e-08
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "S")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "S")] (E, S)
# Z = -5.1232, p-value = 3.003e-07
# alternative hypothesis: true mu is not equal to 0
# 
# [1] "Geargevaggi"
# [1] "Correlation on populations"
# 
# Spearman's rank correlation rho
# 
# data:  flBin2011 and survive2012
# S = 140.68, p-value = 0.6845
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# 0.1473941 
# 
# 
# Pearson's product-moment correlation
# 
# data:  flBin2011 and survive2012
# t = 0.039413, df = 8, p-value = 0.9695
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
#   -0.7441152  0.7562970
# sample estimates:
#   cor 
# 0.01393336 
# 
# [1] "Without Pep1 (outlier and not a natural accession)"
# 
# Pearson's product-moment correlation
# 
# data:  subset(flBin2011, rownames(flBin2011) != "pep1") and subset(survive2012, rownames(survive2012) != "pep1")
# t = 1.7604, df = 7, p-value = 0.1217
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
# -0.4032277  0.9322959
# sample estimates:
# cor 
# 0.5539627 
# 
# [1] "Correlation on families"
# 
# Pearson's product-moment correlation
# 
# data:  percFl_forCorr_families and surv_forCorr_families
# t = 4.6544, df = 159, p-value = 6.809e-06
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.2024490 0.4754878
# sample estimates:
#   cor 
# 0.3462805 
# 
# [1] "Permutation test to compare families from different regions"
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("S", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("S", "F")] (F, S)
# Z = 3.9993, p-value = 6.353e-05
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "F")] (E, F)
# Z = -6.208, p-value = 5.368e-10
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "S")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "S")] (E, S)
# Z = -3.9796, p-value = 6.902e-05
# alternative hypothesis: true mu is not equal to 0
# 
# [1] "Lautaret"
# [1] "Correlation on populations"
# 
# Spearman's rank correlation rho
# 
# data:  flBin2011 and survive2012
# S = 122.19, p-value = 0.9628
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.01825742 
# 
# 
# Pearson's product-moment correlation
# 
# data:  flBin2011 and survive2012
# t = -1.553, df = 7, p-value = 0.1644
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
#   -0.9230416  0.4573509
# sample estimates:
#   cor 
# -0.5062106 
# 
# [1] "Without Pep1 (outlier and not a natural accession)"
# 
# Pearson's product-moment correlation
# 
# data:  subset(flBin2011, rownames(flBin2011) != "pep1") and subset(survive2012, rownames(survive2012) != "pep1")
# t = 1.4936, df = 6, p-value = 0.1859
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
# -0.5188584  0.9389513
# sample estimates:
# cor 
# 0.5206052 
# 
# [1] "Correlation on families"
# 
# Pearson's product-moment correlation
# 
# data:  percFl_forCorr_families and surv_forCorr_families
# t = -0.34831, df = 101, p-value = 0.7283
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.2266431  0.1599592
# sample estimates:
#   cor 
# -0.03463772 
# 
# [1] "Permutation test to compare families from different regions"
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("S", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("S", "F")] (F, S)
# Z = 2.0088, p-value = 0.04456
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "F")] (E, F)
# Z = -3.7265, p-value = 0.0001942
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "S")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "S")] (E, S)
# Z = -2.8265, p-value = 0.004705
# alternative hypothesis: true mu is not equal to 0
# 
# [1] "MPIPZ"
# [1] "Correlation on populations"
# 
# Spearman's rank correlation rho
# 
# data:  flBin2011 and survive2012
# S = 309.77, p-value = 0.0008512
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho 
# -0.8773667 
# 
# 
# Pearson's product-moment correlation
# 
# data:  flBin2011 and survive2012
# t = -2.7379, df = 8, p-value = 0.02553
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
#   -0.9500354  0.1144960
# sample estimates:
#   cor 
# -0.695521 
# 
# [1] "Without Pep1 (outlier and not a natural accession)"
# 
# Pearson's product-moment correlation
# 
# data:  subset(flBin2011, rownames(flBin2011) != "pep1") and subset(survive2012, rownames(survive2012) != "pep1")
# t = -1.9516, df = 7, p-value = 0.09195
# alternative hypothesis: true correlation is not equal to 0
# 99 percent confidence interval:
# -0.9396200  0.3525574
# sample estimates:
# cor 
# -0.5936066 
# 
# [1] "Correlation on families"
# 
# Pearson's product-moment correlation
# 
# data:  percFl_forCorr_families and surv_forCorr_families
# t = -5.1144, df = 159, p-value = 8.953e-07
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.5013876 -0.2348394
# sample estimates:
#   cor 
# -0.3758616 
# 
# [1] "Permutation test to compare families from different regions"
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("S", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("S", "F")] (F, S)
# Z = 3.0484, p-value = 0.002301
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "F")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "F")] (E, F)
# Z = -5.1888, p-value = 2.117e-07
# alternative hypothesis: true mu is not equal to 0
# 
# 
# Asymptotic Two-Sample Fisher-Pitman Permutation Test
# 
# data:  flTimeByRegion$percentage[flTimeByRegion$region %in% c("E", "S")] by flTimeByRegion$region[flTimeByRegion$region %in% c("E", "S")] (E, S)
# Z = -4.6968, p-value = 2.643e-06
# alternative hypothesis: true mu is not equal to 0
# 





