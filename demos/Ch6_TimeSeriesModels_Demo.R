## 1. First, install the devtools package using the RStudio drop-down menu:
## RStudio > Tools > Install Packages > devtools.

## 2. Install all packages that are arguments of library(pkgName) below with:
## RStudio > Tools > Install Packages > pkgName.




# options(download.file.method = "libcurl")
# devtools::install_github("robustport/PCRA")
library(PCRA)
library(xts)
# devtools::install_github("msalibian/RobStatTM") # Recent development version
library(RobStatTM)
library(MASS)
## devtools::install_github("kjellpk/optimalRhoPsi")
library(optimalRhoPsi)
## devtools::install_github("EchoRLiu/robustGarch")
library(robustGarch)
library(fit.models)
fmclass.add.class("lmfm","lmrobdetMM")

# devtools::install_github("robustport/facmodTS", force = T)
library(facmodTS)



##  Figure 6.1 (Replica of Figure 1 in MartinXia2022)

x <- seq(-4.2, 4.2, by = 0.001)
ccopt <- computeTuningPsi_modOpt(0.95)
maxRho <- max(rho_modOpt(x, cc = ccopt))

par(mfrow = c(1,2))
plot(x, rho_modOpt(x, cc = ccopt)/maxRho, type = "l", xlab = "", ylab = "", 
     ylim = c(0,1.2), main = "mOpt Rho Function", cex.main = 1.5, cex.axis = 1.5)
abline(h = 0, lty = "dotted")
plot(x, psi_modOpt(x, cc = ccopt), type = "l", xlab = "", ylab = "", ylim = c(-2.2, 2.2),
     main = "mOpt Psi Function", cex.main = 1.5, cex.axis = 1.5)
par(mfrow = c(1,1))


##  Figure 6.2  (Replica of Figure 2 in MartinXia2022)

ccopt <- computeTuningPsi_modOpt(0.95)
plot(x, wgt_modOpt(x, cc = ccopt), type = "l",
     xlab = "x", ylab = "",cex = 1.5,cex.lab = 1.5)


##  Figure 6.3 (Bottom 2 Plots Replicate of Figure 4 in MartinXia2022)

par(mfrow = c(2,2))
plotLSandRobustSFM(retMER, ylimits = c(-20,40),
				mainText ="Stock MER weekly returns 2013-2014",
            	legendPos = "topleft", 
				makePct = T)
plotLSandRobustSFM(retWTS, ylimits = c(-30,45),
				mainText = "Stock WTS weekly returns 2007-2008",
                legendPos = "topleft",
				makePct = T)
plotLSandRobustSFM(retOFG, ylimits = c(-70,250),
				mainText = "Stock OFG weekly returns 2009-2010",
                legendPos = "topleft",  
				makePct = T)
plotLSandRobustSFM(retDD, ylimits = c(-35,30),
				mainText = "Stock DD weekly returns 1986-1987",
				legendPos = "topleft",		
                makePct = T)
par(mfrow = c(1,1))


##  Figure 6.4 Liquid CRSP stocks market-cap group counts

data(CRSPLiquidMktCapGrpsCnts)
names(CRSPLiquidMktCapGrpsCnts) <- c("MicroCaps","Small Caps","Big Caps","CRSP Liquid")
capGroups <- CRSPLiquidMktCapGrpsCnts[ , 4:1]

p <- plot(capGroups, lty = c("dotted","solid","dashed","dotdash"),
          col = c("black","blue","darkgreen","red"),
          main = "Cap Group Counts")
p <- addLegend("topleft", legend.names = names(capGroups),
               lty = c("dotted","solid","dashed","dashed"),
               lwd = rep(2,4))     
p


##  Figure 6.5

# Replica of Figure 7 in MartinZia2022, freely downloadable at
# https://link.springer.com/article/10.1057/s41260-022-00258-0


##  Figure 6.6

# Replica of Figure 8 in MartinZia2022, freely downloadable at
# https://link.springer.com/article/10.1057/s41260-022-00258-0


##  Table 6.1
##  Replica of Table 1 in MartinZia2022

r1 <- c(0.3, 26.2, 14.1, 6.8, 18.1, 3.3, 3.2, 2.7, 3.2)
r2 <- c(0.5, 11.9, 4.7, 1.8, 7.5, 3.8, 4.2, 3.5, 3.8)

capGroupPcts <- data.frame(rbind(r1,r2))
cnames <- c("Threshold", "MicroPct", "SmallPct", "BigPct", "MarketPct", 
             "MicroPN", "SmallPN", "BigPN", "MarketPN")
names(capGroupPcts) <- cnames
row.names(capGroupPcts) <- NULL


##  Figure 6.7

# Huber rho
rhoHuber = function(x,cc=1.345)
{rho = ifelse(abs(x/cc) <1, 0.5*x^2, cc*abs(x)-0.5*cc^2) 
return(rho)
}
# Huber psi
psiHuber = function(x,cc=1.345)
{psi = ifelse(abs(x/cc) <1, x, cc) 
psi = ifelse(x/cc<=-1, -cc, psi)
return(psi)
}

plotHuberRho <- function(x, k = 1.345, ylim = c(0,3.5)){
  ylab = "rhoHuber(x, c = 1.345)"
  plot(x,rhoHuber(x,k), ylim = ylim, ylab = ylab, type = "l",cex.lab = 1.0)
  linepos1 = matrix(c(-k,ylim[1]-.2,-k,0.5*k^2),byrow = T, nrow = 2)
  lines(linepos1,lty = "dotted")
  linepos2 = matrix(c(k,ylim[1]-.2,k,0.5*k^2),byrow = T, nrow = 2)
  lines(linepos2,lty = "dotted")
  pointspos = matrix(c(-k,0.5*k^2,k,0.5*k^2),byrow = T, nrow = 2)
  points(pointspos,pch = 20)
}

plotHuberPsi <- function(x, k = 1.345, ylim = c(-1.5,+1.5)){
  ylab = "psiHuber(x, c = 1.345)"
  plot(x,psiHuber(x,k), ylim = ylim, ylab = ylab, type = "l",cex.lab = 1.0)
  linepos1 = matrix(c(-k,ylim[1]-.2,-k,-k),byrow = T, nrow = 2)
  lines(linepos1,lty = "dotted")
  linepos2 = matrix(c(k,ylim[1]-.2,k,k),byrow = T, nrow = 2)
  lines(linepos2,lty = "dotted")
  pointspos = matrix(c(-k,-k,k,k),byrow = T, nrow = 2)
  points(pointspos,pch = 20)
}

par(mfrow = c(1,2))
x <- seq(-3.0, +3.0, by = 0.1)
plotHuberRho(x)
plotHuberPsi(x)
par(mfrow = c(1,1))


##  Figure 6.8

par(mfrow = c(1,2))
plotLSandHuberRobustSFM(retPSC, 
                         mainText = "Stock PSC weekly returns 1987-1988",
                         legendPos = "topleft")
plotLSandHuberRobustSFM(retKBH, 
                         mainText = "Stock KBH weekly returns 2007-2008",
                         legendPos = "topleft")
par(mfrow = c(1,1))




##  Figure 6.9

data(managers, package = "PerformanceAnalytics")
names(managers)[7:10] = c("LSEQ","SP500","US10Y","RF")
tsPlotMP(managers, yname = "RETURNS", stripText.cex = 0.5, axis.cex = 0.5)


##  Figure 6.10

library(facmodTS)
fitUpDn4 <- fitTsfmUpDn("HAM4", "SP500", data = managers)
plot(fitUpDn4, SFM.line = T, line.color = c("red"), 
     line.type = c("solid"),  line.width = 1, pch = 16,
     legend.cex = 1.2, cex = 1.0, cex.axis = 1.3,
     cex.lab = 1.3, sfm.line.type = "dotted")
# class(fitUpDn4)
# summary(fitUpDn4)
# getAnywhere(summary.tsfm)  # The summary method for a tsfm object


##  Table 6.2

fitUpDn4 <- fitTsfmUpDn("HAM4", "SP500", data = managers)
UpStats4 <- fitTsfmSingleFactorStats(fitUpDn4$Up)
DnStats4 <- fitTsfmSingleFactorStats(fitUpDn4$Dn)
Beta4 <- fitTsfm("HAM4", "SP500", data = managers)
BetaStats4 <- fitTsfmSingleFactorStats(Beta4)
Betas3Stats4 <- rbind(UpStats4, DnStats4, BetaStats4)
row.names(Betas3Stats4) <- c("Up Market", "Down Market", "All Market")
Betas3Stats4


##  Figure 6.11

fitUpDn6 <- fitTsfmUpDn("HAM6", "SP500", data = managers)
plot(fitUpDn6, LSandRob = T, xlim = c(-0.12, 0.12), ylim = c(-0.05, 0.10),
     line.color = c("red", "black"), line.type = c("dashed", "solid"),
     line.width = c(1,1), pch = 16, cex = 1.0, legend.cex = 1.2, 
	 cex.axis = 1.3, cex.lab = 1.3)



##  Table 6.3

# LS Up and Down Betas
fitLsUpDn6 <- fitTsfmUpDn("HAM6", "SP500", data = managers)
UpLsStats6 <- fitTsfmSingleFactorStats(fitLsUpDn6$Up)
DnLsStats6 <- fitTsfmSingleFactorStats(fitLsUpDn6$Dn)

# Robust mOpt Up and Down Betas
fitRobUpDn6 <- fitTsfmUpDn("HAM6", "SP500", data = managers, fit.method = "Robust")
UpRobStats6 <- fitTsfmSingleFactorStats(fitRobUpDn6$Up)
DnRobStats6 <- fitTsfmSingleFactorStats(fitRobUpDn6$Dn)

# Make Output Data Frame
UpDnLsRob6 <- rbind(UpLsStats6, UpRobStats6, DnLsStats6, 
                    DnRobStats6)
row.names(UpDnLsRob6) <- c("Up Market LS", "Up Market mOpt Robust",
                           "Down Market LS", "Down Market mOpt Robust")
UpDnLsRob6


##  Figure 6.12

# Get last 5 years of HAM4, SP500 and create plot
# range(index(managers))  # Check date range
mgrs <- managers["2002-01-31/",]
tsPlotMP(mgrs[ , c(4,8)], yname = "RETURNS")


##  Figure 6.13

# Make acf plots of HAM2 and SP500
# names(mgrs) # Check names
par(mfrow = c(1,2))
for(k in c(4,8))
{
  acf(mgrs[,k], lag.max = 4, na.action = na.pass, 
      main = names(mgrs[,k]))
}
par(mfrow = c(1,1))


##  Table 6.4

fitMktLagOnlyLS <- fitTsfmLagLeadBeta("HAM4", "SP500", "RF", 
               LagLeadBeta = 2, LagOnly = TRUE, data = mgrs)
tblLagOnlyLS <- fitTsfmStats(fitMktLagOnlyLS, digits = 2)

fitMktLagOnlyRob <- fitTsfmLagLeadBeta("HAM4", "SP500", "RF", 
               LagLeadBeta = 2, LagOnly = TRUE,
               fit.method = "Robust", data = mgrs)
tblLagOnlyRob <- fitTsfmStats(fitMktLagOnlyRob, digits = 2)

row.names(tblLagOnlyRob) <- c("Estimates.rob", "Std.Errors.rob", 
                       "t-Stats.rob")
tblLagOnlyLSRob <- rbind(tblLagOnlyLS, tblLagOnlyRob)
tblLagOnlyLSRob


##  Table 6.5

fitMktLagLeadLS <- fitTsfmLagLeadBeta("HAM4", "SP500", "RF", 
               LagLeadBeta = 1, LagOnly = FALSE, data = mgrs)
tblLS <- fitTsfmStats(fitMktLagLeadLS, digits = 2)

fitMktLagLeadRob <- fitTsfmLagLeadBeta("HAM4", "SP500", "RF", 
               LagLeadBeta = 1, LagOnly = FALSE,
               fit.method = "Robust", data = mgrs)
tblRob <- fitTsfmStats(fitMktLagLeadRob, digits = 2)

row.names(tblRob) <- c("Estimates.rob", "Std.Errors.rob", 
                       "t-Stats.rob")
tblLSRob <- rbind(tblLS, tblRob)
tblLSRob

##  Table 6.6

ret <- "HAM6"
fitMktLagLeadLS <- fitTsfmLagLeadBeta(ret, "SP500", "RF", 
               LagLeadBeta = 1, LagOnly = FALSE, data = mgrs)
tblLS <- fitTsfmStats(fitMktLagLeadLS, digits = 2)

fitMktLagLeadRob <- fitTsfmLagLeadBeta(ret, "SP500", "RF", 
               LagLeadBeta = 1, LagOnly = FALSE,
               fit.method = "Robust", data = mgrs)
tblRob <- fitTsfmStats(fitMktLagLeadRob, digits = 2)

row.names(tblRob) <- c("Estimates.rob", "Std.Errors.rob", 
                       "t-Stats.rob")
tblLSRob <- rbind(tblLS, tblRob)
tblLSRob


##  Table 6.7

ret <- "HAM4"
fitLS_MT <- fitTsfmMT(ret, "SP500", "RF", data = managers)
fitRob_MT <- fitTsfmMT(ret, "SP500", "RF", fit.method = "Robust",
                    data = managers)

tblStatsMT_LS <- fitTsfmStats(fitLS_MT)
names(tblStatsMT_LS)[3] <- "DownMkt"

tblStatsMT_Rob <- fitTsfmStats(fitRob_MT)
names(tblStatsMT_Rob)[3] <- "DownMkt"
row.names(tblStatsMT_Rob) <- c("Estimates.rob", "Std.Errors.rob", "t-Stats.rob")
tblStatsMT_LSRob <- rbind(tblStatsMT_LS, tblStatsMT_Rob)
tblStatsMT_LSRob

##  Table 6.8

# data(managers, package = "PerformanceAnalytics")
# names(managers)[7:10] = c("LSEQ","SP500","US10Y","RF")
# library(facmodTS)
# source("Functions/fitTsfmStats.R")

ret <- "HAM6"
fitLS_MT <- fitTsfmMT(ret, "SP500", "RF", data = managers)
fitRob_MT <- fitTsfmMT(ret, "SP500", "RF", fit.method = "Robust",
                    data = managers)

tblStatsMT_LS <- fitTsfmStats(fitLS_MT)
names(tblStatsMT_LS)[3] <- "DownMkt"

tblStatsMT_Rob <- fitTsfmStats(fitRob_MT)
names(tblStatsMT_Rob)[3] <- "DownMkt"
row.names(tblStatsMT_Rob) <- c("Estimates.rob", "Std.Errors.rob", "t-Stats.rob")
tblStatsMT_LSRob <- rbind(tblStatsMT_LS, tblStatsMT_Rob)
tblStatsMT_LSRob

##  Table 6.9

Period <- c("7/26-6/33","7/33-6/40","7/40-6/47",
                 "7/47-6/54","7/54-6/61","7/61-6/68")
Companies <- c(415,604,731,870,890,847)
Mean <- c(1.051,1.036,0.990,1.010,0.998,0.062)
StdDev <- c(0.462,0.474,0.504,0.409,0.423,0.390)
blumeTab1 <- data.frame(cbind(Period,Companies,Mean,StdDev))
blumeTab1


##  Table 6.10

Period1 <- c("7/26-6/33","7/33-6/40","7/40-6/47",
             "7/47-6/54","7/54-6/61")
Period2 <- c("7/33-6/40","7/40-6/47",
             "7/47-6/54","7/54-6/61","7/61-6/68")
a <- c(0.320,0.265,0.526,0.343,0.399)
b <- c(0.714,0.750,0.489,0.677,0.546)

blumeTab4 <- data.frame(cbind(Period2, Period1, a, b))
names(blumeTab4) <- c("Period 2",  "Period 1",
                      "Intercept(a)", "Slope(b)")
blumeTab4


##  Figure 6.14

data(datFF4W)
datFF4 <- datFF4W["2008-01-04/2008-12-26",]
names(datFF4)[c(1,4)] <- c("MKT","MOM")
datFF4.df <- as.data.frame(datFF4)
data(retFNB)
dat5 <- data.frame(retFNB,datFF4.df)
pairs(dat5,pch=16,cex = .8)


##  Table 6.11

data(datFF3W)
datFF3 <- datFF3W["2008-01-04/2008-12-26",]
names(datFF3)[1] <- "MKT"
data(datFF4W)
datFF4 <- datFF4W["2008-01-04/2008-12-26",]
names(datFF4)[c(1,4)] <- c("MKT","MOM")
data(retFNB)
# range(index(retFNB)) # "2008-01-04" "2008-12-26"

title = "Robust mOpt and LS FF3 fit of FNB returns"
fitFF3 <- fitReturnsToFF3model(retFNB,datFF3,title = title)

title = "Robust mOpt and LS FFC4 fit of FNB returns"
fitFF4 <- fitReturnsToFF4model(retFNB,datFF4,title = title)

fitFF3tab <- fitFF3$LSRobfit
fitFF3tab$MOM <- c(" "," ")
fitFF3tab <- fitFF3tab[,c(1:4,6,5)]
row.names(fitFF3tab) <- c("FF3-LS","FF3-mOpt")
fitFF4tab <- fitFF4$LSRobfit
row.names(fitFF4tab) <- c("FFC4-LS","FFC4-mOpt")
blankRow <- rep(" ",6)
fitFF4tab5row <- rbind(fitFF3tab,blankRow,fitFF4tab)
row.names(fitFF4tab5row)[3] <- ""
fitFF4tab5row


##  Figure 6.15

fitsRobAndLS <- fitFF4$fitsRobAndLS
sideBySideQQPlot(fitsRobAndLS, fun = residuals, main = NULL,
                 xlab = "Standard Normal Quantiles",
                 ylab = "Ordered Residuals", pch = 18)


##  Table 6.12

pvalsFF3 <- fitFF3$pvalsCompare
pvalsFF3$MOM <- c(" ")
pvalsFF4 <- fitFF4$pvalsCompare
pvals <- rbind(pvalsFF3,pvalsFF4)
row.names(pvals) <- c("FF3 Fit","FFC4 Fit")
pvals


##  Table 6.13

# Use step.lmrobdetMM to get best 1-factor deletion from FFC4 model

regDatFF3 <- cbind(retFNB,datFF3)
names(regDatFF3) <- c("retFNB",names(regDatFF3)[2:4])
regDatFF3.df <- as.data.frame(regDatFF3)
fitFF3 <- lmrobdetMM(retFNB~.,data = regDatFF3.df)
step.lmrobdetMM(fitFF3) # To see results needed for Table 6.13

regDatFF4 <- cbind(retFNB,datFF4)
names(regDatFF4) <- c("retFNB",names(regDatFF4)[2:5])
regDatFF4.df <- as.data.frame(regDatFF4)
fitFF4 <- lmrobdetMM(retFNB~.,data = regDatFF4.df)
step.lmrobdetMM(fitFF4) # To see results needed for Table 6.13

# Construct Table 6.13 from above results
Factors4 <- c("All Four","-- MKT","-- SMB","-- HML","-- MOM")
RFPE4 <- c(0.222,0.237,0.236,0.219,0.232)
Factors3 <- c("Best Three","-- MKT","-- SMB","-- MOM","")
RFPE3 <- c(0.219,0.251,0.231,0.262," ")
stepsRFPE <- data.frame(cbind(Factors4, RFPE4, Factors3, RFPE3))
headerText <- c("Full Model"=2,"MKT+SMB+MOM"=2)
names(stepsRFPE) <- rep(c("Factor","RFPE"),2)
row.names(stepsRFPE) <- NULL
stepsRFPE


##  Table 6.14

# Get AIC with MASS package function stepAIC

library(MASS)
fitFF4 <- lm(retFNB ~ . , data = regDatFF4.df)
stepAIC(fitFF4, direction = "backward")

# AIC results based on computation stepAIC computation above
Factors4 <- c("All Four","-- MOM","-- HML","-- SMB","-- MKT")
AIC4 <- c(-294.6,-296.5,-293.1,-292.1,-284.5)
Factors3 <- c("Best Three","-- SMB","-- HML","-- MKT","")
AIC3 <- c(-296.5,-293.9,-292.4,-282.6,"")
stepsAIC <- data.frame(cbind(Factors4,AIC4,Factors3,AIC3))
headerText <- c("Full Model"=2,"MKT+SMB+HML"=2)
names(stepsAIC) <- rep(c("Factor","AIC"),2)
row.names(stepsAIC) <- NULL
stepsAIC



##  Figure 6.16

stocksCRSPweekly <- getPCRAData("stocksCRSPweekly")
dateRange    <- c("2002-01-01", "2011-12-31")
stockItems <- c("Date", "TickerLast", "Return")
returns <- selectCRSPandSPGMI("weekly",
                                 dateRange = dateRange,
                                 stockItems = stockItems, 
                                 factorItems = NULL, 
                                 outputType = "xts")
retGNCMA <- returns[,"GNCMA"]
# source("Functions/ewmaMeanVol.R")
library(xts)
lambda <- 0.9
nstart <- 10
cc <- 2.0
ewmaClassic <- ewmaMeanVol(retGNCMA,nstart = nstart, robMean = F, robVol = F, cc = cc,
                           lambdaMean = lambda, lambdaVol = lambda)
vol.classic <- ewmaClassic$ewmaVol
both.ts <- cbind(retGNCMA,vol.classic)
names(both.ts) <- c("GNCMA Returns","EWMAV")
tsPlotMP(both.ts, yname = NULL, stripText.cex = 0.7)


##   Figure 6.17

# Huber psi
psiHuber = function(x, cc = 2.0)
  {psi = ifelse(abs(x/cc) < 1, x, cc) 
  psi = ifelse(x/cc <= -1, -cc, psi)
  return(psi)
  }

x <- seq(-3.5,3.5,.01)
cc <- 2.0  
ylim <- c(-3.0, 3.0)
ylab <- expression(paste(psi ["hub"],"(x)"))
plot(x,psiHuber(x,cc), ylim = ylim, ylab = ylab, type = "l",
	 cex.lab = 1.4)
linepos1 <- matrix(c(-cc,ylim[1]-.2,-cc,-cc),byrow = T, nrow = 2)
lines(linepos1,lty = "dotted")
linepos2 <- matrix(c(cc,ylim[1]-.2,cc,cc),byrow = T, nrow = 2)
lines(linepos2,lty = "dotted")
pointspos <- matrix(c(-cc,-cc,cc,cc),byrow = T, nrow = 2)
points(pointspos,pch = 20)
value <- bquote(c == .(cc))
text(0, 3, value, pos = 1)
par(mfrow = c(1,1))


##  Figure 6.18

lambda <- 0.9
nstart <- 10
cc <- 2.0
ewmaClassic <- ewmaMeanVol(retGNCMA, nstart = 10, robMean = F, robVol = F, cc = cc,
                             lambdaMean = lambda, lambdaVol = lambda)
vol.classic <- ewmaClassic$ewmaVol
ewmaRob <- ewmaMeanVol(retGNCMA, nstart = 10, robMean = T, robVol = T, cc = cc,
                         lambdaMean = lambda, lambdaVol = lambda)
vol.rob <- ewmaRob$ewmaVol

par(mfrow = c(2,1))
both.ts <- cbind(retGNCMA,ewmaRob$ewmaMean)
ylim <- 1.2*range(both.ts)
plot.zoo(both.ts,plot.type = "single",lty = c(1,2), col = c("black","blue"), xlab = "",
  ylab = "Returns",ylim=ylim,main = "GNCMA RETURNS AND ROBUST EWMA MEAN")
legend("topleft",c("Stock Returns","Robust EWMA Mean"),cex = 0.8, lty = c(1,2),
       col = c("black","blue"),bty="n")
both.ts <- (cbind(vol.rob,vol.classic))
plot.zoo(both.ts,plot.type = "single",lty = c(1,2), col = c("black","red"),
            xlab = "", ylab = "Volatility", main = "CLASSICAL & ROBUST EWMA VOLATILITY")
legend("topleft",c("Robust EWMA Volatility","Classical EWMA Volatility"),lty = c(1,2),
       cex = 0.8, col = c("black","red"), bty="n")
par(mfrow = c(1,1))


##  Figure 6.19

acfRet <- acf(retGNCMA, lag.max = 6, plot = F)
acfAbsRet <- acf(abs(retGNCMA), lag.max = 6, plot = F)
par(mfrow = c(1,2))
par(pty = "s")
plot(acfRet, main = 'GNCMA Returns')
plot(acfAbsRet, main = 'GNCMA Absolute Returns')
par(pty = "m")
par(mfrow = c(1,1))


##  Figure 6.20

robFitqmle <- robGarch(retGNCMA, fitMethod = "QML")
plot(robFitqmle)

# coef(robFitqmle)
# sum(coef(robFitqmle)[2:3])


##  Figure 6.21

Mrho <- function(x)
{
  a <- 4.0
  b <- 4.3
  n <- length(x)
  rho <- rep(0,n)
  for(i in 1:n){
    if(x[i] < 4.0){
      rho[i] <-  x[i]
    } else {
      if(x[i] > 4.30){
        rho[i] <- 4.15
      } else {
        u <- x[i]
        rho[i] <- (2*(
          0.25*(u^4-a^4)
          - (1/3)*(2*a+b)*(u^3-a^3)
          + 0.5*(2*a*b+a^2)*(u^2-a^2)
        )/(b-a)^3
        -(2*b*a^2)*(u-a)/(b-a)^3
        -(1/3)*(u-a)^3/(b-a)^2
        + u)
      }
    }
  }
  rho
}

x <- seq(0,5,by = 0.1)
par(pty = "s")
plot(x,Mrho(x),type = "l", ylim = c(0,5), ylab = "p(x)")
abline(h = 4.0, lty = "dotted", lwd = 0.7)
abline(v = 4.0, lty = "dotted", lwd = 0.7)
par(pty = "m")


##  Figure 6.22

robFitBM <- robGarch(retGNCMA, fitMethod = "BM")
plot(robFitBM)


##  Table 6.15

# Standard error of sample mean for AR1 process
seMeanAR1 = function(phi, n = 60, sigma = 1){
  x = sqrt(1 + 2 * (phi - phi^n) / (1 - phi)  
        - 2/n * (phi-n*phi^n+(n-1)*phi^(n+1))/(1-phi)^2)
  x*sigma/sqrt(n)
}

# AR1 coefficient, returns stdDev, and sample size values
phis <- c(0, 0.1, 0.2, 0.3, 0.4)
phi0 <- rep(0,5)
sigma <- 0.07
n <- 60

# Compute SE Mean with Serial Correlation
sdCorr <- seMeanAR1(phis, n=n, sigma=sigma)
sdIID <- seMeanAR1(phi0, n=n, sigma=sigma)

# Compute ratio of SE's and CIiid error rate
z025 <- qnorm(.025)
ratio <- sdIID/sdCorr
arg <- z025*ratio
alphaAssumeIID <- 2*pnorm(arg)
x1 <- round(sdCorr,4)
x2 <- round(100*(sdCorr - sdIID)/sdIID,1)
x3 <- round(100*alphaAssumeIID,1)
x4 <- round(100*(alphaAssumeIID - .05)/.05)
out <- data.frame(cbind(phis, x1, x2, x3, x4))
names(out) <- c("PHI","SDcor","PI-SDcor(%)","ER(%)","PI-ER(%)")
out
