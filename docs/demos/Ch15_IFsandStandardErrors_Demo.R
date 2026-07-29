library(PCRA)
library(PerformanceAnalytics)
library(xts)

## Figure 17.1
data(edhec, package = "PerformanceAnalytics")
colnames(edhec) <- c("CA", "CTAG", "DIS", "EM", "EMN", "ED", "FIA", 
                     "GM", "LS", "MA", "RV", "SS", "FoF")
library(RPEIF)
par(mfrow = c(1, 2))
outSD <- IF.SD(returns = edhec$CA, evalShape = T, IFplot = T)
outSR <- IF.SemiSD(returns = edhec$CA, evalShape = T, IFplot = T)
# outSD <- IF.SR(returns = edhec$CA, evalShape = T, IFplot = T)
# outSR <- IF.DSR(returns = edhec$CA, evalShape = T, IFplot = T)
par(mfrow = c(1, 1))

## Table 17.1
library(PCRA)
library(data.table)

# Load CRSP smallcap stocks from 2006 to 2015
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return")
dateRange <- c("2006-01-31", "2015-12-31")
returnsAll <- selectCRSPandSPGMI("monthly", dateRange = dateRange,  
                        stockItems = stockItems, factorItems = NULL,
                        subsetType = "CapGroupLast",
                        subsetValues = "SmallCap",
                        outputType= "xts")
tickers <- names(returnsAll)

# Select set of 10 stocks
k <- 0
indexes <- seq(1+k, 100+k, by = 10)
tickerSet <- tickers[indexes]
returns <- returnsAll[, tickerSet]

# Compute sample mean and its SE, and ratio of latter to former
n <- nrow(returns)
mu <- 100*apply(returns, 2, mean)
stdev <- apply(returns, 2, sd)
stderr <- 100*stdev/sqrt(n)
index <- order(mu, decreasing = TRUE)
mu <- mu[index]
stderr <- stderr[index]
ratio <- mu/stderr
# range(abs(mu)/stderr) # Minimum = 0.02 and max = 2.03

out <- data.frame(rbind(mu, stderr, ratio))
out <- round(out, 2)
row.names(out) <- c("Sample Mean", "Standard Error", "Mean/SE Ratio")
out


## Figure 17.2
library(ggplot2)
x <- factor(names(mu),levels=names(mu))
y <- mu
lo <- mu - 1.96*stderr
hi <- mu + 1.96*stderr
dat <-  data.frame(x, y, lo, hi)
ggplot(dat, aes(x, y)) + geom_point() + 
  geom_errorbar(aes(ymin = lo, ymax = hi))


##  Table 17.2
library(PCRA)
library(data.table)
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return") 
#                "Ret13WkBill", "MktIndexCRSP")
dateRange <- c("2011-01-31", "2015-12-31")
returnsAll5yr <- selectCRSPandSPGMI("monthly", dateRange = dateRange,  
                                 stockItems = stockItems, factorItems = NULL,
                                 subsetType = "CapGroupLast",
                                 subsetValues = "SmallCap",
                                 outputType= "xts")
returnsAll5yr <- returnsAll5yr[,-c(107, 108)] # Remove Market and RiskFree
tickers <- names(returnsAll5yr)
# length(tickers)
# Select set of 10 stocks
k <- 0
indexes <- seq(1+k, 100+k, by = 10)
tickerSet <- tickers[indexes]
returns <- returnsAll5yr[, tickerSet]

volEst <- function(x)
{
  n <- length(x)
  vol <- ((n-1)/n)*sd(x) # Slightly biased version
  return(vol)
}

SEvol <- function(x)
{
  n <- length(x)
  sigmaEst <- volEst(x)
  mu4Est <- (1/n)*sum((x-mean(x))^4)
  VarVol <- (mu4Est - sigmaEst^4)/(4*sigmaEst^2)
  SE <- sqrt(VarVol/n)
  return(SE)
}

SEvolNdist <- function(x)
{
  n <- length(x)
  sigmaEst <- volEst(x)
  VarVol <- 0.5*sigmaEst^2
  SE <- sqrt(VarVol/n)
  return(SE)
}

Vols <- 100*apply(returns, 2, volEst)
SEnorm <- 100*apply(returns, 2, SEvolNdist)
SE <- 100*apply(returns, 2, SEvol)
id <- order(Vols)
Vols <- Vols[id]
SEnorm <- SEnorm[id]
RatioVolsSEnorm <- Vols/SEnorm
SE <- SE[id]
RatioVolsSE <- Vols/SE
out <- rbind(Vols, SEnorm, SE, RatioVolsSEnorm, RatioVolsSE)
row.names(out) <- c("Vol", "SEnorm", "Vol/SEnorm", "SE", "Vol/SE")
out <- round(out, 2)
out

# The SE is constant at 10.95
# range(Vols/SE) # This ratio ranges from 6.73 to 12.82


##  Table 17.3
sharpeRatio = function(x, rf = 0, annual = FALSE) {   
  sr = mean(x - rf)/sd(x)    
  if(annual) {sr = sqrt(12) * sr}   
  sr 
}

SR <- sharpeRatio(returns)

# Function for SE of SR assuming normal returns distribution 
SEsharpeNdist <- function(x)
{
  n <- length(x)
  SR <- sharpeRatio(x)
  VarSRnormal <- 1+0.5*SR^2
  SE_SRnormal <- sqrt(VarSRnormal/n)
  SE_SRnormal
}

# Function for SE of SR by IFavar Method
SEsharpe <- function(x)
{
  n <- length(x)
  SRest <- sharpeRatio(x)
  SK <- SKest(x)
  eKR <- KRest(x)
  VarSR <- 1 - SK*SR + (eKR/4 + 0.5)*SR^2
  SE_SR <- sqrt(VarSR/n)
  SE_SR
}

SR <- apply(returns, 2, sharpeRatio)
SEnorm <- apply(returns, 2, SEsharpeNdist)
SE <- apply(returns, 2, SEsharpe)
id <- order(SR, decreasing = TRUE)
SR <- SR[id]
SEnorm <- SEnorm[id]
SE <- SE[id]
SRtoSEnormRatio <- SR/SEnorm
SRtoSERatio <- SR/SE

out <- round(data.frame(rbind(SR, SEnorm, SE, SRtoSEnormRatio, SRtoSERatio)),3)
row.names(out)[4:5] <- c("SR/SEnorm", "SR/SE")
out


##  Figure 17.3
#influence function of skewness for N(0,1) and N(0,4)

skIF  <- function(r, mu, sd, sk) {
  dis <- r-mu
  result <- (dis^3-3*(sd^2)*dis)/sd^3 + (0.5-3*dis^2/2/sd^2)*sk
  return(result)
}
rlim <- 4
mu <- 0
sk <- 0
r  <- seq(mu - rlim,mu + rlim,0.001)
plot(r, skIF(r,mu,1,sk), type = 'l', lty = 'solid', cex.lab = 1.5,
     xlab = 'r', ylab = 'IF(r)', main = NULL, lwd = 1, col = 'black')
lines(r, skIF(r,mu, 2,sk), type = 'l', lty = 'longdash', lwd = 1.5, col = 'black')
abline(h = 0, lty = "dotted", lwd = 0.5)
legend('topleft', legend=c('N(0,1)', 'N(0,4)'), lty = c(1,2), bty = "n", 
       lwd = c(1,1.5), col = c('black', 'black'), cex = 1.5)



## Figure 17.4
#influence function of kurtosis for N(0,1) and N(0,4)

krIF <- function(r, mu, sd, sk, kt) {
  dis <- r-mu
  result <- (dis^4-4*(sd^3)*dis*sk)/sd^4 + (1-2*dis^2/sd^2)*kt
  return(result)
}
rlim <- 4
mu <- 0
sk <- 0
kr <- 3
r <- seq(mu - rlim,mu + rlim,0.001)
plot(r, krIF(r,mu,1,sk,kr), type = 'l', lty = 'solid',
     xlab = 'r', ylab = 'Influence Function', main = NULL, lwd = 1, col = 'black')
lines(r, krIF(r,mu,2,sk,kr), type = 'l', lty = 'longdash', lwd = 1.5, col = 'black')
abline(h = 0, lty = "dotted", lwd = 0.5)
legend('center', legend=c('N(0,1)', 'N(0,4)'), lty = c(1,2), bty = 'n',
       lwd = c(1,1.5), col = c('black', 'black'),cex = 2.0)



## Table 17.4
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

## ----label = "hedgeFundsReturns", echo = F,results = F, warning = F-----------
library(PCRA)
library(RPESE)
library(xts)
data(edhec, package = "PerformanceAnalytics")
colnames(edhec) <- c("CA", "CTAG", "DIS", "EM", "EMN", "ED", "FIA", "GM", "LS", "MA",
                     "RV", "SS", "FoF")
# range(index(edhec))
# Select first 4 hedge funds from 2010 through 2019
edhec4short <- edhec["2010/",1:4]
png(file = "Plots/hedgeFundsReturns.png", width = 5, height = 3,
    units = "in", pointsize = 6, res = 600)
tsPlotMP(edhec4short, scaleType = "same", stripText.cex = 0.4, axis.cex = 0.4)
dev.off()


## ----label = "hedgeFundsSEsTab", echo = F-------------------------------------
SRout <- SR.SE(edhec4short, corOut = "retCor")
out <- printSE(SRout, round.digit = 2)
out <- data.frame(out)
names(out) <- c("SR", "SEiid", "SEcor", "RetCor")
kable(out, format = "latex", booktabs = T, linesep = "", align = "c") %>%
	kable_styling(table.env = "center") %>%
	column_spec(1:4,width = "0.65in")


## ----results = "asis",echo = F, warning = FALSE-------------------------------
library(PCRA)
library(data.table)
library(xts)

# Load CRSP smallcap stocks from 2006 to 2015
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return") 
dateRange <- c("2006-01-31", "2015-12-31")
returnsAll <- selectCRSPandSPGMI("monthly", dateRange = dateRange,  
                        stockItems = stockItems, factorItems = NULL,
                        subsetType = "CapGroupLast",
                        subsetValues = "SmallCap",
                        outputType= "xts")
returnsAll <- returnsAll[,-c(107, 108)] # Remove Market and RiskFree
tickers <- names(returnsAll)

# Select set of 10 stocks
k <- 0
indexes <- seq(1+k, 100+k, by = 10)
tickerSet <- tickers[indexes]
returns <- returnsAll[, tickerSet]

# Compute sample mean and its SE, and ratio of latter to former
n <- nrow(returns)
mu <- 100*apply(returns, 2, mean)
stdev <- apply(returns, 2, sd)
stderr <- 100*stdev/sqrt(n)
index <- order(mu, decreasing = TRUE)
mu <- mu[index]
stderr <- stderr[index]

# range(abs(mu)/stderr) # Minimum = 0.02 and max = 2.03

# Bootstrap sample mean with special sample mean function for bootstrap
sampleMeanBoot <- function(x,idx){
  n <- length(x)
  out <- sum(x[idx])/n
  return(out)
}

library(boot)
B <- 200
set.seed(50)
returnsMat <- coredata(returns)
p <- ncol(returns)
reps <- matrix(rep(0,B*p), ncol = p)
seBoot <- rep(0,p)
for(i in 1:p){
  bout <- boot(returnsMat[,i], sampleMeanBoot, B)
  reps[,i] <- bout$t
  seBoot[i] <- sd(as.numeric(bout$t))
}
bootMu <- 100*apply(reps[,index], 2, mean)
bootSE <- 100*seBoot[index]

# Form table data frame
out <- data.frame(rbind(mu, bootMu, stderr, bootSE))
out <- round(out, 2)
row.names(out) <- c("Sample Mean", "Boot Mean", "Sample SE", "Boot SE")
kable(out, format = "latex", booktabs = T, linesep = "", align = "c") %>%
	kable_styling(table.env = "center") %>%
	column_spec(1,width = "1.5in") %>%
	column_spec(2:11, width = "0.5in") %>%
	pack_rows(index= c('Sample & Boot Means' = 2, "Sample and Boot SE's" = 2))


## ----label = "bootstrapDistSampleMean", echo = F,results = F, warning = F-----
png(file = "Plots/bootstrapDistSampleMean.png", width = 6,height = 5,units = "in",pointsize = 8, res = 600)
reps = reps[, index]
boxplot(100*data.frame(reps), names = colnames(returns[, index]), 
        outline = T, col = "lightblue", notch = FALSE)
abline(h = 0, lty = "dotted")
dev.off()


## ----label ='BootstrapSharpeRatio', echo = F,results = F, warning = F---------
sharpeRatioBoot = function(x, idx, rf = 0, annual = FALSE) {   
	sr = mean(x[idx] - rf)/sd(x[idx])    
	if(annual) {sr = sqrt(12) * sr}   
	sr 
}
# Modify above to handle time-varying risk-free rate 

library(boot)
set.seed(50)
B <- 200
set.seed(50)
returnsMat <- coredata(returns)
p <- ncol(returns)
reps <- matrix(rep(0,B*p), ncol = p)
SEboot <- rep(0,p)
for(i in 1:p){
  bout <- boot(returnsMat[,i], sharpeRatioBoot, B)
  reps[,i] <- bout$t
  SEboot[i] <- sd(as.numeric(bout$t))
}
png(file = "Plots/BootstrapSharpeRatio.png", width = 6,height = 5,units = "in",pointsize = 8, res = 600)
reps = reps[, index]
boxplot(data.frame(reps), names = colnames(returns[, index]), 
        outline = T, col = "lightblue", ylim = c(-0.5,0.9))
abline(h = 0, lty = "dotted")
dev.off()


## ----label = "hedgeFundsSEsTabRPESEwithBoot_iidAndBoot_Cor", echo = F---------
se.method <- c("IFiid", "IFcor", "IFcorAdapt",  "IFcorPW", 
               "BOOTiid", "BOOTcor")[c(1, 4, 5, 6)]
SRout <- SR.SE(edhec4short, se.method = se.method, corOut = "retCor")
out <- printSE(SRout, round.digit = 2)
out <- data.frame(out)
names(out) <- c("SR", "SE-iid", "SE-cor", "SEboot-iid", 
                "SEboot-cor", "RetCor")
kable(out, format = "latex", booktabs = T, linesep = "", align = "c") %>%
	kable_styling(table.env = "center") %>%
	column_spec(1:6,width = "0.85in")

