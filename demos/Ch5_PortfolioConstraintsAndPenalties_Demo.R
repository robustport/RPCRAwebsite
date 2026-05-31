library(PCRA)
library(data.table)
library(PortfolioAnalytics)
library(CVXR)

## Figure 5.1

# Select 10 midcap stocks and risk-free rate
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return",
                "MktIndexCRSP", "Ret13WkBill")
dateRange <- c("1997-01-31", "2001-12-31")
ret <- selectCRSPandSPGMI(periodicity = "monthly",
                      dateRange = dateRange, 
                      stockItems = stockItems, 
                      factorItems = NULL,
                      subsetType = "CapGroupLast",
                      subsetValues = "MidCap",
                      outputType = "xts")
dim(ret)
# names(ret)[68:69] # 68 is the Market return and 69 is the RiskFree return

k <- 5  # The next line is for users to experiment with different stocks
nset <- seq(k, k+45, by = 5)
length(nset)
midcap10andRF <- ret[, c(nset, 69)]
names(midcap10andRF)[11] <- "RiskFree"
tsPlotMP(midcap10andRF, Pct = T, scaleType = "free",
         stripText.cex = 0.5, axis.cex = 0.5)


## Table 5.1

mu <- apply(midcap10andRF, 2, mean)
stdDev <- apply(midcap10andRF, 2, sd)
dat <- data.frame(rbind(mu, stdDev))
dat <- 100*round(dat, 4)
row.names(dat) <- c("Mean (%)", "StdDev (%)")
align <- rep("r", 11)
dat


## Table 5.2

midcap10 <- midcap10andRF[,1:10]
riskFree <- mean(midcap10andRF[,11])
print(riskFree)
funds <- colnames(midcap10)
pspec.base <- portfolio.spec(funds)
pspec.fi <- add.constraint(portfolio=pspec.base, type="full_investment")
pspec.uc <- add.objective(portfolio=pspec.fi, type="risk", name="var")
opt.uc <- optimize.portfolio(midcap10,portfolio = pspec.uc, optimize_method="CVXR")
names  <- c("WTS.UC", "MU.UC", "SD.UC", "SR.UC")
out.uc <- opt.outputMvoPCRA(opt.uc, midcap10, itemNames=names, rf=riskFree, digits = 4)
pspec.lo <- add.constraint(portfolio=pspec.uc, type="long_only")
pspec.box   <- add.constraint(portfolio=pspec.lo, type="box", min=.03, max=.25, indexnum=2)
pspec.shbox <- add.constraint(portfolio=pspec.lo, type="box", min=-.03, max=.25, indexnum=2)
opt.lo  <- optimize.portfolio(midcap10,pspec.lo, optimize_method="CVXR")
names   <- c("WTS.LO", "MU.LO", "SD.LO", "SR.LO")
out.lo  <- opt.outputMvoPCRA(opt.lo, midcap10, itemNames=names, rf=riskFree, digits = 4)
opt.box <- optimize.portfolio(midcap10, pspec.box, optimize_method="CVXR")
names   <- c("WTS.BOX", "MU.BOX", "SD.BOX", "SR.BOX")
out.box <- opt.outputMvoPCRA(opt.box,midcap10,itemNames=names,rf=riskFree,digits = 4)
opt.shbox <- optimize.portfolio(midcap10, pspec.shbox, optimize_method="CVXR")
names <- c("WTS.SHBOX", "MU.SHBOX", "SD.SHBOX", "SR.SHBOX")
out.shbox <- opt.outputMvoPCRA(opt.shbox, midcap10, itemNames=names, rf=riskFree, digits = 4)

compWts <- rbind(out.uc[[1]],out.lo[[1]],out.box[[1]],out.shbox[[1]])
compWts <- data.frame(compWts)
row.names(compWts) <- c("Unconstrained","Long Only","Long Box","Short Box")
compWts <- round(compWts,3)
align <- rep("c",10)
compWts


## Table 5.3

dat <- rbind(out.uc[2:4],out.lo[2:4],out.box[2:4],out.shbox[2:4])
mu = 100*as.numeric(dat[,1])
sd = 100*as.numeric(dat[,2])
sr = round(as.numeric(dat[,3]),3)
gmvPortsMuSdSR <- data.frame(mu,sd,sr)
names(gmvPortsMuSdSR) <- c("Mean(%)","StdDev(%)","Sharpe Ratio")
row.names(gmvPortsMuSdSR) <- c("Unconstrained","Long Only","Long Box","Short Box")
gmvPortsMuSdSR


## Figure 5.2

tsPlotMP(crsp.returns8, Pct = T, scaleType = "same",
         stripText.cex = 0.5, axis.cex = 0.5)


## Table 5.4

funds8 <- names(crsp.returns8)
cap.labels <- c(rep("MICRO",2),rep("SMALL",2),rep("MID",2),rep("LARGE",2))
pspec.base8 <- portfolio.spec(assets=funds8,category_labels=cap.labels)
pspec.fi8 <- add.constraint(pspec.base8, type="full_investment")
pspec.lo8 <- add.constraint(pspec.fi8, type="long_only")
pspec.lo8 <- add.objective(pspec.lo8, type="risk", name="var")
pspec.groups8 <- add.constraint(pspec.lo8, type="group",
                                groups=pspec.base8$category_labels,
                                group_min=c(0.1,0.15,0,0),
                                group_max=c(0.25,0.35,0.35,0.45))

# GMV portfolios with lo8 and groups8 constraints
opt.groups8 <- optimize.portfolio(crsp.returns8,pspec.groups8,optimize_method="CVXR")
names <- c("WTS.GC","MU.GC","SD.GC","SR.GC") # GC = Group Constraints
out.groups8 <- opt.outputMvoPCRA(opt.groups8,crsp.returns8,itemNames=names,digits=4,rf=0)
opt.lo8 <- optimize.portfolio(crsp.returns8,pspec.lo8,optimize_method="CVXR")
names <- c("WTS.LO","MU.LO","SD.LO","SR.LO") # LO = Long-Only
out.lo8 <- opt.outputMvoPCRA(opt.lo8,crsp.returns8,itemNames=names,digits=4,rf=0)
wts.both <- data.frame(rbind(out.groups8$WTS.GC,out.lo8$WTS.LO))
wts.both <- round(wts.both,3)
row.names(wts.both) <- c("Groups Long Boxes","Long Only")
wts.both


## Figure 5.3

# Right-hand figure
chart.GroupWeights(opt.lo8, grouping = "category", plot.type = "barplot", col = "cyan", ylim = c(0,1),
					main = "", cex.lab = 1.2, cex.axis = 1.2) 
#Left-hand figure
chart.GroupWeights(opt.groups8, grouping = "groups", plot.type = "barplot", col = "cyan" ,ylim = c(0,1),
					main = "", cex.lab = 1.5, cex.axis = 1.5) 


## Table 5.5

# Group weights
wts.groups8 <- opt.groups8$weights[c(1,3,5,7)]+opt.groups8$weights[c(2,4,6,8)]
wts.lo8 <- opt.lo8$weights[c(1,3,5,7)]+opt.lo8$weights[c(2,4,6,8)]
wts.groupsBoth <- data.frame(rbind(wts.groups8,wts.lo8))
wts.groupsBoth <- round(wts.groupsBoth,3)
names(wts.groupsBoth) <- c("Microcap","Smallcap","Midcap","Largecap")
row.names(wts.groupsBoth) <- c("Groups Long Boxes","Long Only")
wts.groupsBoth


## Table 5.6

dat <- rbind(out.groups8[2:4],out.lo8[2:4])
mu = 100*as.numeric(dat[,1])
sd = 100*as.numeric(dat[,2])
sr = round(as.numeric(dat[,3]),3)
gmvPortsMuSdSR <- data.frame(mu,sd,sr)
names(gmvPortsMuSdSR) <- c("Mean(%)","StdDev(%)","Sharpe Ratio")
row.names(gmvPortsMuSdSR) <- c("Groups Long Boxes","Long Only")
gmvPortsMuSdSR


## Table 5.7

pspec.lo.maxmean <- add.objective(portfolio=pspec.lo,type="return",
                                  name="mean",indexnum = 1)
opt <- optimize.portfolio(midcap10,pspec.lo.maxmean,
						  optimize_method = c("CVXR", "SCS"))
names <- c("WTS.LO.MAXMEAN","MEAN.LO.MAXMEAN","STDEV.LO.MAXMEAN","SR.LO.MAXMEAN")
out.maxMeanLO <- opt.outputMvoPCRA(opt,midcap10,itemNames = names, 
                                   digits = 3)
pspec.box.maxmean <- add.objective(portfolio=pspec.box,type="return",name="mean",
                                   indexnum = 1)
opt <- optimize.portfolio(midcap10,pspec.box.maxmean,
						   optimize_method = c("CVXR", "SCS"))
names <- c("WTS.BOX.MAXMEAN","MEAN.BOX.MAXMEAN","VOL.BOX.MAXMEAN","SR.BOX.MAXMEAN")
out.maxMeanBox <- opt.outputMvoPCRA(opt,midcap10,itemNames = names, 
                                    digits = 3)
outAll <- rbind(out.maxMeanLO[[1]],out.maxMeanBox[[1]])
compWts <- data.frame(outAll)
row.names(compWts) <- c("Long Only","Box[3%,25%]")
align <- rep("c",10)
compWts


## Table 5.8

midcap10MeansSorted <- round(100*sort(apply(midcap10,2,mean,drop = F),decreasing = T),2)
namesSorted <- names(midcap10MeansSorted)
midcap10VolSorted <- round(100*apply(midcap10[,namesSorted],2,sd),1)
dat <- data.frame(rbind(midcap10MeansSorted,midcap10VolSorted))
row.names(dat) <- c("Mean (%)","StdDev (%)")
align <- rep("c",10)
dat


## Table 5.9

funds <- colnames(midcap10)
pspec.qu = portfolio.spec(funds)
pspec.qu = add.constraint(portfolio=pspec.qu, type="full_investment")
pspec.qu = add.constraint(portfolio=pspec.qu, type="box",min=.03, max=.25)
pspec.qu = add.objective(portfolio=pspec.qu, type="quadratic_utility",
                         risk_aversion= 0.01)
opt1 = optimize.portfolio(midcap10, pspec.qu, optimize_method="CVXR")
names = c("WTS.QU.01","MEAN.QU.01","SD.QU.01","SR.QU.01")
outMaxQU <- opt.outputMvoPCRA(opt1,midcap10,itemNames = names,digits = 3)
outMaxQUwts <- as.data.frame(matrix(outMaxQU$WTS.QU.01,nrow = 1))
names(outMaxQUwts) <- names(midcap10)
row.names(outMaxQUwts) <- "Weights"
align <- rep("c",10)
outMaxQUwts


## Figure 5.4

# RF <- round(mean(midcap10andRF[,11]),3) Probably not needed
funds <- colnames(midcap10)
pspec.uc <- portfolio.spec(funds)
pspec.fi <- add.constraint(portfolio=pspec.uc, type="full_investment")
pspec.lo <- add.constraint(pspec.fi, type="long_only")
efront.lo <- create.EfficientFrontier(midcap10, pspec.lo,
              type="mean-StdDev", n.portfolios = 30)
pspec100.200 <- add.constraint(pspec.fi, type="box", min = -1.0,
                max = 2.0)
efront100.200 <- create.EfficientFrontier(midcap10, pspec100.200,
                              type="mean-StdDev",n.portfolios = 30)
pspec.list = list(pspec100.200,pspec.lo) # Approximation of FIonly
pspec.list = combine.portfolios(pspec.list)
legend.labels = c("Unconstrained","Long Only")

chart.EfficientFrontierOverlay(midcap10, pspec.list,
					    type="mean-StdDev", main = "",
                        match.col="StdDev", legend.loc="topleft",
                        legend.labels=legend.labels, cex.legend=1.2,
                        labels.assets=T,xlim = c(0.00,0.18),
                        ylim = c(-0.005,0.05),col = c("red","black"),
                        lty = c(3,1),lwd = c(1.0,1.0),cex = 1.0)


## Figure 5.5

midcap10 <- midcap10andRF[,1:10]
funds <- colnames(midcap10)
pspec.lo = portfolio.spec(funds)
pspec.lo = add.constraint(pspec.lo, type="full_investment")
pspec.lo = add.constraint(pspec.lo, type="long_only")
efront.lo = create.EfficientFrontier(midcap10, pspec.lo, type="mean-StdDev", n.portfolios = 30)

riskFree <- mean(midcap10andRF[,11])
chart.EfficientFrontier(efront.lo, match.col="StdDev", type="l", rf = riskFree, main = "", cex = .8,
                        xlim = c(0.00,0.18), ylim = c(-0.005,0.05))



## Figure 5.6

chart.EF.Weights(efront.lo, match.col="StdDev",
    colorset = topo.colors(10))


## Figure 5.7

pspec.base = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec.base, type="full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.box = add.constraint(pspec.fi, type="box", min=.03, max=.25)
pspec.shbox = add.constraint(pspec.fi, type="box", min=-.05, max=.25)
pspec.list = list(pspec.lo, pspec.box, pspec.shbox)
pspec.list = combine.portfolios(pspec.list)
legend.labels = c("Long Only", "Long Box", "Short Box")

chart.EfficientFrontierOverlay(midcap10, pspec.list, type="mean-StdDev",
                               match.col="StdDev", legend.loc="topleft",
                               legend.labels=legend.labels, cex.legend=0.8,
                               labels.assets=T,xlim = c(0.00,.18),
                               ylim = c(-0.005,0.04),
                               lty = c(1,2,5),lwd = c(1,1.3,1.3))


## Figure 5.8

funds <- colnames(midcap10)
pspec.uc <- portfolio.spec(funds)
pspec.fi <- add.constraint(portfolio=pspec.uc, type="full_investment")
pspec.lo <- add.constraint(pspec.fi, type="long_only")
efront.lo <- create.EfficientFrontier(midcap10, pspec.lo,
              type="mean-StdDev", n.portfolios = 30)
pspec20.120 <- add.constraint(pspec.fi, type="box", min = -0.10,
                             max = 1.10)
efront20.120 <- create.EfficientFrontier(midcap10, pspec20.120,
                             type="mean-StdDev",n.portfolios = 30)
pspec100.200 <- add.constraint(pspec.fi, type="box", min = -1.0,
                max = 2.0)
efront100.200 <- create.EfficientFrontier(midcap10, pspec100.200,
                              type="mean-StdDev",n.portfolios = 30)
pspec.list <- list(pspec.lo, pspec20.120,pspec100.200)
pspec.list <- combine.portfolios(pspec.list)
legend.labels <- c("Unconstrained","LongShort 120-20","Long Only")

chart.EfficientFrontierOverlay(midcap10, pspec.list, type="mean-StdDev",
                        match.col="StdDev", legend.loc="topleft",
                        legend.labels=legend.labels, cex.legend=0.8,
                        labels.assets=T,xlim = c(0.03,0.18),
                        ylim = c(0,0.06),col = c("red","black","black"),
                        lty = c(3,1,2),lwd = c(1,1.0,1.0))


## Figure 5.9

pspec.list8 <- list(pspec.lo8, pspec.groups8)
pspec.list8 = combine.portfolios(pspec.list8)
legend.labels <- c("Long Only","Groups Long Box Constraints")
chart.EfficientFrontierOverlay(crsp.returns8,pspec.list8,type="mean-StdDev",
                                match.col="StdDev", legend.loc="topleft",
                                legend.labels=legend.labels, cex.legend=0.8,
                                labels.assets=T,lty = c(1,2),lwd = c(1,1.3),
								                n.portfolios = 100)


## Figure 5.10

efront.groups8=create.EfficientFrontier(crsp.returns8,pspec.groups8,type="mean-StdDev", n.portfolios = 20)
chart.EF.Weights(efront.groups8,match.col = "StdDev",colorset = topo.colors(10))


## Figure 5.11

efront.groups8=create.EfficientFrontier(crsp.returns8,pspec.groups8,type="mean-StdDev", n.portfolios = 20)
chart.EF.Weights(efront.groups8,by.groups = T, match.col = "StdDev",colorset = topo.colors(10))


## Table 5.10

midcap10andMkt <- ret[,c(nset,68)]
names(midcap10andMkt)[11] <- "Market"
midcap10 <- midcap10andMkt[,1:10]
funds <- colnames(midcap10)
mkt = midcap10andMkt[,11]
betas = as.numeric(as.vector(cov(midcap10,mkt)))/as.vector(var(mkt))
names(betas) = funds
betas <- round(betas,2)
midcap10Means <- round(100*apply(midcap10,2,mean,drop = F),2)
dat <- data.frame(rbind(betas,midcap10Means))
row.names(dat) <- c("Stock Betas","Mean Returns(%)")
align <- rep("c",10)
dat


## Table 5.11

pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec,type = "full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.mktExp = add.constraint(pspec.lo, type="factor_exposure",B=betas, lower=0, upper=.6)
pspec.mktExp = add.objective(portfolio=pspec.mktExp, type="risk", name="var")
opt = optimize.portfolio(midcap10, pspec.mktExp, optimize_method="CVXR")
betaPort0006 = round(as.numeric(opt$weights%*%betas),4)
names = c("WTS.beta0006","MEAN.beta0006","SD.beta0006","SR.beta0006")
out.mktbeta0006 <- opt.outputMvoPCRA(opt,midcap10,itemNames=names,digits = 4)

pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec,type = "full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.mktExp = add.constraint(pspec.lo, type="factor_exposure",B=betas, lower=.8, upper=1.0)
pspec.mktExp = add.objective(portfolio=pspec.mktExp, type="risk", name="var")
opt = optimize.portfolio(midcap10, pspec.mktExp, optimize_method="CVXR")
betaPort0810 = round(as.numeric(opt$weights%*%betas),4)
names = c("WTS.beta0810","MEAN.beta0810","SD.beta0810","SR.beta0810")
out.mktbeta0810 <- opt.outputMvoPCRA(opt,midcap10,itemNames=names,digits = 4)

pspec = portfolio.spec(assets = funds)
pspec = add.constraint(portfolio = pspec, type = "long_only")
pspec = add.constraint(portfolio=pspec, type="factor_exposure",B=betas, lower= 1.0, upper=1.1)
pspec = add.objective(portfolio=pspec, type="risk", name="var")
opt = optimize.portfolio(midcap10, pspec, optimize_method="CVXR")
betaPort1011 = round(as.numeric(opt$weights%*%betas),4)
names = c("WTS.beta1011","MEAN.beta1011","SD.beta1011","SR.beta1011")
out.mktbeta1011 <- opt.outputMvoPCRA(opt,midcap10,itemNames=names,digits = 4)

compWts <- rbind(out.mktbeta0006[[1]],out.mktbeta0810[[1]],out.mktbeta1011[[1]])
compWts <- data.frame(compWts)
row.names(compWts) <- c("Beta 00-06","Beta 08-10","Beta 10-11")
compWts <- round(compWts,3)
align <- rep("c",10)
compWts


## Table 5.12

dat <- rbind(out.mktbeta0006[2:4],out.mktbeta0810[2:4],out.mktbeta1011[2:4])
beta <- round(c(betaPort0006,betaPort0810,betaPort1011),3)
mu = 100*as.numeric(dat[,1])
sd = 100*as.numeric(dat[,2])
sr = round(as.numeric(dat[,3]),3)
betaCstPortStats <- data.frame(beta,mu,sd,sr)
names(betaCstPortStats) <- c("Beta","Mean(%)","StdDev(%)","Sharpe Ratio")
row.names(betaCstPortStats) <- c("Beta 0.0-0.6","Beta 0.8-1.0","Beta 1.0-1.1")
betaCstPortStats


## Figure 5.12

pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec,type = "full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.mktExp0810 = add.constraint(pspec.lo, type="factor_exposure",B=betas, lower=.8, upper=1.0)
pspec.lo.mktExp = list(pspec.lo, pspec.mktExp0810)
pspec.lo.mktExp = combine.portfolios(pspec.lo.mktExp)
legend.labels <- c("Long Only","Market Exposure [0.8,1.0]")
chart.EfficientFrontierOverlay(midcap10,pspec.lo.mktExp,type="mean-StdDev",
                               match.col="StdDev", legend.loc="topleft",
                               legend.labels=legend.labels, cex.legend=0.8,
                               labels.assets=T,lty = c(1,2),lwd = c(1,1.3),
                               xlim = c(0.03,0.18), ylim = c(-0.005,0.04),
                               n.portfolios = 50)


## Figure 5.13

pspec = portfolio.spec(assets=funds)
pspec.fi = add.constraint(pspec,type = "full_investment")
pspec.lo = add.constraint(pspec.fi, type="long_only")
pspec.mktExp0810 = add.constraint(pspec.lo, type="factor_exposure",B=betas, lower=.8, upper=1.0)
efront.mktExp0810 = create.EfficientFrontier(midcap10,pspec.mktExp0810,type="mean-StdDev", n.portfolios = 50)
chart.EF.Weights(efront.mktExp0810,match.col = "StdDev",colorset = topo.colors(10))


## Table 5.13

pspec = portfolio.spec(assets=funds)
pspec.dn = add.constraint(pspec, type="dollar_neutral")
pspec.dnShBox = add.constraint(pspec.dn, type="box",min = -.3, max = 1.8)
pspec.dnShBox=add.objective(pspec.dnShBox, type="risk", name="var")
pspec.dnShBoxMuTarget=add.constraint(pspec.dnShBox, type="return",return_target=.0165)
opt = optimize.portfolio(midcap10, pspec.dnShBoxMuTarget, optimize_method = "CVXR")
names = c("WTS.DN","MEAN.DN","SD.DN","IR.DN")  # "DN" = dollar neutral
out.DN <- opt.outputMvoPCRA(opt,midcap10,itemNames = names,digits = 4)
list(SUM.DN.WTS = round(sum(opt$weights),5)) # Check that it is dollar neutral

dat <- as.data.frame(matrix(out.DN$WTS.DN,nrow = 1))
dat <- round(dat,3)
names(dat) <- names(midcap10) 
row.names(dat) <- "Weights"
align <- rep("c",10)
dat


## Table 5.14

dat <- rbind(out.DN[2:4])
mu = 100*as.numeric(dat[,1])
sd = 100*as.numeric(dat[,2])
sr = round(as.numeric(dat[,3]),3)
DNportMuSdSR <- as.data.frame(matrix(c(mu,sd,sr),nrow = 1))
names(DNportMuSdSR) <- c("Mean(%)","StdDev(%)","IR")
row.names(DNportMuSdSR) <- "Performance"
DNportMuSdSR


## Figure 5.14

efront.dnShBox = create.EfficientFrontier(midcap10,pspec.dnShBox,type = "mean-StdDev",
                                          n.portfolios = 20)
chart.EfficientFrontier(efront.dnShBox,match.col = "StdDev",rf=NULL,tangent.line = F,
                        xlim = c(0.0,0.18), ylim = c(-0.005,0.045),
                        type = "l", lty = 1, col = "black",lwd =1.5, cex = .6)


## Figure 5.15

efront.dnShBox = create.EfficientFrontier(midcap10,pspec.dnShBox,type = "mean-StdDev",
                                          n.portfolios = 20)
chart.EF.Weights(efront.dnShBox,match.col = "StdDev",colorset = topo.colors(10))


## Figure 5.16

library(PCRA)
library(data.table)
library(PortfolioAnalytics)
dateRange <- c("2008-01-31","2012-12-31") 
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return")
retAll <- selectCRSPandSPGMI(periodicity = "monthly",
                          dateRange = dateRange, 
                          stockItems = stockItems, 
                          factorItems = NULL,
                          subsetType = "CapGroupLast",
                          subsetValues = "MidCap",
                          outputType = "xts")
tickers <- names(retAll)  # 67

## Extract a random subset of retAll with set.seed() for reproducibility
n <- 20
set.seed(504)  # 933
tickerSet <- sample(tickers, size = n)
returns <- retAll[ , tickerSet]

## TO1, TO2, T03 increasingly restrictive turnover constraints

# TO1 = 1.0
funds <- colnames(returns)
pspec <- portfolio.spec(assets=funds)
pspec.fi <- add.constraint(pspec, type = "full_investment")
pspec.lo <- add.constraint(pspec.fi, type="long_only")
pspec.TO1 <- add.constraint(pspec.lo, type="turnover", turnover_target = 1.0, 
                            turnover_penalty = 0)
pspec.TO1 <- add.objective(portfolio=pspec.TO1, type="risk", name="var")

# TO2 = 0.5
pspec.TO2 <- add.constraint(pspec.lo, type="turnover", turnover_target = 0.5, 
                            turnover_penalty = 0)
pspec.TO2 <- add.objective(portfolio=pspec.TO2, type="risk", name="var")

# TO3 = 0.3
pspec.TO3 <- add.constraint(pspec.lo, type="turnover", turnover_target = 0.3, 
                             turnover_penalty = 0)
pspec.TO3 <- add.objective(portfolio=pspec.TO3, type="risk", name="var")

# No turnover constraint
pspec.NTO <- add.objective(portfolio = pspec.lo, type = "risk", name = "var")

# List of portfolio specs to be used by chart.EfficientFrotierOerlay
portList <- combine.portfolios(list(pspec.NTO, pspec.TO1, pspec.TO2, pspec.TO3))

legend_labels <- c("No TOC", "TOC = 1.0", "TOC = 0.5", "TOC = 0.3")
chart.EfficientFrontierOverlay(R = returns, portfolio_list = portList, 
                      type = "mean-StdDev", match.col = "StdDev", 
                      legend.labels = legend_labels, legend.loc = "bottomright", 
                      col = c("black", "dark green", "dark blue", "dark red"),
                      lwd = c(2,2,2,2))


## Table 5.15

vars10 <-round(apply(midcap10,2,var),4)
dat <- sort(vars10, decreasing = TRUE)
dat <- data.frame(t(dat))
row.names(dat) <- "Variances"
# mean(as.numeric(dat))
dat


## Table 5.16

funds <- colnames(midcap10)
pspec <- portfolio.spec(funds)
pspec <- add.constraint(portfolio=pspec, type="full_investment")
pspec <- add.constraint(portfolio=pspec, type="long_only")
pspec <- add.objective(portfolio=pspec, type="risk", name="var")
gmv.lo <- optimize.portfolio(midcap10,pspec,optimize_method="CVXR") # For reference comparisons

# Add the HHI penalty concentration risk aversion (CRA) values 0%, 1%, 10%
pspec.cra0 <- add.objective(pspec, type="weight_concentration", name="HHI",
                                conc_aversion=0)
minvar.lo.cra0 <- optimize.portfolio(midcap10,pspec.cra0,optimize_method="CVXR")
# all.equal(gmv.lo$weights, minvar.lo.cra0$weights) # Check for equality

pspec.cra1 <- add.objective(pspec, type="weight_concentration", name="HHI",
                                conc_aversion = 0.01)
minvar.lo.cra1 <- optimize.portfolio(midcap10,pspec.cra1,optimize_method="CVXR")

pspec.cra10 <- add.objective(pspec, type="weight_concentration", name="HHI",
                                conc_aversion = 0.1)
minvar.lo.cra10 <- optimize.portfolio(midcap10,pspec.cra10,
                                   optimize_method="CVXR")

names <- c("CRA0.WTS","CRA0.MEAN","CRA0.VOL","CRA0.SR")
out.cra0 <- opt.outputMvoPCRA(minvar.lo.cra0,midcap10,itemNames = names,digits = 3)
names <- c("CRA1.WTS","CRA1.MEAN","CRA1.VOL","CRA1.SR")
out.cra1 <- opt.outputMvoPCRA(minvar.lo.cra1,midcap10,itemNames = names,digits = 3)
names <- c("CRA10.WTS","CRA10.MEAN","CRA10.VOL","CRA10.SR")
out.cra10 <- opt.outputMvoPCRA(minvar.lo.cra10,midcap10,itemNames = names,digits = 3)
outWts <- rbind(out.cra0$CRA0.WTS, out.cra1$CRA1.WTS, out.cra10$CRA10.WTS)
outWts <- data.frame(outWts)
outWts


## Table 5.17

stats.cra0 <- c(out.cra0[[2]], out.cra0[[3]], out.cra0[[4]])
stats.cra1 <- c(out.cra1[[2]], out.cra1[[3]], out.cra1[[4]])
stats.cra10 <- c(out.cra10[[2]], out.cra10[[3]], out.cra10[[4]])
statsAll <- rbind(stats.cra0, stats.cra1, stats.cra10)
statsAll <- data.frame(statsAll)
names(statsAll) <- c("Mean", "StdDev", "SR")
row.names(statsAll) <- row.names(outWts)
statsAll


## Figure 5.17

par(mfrow = c(3,1))
barplot(minvar.lo.cra0$weights, col = "blue",ylim = c(0,.3), cex.axis = 1.5, cex.names = 1.5, cex = 1.5)
title(main = "CRA = 0%")
barplot(minvar.lo.cra1$weights, col = "blue",ylim = c(0,.3), cex.axis = 1.5, cex.names = 1.5, cex = 1.5)
title(main = "CRA = 1%")
barplot(minvar.lo.cra10$weights, col = "blue",ylim = c(0,.3), cex.axis = 1.5, cex.names = 1.5, cex = 1.5)
title(main = "CRA = 10%")
par(mfrow = c(1,1))


## Figure 5.18

pspec.base <- portfolio.spec(funds)
pspec.fi <- add.constraint(pspec.base, type="full_investment")
pspec.lo <- add.constraint(pspec.fi, type="long_only")
pspec.lo <- add.objective(pspec.lo, type="return", name="mean")
pspec.lo <- add.objective(pspec.lo,type = "risk", name = "var")
pspec.cra0 <- add.objective(pspec.lo, type="weight_concentration", name="HHI",
                                conc_aversion=0)
pspec.cra1 <- add.objective(pspec.lo, type="weight_concentration", name="HHI",
                                conc_aversion = 0.01)
pspec.cra10 <- add.objective(pspec.lo, type="weight_concentration", name="HHI",
                                conc_aversion = 0.1)
pspec.list <- list(pspec.cra0, pspec.cra1, pspec.cra10)
pspec.list <- combine.portfolios(pspec.list)
legend.labels = c("CRA = 0", "CRA = 1%", "CRA = 10%")

chart.EfficientFrontierOverlay(midcap10,pspec.list,type="mean-StdDev",match.col="StdDev",
                    legend.loc="topleft",legend.labels=legend.labels,cex.legend=0.8,
                    labels.assets=T,xlim = c(.04,.18),ylim = c(-0.005,0.035),
                    lty = c(1,2,5),lwd = c(1,1.3,1.3))


## Figure 5.19

efront.cra0 <- create.EfficientFrontier(midcap10,pspec.cra0,type="mean-StdDev",n.portfolios = 30)
chart.EF.Weights(efront.cra0,match.col="StdDev",colorset=topo.colors(10))


## Figure 5.20

efront.cra1 <- create.EfficientFrontier(midcap10,pspec.cra1,type="mean-StdDev",n.portfolios = 30)
chart.EF.Weights(efront.cra1,match.col="StdDev",colorset=topo.colors(10))


## Figure 5.21

pspec.shboxloose <- add.constraint(pspec.box,type = "box",min = -1,max = 2,indexnum = 2)

bootEfronts(midcap10,pspec.shboxloose, rf=.005, npoints=20, B = 5,Seed=5329,gmv=T, maxSR=T,
          xlim =c(0,0.2),ylim=c(0,0.10),digits = 3)


## Figure 5.22

bootEfronts(midcap10,pspec.lo, rf=.005, npoints=20, B = 5,Seed=5329,gmv=T, maxSR=T,
          xlim =c(0,0.2),ylim=c(0,0.10),digits = 3)


## Figure 5.23

# Select 10 midcap stocks and risk-free rate
stockItems <- c("Date", "TickerLast", "CapGroupLast", "Return",
                "Ret13WkBill")
dateRange <- c("1997-01-31", "2001-12-31")
ret <- selectCRSPandSPGMI(periodicity = "monthly",
                          dateRange = dateRange, 
                          stockItems = stockItems, 
                          factorItems = NULL,
                          subsetType = "CapGroupLast",
                          subsetValues = "MidCap",
                          outputType = "xts")

# ret has 68 columns, with midcap stocks in columns 1-67, and
# the CRSP risk-free T-Bill returns in column 68
nset <- seq(5, 50, by = 5)

# Create xts object of the above 10 stock returns and T-Bill returns
midcap10andRF <- ret[ , c(nset,68)]

RF <- mean(midcap10andRF[ , 11])
p <- ncol(midcap10andRF)

# Create unconstrained efficient frontier (efront)
mu <- seq(.005, .065, by = 0.01)
n <- length(mu)
muVolWtsCashRisky <- matrix(rep(0, (2+p)*n), nrow = n)
dim(muVolWtsCashRisky)
for(i in 1:n){
  mu0 <- mu[i]
  out1 <- minVarCashRisky(midcap10andRF, mu0)
  muVolWtsCashRisky[i,] <- c(out1$StdDev, out1$Mean, out1$Wgts)
}


# Plot unconstrained efficient frontier
plot(muVolWtsCashRisky[,1], muVolWtsCashRisky[,2],  
     type = "l", lty = "solid", 
     xlim = c(0.0,.21), ylim = c(-0.005,0.08), lwd = 1.3,
     xlab = "Standard Deviation", ylab = "Mean Return")

# Create and plot stocks long-only and cash long-short efront
mu <- seq(.005, 0.06, by = 0.005) # Convenient 0.06 for the plot
n <- length(mu)
muVolWtsCashRiskyLO <- matrix(rep(0,(2+p)*n), nrow=n)
for(i in 1:n){ 
  mu0 <- mu[i]
  out2 <- minVarCashRisky(midcap10andRF, mu0, LO = TRUE)
  muVolWtsCashRiskyLO[i,] <- c(out2$StdDev, out2$Mean, out2$Wgts)
}
x <- muVolWtsCashRiskyLO
lines(x[,1], x[,2],lty = "dashed", lwd = 1.5, col = "darkred")

# Create and plot stocks-only efficient frontier
#### source("Code TC/minVarRiskyLO.R")
returns10 <- midcap10andRF[, 1:10]
# apply(returns10, 2, mean)  # gives max mean = 0.3025
mu <- seq(0.01, 0.0302, by = 0.001)
n <- length(mu)
p1 <- ncol(returns10)
muVolWtsRiskyLO <- matrix(rep(0,(2+p1)*n), nrow=n)
funds <- colnames(returns10)
pspec.base <- portfolio.spec(funds)
for(i in 1:n){ 
  mu0 <- mu[i]
  out3 <- minVarRiskyLO(returns10, mu0)
  muVolWtsRiskyLO[i,] <- c(out3$StdDev, out3$Mean,out3$Wgts)
}
lines(muVolWtsRiskyLO[,1],muVolWtsRiskyLO[,2], lty = "dotted", 
      col = "blue", lwd = 1.5)

# Add stocks to plot
mu = apply(returns10, 2, mean)
vol = apply(returns10, 2, sd)
points(vol, mu, pch = 19, cex = 0.7)
text(vol, mu, names(returns10), cex = 0.7, pos = 4)
# Create legend 
leg.labels <- c("Stocks Long-Short & Cash Long-Short", 
                "Stocks Long-Only & Cash Long-Short" ,
               "Stocks Long-Only  ")
legend(x="topleft", legend = leg.labels,lty = c(1,2,3), bty = "n", 
       cex = .9, lwd = c(1.5,1.5,2.0), 
       col = c("black", "darkred", "blue"))
text(x = 0.045, y = 0.065, "Mean Risk-Free Rate  =  0.27%", cex=0.9)


## Figure 5.24

plot(muVolWtsCashRisky[,1], muVolWtsCashRisky[,2], type = "l",
     lty = "solid",
     xlab = "Portfolio Standard Deviation", 
     ylab = "Portfolio Mean Return",
     xlim = c(0.0,.18), ylim = c(0.0,0.065), lwd = 1.3)

x <- muVolWtsCashRiskyLO   
lines(x[ , 1],x[ , 2],lty = "dashed", lwd = 1.3, col = "darkred")

# Points on Cash and Stocks Long-Only Efficient Frontier
mu <- seq(.02, .055, length.out =  5)
n5 <- length(mu)
muVolWtsCashRiskyLO5 <- matrix(rep(0,(2+p)*n5), nrow = n5)
for(i in 1:n5){
  mu0 <- mu[i]
  out5 <- minVarCashRisky(midcap10andRF, mu0, LO = TRUE)
  muVolWtsCashRiskyLO5[i,] <- c(out5$StdDev, out5$Mean, out5$Wgts)
}
points(muVolWtsCashRiskyLO5[,1], muVolWtsCashRiskyLO5[,2], cex = 1.2)

# TC of Cash-Stocks Long-Only Portfolios and Implied Long-Short Mean Return
#### source("Code TC/transferCoef.R")
TC <- rep(0,n5)
names(midcap10andRF)[11] <- "RiskFree"

for(i in 1:n5){
  wtVec <- muVolWtsCashRiskyLO5[i,3:12]
  TC[i] <- transferCoef(midcap10andRF, wtVec = wtVec)
}
x <- muVolWtsCashRiskyLO5[,1]
y <- muVolWtsCashRiskyLO5[,2]/TC
points(x, y, pch = 19, cex = 1.2)

# TC  # This prints the five TC values cited in the text

# Create legend
leg.labels <- c("Stocks & Cash Long-Short Portfolios",
                "Stocks Long-Only & Cash Portfolios")
legend(x="topleft",legend = leg.labels,lty = c(1,2),bty = "n",cex = 0.9,
      lwd = c(1.5,1.5),col = c("black","blue"))
leg.labels <- c("TC Implied Stocks & Cash Long-Short",
               "Stocks Long-Only & Cash") 
legend(x=0.038,y=0.058,legend = leg.labels,bty = "n",pch = c(19,1),
       col = c("black","blue"),cex = 0.9)


