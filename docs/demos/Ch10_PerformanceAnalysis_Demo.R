## 1. First, install the devtools package using the drop-down menu:
## RStudio > Tools > Install Packages > devtools

## 2. Next, install the PCRA package FROM THE RStudio CONSOLE using
## devtools::install_github("robustport/PCRA"), and load it with:
library(PCRA)
# NOTE: This PCRA is the latest version. DO NOT INSTALL PCRA from CRAN!

## 3. Install all packages that are arguments of library(packageName) below with:
## RStudio > Tools > Install Packages > PackageName
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(pa)
library(foreach)
library(ggplot2)
library(ggpubr)
# library(lemon)
library(gridExtra)
library(dplyr)
library(RobStatTM)
library(xts)
library(zoo)
library(yfR)
library(alfred)
library(lubridate)
library(boot)
library(reshape2)
library(scales)
library(quadprog)
library(jrvFinance)

## 4. Next, ensure that you have installed optimalRhoPsi
## FROM THE RStudio CONSOLE using
## devtools::install_github("kjellpk/optimalRhoPsi"), and load it with:
library(optimalRhoPsi)

# NOTE:  When loading packages, you can safely ignore the various comments
# that appear in the console in red after each package is loaded, including,
# but not limited to:
# "method overwritten", "using 6 threads", "object is masked", etc. etc.

## 5  . Finally, in RStudio select:
## Session > Set Working Directory > To Source File Location
## This makes the folder where you saved the demo script the R working directory
##
## Later, when you have more experience with the demo, you can set the working
## directory automatically using the rstudioapi library and run the entire file
## file by clicking on the Source button at the top right of the Source pane.
## To do so, first uncomment the two lines that obtain the current path and set
## the working directory using rstudioapi::getActiveDocumentContext()$path,
## then click on Source at the top right of the active document pane


# Set the working directory automatically using the rstudioapi library
# when sourcing the entire file
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

# YOU ARE NOW READY TO RUN THE CH 10 REPRODUCIBILITY CODE

### These functions are used in mutiple places - run them first#################
max_plus <- function(x, lower_bound = 0) {
  # lower_bound can be a single number or a vector wtih the same length as x
  pmax(x, lower_bound)
}


min_minus <- function(x, upper_bound = 0) {
  # lower_bound can be a single number or a vector wtih the same length as x
  pmin(x, upper_bound)
}


downside_risk <- function(x, MAR = 0) {
  # lower_bound can be a single number or a vector wtih the same length as x
  sqrt(2 * mean(pmin(x, MAR)^2))
}


es <- function(x, alpha = 0.5, scaled = TRUE) {
  upper_bound <- quantile(x, alpha)[[1]]
  es_raw <- mean(x[x <= upper_bound]) * -1

  if (!scaled) {
    return(es_raw)
  } else {
    # Compute an equivalent normal ES
    r_bar <- mean(x)
    sigma_eff <- alpha * (es_raw + r_bar) / dnorm(qnorm(alpha))
    return(sigma_eff)
  }
}

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}


# Main Code #########################################################

##  Table 10.1
writeLines("\n\nTable 10.1")
MAR <- 0 # Minimum Acceptable Return
ES_ALPHA <- 0.5 # Percentile for Expected Shortfall
START_DATE_YF <- as.Date("1989-12-01")
START_DATE_FRED <- as.Date("1990-01-01")
END_DATE <- as.Date("2019-12-31")

# # Execute this code if you want to get data from Yahoo Finance
# # Get returns of Berkshire Hathaway and S&P 500 from Yahoo Finance
# # Filter daily prices to get the last price for the month as Yahoo's
# # end of month data is unreliable and also to protect againt
# # non-synchronous trading. We don't control the sequence in which items
# # are downloaded, so resequence the columns to Date, Manager, Benchmark
# df <- yf_get(tickers = c("BRK-A", "^SP500TR"),
#              first_date = START_DATE_YF,
#              last_date  = END_DATE,
#              freq_data  = "daily") |>
#           as.data.frame() |>
#           within(delta <- ref_date - lubridate::floor_date(ref_date, "month")) |>
#           within(ref_date <- zoo::as.yearmon(ref_date)) |>
#           subset(delta == ave(delta, ref_date, FUN = max)) |>
#           subset(select = c("ref_date", "ticker", "price_adjusted")) |>
#           stats::reshape(idvar = "ref_date",
#                          timevar = "ticker",
#                          direction = "wide")
#
# colnames(df)[(grep("date",  colnames(df)))] <- "Date"
# colnames(df)[(grep("SP500", colnames(df)))] <- "SP500"
# colnames(df)[(grep("BRK",  colnames(df)))]  <- "BRK"
#
# df <- df |>
#        within(BRK   <- BRK / dplyr::lag(BRK) - 1) |>
#        within(SP500 <- SP500 / dplyr::lag(SP500) - 1) |>
#        na.omit()

# # Resequence the columns so that the portfolio comes before the benchmark
# # Then renumber rows sequentially starting at 1 and save the data frame
# df <- df[, c("Date", "BRK", "SP500")]
# rownames(df) <- 1:nrow(df)

# # Get monthly 3-month T bill yields from FRED
# # USe the alfred package as it does not require an API key
# # For simplicity, assume return = (yield expressed as a decimal)/ 12
# # i.e. yield/1200
# GS3M  <- alfred::get_fred_series("GS3M",
#                                  observation_start = START_DATE_FRED,
#                                  observation_end   = END_DATE) |>
#           `colnames<-`(c("Date", "GS3M_ret")) |>
#           within(Date <- zoo::as.yearmon(Date)) |>
#           within(GS3M_ret <- GS3M_ret / 1200)
#
# # Merge both dataframes
# BRK_SPTR_rf <- merge(df, GS3M, by = "Date")

df <- get(load("./Data/BRK_SPTR_rf.rda"))
# Compute excess returns relative to the risk free rate
BRK_SPTR_rf$SnP_xs_ret <- BRK_SPTR_rf$SP500 - BRK_SPTR_rf$GS3M_ret
BRK_SPTR_rf$brk_xs_ret <- BRK_SPTR_rf$BRK - BRK_SPTR_rf$GS3M_ret

# Compute downside excess returns relative to MAR
BRK_SPTR_rf$SnP_ds_ret <- min_minus(BRK_SPTR_rf$SP500, MAR)
BRK_SPTR_rf$brk_ds_ret <- min_minus(BRK_SPTR_rf$BRK, MAR)

# Compute logarithmic returns
BRK_SPTR_rf$SnP_ln_ret <- log(1 + BRK_SPTR_rf$SP500)
BRK_SPTR_rf$brk_ln_ret <- log(1 + BRK_SPTR_rf$BRK)
BRK_SPTR_rf$rf_ln_ret <- log(1 + BRK_SPTR_rf$GS3M_ret)

# Compute logarithmic excess returns
BRK_SPTR_rf$SnP_ln_xs_ret <- BRK_SPTR_rf$SnP_ln_ret - BRK_SPTR_rf$rf_ln_ret
BRK_SPTR_rf$brk_ln_xs_ret <- BRK_SPTR_rf$brk_ln_ret - BRK_SPTR_rf$rf_ln_ret

# Compute downside logarithmic returns (not used, computed as a sanity check)
BRK_SPTR_rf$SnP_ds_ln_ret <- min_minus(BRK_SPTR_rf$SnP_ln_ret, MAR)
BRK_SPTR_rf$brk_ds_ln_ret <- min_minus(BRK_SPTR_rf$brk_ln_ret, MAR)

# Compute active return and active log return vs. the S&P
BRK_SPTR_rf$brk_act_ret <- BRK_SPTR_rf$BRK - BRK_SPTR_rf$SP500
BRK_SPTR_rf$brk_ln_act_ret <- BRK_SPTR_rf$brk_ln_ret - BRK_SPTR_rf$SnP_ln_ret

# Compute the numerators for the various ratios
mean_xs_SnP <- mean(BRK_SPTR_rf$SnP_xs_ret)
mean_xs_brk <- mean(BRK_SPTR_rf$brk_xs_ret)

mean_ln_xs_SnP <- mean(BRK_SPTR_rf$SnP_ln_xs_ret)
mean_ln_xs_brk <- mean(BRK_SPTR_rf$brk_ln_xs_ret)

mean_act_brk <- mean(BRK_SPTR_rf$brk_act_ret)
mean_ln_act_brk <- mean(BRK_SPTR_rf$brk_ln_act_ret)

# Compute the symmetric denominators for the various ratios
sd_xs_SnP <- sd(BRK_SPTR_rf$SnP_xs_ret)
sd_xs_brk <- sd(BRK_SPTR_rf$brk_xs_ret)

sd_ln_xs_SnP <- sd(BRK_SPTR_rf$SnP_ln_xs_ret)
sd_ln_xs_brk <- sd(BRK_SPTR_rf$brk_ln_xs_ret)

sd_act_brk <- sd(BRK_SPTR_rf$brk_act_ret)
sd_ln_act_brk <- sd(BRK_SPTR_rf$brk_ln_act_ret)

# Compute the asymmetric (downside) denominators for the various ratios
dsr_MAR_SnP <- downside_risk(BRK_SPTR_rf$SP500, MAR)
dsr_MAR_brk <- downside_risk(BRK_SPTR_rf$BRK, MAR)

dsr_ln_MAR_SnP <- downside_risk(BRK_SPTR_rf$SnP_ln_ret, MAR)
dsr_ln_MAR_brk <- downside_risk(BRK_SPTR_rf$brk_ln_ret, MAR)

dsr_act_MAR_brk <- downside_risk(BRK_SPTR_rf$brk_act_ret, MAR)
dsr_ln_act_MAR_brk <- downside_risk(BRK_SPTR_rf$brk_ln_act_ret, MAR)

es_half_SnP <- es(BRK_SPTR_rf$SP500, ES_ALPHA, TRUE)
es_half_brk <- es(BRK_SPTR_rf$BRK, ES_ALPHA, TRUE)

es_ln_half_SnP <- es(BRK_SPTR_rf$SnP_ln_ret, ES_ALPHA, TRUE)
es_ln_half_brk <- es(BRK_SPTR_rf$brk_ln_ret, ES_ALPHA, TRUE)

# Now compute the performance measures
SR_SnP <- sqrt(12) * mean_xs_SnP / sd_xs_SnP
DSR_SnP <- sqrt(12) * mean_xs_SnP / dsr_MAR_SnP
LSR_SnP <- sqrt(12) * mean_ln_xs_SnP / sd_ln_xs_SnP
DLSR_SnP <- sqrt(12) * mean_ln_xs_SnP / dsr_ln_MAR_SnP
ESR_SnP <- sqrt(12) * mean_xs_SnP / es_half_SnP
LESR_SnP <- sqrt(12) * mean_ln_xs_SnP / es_ln_half_SnP

SR_brk <- sqrt(12) * mean_xs_brk / sd_xs_brk
DSR_brk <- sqrt(12) * mean_xs_brk / dsr_MAR_brk
LSR_brk <- sqrt(12) * mean_ln_xs_brk / sd_ln_xs_brk
DLSR_brk <- sqrt(12) * mean_ln_xs_brk / dsr_ln_MAR_brk
ESR_brk <- sqrt(12) * mean_xs_brk / es_half_brk
LESR_brk <- sqrt(12) * mean_ln_xs_brk / es_ln_half_brk

IR_brk <- sqrt(12) * mean_act_brk / sd_act_brk
DIR_brk <- sqrt(12) * mean_act_brk / dsr_act_MAR_brk
LIR_brk <- sqrt(12) * mean_ln_act_brk / sd_ln_act_brk
DLIR_brk <- sqrt(12) * mean_ln_act_brk / dsr_ln_act_MAR_brk

SnP_stats <- round(c(SR_SnP, DSR_SnP, LSR_SnP, DLSR_SnP, ESR_SnP, LESR_SnP), 2)
brk_stats <- round(c(SR_brk, DSR_brk, LSR_brk, DLSR_brk, ESR_brk, LESR_brk), 2)

# Create a data frame for printing using kable
brk_vs_SnP <- data.frame(rbind(SnP_stats, brk_stats))
colnames(brk_vs_SnP) <- c("SR", "DSR", "LSR", "DLSR", "ESR", "LESR")
rownames(brk_vs_SnP) <- c("S&P 500 Total Return", "Berkshire Hathaway")

# Display the dataframe
print(brk_vs_SnP)


# Figure 10.1
# Plot the Standard deviation of the Sharpe Ratio for normal RVs
# and also the number of years it takes to build a confidence interval
# whose width a fraction alpha of the mean
writeLines("\n\nFigure 10.1")
alpha <- 0.25

df <- data.frame(SR = seq(0.25, 2, 0.25))
df[["SD(Sharpe Ratio)"]] <- sqrt(1 + df[["SR"]]^2 / 2)
df[["T(Years)"]] <- (1 + df[["SR"]]^2) / (alpha^2 * df[["SR"]]^2)

# Created a melted dataframe for plotting and also change levels
# so that T plots above SD(SR) in the legend
df_melt <- df |>
  reshape2::melt(id.vars = "SR", variable.name = "Item")

df_melt[["Item"]] <- relevel(df_melt[["Item"]], "T(Years)")

# Now create a plot
plt1 <- df_melt |>
  ggplot(aes(x = SR, y = value)) +
  geom_line(aes(color = Item), linewidth = 1) +
  scale_x_continuous(
    name = "Annual Sharpe Ratio",
    breaks = seq(0.25, 2, 0.25)
  ) +
  scale_y_continuous(
    name = paste0(
      "SD(Sharpe Ratio)  /  Years required for SE(SR) to be <",
      round(100 * alpha), "% of SR"
    ),
    breaks = 2^c(0:8),
    trans = "log2"
  ) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.4, 0.9),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
print(plt1)


## Figure 10.2
## Plot the Bootstrap distribution of Sharpe Ratios for hedge fund indices
writeLines("\n\nFigure 10.2")

sharperatio <- function(x, idx, rf = 0, annual = T) {
  sr <- mean(x[idx] - rf) / sd(x[idx])
  if (annual) {
    sr <- sqrt(12) * sr
  }
  return(sr)
}

returns <- coredata(PCRA::HFstrategies)
B <- 4000
p <- length(colnames(returns))

bootSamples <- matrix(rep(0, B * p), ncol = p)
se <- rep(0, p)

for (i in 1:p) {
  bootstrapOut <- boot::boot(returns[, i], sharperatio, B)
  bootSamples[, i] <- bootstrapOut$t
  se[i] <- sd(as.numeric(bootstrapOut$t))
}

# Resequence strategies by descending median return
median_ret <- apply(bootSamples, 2, median)
index <- order(-median_ret)
bootSamples <- as.data.frame(bootSamples[, index])
colnames(bootSamples) <- colnames(returns[, index])
bootSamples[["Month"]] <- rownames(bootSamples)

bootSamples_melt <- bootSamples |>
  reshape2::melt(
    id.vars = "Month",
    variable.name = "Strategy"
  )

box_plt_bootSamples <- ggplot(
  bootSamples_melt,
  aes(x = Strategy, y = value, fill = Strategy)
) +
  geom_boxplot(notch = TRUE) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    title = "Bootstrap Distributions of Sharpe Ratios",
    x = "Hedge Fund Strategy",
    y = "Sharpe Ratio"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "solid",
    linewidth = 1,
    colour = "black"
  )

print(box_plt_bootSamples)


## Figure 10.3
## Plot the multiplier f for various values of phi
writeLines("\n\nFigure 10.3")

f <- function(phi, N) {
  x <- 1 - (1 - phi^N) / (N * (1 - phi))
  x <- 1 / sqrt(1 + 2 * phi * x / (1 - phi))
  return(x)
}

phi_vec <- c(-0.5, -0.25, 0, 0.25, 0.5)
df <- data.frame(N = seq(1, 10, 1))

for (phi in phi_vec) {
  if (phi < 0) {
    header <- paste0("\U1D719 = ", phi)
  } else if (phi == 0) {
    header <- paste0("\U1D719 =  ", phi)
  } else {
    header <- paste0("\U1D719 = +", phi)
  }
  df[[header]] <- f(phi, df[["N"]])
}

df_melt <- df |>
  reshape2::melt(id.vars = "N", variable.name = "phi")

# Now create the plot
plt2 <- df_melt |>
  ggplot(aes(x = N, y = value)) +
  geom_line(aes(color = phi, linetype = phi), linewidth = 0.75) +
  labs(x = "N", y = "f") +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.78),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(face = "italic", size = 14)
  )

print(plt2)

## Table 10.2.
## The data presented is drawn from Cornell. Medallion Fund: The Ultimate Counterexample?
## Journal of Portfolio Management 46 (4): 156–159. Exhibit 1. DOI:  https://doi.org/10.3905/jpm.2020.1.128.
## It is not distributed with the PCRA package.

## Tables 10.3 and 10.4.
## Attribute Berskshire Hathaway's returns using the q5 factor model
## All coefficients and standard errors in Table 1.3 are multiplied by 100
## so that \alpha can be expressed in percentage points per month.
writeLines("\n\nTables 10.3 and 10.4")

START_DATE <- as.Date("1989-12-31")
END_DATE <- as.Date("2019-12-31")

# Read in data for both the q5 Model and BRK and then fix the date using as.yearmon

# q5_data       <- read.csv("https://global-q.org/uploads/1/2/2/6/122679606/q5_factors_monthly_2024.csv")
load("./Data/q5_data.rda")
q5_data$Date <- as.yearmon(paste(q5_data$year, q5_data$month), format = "%Y %m")
q5_data <- subset(q5_data, select = -c(year, month))

# The Berkshire Hathaway and S&P 500 data is stored as the dataframe df in BRKSP500.rda.
BRK_data <- get(load("./Data/BRKSP500.rda"))

# Execute this if you want to get data from the internet
# Start reading BRK's prices a few days before month end to ensure it was traded
# BRK_data <- yf_get(tickers = c("BRK-A"),
#                    first_date = START_DATE - 5,
#                    last_date  = END_DATE,
#                    freq_data  = "daily") |>
#               as.data.frame() |>
#               within(delta <- ref_date - lubridate::floor_date(ref_date, "month")) |>
#               within(ref_date <- zoo::as.yearmon(ref_date)) |>
#               subset(delta == ave(delta, ref_date, FUN = max)) |>
#               subset(select = c("ref_date", "ticker", "price_adjusted")) |>
#               stats::reshape(idvar = "ref_date",
#                              timevar = "ticker",
#                              direction = "wide") |>
#               `colnames<-`(c("Date", "BRK")) |>
#               within(BRK <- BRK / dplyr::lag(BRK) - 1) |>
#               na.omit()


# q5_data is high by a factor of 100 - scale it down to express it as a decimal
q5_data[, c("R_F", "R_MKT", "R_ME", "R_IA", "R_ROE", "R_EG")] <-
  q5_data[, c("R_F", "R_MKT", "R_ME", "R_IA", "R_ROE", "R_EG")] / 100

# Match the dates for Berkshire Hathaway and the q5 Model after making them yearmons
START_DATE <- as.yearmon(START_DATE)
END_DATE <- as.yearmon(END_DATE)

q5 <- q5_data[q5_data$Date > START_DATE & q5_data$Date <= END_DATE, ]
BRK <- BRK_data[BRK_data$Date > START_DATE & BRK_data$Date <= END_DATE, ]

# After matching dates to equalize length, create excess returns for the market and for BRK
q5$rm_rf <- q5$R_MKT - q5$R_F
BRK$BRK_rf <- BRK$BRK - q5$R_F

# Now compute the 5 factor alpha using both lm and lmrobdetMM
fitLS <- lm(BRK$BRK_rf ~ q5$rm_rf + q5$R_ME + q5$R_IA + q5$R_ROE + q5$R_EG)
fitRob <- lmrobdetMM(BRK$BRK_rf ~ q5$rm_rf + q5$R_ME + q5$R_IA + q5$R_ROE + q5$R_EG)

sumFitLS <- summary(fitLS)
sumFitRob <- summary(fitRob)

# class(sumFit), names(sumFit), sumFit$coefficients, class(sumFit$coefficients), row.names(sumFit$coefficients)

coeffsLS <- as.data.frame(sumFitLS$coefficients)
coeffsLS[, c("Estimate", "Std. Error")] <- round(coeffsLS[, c("Estimate", "Std. Error")] * 100, 3)
coeffsLS[, c("t value", "Pr(>|t|)")] <- round(coeffsLS[, c("t value", "Pr(>|t|)")], 2)

coeffsRob <- as.data.frame(sumFitRob$coefficients)
coeffsRob[, c("Estimate", "Std. Error")] <- round(coeffsRob[, c("Estimate", "Std. Error")] * 100, 3)
coeffsRob[, c("t value", "Pr(>|t|)")] <- round(coeffsRob[, c("t value", "Pr(>|t|)")], 2)

my_row_names <- c("Alpha", "rM-rf", "Size", "Investment/Assets", "ROE", "Expected Growth")
my_col_names <- c("Coeff x 100", "Std. Err. x 100", "t(Coef.)", "Pr[>|t|]")

rownames(coeffsLS) <- my_row_names
colnames(coeffsLS) <- my_col_names

rownames(coeffsRob) <- my_row_names
colnames(coeffsRob) <- my_col_names

print(coeffsLS)
writeLines("\n")
print(coeffsRob)


## Table 10.5.
## The impact of rebalancing on Sharpe Ratios
# Leland's two period Sharpe ratio
writeLines("\n\nTable 10.5")

two_period_sr <- function(r_stock_up = 0.1, r_stock_down = 0.1,
                          p_stock_up = 0.6, mu_cash = 0.02,
                          sell_stock_up = 1.0, buy_stock_down = 1.0,
                          initial_stock = 0.5, initial_cash = 0.5) {
  # t = 0
  # print(c("r_stock_up=", r_stock_up))
  # print(c("r_stock_down=", r_stock_down))
  # print(c("p_stock_up=", p_stock_up))
  # print(c("mu_cash=", mu_cash))
  # print(c("sell_stock_up=", sell_stock_up))
  # print(c("buy_stock_down=", buy_stock_down))
  # print(c("initial_stock=", initial_stock))
  # print(c("initial_cash=", initial_cash))

  s0 <- initial_stock
  b0 <- initial_cash
  V0 <- s0 + b0
  # print(c("s0=", s0, "b0=", b0, "V0=",V0))
  # t = 1, stocks go up, sell the appropriate amount of stock, and put the proceeds into cash
  s1u <- s0 * (1 + r_stock_up) * (1 - sell_stock_up)
  b1u <- b0 * (1 + mu_cash) + s0 * (1 + r_stock_up) * sell_stock_up
  V1u <- s1u + b1u
  p1u <- p_stock_up
  # print(c("s1u=", s1u, "b1u=", b1u, "V1u=", V1u, "p1u=", p1u))

  # t = 1, stocks go down, use the appropriate amount of cash to buy stock
  s1d <- s0 * (1 + r_stock_down) + b0 * (1 + mu_cash) * buy_stock_down
  b1d <- b0 * (1 + mu_cash) * (1 - buy_stock_down)
  V1d <- s1d + b1d
  p1d <- 1 - p_stock_up
  # print(c("s1d=", s1d, "b1d=", b1d, "V1d=", V1d, "p1d=", p1d))

  # t = 2, stocks go up / up
  s2uu <- s1u * (1 + r_stock_up)
  b2uu <- b1u * (1 + mu_cash)
  V2uu <- s2uu + b2uu
  p2uu <- p_stock_up * p_stock_up
  # print(c("s2uu=", s2uu, "b2uu=", b2uu, "V2uu=", V2uu, "p2uu=", p2uu))

  # t = 2, stocks go up / down
  s2ud <- s1u * (1 + r_stock_down)
  b2ud <- b1u * (1 + mu_cash)
  V2ud <- s2ud + b2ud
  p2ud <- p_stock_up * (1 - p_stock_up)
  # print(c("s2ud=", s2ud, "b2ud=", b2ud, "V2ud=", V2ud, "p2ud=", p2ud))

  # t = 2, stocks go down / up
  s2du <- s1d * (1 + r_stock_up)
  b2du <- b1d * (1 + mu_cash)
  V2du <- s2du + b2du
  p2du <- (1 - p_stock_up) * p_stock_up
  # print(c("s2du=", s2du, "b2du=", b2du, "V2du=", V2du, "p2du=", p2du))

  # t = 2, stocks go down / down
  s2dd <- s1d * (1 + r_stock_down)
  b2dd <- b1d * (1 + mu_cash)
  V2dd <- s2dd + b2dd
  p2dd <- (1 - p_stock_up) * (1 - p_stock_up)
  # print(c("s2dd=", s2dd, "b2dd=", b2dd, "V2dd=", V2dd, "p2dd=", p2dd))

  r2_xs <- c(V2uu, V2ud, V2du, V2dd) / V0 - (1 + mu_cash)
  p2 <- c(p2uu, p2ud, p2du, p2dd)

  E_xs_r <- sum(r2_xs * p2)
  E_xs_r_sq <- sum((r2_xs^2) * p2)
  sigma <- sqrt(E_xs_r_sq - (E_xs_r^2))

  sr <- E_xs_r / sigma
  sr
}

# Parameters for one-period (i.e. 6 month) returns
r_stock_up <- 0.1
r_stock_down <- -0.1
p_stock_up <- 0.8
mu_cash <- 0.0
initial_stock <- 0.5
initial_cash <- 0.5

# Sweep sell_stock_up and buy_stock_down from 0 to 1 and compute the 2 period SR for each pair of values
n_pts <- 10
sr_mat <- matrix(0, nrow = (n_pts + 1), ncol = (n_pts + 2))
my_col_names <- rep("$f_{s} \\downarrow / f_{c} \\rightarrow$", times = n_pts + 2)

for (i in 0:n_pts) {
  for (j in 0:n_pts) {
    buy_stock_down <- i / n_pts
    sell_stock_up <- j / n_pts

    my_col_names[i + 2] <- toString(round(buy_stock_down, digits = 3))

    sr_mat[i + 1, 1] <- buy_stock_down
    sr_mat[i + 1, j + 2] <- two_period_sr(
      r_stock_up, r_stock_down, p_stock_up,
      mu_cash,
      sell_stock_up, buy_stock_down,
      initial_stock, initial_cash
    )
  }
}

sr_dataframe <- round(as.data.frame(sr_mat), 3)
colnames(sr_dataframe) <- my_col_names
print(sr_dataframe)

## Figure 10.5
writeLines("\n\nFigure 10.5")

data(edhec)
hfnames <- c(
  "CA", "CTA", "DIS", "EM", "EMN", "ED", "FIA",
  "GM", "LSE", "MA", "RV", "SS", "FOF"
)
names(edhec) <- hfnames

edhecS <- edhec["2005-01-01/2018-12-31"]

hfRet <- edhecS[, 13] # The FOF response time series
hfFactors <- edhecS[, 1:12] # The 12 hedge funds predictor variables

regDat <- cbind(hfFactors, hfRet)
robFit <- lmrobdetMM(FOF ~ ., data = regDat) # To exclude the intercept, use lmrobdetMM(FOF ~.-1, data = regDat)
wtsRobfit <- robFit$rweights
dateVec <- as.Date(names(wtsRobfit))
wtsRobfit <- xts(wtsRobfit, order.by = dateVec)
all.ts <- cbind(hfFactors, hfRet, wtsRobfit)
tsPlotMP(all.ts, layout = c(2, 7), stripText.cex = 0.6, axis.cex = 0.6)


## Figures 10.6 and 10.7
writeLines("\n\nFigures 10.6 and 10.7")

styleAnalysis <- function(port_ret, style_indices_ret) {
  port_ret_mat <- as.matrix(port_ret)
  indx_ret_mat <- as.matrix(style_indices_ret)

  D <- t(indx_ret_mat) %*% indx_ret_mat
  d <- t(port_ret_mat) %*% indx_ret_mat
  p <- length(d)

  A <- cbind(rep(1, p), diag(p))
  b <- c(1, rep(0, p))

  soln <- quadprog::solve.QP(D, d, A, b, meq = 1)
  betas <- round(soln$solution, 2)

  return(betas)
}

data(edhec)
hfnames <- c(
  "CA", "CTA", "DIS", "EM", "EMN", "ED", "FIA",
  "GM", "LSE", "MA", "RV", "SS", "FOF"
)
names(edhec) <- hfnames

edhecS <- edhec["2005-01-01/2018-12-31"]

hfRet <- edhecS[, 13] # The FOF response time series
hfFactors <- edhecS[, 1:12] # The 12 hedge funds predictor variables

regDat <- cbind(hfFactors, hfRet)

## lsFit
lsFit <- lm(FOF ~ ., data = regDat) # To exclude the intercept, use lm(FOF ~.-1, data = regDat)
styleWtsLSunconstrained <- lsFit$coefficients
styleWtsLSunconstrained <- styleWtsLSunconstrained / sum(styleWtsLSunconstrained) # Normalize weights to sum to 1

## robFit
robFit <- lmrobdetMM(FOF ~ ., data = regDat) # To exclude the intecept, use lmrobdetMM(FOF ~.-1, data = regDat)
styleWtsRobunconstrained <- robFit$coefficients
styleWtsRobunconstrained <- styleWtsRobunconstrained / sum(styleWtsRobunconstrained) # Normalize weights to sum to 1

styleWtsunconstrainedLS_Rob <- rbind(styleWtsLSunconstrained, styleWtsRobunconstrained)

# LS constrained style weights
styleWtsLSconstrained <- styleAnalysis(hfRet, hfFactors)
names(styleWtsLSconstrained) <- names(hfFactors)

# Robust constrained style weights
robFit <- lmrobdetMM(FOF ~ ., data = regDat) # to keep intercept, use lmrobdetMM(FOF ~.-1, data = regDat)

wtsRobfit <- robFit$rweights
V <- diag(sqrt(wtsRobfit)) # W =V'V
y1 <- V %*% hfRet
X1 <- V %*% hfFactors
styleWtsRobconstrained <- styleAnalysis(y1, X1) # Use the WLS analysis
names(styleWtsRobconstrained) <- names(hfFactors)

styleWtsconstrainedLS_Rob <- rbind(styleWtsLSconstrained, styleWtsRobconstrained)

barplot(styleWtsunconstrainedLS_Rob,
  col = c("grey", "darkcyan"),
  border = "white",
  font.axis = 2,
  beside = T,
  xlab = "Hedge Fund Index Styles",
  font.lab = 2
)

legend("topleft", c("LS Unconstrained Style Weights", "Robust Unconstrained Style Weights"),
  fill = c("grey", "darkcyan"), bty = "n"
)

barplot(styleWtsconstrainedLS_Rob,
  col = c("grey", "darkcyan"),
  border = "white",
  font.axis = 2,
  beside = T,
  xlab = "Hedge Fund Index Styles",
  font.lab = 2
)

legend("topleft", c("LS Constrained Style Weights", "Robust Constrained Style Weights"),
  fill = c("grey", "darkcyan"), bty = "n"
)


## Figure 10.8 and Table 10.6
writeLines("\n\nFigure 10.8 and Table 10.6")
# # Read in CBOE data and convert mm/dd/yyyy dates to zoo's yearmon
# # Fill in BXY's 24 missing months as it starts later than the other series
CBOE_data <- PCRA::CboeOptionStrategies
CBOE_mths <- as.yearmon(CBOE_data$Date)
CBOE_data[1:24, "BXY"] <- CBOE_data[25, "BXY"]

# Compute cumulative retutns for the S&P 500 and the risk free rate
SPTR_cum <- CboeOptionStrategies[["SPTR"]] / CboeOptionStrategies[1, "SPTR"]
GS3M_cum <- c(1, head(cumprod(1 + CboeOptionStrategies[["GS3M"]] / 1200), -1))
SPTR_vs_GS3M_cum <- SPTR_cum / GS3M_cum
SPTR_vs_GS3M_log_mthly <- diff(log(SPTR_vs_GS3M_cum), 1)
N_months <- length(SPTR_vs_GS3M_log_mthly)

# Discard unused columns, keep only relevant strategies
CBOE_data <- CBOE_data[, !(colnames(CBOE_data) %in%
  c("Date", "VIX", "VXO", "SPX", "SPTR", "GS3M"))]
strategies <- colnames(CBOE_data)

# Compute cumulative return relatives (total return, vs. SPTR and vs. GS3M)
CBOE_data <- sweep(CBOE_data, 2, unlist(CBOE_data[1, ]), "/")
CBOEvsSPTR <- sweep(CBOE_data, 1, SPTR_cum, "/")
CBOEvsGS3M <- sweep(CBOE_data, 1, GS3M_cum, "/")

# Take logs to get cumulative log excess / active returns
CBOEvsSPTR_log <- log(CBOEvsSPTR)
CBOEvsGS3M_log <- log(CBOEvsGS3M)

# Take first differences to get monthly log excess / active returns
# Add the S&P 500 at the end - its active return is 0
CBOEvsSPTR_log_mthly <- as.data.frame(sapply(CBOEvsSPTR_log, diff, 1))
CBOEvsSPTR_log_mthly <- cbind(CBOEvsSPTR_log_mthly, rep(0, N_months))
colnames(CBOEvsSPTR_log_mthly)[length(colnames(CBOEvsSPTR_log_mthly))] <- "S&P 500"

CBOEvsGS3M_log_mthly <- as.data.frame(sapply(CBOEvsGS3M_log, diff, 1))
CBOEvsGS3M_log_mthly <- cbind(CBOEvsGS3M_log_mthly, SPTR_vs_GS3M_log_mthly)
colnames(CBOEvsSPTR_log_mthly)[length(colnames(CBOEvsSPTR_log_mthly))] <- "S&P 500"

# Add back the date column and a column of 0's for the active returns of the S&P 500
CBOEvsSPTR_log <- cbind(CBOE_mths, CBOEvsSPTR_log)
colnames(CBOEvsSPTR_log)[1] <- "Date"

CBOEvsGS3M_log <- cbind(CBOE_mths, CBOEvsGS3M_log)
colnames(CBOEvsGS3M_log)[1] <- "Date"

# Now make a ggplot of the cumulative active return
# Start by defining the line colors and line types
line_types <- rep(c("solid", "longdash", "dotted"), 4)
# line_colors  <- c("antiquewhite3", "burlywood3", "goldenrod3",
#                   "darkslategrey", "coral2",     "green4",
#                   "firebrick3",    "lightblue4", "mediumblue",
#                   "black" )
line_colors <- c(
  rep("darkslategrey", 3), rep("green4", 3),
  rep("firebrick3", 3), rep("black", 3)
)

# First create a graph for active returns

# Melt the data into a long format
df_tmp <- reshape2::melt(CBOEvsSPTR_log,
  id.vars = c("Date"),
  variable.name = "Strategy"
)

# Get ready to write the png file, then create the plot
# if(file.exists(png_fn_1)){file.remove(png_fn_1)}
#
# png(filename = png_fn_1, width = 800, height = 800, units = "px",
#     pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
#     type = c("windows", "cairo", "cairo-png"), antialias = "d")

plt11_5a <- ggplot(
  data = df_tmp,
  aes(
    x = zoo::as.Date(Date), y = value,
    color = Strategy, linetype = Strategy
  )
) +
  geom_line(linewidth = 0.75) +
  labs(x = "Date", y = "Cumulative Log Active Return vs. S&P 500 (SPTR)") +
  scale_color_manual(name = "Strategy", values = line_colors) +
  scale_linetype_manual(name = "Strategy", values = line_types) +
  scale_x_date(limits = c(
    zoo::as.Date(df_tmp$Date[1]),
    zoo::as.Date(df_tmp$Date[nrow(df_tmp)])
  )) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.23, 0.33),
    legend.text = element_text(size = 13, face = "bold")
  )

# Need to show the plot to make it visible in RStudio
show(plt11_5a)

# # Close the png driver
# dev.off()

# Next create a graph for excess returns relative to T-Bills

# Melt the data into a long format
df_tmp <- reshape2::melt(CBOEvsGS3M_log,
  id.vars = c("Date"),
  variable.name = "Strategy"
)

# Get ready to write the png file, then create the plot
# if(file.exists(png_fn_2)){file.remove(png_fn_2)}

#
# png(filename = png_fn_2, width = 800, height = 800, units = "px",
#     pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
#     type = c("windows", "cairo", "cairo-png"), antialias = "d")

plt11_5b <- ggplot(
  data = df_tmp,
  aes(
    x = zoo::as.Date(Date), y = value,
    color = Strategy, linetype = Strategy
  )
) +
  geom_line(linewidth = 0.75) +
  labs(x = "Date", y = "Cumulative Log Excess Return vs. 3 month T-Bills (GS3M)") +
  scale_color_manual(name = "Strategy", values = line_colors) +
  scale_linetype_manual(name = "Strategy", values = line_types) +
  scale_x_date(limits = c(
    zoo::as.Date(df_tmp$Date[1]),
    zoo::as.Date(df_tmp$Date[nrow(df_tmp)])
  )) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.23, 0.73),
    legend.text = element_text(size = 13, face = "bold")
  )

# Need to show the plot to make it visible in RStudio
show(plt11_5b)

# # Close the png driver
# dev.off()

# Get two dataframes with time series of excess log returns and active log returns
# and a dataframe with summary statistics
# Add
N <- length(colnames(CBOEvsSPTR_log_mthly))
df_stats <- data.frame(
  Strategy = colnames(CBOEvsSPTR_log_mthly),
  Sigma_xs = rep(0, N),
  Sigma_act = rep(0, N),
  SD_xs = rep(0, N),
  SD_act = rep(0, N),
  Skew_xs = rep(0, N),
  Skew_act = rep(0, N),
  Kurt_xs = rep(0, N),
  Kurt_act = rep(0, N),
  LSR = rep(0, N),
  DLSR = rep(0, N),
  LIR = rep(0, N),
  DLIR = rep(0, N)
)

# Compute all the summary statistics and populate df_stats
strat_mean_excess <- apply(CBOEvsGS3M_log_mthly, 2, mean)
strat_mean_active <- apply(CBOEvsSPTR_log_mthly, 2, mean)

strat_stdev_excess <- apply(CBOEvsGS3M_log_mthly, 2, sd)
strat_stdev_active <- apply(CBOEvsSPTR_log_mthly, 2, sd)

strat_semi_deviation_excess <- apply(CBOEvsGS3M_log_mthly, 2, downside_risk, 0)
strat_semi_deviation_active <- apply(CBOEvsSPTR_log_mthly, 2, downside_risk, 0)

strat_skew_excess <- apply(CBOEvsGS3M_log_mthly, 2, moments::skewness)
strat_skew_active <- apply(CBOEvsSPTR_log_mthly, 2, moments::skewness)

strat_kurt_excess <- apply(CBOEvsGS3M_log_mthly, 2, moments::kurtosis)
strat_kurt_active <- apply(CBOEvsSPTR_log_mthly, 2, moments::kurtosis)

df_stats["Sigma_xs"] <- round(100 * strat_stdev_excess, 2)
df_stats["Sigma_act"] <- round(100 * strat_stdev_active, 2)

df_stats["SD_xs"] <- round(100 * strat_semi_deviation_excess, 2)
df_stats["SD_act"] <- round(100 * strat_semi_deviation_active, 2)

df_stats["Skew_xs"] <- round(strat_skew_excess, 2)
df_stats["Skew_act"] <- round(strat_skew_active, 2)

df_stats["Kurt_xs"] <- round(strat_kurt_excess, 2)
df_stats["Kurt_act"] <- round(strat_kurt_active, 2)

df_stats["LSR"] <- round(strat_mean_excess / strat_stdev_excess, 2)
df_stats["DLSR"] <- round(strat_mean_excess / strat_semi_deviation_excess, 2)

df_stats["LIR"] <- round(strat_mean_active / strat_stdev_active, 2)
df_stats["DLIR"] <- round(strat_mean_active / strat_semi_deviation_active, 2)


# Get rid of the nans that arise when dividing by 0. We have to create
# an is.nan.data.frame functions as is.nan only works on vectors.
df_stats[is.nan.data.frame(df_stats)] <- ""

print(df_stats)

# # Finally clean up all the column names
# colnames(df_stats) <-c("Strategy",
#                        "$\\sigma_{xs} \\left(\\%\\right)$",      "$\\sigma_{act} \\left(\\%\\right)$",
#                        "$\\mathrm{SD}_{xs} \\left(\\%\\right)$", "$\\mathrm{SD}_{act} \\left(\\%\\right)$",
#                        "$\\mathrm{Skew}_{xs}$", "$\\mathrm{Skew}_{act}$",
#                        "$\\mathrm{Kurt}_{xs}$", "$\\mathrm{Kurt}_{act}$",
#                        "LSR", "DLSR", "LIR", "DLIR")


## Example 10.6 and Table 10.7.
writeLines("\n\nExample 10.6 and Table 10.7")

rb <- 0.1
df <- data.frame(
  Date = as.Date(c("2024/12/31", "2025/12/31", "2026/12/31")),
  Contrib = c(100, 0, 0),
  Distrib = c(0, 50, 0),
  Resid = c(0, 0, 75),
  discountFactor = c(1, 1 / (1 + rb), 1 / (1 + rb)^2)
)
df[["PV_Contrib"]] <- df[["Contrib"]] * df[["discountFactor"]]
df[["PV_Distrib"]] <- df[["Distrib"]] * df[["discountFactor"]]
df[["PV_Resid"]] <- df[["Resid"]] * df[["discountFactor"]]
df[["NetCF"]] <- df[["Distrib"]] + df[["Resid"]] - df[["Contrib"]]
df[["PV_NetCF"]] <- df[["PV_Distrib"]] + df[["PV_Resid"]] - df[["PV_Contrib"]]

irr <- jrvFinance::irr(df[["NetCF"]])
da <- jrvFinance::irr(df[["PV_NetCF"]])

df[["discountFactor"]] <- round(df[["discountFactor"]], 3)
df[["PV_Contrib"]] <- round(df[["PV_Contrib"]], 3)
df[["PV_Distrib"]] <- round(df[["PV_Distrib"]], 3)
df[["PV_Resid"]] <- round(df[["PV_Resid"]], 3)
df[["PV_NetCF"]] <- round(df[["PV_NetCF"]], 3)

MOIC <- sum(df[["Distrib"]] + df[["Resid"]]) / sum(df[["Contrib"]])
DPI <- sum(df[["Distrib"]]) / sum(df[["Contrib"]])
RVPI <- sum(df[["Resid"]]) / sum(df[["Contrib"]])
TVPI <- sum(df[["Distrib"]] + df[["Resid"]]) / sum(df[["Contrib"]])
PME <- sum(df[["PV_Distrib"]] + df[["PV_Resid"]]) / sum(df[["PV_Contrib"]])

colnames(df) <- c(
  "Date", "Contribution", "Distribution", "Residual Value", "Discount Factor",
  "PV(Contribution)", "PV(Distribution)", "PV(Residual Value)",
  "Net Cash Flow", "PV(Net Cash Flow)"
)
df

writeLines(paste0("MOIC = ", MOIC))
writeLines(paste0("DPI  = ", DPI))
writeLines(paste0("RVPI = ", RVPI))
writeLines(paste0("TVPI = ", TVPI))
writeLines(paste0("PME  = ", PME))

## Figure 10.9 - 10.12
writeLines("\n\nFigures 10.9 - 10.12 were created using Excel.")


## Figures 10.13 and 10.14
writeLines("\n\nFigures 10.13 and 10.14")
# muEst returns a simple weighted average of the current return and the last period's return.
# r      = Current period return
# mu0    = Mean return from the last period
# sigma0 = Estimate of volatility from the last period. If unavailable, set sigma0 = 0
# win_level = Number of standard deviations at which we winsorize (default: win_level =4)
# lambda    = Exponential weighting constant (default: 0.9)
muEst <- function(r, mu0, sigma0, win_level = 4, lambda = 0.9) {
  # Winsorize if the current return is exceptionally large
  if (abs(r - mu0) > win_level * sigma0) {
    r <- mu0 + sign(r - mu0) * win_level * sigma0
  }
  return(lambda * mu0 + (1 - lambda) * r)
}


# sigmaEst computes a EW estimate of volatility
# r          = Current period return
# mu0        = Prior period mean return (often set to 0 in finance as market efficiency diminishes the mean)
# sigma0     = Prior period estimated volatility. If unavailable (e.g. in the first period), sigma0 = 0
# win_level  = Number of standard deviations at which we winsorize (default: 4)
# lambda_in  = EW constant when the data seems consistent with the current estimate of volatility (default: 0.9)
# lambda_out = EW constant when the data seems inconsistent with the current estimate of volatility (default: 0.8)

sigmaEst <- function(r, mu0, sigma0, win_level = 4, lambda_in = 0.9, lambda_out = 0.8) {
  # Shift lambda down if current return is exceptionally large or exceptionally small
  lambda <- ifelse((abs(r - mu0) > sigma0 / win_level) &&
    (abs(r - mu0) < sigma0 * win_level),
  lambda_in, lambda_out
  )
  return(sqrt(lambda * sigma0^2 + (1 - lambda) * (r - mu0)^2))
}


# Charting function, which takes a results object, extracts all the data and then plots it
chartCusum <- function(results_obj, digits = 3, select_option = NULL,
                       print_to_screen = TRUE, print_to_png = TRUE, print_to_pdf = FALSE,
                       png_fn = NULL, pdf_fn = NULL, ...) {
  options(digits = digits)
  select_option.vec <- select_option
  select_option <- select_option[1]

  repeat {
    switch(select_option,
      `1L` = {
        # Turn zoo objects into dataframes for ggplot
        obj1 <- fortify.zoo(100 * results$Log_Active_Returns)
        obj2 <- fortify.zoo(results$Information_Ratios)
        obj3 <- fortify.zoo(100 * sqrt(12) * results$Tracking_Error)
        obj4 <- fortify.zoo(100 * results$Excess_Volatility[, 3])

        #### MONTHLY ACTIVE LOG RETURN PLOT
        names(results$Annual_Moving_Average) <- "AnnualMovingAverage"
        log_ret_plot <- ggplot(data = obj1, aes(x = Index, y = get(ptfName))) +
          geom_bar(
            stat = "identity",
            aes(fill = get(ptfName) < 0),
            colour = "NA"
          ) +
          scale_fill_manual(
            guide = "none",
            breaks = c(TRUE, FALSE),
            values = c("#F32008", "#269626")
          ) +
          scale_x_continuous(breaks = pretty_breaks(n = 5)) +
          geom_line(
            data = results$Annual_Moving_Average * 100,
            aes(
              x = Index,
              y = AnnualMovingAverage,
              colour = "12 Month Moving Average"
            ),
            linewidth = 0.8
          ) +
          labs(
            title = "Monthly Active Log Return",
            y = "Active Log Return (%)", x = "Month"
          ) +
          scale_colour_manual("", values = c("12 Month Moving Average" = "#3364f6")) +
          theme(
            panel.border = element_rect(
              colour = "black",
              fill = NA
            ),
            legend.position = "inside",
            legend.position.inside = c(.5, .1),
            legend.direction = "horizontal",
            legend.background = element_rect(fill = "transparent"),
            panel.grid.major = element_line(
              linewidth = 0.5,
              linetype = "solid",
              colour = "grey"
            ),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18),
            legend.key = element_rect(fill = NA, color = NA)
          ) +
          guides(color = guide_legend(override.aes = list(fill = NA))) +
          geom_hline(yintercept = 0)


        #### LOGARITHMIC TRACKING ERROR PLOT
        tracking_err_plot <- ggplot(data = obj3, aes(x = Index, y = Active)) +
          geom_line(aes(), color = 4, linewidth = 0.7) +
          labs(
            title = "Annualized Tracking Error",
            y = "Standard Deviation of Active Log Returns (%)",
            x = "Month"
          ) +
          theme(
            panel.border = element_rect(
              colour = "black",
              fill = NA
            ),
            panel.background = element_blank(),
            panel.grid.major = element_line(
              linewidth = 0.5,
              linetype = "solid",
              colour = "grey"
            ),
            plot.title = element_text(hjust = 0.5, size = 18)
          ) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = pretty_breaks(n = 5))


        #### LOGARITHMIC INFORMATION RATIO TWO COLOR GRAPH
        log_ir_graph <- ggplot(data = obj2, aes(x = Index, y = get(ptfName))) +
          geom_bar(
            stat = "identity",
            aes(fill = get(ptfName) < 0),
            colour = "NA"
          ) +
          scale_fill_manual(
            guide = "none",
            breaks = c(TRUE, FALSE),
            values = c("#F32008", "#269626")
          ) +
          labs(
            title = "Estimated Annualized Log IR",
            y = "Logarithmic IR", x = "Month"
          ) +
          theme(
            panel.border = element_rect(
              colour = "black",
              fill = NA
            ),
            panel.background = element_blank(),
            panel.grid.major = element_line(
              linewidth = 0.5,
              linetype = "solid",
              colour = "grey"
            ),
            plot.title = element_text(hjust = 0.5, size = 18)
          ) +
          geom_hline(yintercept = 0) +
          scale_x_continuous(breaks = pretty_breaks(n = 5))


        #### EXCESS VOL GRAPH
        excess_vol <- ggplot(
          data = obj4,
          aes(x = Index, y = ExcessVol)
        ) +
          geom_line(aes(), color = 4, linewidth = 0.7) +
          labs(
            title = "Vol(Portfolio) - Vol(Benchmark)",
            y = "Difference in Annualized Std. Dev. of Log Returns (%)",
            x = "Month"
          ) +
          geom_hline(
            yintercept = 0,
            linetype = "dotted",
            colour = "black"
          ) +
          theme(
            panel.border = element_rect(
              colour = "black",
              fill = NA
            ),
            panel.background = element_blank(),
            panel.grid.major = element_line(
              linewidth = 0.5,
              linetype = "solid",
              colour = "grey"
            ),
            plot.title = element_text(hjust = 0.5, size = 18)
          ) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = pretty_breaks(n = 5))

        if (min(obj4$ExcessVol) < 0) {
          excess_vol <- excess_vol + geom_hline(yintercept = 0)
        }

        # Create a title for the page
        ptf_vs_bmk <- paste0(
          ptfName, " vs. ", bmkName, "\n",
          index(df_xts)[1], " to ", tail(index(df_xts), 1)
        )
        page_title <- text_grob(ptf_vs_bmk, size = 15, face = "bold")

        # View graphs
        if (print_to_screen) {
          grid.arrange(log_ret_plot, tracking_err_plot,
            log_ir_graph, excess_vol,
            layout_matrix = rbind(c(1, 2), c(3, 4)),
            top = page_title
          )
        }

        ## Save plots to pdf
        if (print_to_pdf) {
          pdf(pdf_fn, width = 8, height = 8)
          grid.arrange(log_ret_plot, tracking_err_plot,
            log_ir_graph, excess_vol,
            layout_matrix = rbind(c(1, 2), c(3, 4)),
            top = page_title
          )
          dev.off()
        }

        # Save plots to png
        if (print_to_png) {
          png(
            filename = png_fn, width = 8, height = 8, units = "in",
            pointsize = 12, bg = "white", res = 144,
            family = "", restoreConsole = TRUE,
            type = c("windows", "cairo", "cairo-png"), antialias = "d"
          )

          grid.arrange(log_ret_plot, tracking_err_plot,
            log_ir_graph, excess_vol,
            layout_matrix = rbind(c(1, 2), c(3, 4)),
            top = page_title
          )
          dev.off()
        }
      },
      `2L` = {
        names(results$Annualized_Cusum_Log_AR) <- "Cusum_Log_AR"
        names(results$Annualized_Cusum_Log_IR) <- "CusumIR"
        names(results$Lindleys_recursion) <- "LRTest"

        # Turn zoo objects into dataframes for ggplot
        # Negate Lindley's recursion so that it plots correctly
        obj1 <- fortify.zoo(results$Annualized_Cusum_Log_AR)
        obj2 <- fortify.zoo(results$Annualized_Cusum_Log_IR)
        obj3 <- fortify.zoo(results$Lindleys_recursion * -1)
        obj4 <- fortify.zoo(results$Means * 100)

        colors1 <- c(
          "firebrick4", "firebrick3", "firebrick2", 1,
          "green2", "green3", "green4"
        )


        # CUSUM LOG ACTIVE RETURN PLOT:
        cusum_active_log_ret <- ggplot(
          data = obj1,
          aes(x = Index, y = Cusum_Log_AR)
        ) +
          geom_line(aes(), colour = 4, linewidth = 0.7) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(
              y = `Ray-3`,
              colour = paste0(
                "Ann. Active Log Return = ",
                results$`Log_AR_Slopes`[1],
                "% / annum"
              )
            )
          ) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(
              y = `Ray-2`,
              colour = paste0(
                "Ann. Active Log Return = ",
                results$`Log_AR_Slopes`[2],
                "% / annum"
              )
            )
          ) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(
              y = `Ray-1`,
              colour = paste0(
                "Ann. Active Log Return = ",
                results$`Log_AR_Slopes`[3],
                "% / annum"
              )
            )
          ) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(
              y = `Ray0`,
              colour = paste0(
                "Ann. Active Log Return = ",
                results$`Log_AR_Slopes`[4],
                "% / annum"
              )
            )
          ) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(
              y = `Ray+1`,
              colour = paste0(
                "Ann. Active Log Return = ",
                results$`Log_AR_Slopes`[5],
                "% / annum"
              )
            )
          ) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(
              y = `Ray+2`,
              colour = paste0(
                "Ann. Active Log Return = ",
                results$`Log_AR_Slopes`[6],
                "% / annum"
              )
            )
          ) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(
              y = `Ray+3`,
              colour = paste0(
                "Ann. Active Log Return = ",
                results$`Log_AR_Slopes`[7],
                "% / annum"
              )
            )
          ) +
          scale_colour_manual(
            breaks = paste0(
              "Ann. Active Log Return = ",
              results$`Log_AR_Slopes`,
              "% / annum"
            ),
            values = colors1,
            name = "Slopes of guide lines "
          ) +
          theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(
              hjust = 0.5,
              size = 18
            ),
            panel.grid.major.x = element_line(
              linewidth = 0.5,
              linetype = "solid",
              colour = "grey"
            ),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.direction = "vertical",
            panel.border = element_rect(
              colour = "black",
              fill = NA
            ),
            legend.key = element_rect(
              colour = "transparent",
              fill = "transparent"
            ),
            legend.background = element_blank(),
            legend.position = "inside",
            legend.position.inside = c(0.65, 0.25),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.3, "cm"),
            legend.title = element_text(size = 12)
          ) +
          geom_line(
            data = results$Protractor_Log_AR,
            aes(y = `Ray0`, colour = paste0(
              "Ann. Active Log Return = ",
              results$`Log_AR_Slopes`[4],
              "% / annum"
            ))
          ) +
          labs(
            x = "Year",
            title = "Cumulative Active Log Return"
          ) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_continuous(
            breaks = pretty_breaks(n = 10),
            expand = c(0, 0)
          )


        # CUSUM LOGARITHMIC IR PLOT
        cusum_log_ir <- ggplot(
          data = obj2,
          aes(x = Index, y = CusumIR)
        ) +
          geom_line(aes(), colour = 4, linewidth = 0.7) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray-3`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[1]
              )
            )
          ) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray-2`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[2]
              )
            )
          ) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray-1`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[3]
              )
            )
          ) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray0`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[4]
              )
            )
          ) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray+1`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[5]
              )
            )
          ) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray+2`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[6]
              )
            )
          ) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray+3`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[7]
              )
            )
          ) +
          scale_colour_manual(
            breaks = paste0(
              "Annualized Log IR = ",
              results$IR_Slopes
            ),
            values = colors1,
            name = "Slopes of guide lines "
          ) +
          theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 18),
            panel.grid.major.x = element_line(
              linewidth = 0.5,
              linetype = "solid",
              colour = "grey"
            ),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.3, "cm"),
            legend.direction = "vertical",
            legend.key = element_rect(
              colour = "transparent",
              fill = "transparent"
            ),
            legend.background = element_blank(),
            legend.position = "inside",
            legend.position.inside = c(0.75, 0.25),
            panel.border = element_rect(
              colour = "black",
              fill = NA
            )
          ) +
          geom_line(
            data = results$Protractor_IR,
            aes(
              y = `Ray0`,
              colour = paste0(
                "Annualized Log IR = ",
                results$`IR_Slopes`[4]
              )
            )
          ) +
          labs(
            x = "Year",
            title = "Cumulative Sum: Logarithmic IR"
          ) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_continuous(
            breaks = pretty_breaks(n = 10),
            expand = c(0, 0)
          )


        ###### Lindley's Recursion for the Likelihood Ratio Test
        cusum_levels <- c(-4.25, -5.62, -6.66)
        label_levels <- cusum_levels + 0.30
        lr_test <- ggplot(
          data = obj3,
          aes(x = Index, y = LRTest)
        ) +
          geom_line(aes(), color = 4, linewidth = 0.7) +
          labs(
            title = "Likelihood Ratio Test",
            x = "Year", y = "Scaled Likelihood Ratio"
          ) +
          theme(
            panel.border = element_rect(colour = "black", fill = NA),
            panel.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
          ) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_x_continuous(
            expand = c(0, 0),
            breaks = pretty_breaks(n = 10)
          ) +
          geom_hline(yintercept = 0) +
          geom_hline(
            yintercept = cusum_levels[1],
            colour = "green4", linewidth = 1
          ) +
          annotate("text",
            x = index(df_xts)[1], y = label_levels[1],
            hjust = 0, label = "   36 mths | 22 mths"
          ) +
          geom_hline(yintercept = cusum_levels[2], colour = "gold", linewidth = 1) +
          annotate("text",
            x = index(df_xts)[1], y = label_levels[2],
            hjust = 0, label = "   60 mths | 32 mths"
          ) +
          geom_hline(yintercept = cusum_levels[3], colour = "red", linewidth = 1) +
          annotate("text",
            x = index(df_xts)[1], y = label_levels[3],
            hjust = 0, label = "   84 mths | 41 mths"
          ) +
          theme(plot.title = element_text(hjust = 0.5, size = 18)) +
          annotate("text",
            x = index(df_xts)[1], y = -2, hjust = 0,
            label = "  Expected Time to First Crossing\n   IR = 0.5 | IR = 0 ",
            size = 4
          )


        ###### Portfolio Vs Benchmark Scatter plot and Regression Line
        portRet <- obj4$Portfolio
        benchRet <- obj4$Benchmark

        robust_fit <- lmrobdetMM(portRet ~ benchRet)
        robust_intercept <- robust_fit$coefficients[1]
        robust_slope <- robust_fit$coefficients[2]

        annualized_alpha <- round(12 * robust_intercept, 2)
        robust_beta <- round(robust_slope, 2)

        ptf_vs_bmk_scatter_plot <- ggplot(
          data = obj4,
          aes(
            x = Benchmark,
            y = Portfolio
          )
        ) +
          geom_point(aes(), colour = 4, shape = 4) +
          geom_hline(
            yintercept = 0,
            linetype = "dotted",
            colour = "black"
          ) +
          geom_vline(
            xintercept = 0,
            linetype = "dotted",
            colour = "black"
          ) +
          labs(
            x = "Benchmark Log Return (%)",
            y = "Portfolio Log Return (%)",
            title = "Portfolio vs. Benchmark"
          ) +
          theme(
            panel.border = element_rect(
              colour = "black",
              fill = NA
            ),
            legend.position = "none",
            plot.title = element_text(
              hjust = 0.5,
              size = 18
            ),
            panel.background = element_blank()
          ) +
          annotate("text",
            x = min(benchRet),
            y = max(portRet),
            label = c(
              paste0(
                "  Robust Alpha: ",
                annualized_alpha, "% / ann.\n"
              ),
              paste0(
                "\n  Robust Beta:  ",
                robust_beta
              )
            ),
            hjust = 0, vjust = 1
          ) +
          geom_abline(aes(
            slope = robust_slope,
            intercept = annualized_alpha / 12,
            color = "red4"
          ), linewidth = 1)


        # Create a title for the page
        ptf_vs_bmk <- paste0(
          ptfName, " vs. ", bmkName, "\n",
          index(df_xts)[1], " to ", tail(index(df_xts), 1)
        )
        page_title <- text_grob(ptf_vs_bmk, size = 15, face = "bold")

        # View graphs
        if (print_to_screen) {
          grid.arrange(cusum_active_log_ret, cusum_log_ir,
            lr_test, ptf_vs_bmk_scatter_plot,
            layout_matrix = rbind(c(1, 2), c(3, 4)),
            top = page_title
          )
        }


        # Save plots to pdf
        if (print_to_pdf) {
          pdf(pdf_fn, width = 8, height = 8)
          grid.arrange(cusum_active_log_ret, cusum_log_ir,
            lr_test, ptf_vs_bmk_scatter_plot,
            layout_matrix = rbind(c(1, 2), c(3, 4)),
            top = page_title
          )
          dev.off()
        }

        # Save plots to png
        if (print_to_png) {
          png(
            filename = png_fn, width = 8, height = 8, units = "in",
            pointsize = 12, bg = "white", res = 144,
            family = "", restoreConsole = TRUE,
            type = c("windows", "cairo", "cairo-png"), antialias = "d"
          )

          grid.arrange(cusum_active_log_ret, cusum_log_ir,
            lr_test, ptf_vs_bmk_scatter_plot,
            layout_matrix = rbind(c(1, 2), c(3, 4)),
            top = page_title
          )
          dev.off()
        }
      },
      invisible()
    )


    if (select_option == 0 || length(select_option.vec) == 1) {
      break
    }
    if (length(select_option.vec) > 1) {
      select_option.vec <- select_option.vec[-1]
      select_option <- select_option.vec[1]
      par(ask = TRUE)
    } else {
      select_option <- NULL
    }
  }
  par(ask = FALSE)
}


# Compute the CUSUM recursion and all the elements needed for the CUSUM plots
cusumActiveMgr <- function(portfolioName, benchmarkName, ret_df,
                           upper_IR = 0.5, lower_IR = 0,
                           lambda_in = 0.9, lambda_out = 0.8,
                           winsorize = 4, filterStd = FALSE) {
  # Record the call as an element to be returned
  this.call <- match.call()

  # Check to ensure that all arguments are valid
  if (missing(ret_df) || !is.xts(ret_df)) {
    stop("Invalid args: ret_df must be an xts object")
  }

  if (missing(portfolioName) || !is.character(portfolioName)) {
    stop("Invalid args: portfolioName must be a character string")
  }

  if (missing(benchmarkName) || !is.character(benchmarkName)) {
    stop("Invalid args: benchmarkName must be a character string")
  }

  if (winsorize < 1) {
    stop("Invalid args: the threshold for winsorization (winsorize) should be > 1")
  }

  if (lambda_in < 0 || lambda_in > 1 || lambda_out < 0 || lambda_out > 1) {
    stop("Invalid args: both lambdas must lie between 0 and 1")
  }

  if (!is.logical(filterStd)) {
    stop("Invalid args: filterStd must be a logical value")
  }

  # Obtain the returns of the porttolio and the benchmark
  portfolioReturns <- ret_df[, portfolioName]
  benchmarkReturns <- ret_df[, benchmarkName]
  n <- length(portfolioReturns)

  if (n < 2) {
    stop("Invalid args: portfolio returns and benchmark returns must have length >= 2")
  }

  if (n != length(benchmarkReturns)) {
    stop("Invalid args: portfolio returns and benchmark returns must have the same length")
  }

  # Initialize logarithmic active returns, IR, Lindley's Recursion and TE
  prior_month <- as.yearmon(first(index(portfolioReturns))) - 1 / 12
  all_Months <- c(prior_month, index(portfolioReturns))
  Lindley <- xts(rep(0, n + 1), order.by = all_Months)

  # Compute the Logarithmic Active Returns
  logActiveReturns <- log((1 + portfolioReturns) / (1 + benchmarkReturns))

  # Compute the current mean return, as well as the
  # unfiltered and filtered Std. Dev.
  # of the portfolio, its benchmark and the active return
  Means <- matrix(0, ncol = 3, nrow = n + 1)
  uStds <- Means
  fStds <- Means

  Means[1, 1] <- ifelse(n >= 11, mean(portfolioReturns[1:11]), mean(portfolioReturns))
  Means[1, 2] <- ifelse(n >= 11, mean(benchmarkReturns[1:11]), mean(benchmarkReturns))
  Means[1, 3] <- ifelse(n >= 11, mean(logActiveReturns[1:11]), mean(logActiveReturns))

  uStds[1, 1] <- ifelse(n >= 6, 1.25 * median(abs(portfolioReturns[1:6])),
    1.25 * median(abs(portfolioReturns))
  )
  uStds[1, 2] <- ifelse(n >= 6, 1.25 * median(abs(benchmarkReturns[1:6])),
    1.25 * median(abs(benchmarkReturns))
  )
  uStds[1, 3] <- ifelse(n >= 6, 1.25 * median(abs(logActiveReturns[1:6])),
    1.25 * median(abs(logActiveReturns))
  )

  fStds[1, 1] <- uStds[1, 1]
  fStds[1, 2] <- uStds[1, 2]
  fStds[1, 3] <- uStds[1, 3]

  # Update the means and unfiltered standard deviations for the portfolio and benchmark
  for (i in 1:n) {
    Means[i + 1, 1] <- muEst(
      coredata(portfolioReturns[i]),
      Means[i, 1], uStds[i, 1],
      winsorize, lambda_in
    )
    Means[i + 1, 2] <- muEst(
      coredata(benchmarkReturns[i]),
      Means[i, 2], uStds[i, 2],
      winsorize, lambda_in
    )
    Means[i + 1, 3] <- muEst(
      coredata(logActiveReturns[i]),
      Means[i, 3], uStds[i, 3],
      winsorize, lambda_in
    )

    uStds[i + 1, 1] <- sigmaEst(
      coredata(portfolioReturns[i]),
      Means[i + 1, 1], uStds[i, 1],
      winsorize, lambda_in, lambda_out
    )
    uStds[i + 1, 2] <- sigmaEst(
      coredata(benchmarkReturns[i]),
      Means[i + 1, 2], uStds[i, 2],
      winsorize, lambda_in, lambda_out
    )
    uStds[i + 1, 3] <- sigmaEst(
      coredata(logActiveReturns[i]),
      Means[i + 1, 3], uStds[i, 3],
      winsorize, lambda_in, lambda_out
    )
  }

  Stds <- uStds

  if (filterStd) {
    # Filter the standard deviations - allow it to rise immediately,
    # but average the past value and the current estimate when falling
    for (i in 1:n) {
      fStds[i + 1, 1] <- ifelse(uStds[i + 1, 1] > uStds[i, 1],
        uStds[i + 1, 1], 0.5 * (uStds[i, 1] + uStds[i + 1, 1])
      )
      fStds[i + 1, 2] <- ifelse(uStds[i + 1, 2] > uStds[i, 2],
        uStds[i + 1, 2], 0.5 * (uStds[i, 2] + uStds[i + 1, 2])
      )
      fStds[i + 1, 3] <- ifelse(uStds[i + 1, 3] > uStds[i, 3],
        uStds[i + 1, 3], 0.5 * (uStds[i, 3] + uStds[i + 1, 3])
      )
    }
    Stds <- fStds
  }

  Means <- xts(Means, order.by = all_Months)
  Stds <- xts(Stds, order.by = all_Months)
  colnames(Means) <- colnames(Stds) <- c("Portfolio", "Benchmark", "Active")

  # Excess volatility: difference between vol of portfolio and benchmark
  xsVol <- matrix(0, ncol = 3, nrow = n + 1)
  xsVol[1, 1] <- sqrt(12) * sd(coredata(portfolioReturns))
  xsVol[1, 2] <- sqrt(12) * sd(coredata(benchmarkReturns))
  xsVol[2:(n + 1), ] <- Stds[2:(n + 1), ] * sqrt(12)
  xsVol[, 3] <- xsVol[, 1] - xsVol[, 2]
  xsVol <- xts(xsVol, order.by = all_Months)
  colnames(xsVol) <- c("PortfolioVol", "BenchmarkVol", "ExcessVol")

  # Average level of the upper and lower IR inputs, used for Lindley's recursion
  avg_IR_Level <- 0.5 * (upper_IR + lower_IR) / sqrt(12)

  ##### Begin looping through the new returns#####
  IR <- coredata(logActiveReturns) / coredata(Stds[-(n + 1), 3])
  IR <- xts(IR, order.by = index(portfolioReturns))

  for (i in 1:length(portfolioReturns)) {
    # Lindley's recursion applied to the sequence of estimated log IRs
    # If it exceeds the decision threhsold, reset it to 0
    Lindley[i + 1] <- ifelse(coredata(Lindley[i]) - coredata(IR[i]) + avg_IR_Level < 0, 0,
      ifelse(coredata(Lindley[i]) > 6.66, max(0, avg_IR_Level - IR[i]),
        coredata(Lindley[i]) - coredata(IR[i]) + avg_IR_Level
      )
    )
  }

  # 12 month moving average returns
  AMA <- xts(rep(0, n), order.by = index(portfolioReturns))
  for (i in 1:n) {
    AMA[i] <- ifelse(i < 12, mean(logActiveReturns[1:i]), mean(logActiveReturns[(i - 11):i]))
  }

  # CUSUM IR
  cusum_IR <- xts(cumsum(coredata(IR)), order.by = index(IR))

  # Information obtained from Annualized Log IR
  annualized_IR <- sqrt(12) * coredata(cusum_IR)
  lowerLim_IR <- min(annualized_IR)
  upperLim_IR <- max(annualized_IR)

  spread_IR <- upperLim_IR - lowerLim_IR
  avg_IR <- spread_IR / n

  upperPos_IR <- which.max(annualized_IR)
  lowerPos_IR <- which.min(annualized_IR)

  med_IR <- lowerLim_IR + 0.5 * spread_IR
  peak_IR <- spread_IR / (upperPos_IR - lowerPos_IR)
  max_IR <- 0.5 * ceiling(abs(peak_IR) + abs(avg_IR))

  protractor_width_IR <- ceiling(0.9 * spread_IR / (2 * max_IR))
  protractor_height_IR <- abs(protractor_width_IR * max_IR)

  # There are 7 rays in the IR protractor
  Rays_IR <- matrix(0, ncol = 7, nrow = n + 1)
  Rays_IR[1, 1] <- med_IR + protractor_height_IR
  for (j in 2:7) {
    Rays_IR[1, j] <- Rays_IR[1, 1] - (j - 1) * protractor_height_IR / 3
  }

  for (i in 2:(n + 1)) {
    for (j in 1:4) {
      Rays_IR[i, j] <- max(
        Rays_IR[i - 1, j] - ((Rays_IR[1, j] - med_IR) / protractor_width_IR),
        med_IR
      )
    }
    for (j in 5:7) {
      Rays_IR[i, j] <- min(
        Rays_IR[i - 1, j] - ((Rays_IR[1, j] - med_IR) / protractor_width_IR),
        med_IR
      )
    }
  }

  IR_slopes <- c()
  for (i in -3:3) {
    if (i != 0) {
      IR_slopes <- append(IR_slopes, (i / 3) * max_IR)
    } else {
      IR_slopes <- append(IR_slopes, 0)
    }
  }

  Rays_IR <- xts(Rays_IR, order.by = all_Months)
  colnames(Rays_IR) <- c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")

  # CUSUM of Log Active Returns
  cusum_Log_AR <- xts(100 * cumsum(coredata(logActiveReturns)),
    order.by = index(logActiveReturns)
  )

  # Information obtained from annualized active returns
  annualized_Log_AR <- 12 * coredata(cusum_Log_AR)
  annualized_Log_AR <- 12 * coredata(cusum_Log_AR)
  lowerLim_Log_AR <- min(annualized_Log_AR)
  upperLim_Log_AR <- max(annualized_Log_AR)

  spread_Log_AR <- upperLim_Log_AR - lowerLim_Log_AR
  avg_Log_AR <- spread_Log_AR / n

  upperPos_Log_AR <- which.max(annualized_Log_AR)
  lowerPos_Log_AR <- which.min(annualized_Log_AR)

  med_Log_AR <- lowerLim_Log_AR + 0.5 * spread_Log_AR
  peak_Log_AR <- spread_Log_AR / (upperPos_Log_AR - lowerPos_Log_AR)
  max_Log_AR <- 0.5 * ceiling(abs(peak_Log_AR) + abs(avg_Log_AR))
  protractor_width_Log_AR <- ceiling(0.9 * spread_Log_AR / (2 * max_Log_AR))
  protractor_height_Log_AR <- abs(protractor_width_Log_AR * max_Log_AR)

  # There are 7 rays in the Active Return protractor
  Rays_Log_AR <- matrix(0, ncol = 7, nrow = n + 1)
  Rays_Log_AR[1, 1] <- med_Log_AR + protractor_height_Log_AR
  for (j in 2:7) {
    Rays_Log_AR[1, j] <- Rays_Log_AR[1, 1] - (j - 1) * protractor_height_Log_AR / 3
  }

  for (i in 2:(n + 1)) {
    for (j in 1:4) {
      Rays_Log_AR[i, j] <- max(
        Rays_Log_AR[i - 1, j] - ((Rays_Log_AR[1, j] - med_Log_AR) / protractor_width_Log_AR),
        med_Log_AR
      )
    }
    for (j in 5:7) {
      Rays_Log_AR[i, j] <- min(
        Rays_Log_AR[i - 1, j] - ((Rays_Log_AR[1, j] - med_Log_AR) / protractor_width_Log_AR),
        med_Log_AR
      )
    }
  }

  Rays_Log_AR <- xts(Rays_Log_AR, order.by = all_Months)
  colnames(Rays_Log_AR) <- c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")
  Log_AR_Slopes <- c()
  for (i in -3:3) {
    if (i != 0) {
      Log_AR_Slopes <- append(Log_AR_Slopes, (i / 3) * max_Log_AR)
    } else {
      Log_AR_Slopes <- append(Log_AR_Slopes, 0)
    }
  }
  # Convert all the remaining matrices, vectors etc. to xts objects
  annualized_IR <- xts(annualized_IR, order.by = index(portfolioReturns))
  annualized_Log_AR <- xts(annualized_Log_AR, order.by = index(portfolioReturns))

  # Return the updated likelihood ratios exceeding the threshold
  return(list(
    "Log_Active_Returns" = logActiveReturns,
    "Annual_Moving_Average" = AMA,
    "Tracking_Error" = Stds[, 3],
    "Information_Ratios" = IR,
    "Lindleys_recursion" = Lindley,
    "Annualized_Cusum_Log_IR" = annualized_IR,
    "Annualized_Cusum_Log_AR" = annualized_Log_AR,
    "Means" = Means,
    "Protractor_IR" = Rays_IR,
    "Protractor_Log_AR" = Rays_Log_AR,
    "Standard_Deviations" = Stds,
    "Excess_Volatility" = xsVol,
    "Log_AR_Slopes" = round(Log_AR_Slopes, 2),
    "IR_Slopes" = round(IR_slopes, 2)
  ))
}


# Simulation for ARL's, too slow for really good confidence intervals, do it in Python or C instead
simulateARL <- function(mu, Threshold, delta, k = 3, lambda = 0.9, Fixed_Sigma = 1) {
  N_Events <- 0
  Sum <- 0
  SumSq <- 0
  ThreeSigmaOverMu <- delta + 1
  while (ThreeSigmaOverMu > delta) {
    L <- 0
    N <- 0
    r_n_1 <- 0
    sigma_n <- 1
    sigma_n_1 <- 1
    while (L < Threshold) {
      r_n <- rnorm(n = 1, mean = mu, sd = 1)
      if ((N > 1) && (Fixed_Sigma == 1)) {
        sigma_n <- sqrt(lambda * sigma_n_1^2 + (1 - lambda) * 0.5 * (r_n - r_n_1)^2)
      } else {
        sigma_n <- 1
      }
      IR_hat <- r_n / sigma_n_1
      sigma_n_1 <- sigma_n
      r_n_1 <- r_n
      L <- max(0, L - IR_hat + mean(mu))
      N <- N + 1
    }
    N_Events <- N_Events + 1
    Sum <- Sum + N
    SumSq <- SumSq + N^2
    ARL <- Sum / N_Events
    Sigma <- ifelse(N_Events < 10, 1e5, sqrt((SumSq - Sum^2 / N_Events) / (N_Events * (N_Events - 1))))
    ThreeSigmaOverMu <- k * Sigma / ARL
  }
  s <- Sigma * sqrt(N_Events)
  return(c(ARL, s))
}


############################ < MAIN SCRIPT >###############################
START_DATE_YF <- as.Date("1989-12-01")
END_DATE <- as.Date("2024-12-31")

# # Get returns of Berkshire Hathaway and S&P 500 from Yahoo Finance
# # Filter daily prices to get the last price for the month as Yahoo's
# # end of month data is unreliable and also to protect againt
# # non-synchronous trading. We don't control the sequence in which items
# # are downloaded, so resequence the columns to Date, Manager, Benchmark
# df <- yf_get(tickers = c("BRK-A", "^SP500TR"),
#              first_date = START_DATE_YF,
#              last_date  = END_DATE,
#              freq_data  = "daily") |>
#   as.data.frame() |>
#   within(delta <- ref_date - lubridate::floor_date(ref_date, "month")) |>
#   within(ref_date <- zoo::as.yearmon(ref_date)) |>
#   subset(delta == ave(delta, ref_date, FUN = max)) |>
#   subset(select = c("ref_date", "ticker", "price_adjusted")) |>
#   stats::reshape(idvar = "ref_date",
#                  timevar = "ticker",
#                  direction = "wide")
#
# colnames(df)[(grep("date",  colnames(df)))] <- "Date"
# colnames(df)[(grep("SP500", colnames(df)))] <- "SP500"
# colnames(df)[(grep("BRK",  colnames(df)))] <- "BRK"
#
# df <- df |>
#         within(BRK   <- BRK / dplyr::lag(BRK) - 1) |>
#         within(SP500 <- SP500 / dplyr::lag(SP500) - 1) |>
#         na.omit()
#
# # Resequence the columns so that the portfolio comes before the benchmark
# # Then renumber rows sequentially starting at 1 and save the data frame
#
# df <- df[, c("Date", "BRK", "SP500")]
# rownames(df) <- 1:nrow(df)
# save(df, file = "./data/BRKSP500.Rda")
#
#
firstMonth <- as.yearmon("Jan 1970")

rda_fn <- "./data/BRKSP500.Rda"

# Read in the data and represent the dates as yearmons
# The data has 3 columns: Date, Portfolio Return and Benchmark Return
df <- get(load(rda_fn))
df$Date <- as.yearmon(df$Date)

# Turn the dataframe into an xts object and drop the column of dates
df_xts <- as.xts(df[, -1], order.by = df$Date)
df_xts <- df_xts[df$Date >= firstMonth]

ptfName <- colnames(df)[2]
bmkName <- colnames(df)[3]

png_fn1 <- paste0("./Plots/", ptfName, " vs. ", bmkName, " page 1.png")
png_fn2 <- paste0("./Plots/", ptfName, " vs. ", bmkName, " page 2.png")
pdf_fn1 <- paste0("./Plots/", ptfName, " vs. ", bmkName, " page 1.pdf")
pdf_fn2 <- paste0("./Plots/", ptfName, " vs. ", bmkName, " page 2.pdf")

results <- cusumActiveMgr(
  portfolioName = ptfName,
  benchmarkName = bmkName,
  ret_df = df_xts,
  upper_IR = 0.5, lower_IR = 0.0,
  lambda_in = 0.9, lambda_out = 0.8,
  winsorize = 4, filterStd = FALSE
)

chartCusum(results,
  select_option = 1,
  print_to_screen = TRUE,
  print_to_png = FALSE, png_fn = png_fn1,
  print_to_pdf = FALSE, pdf_fn = pdf_fn1
)

chartCusum(results,
  select_option = 2,
  print_to_screen = TRUE,
  print_to_png = FALSE, png_fn = png_fn2,
  print_to_pdf = FALSE, pdf_fn = pdf_fn2
)


## Tables 10.10, 10.11 and 10.12
writeLines("\n\nTables 10.9, 10.10 and 10.11")
# Read in data for Jan 2010 from the pa package
data("jan")

# Next compute the Brinson-Hood-Beebower attribution for a single month
attrib_jan <- brinson(
  x = jan, date.var = "date", cat.var = "sector",
  bench.weight = "benchmark", portfolio.weight = "portfolio",
  ret.var = "return"
)

# Create better row and column headers / labels
my_sectors <- c(
  "Energy", "Materials",
  "Industrials", "Consumer Discretionary",
  "Consumer Staples", "Health Care",
  "Financials", "Information Technology",
  "Telecom Services", "Utilities"
)
my_exposure_col_names <- c("Portfolio (%)", "Benchmark (%)", "Difference (%)")
my_attribution_col_names <- c("Allocation (%)", "Selection (%)", "Interaction (%)")
total_attribution_row_names <- c(
  "Allocation (%)", "Selection (%)", "Interaction (%)",
  "Total Active Return (%)"
)

# Compute the exposures and convert them to percentages (they are expressed as decimals)
exposureDf <- as.data.frame(exposure(attrib_jan, var = "sector"))
exposureDf <- round(exposureDf * 100, 1)
rownames(exposureDf) <- my_sectors
colnames(exposureDf) <- my_exposure_col_names

# Convert all the contributions to percentages (they are expressed as basis points)
return1Df <- as.data.frame(returns(attrib_jan)[1])
return1Df <- round(return1Df / 100, 2)
rownames(return1Df) <- c(my_sectors, "Total")
colnames(return1Df) <- my_attribution_col_names

# Convert all the totals to percentages (they are expressed as decimals)
return2Df <- as.data.frame(returns(attrib_jan)[2])
return2Df <- round(return2Df * 100, 2)
colnames(return2Df) <- "Total"
rownames(return2Df) <- total_attribution_row_names

print(exposureDf)
writeLines("\n")
print(return1Df)
writeLines("\n")
print(return2Df)


## Figure 10.15
writeLines("\n\nFigure 10.15 was created using a Python simulation that runs for over a day")
