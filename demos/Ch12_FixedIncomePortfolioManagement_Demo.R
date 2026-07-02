#Remove all objects
rm(list = ls(all = TRUE))                       #Clear all objects in the workspace
if(!is.null(dev.list()["RStudioGD"])) dev.off() #Clear all graphs

# psych package has pairs.panels, tidyverse includes dplyr for manipulating data, 
# readxl for reading from Excel
library(tidyverse)
library(Quandl)
library(readxl)
library(lubridate)
library(zoo)
library(psych)
library(DescTools)
library(reshape2)
library(PCRA)
library(fredr)
library(robust)
library(xtable)
library(ggpubr)
library(kableExtra)
usepackage_latex("threeparttablex")
library(rstudioapi)

# Set the working directory automatically using the rstudioapi library
# This allows us to source the file without worrying about paths
current_path = getActiveDocumentContext()$path 
setwd(dirname(current_path ))

####### CREATE TABLE 14.1 ###########
df_bond    <- data.frame(matrix(0, nrow = 7, ncol = 6))
df_bond$X1 <- c(0,  0.5,  1.0,  1.5,  2.0,  2.5,  3.0)
df_bond$X2 <- c("", 2.50, 2.50, 2.50, 2.50, 2.50, 2.50)
df_bond$X3 <- c("",   "",   "",   "",   "",   "", 100.00)
df_bond$X4 <- c("", 2.50, 2.50, 2.50, 2.50, 2.50, 102.50)
df_bond$X5 <- c("", 0.96618, 0.93351, 0.90194, 0.87144, 0.84197, 0.81350) 
df_bond$X6 <- c("", 2.4155,  2.3338,  2.2549,  2.1786,  2.1049, 83.3838)

#Rename rows and columns and reformat the table
colnames(df_bond) <-c( "$\\tau \\mathrm{(Years)}$", "Coupon payment", "Principal payment",
				   	"$c_{t + \\tau}$", 
					   "$\\frac {1}{\\left(1 + \\frac{y}{2} \\right)^{2 \\tau}}$",
					   "$\\frac {c_{t+\\tau}}{\\left(1 + \\frac{y}{2} \\right)^{2 \\tau}}$")

kable(df_bond, format = "latex", booktabs = TRUE, linesep = "", row.names = FALSE,
	  align = c("c", "c", "c", "r", "r", "r"), escape = FALSE) %>%
	  kable_styling("striped", full_width = F, font_size = 10, table.env = "center") %>%
	  column_spec( 1, width = "4em") %>%
      column_spec( 2, width = "4em") %>% 
      column_spec( 3, width = "4em") %>%
      column_spec( 4, width = "4em") %>% 
      column_spec( 5, width = "4em") %>% 
      column_spec( 6, width = "4em") 

print(df_bond)


######## CREATE TABLE 14.2 ###########
df_bond    <- data.frame(matrix(0, nrow = 4, ncol = 6))

df_bond$X1 <- c(0,     1,   2,   3)
df_bond$X2 <- c("",  105,  "",  "")
df_bond$X3 <- c("",    6, 106,  "")
df_bond$X4 <- c("",    7,   7, 107)
df_bond$X5 <- c("", 0.952381, 0.889488, 0.814083)
df_bond$X6 <- c("", "4.87902\\%", "5.85547\\%", "6.85642\\%")

#Rename rows and columns and reformat the table
colnames(df_bond) <-c( "$\\tau \\mathrm{(Years)}$", "$c^{1}_{t + \\tau}$",
				   	"$c^{2}_{t + \\tau}$", "$c^{3}_{t + \\tau}$", 
					   "$d_{t + \\tau}$", "$r_{\\tau}$")

kable(df_bond, format = "latex", booktabs = TRUE, linesep = "", row.names = FALSE,
	  align = c("c", "c", "c", "c", "c", "c"), escape = FALSE) %>%
	  kable_styling("striped", full_width = F, font_size = 10, table.env = "center") %>%
	  column_spec( 1, width = "4em") %>%
      column_spec( 2, width = "4em") %>% 
      column_spec( 3, width = "4em") %>%
      column_spec( 4, width = "4em") %>% 
      column_spec( 5, width = "4em") %>% 
      column_spec( 6, width = "4em") 
     
print(df_bond)


######## CREATE FIGURE 14.3 ###########
apiKey = "xW52N8dGxs_7hFUTCYS7"
Quandl.api_key(apiKey)

# Output file names
NSS_PAR_YIELD_FN  <- "Plots/NSS_Par_Yield_Curve.png"
NSS_ZC_YIELD_FN   <- "Plots/NSS_ZC_Yield_Curve.png"
NSS_PARAMS_FN     <- "Plots/NSS_params.png"

#Start and end dates for the data
today      <- Sys.Date()
START_DATE <- as.Date("1961-06-30")
END_DATE   <- as.Date("2023-12-31")

#################### Functions ###########################################
# To get the GSW NSS yield at any tenor on any date, pass the date (dt) and
# the tenor (t) along with the params array to CalcNSSYield

CalcNSSYield <- function(dt, t, params){
  NSSrow = which(params$Date == t)
  yDate <- params[NSSrow, 1]
  beta0 <- params[NSSrow, 2]
  beta1 <- params[NSSrow, 3]
  beta2 <- params[NSSrow, 4]
  beta3 <- params[NSSrow, 5]
  tau1  <- params[NSSrow, 6]
  tau2  <- params[NSSrow, 7]
  
  y_t <- beta0 + beta1 *   (1 - exp(-t/tau1))/(t/tau1)
  + beta2 * ( (1 - exp(-t/tau1))/(t/tau1) - exp(-t/tau1) )
  + beta3 * ( (1 - exp(-t/tau2))/(t/tau2) - exp(-t/tau2) )
  
  return(y_t)
}

################ Main Section ###########################################
# The Gurkaynak Wright Sack yield curves are an off-the-run Treasury 
# yield curve based on a large set of outstanding Treasury notes 
# and bonds, and are based on a continuous compounding convention. 
# See https://www.federalreserve.gov/pubs/feds/2006/200628/200628abs.html
# Values are daily estimates of the yield curve from 6/14/1961 for the 
# entire maturity range spanned by outstanding Treasury securities.

# Get the GSW US Treasury OTR Zero-Coupon Yield Curve from 1 yr to 30 yrs
# The first column has dates - convert it using as.Date
UST_ZCPN     <- Quandl("FED/SVENY")
UST_ZCPN[,1] <- as.Date(UST_ZCPN[,1])
UST_ZCPN     <- UST_ZCPN %>%
                  filter(between(Date, START_DATE, END_DATE))

d1 = format(START_DATE, "%m/%d/%Y")
d2 = format(END_DATE, "%m/%d/%Y")

cat("Summary statistics: Gurkaynak Sack Wright NSS Zero Coupon Rates from Quandl:",
    d1, " to ", d2, "\n", sep = " ")
print(describe(UST_ZCPN[c("SVENY01", "SVENY02", "SVENY05", "SVENY07",
                          "SVENY10", "SVENY20", "SVENY30")]))
writeLines("\n")

# if (file.exists(NSS_ZC_YIELD_FN))  file.remove(NSS_ZC_YIELD_FN)
# png(filename = NSS_ZC_YIELD_FN, width = 480, height = 480, units = "px", pointsize = 12,
#     bg = "white", res = NA, family = "", restoreConsole = TRUE,
#     type = c("windows", "cairo", "cairo-png"), antialias = "d")
#   
  pairs.panels(UST_ZCPN[c("SVENY01","SVENY02","SVENY05", "SVENY07",
                          "SVENY10", "SVENY20", "SVENY30")], lm = FALSE,
                        main = paste( "\n", "Gurkaynak Sack Wright NSS Zero Coupon Rates:",
                                      d1, " to ", d2, sep = " ") )
# dev.off()

# Get the GSW US Treasury OTR Par Yield Curve from 1 yr to 30 yrs
# The first column has dates - convert it using as.Date
# print("Downloading Gurkaynak Sack Wright NSS Par Yields from Quandl")
UST_ParYC     <- Quandl("FED/SVENPY")
UST_ParYC[,1] <- as.Date(UST_ParYC[,1])
UST_ParYC     <- UST_ParYC %>%
                    filter(between(Date, START_DATE, END_DATE))

# cat("Summary statistics: Gurkaynak Sack Wright NSS Par Yields from Quandl:", d1, " to ", d2, "\n", sep = " ")
# print(describe(UST_ParYC[c("SVENPY01", "SVENPY02", "SVENPY05", "SVENPY07", 
#                            "SVENPY10", "SVENPY20", "SVENPY30")]))
# writeLines("\n")

# if (file.exists(NSS_PAR_YIELD_FN))  file.remove(NSS_PAR_YIELD_FN)
# png(filename = NSS_PAR_YIELD_FN, width = 480, height = 480, units = "px", pointsize = 12,
#     bg = "white", res = NA, family = "", restoreConsole = TRUE,
#     type = c("windows", "cairo", "cairo-png"), antialias = "d")

  pairs.panels(UST_ParYC[c("SVENPY01", "SVENPY02", "SVENPY05", "SVENPY07", 
                           "SVENPY10", "SVENPY20", "SVENPY30")], lm = FALSE, 
                         main = paste("\n", "Gurkaynak Sack Wright NSS Par Yields:", 
                                      d1, " to ", d2, sep = " "))
# dev.off()

# Get the GSW US Treasury Nelson Siegel Svensson parameters
# The first column has dates - convert it using as.Date
params     <- Quandl("FED/PARAMS")
params[,1] <- as.Date(params[,1])
params     <- params %>%
                filter(between(Date, START_DATE, END_DATE))

cat("Summary statistics: Gurkaynak Sack Wright NSS Parameters:", d1, " to ", d2, "\n", sep = " ")
print(describe(params[c("BETA0", "BETA1", "BETA2", "BETA3", "TAU1", "TAU2")]))
writeLines("\n")

# if (file.exists(NSS_PARAMS_FN))  file.remove(NSS_PARAMS_FN)
# png(filename = NSS_PARAMS_FN, width = 480, height = 480, units = "px", pointsize = 12,
#     bg = "white", res = NA, family = "", restoreConsole = TRUE,
#     type = c("windows", "cairo", "cairo-png"), antialias = "d")

  pairs.panels(params[c("BETA0", "BETA1", "BETA2", "BETA3", "TAU1", "TAU2")],
               lm = FALSE, main = paste("\n", "Gurkaynak Sack Wright NSS Parameters:", 
                                        d1, " to ", d2, sep = " "))
# dev.off()



######## CREATE FIGURE 14.4 ###########

### Constants #################################################################
LIU_WU_DAILY_FN    <- "Liu Wu Daily Zero Coupon Yield Curve.xlsx"
LIU_WU_MONTHLY_FN  <- "Liu Wu Monthly Zero Coupon Yield Curve.xlsx"

DATA_FOLDER    <- "../Data/"
PLOTS_FOLDER   <- "../Plots/"
DAILY_PNG_FN   <- "Liu-Wu Yield Curve Examples - Daily data.png"
MONTHLY_PNG_FN <- "Liu-Wu Yield Curve Examples - Monthly data.png"

LAST_EMPTY_ROW     <- 8

DAILY_PLOT_DATES   <- as.Date(c("1961-06-16", "1971-07-27", "1981-01-29", 
                                "1990-01-26", "2018-06-18", "2020-12-28"))
MONTHLY_PLOT_DATES <- as.Date(c("1961-06-30", "1971-09-30", "1981-01-31",
                                "1990-01-31", "2018-06-30", "2020-12-31"))
X_AXIS_BREAKS      <- c("1m",   "24m",  "60m",  "84m", 
                        "120m", "180m", "240m", "300m", "360m")
Y_AXIS_BREAKS      <- c(0, 1, 2, 4, 6, 8, 10, 12, 14, 16)

### Functions ##############################################################################
read_survey_data <- function() {
  # Read in the daily and month-end Liu-Wu yield curve data, then clean it up
  daily_fn   <- paste0(DATA_FOLDER, LIU_WU_DAILY_FN)
  df_daily   <- read_excel(daily_fn)
  df_daily   <- clean_up_liu_wu_dataframe(df_daily, daily = TRUE)
  
  monthly_fn <- paste0(DATA_FOLDER, LIU_WU_MONTHLY_FN)
  df_monthly <- read_excel(monthly_fn)
  df_monthly <- clean_up_liu_wu_dataframe(df_monthly, daily = FALSE)
  
  return_list <- list(df_daily, df_monthly)
  
  return(return_list) 
}



clean_up_liu_wu_dataframe <- function(df, daily = TRUE) {
  # Turn the tibble into a dataframe and rename all the columns
  df  <- as.data.frame(df)
  tenors        <- as.character(paste0(1:360, "m"))
  new_col_names <- c("Date", tenors)
  colnames(df)  <- new_col_names
  
  # Drop the blank / irrelevant rows at the top
  df <- df[-1:-LAST_EMPTY_ROW, ]
  
  # Reformat the dates as yyyy-mm-dd. For monthlies, get the last day of the month
  # using the tricks in https://stat.ethz.ch/pipermail/r-help/2007-September/141796.html and
  # https://stackoverflow.com/questions/67148448/converting-yyyymm-to-the-last-weekday-or-business-day-of-the-month#67148646
  if (daily) {
    df[, 1] <- as.Date(df[[1]], "%Y%m%d")
    
  } else {
    df[, 1] <- as.Date(as.yearmon(df[[1]], "%Y%m"), frac = 1)
    # Make this the last weekday of the month if needed
    # df[, 1] <- df[[1]] - pmax(as.numeric(format(df[[1]], "%u")) - 5, 0)
    
  }
  
  # All yields are read in as character strings - convert them to doubles
  df[tenors] <- lapply(df[tenors], as.numeric)
  
  return(df)
}



plot_liu_wu_yield_curve <- function(df, daily = TRUE) {
  
  if (daily) {
    dt_list <- DAILY_PLOT_DATES
    png_fn  <- paste0(PLOTS_FOLDER, DAILY_PNG_FN) 

  } else {
    dt_list <- MONTHLY_PLOT_DATES
    png_fn  <- paste0(PLOTS_FOLDER, MONTHLY_PNG_FN) 
    
  }
  
  # Find the number of valid tenors in each row, then select the relevant dates
  # Suprisingly, df$Not_NA <- rowSums(!is.na(df)) is orders of magnitude faster
  # than the dplyr equivalent. See https://stackoverflow.com/questions/67150864/
  # problem-using-rowwise-to-count-the-number-of-nas-in-each-row-of-a-dataframe
  df$Not_NA <- rowSums(!is.na(df))
  max_cols <- df %>%
                filter(Date %in% dt_list) %>%
                select(Not_NA) %>%
                max(.)
  
  df_plot <- df[1:max_cols] %>%
                filter(Date %in% dt_list) %>%
                melt(id.vars = "Date")
  
  # Activate the png driver after deleting the old file, then write the plot
  # file.remove(png_fn)
  # png(filename = png_fn, width = 800, height = 800, units = "px",
  #     pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
  #     type = c("windows", "cairo", "cairo-png"), antialias = "d")    
  
    line_types  <- c("solid", "longdash", "dashed", "dotted", "longdash", "solid")
    line_colors <- c("orangered2", "green4", "darkslategrey", 
                     "dodgerblue4", "purple", "burlywood3")
  
    p <- df_plot %>%
          ggplot(aes(x = variable, y = value)) +
          geom_line(aes(color = as.factor(Date), linetype = as.factor(Date), 
                        group = Date), linewidth = 1) +
          labs(x = "Tenor", y = "Yield (%)", color = "Date", linetype = "Date") +
          scale_color_manual(values = line_colors) +
          scale_linetype_manual(values = line_types) +
          theme_light() +
          theme( plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
                 plot.title.position = "plot",
                 axis.title.x = element_text(size = 18),
                 axis.title.y = element_text(size = 18), 
                 legend.title = element_text(size = 18),
                 axis.text    = element_text(size = 16),
                 legend.text  = element_text(size = 16)) +
          scale_x_discrete(breaks = X_AXIS_BREAKS) +
          scale_y_continuous(breaks = Y_AXIS_BREAKS)

    # Need to print(p) to make it visible in RStudio
    print(p)
    
  # dev.off()

}

#############################Main code###################################
df_list        <- read_survey_data()

liu_wu_daily   <- df_list[[1]]
liu_wu_monthly <- df_list[[2]]

plot_liu_wu_yield_curve(liu_wu_daily,   daily = TRUE)
plot_liu_wu_yield_curve(liu_wu_monthly, daily = FALSE)



######## GET FRED DATA NEEDED TO CREATE TABLE 14.3 and FIGURE 14.5###########

# Key that allows us to download data from FRED.
fredr_set_key("caf71401e549e0570139e5188af2e40b")

FREQ   <- "d" # "d" for daily, "w" for weekly, "m" for monthly
MAX_NA <- 3   # Maximum number of NA's allowed in a row

# List of FRED tickers for nominal rates, real rates and T-bills
NOMINAL_TICKERS    <- c("DGS1","DGS2","DGS3","DGS5","DGS7","DGS10","DGS20","DGS30")
NOMINAL_MATURITIES <- c("1 Yr.","2 Yr.","3 Yr.","5 Yr.","7 Yr.","10 Yr.","20 Yr.","30 Yr.")
REAL_TICKERS       <- c("DFII5","DFII7","DFII10","DFII20","DFII30")
REAL_MATURITIES    <- c("5 Yr.","7 Yr.","10 Yr.","20 Yr.","30 Yr.")
TBILL_TICKERS      <- c("DGS1MO","DTB4WK","DTB3","DTB6","DTB1YR")
TBILL_MATURITIES   <- c("1 Mo.","4 Wk.","3 Mo.","6 Mo.","12 Mo.")

# Base file names
ALL_QQ_FN <- "Plots/IR_QQ_Normal.png"
PANELS_FN <- "Plots/IR_Moves.png"
TS_FN     <- "Plots/IR_History.png"

# Start and end dates for the data
START_DATE = as.Date("1980-12-31")
END_DATE   = as.Date("2023-12-31")


for (rate_type in c("Nominal", "Real", "TBill")) {
  
  # Tickers, maturities and rate types
  if (rate_type == "Nominal") {
    tickers    <- NOMINAL_TICKERS
    maturities <- NOMINAL_MATURITIES
    writeLines("Processing Nominal Rates")
    
  } else if (rate_type == "Real") {
    tickers    <- REAL_TICKERS
    maturities <- REAL_MATURITIES
    writeLines("Processing Real Rates")
    
  } else if (rate_type == "TBill") {
    tickers    <- TBILL_TICKERS
    maturities <- TBILL_MATURITIES
    writeLines("Processing T-Bill Yields")
    
  }
  
  all_QQ_fn_final <- paste0(strsplit(ALL_QQ_FN, ".png")[[1]][1], "_", rate_type, ".png")
  panels_fn_final <- paste0(strsplit(PANELS_FN, ".png")[[1]][1], "_", rate_type, ".png")
  ts_fn_final     <- paste0(strsplit(TS_FN,     ".png")[[1]][1], "_", rate_type, ".png")
  
  # Download data from the Federal Reserve Bank of St. Louis' FRED database in a tibble
  # Convert the values to a decimal i.e. 1 -> 0.01 = 1%
  rates <- map_dfr(tickers, fredr)
  rates <- rates[, c("date", "series_id", "value")]
  rates[, "value"] <- rates[, "value"] / 100
  
  # Build a data frame of daily rates
  IRdaily <- tidyr::pivot_wider(rates, id_cols = date, names_from = series_id)
  IRdaily <- as.data.frame(IRdaily)
  IRdaily$date <- as.Date(IRdaily$date)
  
  # Get the FRED names of all the columns other than the date in the data table
  # Rename tickers to the appropriate maturities.
  # Ensure that the names are in the right sequence, then rename them
  IRdaily  <- IRdaily[c("date", tickers)]
  indices  <- match(tickers, names(IRdaily))
  colnames(IRdaily)[indices] <-  maturities
  
  # Cut out the appropriate set of dates and remove rows in which most of the 
  # interest rates are missing. If we know where to expect missing items, 
  # we can create a subset of maturities and write
  # IRdaily <- IRdaily[complete.cases(IRdaily[, maturities_subset]), ]
  # and if we want all maturities to be present, we can write
  # IRdaily <- IRdaily[complete.cases(IRdaily[, maturities]), ]. 
  # In practice, some interest rates start later than others (because of the 
  # pattern of issuance by the government) so we have just a few NAs in a row
  # or every single interest rate is missing. So we do this a little differently
  IRdaily$count_NA <- rowSums(is.na(IRdaily))
  IRdaily <- IRdaily %>%
    filter(between(date, START_DATE, END_DATE)) %>% 
    filter(count_NA <= MAX_NA) %>%
    select(-count_NA)
  
  # Including slopes renders the covariance matrix non-invertible
  # Useful if we just want the time series without the covariance matrix
  #if (rate_type == "Nominal") {
  #  IRdaily[,TwosFives:=(DGS5-DGS2)] 
  #  IRdaily[,FivesTwentys:=(DGS20-DGS5)] 
  #  IRdaily[,TwosTens:=(DGS10-DGS2)]
  #  
  #} else if (rate_type == "Real") {
  #  IRdaily[,TensThirtys:=(DFII30-DFII10)] 
  #  IRdaily[,FivesTwentys:=(DFII20-DFII5)] 
  #  IRdaily[,FivesTens:=(DFII10-DFII5)] 
  #}
  
  # Now compute weekly and monthly snapshots of rates. End the week on Friday
  # We can make any day of the week the last day. The group_by() changes IRmonthly 
  # from a data frame to a tibble, which causes problems later on, so change it back.
  # This arises because Tibbles are strict about subsetting. 
  # If we try to access a variable that does not exist (YYYYmm), we get an error
  IRweekly  <- IRdaily[weekdays(IRdaily$date) == "Friday", ]
  IRmonthly <- IRdaily %>% 
    group_by(YYYYmm = strftime(date, "%Y-%m")) %>%
    filter(date == max(date)) %>%
    as.data.frame() %>%
    select(-YYYYmm) %>%
    ungroup()
  
  # Compute the daily, weekly and monthly differences 
  # and drop the first row which has NA's for first differences
  deltaIRdaily   <- IRdaily %>% 
    mutate(across(-date, ~c(NA, diff(.)))) %>%
    filter(date > min(date))
  deltaIRweekly  <- IRweekly %>% 
    mutate(across(-date, ~c(NA, diff(.)))) %>%
    filter(date > min(date))
  deltaIRmonthly <- IRmonthly %>% 
    mutate(across(-date, ~c(NA, diff(.)))) %>%
    filter(date > min(date))
  
  # Store data on nominal yields
  if (rate_type == "Nominal") {
    IR_nominal_daily       <- IRdaily
    delta_IR_nominal_daily <- deltaIRdaily
  }
  
  # Choose the appropriate dataset
  if (FREQ == "d"){
    IR       <- IRdaily
    deltaIR  <- deltaIRdaily
    freqLong <- "Daily"
    
  } else if (FREQ == "w") {  
    IR       <- IRweekly
    deltaIR  <- deltaIRweekly
    freqLong <- "Weekly"
    
  } else {
    IR       <- IRmonthly
    deltaIR  <- deltaIRmonthly
    freqLong <- "Monthly"
    
  }
  
  # Do a little EDA on deltaIR using QQ plots and pairs.panels
  # The code is counter-intuitive, see the answer to the question at
  # https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r
  deltaIR.df <- deltaIR[, maturities]
  myPlots    <- vector(mode = "list", length = length(maturities) )
  for(i in 1:length(maturities)) local({
    i = i;
    myPlots[[i]] <<- ggplot(deltaIR.df, aes(sample = deltaIR.df[, i])) +
      stat_qq(size = 0.5) +
      stat_qq_line() +
      labs(title=maturities[i],
           x = "Normal Quantiles",
           y = paste("Sample Quantiles: ", maturities[i], "Interest Rate", sep = " " )) +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 6)) +
      theme_minimal() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  })
  
  # Now plot the QQ plots in two rows and save the result to a png file
  allQQplots <- ggarrange(plotlist = myPlots, nrow = 2, ncol = ceiling(length(myPlots)/2))
  
  # if (file.exists(all_QQ_fn_final)) file.remove(all_QQ_fn_final)
  # png(all_QQ_fn_final)
  print(allQQplots)
  # dev.off()
  
  # Next get a summary using pairs.panels and save it. To get a png file, just open it
  # and run pairs.panels. No need to create and then print a pairs.panels object
  par(mfrow = c(1,1))
  
  # if (file.exists(panels_fn_final))  file.remove(panels_fn_final)
  # png(filename = panels_fn_final, width = 480, height = 480, units = "px", pointsize = 12,
  #     bg = "white", res = NA, family = "", restoreConsole = TRUE,
  #     type = c("windows", "cairo", "cairo-png"), antialias = "d")
  # 
  pairs.panels(deltaIR.df[, maturities], lm = FALSE,
               main = paste0(freqLong, " US Treasury Interest Rate Moves") )
  # dev.off()
  
  # Finally plot time series of interest rates at all tenors
  ir_melt <- melt(IR, id.vars = "date", na.rm = FALSE)
  
  p <- ggplot(ir_melt, aes(date, value, color = variable)) +
    geom_line() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    labs(x = "Date", y = "Interest Rate (%)", color = "Tenor")
  
  # if (file.exists(ts_fn_final)) file.remove(ts_fn_final)
  # png(filename = ts_fn_final, width = 480, height = 480, units = "px", pointsize = 12, 
  #     bg = "white", res = NA, family = "", restoreConsole = TRUE, 
  #     type = c("windows", "cairo", "cairo-png"), antialias = "d")
  #   
  print(p)
  # 
  # dev.off()
  
}

# Compute classical and robust correlation and covariance matrices. Use"pairwiseGK" 
# for the Gnanadesikan-Kettenring pairwise estimator from 
# library robust, because most estimators including covRob in robStatTM
# need an invertible covariance matrix, and the presence of slopes, which 
# are the differences of rates, makes the covariance matrix singular
# delta_IR_nominal_daily is a data table, so its columns must be accessed using ..

classical_vols <- sapply(delta_IR_nominal_daily[ , NOMINAL_MATURITIES], sd, na.rm = TRUE)

classical_cor <- cor(delta_IR_nominal_daily[ , NOMINAL_MATURITIES], use = "pairwise")
robust_cor    <- robust::covRob(delta_IR_nominal_daily[ , NOMINAL_MATURITIES], 
                                corr = TRUE, 
                                estim = "pairwiseGK", 
                                na.action = na.omit)$cov

classical_cov <- diag(classical_vols) %*% classical_cor %*% diag(classical_vols)
robust_cov    <- robust::covRob(delta_IR_nominal_daily[ , NOMINAL_MATURITIES], 
                                corr = FALSE, 
                                estim = "pairwiseGK", 
                                na.action = na.omit)$cov

classical_eigen_decomposition <- eigen(classical_cov, symmetric = TRUE, only.values = FALSE)
robust_eigen_decomposition    <- eigen(robust_cov,    symmetric = TRUE, only.values = FALSE)

classical_eigenvalues         <- classical_eigen_decomposition$values
robust_eigenvalues            <- robust_eigen_decomposition$values

#Multiply eigenvectors by -1 as the first eigenvector has all negative values
classical_eigenvectors        <- classical_eigen_decomposition$vectors * -1
robust_eigenvectors           <- robust_eigen_decomposition$vectors    * -1

classical_pct_of_risk         <- classical_eigenvalues / sum(classical_eigenvalues)
robust_pct_of_risk            <- robust_eigenvalues    / sum(robust_eigenvalues)

classical_cum_pct_of_risk     <- cumsum(classical_pct_of_risk)
robust_cum_pct_of_risk        <- cumsum(robust_pct_of_risk)

# The robust vol is really a robust scale estimator
robust_vols    <- sqrt(diag(robust_cov))
classicalAndRobustVols <- rbind(classical_vols, robust_vols)



####### CREATE TABLE 14.3 ###########
df_cov <- as.data.frame(classical_cov) %>%    
			 mutate_if(is.numeric, format, digits = 2, nsmall =2, scientific = TRUE)

colnames(df_cov) <- NOMINAL_MATURITIES
rownames(df_cov) <- NOMINAL_MATURITIES

kable(df_cov, format = "latex", booktabs = TRUE, linesep = "", row.names = TRUE, escape = FALSE) %>%
	  kable_styling("striped", full_width = F, font_size = 10, table.env = "center") %>%
	  column_spec( 1:9, width = "4em") 		
     
print(df_cov)


####### CREATE TABLE 14.4 ###########
df_eigenvalues <- data.frame(
							 X1 = c(1, 2, 3, 4),
							 X2 = classical_eigenvalues[1:4],
							 X3 = classical_pct_of_risk[1:4],
							 X4 = classical_cum_pct_of_risk[1:4]
							)     

df_eigenvalues <- df_eigenvalues %>% 
				    mutate(X2 = format(X2, digits = 2, nsmall = 2, scientific = TRUE)) %>%
				    mutate(X3 = round(X3, 3)) %>%
				    mutate(X4 = round(X4, 3))

colnames(df_eigenvalues) <- c("Eigenvalue", "Value", "Fraction of Risk", "Cumulative Fraction of Risk")

kable(df_eigenvalues, format = "latex", booktabs = TRUE, align = rep("c", 4), linesep = "", row.names = FALSE, escape = FALSE) %>%
	  kable_styling("striped", full_width = F, font_size = 10, table.env = "center") %>%
	  column_spec( 1:2, width = "5em") %>%
	  column_spec( 3:4, width = "7em")
     
print(df_eigenvalues)


####### CREATE TABLE 14.5 ###########
df_vol_cor    <- data.frame(X1  = names(classical_vols),
							X2  = paste0(round(10000*unname(classical_vols),2), " bps"), 
							X3  = rep(" ", length(classical_vols)),
							X4  = names(classical_vols),
							X5  = round(classical_cor[, 1], 2),
							X6  = round(classical_cor[, 2], 2),
							X7  = round(classical_cor[, 3], 2),
							X8  = round(classical_cor[, 4], 2),
							X9  = round(classical_cor[, 5], 2),
							X10 = round(classical_cor[, 6], 2),
							X11 = round(classical_cor[, 7], 2),
							X12 = round(classical_cor[, 8], 2)
						   )
colnames(df_vol_cor) <- c("Tenor", "Daily Std. Dev", " ", "Tenor", NOMINAL_MATURITIES)
kable(df_vol_cor, format = "latex", booktabs = TRUE, align = c(rep("c", 4), "r", rep("c",8)),
	  linesep = "", row.names = FALSE, escape = FALSE) %>%
	  kable_styling("striped", full_width = F, font_size = 9, table.env = "center") %>%
	  column_spec( 1,    width = "3em") %>%
	  column_spec( 2,    width = "6em") %>%
	  column_spec( 3,    width = "4em") %>%
	  column_spec( 4:12, width = "3em") %>%
	  add_header_above(c("Volatility" = 2, " " = 1, "Correlation" = 9))

print(df_vol_cor)


# ######## CREATE FIGURE 14.5 ###########
# #Key that allows us to download data from FRED. 
# fredr_set_key("caf71401e549e0570139e5188af2e40b")
# 
# FREQ   <- "d" # "d" for daily, "w" for weekly, "m" for monthly
# MAX_NA <- 3   # Maximum number of NA's allowed in a row
# 
# # List of FRED tickers for nominal rates, real rates and T-bills
# NOMINAL_TICKERS    <- c("DGS1","DGS2","DGS3","DGS5","DGS7","DGS10","DGS20","DGS30")
# NOMINAL_MATURITIES <- c("1 Yr.","2 Yr.","3 Yr.","5 Yr.","7 Yr.","10 Yr.","20 Yr.","30 Yr.")
# REAL_TICKERS       <- c("DFII5","DFII7","DFII10","DFII20","DFII30")
# REAL_MATURITIES    <- c("5 Yr.","7 Yr.","10 Yr.","20 Yr.","30 Yr.")
# TBILL_TICKERS      <- c("DGS1MO","DTB4WK","DTB3","DTB6","DTB1YR")
# TBILL_MATURITIES   <- c("1 Mo.","4 Wk.","3 Mo.","6 Mo.","12 Mo.")
# 
# # Base file names
# ALL_QQ_FN <- "Plots/IR_QQ_Normal.png"
# PANELS_FN <- "Plots/IR_Moves.png"
# TS_FN     <- "Plots/IR_History.png"
# 
# # Start and end dates for the data
# START_DATE = as.Date("1980-12-31")
# END_DATE   = as.Date("2020-12-31")
# 
# for (rate_type in c("Nominal", "Real", "TBill")) {
#   
#   # Tickers, maturities and rate types
#   if (rate_type == "Nominal") {
#     tickers    <- NOMINAL_TICKERS
#     maturities <- NOMINAL_MATURITIES
#     writeLines("Processing Nominal Rates")
# 
#   } else if (rate_type == "Real") {
#     tickers    <- REAL_TICKERS
#     maturities <- REAL_MATURITIES
#     writeLines("Processing Real Rates")
#     
#   } else if (rate_type == "TBill") {
#     tickers    <- TBILL_TICKERS
#     maturities <- TBILL_MATURITIES
#     writeLines("Processing T-Bill Yields")
#     
#   }
# 
#   all_QQ_fn_final <- paste0(strsplit(ALL_QQ_FN, ".png")[[1]][1], "_", rate_type, ".png")
#   panels_fn_final <- paste0(strsplit(PANELS_FN, ".png")[[1]][1], "_", rate_type, ".png")
#   ts_fn_final     <- paste0(strsplit(TS_FN,     ".png")[[1]][1], "_", rate_type, ".png")
#   
#   # Download data from the Federal Reserve Bank of St. Louis' FRED database in a tibble
#   # Convert the values to a decimal i.e. 1 -> 0.01 = 1%
#   rates <- map_dfr(tickers, fredr)
#   rates[, "value"] <- rates[, "value"] / 100
#   
#   # Build a data frame for daily rates as well as some slopes
#   # Earlier on, it seems that fredr generated a data table, but it
#   # now generates data frames, which is fine as ggplot2 requires a data frame
#   IRdaily <- dcast(rates, date~series_id)
#   IRdaily <- IRdaily[, c("date", tickers)]
#   IRdaily$date <- as.Date(IRdaily$date)
#   
#   # Get the FRED names of all the columns other than the date in the data table
#   # then rename them to the appropriate maturities
#   setnames(IRdaily, old = setdiff(names(IRdaily), "date"), new = maturities)
#   
#   # Cut out the appropriate set of dates and remove rows in which most of the 
#   # interest rates are missing. If we know where to expect missing items, 
#   # we can create a subset of maturities and write
#   # IRdaily <- IRdaily[complete.cases(IRdaily[, maturities_subset]), ]
#   # and if we want all maturities to be present, we can write
#   # IRdaily <- IRdaily[complete.cases(IRdaily[, maturities]), ]. 
#   # In practice, some interest rates start later than others (because of the 
#   # pattern of issuance by the government) so we have just a few NAs in a row
#   # or every single interest rate is missing. So we do this a little differently
#   IRdaily$count_NA <- rowSums(is.na(IRdaily))
#   IRdaily <- IRdaily %>%
#                 filter(between(date, START_DATE, END_DATE)) %>% 
#                 filter(count_NA <= MAX_NA) %>%
#                 select(-count_NA)
#   
#   # Including slopes renders the covariance matrix non-invertible
#   # Useful if we just want the time series without the covariance matrix
#   #if (rate_type == "Nominal") {
#   #  IRdaily[,TwosFives:=(DGS5-DGS2)] 
#   #  IRdaily[,FivesTwentys:=(DGS20-DGS5)] 
#   #  IRdaily[,TwosTens:=(DGS10-DGS2)]
#   #  
#   #} else if (rate_type == "Real") {
#   #  IRdaily[,TensThirtys:=(DFII30-DFII10)] 
#   #  IRdaily[,FivesTwentys:=(DFII20-DFII5)] 
#   #  IRdaily[,FivesTens:=(DFII10-DFII5)] 
#   #}
#   
#   # Now compute weekly and monthly snapshots of rates. End the week on Friday
#   # We can make any day of the week the last day. The group_by() changes IRmonthly 
#   # from a data frame to a tibble, which causes problems later on, so change it back.
#   # This arises because Tibbles are strict about subsetting. 
#   # If we try to access a variable that does not exist (YYYYmm), we get an error
#   IRweekly  <- IRdaily[weekdays(IRdaily$date) == "Friday", ]
#   IRmonthly <- IRdaily %>% 
#                 group_by(YYYYmm = strftime(date, "%Y-%m")) %>%
#                 filter(date == max(date)) %>%
#                 as.data.frame() %>%
#                 select(-YYYYmm) %>%
#                 ungroup()
#   
#   # Compute the daily, weekly and monthly differences 
#   # and drop the first row which has NA's for first differences
#   deltaIRdaily   <- IRdaily %>% 
#                       mutate(across(-date, ~c(NA, diff(.)))) %>%
#                       filter(date > min(date))
#   deltaIRweekly  <- IRweekly %>% 
#                       mutate(across(-date, ~c(NA, diff(.)))) %>%
#                       filter(date > min(date))
#   deltaIRmonthly <- IRmonthly %>% 
#                       mutate(across(-date, ~c(NA, diff(.)))) %>%
#                       filter(date > min(date))
#   
#   # Choose the appropriate dataset
#   if (FREQ == "d"){
#     IR       <- IRdaily
#     deltaIR  <- deltaIRdaily
#     freqLong <- "Daily"
#   
#   } else if (FREQ == "w") {  
#     IR       <- IRweekly
#     deltaIR  <- deltaIRweekly
#     freqLong <- "Weekly"
#   
#   } else {
#     IR       <- IRmonthly
#     deltaIR  <- deltaIRmonthly
#     freqLong <- "Monthly"
#     
#   }
#   
#   # Do a little EDA on deltaIR using QQ plots and pairs.panels
#   # The code is counter-intuitive, see the answer to the question at
#   # https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-in-r
#   deltaIR.df <- deltaIR[, maturities]
#   myPlots    <- vector(mode = "list", length = length(maturities) )
#   for(i in 1:length(maturities)) local({
#     i = i;
#     myPlots[[i]] <<- ggplot(deltaIR.df, aes(sample = deltaIR.df[, i])) +
#                      stat_qq(size = 0.5) +
#                      stat_qq_line() +
#                      labs(title=maturities[i],
#                      x = "Normal Quantiles",
#                      y = paste("Sample Quantiles: ", maturities[i], "Interest Rate", sep = " " )) +
#                      theme(plot.title = element_text(size = 10),
#                      axis.title = element_text(size = 6)) +
#                      theme_minimal() +
#                      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
#   })
#   
#   # Now plot the QQ plots in two rows and save the result to a png file
#   allQQplots <- ggarrange(plotlist = myPlots, nrow = 2, ncol = ceiling(length(myPlots)/2))
#   
#   # if (file.exists(all_QQ_fn_final)) file.remove(all_QQ_fn_final)
#   # png(all_QQ_fn_final)
#     print(allQQplots)
#   # dev.off()
#   
#   # Next get a summary using pairs.panels and save it. To get a png file, just open it
#   # and run pairs.panels. No need to create and then print a pairs.panels object
#   par(mfrow = c(1,1))
#   
#   # if (file.exists(panels_fn_final))  file.remove(panels_fn_final)
#   # png(filename = panels_fn_final, width = 480, height = 480, units = "px", pointsize = 12,
#   #     bg = "white", res = NA, family = "", restoreConsole = TRUE,
#   #     type = c("windows", "cairo", "cairo-png"), antialias = "d")
#   # 
#     pairs.panels(deltaIR.df[, maturities], lm = FALSE,
#                  main = paste0(freqLong, " US Treasury Interest Rate Moves") )
#   # dev.off()
#   
#   # Finally plot time series of interest rates at all tenors
#   ir_melt <- melt(IR, id.vars = "date", na.rm = FALSE)
#   
#   p <- ggplot(ir_melt, aes(date, value, color = variable)) +
#               geom_line() +
#               scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#               theme_minimal() +
#               labs(x = "Date", y = "Interest Rate (%)", color = "Tenor")
#     
#   # if (file.exists(ts_fn_final)) file.remove(ts_fn_final)
#   # png(filename = ts_fn_final, width = 480, height = 480, units = "px", pointsize = 12, 
#   #     bg = "white", res = NA, family = "", restoreConsole = TRUE, 
#   #     type = c("windows", "cairo", "cairo-png"), antialias = "d")
#   #   
#     print(p)
#   # 
#   # dev.off()
# }



######## CREATE TABLE 14.6 ###########
df_predict    <- data.frame(matrix(0, nrow = 6, ncol = 7))
df_predict$X1 <- c("0", "$\\nicefrac{1}{m}$", "$\\nicefrac{2}{m}$", "$\\vdots$",
                   "$T - \\nicefrac{1}{m}$", "$T$" )
df_predict$X2 <- c("$y^{ref}_{0}$", "$y^{ref}_{\\nicefrac{1}{m}}$", 
				   "$y^{ref}_{\\nicefrac{2}{m}}$", "$\\vdots$", 
				   "$y^{ref}_{T - \\nicefrac{1}{m}}$", "$y^{ref}_{T}$")
df_predict$X3 <- c("$s_{1}$", "$s_{1}$", "$s_{1}$", 
				   "$s_{1}$", "$s_{1}$", "$s_{1}$" )
df_predict$X4 <- c("$s_{2}$", "$s_{2}$", "$s_{2}$", 
				   "$s_{2}$", "$s_{2}$", "$s_{2}$" )
df_predict$X5 <- c("--", "$\\frac{y^{ref}_{0} + s_{1}}{m}$", 
				   "$\\frac{y^{ref}_{\\nicefrac{1}{m}} + s_{1}}{m}$", 
				   "$\\vdots$",
				   "$\\frac{y^{ref}_{T - \\nicefrac{1}{m}} + s_{1}}{m}$", 
				   "$\\frac{y^{ref}_{T} + s_{1}}{m}$" )
df_predict$X6 <- c("$\\frac{y^{ref}_{\\nicefrac{-1}{m}} + s_{2}}{m}$", 
				   "$\\frac{y^{ref}_{0} + s_{2}}{m}$", 
				   "$\\frac{y^{ref}_{\\nicefrac{1}{m}} + s_{2}}{m}$", 
				   "$\\vdots$",
				   "$\\frac{y^{ref}_{T - \\nicefrac{1}{m}} + s_{2}}{m}$", 
				   "$\\frac{y^{ref}_{T} + s_{2}}{m}$" )
df_predict$X7 <- c("--", rep("$\\frac{s_{1} - s_{2}}{m}$", 5))

#Rename rows and columns and reformat the table
colnames(df_predict) <-c("Time", "$y^{t}_{ref}$",  "$s_{1,t}$", "$s_{2,t}$", 
						 "$c_{1,t}$", "$c_{2,t}$", "$c_{1,t} - c_{2,t}$" )

kable(df_predict, format = "latex", booktabs = TRUE, linesep = "", row.names = FALSE,
	  align = c("l", "l", "l", "l", "l", "l", "c"), escape = FALSE) %>%
	  kable_styling("striped", full_width = F, font_size = 10, table.env = "center") %>%
	  column_spec( 1, width = "4em") %>%
      column_spec( 2, width = "4em") %>% 
      column_spec( 3, width = "4em") %>%
      column_spec( 4, width = "4em") %>% 
      column_spec( 5, width = "6em") %>% 
      column_spec( 6, width = "6em") %>% 
      column_spec( 7, width = "6em")  

print(df_predict)


######## CREATE TABLE 14.7 ###########
df_predict    <- data.frame(matrix(0, nrow = 6, ncol = 8))
df_predict$X1 <- c(0, 1, 2, 3, 4, 5)
df_predict$X2 <- c(300, 303, 310, 330, 305, 311)
df_predict$X3 <- c("", "$1.00\\%$", "$2.31\\%$", 
				   "$6.45\\%$", "$-8.79\\%$", "$3.32\\%$")
df_predict$X4 <- c("$\\$100.00$", "$\\$101.00$", "$\\$103.33$", 
				   "$\\$110.00$", "$\\$100.33$",  "$\\$103.67$")
df_predict$X5 <- c("", "$\\$5.05$", "$\\$5.17$", 
				   "$\\$5.50$", "$\\$5.02$", "$\\$5.18$")
df_predict$X6 <- c("", "", "", "", "", "$\\$103.67$")
df_predict$X7 <- c("", "$\\$5.05$", "$\\$5.17$", "$\\$5.50$", "$\\$5.02$", "$\\$108.85$")
df_predict$X8 <- c("", "$\\$5.00$", "$\\$5.00$", "$\\$5.00$", "$\\$5.00$", "$\\$105.00$")

#Rename rows and columns and reformat the table
colnames(df_predict) <-c("Year", "CPI",  "Inflation", "Indexed Principal Amount", 
						 "Nominal Coupon", "Nominal Principal Payment", 
						 "Total Nominal Cash Flow", "Total Real Cash Flow" )

kable(df_predict, format = "latex", booktabs = TRUE, linesep = "", row.names = FALSE,
	  align = c("c", "c", "r", "r", "r", "r", "r", "r"), escape = FALSE) %>%
	  kable_styling("striped", full_width = F, font_size = 10, table.env = "center") %>%
	  column_spec( 1, width = "4em") %>%
      column_spec( 2, width = "4em") %>% 
      column_spec( 3, width = "5em") %>%
      column_spec( 4, width = "5em") %>% 
      column_spec( 5, width = "5em") %>% 
      column_spec( 6, width = "6em") %>% 
      column_spec( 7, width = "6em") %>% 
      column_spec( 8, width = "5em")  


print(df_predict)

######## CREATE FIGURE 14.10 ###########
#Key that allows us to download data from FRED. 
fredr_set_key("caf71401e549e0570139e5188af2e40b")

#Switch
freq = "d" #"d" for daily, "w" for weekly, "m" for monthly 

# Output files
allQQ_fn  <- "Plots/beiQQNorm.png"
panels_fn <- "Plots/beiMoves.png"
ts_fn     <- "Plots/beiHistory.png"

#List of FRED tickers for breakeven inflation
bei_tickers <- c("T5YIE", "T10YIE")

#List of names for these tickers
bei_names <- c("5 Year Breakeven Inflation", "10 Year Breakeven Inflation")

# Download data from the Federal Reserve Bank of St. Louis' FRED database in a tibble
# Convert the values to a decimal i.e. 1 -> 0.01 = 1%
bei_FRED <- map_dfr(bei_tickers, fredr)
bei_FRED[, "value"] <- bei_FRED[, "value"] / 100

#Build a data table for breakevens 
bei_daily <- dcast(bei_FRED, date~series_id)
bei_daily <- bei_daily[, c("date", bei_tickers)]
bei_daily$date <- as.Date(bei_daily$date)

#Cut out the appropriate data segment
bei_daily$count_NA <- rowSums(is.na(bei_daily))
bei_daily <- bei_daily %>%
              filter(between(date, START_DATE, END_DATE)) %>% 
              filter(count_NA <= MAX_NA) %>%
              select(-count_NA)

# Rename tickers to the appropriate maturities.
# Ensure that the names are in the right sequence, then rename them
indices  <- match(bei_tickers, names(bei_daily))
colnames(bei_daily)[indices] <-  bei_names

# Now compute weekly and monthly snapshots of rates. End the week on Friday
# We can make any day of the week the last day. The group_by() changes IRmonthly 
# from a data frame to a tibble, which causes problems later on, so change it back.
# This arises because Tibbles are strict about subsetting. 
# If we try to access a variable that does not exist (YYYYmm), we get an error
bei_weekly  <- bei_daily[weekdays(bei_daily$date) == "Friday", ]
bei_monthly <- bei_daily %>% 
                group_by(YYYYmm = strftime(date, "%Y-%m")) %>%
                filter(date == max(date)) %>%
                as.data.frame() %>%
                select(-YYYYmm) %>%
                ungroup()

# Compute the daily, weekly and monthly differences and add a date column to the difference df 
# Compute the daily, weekly and monthly differences 
# and drop the first row which has NA's for first differences
delta_bei_daily   <- bei_daily %>% 
  mutate(across(-date, ~c(NA, diff(.)))) %>%
  filter(date > min(date))

delta_bei_weekly  <- bei_weekly %>% 
  mutate(across(-date, ~c(NA, diff(.)))) %>%
  filter(date > min(date))

delta_bei_monthly <- bei_monthly %>% 
  mutate(across(-date, ~c(NA, diff(.)))) %>%
  filter(date > min(date))

#Clean up NA's - these appear in the FRED data, but not on the Federal Reserve's website 
bei_daily   <- na.omit(bei_daily) 
bei_weekly  <- na.omit(bei_weekly) 
bei_monthly <- na.omit(bei_monthly) 
delta_bei_daily   <- na.omit(delta_bei_daily) 
delta_bei_weekly  <- na.omit(delta_bei_weekly) 
delta_bei_monthly <- na.omit(delta_bei_monthly)

#Choose the appropriate dataset 
if (freq == "d"){
  bei = bei_daily
  delta_bei = delta_bei_daily
  freqLong = "Daily"
  
} else if (freq == "w") {
  bei = bei_weekly
  delta_bei = delta_bei_daily
  freqLong = "Weekly"
  
} else {
  bei = bei_monthly
  delta_bei = delta_bei_daily
  freqLong = "Monthly"    
}

#First do a little EDA on delta_bei using Q-Q plots and pairs.panels 
#ggplot requires a data frame, so coerce the data table into a data frame
#The code is counter-intuitive, see the answer to the questio
#https://stackoverflow.com/questions/31993704/storing-ggplot-objects-in-a-list-from-within-loop-#in-r 
delta_bei.df = delta_bei[, bei_names]
myPlots = list() 
for(i in 1:length(bei_names)) local({
  i = i;
  myPlots[[i]] <<-ggplot(delta_bei.df, aes(sample = delta_bei.df[, i])) +
                    stat_qq(size = 0.5) +
                    stat_qq_line() +
                    labs(title=bei_names[i],
                         x = "Normal Quantiles",
                         y = paste0("Sample Quantiles: ", bei_names[i] )) +                   
                    theme(plot.title = element_text(size = 10),
                          axis.title = element_text(size = 8)) +
                    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
                    theme_minimal()
}) 

#Now plot the Q-Q plots in two rows 
allQQplots <- ggarrange(plotlist = myPlots, nrow = 2, ncol = ceiling(length(myPlots)/2))

# if (file.exists(allQQ_fn)) file.remove(allQQ_fn)

# png(allQQ_fn)
print(allQQplots)
# dev.off()


par(mfrow = c(1,1))
# if (file.exists(panels_fn)) file.remove(panels_fn)
# 
# png(filename = panels_fn, width = 480, height = 480, units = "px", 
#     pointsize = 12, bg = "white", res = NA, family = "", 
# 	  restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), antialias = "d") 
pairs.panels(delta_bei.df[, bei_names], lm = FALSE,
             main = paste(freqLong, " US Treasury Breakeven Inflation Moves") )
# dev.off()


bei_melt <- melt(bei, id.vars = "date")

# if (file.exists(ts_fn)) file.remove(ts_fn)

p <- ggplot(bei_melt, aes(date, value, col = variable)) +
      geom_line() +
      scale_color_manual(values = c("darkseagreen4", "goldenrod4")) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_bw() +
      theme(legend.position = c(0.60, 0.40),
            legend.title=element_text(size=12),      
            legend.text=element_text(size=11)) +
      labs(x = "Date", y = "Breakeven Inflation (%)", color = "Series")
# png(filename = ts_fn, width = 480, height = 480, units = "px", 
#     pointsize = 12, bg = "white", res = NA, family = "", 
#     restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), antialias = "d") 
print(p)
# dev.off()




