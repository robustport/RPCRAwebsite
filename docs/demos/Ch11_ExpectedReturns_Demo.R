# Unfortunately, every single plot or table in this chapter must be generated manually,
# as the underlying data is not distributable. Even so, the last code segment regenerates
# Figure 1.6 with the smaller PCRA dataset.

library(PCRA)
library(RobStatTM)
library(tidyverse)
library(readxl)
library(DescTools)
library(kableExtra)
library(fredr)
library(reshape2)
library(psych)
library(openxlsx)
library(quadprog)
library(rstudioapi)
library(zoo)

########################################Functions###################
my_merge <- function(df1, df2){  
  merge(df1, df2, by = "Date")
}


normalize_to_1 <- function(x) {
  # Divide all entries in a a vector by its first non-NA item,
  # so that the first non-NA item gets transformed to 1
  if(is.vector(x)){
    x_min <- x[min(which(!is.na(x)))]
  } else{
    x_min <- x[min(which(!is.na(x))), ]  
  }
  
  x <- x / x_min
  return(x)
}


growthrate <- function(d_t, k_e, mv, expLife) {
  # Computes the growth rate that is consistent with the
  # annual distributions, cost of equity, market value and 
  # expected life in a single stage probabilistic Gordon
  # Growth model
  g_numerator   <- mv * (k_e * expLife + 1) - d_t * expLife
  g_denominator <- mv * (expLife - 1) + d_t * expLife
  g <- g_numerator/ g_denominator
  return(g)
}


in_sample_plots <- function(df, x, x2, y, xlabels, ylabels, plt_titles, linear = TRUE){
  #Treat linear and quadratic fits separately
  T <- dim(df)[1]
  
  for (i in 1:length(x)){

    if (IN_SAMPLE_PLOTS){
      if (linear == TRUE){
        png_fn   <- paste0(TEMP_DIR, y[i], "_", x[i], "_", "in_sample_Linear.png")
        
      } else {
        png_fn   <- paste0(TEMP_DIR, y[i], "_", x[i], "_", x2[i], "_", "in_sample_Quadratic.png")
        
      }
      
      # Start the png driver
      png(filename = png_fn, width = 480, height = 480, units = "px",
          pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
          type = c("windows", "cairo", "cairo-png"), antialias = "d") 
      
      df1 <- df[, c(x[i], x2[i], y[i], "Year")]
      df1 <- df1[complete.cases(df1),]
      x_  <- df1[[x[i]]]
      x2_ <- df1[[x2[i]]]
      y_  <- df1[[y[i]]]
      yr  <- df1[["Year"]]
      
      plot(x_, y_, pch = 16, cex = 1.0, col = "blue", 
           xlab = xlabels[i], ylab = ylabels[i], main = plt_titles[i])
      text(x_, y_, pos = 2, offset = 1, cex = 0.6, col = "black", labels = as.factor(yr) )
    }
    
    # Treat linear and quadratic fits separately
    # Use predict to calculate estimated values as abline doesn't work
    if (linear){    
      # Linear fit
      fit      <- RobStatTM::lmrobdetMM(y_ ~ x_)
      best_fit <- predict(fit, df)
      
      if (IN_SAMPLE_PLOTS){lines(x_, best_fit, col = "red", type = "p")}
      
      coeff <- round(coef(fit),3)
      rsq   <- paste0(round(100* summary(fit)$adj.r.squared,1), "%")
      eq    <- paste0("y = ", coeff[1], " + ", coeff[2], "x  Adj. R2 = ", rsq)
      
      current_fcst <- coeff[1] + coeff[2] * df[[x[i]]][T]
      
    } else {
      # Quadratic fit
      fit      <- RobStatTM::lmrobdetMM(y_ ~ x_ + x2_)
      best_fit <- predict(fit, df)
      
      if (IN_SAMPLE_PLOTS){lines(x_, best_fit, col = "red", type = "p")}
      
      coeff <- round(coef(fit),3)
      rsq   <- paste0(round(100* summary(fit)$adj.r.squared,1), "%")
      eq    <- paste0("y = ", coeff[1]," + ", coeff[2], "x + ", coeff[3], 
                      "x^2  Adj. R2 = ", rsq)
      
      current_fcst <- coeff[1] + coeff[2] * df[[x[i]]][T] + coeff[3] * df[[x2[i]]][T]
    }
    
    if (IN_SAMPLE_PLOTS){
      mtext(eq, cex = 0.8)
      dev.off()
    }
    
    writeLines("\n\n")
    writeLines(plt_titles[i])
    writeLines(eq)
    writeLines(paste0("Current Forecast = ", round(100 * current_fcst,2), "%"))
    print(summary(fit))
  }
  
  if(PAUSE){
    if (linear){
      readline(prompt = "Finished in-sample linear fits: Hit ENTER to continue")
      
    } else {
      readline(prompt = "Finished in-sample quadratic fits: Hit ENTER to continue")
      
    }
  }
}



in_sample_plots_vs_forecast <- function(df, x, x2, y, xlabels, ylabels, plt_titles, linear = TRUE){
  T <- dim(df)[1]
  
  for (i in 1:length(x)){
    # Create the file in case we need to write out plots
    if (linear){
      png_fn <- paste0(TEMP_DIR, y[i], "_", x[i], "_", "in_sample_realized_vs_forecast.png")
      
    } else {
      png_fn <- paste0(TEMP_DIR, y[i], "_", x[i], "_", x2[i], "_", "in_sample_realized_vs_forecast.png")
      
    }
    
    if (IN_SAMPLE_PLOTS){
      png(filename = png_fn, width = 480, height = 480, units = "px",
          pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
          type = c("windows", "cairo", "cairo-png"), antialias = "d") 
    }
    
    df1 <- df[, c(x[i], x2[i], y[i], "Year")]
    df1 <- df1[complete.cases(df1),]
    x_  <- df1[[x[i]]]
    x2_ <- df1[[x2[i]]]
    y_  <- df1[[y[i]]]
    yr  <- df1[["Year"]]
    
    #Treat linear and quadratic fits separately
    # Use predict to calculate estimated values as abline doesn't work
    if (linear){    
      # Linear fit
      fit      <- lmrobdetMM(y_ ~ x_)
      best_fit <- predict(fit, df)
      
      res <- y_ - best_fit
      mse <- sqrt(mean(res^2, na.rm = TRUE))
      
      coeff <- round(coef(fit),3)
      rsq   <- paste0(round(100* summary(fit)$adj.r.squared,1), "%")
      eq    <- paste0("Corr(y,x)= ", round(cor(x_, y_), 2),
                      "  Adj. R2 = ", rsq,
                      "   MSE= ", round(100*mse, 2), "%")
      
      current_fcst <- coeff[1] + coeff[2] * df[[x[i]]][T]
      
      if (IN_SAMPLE_PLOTS){
        plot(best_fit, y_, pch = 16, cex = 1.0, col = "blue", 
             xlab = xlabels[i], ylab = ylabels[i], main = plt_titles[i])
        abline(a = 0, b = 1, col = 'goldenrod4', lwd = 2, lty = "dashed")
        text(best_fit, y_, pos = 2, offset = 1, cex = 0.6, col = "black", labels = as.factor(yr) )
      }
      
    } else {
      # Quadratic fit
      fit      <- lmrobdetMM(y_ ~ x_ + x2_)
      best_fit <- predict(fit, df)
      
      res <- y_ - best_fit
      mse <- sqrt(mean(res^2, na.rm = TRUE))
      
      coeff <- round(coef(fit),3)
      rsq   <- paste0(round(100* summary(fit)$adj.r.squared,1), "%")
      eq    <- paste0("Corr(y,x)= ", round(cor(x_, y_), 2),
                      "  Adj. R2 = ", rsq,
                      "   MSE= ", round(100*mse, 2), "%")
      
      current_fcst <- coeff[1] + coeff[2] * df[[x[i]]][T] + coeff[3] * df[[x2[i]]][T]
      if (IN_SAMPLE_PLOTS){
        plot(best_fit, y_, pch = 16, cex = 1.0, col = "blue", 
             xlab = xlabels[i], ylab = ylabels[i], main = plt_titles[i])
        abline(a = 0, b = 1, col = 'goldenrod4', lwd = 2, lty = "dashed")
        text(best_fit, y_, pos = 2, offset = 1, cex = 0.6, col = "black", labels = as.factor(yr) )
      }
    }
    
    if (IN_SAMPLE_PLOTS){
      mtext(eq, cex = 0.8)
      dev.off()
    }
    
    writeLines("\n\n")
    writeLines(plt_titles[i])
    writeLines(eq)
    writeLines(paste0("Current Forecast = ", round(100 * current_fcst,2), "%"))
    print(summary(fit)) 
  }
  
  if(PAUSE){
    if (linear){
      readline(prompt = "Finished in-sample linear fits: Hit ENTER to continue")
      
    } else {
      readline(prompt = "Finished in-sample quadratic fits: Hit ENTER to continue")
      
    }
  }
}



out_of_sample_plots <- function(df, x, x2, y, xlabels, ylabels, plt_titles, linear = TRUE){
  # Treat linear and quadratic / bi-variate fits separately
  # lmrobdetMM ignores NA's, so the left boundary for regressions can start at 1
  
  T <- dim(df)[1]
  for (i in 1:length(x)){
    n_min <- sum(is.na(df[[x[i]]])) + MIN_OUT_OF_SAMPLE_WINDOW
    n_max <- dim(df)[1] - FCST_HORIZON
    
    coeff <- array( NA, dim = c(T, 3) )   
    out_samp_pred <- rep( NA, T )
    predict_error <- rep( NA, T )
    if (linear){
      # Linear fit
      for (j in (n_min:n_max)){
        fit <- lmrobdetMM(df[[y[i]]][1:j] ~ df[[x[i]]][1:j])
        fit_coeffs <- coef(fit)
        coeff[j + FCST_HORIZON, 1:2 ]   <- fit_coeffs
        out_samp_pred[j + FCST_HORIZON] <- fit_coeffs[1] + 
          fit_coeffs[2] * df[[x[i]]][j + FCST_HORIZON]
        predict_error[j + FCST_HORIZON] <- out_samp_pred[j + FCST_HORIZON] -
          df[[y[i]]][j + FCST_HORIZON]
      }
      
      df[paste0('lin_alpha_', x[i])] <- coeff[ , 1]
      df[paste0('lin_beta_',  x[i])] <- coeff[ , 2]
      df[paste0('lin_fcst_',  x[i])] <- out_samp_pred
      df[paste0('lin_error_', x[i])] <- predict_error
      
    } else {
      # Quadratic fit
      for (j in (n_min:n_max)){
        fit <- lmrobdetMM(df$SP500NomNYrFwdRet[1:j] ~  df[[x[i]]][1:j]
                          + df[[x2[i]]][1:j])
        fit_coeffs <- coef(fit)
        coeff[j + FCST_HORIZON, 1:3 ]   <- fit_coeffs
        out_samp_pred[j + FCST_HORIZON] <- fit_coeffs[1] + 
          fit_coeffs[2] * df[[x[i]]][j + FCST_HORIZON] +
          fit_coeffs[3] * df[[x2[i]]][j + FCST_HORIZON]
        predict_error[j + FCST_HORIZON] <- out_samp_pred[j + FCST_HORIZON] -
          df[[y[i]]][j + FCST_HORIZON]
      }
      
      df[paste0('quad_alpha_', x[i], '_', x2[i] )] <- coeff[ , 1]
      df[paste0('quad_beta_',  x[i] )] <- coeff[ , 2]
      df[paste0('quad_beta_',  x2[i])] <- coeff[ , 3]
      df[paste0('quad_fcst_',  x[i] )] <- out_samp_pred
      df[paste0('quad_error_', x[i] )] <- predict_error
    }
    
    prefix = ifelse(linear, "lin_fcst_", "quad_fcst_")
    
    x_ <- df[[paste0(prefix,  x[i])]][ (n_min + FCST_HORIZON) : n_max]
    y_ <- df[[y[i]]][ (n_min + FCST_HORIZON) : n_max]
    yr <- df[["Year"]][ (n_min + FCST_HORIZON) : n_max]
    
    fit        <- lmrobdetMM(y_ ~ x_)
    fit_coeffs <- round(coef(fit),3)
    rsq        <- paste0(round(100* summary(fit)$adj.r.squared,1), "%")
    
    mse <- sqrt(mean(predict_error^2, na.rm = TRUE))
    
    eq  <- paste0("y= ", fit_coeffs[1], " + ", fit_coeffs[2], "x  Adj. R^2= ", rsq, 
                  "   MSE= ", round(100*mse, 2), "%")
    
    eq1 <- paste0("Corr(y,x)= ", round(cor(x_, y_), 2),
                  "  Adj. R2 = ", rsq,
                  "   MSE= ", round(100*mse, 2), "%")
    
    if (OUT_OF_SAMPLE_PLOTS){
      if (linear == TRUE){
        png_fn   <- paste0(TEMP_DIR, y[i], "_", x[i], "_", "_out_of_sample_Linear.png")
        
      } else{
        png_fn   <- paste0(TEMP_DIR, y[i], "_", x[i], "_", x2[i], "_", "out_of_sample_Quadratic.png")
        
      }
      
      png(filename = png_fn, width = 480, height = 480, units = "px",
          pointsize = 12, bg = "white", res = NA, family = "", 
          restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), antialias = "d") 
      
      plot(x_ , y_ , pch = 16, cex = 1.0, col = "blue", 
           xlab = xlabels[i], ylab = ylabels[i], main = plt_titles[i])
      text(x_, y_, pos = 2, offset = 1, cex = 0.6, col = "black", labels = as.factor(yr) )
      
      # abline(fit, col = "red",  lwd = 2, lty = "dashed")
      abline(a=0, b=1, col='goldenrod4', lwd=2, lty="dashed")
      
      #mtext(eq, cex = 0.8)
      mtext(eq1, cex = 0.8)
      dev.off()
    }
    
    writeLines("\n")
    writeLines(plt_titles[i])
    writeLines(eq)
    print(summary(fit))
    
    regtype = ifelse(linear, "Linear Forecast", "Quadratic Forecast")
    
    writeLines(paste0("Current parameters and estimate, ", regtype))
    writeLines(paste0('alpha_', x[i],    " = ", round(coeff[T, 1], 3) ))
    writeLines(paste0('beta_',  x[i],    " = ", round(coeff[T, 2], 3) ))
    if (linear == FALSE){ 
      writeLines(paste0('beta_',  x2[i], " = ", round(coeff[T, 3], 3) ))
    }
    
    writeLines(paste0(plt_titles[i], ": MSE= ", round(100*mse, 2), "%"))		
    
    if (linear == TRUE){
      writeLines(paste0('Current linear fcst_',     x[i], " = ", 
                        round(100*out_samp_pred[T], 2), "%" ))
    } else {
      writeLines(paste0('Current quadratic fcst_',  x[i], " = ", 
                        round(100*out_samp_pred[T], 2), "%" ))
    }
    writeLines(eq1)
    writeLines("\n\n")
    
    if(PAUSE){readline(prompt = "Hit ENTER to continue")}
  }
  return(df)
}



composite_out_of_sample_plot <- function(df, x_vars, y, xlabels, ylabels, plt_titles){
  # Composite forecast, put in 2, or 3 or 4 variables
  # lmrobdetMM ignores NA's, so the left boundary for regressions can start at 1
  
  T <- dim(df)[1]
  for (i in 1:length(x_vars)){
    regressors <- x_vars[[i]]
    
    n_min <- max(colSums(is.na(df[regressors]))) + MIN_OUT_OF_SAMPLE_WINDOW
    n_max <- dim(df)[1] - FCST_HORIZON
    
    coeff <- array( NA, dim = c(T, length(regressors) + 1) )   
    out_samp_pred <- rep( NA, T )
    predict_error <- rep( NA, T )
    
    for (j in (n_min:n_max)){
      fit <- lmrobdetMM(df[1:j, y[i]] ~ ., data = df[1:j, regressors ])
      fit_coeffs <- coef(fit)
      coeff[j + FCST_HORIZON, 1: (length(regressors) + 1) ] <- fit_coeffs
      out_samp_pred[j + FCST_HORIZON] <- fit_coeffs[1] + 
        sum(fit_coeffs[2: (length(regressors) + 1)] * 
              df[j + FCST_HORIZON, regressors] )
      predict_error[j + FCST_HORIZON] <- out_samp_pred[j + FCST_HORIZON] -
        df[[y[i]]][j + FCST_HORIZON]
    }
    
    
    df[paste0("Composite alpha_", paste0(regressors, collapse= "_"))] <- coeff[ , 1]
    for (k in (2:length(regressors))){
      df[paste("Composite beta", paste(regressors, collapse= "_"), k, sep = "_")]  <- coeff[ , k]
    }
    
    df[paste0("Composite fcst_",  paste0(regressors, collapse= "_"))] <- out_samp_pred
    df[paste0("Composite error_", paste0(regressors, collapse= "_"))] <- predict_error
    
    prefix = "Composite fcst_"
    
    x_ <- df[[paste0(prefix,  paste0(regressors, collapse= "_"))]][ (n_min + FCST_HORIZON) : n_max]
    y_ <- df[[y[i]]][ (n_min + FCST_HORIZON) : n_max]
    yr <- df[["Year"]][ (n_min + FCST_HORIZON) : n_max]
    
    fit        <- lmrobdetMM(y_ ~ x_)
    fit_coeffs <- round(coef(fit),3)
    rsq        <- paste0(round(100* summary(fit)$adj.r.squared,1), "%")
    
    mse <- sqrt(mean(predict_error^2, na.rm = TRUE))
    
    eq  <- paste0("y= ", fit_coeffs[1], " + ", fit_coeffs[2], "x  Adj. R^2= ", rsq, 
                  "   MSE= ", round(100*mse, 2), "%")
    eq1 <- paste0("Corr(y,x)= ", round(cor(x_, y_), 2),
                  "  Adj. R2 = ", rsq,
                  "   MSE= ", round(100*mse, 2), "%")
    
    if (OUT_OF_SAMPLE_PLOTS){
      png_fn   <- paste0(TEMP_DIR, y[i], "_", paste(regressors, collapse= "_"), 
                         "_out_of_sample_Composite.png")
      
    }
    
    png(filename = png_fn, width = 480, height = 480, units = "px",
        pointsize = 12, bg = "white", res = NA, family = "", 
        restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), antialias = "d") 
    
    plot(x_ , y_ , pch = 16, cex = 1.0, col = "blue", 
         xlab = xlabels[i], ylab = ylabels[i], main = plt_titles[i])
    text(x_, y_, pos = 2, offset = 1, cex = 0.6, col = "black", labels = as.factor(yr) )
    
    # abline(fit, col = "red",  lwd = 2, lty = "dashed")
    abline(a=0, b=1, col='goldenrod4', lwd=2, lty="dashed")
    
    #mtext(eq, cex = 0.8)
    mtext(eq1, cex = 0.8)
    dev.off()
    
    
    writeLines("\n\n")
    writeLines(plt_titles[i])
    writeLines(eq)
    print(summary(fit))
    
    writeLines(paste0("Current parameters and estimate, composite forecast"))
    writeLines(paste0('alpha_', paste(regressors, collapse= "_"), " = ", round(coeff[T, 1], 3) ))
    for (k in (1:length(regressors))){
      writeLines(paste0('beta_',  regressors[k],    " = ", round(coeff[T, k+1], 3) ))
    }
    
    writeLines(paste0(plt_titles[i], ": MSE= ", round(100*mse, 2), "%"))		
    
    writeLines(paste0('Current composite fcst_',  paste0(regressors, collapse= "_"), " = ", 
                      round(100*out_samp_pred[T], 2), "%" ))
    writeLines(eq1)
    
    if(PAUSE){readline(prompt = "Hit ENTER to continue")}
    
  }
  return(df)
}



combined_out_of_sample_plots <- function(df, pred1, pred2, error1, error2, y, 
                                         xlabels, ylabels, plt_titles,
                                         weighting = 1, additive = TRUE){
  
  # weighting = 1 for equally weighted forecasts
  # weighting = 2 for 1/variance weighted forecasts
  # weighting = 3 for 1/second moment weighted forecasts
  # weighting = 4 for unconstrained Granger-Ramanathan forecast
  # weighting = 5 for quadratic programming forecast
  
  for (i in 1:length(pred1)){
    if(weighting == 1){             # Equal weight
      if(additive){
        combined_fcst     <- paste0('Eq_Wt_Additive_', pred1[i], "_", pred2[i] )
        combined_fcst_err <- paste0('error_', combined_fcst)
        
      } else {
        combined_fcst     <- paste0('Eq_Wt_Mutliplicative_', pred1[i], "_", pred2[i] )
        combined_fcst_err <- paste0('error_', combined_fcst)
        
      }
      
    } else if(weighting == 2){      # 1/ Error Variance weight
      if(additive){
        combined_fcst     <- paste0('Inverse_var_Wt_Additive_', pred1[i], "_", pred2[i] )
        combined_fcst_err <- paste0('error_', combined_fcst)
        
      } else {
        combined_fcst     <- paste0('Inverse_var_Wt_Multiplicative_', pred1[i], "_", pred2[i] )
        combined_fcst_err <- paste0('error_', combined_fcst)
        
      }
      
    } else if(weighting == 3){      # 1 / Second moment weight
      if(additive){
        combined_fcst     <- paste0('2nd_moment_Wt_Additive_', pred1[i], "_", pred2[i] )
        combined_fcst_err <- paste0('error_', combined_fcst)
        
      } else {
        combined_fcst     <- paste0('2nd_moment_Wt_Multiplicative_', pred1[i], "_", pred2[i] )
        combined_fcst_err <- paste0('error_', combined_fcst)
        
      }
      
    } else if(weighting == 4){      # Unconstrained Robust Granger_Ramanathan forecast
      combined_fcst     <- paste0('Granger_Ramanathan_unconstrained_forecast_', pred1[i], "_", pred2[i] )
      combined_fcst_err <- paste0('error_', combined_fcst)
      
    }  else if(weighting == 5){      # Quadratic Programming / Style Analysis forecast
      combined_fcst     <- paste0('Quadratic_Programming_positive_w_forecast_', pred1[i], "_", pred2[i] )
      combined_fcst_err <- paste0('error_', combined_fcst)
      
    }
    
    png_fn <- paste0(TEMP_DIR,  "Out_of_Sample", "_", combined_fcst, ".png") 
    
    #Start computing variance, second moment etc. only after 4 realizations are available
    n_min  <- max( sum(is.na(df[[pred1[i]]])), sum(is.na(df[[pred2[i]]])) ) + 1
    N_obs  <- dim(df)[1]
    n_max  <- N_obs - FCST_HORIZON
    
    
    if(weighting == 1){
      if(additive){
        # Annualized Equally weighted average of the cumulative return of the two forecasts
        lt_fcst1 <- df[, pred1[i]]
        lt_fcst2 <- df[, pred2[i]]
        
        df[combined_fcst] <- rowMeans(cbind(lt_fcst1, lt_fcst2))
        
      } else {
        # Annualized geometric mean of the annualized return of the two forecasts
        lt_fcst1 <- (1 + df[, pred1[i]]) ^ 0.5
        lt_fcst2 <- (1 + df[, pred2[i]]) ^ 0.5
        
        df[combined_fcst] <- lt_fcst1 * lt_fcst2 - 1
        
      }
      
      df[combined_fcst_err] <- df[[y[i]]] - df[[combined_fcst]] 
      
    } else if(weighting == 2) {
      if(additive){
        # 1 / Variance weighted average of the two forecasts
        # Start by creating an equal weighted average. 
        # This is used for the first few points when we don't have enough
        # data to compute the variance.
        lt_fcst1 <- df[, pred1[i]]
        lt_fcst2 <- df[, pred2[i]]
        
        df[combined_fcst]     <- rowMeans(cbind(lt_fcst1, lt_fcst2))
        df[combined_fcst_err] <- df[[y[i]]] - df[[combined_fcst]]
        
        for(j in (n_min + MIN_VAR_WINDOW + FCST_HORIZON - 1): N_obs ){
          var1 <- var(df[[error1[i]]][n_min: (j - FCST_HORIZON)])
          var2 <- var(df[[error2[i]]][n_min: (j - FCST_HORIZON)])
          
          w1 <- (1/var1) / (1/var1 + 1/var2)
          w2 <- (1/var2) / (1/var1 + 1/var2)
          
          lt_fcst1 <- df[[pred1[i]]][j]
          lt_fcst2 <- df[[pred2[i]]][j]
          
          df[[combined_fcst]][j] <- w1 * lt_fcst1 + w2 * lt_fcst2
          if (j <= n_max){
            df[[combined_fcst_err]][j] <-  df[[y[i]]][j] - df[[combined_fcst]][j]
          }
        }
        
      } else {
        # 1 / Variance weighted geometric mean of the two forecasts
        # Start by creating an equal weighted geometric mean. 
        # This is used for the first few points when we don't have enough
        # data to compute the variance.
        lt_fcst1 <- 1 + df[, pred1[i]]
        lt_fcst2 <- 1 + df[, pred2[i]]
        
        df[combined_fcst]     <- (lt_fcst1 * lt_fcst2) ^ 0.5 - 1
        df[combined_fcst_err] <- df[[y[i]]] - df[[combined_fcst]]
        
        for(j in (n_min + MIN_VAR_WINDOW + FCST_HORIZON - 1): N_obs ){
          var1 <- var(df[[error1[i]]][n_min: (j - FCST_HORIZON)] )
          var2 <- var(df[[error2[i]]][n_min: (j - FCST_HORIZON)])
          
          w1 <- (1/var1) / (1/var1 + 1/var2)
          w2 <- (1/var2) / (1/var1 + 1/var2)
          
          lt_fcst1 <- (1 + df[[pred1[i]]][j])
          lt_fcst2 <- (1 + df[[pred2[i]]][j])
          
          df[[combined_fcst]][j] <- (lt_fcst1 ^ w1) * (lt_fcst2 ^ w2) - 1
          if (j <= n_max){
            df[[combined_fcst_err]][j] <- df[[y[i]]][j] - df[[combined_fcst]][j]
          }
          
        }
        
      }
      
    } else if(weighting == 3) {
      if(additive){
        # 1 / 2nd moment weighted average of the two forecasts
        # Start by creating an equal weighted average. 
        # This is used for the first few points when we don't have enough
        # data to compute the second moment.
        lt_fcst1 <- df[, pred1[i]]
        lt_fcst2 <- df[, pred2[i]]
        
        df[combined_fcst]     <- rowMeans(cbind(lt_fcst1, lt_fcst2) )
        df[combined_fcst_err] <- df[[y[i]]] - df[[combined_fcst]] 
        
        for(j in (n_min + MIN_VAR_WINDOW + FCST_HORIZON - 1): N_obs ){
          second_mom1 <- sum(df[[error1[i]]][n_min: (j - FCST_HORIZON)] ^ 2)
          second_mom2 <- sum(df[[error2[i]]][n_min: (j - FCST_HORIZON)] ^ 2)
          
          w1 <- (1/second_mom1) / (1/second_mom1 + 1/second_mom2)
          w2 <- (1/second_mom2) / (1/second_mom1 + 1/second_mom2)
          
          lt_fcst1 <- df[[pred1[i]]][j]
          lt_fcst2 <- df[[pred2[i]]][j]
          
          df[[combined_fcst]][j] <- ( w1 * lt_fcst1 + w2 * lt_fcst2)
          if (j <= n_max){
            df[[combined_fcst_err]][j] <- df[[y[i]]][j] - df[[combined_fcst]][j]
          }
          
        }
        
      } else {
        # 1 / Variance weighted geometric mean of the two forecasts
        # Start by creating an equal weighted geometric mean. 
        # This is used for the first few points when we don't have enough
        # data to compute the variance.
        lt_fcst1 <- 1 + df[, pred1[i]]
        lt_fcst2 <- 1 + df[, pred2[i]]
        
        df[combined_fcst]     <- (lt_fcst1 * lt_fcst2) ^ 0.5 - 1
        df[combined_fcst_err] <- df[[y[i]]] - df[[combined_fcst]]
        
        for(j in (n_min + MIN_VAR_WINDOW + FCST_HORIZON - 1): N_obs ){
          second_mom1 <- sum(df[[error1[i]]][n_min: (j - FCST_HORIZON)] ^ 2)
          second_mom2 <- sum(df[[error2[i]]][n_min: (j - FCST_HORIZON)] ^ 2)
          
          w1 <- (1/second_mom1) / (1/second_mom1 + 1/second_mom2)
          w2 <- (1/second_mom2) / (1/second_mom1 + 1/second_mom2)
          
          lt_fcst1 <- (1 + df[[pred1[i]]][j])
          lt_fcst2 <- (1 + df[[pred2[i]]][j])
          
          df[[combined_fcst]][j] <- (lt_fcst1 ^ w1) * (lt_fcst2 ^ w2) - 1
          if (j <= n_max){
            df[[combined_fcst_err]][j] <- df[[y[i]]][j] - df[[combined_fcst]][j]
          }
          
        }
        
      }
      
    } else if(weighting == 4) {
      # Unconstrained Granger Ramanathan forecast using robust regression
      # Start by creating an equal weighted average. 
      # This is used for the first few points when we don't have enough
      # data to compute the Granger Ramanathan weights.
      lt_fcst1 <- df[, pred1[i]]
      lt_fcst2 <- df[, pred2[i]]
      
      # Compute an error based on the equally weighted average, replace it
      # if we get a valid Granger Ramanathan forecast
      df[combined_fcst]     <- rowMeans(cbind(lt_fcst1, lt_fcst2) )
      df[combined_fcst_err] <- df[[y[i]]] - df[[combined_fcst]]
      
      for(j in (n_min + MIN_VAR_WINDOW + FCST_HORIZON - 1): N_obs ){
        
        # Now create time series of gross returns for both predictors and the actual return 
        # over FCST_HORIZON years
        x1_ <- df[[pred1[i]]][ n_min : (j - FCST_HORIZON)]
        x2_ <- df[[pred2[i]]][ n_min : (j - FCST_HORIZON)]
        y_  <- df[[y[i]]][ n_min : (j - FCST_HORIZON)]
        
        # For reasons I don't understand, lmrobdet works much worse than lm. Keep it around
        # but use lm for now
        # fit <- try({lmrobdetMM(y_ ~ x1_ + x2_)}, silent = TRUE)
        fit <- try({lm(y_ ~ x1_ + x2_)}, silent = TRUE)
        
        # If we use lmrobdetMM, we need the try construct, as it results in an an error
        # if it doesn't have enough observations. In particular, all the coefficients 
        # can sometimes be 0 even if no error is raised. 
        # In these cases, just use an equal weighted forecast
        if( "try-error" %in% class(fit) ) {
          writeLines("Caught and discarded an error in lmrobdetMM")
          #          fit <- try({lm(y_ ~ x1_ + x2_)}, silent = TRUE)
          
        }
        
        fit_coeffs  <- round(coef(fit), 3)
        GR_intrcpt  <- fit_coeffs[1]
        GR_slope_x1 <- fit_coeffs[2]
        GR_slope_x2 <- fit_coeffs[3]
        
        if (GR_intrcpt == 0 && GR_slope_x1 == 0 && GR_slope_x2 == 0){
          writeLines("Caught and discarded an error in lmrobdetMM")
          
        } else {
          df[[combined_fcst]][j] <- GR_intrcpt + 
            GR_slope_x1 * lt_fcst1[j] +
            GR_slope_x2 * lt_fcst2[j]
          if (j <= n_max){
            df[[combined_fcst_err]][j] <- df[[y[i]]][j] - df[[combined_fcst]][j]
          }
          
        }
        
      }
      
    } else if(weighting == 5) {
      # Constrained forecast using quadratic programming
      # Start by creating an equal weighted average. 
      # This is used for the first few points when we don't have enough
      # data to compute the zero bias weights.
      lt_fcst1 <- df[, pred1[i]]
      lt_fcst2 <- df[, pred2[i]]
      
      # Compute an error based on the equally weighted average, 
      # replace it if we get a valid Granger Ramanathan forecast
      df[combined_fcst]     <- rowMeans(cbind(lt_fcst1, lt_fcst2) )       
      df[combined_fcst_err] <- df[[y[i]]] - df[[combined_fcst]]
      
      for(j in (n_min + MIN_VAR_WINDOW + FCST_HORIZON - 1): N_obs ){
        
        # Now create time series of gross returns for both predictors and the actual return 
        # over FCST_HORIZON years
        x1_ <- df[[pred1[i]]][ n_min : (j - FCST_HORIZON)]
        x2_ <- df[[pred2[i]]][ n_min : (j - FCST_HORIZON)]
        y_  <- df[[y[i]]][ n_min : (j - FCST_HORIZON)]
        
        # solve.QP has truly awful notation and the docs are of little help.  
        # See https://stats.stackexchange.com/questions/79059/linear-regression-with-constrained-coefficient
        # and https://cran.r-project.org/web/packages/quadprog/quadprog.pdf
        # X_mat has a column of 1's for the constant, then come the regressors
        x_mat <- matrix(c(rep(1, length(x1_)), x1_, x2_), ncol=3) 
        y_mat <- matrix(y_)
        
        D <- t(x_mat) %*% x_mat # D = X'X
        d <- t(y_mat) %*% x_mat # d = Y'X
        p <- length(d)
        
        A <- diag(p)                 # A matrix for A beta >= b
        b <- c(-1000, rep(0, p - 1)) # Lower bounds on coefficients
        
        soln  <- solve.QP(D, d, A, b, meq = 0)
        betas <- soln$solution
        
        df[[combined_fcst]][j] <- betas[1] + 
          betas[2] * lt_fcst1[j] +
          betas[3] * lt_fcst2[j]
        if (j <= n_max){
          df[[combined_fcst_err]][j] <- df[[y[i]]][j] - df[[combined_fcst]][j]
        }
        
      }
    }
    
    x_ <- df[[combined_fcst]][ n_min : n_max]
    y_ <- df[[y[i]]][ n_min : n_max]
    yr <- df[["Year"]][ n_min : n_max]
    
    fit        <- lmrobdetMM(y_ ~ x_)
    fit_coeffs <- round(coef(fit), 3)
    rsq        <- paste0(round(100* summary(fit)$adj.r.squared,1), "%")
    
    mse  <- sqrt(mean(df[[combined_fcst_err]]^2, na.rm = TRUE))
    bias <- mean(df[[combined_fcst_err]], na.rm = TRUE)
    min_err <- min(df[[combined_fcst_err]], na.rm = TRUE)
    max_err <- max(df[[combined_fcst_err]], na.rm = TRUE)
    
    eq  <- paste0("y= ", fit_coeffs[1], " + ", fit_coeffs[2], "x  Adj. R^2= ", rsq, 
                  "   MSE= ", round(100*mse, 2), "%")
    eq1 <- paste0("Corr(y,x)= ", round(cor(x_, y_), 2),  
                  "  Adj. R^2= ", rsq, "   MSE= ", round(100*mse, 2), "%")
    eq2 <- paste0(eq1, "   Bias= ", round(100*bias, 2), "%",
                  "   Min(e_t)= ", round(100*min_err, 2), "%",
                  "   Max(e_t)= ", round(100*max_err, 2), "%")
    
    if(COMBINED_PLOTS){
      png(filename = png_fn, width = 480, height = 480, units = "px",
          pointsize = 12, bg = "white", res = NA, family = "", 
          restoreConsole = TRUE, type = c("windows", "cairo", "cairo-png"), antialias = "d") 
      
      plot(x_ , y_ , pch = 16, cex = 1.0, col = "blue", 
           xlab = xlabels[i], ylab = ylabels[i], main = plt_titles[i])
      text(x_, y_, pos = 2, offset = 1, cex = 0.6, col = "black", 
           labels = as.factor(yr) )
      
      # abline(fit, col = "red", lwd = 2, lty = "dashed")
      abline(a=0, b=1, col='goldenrod4', lwd=2, lty="dashed")
      
      # mtext(eq,  cex = 0.8)
      mtext(eq1, cex = 0.8)
      
      dev.off()
    }
    
    writeLines(paste0(plt_titles[i], ": MSE= ", round(100*mse, 2), "%"))
    writeLines(eq2)
    writeLines(paste0("Current Forecast = ", round (100 * df[[combined_fcst]][ N_obs], 2), "% / annum"))
    writeLines("\n")
    
    if(PAUSE){ readline(prompt = "Hit ENTER to continue")}
  }
  
  return(df)       
}


########################################Constants##################

# Directory for all downloads
TEMP_DIR <- "C:/Temp/"

# Key that allows us to download data from the Federal Reserve's FRED database.
fredr_set_key("caf71401e549e0570139e5188af2e40b")

# URL for Fama French 3 factor model
ff_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip"

# URL and filename prefixes for Russell data files
RUSSELL_URL <- "https://research.ftserussell.com/products/russell-index-values/home/getfile?id="
RUSSELL_HIST_PREFIX <- "valueshist_"
RUSSELL_YTD_PREFIX  <- "valuesytd_"

R1000_fn    <- "US1000.csv"
R1000G_fn   <- "US1001.csv"
R1000V_fn   <- "US1002.csv"

RMidcap_fn  <- "US5015.csv"
RMidcapG_fn <- "US5006.csv"
RMidcapV_fn <- "US5007.csv"

R2000_fn    <- "US2000.csv"
R2000G_fn   <- "US2001.csv"
R2000V_fn   <- "US2002.csv"

# URL and files names for q5 factors
Q5_URL     <- "https://global-q.org/uploads/1/2/2/6/122679606/"
QFACTOR_FN <- "q5_factors_monthly_"
OUTPUT_FN  <- "Q5 Factor Returns"
WRITE_Q5_DATA <- TRUE  # TRUE to write two csv files with monthly & cumulative returns

# Files name for Short-term High Grade Credit Factor
CREDIT_FN <- "Short term Ultra High Grade Credit Risk Premium.csv"

# Apple financials for expected life tradeoff. Numbers taken from 
# the statement of cash flows in Apple's 10-k, availabe at
# https://investor.apple.com/sec-filings/default.aspx
ANN_DIVIDEND   <- 14.841E9
ANN_REPURCHASE <- 89.402E9
TSY_YLD_10YR   <- 0.0477
EQUITY_PREMIUM <- 0.02
MARKET_VALUE   <- 2.22e12


#Pre-populated S&P data file for CAPE studies
SP_DATA_FN <- "SandP500 Historical Data.csv"


# Set minimum amount of data for an out of sample prediction and forecast horizon in years
# Set DISPLAY_SUMMARIES = TRUE if want to summarize the input data
# MIN_VAR_WINDOW is the minimum number of realizations needed to compute error variance / 2nd mom. 
MIN_OUT_OF_SAMPLE_WINDOW <- 15
FCST_HORIZON <- 10
DISPLAY_SUMMARIES   <- TRUE
IN_SAMPLE_PLOTS     <- TRUE
OUT_OF_SAMPLE_PLOTS <- TRUE
COMBINED_PLOTS      <- TRUE
MIN_VAR_WINDOW      <- 4
WRITE_DATAFRAME     <- TRUE
PAUSE <- FALSE

DF_FN    <- "CAPE Studies Dataframe.xlsx"

####################################### Main Script ##################

# Set the working directory automatically using the rstudioapi library
current_path <- getActiveDocumentContext()$path 
#setwd(dirname(current_path ))
setwd(TEMP_DIR)

# Table 11.1
writeLines("Data for Table 1 must be downloaded manually from Bloomberg,")
writeLines("and will require a license. The relevant Bloomberg codes are:")
writeLines("1. LUATTRUU <Index>: Bloomberg Barclays U.S. Treasury Index") 
writeLines("2. LUACTRUU <Index>: Bloomberg Barclays U.S. Corporate Index")
writeLines("3. SPTR <Index>: S&P 500 Total Return Index\n\n")

# Table 11.2
writeLines("All returns in Table 2 are expresssed as decimals for simplicity")
writeLines("df_bond can be further process via kableExtra for elegant output")
df_bond <- data.frame(t   = seq(0, 10, 1),
                      y_t = seq(.1, 0, -.01)
                     )

df_bond$p_t              <- 100 / (1 + df_bond$y_t) ^ (10 - df_bond$t)
df_bond$r_prior_yr       <- df_bond$p_t / lag(df_bond$p_t, 1) - 1
df_bond$r_from_inception <- (df_bond$p_t / df_bond$p_t[1]) ^(1 / df_bond$t) - 1
df_bond$r_to_maturity    <- (100 / df_bond$p_t) ^(1/(10 - df_bond$t)) - 1

# Round all numbers
df_bond$p_t              <- round(df_bond$p_t, 2)
df_bond$r_prior_yr       <- round(df_bond$r_prior_yr, 4)
df_bond$r_from_inception <- round(df_bond$r_from_inception, 4)
df_bond$r_to_maturity    <- round(df_bond$r_to_maturity, 4)
print(df_bond)

writeLines("The kableExtra code for a pretty output is provided below\n\n")
kable(df_bond, format = "latex", booktabs = T, linesep = "", row.names = FALSE,
      align = c("c", "c", "r", "c", "c", "c"),  escape = F) %>% 
  kable_styling("striped", full_width = F, font_size = 10 ) %>%
  column_spec( 1:6,  monospace= TRUE, width = "3em" ) %>%   
  row_spec( 0, hline_after = TRUE ) %>%
  row_spec(11, hline_after = TRUE )

# Figure 11.1
writeLines("Data for Figure 2 is obtained from the PCRA dataset, which is provided by S&P Global Markets Intelligence")


# Figure 11.2
writeLines("Data for Figure 2 is obtained from Robert Shiller's website and the Federal Reserve's FRED database")
writeLines("Row 4 has all the relevant categories, while the semi-annual notional amounts are found starting in Row 20.")
writeLines(" Data for the S&P 500 can be downloaded from https://shillerdata.com though the URL has become complex in recent years")
download.file("https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/25d6827d-c04b-447a-bb6d-918d5d88be49/ie_data.xls", 
              destfile=paste0(TEMP_DIR, "shiller_data.xls"), mode ="wb")
SandP_earnings      <- read_excel(paste0(TEMP_DIR, "shiller_data.xls"), 
                                  sheet = "Data", skip = 7)
SandP_earnings      <- SandP_earnings[ , colnames(SandP_earnings) %in% c("Date", "E")]
SandP_earnings      <- SandP_earnings[complete.cases(SandP_earnings), ]
SandP_earnings$Date <- round(SandP_earnings$Date, 2)
SandP_earnings      <- SandP_earnings[abs(SandP_earnings$Date - floor(SandP_earnings$Date) - 0.12) < 0.001, ]
SandP_earnings$Date <- as.integer(SandP_earnings$Date)
colnames(SandP_earnings) <- c("Year", "SandP 500 EPS")

writeLines("\nObtaining data from FRED will require the fredr package and a FRED API key: for details see")
writeLines("https://cran.r-project.org/web/packages/fredr/vignettes/fredr.html")
nom_GDP      <- map_dfr("GDPA", fredr)
nom_GDP$Date <- as.integer(format(nom_GDP$date,'%Y'))
nom_GDP      <- nom_GDP[, c("Date", "value")]
colnames(nom_GDP) <- c("Year", "Nominal GDP")

eps_gdp <- merge(SandP_earnings, nom_GDP, by = "Year")
eps_gdp$EPS_to_GDP <- eps_gdp[, "SandP 500 EPS"] / eps_gdp[, "Nominal GDP"]
eps_gdp$EPS_to_GDP <- eps_gdp$EPS_to_GDP / eps_gdp[1, "EPS_to_GDP"]

# Create a ggplot, then output a png file
# Delete the existing file if it exists, then recreate it
# PNG_fn_1 <- "../Plots/EPS vs. Nominal GDP Growth.PNG"
# if(file.exists(PNG_fn_1)){file.remove(PNG_fn_1)}
# png(filename = PNG_fn_1, width = 900, height = 650, units = "px",
#     pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
#     type = c("windows", "cairo", "cairo-png"), antialias = "d")  
  
  plt <- ggplot(eps_gdp, aes(x = Year, y = EPS_to_GDP )) +
          geom_line() +
          labs(y = "S&P 500 EPS / Nominal GDP Normalized") +
          theme_bw()  + 
          theme(aspect.ratio = 2/3,
                # The order of edges for plot.margin is unit(c(t, r, b, l), units) 
                plot.margin  = unit(c(t = 0.25, r = 1, b = 0.25, l = 0.25), "cm")) +
          theme(axis.title.x = element_text(size = 18, face = "bold", 
                                            hjust=0.5, margin = margin(t = 20)),
                axis.title.y = element_text(size = 18, face = "bold", 
                               hjust=0.5, margin = margin(r = 10)),
                axis.text.x  = element_text(size = 16),
                axis.text.y  = element_text(size = 16),
               )
  
  show(plt)
  
# dev.off()  
# Need to show(plt) to make it visible in RStudio
show(plt)


# Figure 11.3
# Only a portion of this graph can be recreated, as the data on the Russell 
# Style indices is not public. It is possible to obtain Russell index data from
# a website, but the data starts only in 2005.
# Download the file, which is delivered as a zip file then unzip it
download.file(ff_url, paste0(TEMP_DIR, "FF3.zip"))
unzip(paste0(TEMP_DIR, "FF3.zip"), exdir = TEMP_DIR)
ff3_1926 <- read.csv(paste0(TEMP_DIR, "F-F_Research_Data_Factors.CSV"), skip = 3)

# Clean up column names, discard annual returns starting at first_bad_row, 
# then convert to a decimal
colnames(ff3_1926) <- c("Date", "Rm_Rf", "SMB", "HML", "rf")
first_bad_row <- grep("Annual", ff3_1926[, "Date"] )
ff3_1926 <- head(ff3_1926, first_bad_row - 1)
ff3_1926[, !colnames(ff3_1926) == "Date"] <- sapply(ff3_1926[, !colnames(ff3_1926) == "Date"], 
                                                    as.numeric) / 100
ff3_1926[, "Date"] <- as.yearmon(ff3_1926[, "Date"], format = "%Y%m")

# Add Jan and non-Jan months for SMB, and create a dataset starting in 1978
ff3_1926$SMB_Jan    <- ifelse(month(ff3_1926$Date) ==12, ff3_1926$SMB, 0)
ff3_1926$SMB_nonJan <- ifelse(month(ff3_1926$Date) !=12, ff3_1926$SMB, 0)
ff3_1978 <- ff3_1926[ff3_1926$Date > as.yearmon("197812",format = "%Y%m"), ]

# Compute cumulative returns starting in 1926 and 1978.
ff3_1926[1, !colnames(ff3_1926) == "Date"] <- 0
ff3_1978[1, !colnames(ff3_1978) == "Date"] <- 0
ff3_1926[!colnames(ff3_1926) == "Date"] <- cumprod(1 + ff3_1926[!colnames(ff3_1926) == "Date"])
ff3_1978[!colnames(ff3_1978) == "Date"] <- cumprod(1 + ff3_1978[!colnames(ff3_1978) == "Date"])

plt <- ggplot(ff3_1978, aes(x = Date, y = HML )) +
        geom_line() +
        labs(y = "Fama-French HML Factor: Cumulative Return") +
        theme_bw()  + 
        theme(aspect.ratio = 2/3)

# Need to show(plt) to make it visible in RStudio
show(plt)

# Figure 11.4 and 11.6
# The data required for this plot is proprietary to LSE Group and therefore cannot be 
# provided, but we can create a variety of plots starting in 2005 
# from data made available to the public by LSE Group / Russell indexes
# Start by downloading the index values files from RUSSELL_URL to C:/Temp
# Russell 1000, 1000 Growth, 1000 Value  
# Russell Midcap, MidCap Growth, Midcap Value  
# Russell 2000, 2000 Growth, 2000 Value  
# Store all the csv files in TEMP_DIR
# Create a collection of plots that are instructive to view

OUTPUT_fn   <- paste0(TEMP_DIR, "Russell Index Returns.csv")


# List of all suffixes in the order we want them in the dataframe
fn_list     <- c(R1000_fn,   R1000G_fn,   R1000V_fn,
                 RMidcap_fn, RMidcapG_fn, RMidcapV_fn,
                 R2000_fn,   R2000G_fn,   R2000V_fn)

# List to hold all dataframes as they are read in
df_list <- vector(mode = "list", length = length(fn_list))

for(i in 1:length(fn_list)){
  fn <- fn_list[i]
  
  fn_hist  <- paste0(TEMP_DIR, RUSSELL_HIST_PREFIX, fn)
  fn_ytd   <- paste0(TEMP_DIR, RUSSELL_YTD_PREFIX,  fn)
  
  url_hist <- paste0(RUSSELL_URL, RUSSELL_HIST_PREFIX, fn)
  url_ytd  <- paste0(RUSSELL_URL, RUSSELL_YTD_PREFIX,  fn)
  
  # Download the return files from Russell's website
  download.file(url_hist, fn_hist, mode = "wb")
  download.file(url_ytd,  fn_ytd,  mode = "wb")
  
  # Read in the historical and ytd data and rbind them. Drop the last line 
  # of the YTD file as it has the last day of the prior year. If we don't, we 
  # get two lines with the same date and this messes up the return computation.
  # The files have data in reverse chronological order
  df_hist  <- read.csv(fn_hist)
  df_ytd   <- read.csv(fn_ytd)
  df_all   <- rbind(df_hist, head(df_ytd, -1))
  
  # Extract the index's name, then keep only Date, Price Index and
  # Total Return Index columns
  idx_name <- df_all[1, 1]
  df_all   <- df_all[ , c("Date", "Value_Without_Dividends__USD_",
                          "Value_With_Dividends__USD_")]
  
  # Reformat and order the date column in ascending order
  df_all$Date <- as.Date(df_all$Date, format = "%m/%d/%Y")
  df_all      <- df_all[order(df_all$Date), ]
  
  # Rename the columns using the index name 
  # Finally update the list of Total Return data frames
  colnames(df_all) <- c("Date", paste0(idx_name, " Price Return"),
                        paste0(idx_name, " Total Return"))
  df_list[[i]]     <- df_all
}

# Merge all the dataframes on Date using Reduce
Russell_data <- Reduce(my_merge, df_list)

# Clean up all the special characters in the index names
colnames(Russell_data) <- gsub("[[:punct:]]", "", colnames(Russell_data))

# Scale all indices so that they start at 1, 
# Then create all the SMB and HML equivalents
norm_return <- as.data.frame(lapply(Russell_data[ , which(colnames(Russell_data) 
                                                          != "Date")], 
                                    FUN =  normalize_to_1))
Russell_data <- cbind(Russell_data["Date"], norm_return)
Russell_data["HML_Russell_Large"] <- Russell_data["Russell.1000.Value.Total.Return"] /
                                     Russell_data["Russell.1000.Growth.Total.Return"]
Russell_data["HML_Russell_Mid"]   <- Russell_data["Russell.Midcap.Value.Total.Return"] /
                                     Russell_data["Russell.Midcap.Growth.Total.Return"]
Russell_data["HML_Russell_Small"] <- Russell_data["Russell.2000.Value.Total.Return"] /
                                     Russell_data["Russell.2000.Growth.Total.Return"]
Russell_data["SMB_Russell_Large"] <- Russell_data["Russell.2000.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]
Russell_data["SMB_Russell_Mid"]   <- Russell_data["Russell.2000.Total.Return"] /
                                     Russell_data["Russell.Midcap.Total.Return"]
Russell_data["SMB_Russell_Large"] <- Russell_data["Russell.2000.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]
Russell_data["SMB_Russell_Mid"]   <- Russell_data["Russell.2000.Total.Return"] /
                                     Russell_data["Russell.Midcap.Total.Return"]
Russell_data["SMB_Russell_SV_LV"] <- Russell_data["Russell.2000.Value.Total.Return"] /
                                     Russell_data["Russell.1000.Value.Total.Return"]
Russell_data["SMB_Russell_SV_MV"] <- Russell_data["Russell.2000.Value.Total.Return"] /
                                     Russell_data["Russell.Midcap.Value.Total.Return"]
Russell_data["SMB_Russell_MV_LV"] <- Russell_data["Russell.Midcap.Value.Total.Return"] /
                                     Russell_data["Russell.1000.Value.Total.Return"]
Russell_data["SMB_Russell_SG_LG"] <- Russell_data["Russell.2000.Growth.Total.Return"] /
                                     Russell_data["Russell.1000.Growth.Total.Return"]
Russell_data["SMB_Russell_SG_MG"] <- Russell_data["Russell.2000.Growth.Total.Return"] /
                                     Russell_data["Russell.Midcap.Growth.Total.Return"]
Russell_data["SMB_Russell_MG_LG"] <- Russell_data["Russell.Midcap.Value.Total.Return"] /
                                     Russell_data["Russell.1000.Value.Total.Return"]
Russell_data["R1V_vs_R1K"]        <- Russell_data["Russell.1000.Value.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]
Russell_data["R1G_vs_R1K"]        <- Russell_data["Russell.1000.Growth.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]
Russell_data["RMV_vs_R1K"]        <- Russell_data["Russell.Midcap.Value.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]
Russell_data["RMG_vs_R1K"]        <- Russell_data["Russell.Midcap.Growth.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]
Russell_data["R2V_vs_R1K"]        <- Russell_data["Russell.2000.Value.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]
Russell_data["R2G_vs_R1K"]        <- Russell_data["Russell.2000.Growth.Total.Return"] /
                                     Russell_data["Russell.1000.Total.Return"]


# Use the new R 4.2 pipe to melt the dataframes and plot everything in one plot
Russell_HML <- Russell_data[, c("Date", "HML_Russell_Large", "HML_Russell_Mid", 
                                "HML_Russell_Small")] |> 
                 reshape2::melt(id.vars = "Date", variable.name = "Value_vs_Growth")
Russell_SMB <- Russell_data[, c("Date", "SMB_Russell_Large", "SMB_Russell_Mid")] |> 
                 reshape2::melt(id.vars =  "Date", variable.name = "Small_vs_Large")
Russell_SMB_Value  <- Russell_data[, c("Date", "SMB_Russell_SV_LV", "SMB_Russell_SV_MV",
                                       "SMB_Russell_MV_LV")] |> 
                        reshape2::melt(id.vars = "Date", variable.name = "Small_vs_Large_Value")
Russell_SMB_Growth <- Russell_data[, c("Date", "SMB_Russell_SG_LG", "SMB_Russell_SG_MG",
                                       "SMB_Russell_MG_LG")] |> 
                        reshape2::melt(id.vars = "Date", variable.name = "Small_vs_Large_Growth")
Russell_All_vs_R1K <- Russell_data[, c("Date", "R1V_vs_R1K", "R1G_vs_R1K",
                                               "RMV_vs_R1K", "RMG_vs_R1K",
                                               "R2V_vs_R1K", "R2G_vs_R1K")] |> 
                        reshape2::melt(id.vars = "Date", variable.name = "Styles_vs_Market")

# Plot Russell_HML
plt <- ggplot(Russell_HML, aes(x = Date, y = value )) +
        geom_line(aes(color = Value_vs_Growth), linewidth = .5) +
        labs(y = "Russell HML Factor: Cumulative Relative Return") +
        theme_bw()   +
        scale_y_continuous(limits = c(0.4, 1.2), 
                           breaks = seq(0.4, 1.2, 0.1),
                           trans = "log2")

# Need to show(plt) to make it visible in RStudio
show(plt)

# Plot Russell_SMB
plt <- ggplot(Russell_SMB, aes(x = Date, y = value )) +
        geom_line(aes(color = Small_vs_Large), linewidth = .5) +
        labs(y = "Russell SMB Factor: Cumulative Relative Return") +
        theme_bw()   +
        scale_y_continuous(limits = c(0.7, 1.2), 
                           breaks = seq(0.7, 1.2, 0.1),
                           trans = "log2")

# Need to show(plt) to make it visible in RStudio
show(plt)

# Plot Russell_SMB_Value
plt <- ggplot(Russell_SMB_Value, aes(x = Date, y = value )) +
        geom_line(aes(color = Small_vs_Large_Value), linewidth = .5) +
        labs(y = "Russell SMB Factor (Value Only): Cumulative Relative Return") +
        theme_bw()   +
        scale_y_continuous(limits = c(0.6, 1.3), 
                           breaks = seq(0.6, 1.3, 0.1),
                           trans = "log2")

# Need to show(plt) to make it visible in RStudio
show(plt)

# Plot Russell_SMB_Growth
plt <- ggplot(Russell_SMB_Growth, aes(x = Date, y = value )) +
        geom_line(aes(color = Small_vs_Large_Growth), linewidth = .5) +
        labs(y = "Russell SMB Factor (Growth Only): Cumulative Relative Return") +
        theme_bw()   +
        scale_y_continuous(limits = c(0.6, 1.3), 
                           breaks = seq(0.6, 1.3, 0.1),
                           trans = "log2")

# Need to show(plt) to make it visible in RStudio
show(plt)

# Plot Russell_All_vs_R1K
plt <- ggplot(Russell_All_vs_R1K, aes(x = Date, y = value )) +
        geom_line(aes(color = Styles_vs_Market), linewidth = .5) +
        labs(y = "Russell Indices vs. Russell 1000: Cumulative Relative Return") +
        theme_bw()   +
        scale_y_continuous(limits = c(0.5, 1.5), 
                           breaks = seq(0.5, 1.5, 0.1),
                           trans = "log2")

# Need to show(plt) to make it visible in RStudio
show(plt)

# Figure 11.5 
# Only a portion of this graph can be recreated, as the data on the Russell 
# Style indices is not public. 
plt <- ggplot(ff3_1978, aes(x = Date, y = SMB )) +
        geom_line() +
        labs(y = "Fama-French SMB Factor: Cumulative Return") +
        theme_bw()  + 
        theme(aspect.ratio = 2/3)

# Need to show(plt) to make it visible in RStudio
show(plt)

# Figure 11.7
# Use the new R 4.2 pipe to melt the dataframe and plot everything in one plot
ff3_1926_melt <- ff3_1926[, c("Date", "HML", "SMB")] |> 
                  reshape2::melt(id.vars = "Date", variable.name = "FF3_Factors")
plt <- ggplot(ff3_1926_melt, aes(x = Date, y = value )) +
        geom_line(aes(color = FF3_Factors), linewidth = .5) +
        labs(y = "Cumulative Return to SMB and HML") +
        scale_y_continuous(limits = c(0.5, 64), 
                           breaks = 2^seq(-1, 6, 1),
                           trans = "log2") +
        theme_bw()  + 
        theme(aspect.ratio = 2/3)

# Need to show(plt) to make it visible in RStudio
show(plt)

# Figure 11.8
# Use the new R 4.2 pipe to melt the dataframe and plot everything in one plot
ff3_1926_melt <- ff3_1926[, c("Date", "SMB_Jan", "SMB_nonJan")] |> 
                  reshape2::melt(id.vars = "Date", variable.name = "FF3_Factors")
plt <- ggplot(ff3_1926_melt, aes(x = Date, y = value )) +
        geom_line(aes(color = FF3_Factors), linewidth = .5) +
        labs(y = "Cumulative Return to Size: January vs. Non-January months") +
        scale_y_continuous(limits = c(0.5, 8), 
                           breaks = 2^seq(-1, 3, 1),
                           trans = "log2") +
        theme_bw() + 
        theme(aspect.ratio = 2/3)

# Need to show(plt) to make it visible in RStudio
show(plt)

# Figure 11.9
# Three phases for the Q5 factor model:
# 1967 to 1999, 1999 to 2009 and 2009 to present time
PHASE_1_END <- zoo::as.yearmon("DEC 1999")
PHASE_2_END <- zoo::as.yearmon("DEC 2009")

DATA_fn_1 <- paste0(TEMP_DIR, OUTPUT_FN, " Monthly.csv")
DATA_fn_2 <- paste0(TEMP_DIR, OUTPUT_FN, " Cumulative.csv")

# Download the q5 data file. The file has data till the end of the prior year, 
# but may not have been updated as yet.
current_year  <- as.integer(format(Sys.Date(), "%Y"))
last_year     <- current_year - 1
two_years_ago <- current_year - 2

url_last_year     <- paste0(Q5_URL, QFACTOR_FN, last_year, ".csv")
url_two_years_ago <- paste0(Q5_URL, QFACTOR_FN, two_years_ago, ".csv")

tryCatch(X1 <- read.csv(url_last_year),
         error = function(e) print(paste(url_last_year, 'did not work out')))
tryCatch(X2 <- read.csv(url_two_years_ago),
         error = function(e) print(paste(url_two_years_ago, 'did not work out')))


if(!(exists("X1") | exists("X2")) ){
  # Could not read data - website may be experiencing difficulties
  stop("No data was read from global-q.org")
} else if(!exists("X1")){
    if(object.size(X2) > 5000){ 
      df_all <- X2
    }
  
} else if(!exists("X2")){
    if(object.size(X1) > 5000){ 
      df_all <- X1
    }
  
} else if((object.size(X1) < 5000) & (object.size(X2) < 5000)){ 
  # Read mininal data - website may be experiencing difficulties
  stop("No data was read from global-q.org")
  
} else if(object.size(X1) > object.size(X2)){
  writeLines("Prior year data is available.")
  df_all <- X1
  
} else {
  writeLines("Prior year data is unavailable. Defaulting to two year old data")
  df_all <- X2
  
}

if(WRITE_Q5_DATA){
  # Write out the raw monthly returns in the authors' own format
  write.csv(df_all, DATA_fn_1, row.names = FALSE)
}

# Create a new date column, remove the year and month columns in df_all
# then turn all returns into gross returns
df_dt      <- data.frame(Date = rep(NA, nrow(df_all)))
df_dt$Date <- zoo::as.yearmon(paste0(df_all$month, "-1-", df_all$year ), format = "%m-%d-%Y")
df_all       <- 1 + df_all[ , !(names(df_all) %in% c("year", "month"))] / 100
df_all       <- cbind(df_dt, df_all)

# Create the corresponding dataframes for the first, second and third phases
df1 <- df_all[df_all$Date <= PHASE_1_END,  ]
df2 <- df_all[(df_all$Date > PHASE_1_END) & (df_all$Date <= PHASE_2_END),  ]
df3 <- df_all[df_all$Date  > PHASE_2_END,  ]

# To start the cumulation, create an initial row of 1's starting in the
# prior month, then compute the cumulative product of the gross returns
df_dt      <- df_all[1, ]
df_dt[1, ] <- c(df_dt$Date[1] - 1/12, rep(1, 6) )
df_all     <- rbind(df_dt, df_all)
df_all_cum <- cbind(df_all["Date"], cumprod(df_all[,  (names(df_all) != "Date")]))

# Cumulate Phase 1 returns
df_dt      <- df1[1, ]
df_dt[1, ] <- c(df_dt$Date[1] - 1/12, rep(1, 6) )
df1        <- rbind(df_dt, df1)
df1_cum    <- cbind(df1["Date"], cumprod(df1[,  (names(df1) != "Date")]))

# Cumulate Phase 2 returns
df_dt      <- df2[1, ]
df_dt[1, ] <- c(df_dt$Date[1] - 1/12, rep(1, 6) )
df2        <- rbind(df_dt, df2)
df2_cum    <- cbind(df2["Date"], cumprod(df2[,  (names(df2) != "Date")]))

# Cumulate Phase 3 returns
df_dt      <- df3[1, ]
df_dt[1, ] <- c(df_dt$Date[1] - 1/12, rep(1, 6) )
df3        <- rbind(df_dt, df3)
df3_cum    <- cbind(df3["Date"], cumprod(df3[,  (names(df3) != "Date")]))

if(WRITE_Q5_DATA){
  # Write out the cumulative gross returns
  write.csv(df_all_cum, DATA_fn_2, row.names = FALSE)
}

# Create a ggplot of the cumulative returns of the q5 model factors. 
# Use the new R 4.2 pipe to melt the dataframe
df1_plot <- df1_cum |> 
              reshape2::melt(id.vars =  "Date", variable.name = "Factors")
df2_plot <- df2_cum |> 
              reshape2::melt(id.vars =  "Date", variable.name = "Factors")
df3_plot <- df3_cum |> 
              reshape2::melt(id.vars =  "Date", variable.name = "Factors")
df_all_plot <- df_all_cum |> 
                reshape2::melt(id.vars =  "Date", variable.name = "Factors")

# Select colors and line types
line_colors <- c("orangered2", "green3", "darkslategrey", 
                 "burlywood4", "purple", "goldenrod3")
line_types  <- c("solid",  "longdash", "dashed", 
                 "dotted", "longdash", "solid")

# Delete the existing ggplots if they exist, then recreate then
# Create a ggplot for the first period, then output a png file
plt <- ggplot(data = df1_plot, aes(x = Date, y = value)) + 
        geom_line(aes(color = Factors, linetype = Factors), linewidth = 1.25) +
        scale_linetype_manual(values = line_types) +
        scale_color_manual(values = line_colors) +
        scale_y_continuous(limits = c(0.5, 64), 
                           breaks = c(0.5, 2^c(0:6)),
                           trans = "log2") +
        labs(title = "1/1967 to 12/1999" , x = "Date", y = "Cumulative Gross Return") +
        theme_bw() +         
        theme(plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold"),
              axis.title.y = element_text(size = 18, face = "bold"),
              axis.text.x  = element_text(size = 16, angle=90, vjust=0.5,
                                          margin = margin(t = 10)),
              axis.text.y  = element_text(size = 16, angle=0,  hjust=0.5,
                                          margin = margin(r = 10)),
              legend.text  = element_text(size = 16,
                                         margin = margin(t = 5, b=10)),
              legend.title = element_text(size = 18))

# Need to show(plt) to make it visible in RStudio
show(plt)

# Create a ggplot for the second phase, then output a png file
plt <- ggplot(data = df2_plot, aes(x = Date, y = value)) + 
        geom_line(aes(color = Factors, linetype = Factors), linewidth = 1.25) +
        scale_linetype_manual(values = line_types) +
        scale_color_manual(values = line_colors) +
        scale_y_continuous(trans = "log2") +
        labs(title = "1/2000 to 12/2009", x = "Date", y = "Cumulative Gross Return") +
        theme_bw() +         
        theme(plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold"),
              axis.title.y = element_text(size = 18, face = "bold"),
              axis.text.x  = element_text(size = 16, angle=90, vjust=0.5,
                                          margin = margin(t = 10)),
              axis.text.y  = element_text(size = 16, angle=0,  hjust=0.5,
                                          margin = margin(r = 10)),
              legend.text  = element_text(size = 16,
                                          margin = margin(t = 5, b=10)),
              legend.title = element_text(size = 18))

# Need to show(plt) to make it visible in RStudio
show(plt)

# Create a ggplot for the third phase, then output a png file
plt <- ggplot(data = df3_plot, aes(x = Date, y = value)) + 
        geom_line(aes(color = Factors, linetype = Factors), linewidth = 1.25) +
        scale_linetype_manual(values = line_types) +
        scale_color_manual(values = line_colors) +
        scale_y_continuous(trans = "log2") +
        labs(title = "1/2010 onward", x = "Date", y = "Cumulative Gross Return") +
        theme_bw() +         
        theme(plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold"),
              axis.title.y = element_text(size = 18, face = "bold"),
              axis.text.x  = element_text(size = 16, angle=90, vjust=0.5,
                                          margin = margin(t = 10)),
              axis.text.y  = element_text(size = 16, angle=0,  hjust=0.5,
                                          margin = margin(r = 10)),
              legend.text  = element_text(size = 16,
                                          margin = margin(t = 5, b=10)),
              legend.title = element_text(size = 18))

# Need to show(plt) to make it visible in RStudio
show(plt)

# Create a ggplot for the entire period, then output a png file
plt <- ggplot(data = df_all_plot, aes(x = Date, y = value)) + 
        geom_line(aes(color = Factors, linetype = Factors), linewidth = 1.25) +
        scale_linetype_manual(values = line_types) +
        scale_color_manual(values = line_colors) +
        scale_y_continuous(limits = c(0.5, 256), 
                           breaks = c(0.5, 2^c(0:8)),
                           trans = "log2") +
        labs(title = "Entire Period", x = "Date", y = "Cumulative Gross Return") +
        theme_bw() +         
        theme(plot.title   = element_text(size = 20, face = "bold", hjust = 0.5),
              axis.title.x = element_text(size = 18, face = "bold"),
              axis.title.y = element_text(size = 18, face = "bold"),
              axis.text.x  = element_text(size = 16, angle=90, vjust=0.5,
                                          margin = margin(t = 10)),
              axis.text.y  = element_text(size = 16, angle=0,  hjust=0.5,
                                          margin = margin(r = 10)),
              legend.text  = element_text(size = 16,
                                          margin = margin(t = 5, b=10)),
              legend.title = element_text(size = 18))

# Need to show(plt) to make it visible in RStudio
show(plt)

# Figure 11.10
# The credit risk factor will finally be part of the PCRA dataset
# Keep returns and yields, drop the rest.
df_credit <- read.csv(paste0(TEMP_DIR, CREDIT_FN))
df_credit <- df_credit[, colnames(df_credit) %in% 
                         c("Date", 
                           "C110_Total_Return", "C110_YTM",
                           "G1O2_Total_Return", "G1O2_YTM")]
df_credit$Date   <- zoo::as.yearmon(df_credit$Date, format = "%m/%d/%Y")
df_credit$Spread <- df_credit$C110_YTM - df_credit$G1O2_YTM

# Drop the first two rows, which have NAs and start with a row of zeros
# Convert all returns and yields to decimals.
# When computing cumulative spread returns, divide annual spread by 12
df_credit <- tail(df_credit, -2)
df_credit[1, colnames(df_credit) != "Date"] <- 0
df_credit[,  colnames(df_credit) != "Date"] <- df_credit[, colnames(df_credit) != "Date"] / 100
df_credit[, "C110_Cum_Return"] <- cumprod(1 + df_credit[, "C110_Total_Return"])
df_credit[, "G1O2_Cum_Return"] <- cumprod(1 + df_credit[, "G1O2_Total_Return"])
df_credit[, "Credit_Factor_Return"] <- df_credit[, "C110_Cum_Return"] /
                                       df_credit[, "G1O2_Cum_Return"] - 1
df_credit[, "Cumulative_Spread"]    <- cumprod(1 + df_credit[, "Spread"] / 12) - 1

# Create a ggplot of the sread. 
# Use the new R 4.2 pipe to melt the dataframe
df_credit_melt <- df_credit[, c("Date", "Credit_Factor_Return",
                              "Spread", "Cumulative_Spread")] |> 
                    reshape2::melt(id.vars =  "Date", 
                                   variable.name = "High_Grade_Credit")

# To plot the spread on the second axis, scale it by 5
df_credit_melt$value <- ifelse(df_credit_melt$High_Grade_Credit =="Spread", 
                               df_credit_melt$value*5,  df_credit_melt$value)

plt <- ggplot(data = df_credit_melt, aes(x = Date, y = value)) + 
        geom_line(aes(color = High_Grade_Credit, linetype = High_Grade_Credit)) +
        labs(x = "Date", y = "Cumulative Return / Cumulative Spread") +
        scale_y_continuous(limits = c(0, 0.4), 
                           sec.axis = sec_axis(~ . / 5, name = "Spread")) +
        theme_bw()



# Need to show(plt) to make it visible in RStudio
show(plt)

# Figures 11 and 12 are copied directly from the source and we 
# have obtained permission to include them in this textbook
# The data that underlies them is not available to the public

# Figure 14
d_t <- ANN_DIVIDEND + ANN_REPURCHASE
k_e <- TSY_YLD_10YR + EQUITY_PREMIUM
mv  <- MARKET_VALUE

df_Apple <- data.frame(Expected_Life = seq(10, 200, 10))
df_Apple["g"] <- apply(df_Apple["Expected_Life"], 1, growthrate, 
                       d_t = d_t, k_e = k_e, mv = mv)
plt <- ggplot(data = df_Apple, aes(x = g, y = Expected_Life)) + 
        geom_line() +
        labs(x = "Long Term Growth Rate", y = "Expected Life") +
        theme_bw()

show(plt)


# Figure 15
capedata <- read.csv(paste0(TEMP_DIR, SP_DATA_FN))

# Create the 10 year nominal forward return
capedata$SP500Nom10YrFwdRet <- NA
capedata$SP500Nom10YrFwdRet <- c((rollapply(1 + capedata$SP500Nom1YrFwdRet, 10, 
                                            FUN = prod )) ^ (1/10) - 1, 
                               rep(NA, times = 9))
# The forecast horizon (N years) is set in the settings, and as we 
# will need to evaluate N year forecasts, we create a N year forward return
capedata$SP500NomNYrFwdRet <- NA 
capedata$SP500NomNYrFwdRet <- c( (zoo::rollapply(1 + capedata$SP500Nom1YrFwdRet, 
                                  FCST_HORIZON, FUN = prod )) ^ (1/FCST_HORIZON) - 1, 
                                  rep(NA, times = (FCST_HORIZON - 1) ) )


# Create the Forward Earnings Yield E_t+1/P and its square
capedata$Fwd_EY    <- lead(capedata$SP500EpsAll4Q, 1 ) / capedata$SP500PriceClose
capedata$Fwd_EY_sq <- capedata$Fwd_EY^2

# Create linear and quadratic fits for 1-year forward S&P return
fit_1_lin  <- lmrobdetMM(capedata$SP500Nom1YrFwdRet ~ capedata$Fwd_EY)
fit_1_quad <- lmrobdetMM(capedata$SP500Nom1YrFwdRet ~ capedata$Fwd_EY + capedata$Fwd_EY_sq)

# coeff_1_lin <- round(coef(fit_1_lin),4)
# rsq_1_lin   <- round(summary(fit_1_lin)$r.squared,4)
#
# coeff_1_quad <- round(coef(fit_1_quad),4)
# rsq_1_quad   <- round(summary(fit_1_quad)$r.squared,4)

# Use "predict" function to calculate estimated values as abline doesn't work
capedata$predict_ret_1_lin  <- predict(fit_1_lin, capedata) 
capedata$predict_ret_1_quad <- predict(fit_1_quad, capedata)

# Create linear and quadratic fits for 10-year forward S&P return
fit_10_lin  <- lmrobdetMM(capedata$SP500Nom10YrFwdRet ~ capedata$Fwd_EY)
fit_10_quad <- lmrobdetMM(capedata$SP500Nom10YrFwdRet ~ capedata$Fwd_EY + capedata$Fwd_EY_sq)

# coeff_10_lin <- round(coef(fit_10_lin),4)
# rsq_10_lin   <- round(summary(fit_10_lin)$r.squared,4)
# 
# coeff_10_quad <- round(coef(fit_10_quad),4)
# rsq_10_quad   <- round(summary(fit_10_quad)$r.squared,4)

# Use "predict" function to calculate estimated values as abline doesn't work
capedata$predict_ret_10_lin  <- predict(fit_10_lin, capedata) 
capedata$predict_ret_10_quad <- predict(fit_10_quad, capedata)

plt <- ggplot(data = capedata, aes(x = Fwd_EY, y = SP500Nom1YrFwdRet)) +
        geom_point(color = "darkolivegreen4", size = 4) +
        labs(x = expression( frac(italic(E)[italic(t)+1], italic(P)[t]) ), 
             y = "1 Year Forward Nominal Return") +
        geom_line(aes(x = Fwd_EY, y = predict_ret_1_lin),  color = "black",      linewidth = 1.5) +
        geom_line(aes(x = Fwd_EY, y = predict_ret_1_quad), color = "firebrick1", linewidth = 1.5) +
        theme_bw() +
        theme(axis.title.x = element_text(size = 22, face = "bold"),
              axis.title.y = element_text(size = 22, face = "bold"),
              axis.text.x  = element_text(size = 20),
              axis.text.y  = element_text(size = 20))
show(plt)

png_fn <- paste0(TEMP_DIR, "SP500_1YrVsFwdEY.png")

# Start the png driver
png(filename = png_fn, width = 480, height = 480, units = "px",
    pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"), antialias = "d") 
  print(plt) 
dev.off()

plt <- 	ggplot(data = capedata, aes(x = Fwd_EY, y = SP500Nom10YrFwdRet)) +
          geom_point(color = "darkolivegreen4", size = 4) +
          labs(x = expression( frac(italic(E)[italic(t)+1], italic(P)[t]) ), 
               y = "10 Year Forward Nominal Return (Annualized)") +
          geom_line(aes(x = Fwd_EY, y = predict_ret_10_lin),  color = "black",      linewidth = 1.5) +
          geom_line(aes(x = Fwd_EY, y = predict_ret_10_quad), color = "firebrick1", linewidth = 1.5) +
          theme_bw() +
          theme(axis.title.x = element_text(size = 22, face = "bold"),
                axis.title.y = element_text(size = 22, face = "bold"),
                axis.text.x  = element_text(size = 20),
                axis.text.y  = element_text(size = 20))
show(plt)

png_fn <- paste0(TEMP_DIR, "SP500_10YrVsFwdEY.png")

# Start the png driver
png(filename = png_fn, width = 480, height = 480, units = "px",
    pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"), antialias = "d") 
  print(plt) 
dev.off()


# Figure 16
capedata["EPS_Over_CPI"] <- capedata["SP500EpsAll4Q"] / capedata["CPIAUCNS"]
capedata["EPS_Over_GDP"] <- capedata["SP500EpsAll4Q"] / capedata["GDPA"]
capedata["EPS_Over_Rev"] <- capedata["SP500EpsAll4Q"] / capedata["SP500RevenuePS"]

capedata["EPS_Over_CPI"] <- normalize_to_1(capedata["EPS_Over_CPI"])
capedata["EPS_Over_GDP"] <- normalize_to_1(capedata["EPS_Over_GDP"])
capedata["EPS_Over_Rev"] <- normalize_to_1(capedata["EPS_Over_Rev"])

df_cape_melt <- capedata[, c("Year", "EPS_Over_CPI", 
                             "EPS_Over_GDP", "EPS_Over_Rev")] |>
                  reshape2::melt(id.vars = "Year", variable.name = "Earnings_Growth") 

plt <- ggplot(data = df_cape_melt, aes(x = Year, y = value)) + 
        geom_line(aes(color = Earnings_Growth), linewidth = 1) +
        scale_y_continuous(limits = c(0.0625, 8), 
                           breaks = c(2^c(-4:3)),
                           trans = "log2") +
        labs(x = "Year", y = "Ratio", 
             color ="Cumulative Growth in Earnings vs CPI, GDP and Revenues") +
        theme_bw() +
        theme(legend.position = c(0.45, 0.90),
              legend.background = element_blank(),
              legend.box.background = element_blank(),
              legend.key   = element_blank(),
              legend.title = element_text(size = 16),
              legend.text  = element_text(size = 14),
              axis.title.x = element_text(size = 14, hjust = 0.5,
                                          margin = margin(t = 5)),
              axis.title.y = element_text(size = 14, vjust = 0.5,
                                          margin = margin(r = 20)),
              axis.text.x  = element_text(size = 13, angle=90, vjust=0.5,
                                          margin = margin(t = 10)),
              axis.text.y  = element_text(size = 13, angle=0,  hjust=0.5,
                                          margin = margin(r = 10)))
# Need to show(plt) to make it visible in RStudio
show(plt)

png_fn <- paste0(TEMP_DIR, "SP500_EPSvsCPI_GDP_REV.png")

# Start the png driver
png(filename = png_fn, width = 650, height = 480, units = "px",
    pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"), antialias = "d") 
  print(plt) 
dev.off()


# Table 3
# We will do just the earnings portion here. Generating the 
# corresponding results for operating earnings is left as an exercise

capedata$E1Ratio <- capedata$SP500EpsBest1Q / lag(capedata$SP500EpsBest1Q, 1)
capedata$E2Ratio <- capedata$SP500EpsBest2Q / lag(capedata$SP500EpsBest2Q, 1)
capedata$E3Ratio <- capedata$SP500EpsBest3Q / lag(capedata$SP500EpsBest3Q, 1)
capedata$E4Ratio <- capedata$SP500EpsAll4Q  / lag(capedata$SP500EpsAll4Q,  1)

capedata$E4_next_E1Ratio   <- capedata$SP500EpsAll4Q / lag(capedata$SP500EpsBest1Q, 1)
capedata$E4_next_E2Ratio   <- capedata$SP500EpsAll4Q / lag(capedata$SP500EpsBest2Q, 1)
capedata$E4_next_E3Ratio   <- capedata$SP500EpsAll4Q / lag(capedata$SP500EpsBest3Q, 1)
capedata$E4_next_E4Ratio   <- capedata$SP500EpsAll4Q / lag(capedata$SP500EpsAll4Q,  1)

# Statistics of Ex_{t or  t+1} / En_{t}
col_names <- c("$N_{Obs}$", "$\\overline{x}$", "$\\sigma_{x}$", 
               "Min", "Max", "Skew", "Kurtosis")

row_names <- c("$E_{t+1}/E_{t}$",    "$E3_{t+1}/E3_{t}$", 
               "$E2_{t+1}/E2_{t}$",  "$E1_{t+1}/E1_{t}$", 
               "$E_{t+1}/E3_{t}$",   "$E_{t+1}/E2_{t}$",  "$E_{t+1}/E1_{t}$")

Earnings  <- c("E4Ratio",  "E3Ratio",  "E2Ratio",  "E1Ratio",
               "E4_next_E3Ratio", "E4_next_E2Ratio", "E4_next_E1Ratio")

summ <- subset(describe(capedata[Earnings]),
               select = -c(vars, median, trimmed, range, mad, se) )
summ[ , c(2,3)] <- round(summ[ , c(2,3)], 3) 
summ[ , c(4:5)] <- round(summ[ , c(4:5)], 2)
summ[ , c(6:7)] <- round(summ[ , c(6:7)], 1)

colnames(summ) <- col_names
rownames(summ) <- row_names
print(summ)


# Table 3
# Create cyclically adjusted earnings and valuation ratios
capedata$SP500CAE  <- c(rep(NA, times = 9), 
                        zoo::rollmean(capedata$SP500EpsAll4Q / 
                                      capedata$CPIAUCNS, 10) * 
                        capedata$CPIAUCNS[10: dim(capedata)[1]])

capedata$E3P  <- capedata$SP500EpsBest3Q / capedata$SP500PriceClose
capedata$OEP  <- capedata$SP500OperatingEPS / capedata$SP500PriceClose
capedata$CAEP <- capedata$SP500CAE / capedata$SP500PriceClose
capedata$SP   <- capedata$SP500RevenuePS / capedata$SP500PriceClose

# Create squared valuation ratios for later use
capedata$E3P2  <- capedata$E3P^2
capedata$OEP2  <- capedata$OEP^2
capedata$CAEP2 <- capedata$CAEP^2
capedata$SP2   <- capedata$SP^2

# Now create the robust fits
RobFitE3P  <- lmrobdetMM(SP500Nom10YrFwdRet ~ E3P,  capedata)
RobFitSP   <- lmrobdetMM(SP500Nom10YrFwdRet ~ SP,   capedata)
RobFitOEP  <- lmrobdetMM(SP500Nom10YrFwdRet ~ OEP,  capedata)
RobFitCAEP <- lmrobdetMM(SP500Nom10YrFwdRet ~ CAEP, capedata)

# lmrobdetMM doesn't make it easy to extract information!
# We get what we need from the 9th item of the summary (a list),
# which we convert to a dataframe
summE3P  <- as.data.frame(summary(RobFitE3P)[9])
summSP   <- as.data.frame(summary(RobFitSP)[9])
summOEP  <- as.data.frame(summary(RobFitOEP)[9])
summCAEP <- as.data.frame(summary(RobFitCAEP)[9])

predictor  <- c("$E3_{t}/P_{t}$", "$S_{t}/P_{t}$", 
                "$OE_{t}/P_{t}$", "$\\nicefrac{1}{CAPE_{t}}$")

firstYr    <- capedata[c( min(which(!is.na(capedata$E3P ))),
                          min(which(!is.na(capedata$SP  ))),
                          min(which(!is.na(capedata$OEP ))),
                          min(which(!is.na(capedata$CAEP)))
                        ), "Year"]

coeffs     <- as.data.frame(rbind(summE3P[, 1], summSP[, 1], 
                               summOEP[, 1], summCAEP[, 1]) )

fcst_10    <- c(coeffs[1,1] + coeffs[1,2] * tail(capedata, 1)[["E3P"]],
                coeffs[2,1] + coeffs[2,2] * tail(capedata, 1)[["SP"]],
                coeffs[3,1] + coeffs[3,2] * tail(capedata, 1)[["OEP"]],
                coeffs[4,1] + coeffs[4,2] * tail(capedata, 1)[["CAEP"]])

scaled_t   <- c(summE3P[2, 3], summSP[2, 3], 
                summOEP[2, 3], summCAEP[2, 3]) / sqrt(10)

df_predict <- cbind(predictor, firstYr, coeffs, fcst_10, scaled_t)
rownames(df_predict) <- c("$E3_{t}/P_{t}$", "$S_{t}/P_{t}$", 
                          "$OE_{t}/P_{t}$", "$\\nicefrac{1}{CAPE_{t}}$")
colnames(df_predict) <-c("Predictor", "Initial Year", 
                         "$\\alpha_{10\\ yr}$", "$\\beta_{10\\ yr}$", 
                         "$\\hat{r}_{t,\\ t\\,+\\,10}$",
                         "$\\frac{t\\left(\\beta\\right)_{10\\ yrs}}{\\sqrt{10}}$")

kable(df_predict, format = "latex", booktabs = T, linesep = "", row.names = FALSE,
      align = c("l", "c", "r", "c", "r", "c", "c", "c", "c", "c"), escape = FALSE) %>%
      kable_styling("striped", full_width = F, font_size = 10 ) %>%
        column_spec( 1,  width = "5.5em") %>%
        column_spec( 2,  width = "5em") %>%
        column_spec( 3,  width = "4em") %>% 
        column_spec( 4,  width = "4em") %>%
        column_spec( 5,  width = "4em") %>%
        column_spec( 6,  width = "4em") %>%
        add_indent(1:4) %>%
        footnote(general = c("Professor Robert Shiller, S&P Capital IQ Analyst’s Handbook", 
                             "Dow Jones S&P Indices, Authors’ own calculations"), 
                             general_title = "Source:")


# Figures 17-20
######## SECTION 2: In-sample linear and quadratic fits ###########
# x1, x2 and xlabels are the same for all regressions
x1 <- c("E3P",  "CAEP",  "SP" )
x2 <- c("E3P2", "CAEP2", "SP2")
xlabels  <- c("Best 3 Quarters E/P", "CAE/P", "All 4 Quarters S/P")

# One and ten year y-variables and y labels are the same for all regressions
y1 <- rep("SP500Nom1YrFwdRet", length(x1))
yN <- rep("SP500NomNYrFwdRet", length(x1))

ylabels1 <- rep("1-Year Forward Return: S&P 500", length(x1) )
ylabelsN <- rep( paste0(FCST_HORIZON, "-Year Forward Return: S&P 500"), length(x1) )

# Plot titles: Linear Fit
plt_titles_1_lin <- c( "In-Sample: 1-yr Return vs E3/P",
                       "In-Sample: 1-yr Return vs CAE/P",
                       "In-Sample: 1-yr Return vs S/P"
                     )

plt_titles_N_lin <- c( paste0("In-Sample: ", FCST_HORIZON, "-yr Return vs E3/P"),
                       paste0("In-Sample: ", FCST_HORIZON, "-yr Return vs CAE/P"),
                       paste0("In-Sample: ", FCST_HORIZON, "-yr Return vs S/P")
                     )

# Linear in-sample fits: 1 and FCST_HORIZON year returns with valuation ratio on x-axis
# in_sample_plots(capedata, x1, x2, y1, xlabels, ylabels1, plt_titles_1_lin, linear = TRUE)
in_sample_plots(capedata, x1, x2, yN, xlabels, ylabelsN, plt_titles_N_lin, linear = TRUE)

# Plot titles: Quadratic Fit
plt_titles_1_quad  <- c("In-Sample: 1-yr Return vs E3/P & E2/P^2",
                        "In-Sample: 1-yr Return vs CAE/P & CAE/P^2",
                        "In-Sample: 1-yr Return vs S/P & S/P^2")

plt_titles_N_quad <- c(paste0("In-Sample: ", FCST_HORIZON, "-yr Return vs E3/P & E3/P^2"),
                       paste0("In-Sample: ", FCST_HORIZON, "-yr Return vs CAE/P & CAE/P^2"),
                       paste0("In-Sample: ", FCST_HORIZON, "-yr Return vs S/P & S/P^2"))

# Quadratic in-sample fits: 1 and FCST_HORIZON year returns
# in_sample_plots(capedata, x1, x2, y1, xlabels, ylabels1, plt_titles_1_quad, linear = FALSE)
in_sample_plots(capedata, x1, x2, yN, xlabels, ylabelsN, plt_titles_N_quad, linear = FALSE)

# Linear in-sample fits: 1 and FCST_HORIZON year returns with fitted return on x-axis
# xlabels1 <- rep(paste0("Fitted 1-Year Forward Return: S&P 500"), length(x1))
xlabelsN <- rep(paste0("Fitted ",FCST_HORIZON, "-Year Forward Return: S&P 500"), length(x1))
ylabelsN <- rep(paste0("Actual ",   FCST_HORIZON, "-Year Forward Return: S&P 500"), length(x1))

x1 <- c("E3P",  "CAEP",  "SP")
x2 <- c("E3P2", "CAEP2", "SP2")

# in_sample_plots_vs_forecast(capedata, x1, x2, y1, xlabels1, ylabels1, plt_titles_1_lin, linear = TRUE)
in_sample_plots_vs_forecast(capedata, x1, x2, yN, xlabelsN, ylabelsN, plt_titles_N_lin, linear = TRUE)

# in_sample_plots_vs_forecast(capedata, x1, x2, y1, xlabels1, ylabels1, plt_titles_1_quad, linear = FALSE)
in_sample_plots_vs_forecast(capedata, x1, x2, yN, xlabelsN, ylabelsN, plt_titles_N_quad, linear = FALSE)

writeLines("Finished Section 2")

######## SECTION 3: Out-of-sample linear and quadratic fits ###########
# Don't bother with 1 year returns, do only N year returns
# Reuse y variables from in-sample regressions
# Remove OE/P from x1 and x2 as the history is too short

x1 <- c("E3P",  "CAEP", "SP")
x2 <- c("E3P2", "CAEP2","SP2" )

# Create x and y labels, which are the same for all regressions
xlabelsN <- rep(paste0("Predicted ",FCST_HORIZON, "-Year Forward Return: S&P 500"), length(x1))
ylabelsN <- rep( paste0("Actual ",  FCST_HORIZON, "-Year Forward Return: S&P 500"), length(x1))

# Plot titles: Linear Fit
plt_titlesN <- c(paste0("Out-of-Sample: ", FCST_HORIZON, "-yr Return vs E3/P"),
                 paste0("Out-of-Sample: ", FCST_HORIZON, "-yr Return vs CAE/P"),
                 paste0("Out-of-Sample: ", FCST_HORIZON, "-yr Return vs S/P") )

# Create and save linear out-of-sample fits: FCST_HORIZON year returns
capedata <- out_of_sample_plots(capedata, x1, x2, yN, xlabelsN, ylabelsN, plt_titlesN, linear = TRUE)

# Plot titles: Quadratic Fit
plt_titlesN <- c(paste0("Out-of-Sample: ", FCST_HORIZON, "-yr Return vs E3/P & E3/P^2"),
                 paste0("Out-of-Sample: ", FCST_HORIZON, "-yr Return vs CAE/P & CAE/P^2"),
                 paste0("Out-of-Sample: ", FCST_HORIZON, "-yr Return vs S/P & S/P^2") 
                )

# Create and save quadratic out-of-sample fits: FCST_HORIZON year returns
capedata <- out_of_sample_plots(capedata, x1, x2, yN, xlabelsN, ylabelsN, plt_titlesN, linear = FALSE)


writeLines("Finished Section 3")
######## SECTION 4: Combined Out-of-sample fits ###########
# Don't bother with 1 year returns, do only N year returns
# Choose the two predictors and their foercast errors  for use when combining
pred1  <- c("lin_fcst_E3P", "quad_fcst_E3P", "lin_fcst_CAEP", "quad_fcst_CAEP")
pred2  <- c("lin_fcst_SP",  "quad_fcst_SP",  "lin_fcst_SP",  "quad_fcst_SP" )

error1 <- c("lin_error_E3P", "quad_error_E3P", "lin_error_CAEP", "quad_error_CAEP")
error2 <- c("lin_error_SP",  "quad_error_SP",  "lin_error_SP",   "quad_error_SP")

# Create new a y array, whose entries are the same for all regressions
# This is overkill, but allows us to test different time horizons etc. by
# having different return series for each of the regressions
yN <- rep("SP500NomNYrFwdRet", length(pred1))

# Create x1 and y labels, which are the same for all regressions
xlabelsN <- rep(paste0("Predicted ",FCST_HORIZON, "-Year Forward Return: S&P 500"), length(x1))
ylabelsN <- rep(paste0("Actual ",   FCST_HORIZON, "-Year Forward Return: S&P 500"), length(x1))

# First compute equal weighted additive forecasts
plt_titles <- c("Equally Wtd. Additive Linear forecasts: E3/P & S/P",
                "Equally Wtd. Additive Quadratic forecasts: E3/P & S/P",
                "Equally Wtd. Additive Linear forecasts: CAE/P & S/P",
                "Equally Wtd. Additive Quadratic forecasts: CAE/P & S/P")

capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
                                     xlabelsN, ylabelsN, plt_titles, 
                                     weighting = 1, additive = TRUE)
writeLines("\n")


# plt_titles <- c("Equally Wtd. Multiplicative Linear forecasts: E3/P & S/P",
# # Next compute equal weighted multiplicative forecasts
#                 "Equally Wtd. Multiplicative Quadratic forecasts: E3/P & S/P",
#                 "Equally Wtd. Multiplicative Linear forecasts: CAE/P & S/P",
#                 "Equally Wtd. Multiplicative Quadratic forecasts: CAE/P & S/P")
#
# capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
#                                       xlabels, ylabelsN, plt_titles,
#                                       weighting = 1, additive = FALSE)
# writeLines("\n")


# Next compute 1/Error Variance weighted additive forecasts
plt_titles <- c("1/Error Var. Wtd. Additive Linear forecasts: E3/P & S/P",
                "1/Error Var. Wtd. Additive Quadratic forecasts: E3/P & S/P",
                "1/Error Var. Wtd. Additive Linear forecasts: CAE/P & S/P",
                "1/Error Var. Wtd. Additive Quadratic forecasts: CAE/P & S/P")

capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
                                     xlabelsN, ylabelsN, plt_titles,
                                     weighting = 2, additive = TRUE)
writeLines("\n")

# # Next compute 1/Error Variance weighted multiplicative forecasts
# plt_titles <- c("1/Error Var. Wtd. Multiplicative Linear forecasts: E3/P & S/P",
#                 "1/Error Var. Wtd. Multiplicative Quadratic forecasts: E3/P & S/P",
#                 "1/Error Var. Wtd. Multiplicative Linear forecasts: CAE/P & S/P",
#                 "1/Error Var. Wtd. Multiplicative Quadratic forecasts: CAE/P & S/P")
#
# capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
#                                      xlabelsN, ylabelsN, plt_titles,
#                                      weighting = 2, additive = FALSE)
# writeLines("\n")


# Next compute Second moment weighted additive forecasts
plt_titles <- c("Second moment Wtd. Additive Linear forecasts: E3/P & S/P",
                "Second moment Wtd. Additive Quadratic forecasts: E3/P & S/P",
                "Second moment Wtd. Additive Linear forecasts: CAE/P & S/P",
                "Second moment Wtd. Additive Quadratic forecasts: CAE/P & S/P")

capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
                                     xlabelsN, ylabelsN, plt_titles,
                                     weighting = 3, additive = TRUE)
writeLines("\n")


# # Next compute Second moment weighted multiplicative forecasts
# plt_titles <- c("Second moment Wtd. Multiplicative Linear forecasts: E3/P & S/P",
#                 "Second moment Wtd. Multiplicative Quadratic forecasts: E3/P & S/P",
#                 "Second moment Wtd. Multiplicative Linear forecasts: CAE/P & S/P",
#                 "Second moment Wtd. Multiplicative Quadratic forecasts: CAE/P & S/P")
#
# capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
#                                      xlabelsN, ylabelsN, plt_titles,
#                                      weighting = 3, additive = FALSE)
# writeLines("\n")


# Next compute Granger Ramanathan unconstrained additive forecasts
plt_titles <- c("Granger Ramanathan unconstrained Additive Linear forecasts: E3/P & S/P",
                "Granger Ramanathan unconstrained Additive Quadratic forecasts: E3/P & S/P",
                "Granger Ramanathan unconstrained Additive Linear forecasts: CAE/P & S/P",
                "Granger Ramanathan unconstrained Additive Quadratic forecasts: CAE/P & S/P")

capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
                                     xlabelsN, ylabelsN, plt_titles,
                                     weighting = 4, additive = TRUE)
writeLines("\n")


# Next compute QP based constrained additive forecasts
plt_titles <- c("Quadratic Programming with Linear forecasts: E3/P & S/P",
                "Quadratic Programming with Quadratic forecasts: E3/P & S/P",
                "Quadratic Programming with Linear forecasts: CAE/P & S/P",
                "Quadratic Programming with Quadratic forecasts: CAE/P & S/P")

capedata <- combined_out_of_sample_plots(capedata, pred1, pred2, error1, error2, yN,
                                     xlabelsN, ylabelsN, plt_titles,
                                     weighting = 5, additive = TRUE)
writeLines("\n")


# Finally compute multivariate forecasts
x_vars  <- list(c("E3P", "SP"), c("CAEP", "SP"), c("E3P", "SP", "E3P2", "SP2"), 
                c("CAEP", "SP", "CAEP2", "SP2"))
# Create new a y array, whose entries are the same for all regressions
# This is overkill, but allows us to test different time horizons etc. by
# having different return series for each of the regressions
yN <- rep("SP500NomNYrFwdRet", length(x_vars))

plt_titles <- c("Multivariate forecast: E3/P & S/P",
                "Multivariate forecast: CAE/P & S/P",
                "Multivariate forecast: E3/P, S/P, E3/P^2 & S/P^2",
                "Multivariate forecast: CAE/P, S/P, CAE/P^2 & S/P^2")
capedata <- composite_out_of_sample_plot(capedata, x_vars, yN, xlabelsN, ylabelsN, plt_titles)

writeLines("\n")


writeLines("Finished Section 4")

######## SECTION 5: Write the dataframe to an xlsx file  ###########
if(WRITE_DATAFRAME){
  #  write_xlsx(capedata, DF_FN, col_names = TRUE)
  write.xlsx(capedata, DF_FN, overwrite = WRITE_DATAFRAME)
}

####################################################################


# Figure 11.21
# Download the relevant data from FRED, then create the graph
df_list <- vector(mode = "list", length = 3)

chauvet_prob       <- as.data.frame(fredr("RECPROUSM156N"))
chauvet_prob$Date  <- zoo::as.yearmon(format(chauvet_prob$date,format = '%Y-%m-%d'))
chauvet_prob       <- chauvet_prob[complete.cases(chauvet_prob), c("Date", "value")]
chauvet_prob$value <- chauvet_prob$value * 0.15 # Scale to plot on the secondary axis
colnames(chauvet_prob) <- c("Date", "Chauvet-Piger Recession Probability")
df_list[[1]]      <- chauvet_prob

us_rec       <- as.data.frame(fredr("USREC"))
us_rec$Date  <- zoo::as.yearmon(format(us_rec$date,format = '%Y-%m-%d'))
us_rec       <- us_rec[complete.cases(us_rec), c("Date", "value")]
us_rec$value <- us_rec$value * 15 # Scale to plot on the secondary axis
colnames(us_rec) <- c("Date", "NBER Dated Recession, FRED series USREC")
df_list[[2]]     <- us_rec

un_rate       <- as.data.frame(fredr("UNRATE"))
un_rate$Date  <- zoo::as.yearmon(format(un_rate$date,format = '%Y-%m-%d'))
un_rate       <- un_rate[complete.cases(un_rate), c("Date", "value")]
colnames(un_rate) <- c("Date", "Unemployment Rate (U-3)")
df_list[[3]]      <- un_rate


df_chauvet <- Reduce(my_merge, df_list)

df_melt <- df_chauvet |> 
            reshape2::melt(id.vars = "Date", variable.name = "Variable")

plt <- ggplot(df_melt, aes(x = Date, y= value)) +
        geom_line(aes(color = Variable, linewidth = Variable)) +
        scale_color_manual(values = c("dark green", "azure3", "goldenrod4")) +
        scale_discrete_manual(aesthetic = "linewidth", values = c(.7, .7, .7)) +
        labs(x = "Date", y = "Unemployment Rate")  +
        theme_bw() +
        theme(legend.position = c(0.6, 0.8),
              legend.title    = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_blank(),
              legend.key = element_blank(),
              legend.text = element_text(size = 16),
              axis.title.x    = element_text(size = 14, hjust = 0.5,
                                             margin = margin(t = 5)),
              axis.title.y    = element_text(size = 14, vjust = 0.5,
                                             margin = margin(r = 20)),
              axis.title.y.right = element_text(size = 14, vjust = 0.5,
                                                margin = margin(l = 20)),
              axis.text.x  = element_text(size = 13, angle=90, vjust=0.5,
                                          margin = margin(t = 10)),
              axis.text.y  = element_text(size = 13, angle=0,  hjust=0.5,
                                          margin = margin(r = 10)),
              plot.margin  = unit(c(t = 0.25, r = 1, b = 0.25, l = 0.25), "cm")) +
        scale_x_continuous(n.breaks = 6) +
        scale_y_continuous(limits = c(0, 15),
                           sec.axis = sec_axis(~ . / 15,
                                               name = "USREC and Chauvet-Piger Probability"))
show(plt)

png_fn <- paste0(TEMP_DIR, "Chauvet-Piger Recession Probability.png")

# Start the png driver
png(filename = png_fn, width = 800, height = 480, units = "px",
    pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"), antialias = "d") 
  print(plt) 
dev.off()

# Table 11.8
# We earlier scaled USREC and the Chauvet-Piger Recession Probability by 15 so that they
# would plot correctly on the second y axis. Now scale them back to [0, 1]
df_chauvet[["NBER Dated Recession, FRED series USREC"]] <-
  df_chauvet[["NBER Dated Recession, FRED series USREC"]] / 15
df_chauvet[["Chauvet-Piger Recession Probability"]] <-
  df_chauvet[["Chauvet-Piger Recession Probability"]] / 15
us_rec[["NBER Dated Recession, FRED series USREC"]] <- 
  us_rec[["NBER Dated Recession, FRED series USREC"]] / 15

df_chauvet["USREC_start"] <- NA
df_chauvet["USREC_end"]   <- NA
df_chauvet["CPREC_start"] <- NA
df_chauvet["CPREC_end"]   <- NA

EXPANSION <- 0
RECESSION <- 1

j_USREC   <- 1
j_CPREC   <- 1

Nmonths_chauvet   <- dim(df_chauvet)[1]

# Find the first recession, and start from there
first_recession <- min(which(df_chauvet[["NBER Dated Recession, FRED series USREC"]] == 1))
cur_state       <- RECESSION
df_chauvet[1,"USREC_start"] <- df_chauvet[first_recession, "Date"]

for(i in (first_recession: Nmonths_chauvet) ){
  if( (cur_state == EXPANSION) &
      (df_chauvet[i, "NBER Dated Recession, FRED series USREC"] == 1) ){
    cur_state <- RECESSION
    df_chauvet[j_USREC,"USREC_start"] <- df_chauvet[i, "Date"]
    
  } else if( (cur_state == RECESSION) &
             (df_chauvet[i, "NBER Dated Recession, FRED series USREC"] == 0) ) {
    cur_state <- EXPANSION
    df_chauvet[j_USREC,"USREC_end"] <- df_chauvet[i, "Date"]
    j_USREC <- j_USREC + 1
    
  }
}


# Find the first recession identified by the Chauvet-Piger algorithm, and start from there
first_recession <- min(which(df_chauvet[["Chauvet-Piger Recession Probability"]] > 0.5))
cur_state       <- RECESSION
df_chauvet[1, "CPREC_start"] <- df_chauvet[first_recession, "Date"]

for(i in (first_recession:Nmonths_chauvet) ){
  if( (cur_state == EXPANSION) &
      (df_chauvet[i, "Chauvet-Piger Recession Probability"] >= 0.5) ){
    cur_state  <- RECESSION
    df_chauvet[j_CPREC,"CPREC_start"] <- df_chauvet[i, "Date"]
    
  } else if( (cur_state == RECESSION) &
             (df_chauvet[i, "Chauvet-Piger Recession Probability"] < 0.5) ) {
    cur_state  <- EXPANSION
    df_chauvet[j_CPREC,"CPREC_end"] <- df_chauvet[i, "Date"]
    j_CPREC <- j_CPREC + 1
    
  }
}

# Turn all the dates into yearmons
df_chauvet[, "USREC_start"] <- zoo::as.yearmon(df_chauvet[, "USREC_start"])
df_chauvet[, "USREC_end"]   <- zoo::as.yearmon(df_chauvet[, "USREC_end"])

df_chauvet[,"CPREC_start"] <- zoo::as.yearmon(df_chauvet[, "CPREC_start"])
df_chauvet[,"CPREC_end"]   <- zoo::as.yearmon(df_chauvet[, "CPREC_end"])

df_chauvet_print <- df_chauvet[1:(max(j_USREC, j_CPREC)-1),
                               c("USREC_start", "USREC_end", "CPREC_start", "CPREC_end") ]

print(df_chauvet_print)

# Figure 11.22
# Merge the un_rate and us_rec dataframes
#Thresholds to determine the start and end of a recession
THRESHOLD_START_HIGH <- 1.0
THRESHOLD_START_LOW  <- 0.29
THRESHOLD_DELTA      <- 0.14
THRESHOLD_END        <- 0.24
THRESHOLD_SAHM       <- 0.5

# USE_YIELD_CURVE_SLOPE is TRUE if we use 10Yr-1Yr slope. 
# If we ignore it, we can go back to 1949, otherwise start in 1953
USE_YIELD_CURVE_SLOPE <- TRUE
if(!USE_YIELD_CURVE_SLOPE) THRESHOLD_START_HIGH <- THRESHOLD_START_LOW

# Window sizes in months
WINDOW_LONG  <- 12
WINDOW_SHORT <- 4

# State of the economy
EXPANSION <- 0
RECESSION <- 1

# Minimum number of months that must elapse after the end of a recession
# before a new recession can start
MIN_TIME_TO_NEXT <- 1

# Baseline lag is the number of months prior to the current month that the
# baseline level of unemployment is computed
BASELINE_LAG <- 1

# Download data for the Sahm filter from FRED
sahm_filter       <- as.data.frame(fredr("SAHMCURRENT"))
sahm_filter$Date  <- zoo::as.yearmon(format(sahm_filter$date,format = '%Y-%m-%d'))
sahm_filter       <- sahm_filter[complete.cases(sahm_filter), c("Date", "value")]
colnames(sahm_filter) <- c("Date", "Sahm Filter")

if(USE_YIELD_CURVE_SLOPE){
  US_10Y        <- as.data.frame(fredr("GS10"))
  US_10Y$Date   <- zoo::as.yearmon(format(US_10Y$date,format = '%Y-%m-%d'))
  US_10Y        <- US_10Y[complete.cases(US_10Y), c("Date", "value")]
  colnames(US_10Y) <- c("Date", "10Y Constant Maturity Treasury")
  df_list[[4]]     <- US_10Y
  
  US_1Y         <- fredr("GS1")
  US_1Y$Date    <- zoo::as.yearmon(format(US_1Y$date,format = '%Y-%m-%d'))
  US_1Y         <- US_1Y[complete.cases(US_1Y), c("Date", "value")]
  colnames(US_1Y) <- c("Date", "1Y Constant Maturity Treasury")
  df_list[[5]]    <- US_1Y
  
}


df_list        <- vector(mode = "list", length = 5)
df_list[[1]]   <- us_rec
df_list[[2]]   <- un_rate
df_list[[3]]   <- sahm_filter
df_list[[4]]   <- US_10Y
df_list[[5]]   <- US_1Y
df_algorithm_1 <- Reduce(my_merge, df_list)

if(USE_YIELD_CURVE_SLOPE){
  df_algorithm_1[["10 Yr  - 1 Yr Slope"]] <- df_algorithm_1[["10Y Constant Maturity Treasury"]] -
                                             df_algorithm_1[["1Y Constant Maturity Treasury"]]
  # Now discard the 10 year and 1 year rates, keep only the slope
  df_algorithm_1 <- df_algorithm_1[, !grepl("Constant Maturity", colnames(df_algorithm_1))]
}

# Scale unemployment rate by 10 to make it easier to visualize
df_algorithm_1["Unemployment Rate (U-3)/10"]  <- df_algorithm_1["Unemployment Rate (U-3)"]/10

df_algorithm_1[["X_t"]] <- NA
df_algorithm_1[["Y_t"]] <- NA
df_algorithm_1[["delta_X_t"]] <- NA
df_algorithm_1[["delta_Y_t"]] <- NA
df_algorithm_1[["delta_U3"]] <- NA

# To identify recessions we want the comparator to sufficiently exceed the baseline
df_algorithm_1[["HLM_U-3"]]   <- zoo::rollapply(df_algorithm_1[["Unemployment Rate (U-3)"]],
                                                WINDOW_LONG,
                                                \(x) DescTools::HodgesLehmann(x, na.rm = FALSE),
                                                partial = TRUE, align = "right")

df_algorithm_1[["X_t"]]        <- df_algorithm_1[["Unemployment Rate (U-3)"]] -
                                  lag(df_algorithm_1[["HLM_U-3"]], BASELINE_LAG)
df_algorithm_1[1, "X_t"]       <- 0

df_algorithm_1[["delta_X_t"]]  <- df_algorithm_1[["X_t"]] - lag(df_algorithm_1[["X_t"]], 1)
df_algorithm_1[1, "delta_X_t"] <- 0

df_algorithm_1[["delta_U3"]]   <- df_algorithm_1[["Unemployment Rate (U-3)"]] -
                                  lag(df_algorithm_1[["Unemployment Rate (U-3)"]], 1)
df_algorithm_1[1, "delta_U3"]  <- 0

# To identify expansions we want X_t to decline from its local maximum
df_algorithm_1[["Y_t"]]        <- lag(zoo::rollapply(df_algorithm_1[["X_t"]], WINDOW_SHORT, max, 
                                                     partial = TRUE, align = "right"),  BASELINE_LAG) -
                                  df_algorithm_1[["X_t"]]
df_algorithm_1[1, "Y_t"]       <- 0

df_algorithm_1[["delta_Y_t"]]  <- df_algorithm_1[["Y_t"]] - lag(df_algorithm_1[["Y_t"]], 1)
df_algorithm_1[1, "delta_Y_t"] <- 0

Nmonths_Alg_1 <- dim(df_algorithm_1)[1]

# Initialize the state of the economy till the end of 1949
# to the actual state of the economy.
# The data set starts in the middle of the 1948-49 recession. We assume that 
# Algorithm 1 identified it and so match the first few months of real data
economy_in_recession <- rep(NA, Nmonths_Alg_1)
n_kept_out <- sum(df_algorithm_1[["Date"]] <= "Dec 1949")
economy_in_recession[1: n_kept_out] <- df_algorithm_1[1: n_kept_out, 
                                                      "NBER Dated Recession, FRED series USREC"]

df_algorithm_1["USREC_start"]    <- NA
df_algorithm_1["USREC_end"]      <- NA
df_algorithm_1["ALG1_REC_start"] <- NA
df_algorithm_1["ALG1_REC_end"]   <- NA
df_algorithm_1["SAHM_REC_start"] <- NA
df_algorithm_1[,"ALG1_REC_start_X_t"] <- NA
df_algorithm_1[,"ALG1_REC_start_Y_t"] <- NA
df_algorithm_1[,"ALG1_REC_start_Delta_X_t"] <- NA
df_algorithm_1[,"ALG1_REC_start_Delta_Y_t"] <- NA
df_algorithm_1[,"ALG1_REC_end_X_t"]   <- NA
df_algorithm_1[,"ALG1_REC_end_Y_t"]   <- NA
df_algorithm_1[,"ALG1_REC_end_Delta_X_t"]   <- NA
df_algorithm_1[,"ALG1_REC_end_Delta_Y_t"]   <- NA

j_ALG1_REC <- 1
df_algorithm_1[1,"ALG1_REC_start"] <- df_algorithm_1[1, "Date"]
THRESHOLD_START <- THRESHOLD_START_LOW

for (i in (MIN_TIME_TO_NEXT+1): Nmonths_Alg_1){
  if(economy_in_recession[i-1] == 0){ # Currently in an expansion
    if(USE_YIELD_CURVE_SLOPE){
      if(df_algorithm_1[i, "10 Yr  - 1 Yr Slope"] < 0){ # Slope has turned negative
        # Speed up the response to the recession signal
        THRESHOLD_START <- THRESHOLD_START_LOW      
      }
    }
    
    if((df_algorithm_1$X_t[i] > THRESHOLD_START) &
       (max(df_algorithm_1$delta_X_t[i],
            (df_algorithm_1$delta_X_t[i] + df_algorithm_1$delta_X_t[i-1])) > THRESHOLD_DELTA) &
       (sum(economy_in_recession[(i - MIN_TIME_TO_NEXT): (i-1)]) == 0) )  { # Recession
      # If X_t  crosses THRESHOLD_START and is increasing sufficiently fast, 
      # and it has been sufficiently long since the last recession,
      # we have entered a new recession. Reset the threshold for detection if needed
      economy_in_recession[i] <- 1
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_start"]     <- df_algorithm_1[i, "Date"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_start_X_t"] <- df_algorithm_1[i, "X_t"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_start_Y_t"] <- df_algorithm_1[i, "Y_t"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_start_Delta_X_t"] <- df_algorithm_1[i, "delta_X_t"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_start_Delta_Y_t"] <- df_algorithm_1[i, "delta_Y_t"]
      
      if(USE_YIELD_CURVE_SLOPE) {
        THRESHOLD_START <- THRESHOLD_START_HIGH
      }
      
    } else { # No change in state
      economy_in_recession[i] <- 0
      
    }
    
  } else if(economy_in_recession[i - 1] == 1){  # Currently in a recession
    if((df_algorithm_1$Y_t[i] > THRESHOLD_END) &
       (df_algorithm_1$delta_Y_t[i] > THRESHOLD_DELTA) ){
      # If Y_t crosses THRESHOLD_END and is declining sufficiently fast 
      # we have started a new expansion
      economy_in_recession[i] <- 0
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_end"]     <- df_algorithm_1[i, "Date"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_end_X_t"] <- df_algorithm_1[i, "X_t"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_end_Y_t"] <- df_algorithm_1[i, "Y_t"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_end_Delta_X_t"] <- df_algorithm_1[i, "delta_X_t"]
      df_algorithm_1[j_ALG1_REC, "ALG1_REC_end_Delta_Y_t"] <- df_algorithm_1[i, "delta_Y_t"]
      
      # Increment j_ALG1_REC to prepare for the next cycle
      j_ALG1_REC <- j_ALG1_REC + 1
      
    } else { # No change in state
      economy_in_recession[i] <- 1
      
    }
    
  } #if (economy_in_recession[i-1] == 0)
} # for (i in (MIN_TIME_TO_NEXT+1): Nmonths_Alg_1)

df_algorithm_1["Algorithm 1"] <- economy_in_recession


j_SAHM_REC <- 0
for (i in (MIN_TIME_TO_NEXT+1): Nmonths_Alg_1){
  if((df_algorithm_1[i,   "Sahm Filter"] >= THRESHOLD_SAHM) & 
     (df_algorithm_1[i-1, "Sahm Filter"] < THRESHOLD_SAHM) ){
    j_SAHM_REC <- j_SAHM_REC +1
    df_algorithm_1[j_SAHM_REC, "SAHM_REC_start"] <- df_algorithm_1[i, "Date"]
  }
}

# Find the first official U.S. recession, and identify all U.S. recessions
first_recession <- min(which(df_algorithm_1[["NBER Dated Recession, FRED series USREC"]] == 1))
current_state   <- RECESSION
df_algorithm_1[1,"USREC_start"] <- df_algorithm_1[first_recession, "Date"]

j_USREC    <- 1  
for(i in (first_recession: Nmonths_Alg_1) ){
  if( (current_state == EXPANSION) &
      (df_algorithm_1[i, "NBER Dated Recession, FRED series USREC"] == 1) ){
    current_state <- RECESSION
    df_algorithm_1[j_USREC, "USREC_start"] <- df_algorithm_1[i, "Date"]
    
  } else if( (current_state == RECESSION) &
             (df_algorithm_1[i, "NBER Dated Recession, FRED series USREC"] == 0) ) {
    current_state <- EXPANSION
    df_algorithm_1[j_USREC, "USREC_end"] <- df_algorithm_1[i, "Date"]
    j_USREC <- j_USREC + 1
    
  }
}


# Find the first official U.S. recession, and identify all U.S. recessions
first_recession <- min(which(df_algorithm_1[["NBER Dated Recession, FRED series USREC"]] == 1))
cur_state       <- RECESSION
df_algorithm_1[1,"USREC_start"] <- df_algorithm_1[first_recession, "Date"]
j_USREC    <- 1  

for(i in (first_recession: Nmonths_Alg_1) ){
  if( (cur_state == EXPANSION) &
      (df_algorithm_1[i, "NBER Dated Recession, FRED series USREC"] == 1) ){
    cur_state <- RECESSION
    df_algorithm_1[j_USREC, "USREC_start"] <- df_algorithm_1[i, "Date"]
    
  } else if( (cur_state == RECESSION) &
             (df_algorithm_1[i, "NBER Dated Recession, FRED series USREC"] == 0) ) {
    cur_state <- EXPANSION
    df_algorithm_1[j_USREC, "USREC_end"] <- df_algorithm_1[i, "Date"]
    j_USREC <- j_USREC + 1
    
  }
}

# Turn all the dates into yearmons
df_algorithm_1[, "USREC_start"] <- zoo::as.yearmon(df_algorithm_1[, "USREC_start"])
df_algorithm_1[, "USREC_end"]   <- zoo::as.yearmon(df_algorithm_1[, "USREC_end"])

df_algorithm_1[,"ALG1_REC_start"] <- zoo::as.yearmon(df_algorithm_1[, "ALG1_REC_start"])
df_algorithm_1[,"ALG1_REC_end"]   <- zoo::as.yearmon(df_algorithm_1[, "ALG1_REC_end"])

df_algorithm_1[,"SAHM_REC_start"] <- zoo::as.yearmon(df_algorithm_1[, "SAHM_REC_start"])
df_algorithm_1_print <- df_algorithm_1[1:(max(j_USREC, j_ALG1_REC, j_SAHM_REC)-1),
                                       c("USREC_start", "USREC_end", 
                                         "ALG1_REC_start", "ALG1_REC_end", "SAHM_REC_start") ]

print(df_algorithm_1_print)

# Figure 22
#Set limits for the y axis for all series except the USREC recession indicator
ymin <- -2
ymax <-  4
#Scale the recession indicator so that it plots from 0 to 1 on the secondary axis
df_algorithm_1[ , "NBER Dated Recession, FRED series USREC"] <- 
  df_algorithm_1[ , "NBER Dated Recession, FRED series USREC"] * (ymax - ymin) + ymin
df_melt <- df_algorithm_1[ , c("Date", "X_t", "NBER Dated Recession, FRED series USREC",
                               "Unemployment Rate (U-3)/10", "Sahm Filter") ] |> 
            reshape2::melt(id.vars = "Date", variable.name = "Variable")

plt <- ggplot(df_melt, aes(x = Date, y= value)) +
        geom_line(aes(color = Variable, linewidth = Variable)) +
        scale_color_manual(values = c("dark green", "azure3", "goldenrod4", 
                                      "red", "darkorange1")) +
        scale_discrete_manual(aesthetic = "linewidth", 
                              values = c(.6, .8, .6, .6, .3)) +
        labs(x = "Date", 
             y = expression(paste("U-3/10, Sahm Filter and  ", X[italic(t)]))) +
        theme_bw() +
        theme(legend.position = c(0.45, 0.8),
              legend.title    = element_blank(),
              legend.background = element_blank(),
              legend.box.background = element_blank(),
              legend.key  = element_blank(),
              legend.text = element_text(size = 16),
              axis.title.x = element_text(size = 14, hjust = 0.5,
                                          margin = margin(t = 5)),
              axis.title.y = element_text(size = 14, vjust = 0.5,
                                          margin = margin(r = 20)),
              axis.title.y.right = element_text(size = 14, vjust = 0.5,
                                                margin = margin(l = 20)),
              axis.text.x  = element_text(size = 13, angle=90, vjust=0.5,
                                          margin = margin(t = 10)),
              axis.text.y  = element_text(size = 13, angle=0,  hjust=0.5,
                                          margin = margin(r = 10)),
              plot.margin  = unit(c(t = 0.25, r = 1, b = 0.25, l = 0.25), "cm")) +
        scale_x_continuous(n.breaks = 8) +
        scale_y_continuous(limits = c(ymin, ymax), breaks = seq(ymin, ymax, by = 1),
                           sec.axis = sec_axis(~ (. + 2)/ 6 ,
                                               name = "NBER Dated Recession Indicator USREC")) +
        geom_hline(aes(yintercept = 0),   linetype = "solid") +
        geom_hline(aes(yintercept = 0.3), linetype = "dashed", color = "dark green") +
        geom_hline(aes(yintercept = 0.5), linetype = "dotted", color = "goldenrod4")

show(plt)

png_fn <- paste0(TEMP_DIR, "Enhanced Sahm Filter.png")

# Start the png driver
png(filename = png_fn, width = 800, height = 480, units = "px",
    pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"), antialias = "d") 
  print(plt) 
dev.off()
