############################################################################
# Description: Performs EDA of between stock up/down, returns, price and a suite of explanatory variables and forecast windows (are we forecasting return for tomorrow, or in five days?). Outputs are a table and boxplot for each combo of n, "y_type" (binary, returns etcs) summarising distribution of pvals, (test data) balanced accur, AUC, MAE etc by explanatory variables and % of times variables were selected by glmnet. Also saves list '"D' containing results (see 'initialise nested list' section for details of exactly what it contains)

# Author and date: Jon Ruffell, 30/12/17
############################################################################

#--------------------------------------------------------------------------
#                       PRELIMS
#--------------------------------------------------------------------------

rm(list = ls()); gc()
library(quantmod)
library(caret)
library(glmnet)
library(ggplot2)
library(scales)

# Specify num symbols to sample
n_syms = 300
###################
# n_syms = 5 #for testing!!! >>>
################## 

# Specify files to load and directories
fname = "L_20171231.RData" #name of data to load
# base.dir = "~/Desktop/Jon/Trading"
base.dir = "C:/Users/user/Desktop/Jonathan/Trading"
dat.dir = file.path(base.dir, "Outputs", "PredictorEDA_Out")
code.dir = file.path(base.dir, "Scripts")
out.dir = file.path(base.dir, "Outputs")
out.dir = file.path(out.dir, "PredictorEDA_Out")
dir.create(out.dir, showWarnings = F)
out.dir = file.path(out.dir, gsub("-", "", Sys.Date()))
dir.create(out.dir, showWarnings = F)
out.dir = file.path(out.dir, paste0("NumSyms=", n_syms)); dir.create(out.dir, showWarnings = F)
utility_funs = "UtilityFuns.R"

# Specify forecast windows periods fws and y_types to iterate across (eg fw=3 means 3 day returns, y_types is same as arg in BuildDataset ie binary, returns or price)
# fws = c(1, 2, 3, 4, 5, 7, 10) #>>>
fws = c(1, 3, 5, 10) #>>>
warning("fws must only contain numbers specified in the 'Ns' obj defined within the BuildDataset fun")
# fws = c(1, 3, 5, 7, 10, 15, 20) #>>> 
# y_types = c("binary", "returns", "price") #dont really need price
y_types = c("binary", "returns") 
set_seed = T #for a given n_syms, should we use the same symbols each time?

# Specify inputs to BuidDataset. NB most are the defaults
perc_miss_y = 5; min_obs_y = 200; perc_miss_x = 10; plot_miss = F; min_date = "2012-01-01"

# Specify inputs to BivariatePvals/ROCs
use_model_mat = F

# Specify no. x obs we need (after imputing non-leading NAs and removing test/val obs). Note need to specify here as we deliberately dont specify in BuildDataset (see function description for explanation) 
max_train_date = "2015-12-31" #date after which we won't use data as we want to reserve for test/vals
max_test_date = "2016-12-30" #test obs will be (max_train_date + 1):max_test_date
min_obs_x = 365 #min no. x obs. Any X obs with fewer will be removed from the built dataset. ** NB THIS DOESNT NECESSARILY MEAN THE FINAL DATASET WILL HAVE >=365 OBS - MAYBE ADD A CHECK FOR THIS... ***

# Load data and utility funs
source(file.path(code.dir, utility_funs))
load(file.path(dat.dir, fname)) #loads "L"

# Error checks
if (!all(y_types %in% c("binary", "returns", "price"))) {
  stop("y_types should be 'binary', 'returns' or 'price'")
}
if (!"data" %in% names(L)) {
  stop ("L should have an element called 'data'")
}
if (n_syms > length(L$data)) {
  stop ("n_syms should be less than or equal to length(L$data))")
}


#--------------------------------------------------------------------------
#             INITIALISE NESTED LIST TO STORE RESULTS
#--------------------------------------------------------------------------

# Make nested list to store ALL our results. This gets a bit confusing, but it will be a four level list, where the levels are as follows: n (as in n day returns), y_type, symbol, results for symbol. E.g. we could call pvals for symbol NGT when y_type = binary and forecast_window=1 as D[["forecast_window=1"]][["binary"]][[NGT]][["pvals"]]). Creating 'empty' list for first three levels (we'll add the fourth in the loop below)
syms = names(L$data)
if (set_seed == T) {set.seed(1)}
syms = sample(syms, n_syms)
levl1 = vector("list", length(syms)); names(levl1) = syms #bottom level - named wtih symbols
levl2 = vector("list", length(y_types)); names(levl2) = y_types #mid level - named with binary, price, returns etc
levl3 = vector("list", length(fws)); names(levl3) = paste0("forecast_window=", fws) #top level - named with "forecast_window=1" etc
for (N in names(levl3)) { #above lists aren't nested. Loop nests them one within the other
  for (y_type in names(levl2)) {
    levl2[[y_type]] = levl1 
  }
  levl3[[N]] = levl2
}
D = levl3; rm(levl1, levl2, levl3) #label more conveniently


#--------------------------------------------------------------------------
#         CALCULATE PVALS/ROCS FOR EACH COMBO OF N, Y_TYPE AND SYM
#--------------------------------------------------------------------------

# ####################################
# y_type = "returns" #>>>> FOR TESTING
# sym = names(L$data)[1] #>>>
# sym = "VET"
# fws = 3
# ####################################

# Loop through each combo of n, y_type and sym, computing and storing pvals and ROCs (as output from BivariabePvals/ROCs) as we go 
k = 1; n_combos = length(fws) * length(y_types) * length(syms) #for progress updates
ptm = proc.time()
for (n in fws) { 
  for (y_type in y_types) {
    for (sym in syms) {
      tryCatch({
        cat ("\nUp to iteration", k, "of", n_combos, "(", round(100*k/n_combos, 2), "%)") #progress update
        k = k + 1 #update counter
        
        # Build dataset. NB other than plot_miss = F, these are the default args
        Z = BuildDataset(sym = sym, L = L, y_type = y_type, perc_miss_y = perc_miss_y, min_obs_y = min_obs_y, 
                         perc_miss_x = perc_miss_x, plot_miss = plot_miss, min_date = min_date,
                         forecast_window = n)
        
        # Split data into test/train
        X = Z$X; M = Z$M #extract data and dataset containing missing value summary (M)
        ind1 = which(rownames(X) == max_train_date); ind2 = which(rownames(X) == max_test_date)
        if (length(ind1) == 0 | length(ind2) == 0) {
          stop ("check dates specified in max_train_date/max_test_date exist in the data. If not, add/remove a day")
        }
        Xte = X[(ind1 + 1):ind2, ] #extract test obs 
        X = X[1:ind1, ] #remove obs after to max_train_date
        
        # Remove cols with too many leading NAs and remove incomplete rows
        to_rem = M$Variable[M$NumObsExclLeadingNAs < min_obs_x] #find x vars with too many missing values
        if (length(to_rem) > 0) {
          X = X[ -which(colnames(X) %in% to_rem)]; Xte = Xte[ -which(colnames(Xte) %in% to_rem)] 
          message("\n", length(to_rem), " variables are being removed as they have fewer than ", min_obs_x, " values in the training data after removing leading NAs:\n", paste0(sort(to_rem), collapse = ", ")) 
        }
        X = X[complete.cases(X), ]; Xte = Xte[complete.cases(Xte), ] #remove incomplete rows
        message("\nThe final train dataset for ", sym, " has ", nrow(X), " complete rows and ", ncol(X), " variables. The final test dataset has ", nrow(Xte), " complete rows")
        if (sum(!is.na(X$y)) < min_obs_y) { #check enough y obs. NB we do this within BuildDataset,  
          stop ("You have too few y obs")}   #but not before we remove test/val obs
        
        # Some messy workaround code you coul probaby write better. Basically, BivariateROCs encounters some probs due to some values of eventPos/neg being all zero. Removing these vars if this is the case
        to_rem = c() # remove eventPos/Neg if all 0 
        inds = grep("event", colnames(X))
        if (length(inds) > 0) {
          for (i in inds) {
            if (sum(X[, i], na.rm = T) == 0) {
              to_rem = c(to_rem, i)}
            if (length(to_rem) > 0) {
              X = X[, -to_rem]; Xte = Xte[, -to_rem]}
          }
        }
        
        # Run BivariatePvals/ROCs etc
        y_family = ifelse (y_type == "binary", "binomial", "gaussian") #get y_family arg
        y = X$y; X = X[, -which(colnames(X) == "y")]; yte = Xte$y; Xte = Xte[, -which(colnames(Xte) == "y")]      
        if (y_type == "price") {y = log(y); yte = log(yte)} #log transform price  
        pvals_train = BivariatePvals(y = y, X = X, y_family = y_family, use_model_mat = use_model_mat)
        if (y_family == "binomial") {
          accurs_rocs_test = BivariateAccurROC(y = y, X = X, yte = yte, Xte = Xte, y_family = y_family, 
                                               use_model_mat = use_model_mat)
          MAEs_RMSEs_test = NA
          
          } else {
            MAEs_RMSEs_test = BivariateMAE(y = y, X = X, yte = yte, Xte = Xte, y_family = y_family, 
                                           use_model_mat = use_model_mat)
            accurs_rocs_test = NA
          }
        glmnet_out = GlmnetFun(X = X, y = y, Xte = Xte, yte = yte, y_family = y_family, nfolds = 3, 
                               lambda = "lambda.min", use_model_mat = F) #get variables selected by glmnet

        # Store results in nested list
        out = list("pvals_train" = pvals_train, "accurs_rocs_test" = accurs_rocs_test, 
                   "MAEs_RMSEs_test" = MAEs_RMSEs_test, "SampleSize_train" = nrow(X),
                   "SampleSize_test" = nrow(Xte), "Glmnet_selectedVars" = glmnet_out$SelectedVariables,
                   "Glmnet_metrics_test" = glmnet_out$TestSetMetrics)
        D[[paste0("forecast_window=", n)]][[y_type]][[sym]] = out
      }, error = function(e) {
        message("\nSomething went wrong for ", sym, " - prob didn't meet one of the criteria for BuildDataset (eg too many missing values")
      })
    }
  }
}
proc.time() - ptm


#-------------------------------------------------------------------------------------------------------
#         'DOWN SAMPLE' METRICS IN D SO ALL FORECAST WINDOWS HAVE SAME NUMBER OF VARIABLES FOR A GIVEN METRIC 
#-------------------------------------------------------------------------------------------------------

##### CHUCK? ####
# Some super messy code taht you might want to check/improve. Basically, we have a problem in compring pvals, accuracies etc across forecst windows as the smaller windows have more variables, meaning when we come to look at the median values for the top ranked vars it's 'easier' to get a good score for larger windows. Correcting by making all have the same number of variables as the forecsat window with the min no. variables

# First, for each symbol, save the number of varibles for which we've computed pvals for each (whic is the same as the no. metrics that should be available for the other metrics)
L = vector("list", length(syms)); names(L) = syms  
for (SYM in names(L)) {
  num_vars = c()
  for (FW in fws) {
    for (Y_TYPE in y_types) {
      num_vars = c(num_vars, nrow(D[[paste0("forecast_window=", FW)]][[Y_TYPE]][[SYM]][["pvals_train"]]))
    }
  }
  L[[SYM]] = num_vars
}

# Second, for each dataset of metrics (e.g. 'pval_train') randomly remove variables till all have the same number as the FW with the min number (as contained in L)
for (SYM in names(L)) {
  num_vars = c()
  for (FW in fws) {
    for (Y_TYPE in y_types) {
      d = D[[paste0("forecast_window=", FW)]][[Y_TYPE]][[SYM]]
      for (METRIC in names(d)) {
        if (is.data.frame(d[[METRIC]]) & !(METRIC %in% c("Glmnet_selectedVars", "Glmnet_metrics_test"))) {
          D[[paste0("forecast_window=", FW)]][[Y_TYPE]][[SYM]][[METRIC]] = 
            d[[METRIC]][sample(1:nrow(d[[METRIC]]), min(L[[SYM]])), ]
        }
      }
    }
  }
}


#--------------------------------------------------------------------------
#         SUMMARISE FINDINGS
#--------------------------------------------------------------------------

# Loop which for each n and y_type summarises the pvals/rocs across the symbols. E.g. what's the mean pval for variable 'r_L1' when forecast_window=1 and y_type = binary?
ptm = proc.time()
k = 1; n_combos = length(fws) * length(y_types) #for progress updates
for (n in fws) { 
  out.dir_temp = file.path(out.dir, paste0("forecast_window=", n)); 
  dir.create(out.dir_temp, showWarnings = F)
  for (y_type in y_types) {
    out.dir_temp2 = file.path(out.dir_temp, y_type); dir.create(out.dir_temp2, showWarnings = F)
    
    cat ("\nUp to iteration", k, "of", n_combos, "(", round(100*k/n_combos, 2), "%)") #progress update
    k = k + 1 #update counter
    
    # Loop to extract results across all symbosl given n and y_type and store in summary matrics. First, extract the data with all the symbosl (L1) and all the variables we have data on across those symbols
    L1 = D[[paste0("forecast_window=", n)]][[y_type]]      
    vars = unique(as.character(unlist(lapply(L1, function(l) as.character(l$pvals_train$Variable)))))
    
    # Below loop extracts pval, balance accuracy, AUC, MAE and median AE results across the symbosl (W_p, W_ba, W_auc, W_mae and W_medae respectively) by variable. Each cell corresponds to a pval etc for one combo of symbol (cols) and variable (rows) such that by summarising by row we can get eg median pval for ith variable
    W_p = matrix(NA, nrow = length(vars), ncol = length(names(L1))) #to store results
    rownames(W_p) = vars; colnames(W_p) = names(L1)
    W_ba = W_p; W_auc = W_p; W_mae = W_p; W_medae = W_p; W_glmnet = W_p
    for (i in 1:nrow(W_p)) {
      # cat("\nUp to variable", i, "of", nrow(W_p))
      for (j in 1:ncol(W_p)) {
        i_p = which(L1[[j]]$pvals_train$Variable == rownames(W_p)[i]) #index of variable in pvals datarame in W_p
        i_glmnet = which(L1[[j]]$Glmnet_selectedVars$Variable == rownames(W_p)[i]) #as above for selected vars
        i_ba = NULL; i_auc = NULL; i_mae = NULL; i_medae = NULL #initialise 
        if (y_type == "binary") {
          i_ba = which(L1[[j]]$accurs_rocs_test$Variable == rownames(W_ba)[i]) #as for i_p above
          i_auc = which(L1[[j]]$accurs_rocs_test$Variable == rownames(W_auc)[i])
        } else {
          i_mae = which(L1[[j]]$MAEs_RMSEs_test$Variable == rownames(W_mae)[i])
          i_medae = which(L1[[j]]$MAEs_RMSEs_test$Variable == rownames(W_medae)[i])
        }
        if (length(i_p) >  0) {W_p[i, j] = L1[[j]]$pvals_train$Pval[i_p]}
        if (length(i_glmnet) > 0) {W_glmnet[i, j] = 1} #just record flag if var was selected into final glmnet
        if (length(i_ba) > 0) {W_ba[i, j] = L1[[j]]$accurs_rocs_test$BalancedAccur[i_ba]}
        if (length(i_auc) > 0) {W_auc[i, j] = L1[[j]]$accurs_rocs_test$AUC[i_auc]}
        if (length(i_mae) > 0) {W_mae[i, j] = L1[[j]]$MAEs_RMSEs_test$MAE[i_mae]}
        if (length(i_medae) > 0) {W_medae[i, j] = L1[[j]]$MAEs_RMSEs_test$MedianAE[i_medae]}
      }
    }
    
    # Above datasets still arent summarised enough - contain all the pvlas/rocs. W below summarises into eg median
    W = data.frame("Variable" = vars, 
                   "Perc_inGlmnet" = apply(W_glmnet, 1, function (x) 100 * sum(x == 1, na.rm = T)/length(x)),
                   "Pval_Median" = apply(W_p, 1, median, na.rm = T),
                   "BalancedAccur_Median" = apply(W_ba, 1, median, na.rm = T),
                   "AUC_Median" = apply(W_auc, 1, median, na.rm = T),
                   "MAE_Median" = apply(W_mae, 1, median, na.rm = T),
                   "MedianAE_Median" = apply(W_medae, 1, median, na.rm = T),
                   "Pval_N" = apply(W_p, 1, function(x) sum(!is.na(x))),
                   "BalancedAccur_N" = apply(W_auc, 1, function(x) sum(!is.na(x))),
                   "AUC_N" = apply(W_auc, 1, function(x) sum(!is.na(x))),
                   "MAE_N" = apply(W_mae, 1, function(x) sum(!is.na(x))),
                   "MedianAE_N" = apply(W_medae, 1, function(x) sum(!is.na(x))),
                   "Pval_25perc" = apply(W_p, 1, function(x) quantile(x, 0.25, na.rm = T)),
                   "BalancedAccur_25perc" = apply(W_ba, 1, function(x) quantile(x, 0.25, na.rm = T)),
                   "AUC_25perc" = apply(W_auc, 1, function(x) quantile(x, 0.25, na.rm = T)),
                   "MedianAE_25perc" = apply(W_medae, 1, function(x) quantile(x, 0.25, na.rm = T)),
                   "Pval_75perc" = apply(W_p, 1, function(x) quantile(x, 0.75, na.rm = T)),
                   "BalancedAccur_75perc" = apply(W_ba, 1, function(x) quantile(x, 0.75, na.rm = T)),
                   "AUC_75perc" = apply(W_auc, 1, function(x) quantile(x, 0.75, na.rm = T)),
                   "MAE_75perc" = apply(W_mae, 1, function(x) quantile(x, 0.75, na.rm = T)),
                   "MedianAE_75perc" = apply(W_medae, 1, function(x) quantile(x, 0.75, na.rm = T)),
                   "Pval_PercLT0.1" = apply(W_p, 1, function(x) 100 * sum(x < 0.1, na.rm = T)/sum(!is.na(x))),
                   "Pval_PercLT0.05" = apply(W_p, 1, function(x) 100 * sum(x < 0.05, na.rm = T)/sum(!is.na(x))))
    
    # Add variables which are rankings of each metric. Normally would do with 'order' but for some reason not working - below is workaround
    W = W[order(W$Perc_inGlmnet, decreasing = T), ]; W$Perc_inGlmnet_Rank = 1:nrow(W)
    W = W[order(W$Pval_Median, decreasing = F), ]; W$Pval_Rank = 1:nrow(W)
    W = W[order(W$BalancedAccur_Median, decreasing = T), ]; W$BalancedAccur_Rank = 1:nrow(W) 
    W = W[order(W$AUC_Median, decreasing = T), ]; W$AUC_Rank = 1:nrow(W) 
    W = W[order(W$MAE_Median, decreasing = F), ]; W$MAE_Rank = 1:nrow(W) 
    W = W[order(W$MedianAE_Median, decreasing = F), ]; W$MedianAE_Rank = 1:nrow(W) 
    if (y_type == "binary") { #sort using a final, sensible variable
      W = W[order(W$BalancedAccur_Median, decreasing = T), ]
    } else {
      W = W[order(W$MedianAE_Median, decreasing = F), ]
    }
    
    # Save summary matrix in our D object and as csv
    D[[paste0("forecast_window=", n)]][[y_type]][["SummaryMat"]] = W
    write.csv(W, file.path(out.dir_temp2, paste0("SummaryMat_forecast_window=", n, "_y_type=", y_type, ".csv")), 
              row.names = F)
    
    
    #--------------------------------------------------------------------------
    #         PLOTS
    #--------------------------------------------------------------------------
    
    # Change some options depending on which operating system we're using
    if (.Platform$OS.type == "windows") {
      cex_axis = 0.3; exten = "pdf"
    } else {
      cex_axis = 0.5; exten = "jpeg"
    }
    
    # Make Pvals plot. First, sort plot data (in W_p) by median (as contained in W)
    vars_sorted = W$Variable[order(W$Pval_Median, decreasing = T)]
    inds= match(vars_sorted, rownames(W_p))
    W_p = W_p[inds, ]
    x11(height = 10, width = 13)
    par(mar=c(5.1, 10, 4.1, 2.1))
    boxplot(t(W_p), las = 2, cex.axis = cex_axis, cex = 0.2, lwd = 0.5, horizontal = T)
    abline(v = 0.05, lty = 2, col = alpha("black", 0.5))
    abline(v = 0.1, lty = 2, col = alpha("black", 0.5))
    savePlot(file.path(out.dir_temp2, paste0("BoxPlot_Pvals_forecast_window=", n, "_y_type=", y_type, ".", exten)), 
             type = exten)
    graphics.off()
    

    # Make % selected into glmnet plot. Technically should be barplot but this shows same thing!
    g = ggplot(W, aes(x=reorder(factor(W$Variable), W$Perc_inGlmnet), 
                      y=W$Perc_inGlmnet)) +
      geom_bar(stat='identity') + coord_flip()
    if (.Platform$OS.type == "windows") {
      g = g + theme(axis.text=element_text(size=5))} #axis text is wrong for windows but not for unix
    x11(height = 10, width = 13)
    print(g)
    savePlot(file.path(out.dir_temp2, paste0("BarPlot_PercInGlmnet_forecast_window=", n, "_y_type=", 
                                             y_type, ".", exten)), type = exten)
    graphics.off()

    # Make Balanced Accuracy and AUC plots. 
    if (y_type == "binary") { #else not defined
      
      # Balanced accur
      vars_sorted = W$Variable[order(W$BalancedAccur_Rank, decreasing = T)]
      inds= match(vars_sorted, rownames(W_ba))
      W_ba = W_ba[inds, ]
      x11(height = 10, width = 13)
      par(mar=c(5.1, 10, 4.1, 2.1))
      boxplot(t(W_ba), las = 2, cex.axis = cex_axis, cex = 0.2, lwd = 0.5, horizontal = T)
      abline(v = 0.5, lty = 2, col = alpha("black", 0.5))
      savePlot(file.path(out.dir_temp2, paste0("BoxPlot_BalanceAccur_forecast_window=", n, 
                                               "_y_type=", y_type, ".", exten)), 
               type = exten)
      graphics.off()
      
      # AUC
      vars_sorted = W$Variable[order(W$AUC_Rank, decreasing = T)]
      inds= match(vars_sorted, rownames(W_auc))
      W_auc = W_auc[inds, ]
      x11(height = 10, width = 13)
      par(mar=c(5.1, 10, 4.1, 2.1))
      boxplot(t(W_auc), las = 2, cex.axis = cex_axis, cex = 0.2, lwd = 0.5, horizontal = T)
      abline(v = 0.5, lty = 2, col = alpha("black", 0.5))
      savePlot(file.path(out.dir_temp2, paste0("BoxPlot_AUC_forecast_window=", n, 
                                               "_y_type=", y_type, ".", exten)), 
               type = exten)
      graphics.off()
    }
    
    # Make MAE and median AE plots. 
    if (y_type != "binary") { #else not defined
      
      # MAE
      vars_sorted = W$Variable[order(W$MAE_Rank, decreasing = T)]
      inds= match(vars_sorted, rownames(W_mae))
      W_mae = W_mae[inds, ]
      x11(height = 10, width = 13)
      par(mar=c(5.1, 10, 4.1, 2.1))
      boxplot(t(W_mae), las = 2, cex.axis = cex_axis, cex = 0.2, lwd = 0.5, horizontal = T)
      savePlot(file.path(out.dir_temp2, paste0("BoxPlot_MAE_forecast_window=", n, 
                                               "_y_type=", y_type, ".", exten)), 
               type = exten)
      graphics.off()
      
      # median AE
      vars_sorted = W$Variable[order(W$MedianAE_Rank, decreasing = T)]
      inds= match(vars_sorted, rownames(W_medae))
      W_medae = W_medae[inds, ]
      x11(height = 10, width = 13)
      par(mar=c(5.1, 10, 4.1, 2.1))
      boxplot(t(W_medae), las = 2, cex.axis = cex_axis, cex = 0.2, lwd = 0.5, horizontal = T)
      savePlot(file.path(out.dir_temp2, paste0("BoxPlot_MedianAE_forecast_window=", n, 
                                               "_y_type=", y_type, ".", exten)), 
               type = exten)
      graphics.off()
    }
  }
}
proc.time() - ptm
save(D, file = file.path(out.dir, "D.RData")) #save overall list obj

