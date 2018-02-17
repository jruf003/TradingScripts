############################################################################
# Description: contains stock/exchange foreasting utility functions

# Author and date: Jon Ruffell, ~15/3/17
############################################################################


######################################################################
#       DownloadData FUNCTION
######################################################################

DownloadData = function(syms.df = NULL, L = NULL,
                        from = "1900-01-01", 
                        to = Sys.Date()) {
  #---------------------------------------------------------------------
  # Description: Downloads data using getSymbols and stores the results in a list "L". Will EITHER download the symbols specified in 'syms.df' from scratch, or if an existing 'L' is provided (and one of these two must be), updates this obj with the most recent data. NB at time of writing downloads fail for ~10% of the yahoo symbols, and the approach is to omit these stocks (if we download from scratch) or delete the existing data (if we're updating an existing obj).
  # Args: 
  # syms.df: a two col dataframe with colnames "Symbol" and "SRC" which specify symbols we want to download data for. Only used if we're downloading from scratch. Currently SRC must be yahoo or oanda (e.g. no FRED, google) but this can be updated easily enough.
  # L: a list created by running the current function 'from scratch'. Specifically, must be a two-element list where element 1 is 'data' (a list where ith element is a call to getSymbols) and element 2 is 'meta_data' (a dataframe containing metadata on L$data e.g. SRC.
  # from and to: dates specifying when to extract data from/to (same as these args in getSymbols). If L is specified, 'from' is ignored (we just set from to the latest date + 1).
  # Returns: See description and args > L for details. 
  
  #---------------------------------------------------------------------
  
  # ----------------------------------------------------------------
  #     EXTRACT LIST OF ALL SYMBOLS AVAILABLE IN YAHOO/OANDA
  # ----------------------------------------------------------------
  
  # EXtract list of all symbols avaialble in yahoo/oanda - we use this in error checks below to ensure everything we want to download is available. NOTE WE MIGHT DONWLOAD DATA FROM OTHER SOURCES E.G. FRED - IF SO YOU'LL NEED TO UPDATE THE BELOW AS YOU'RE CURRENTLY ONLY DOWNLOADING SYMBOLS IN YAHOO/OANDA 
  
  # Download all stock symbols and currencies/metals available in yahoo and oanda, respecively. Note we save stocks.df as we use later. HAVING SAID THIS & AS MENTIONED IN 'extra_vars' BELOW STOCKS DOESNT CONTAIN ALL SYMBOLS AVIALABLE IN YAHOO
  stocks.df = stockSymbols(); stocks = stocks.df$Symbol
  curs_mets = oanda.currencies # NB rownames are the symbols. Incl. metals
  
  # Make vector of some extra variables (key indices plus brent oil) which might be useful but which for some reason don't show up in 'stocks'
  extra_vars = c("^DJI", "^FTSE", "SPY", "^HSI", "^SSEC", 
                 "^IXIC", "^GDAXI", "^FCHI", "BNO")
  
  # Separate metals and currencies (by searching from regex "(oz.)" which from looking at curs_mets proceeds each metal eg "Gold (oz.)")
  inds = grep("oz\\.", curs_mets[, 1]) # indices of metals
  curs = rownames(curs_mets)[-inds]; names(curs) = curs_mets[-inds, ]
  mets = rownames(curs_mets)[inds]; names(mets) = curs_mets[inds, ]
  
  # Modify currency and metals so they're exactly as they appear in oanda, namely currencies aren't just e.g. "NZD" but a pair e.g. "NZD/USD". Similarly metals need to be expressed in a given currency e.g. "XAU" -> "XAU/USD" 
  O = outer(curs, curs, FUN = paste, sep = "/") 
  diag(O) = NA # remove diag - otherwise a currency paired against itself!
  curs = as.vector(O)[!is.na(as.vector(O))]
  mets = paste(mets, "USD", sep = "/")
  
  # ----------------------------------------------------------------
  #                 SOME ERROR CHECKS
  # ----------------------------------------------------------------
  
  if (sum(c(is.null(L), is.null(syms.df))) != 1) {
    stop ("You should supply exactly one of L or syms.df")
  }
  if (!is.null(L)) { # some checks specific to when we DO supply L
    if (class(L) != "list") {
      stop ("L should be a list")
    }
    if (!all(names(L) == c("data", "meta_data"))) {
      stop ("L should be a two element list with names 'data' and 'meta_data'")
    }
    if (!(all(c("Symbol", "SRC") %in% colnames(L$meta_data)))) {
      stop ("L$meta_data should include colnames 'Symbol' and 'SRC'")
    }
    if (from != "1900-01-01") {
      stop ("You can't specify a 'from' date if we're adding to an existing L object - for each symbol, we set 'from' to one day after the last date in L for which data were recorded)")
    }
    
    # Create a data.frame specifying the a) symbols and b) SRCs for each symbol in L. We use this in an error check below as well as when we add new data
    syms.df = L$meta_data[, colnames(L$meta_data) %in% c("Symbol", "SRC")]
  }
  if (!is.null(syms.df)) { # checks specific to when we DONT supply L
    if (class(syms.df) != "data.frame") {
      stop("syms.df should be a data frame")
    }
    if (!all(c("Symbol", "SRC") %in% colnames(syms.df))) {
      stop ("syms.df should have colnames 'Symbol' and 'SRC'")
    }
  }
  if (!all(syms.df$SRC %in% c("yahoo", "oanda"))) {
    stop ("All SRCs in syms.df$SRC/L$meta_data$SRC should be one either yahoo or oanda. NOTE: YOU CAN ALSO DOWNLAOD DATA FROM google AND FRED, BUT YOU'LL NEED TO MODIFY THE CODE TO HANDLE THIS. SPECIFICALLY, YOU'LL BE ABLE TO DOWNLOAD DIRECTLY FROM THESE SITES IF YOU DISABLE THE CHECKS THAT ENSURE THE SYMBOLS ARE FOUND IN YAHOO/OANDA, OR ADD TO THESE CHECKS SO THAT THEY ALSO CHECK IF SYMBOLS ARE FOUND IN GOOGLE/FRED")
  }
  
  # Possibly the most important check: ensure all the symbols in syms.df (either specified explicity or extracted from L$meta_data above) are available in yahoo/oanda. If not, we won't be able to download them! 
  syms.df[] = lapply(syms.df, as.character) #convert cols to character
  if (!all(syms.df$Symbol[syms.df$SRC == "yahoo"] %in% 
           c(stocks, extra_vars)) |
      !all(syms.df$Symbol[syms.df$SRC == "oanda"] %in% c(curs, mets))) {
    stop ("Check all the symbols specified in syms.df/L$meta_data are available in yahoo/oanda")
  }
  if (as.Date(from) > Sys.Date() | as.Date(to) > Sys.Date()) {
    stop ("'from' and/or 'to' should be before today's date!")
  }
  
  # ----------------------------------------------------------------
  #           IF L WASN'T SUPPLIED, CREATE IT FROM SCRATCH
  # ----------------------------------------------------------------
  
  if (is.null(L)) { # if L is empty, fill with the vars we specified
    fails = c() # to store any data we fail to download
    L$data = vector(mode = "list", nrow(syms.df)) #initialise list
    names(L$data) = syms.df$Symbol    
    for (i in 1:nrow(syms.df)) {
      cat("Up to symbol", i, "of", nrow(syms.df), #report progress
          "(", round(100 * (i/nrow(syms.df)), 1), "%)\n")
      tryCatch({ #Often can't download data. THis avoids loop breaking 
        L$data[[i]] = getSymbols(syms.df$Symbol[i], 
                                 src = syms.df$SRC[i], 
                                 env = NULL,
                                 from = as.Date(from),
                                 to = as.Date(to)) # the dat
        if (is.OHLC(L$data[[i]])) { 
          v = L$data[[i]] #no idea why, but cant just call adjustOHCL(L$data[[i]]) dirctly
          L$data[[i]] = adjustOHLC(v) #adjust for splits/dividends 
        }
      }, error = function(e) {
        fails <<- c(fails, syms.df$Symbol[i])
      })
    }
  } else { # if we're adding data to an existing L
    
    # ----------------------------------------------------------------
    #           IF L WAS SUPPLIED, ADD ANY NEW DATA TO IT
    # ----------------------------------------------------------------
    
    if (!identical(syms.df$Symbol, names(L$data))) {
      stop ("syms.df$Symbol and names(L$data) should be identical")
    }
    fails = c() #to store symbols we fail to update)
    for (i in 1:length(L$data)) {
      cat("Up to symbol", i, "of", length(L$data), #report progress
          "(", round(100 * (i/length(L$data)), 1), "%)\n")
      last_date = max(index(L$data[[i]])) # last date in current data
      if (last_date > as.Date(to)) {
        stop ("the last recorded date for ", names(L$data)[i], " is later than your 'to' date")
      }
      tryCatch({ #Often can't download data. THis avoids loop breaking
        new_dat = getSymbols(syms.df$Symbol[i], # download the new data
                             src = syms.df$SRC[i], 
                             env = NULL,
                             from = last_date,
                             to = as.Date(to)) 
        if (is.OHLC(new_dat)) { 
          v = new_dat #no idea why, but cant just call adjustOHCL(L$data[[i]]) dirctly
          new_dat = adjustOHLC(v) #adjust for splits/dividends 
        }
        if (as.numeric(index(new_dat)[1] - last_date) > 4) {
          stop ("There may have been an error with updating the data - the new data is >4 days after the last date in L")
        } 
        L$data[[i]] = rbind(L$data[[i]], new_dat) #bind new dat
      }, error = function(e) {
        fails <<- c(fails, syms.df$Symbol[i])
      })
    }
  }
  
  # ----------------------------------------------------------------
  #           ADD/UPDATE A META DATA OBJECT
  # ----------------------------------------------------------------
  
  # Create a "meta data" object which we'll store in L (or overwrite the existing meta data obj if we supplied L). This contains the SRC from which the data was downloaded (so we know where to download from when adding new data to L in future); plus for the stocks, add the additioanl additional meta data as available in call to stockSymbols() (e.g., industry, market cap etc). Note no such info is available for data from oanda hence we have NAs for these symbols. Note if we failed to download a stock we remove this from the data/meta data at the very end.
  inds = match(syms.df$Symbol, stocks.df$Symbol)
  meta_data = data.frame(syms.df, stocks.df[inds, ])
  if (!all(meta_data$Symbol == meta_data$Symbol.1, na.rm = T)) {
    stop ("Something's gone wrong with matching syms.df & stocks.df")
  } else {  # remove defunct symbol column (code above makes two)
    L$meta_data = meta_data[, -which(colnames(meta_data) == "Symbol.1")]
  }
  if (!all(meta_data$Symbol == names(L$data))) {
    stop ("The meta_data$Symbol obj should match names(L$data)")
  }  
  
  # ----------------------------------------------------------------
  #     DELETE DATA FOR ANY SYMBOLS WE FAILED TO DOWNLOAD/UPDATE
  # ----------------------------------------------------------------
  
  # Delete any data for symbols we failed to download/update.
  if (length(fails) > 0) {
    L$data = L$data[-which(names(L$data) %in% fails)]
    L$meta_data = L$meta_data[-which(L$meta_data$Symbol %in% fails), ]
  }
  
  # Create warning of which symbols we failed to downlaod/update
  msg = ifelse(length(fails) == 0,
               "\n*****************************************\nAll symbols were successfully downloaded/updated\n*****************************************\n",
               paste0("\n *******************************************\nWe failed to download/update the following symbols and these have been omitted/deleted:\n", paste(fails, collapse = "\n"), 
                      "\n*******************************************"))
  warning(msg) 
  
  # Remove any duplicate symbols (which can easily happen, eg. stockSymbols() produces duplicate rows)
  inds1 = which(duplicated(names(L$data)))
  inds2 = which(duplicated(L$meta_data$Symbol))
  if (length(inds1) > 0 | length(inds2) > 0) {
    if (!identical(inds1, inds2)) {
      stop ("There were duplicate symbols in L, but for some reason the duplicates in names(L$data) dont match those in L$meta_data$Symbol")
    } else {
      L$data = L$data[-inds1]; L$meta_data = L$meta_data[-inds1, ]
      if (!identical(names(L$data), L$meta_data$Symbol)) {
        stop ("SOmething's gone wrong when deleting duplicate symbols")
      }
    }
  }
  
  # Return L!
  invisible(L) 
}


######################################################################
#               BuildDataset FUNCTION
######################################################################

BuildDataset = function(sym, L, y_type = "binary", forecast_window,
                        perc_miss_y = 5, min_obs_y = 100, 
                        perc_miss_x = 10, plot_miss = T,
                        min_date = "2012-01-01") {
  
  # ---------------------------------------------------------------------
  # Description: builds a modelling dataset X for a given symbol from data saved in L. Currently builds a number of technical indicators for sym;  adds some commodities/indices (eg DJIA)/exchange rates; and creates index (weighted by market cap) from other symbols in same sector & industry; and weekday. ADD MORE VARIABLES
  # Args: sym: the symbol we want to build the dataset for; L: the list containing the data for all the symbols (as output from DownloadData()); y_type: do we want the response to be "price", "return" or "binary" (ie up/down); forecast_window: window to forecast in eg are we trying to predict 5 days ahead? 10 days?; perc_miss_y: the max percent of missing obs allowed within the response (if more than this we get an error); min_obs_y: the min number of non-NA obs in y allowed (if fewer we get an error); perc_miss_x: the amount of NON-LEADING missing values we allow within each X variable (else we dont add it - and NB we dont consider leading missing values as we output this from the function ie the user can decide whether to exclude a variable based on leading NAs at that point); plot_miss: produce a plot summarising the nubmer of leading NAs for each explanatory variable?; min_date: date before which we remove any obs in sym (the rationale being when we define industry and sector indices, we only do so using symbols at least the same length as sym. So, the later the min_date, the less likely other syms in the same industry/sector will be excluded from these indices)
  # Returns: a list comprising the dataset for modelling (X) and summarising number of obs (excluding leading NAs) available for each variable (M). NB this summary is done after we've imputed non-leading NAs.
  # ---------------------------------------------------------------------
  
  library(quantmod)
  library(imputeTS) #interpol of missing vals
  
  # =====================================================================
  #         SOME ERROR CHECKS
  # =====================================================================
  
  # NB virtually all below error checks should be passed if L is output from DownloadData funcion (which it should be) - we're effectively checking this.
  if (class(L) != "list") {
    stop ("L should be a list")
  } 
  if (!all(names(L) == c("data", "meta_data"))) {
    stop ("L should have elements 'data' and 'meta_data' (as output from running the function DownloadData")
  }
  if (!all(unlist(lapply(L$data, function(x) {class(x)[1]})) == "xts")) {
    stop ("All elements of L$data should be xts objs")
  }
  if (!all(c("Symbol", "SRC", "Name", "MarketCap", "Sector", "Industry") 
           %in% colnames(L$meta_data))) {
    stop ("Symbol, SRC, Name, MarketCap, Sector, & Industry should be colnames of L$meta_data") 
  }
  if (!sym %in% names(L$data)) {
    stop ("sym isn't in L")
  }
  if (nrow(L$data[[sym]]) == 1) { #check min number of obs below too, but need a prelim check as else the code between now and that check fails
    stop ("Your data series only has one observation")
  }
  indices = c("^DJI", "^FTSE", "SPY", "^HSI", "^SSEC", 
              "^IXIC", "^GDAXI", "^FCHI")
  # commods = c("XAU/USD", "XAG/USD", "BNO") # gold, silver and brent oil
  commods = c("XAU/USD", "XAG/USD") # gold, silver - removed brent oil as cant seem to download anymore
  ERs = paste("USD", c("NZD", "AUD", "GBP", "EUR", "JPY", "HKD", "CNY"), 
              sep = "/")
  if (!all(c(indices, commods, ERs) %in% names(L$data))) {
    stop ("This code assumes L contains some key indices (e.g dow jones, footsie), commodoties and exchange rates. Should still work if it doesn't, but you might want to find out why they're absent as we build explnatory variables out of these")
  }
  if (!y_type %in% c("price", "returns", "binary")) {
    stop ("y_type should be in price, returns or binary")
  }
  
  
  # =====================================================================
  #         EXTRACT SOME KEY DATA FOR SYM
  # =====================================================================
  
  # Extract price data for sym. The function keeps the adjusted close col (if present) else the single column associated with e.g. exchange rates, commodities. 
  X = ExtractPrice(X = L$data[[sym]], colname = "pL0") 
  X[X[, 1] == 0, 1]  = NA # Replace 0s with NAs. (Assume they're errors, and they cause probs as when we compute returns they can become Inf)
  X$pL0 = na.interpolation(as.numeric(X$pL0), option = "linear") #interpolate missings
  
  # Extract response variable. Options are price, returns or binary returns
  if (y_type == "price") {
    X$y = X$pL0 
  } else if (y_type == "returns") {
    X$y = ndayRets(X$pL0, n = forecast_window)
  } else if (y_type == "binary") {
    X$y = ifelse(ndayRets(X$pL0, n = forecast_window) > 0, 1, 0)
  }
  
  # =====================================================================
  #         ADD SOME EXPLANATORY VARIABLES
  # =====================================================================
  
  # ---------------------------------------------------------------------
  #         ADD SOME TECHNICAL INDICATORS
  # ---------------------------------------------------------------------
  
  # Establish vector of "Ns" which we'll use repeatedly below. Thesee are the values of n for ndayRets we'll iterate over
  # Ns = c(1, 2, 3, 4, 5, 7, 10, 15, 20)
  Ns = c(1, 3, 5, 10)
  
  if (!forecast_window %in% Ns) {
    stop ("forecast_window needs to be one of the numbers specified in 'Ns' (defined within BuildDataset). You can change this easily enough")
  }
  
  # Create a 'key name' text we'll reuse below. This is basically the name of the 'main' variable as it occurs in the data, e.g. rL5_5day if forecast_window=5 and will form the basis of e.g. EMAs etc
  key_name = paste0("rL", forecast_window, "_", forecast_window, "day")
  
  # n-day return on lag l (adj.) price for various ns, where l = forecast window (NB doesnt make sense to have n < forecast window eg seems unlikely that n=1 day returns will predict 5 day returns if forecast window = 5)
  for (n in Ns) { 
    # if (n >= forecast_window) { 
    X = addVar(X = X, var = ndayRets(quantmod::Lag(X$pL0, k = forecast_window), n = n), 
               var_name = paste0("rL", forecast_window, "_", n, "day"), 
               perc_miss = perc_miss_x)
    # }
  }
  
  # Make binary flag of 'key_name' variable i.e. 1 if positive, 0 otherwise
  if (y_type != "price") { #doesnt make sense for price - will always be positive 
    X = addVar(X = X, var = ifelse(X[, key_name] > 0, 1, 0), 
               var_name = paste0(key_name, "_flag"), 
               perc_miss = perc_miss_x)
  }
  
  # (Adj.) price at last lag
  X = addVar(X = X, var = quantmod::Lag(X$pL0, k = forecast_window), 
             var_name = paste0("pL", forecast_window),  #p_L1, p_L2 etc
             perc_miss = perc_miss_x)
  
  # n-day change in high (i.e., return) for various ns 
  ind = grep("High", colnames(L$data[[sym]]))
  if (length(ind) == 1) { #if we have volume recorded in L$data, which we do for the yahoo stocks
    hi = L$data[[sym]][, ind]
    for (n in Ns) {
      X = addVar(X = X, var = ndayRets(quantmod::Lag(hi, k = forecast_window), n = n), 
                 var_name = paste0("hi_rL", forecast_window, "_", n, "day"),
                 perc_miss = perc_miss_x)
    }
  }
  
  # n-day change in low (i.e., return) for various ns 
  ind = grep("Low", colnames(L$data[[sym]]))
  if (length(ind) == 1) { #if we have volume recorded in L$data, which we do for the yahoo stocks
    low = L$data[[sym]][, ind]
    for (n in Ns) { 
      X = addVar(X = X, var = ndayRets(quantmod::Lag(low, k = forecast_window), n = n), 
                 var_name = paste0("lo_rL", forecast_window, "_", n, "day"),
                 perc_miss = perc_miss_x)
    }
  }
  
  # n-day change in Vol (i.e., return) for various ns 
  ind = grep("Volume", colnames(L$data[[sym]]))
  if (length(ind) == 1) { #if we have volume recorded in L$data, which we do for the yahoo stocks
    vol = L$data[[sym]][, ind]
    for (n in Ns) { # don't need lag one as that's already computed
      X = addVar(X = X, var = ndayRets(quantmod::Lag(vol, k = forecast_window), n = n), 
                 var_name = paste0("v_rL", forecast_window, "_", n, "day"),
                 perc_miss = perc_miss_x)
    }
  }
  
  # n-day change in open price (i.e., return) for various ns 
  ind = grep("Open", colnames(L$data[[sym]]))
  if (length(ind) == 1) { #if we have volume recorded in L$data, which we do for the yahoo stocks
    open = L$data[[sym]][, ind]
    for (n in Ns) { # don't need lag one as that's already computed
      X = addVar(X = X, var = ndayRets(quantmod::Lag(open, k = forecast_window), n = n), 
                 var_name = paste0("op_rL", forecast_window, "_", n, "day"),
                 perc_miss = perc_miss_x)
    }
  }
  
  # volatility (sd) of lagged returns computed over 5, 10 15 and 20 days. Done for 'forecast window' lags and n, eg if forecast window is 5 then sd computed for rL5_5days
  for (n in c(5, 10, 15, 20)) {
    X = addVar(X = X, var = runSD(X[, key_name], n = n), 
               var_name = paste0("sd_", key_name, "_n", n),
               perc_miss = perc_miss_x)
  } 
  
  # n-day EMA applied to lagged 'key_name' variable for various ns (where n is the no. days to smooth over)
  for (n in c(3, 5, 10, 20)) { # HERE n IS DAYS TO SMOOTH OVER, NOT PARAMETER FOR ndayRets 
    X = addVar(X = X, var = EMA(X[, key_name], n = n), 
               var_name = paste0("EMA_", key_name, "_n", n),
               perc_miss = perc_miss_x)
  }
  
  # MACD of lagged price, signal, price less signal and cross over. NB we compute cross in two ways: "cross up" if signal crosses MACD and "cross down" if vice versa (or the other way around - i forget) 
  X = addVar(X = X, var = MACD(quantmod::Lag(X$pL0, k = forecast_window))[, "macd"],
             var_name = paste0("MACD_pL", forecast_window), perc_miss = perc_miss_x)
  X = addVar(X = X, var = MACD(quantmod::Lag(X$pL0, k = forecast_window))[, "signal"],
             var_name = paste0("MACD_pL", forecast_window, "_sig"), perc_miss = perc_miss_x)
  X = addVar(X = X, var = X[, ncol(X) - 1] - X[, ncol(X)],
             var_name = paste0("MACD_pL", forecast_window, "_macd-sig"), perc_miss = perc_miss_x)
  v = X[, ncol(X)] > 0 #logical vector: MACD > signal? Extracting for easier handling
  X$temp_colname = c(NA, ifelse(as.logical(v[-1]) == T & 
                                  as.logical(v[-length(v)] == F), 1, 0))
  colnames(X)[ncol(X)] = paste0("MACD_pL", forecast_window, "_crossUp")
  X$temp_colname = c(NA, ifelse(as.logical(v[-1]) == F & 
                                  as.logical(v[-length(v)] == T), 1, 0))
  colnames(X)[ncol(X)] = paste0("MACD_pL", forecast_window, "_crossDown")

  
  # n-day RSI for lagged price and returns for various n=3, 7, 14, 21. Also flag for when its >70 and <30 (common thresholds for overbought/sold). 
  for (n in c(3, 7, 14, 21)) { #NOTE n IS DIFF INPUT FROM ndayRets n
    X = addVar(X = X, var = RSI(X[, paste0("pL", forecast_window)], n = n), 
               var_name = paste0("RSI_", paste0("pL", forecast_window), "_n", n), 
               perc_miss = perc_miss_x)
    v = X[, ncol(X)] #extract RSI var we just created for easeir hanlding
    X = addVar(X = X, var = ifelse(v > 70, 1, 0), 
               var_name = paste0("RSI_", paste0("pL", forecast_window), 
                                 "_n", n, "_70flag"), 
               perc_miss = perc_miss_x)
    X = addVar(X = X, var = ifelse(v < 20, 1, 0), 
               var_name = paste0("RSI_", paste0("pL", forecast_window), 
                                 "_n", n, "_30flag"), 
               perc_miss = perc_miss_x)
  }
  
  # Bollinger band "percent B" applied to lagged price (quantifies price relative to the upper and lower Bollinger Band)
  X = addVar(X = X, var = BBands(quantmod::Lag(X$pL0, k = forecast_window))$pctB, 
             var_name = paste0("BBand_PctB_pL", forecast_window),
             perc_miss = perc_miss_x)
  
  # positive and negative run lengths (see 'RunLengths' in utility funs for exactly what this means)
  run_lengths = RunLength(rets = X[, key_name]) #extract lengths
  X = addVar(X = X, var = run_lengths$PosRunLength, 
             var_name = paste0("PosRunLength_", key_name),
             perc_miss = perc_miss_x)
  X = addVar(X = X, var = run_lengths$NegRunLength, 
             var_name = paste0("NegRunLength_", key_name),
             perc_miss = perc_miss_x)
  
  # flags for when day by day change (i.e., return!) is greater than 10%/less than -10%. Note 10% chosen from looking at one stock - seemed to get right number of obs to be able to learn from while being infrequent enough to be meaningful. Just looked at one stock though! 
  X = addVar(X = X, var = ifelse(X[, key_name] > 0.1, 1, 0),
             var_name = paste0("eventPos_", key_name), perc_miss = perc_miss_x)
  X = addVar(X = X, var = ifelse(X[, key_name] < -0.1, 1, 0),
             var_name = paste0("eventNeg_", key_name), perc_miss = perc_miss_x)
  
  
  # ---------------------------------------------------------------------
  #         REMOVE OBS PRIOR TO min_date
  # ---------------------------------------------------------------------
  
  # Remove any obs prior to min_date, and check if (after this) we have too many missing vals in the response. NB we do this now as otherwise we loose data, e.g. if min date is 1/1/12 and we calculate 20 day returns AFTER cropping, first 20 vals will be NA. If we do before, this wont be the case. Read description for why we do this before next block
  if (min(index(X)) < as.Date(min_date)) {
    X = X[paste0(min_date, "/")]} #remove any obs prior to min_date 
  y_miss = round(100 * sum(is.na(X$y))/nrow(X), 3) #perc missing
  if (y_miss > perc_miss_y) {
    stop ("You're response has more missing values (", y_miss ,"%) than you're stated cutoff (", perc_miss_y, "%)")
  } else if (sum(!is.na(X$y)) < min_obs_y) {
    stop ("You're response has fewer non-missing obs (", 
          sum(!is.na(X$y)) ,") than you're stated cutoff (", min_obs_y, 
          ")")
  } 
  
  # ---------------------------------------------------------------------
  #         EXTRACT KEY INDICES/COMMODITIES/EXCHANGE RATES
  # ---------------------------------------------------------------------
  
  # for key indices, add lagged price (as measure of economic strength). 
  for (k in 1:length(indices)) {
    dat = ExtractPrice(L$data[[which(names(L$data) == indices[k])]])
    sym_name = gsub("[[:punct:]]", "", indices[k]) #for variable naming
    X = addVar(X = X, var = quantmod::Lag(dat, k = forecast_window),
               var_name = paste0(sym_name, "_pL", forecast_window),
               perc_miss = perc_miss_x)
  }    
  
  # for key indices/commodities/exchange rates, compute 1-day lagged returns plus EMA of those returns for n=3 & 5. 
  key_syms = c(indices, commods, ERs)
  prefixs = c(rep("StockIndex", length(indices)), #for variable naming
              rep("Commod", length(commods)), rep("ER", length(ERs)))
  if (sym %in% key_syms) { #remove sym from key_syms if it's present
    i = which(key_syms == sym) 
    key_syms = key_syms[-i]; prefixs = prefixs[-i]
  }
  for (k in 1:length(key_syms)) {
    dat = ExtractPrice(L$data[[which(names(L$data) == key_syms[k])]])
    sym_name = gsub("[[:punct:]]", "", key_syms[k]) #for variable naming
    ncol_X = ncol(X) #save no. cols. Necessary because it's possible that the following line wont add a variable (if perc missing is too high), meaning we shouldn't run the rest of this loop (the EMAs). We can check by comparing ncol(X) before and after running addVar()
    X = addVar(X = X, var = ndayRets(quantmod::Lag(dat, k = forecast_window), 
                                     n = forecast_window), #add returns var
               var_name = paste0(prefixs[k], "_", sym_name, "_", key_name),
               perc_miss = perc_miss_x)
    if (ncol_X != ncol(X)) { #if we succeeded in adding the var 
      i = ncol(X) # add EMA vars. First save variable index
      for (n in c(3, 5, 10, 20)) { 
        X = addVar(X = X, var = EMA(X[, i], n = n), 
                   var_name = paste0("EMA_", prefixs[k], "_", sym_name, "_", 
                                     key_name, "_", n),
                   perc_miss = perc_miss_x)
      }
    }
  }
  
  
  # ---------------------------------------------------------------------
  #     CREATE INDICES FROM OTHER SYMBOLS IN SAME INDUSTRY/SECTOR
  # ---------------------------------------------------------------------
  
  # first extract any other syms in L from the same industries/sectors (if any)
  i = which(L$meta_data$Symbol == sym)
  if (!is.na(L$meta_data$Industry[i])) { #if industry is avaialble
    j = which(L$meta_data$Industry[-i] == L$meta_data$Industry[i])
    same_indust = L$meta_data$Symbol[-i][j]
  } else {
    same_indust = c() #still need this obj to exist
  }
  if (!is.na(L$meta_data$Sector[i])) { #if sector is avaialble
    j = which(L$meta_data$Sector[-i] == L$meta_data$Sector[i])
    same_sect = L$meta_data$Symbol[-i][j]
  } else {
    same_sect = c()
  }
  
  # Add industry index (by taking weighted average of the industry variables weighted by market cap as implemented in CreateIndex())
  if (length(same_indust) > 0) { #check same_indust contains symbols
    industryIndex = CreateIndex(syms = same_indust, n = forecast_window, #n is same arg as in nDayRets 
                                L = L, X = X, perc_miss = 10)
    if (!is.null(industryIndex)) { #if we didn't have 3 syms in same sector, CreateIndex returns 'NULL'
      X = addVar(X = X, var = quantmod::Lag(industryIndex, forecast_window), 
                 var_name = paste0("IndustryIndex_", key_name),
                 perc_miss = perc_miss_x)
      for (n in c(3, 5, 10, 20)) {   # Add EMA of sector index
        X = addVar(X = X, var = EMA(X[, paste0("IndustryIndex_", key_name)], n = n), 
                   var_name = paste0("IndustryIndex_EMA_", key_name, "_n", n),
                   perc_miss = perc_miss_x)
      }
    }
  }
  
  # Repeat for sector (TAKES ~30 SECS)
  if (length(same_sect) > 0) { 
    sectorIndex = CreateIndex(syms = same_sect, n = forecast_window, 
                              L = L, X = X, perc_miss = 10)
    if (!is.null(sectorIndex)) { 
      X = addVar(X = X, var = quantmod::Lag(sectorIndex, k = forecast_window), 
                 var_name = paste0("SectorIndex_", key_name),
                 perc_miss = perc_miss_x)
      for (n in c(3, 5, 10, 20)) {   
        X = addVar(X = X, var = EMA(X[, paste0("SectorIndex_", key_name)], n = n), 
                   var_name = paste0("SectorIndex_EMA_", key_name, "_n", n),
                   perc_miss = perc_miss_x)
      }
    }
  }
  
  # ---------------------------------------------------------------------
  #     SOME FINAL TASKS
  # ---------------------------------------------------------------------
  
  # Remove price at lag zero (didn't delete when defining response variables as we use this variable elsewhere in script. 
  X = X[, -which(colnames(X) == "pL0")]

  # Some vars have inf values (e.g. if we calculate return and the denom is zero in the raw data. Happens eg when computing return on volume, as raw vol is sometimes 0). 
  X[sapply(X, is.infinite)] = NA

  # Convert X to dataframe (can't include factors otherwise, and we want to include day of week)
  dates = index(X) #keep dates for rownames
  X = data.frame(X); row.names(X) = dates 
  
  # Add day of week, month and quarter as variables. MAYBE ADDING LATTER IS BAD IDEA - CANT REALLY LEARN PATTERNS IF ONLY HAVE A YEAR OF DATA, PLUS PATTERNS WILL BE STRONG JUST DUE TO AUTOCORRELATION
  X$Day = as.factor(weekdays(dates))
  # X$Month = as.factor(months(dates))
  # X$Quarter = as.factor(quarters(dates))
  
  # Output summary of the variables we have wrt nubmer of leading NAs/non-missing obs. NOTE THIS EXCLUDES NON-LEADING NAS AS WEVE IMPUTED THEM ALREADY
  M = LeadingNAsSummary(X = X, plot = plot_miss) 
  
  # Return key objs
  return(list("X" = X, "sym" = sym, "M" = M))
}


######################################################################
#                     addVar FUNCTION
######################################################################

addVar = function(X, var_name, var, perc_miss) {
  # Descritpion: adds a new variable "var" to an xts obj "X" and calls it "var_name" IF it has fewer than perc_miss *non-leading* NAs (else just returns X). Also interpolates (non-leading) NAs (after binding to X - i.e. leading NAs dont count if they only predate the first entry in X). NB doesnt check for leading NAs as I plan on doing that in the BuildDataset function.
  
  # Error Checks
  if(!all(c("xts", "zoo") %in% class(X)) | 
     !all(c("xts", "zoo") %in% class(var))) {
    stop ("'X' and 'var' should be of class xts")
  }
  
  # Add variable. Note we check if it meets the perc_missing criterion after adding for the reason in the description. 
  assign(var_name, var) # send variable to global env. and name 
  X = merge(X, get(var_name), join = "left") #add col
  colnames(X)[ncol(X)] = var_name
  
  # Check variable has < perc_miss *non-leading* NAs. If so remove and return X
  ind1 = which(!is.na(X[, ncol(X)]))[1] # first non-NA entry
  inds = ind1:nrow(X) # obs to interpolate across, i.e. all excl. leading NAs
  nas = sum(is.na(X[inds, ncol(X)])) #num. of non-leading NAs
  if (nas/length(inds) > perc_miss/100) { #perc of non-leading NAs
    warning (var_name, " was not added to X as it had ", 
             round(100 * nas/length(inds)), "% non-leading missing values. (cutoff: ", perc_miss, "%)")    
    X = X[, -ncol(X)]; invisible(X)
  } else { #else interpolate and print warning
    X[inds, ncol(X)] = na.interpolation(as.numeric(X[inds, ncol(X)]), 
                                        option = "linear")
    if (nas > 0) {
      warning (var_name, " had ", nas, " non-leading NAs. These have been interpolated")}
  }
  rm(var_name) #remove var from global env
  invisible(X)
}

######################################################################
#                     ndayRets FUNCTION
######################################################################

ndayRets = function(x, n = 1) {
  # Description: computes the n day returns on the vector/matrix x
  if (is.vector(x)) {
    N = length(x)
    rets = (as.numeric(x[-c(1:n)])/as.numeric(x[1:(N - n)])) - 1 #have to convert to numeric for some reason
    rets = as.xts(rets, order.by = index(x[-c(1:n)])) #have to add dates for matching
  } else if (is.matrix(x)) { #same code as above just modified for matrix
    N = nrow(x)
    rets = (as.matrix(x[-c(1:n)])/as.matrix(x[1:(N - n)])) - 1
    rets = as.xts(rets, order.by = index(x[-c(1:n)])) 
  } else {
    stop ("x should be a vetor or matrix")
  }
  return(rets)
}


######################################################################
#                     LeadingNAsSummary FUNCTION
######################################################################

LeadingNAsSummary = function(X, plot = T) {
  # Description: returns dataframe where first col is a variable in X and the other cols are the first non-NA date for that variable, the number of obs for that variable excl. leading NAs and the perc missing due to leading NAs. Also plots this info if requested
  # Args: X, the data frame, plot: if T plots date on y axis vs. variable on x.
  # Returns: see description 
  if (!class(X)[1] %in% c("xts", "data.frame")) {
    stop ("'X' should be an xts obj or data.frame")
  }
  inds = apply(X, 2, function(v) {which(!is.na(v))[1]}) # first non-na index
  FUN = ifelse(class(X)[1] == "xts", index, row.names) #fun for extracting dates differs depending on class of xts
  D = data.frame(colnames(X), FUN(X)[inds], nrow(X) - inds, 
                 round(100 * inds/nrow(X), 1))
  colnames(D) = c("Variable", "FirstDateExclLeadingNAs", 
                  "NumObsExclLeadingNAs", "PercMissDue2LeadingNAs")
  D = D[order(D$FirstDateExclLeadingNAs, decreasing = T), ]
  
  if (plot == T) {
    windows()
    par(mfrow = c(2, 1))
    plot(1:nrow(D), as.Date(D$FirstDate), 
         xlab = "Variable index\n(sorted by first non-NA)", 
         ylab = "First date",
         main = "First non-NA date for each variable")
    plot(1:nrow(D), D$NumObsExclLeadingNAs, 
         xlab = "Variable index\n(sorted by first non-NA)", 
         ylab = "Num. obs", ylim = c(0, max(D$NumObsExclLeadingNAs)),
         main = "Num obs for each variable (after imputing non-leading NAs)")
  }
  # cat("Worst 20 variables w.r.t leading NAs:")
  # print(head(D, 10))
  return(D)
}

######################################################################
#                     BivariatePvals FUNCTION
######################################################################

BivariatePvals = function(y, X, y_family, use_model_mat) {
  # Description: quick function to compute bivariate pvals for each column in X.
  # Args: y, X: the response and explanatory vars, y_family: binomial or gaussian.
  if (!y_family %in% c("gaussian", "binomial")) {
    stop ("y_family should be in gaussian or binomial")
  }
  inds = which(!is.na(y)) # Remove any missing vals
  if (length(inds) > 0) {y = y[inds]; X = X[inds, ]}
  if (use_model_mat == T) {
    X = model.matrix(~., data = X); X = X[, -1] #remove intercept
  }
  M = data.frame(colnames(X), NA); colnames(M) = c("Variable", "Pval")
  for (i in 1:ncol(X)) {
    tryCatch({
      f = glm(y ~ X[, i], family = y_family)
      if (is.numeric(X[, i])) {
        s = summary(f)$coefficients
        M$Pval[i] = s["X[, i]", grep("^Pr", colnames(s))]
      } else if (is.factor(X[, i])) {
        M$Pval[i] = anova(f, test = "Chisq")["X[, i]", "Pr(>Chi)"]
      }
    }, error = function(e) {
      M$Pval[i] = NA
    })
  }
  M = M[order(M$Pval, decreasing = F), ]
  invisible(M)
}


######################################################################
#                     BivariateROCs FUNCTION
######################################################################

#*** IF YOU HAVE TEST DATA AVAILABLE, USE 'BivariateAccurROC' INSTEAD
BivariateROCs = function(y, X, y_family, use_model_mat) {
  # Description: computes AUCs from ROC analysis for each bivariate pair (just a small modification to filterVarImp()). NB roc function returns AUC, but is sensitive to which response level is baseline. ie ROC of 0.05 actually means ROC of 0.95 ie always take 1-ROC if <0.5.
  # Args: y and X: response and explanatory variables. 
  # Returns: 2col dataframe of variable names and bivariate AUCs
  if (y_family != "binomial") {
    stop ("y_family should be binomial for ROC analysis")
  }
  if (use_model_mat == T) {
    X = model.matrix(~., data = X); X = X[, -1] #remove intercept
    X = as.data.frame(X) #filterVarImp needs dataframe
  }
  inds = which(!is.na(y)) # Remove any missing vals
  if (length(inds) > 0) {y = y[inds]; X = X[inds, ]}
  R = filterVarImp(x = X, y = y)
  R = data.frame(rownames(R), R[, 1]) 
  colnames(R) = c("Variable", "AUC")
  R$AUC = ifelse(R$AUC > 0.5, R$AUC, 1 - R$AUC) 
  R = R[order(R$AUC, decreasing = T), ]
  invisible(R)
}

######################################################################
#                     BivariateAccurROC FUNCTION
######################################################################

BivariateAccurROC = function(y, X, yte, Xte, y_family, use_model_mat) {
  # Description: quick function to compute bivariate balanced accuracy, accuracy and AUC for each column in X. NB we use TEST data to do this (which is how ROC here differs from BivariateROCs function)
  # Args: y, X: the response and explanatory vars, yte, Xte, same but for test data; y_family: binomial or gaussian.
  library(pROC)
  library(caret)
  if (y_family != "binomial") {
    stop ("y_family should be binomial")
  }
  if (!identical(colnames(X), colnames(Xte))) {
    stop("This code assumes colnames of X and Xte are identical")
  }
  inds = which(!is.na(y)) # Remove any missing vals
  if (length(inds) > 0) {y = y[inds]; X = X[inds, ]}
  if (use_model_mat == T) {
    X = model.matrix(~., data = X); X = X[, -1]; Xte = model.matrix(~., data = Xte); Xte = Xte[, -1]#remove intercept
  }
  M = data.frame(colnames(X), NA, NA, NA); colnames(M) = c("Variable", "BalancedAccur", "Accur", "AUC") 
  Xy = data.frame(X, y) #see call to glm for why this is necessary
  for (i in 1:ncol(X)) {
    tryCatch({
      f = glm(formula(paste0("y ~ ", colnames(X)[i])), data = Xy, family = y_family) #have to use this syntax so R can recognise ith var in Xte in call to predict
      yhats = predict(f, newdata = Xte, type = "response")
      thresh = quantile(yhats, 1 - mean(y)) # Set threshold for preds such that same proportion of test obs predicted '1' as observed in training data
      yhats = ifelse(yhats > thresh, 1, 0) #mean(yhats); mean(y) #checking
      M$AUC[i] = pROC::auc(pROC::roc(predictor = yhats, response = yte))
      yte = factor(yte, levels = c(0, 1)); yhats = factor(yhats, levels = c(0, 1)) #else get warning in next line      
      C = confusionMatrix(data = yhats, reference = yte)
      M$BalancedAccur[i] = C$byClass["Balanced Accuracy"]
      M$Accur[i] = C$overall["Accuracy"]
    }, error = function(e) {
      print("Error in BivariateAccurROC")
      # M$Pval[i] = NA
    })
  }
  M = M[order(M$BalancedAccur, decreasing = T), ]
  invisible(M)
}

######################################################################
#                     BivariateMAE FUNCTION
######################################################################

BivariateMAE = function(y, X, yte, Xte, y_family, use_model_mat) {
  # Description: quick function to compute bivariate mean abs error (MAE), RMSE and median abs error for each column in X. NB we use TEST data to do this
  # Args: y, X: the response and explanatory vars, yte, Xte, same but for test data; y_family: binomial or gaussian.
  if (y_family != "gaussian") {
    stop ("y_family should be gaussian")
  }
  if (!identical(colnames(X), colnames(Xte))) {
    stop("This code assumes colnames of X and Xte are identical")
  }
  RMSE = function(actual, predicted) sqrt(mean((actual - predicted)^2, na.rm = T)) # Define some prelim functions 
  MAE = function(actual, predicted) mean(abs(actual - predicted), na.rm = T)
  MedAE = function(actual, predicted) median(abs(actual - predicted), na.rm = T)
  inds = which(!is.na(y)) # Remove any missing vals
  if (length(inds) > 0) {y = y[inds]; X = X[inds, ]}
  if (use_model_mat == T) {
    X = model.matrix(~., data = X); X = X[, -1]; Xte = model.matrix(~., data = Xte); Xte = Xte[, -1]#remove intercept
  }
  M = data.frame(colnames(X), NA, NA, NA); colnames(M) = c("Variable", "MAE", "MedianAE", "RMSE") 
  Xy = data.frame(X, y) #see call to glm for why this is necessary
  for (i in 1:ncol(X)) {
    tryCatch({
      f = glm(formula(paste0("y ~ ", colnames(X)[i])), data = Xy, family = y_family) #have to use this syntax so R can recognise ith var in Xte in call to predict
      yhats = predict(f, newdata = Xte)
      M$MAE[i] = MAE(actual = yte, predicted = yhats)
      M$MedianAE[i] = MedAE(actual = yte, predicted = yhats)
      M$RMSE[i] = RMSE(actual = yte, predicted = yhats)
    }, error = function(e) {
      print("Error in BivariateMAE")
    })
  }
  M = M[order(M$MedianAE, decreasing = F), ]
  invisible(M)
}


######################################################################
#                     GlmnetFun FUNCTION
######################################################################

GlmnetFun = function(y, X, yte, Xte, y_family, nfolds = 5, lambda = "lambda.min", use_model_mat) {
  # Description: quick function to perform variable selection via glmnet. Also outputs test set performance metrics
  # Args: y, X: the response and explanatory vars (X can contain the response too, but onyl if it's called 'y'), yte, Xte, same but for test data; y_family: binomial or gaussian, nfolds: same as in glmnet; lambda: either "lambda.min" or "lambda.1se" (for input "s" into predict.cv.glmnet)
  # Returns: list with the selected variables as one element, and as the second a dataframe of test set performance metrics (eg balanced accuracy if binomial, median absolute error if gaussian)
  library(glmnet)  
  library(pROC) #AUC
  library(caret) #confMat
  
  # Prelims  
  if (!y_family %in%  c("binomial", "gaussian")) {
    stop ("y_family should be binomial or gaussian")
  }
  if (!identical(colnames(X), colnames(Xte))) {
    stop("This code assumes colnames of X and Xte are identical")
  }
  if (!lambda %in% c("lambda.1se", "lambda.min")) {
    stop ("lambda should be in lambda.1se or lambda.min")
  }
  factors = colnames(X)[sapply(X, is.factor)] 
  if (length(factors) > 0 & use_model_mat == F) {
    if (!all(factors %in% c("Day", "Month", "Quarter"))) {
      stop ("This code assumes the only factors are Day, Month and Quarter. This is because when converting model matrix names to original colnames, the variable 'tranisitions' have been hard-coded")
    }
  }
  inds = which(!is.na(y)) # Remove any missing vals
  if (length(inds) > 0) {
    y = y[inds]; X = X[inds, ]}
  if ("y" %in% colnames(X)) { #remove response from X/Xte (if present)
    X = X[, -which(colnames(X) == "y")]; Xte = Xte[, -which(colnames(Xte) == "y")]}
  X = model.matrix(~., data = X); X = X[, -1]; Xte = model.matrix(~., data = Xte); Xte = Xte[, -1]#remove intercept
  
  # Fit model and store final variables selected and their coefs
  f = cv.glmnet(x = X, y = y, family = y_family, nfolds = nfolds) # fit model
  M = data.frame("Variable" = dimnames(coef(f, lambda = lambda))[[1]], 
                 "Estimate" = round(matrix(coef(f, lambda = lambda)), 5))
  M = M[M$Estimate > 0, ] #remove nonzero coefs
  
  # If we dont want in model matrix form, have to replace e.g. 'MonthMarch' with e.g. 'Month'. Below loop achieves this. 
  if (use_model_mat == F) {
    M$Variable = as.character(M$Variable)
    for (v in c("Day", "Month", "Quarter")) {
      inds = grep(paste0("^", v), as.character(M$Variable))
      if (length(inds) > 0) {
        if (length(inds) > 1) {
          M = M[-inds[-1], ] # remove multiple eg months etc if there are multiple ones
        }
        M$Variable[inds[1]] = v #replace remaining eg month (which will be eg MonthMay with Month)
        M$Variable[inds[1]] = NA #we no longer have a single coefficient for this variable
      }
    }
  }

  # Save some (test data) model metrics
  yhats = as.numeric(predict(f, s = lambda, newx = Xte, type = "response")) #test set preds
  if (y_family == "binomial") {
    thresh = quantile(yhats, 1 - mean(y)) # Set threshold for preds such that same proportion of test obs predicted '1' as observed in training data
    yhats2 = ifelse(yhats > thresh, 1, 0) #mean(yhats); mean(y) #checking
    AUC = pROC::auc(pROC::roc(predictor = yhats2, response = yte))
    yte = factor(yte, levels = c(0, 1)); yhats2 = factor(yhats2, levels = c(0, 1)) #else get warning in next line      
    C = confusionMatrix(data = yhats2, reference = yte)
    BalancedAccur = C$byClass["Balanced Accuracy"]
    Accur = C$overall["Accuracy"]
    W = data.frame("Metric" = c("BalancedAccur", "Accur", "AUC"), 
                   "Score" = as.numeric(c(BalancedAccur, Accur, AUC)))
  } else if (y_family == "gaussian") {
    RMSE = sqrt(mean((yte - yhats)^2, na.rm = T))
    MAE = mean(abs(yte - yhats), na.rm = T)
    MedianAE = median(abs(yte - yhats), na.rm = T)
    W = data.frame("Metric" = c("MedianAE", "MAE", "RMSE"), 
                   "Score" = as.numeric(c(MedianAE, MAE, RMSE)))
  }
  
  # Also save yhats and yte in case we want to use for other testing
  Y = data.frame("Y_test" = yte, "Yhats_test" = yhats)
  
  return(list("SelectedVariables" = M, "TestSetMetrics" = W, "Preds" = Y))
}


######################################################################
#                     StepAICFun FUNCTION
######################################################################

StepAICFun = function(y, X, yte, Xte, y_family) {
  # Description: quick function to perform variable selection via forward selection. BASED OFF GLMNETFUN Also outputs test set performance metrics
  # Args: y, X: the response and explanatory vars (X can contain the response too, but onyl if it's called 'y'), yte, Xte, same but for test data; y_family: binomial or gaussian, 
  # Returns: list with the selected variables as one element, and as the second a dataframe of test set performance metrics (eg balanced accuracy if binomial, median absolute error if gaussian)
  library(MASS)  
  library(pROC) #AUC
  library(caret) #confMat
  
  # Prelims  
  if (!y_family %in%  c("binomial", "gaussian")) {
    stop ("y_family should be binomial or gaussian")
  }
  if (!identical(colnames(X), colnames(Xte))) {
    stop("This code assumes colnames of X and Xte are identical")
  }
  inds = which(!is.na(y)) # Remove any missing vals
  if (length(inds) > 0) {
    y = y[inds]; X = X[inds, ]}
  if ("y" %in% colnames(X)) { #remove response from X/Xte (if present)
    X = X[, -which(colnames(X) == "y")]; Xte = Xte[, -which(colnames(Xte) == "y")]}
  
  # Fit model and store final variables selected and their coefs
  f_lo = glm(y ~ 1, data = X, family = y_family)
  f_hi = glm(y ~ ., data = X, family = y_family)
  f = stepAIC(f_lo, scope = list("lower" = f_lo, "upper" = f_hi), direction = "forward", trace = F)
  predictors = names(coefficients(f))
  predictors = predictors[-which(predictors == "(Intercept)")]
  
  # Save some (test data) model metrics
  yhats = as.numeric(predict(f, newdata = Xte, type = "response")) #test set preds
  if (y_family == "binomial") {
    thresh = quantile(yhats, 1 - mean(y)) # Set threshold for preds such that same proportion of test obs predicted '1' as observed in training data
    yhats2 = ifelse(yhats > thresh, 1, 0) #mean(yhats); mean(y) #checking
    AUC = pROC::auc(pROC::roc(predictor = yhats2, response = yte))
    yte = factor(yte, levels = c(0, 1)); yhats2 = factor(yhats2, levels = c(0, 1)) #else get warning in next line      
    C = confusionMatrix(data = yhats2, reference = yte)
    BalancedAccur = C$byClass["Balanced Accuracy"]
    Accur = C$overall["Accuracy"]
    W = data.frame("Metric" = c("BalancedAccur", "Accur", "AUC"), 
                   "Score" = as.numeric(c(BalancedAccur, Accur, AUC)))
  } else if (y_family == "gaussian") {
    RMSE = sqrt(mean((yte - yhats)^2, na.rm = T))
    MAE = mean(abs(yte - yhats), na.rm = T)
    MedianAE = median(abs(yte - yhats), na.rm = T)
    W = data.frame("Metric" = c("MedianAE", "MAE", "RMSE"), 
                   "Score" = as.numeric(c(MedianAE, MAE, RMSE)))
  }
  
  # Also save yhats and yte in case we want to use for other testing
  Y = data.frame("Y_test" = yte, "Yhats_test" = yhats)
  
  return(list("SelectedVariables" = predictors, "TestSetMetrics" = W, "Preds" = Y))
}


######################################################################
#                     RpartFun FUNCTION
######################################################################

RpartFun = function(y, X, yte, Xte, y_family, nfolds, prop_train) {
  # Description: quick function to perform variable selection via rpart BASED OFF GLMNETFUN Also outputs test set performance metrics
  # Args: y, X: the response and explanatory vars (X can contain the response too, but onyl if it's called 'y'), yte, Xte, same but for test data; y_family: binomial or gaussian, prop_train/nfolds: args as per MakeTimeSlices 
  # Returns: list with the selected variables as one element, and as the second a dataframe of test set performance metrics (eg balanced accuracy if binomial, median absolute error if gaussian)
  library(pROC) #AUC
  library(caret) #confMat
  
  # Prelims  
  if (!y_family %in%  c("binomial", "gaussian")) {
    stop ("y_family should be binomial or gaussian")
  }
  if (!identical(colnames(X), colnames(Xte))) {
    stop("This code assumes colnames of X and Xte are identical")
  }
  inds = which(!is.na(y)) # Remove any missing vals
  if (length(inds) > 0) {
    y = y[inds]; X = X[inds, ]}
  if ("y" %in% colnames(X)) { #remove response from X/Xte (if present)
    X = X[, -which(colnames(X) == "y")]; Xte = Xte[, -which(colnames(Xte) == "y")]}
  
  # Fit model and store final variables selected and their coefs
  slices = MakeTimeSlices(nfolds = nfolds, prop_train = prop_train, discrete = T)   # Make indices to be used in resampling
  if (y_type == "binary") {   # Set up train control
    y = factor(ifelse(y == 1, "up", "down"))
    yte = factor(ifelse(yte == 1, "up", "down"))
    ctrl = trainControl(classProbs = T, index = slices$train, 
                        indexOut = slices$test,
				preProcOptions = list('freqCut' = 99/1),
                        summaryFunction = twoClassSummary, verboseIter = F)
    caret_metric = "ROC"
  } else { 
    ctrl = trainControl(index = slices$train, 
                        indexOut = slices$test, verboseIter = F,
				preProcOptions = list('freqCut' = 99/1))
    caret_metric = "RMSE" ## THINK ABOUT THIS ONE - MITE WANT TO USE SOMETHING ELSE
  }
  f = train (y = y, x = X, method = "rpart", tuneLength = 40, #fit model. NB tuneLength is quick
             metric = caret_metric,
             trControl = ctrl,
             preProcess = c("scale", "center"))
  
  # Save some (test data) model metrics
  yhats = predict(f, Xte) #test set preds
  if (y_family == "binomial") {
    yte2 = (ifelse(yte == "up", 1, 0))
    yhats2 = (ifelse(yhats == "up", 1, 0))
    AUC = pROC::auc(pROC::roc(predictor = yhats2, response = yte2))
    C = confusionMatrix(data = yhats2, reference = yte2)
    BalancedAccur = C$byClass["Balanced Accuracy"]
    Accur = C$overall["Accuracy"]
    W = data.frame("Metric" = c("BalancedAccur", "Accur", "AUC"), 
                   "Score" = as.numeric(c(BalancedAccur, Accur, AUC)))
  } else if (y_family == "gaussian") {
    RMSE = sqrt(mean((yte - yhats)^2, na.rm = T))
    MAE = mean(abs(yte - yhats), na.rm = T)
    MedianAE = median(abs(yte - yhats), na.rm = T)
    W = data.frame("Metric" = c("MedianAE", "MAE", "RMSE"), 
                   "Score" = as.numeric(c(MedianAE, MAE, RMSE)))
  }
  
  # Also save yhats and yte in case we want to use for other testing
  Y = data.frame("Y_test" = yte, "Yhats_test" = yhats)
  
  return(list("SelectedVariables" = predictors(f), "TestSetMetrics" = W, "Preds" = Y))
}



######################################################################
#                     RunLength FUNCTION
######################################################################

RunLength = function(rets) {
  # Function to return the positive and negative run length for a series of returns. e.g. if the SIGN of the returns is 0, 1, 1, 1, -1, 1, pos run length is 0, 1, 2, 3, 0, 1; neg run length is 0, 0, 0, 0, 1, 0.
  # Args: a returns vector
  # returns: 2 col dataframe with the pos and neg run length. 
  
  # Quick error check
  if (class(rets)[1] != "xts") {
    stop ("rets should be class xts")
  }
  
  # Extract run lengths and values. see ?rle for details
  run_dat = rle(sign(as.numeric(rets)))
  
  # 'expand' run lengths (run_dat contains e.g. '3' if there 3 "1"s, but we want it to read "1, 2, 3" since "3" means the first 1 was of run length 1, the second was length 2 and third is 3.)
  run_lengths = c()
  for (i in 1:length(run_dat$lengths)) {
    run_lengths = c(run_lengths, 1:run_dat$lengths[i])
  }
  
  # 'expand' run values
  run_vals = c()
  for (i in 1:length(run_dat$values)) {
    run_vals = c(run_vals, rep(run_dat$values[i], run_dat$lengths[i]))
  }
  
  # Extract positive run lengths
  pos_runs = numeric(length(run_vals))
  i = which(run_vals == 1)
  pos_runs[i] = run_lengths[i]
  
  # Extract negative run lengths
  neg_runs = numeric(length(run_vals))
  i = which(run_vals == -1)
  neg_runs[i] = run_lengths[i]
  
  # Merge and return
  Z = merge(as.xts(pos_runs, order.by = index(rets)),
            as.xts(neg_runs, order.by = index(rets)))
  colnames(Z) = c("PosRunLength", "NegRunLength")
  if (!identical(index(Z), index(rets))) {
    stop ("Index error")
  } else {
    return(Z)
  }
}


######################################################################
#           ProcessMarketCap FUNCTION
######################################################################

ProcessMarketCap = function(caps) {
  # Description: quick function to convert 'market cap' from 'stockSymbols$MarketCap' to a numeric form (vs "$4.48B"   "$198.11M" etc).
  # Args: the market caps from stockSymbols$MarketCap
  # REturns: the caps in numeric form
  if (length(grep("\\$", caps)) != length(caps)) { # quick check
    stop("This code assumes caps is formatted as characters starting with '$'")
  }
  as.numeric(sub("\\$(\\d+(\\.\\d+)?)[A-Z]?", "\\1", caps)) * 
    ifelse(gsub("[^A-Z]", "", caps) == "M", 1e6,
           ifelse(gsub("[^A-Z]", "", caps) == "B", 1e9, 1.0))
}

######################################################################
#                     CreateIndex FUNCTION
######################################################################

CreateIndex = function(syms, n, L, X, perc_miss = 10) {
  # Description: creates an index of the variables listed in syms by taking a weighted average of each weighted by their market cap (as listed in output from call to stockSymbols())
  # Args: syms: the symbosl from which we'll make the index; n, the parameter of the same name in ndayReturns (i.e when we compute returns, do it over n days); X, the data to which we'll ultimately bind the index (only used as 'scaffolding' such that we can call our addData function which does imputation etc); L, the output of running DonwloadData, perc_miss: max % missing before we dont include the variable in the index
  # Returns: a weighted index of nday returns for the symbols, weighted by market cap
  
  #**********************************************************************
  # NOTE: THE CREATE INDEX FUNCTION DOES NOT LAG THE DATA - THIS MUST BE DONE AFTER (EG THE INDEX AT t IS SAME AS RESPONSE AT t, NOT AT t-1 AS WOULD BE DESIRED")
  #**********************************************************************
  
  # Some error checks
  if (class(L) != "list") {
    stop ("L should be a list")
  } 
  if (!all(names(L) == c("data", "meta_data"))) {
    stop ("L should have elements 'data' and 'meta_data' (as output from running the function DownloadData")
  }
  if (!all(unlist(lapply(L$data, function(x) {class(x)[1]})) == "xts")) {
    stop ("All elements of L$data should be xts objs")
  }
  if (!all(syms %in% names(L$data))) {
    stop("all syms should be in names(L$data)")
  }
  if (!all(syms %in% L$meta_data$Symbol)) {
    stop ("Each variable name in 'syms' should contain a symbol which occurs in L$meta_data$Symbol")
  }
  if (sum(duplicated(syms) > 0)) {
    stop("Each symbol should only occur once")
  }
  if (!all(c("Symbol", "SRC", "Name", "MarketCap", "Sector", "Industry") 
           %in% colnames(L$meta_data))) {
    stop ("Symbol, SRC, Name, MarketCap, Sector, & Industry should be colnames of L$meta_data") 
  }
  
  # Extract one variable to act as 'scaffolding' against which we'll add the symbosl in sym 
  Z = X[, 1]
  
  # Extract the symbols and bind to Z
  inds = paste0(index(Z)[1], "/") #how you index 'from starting date to end' in xts
  for (i in 1:length(syms)) {
    dat = ExtractPrice(L$data[[which(names(L$data) == syms[i])]])
    dat = dat[inds] #excl any obs before first index in Z
    if (nrow(dat) > 0) { #sometimes get prob with above row (can remove all rows, which means following if statement gives error). This is workaround
      dat[dat == 0] = NA  # assume zeros are errors. cause probs as when we compute returns they can become Inf
      if (index(dat)[1] == index(Z)[1] & 
          index(dat)[length(dat)] == index(Z)[nrow(Z)]) { #only incl. vars of same length as Z
        Z = addVar(X = Z, var = dat, var_name = syms[i], 
                   perc_miss = perc_miss) 
      }
    }
  }
  if (ncol(Z) == 1)  {# if we didn't add any variables
    return(NULL)
  } else {
    Z = Z[, -1] #remove the scaffolding
    
    # Extract market caps
    caps = L$meta_data[match(syms, L$meta_data$Symbol), "MarketCap"] 
    names(caps) = syms
    caps = caps[!is.na(caps)] #sometimes get NA if a symbol has no market cap
    caps = ProcessMarketCap(caps) #have to convert to numeric
    
    # Subset Z and reorder to be same as caps
    Z = Z[, which(colnames(Z) %in% names(caps))] #removes any vars from Z that didnt have a market cap
    caps = caps[match(colnames(Z), names(caps))] 
    if (!all(names(caps) == colnames(Z))) {
      stop("caps and colnames(Z) should now be same order")
    }
    # Convert Z from prices to nday returns
    Z = ndayRets(Z, n = n)
    
    # Calculate weighted mean, but only if we have at least 3 stocks in same industry/sector etc 
    if (length(caps) >= 3) {
      wm = apply(Z, 1, function(x) {
        weighted.mean(x, caps, na.rm = T)
      })
      wm = as.xts(wm, order.by = index(Z)) #convert to xts obj
      wm[is.nan(wm)] = NA #convert NaNs to NA (necessary form for eg EMA())
    } else {
      wm = NULL
    }
    return(wm)
  }
}

######################################################################
#                     ExtractPrice FUNCTION
######################################################################

ExtractPrice = function(X, colname = "pL0") {
  # Description: very quick function to extract the price column from a given xts obj, together with some checks. Takes 'adjusted' price if present, else assumes the sole column is price. Purpose is that depending on where we download data from during getSymbols(), we might have one price column (eg for exchange rates), or multiple cols (open, close, adjusted etc, eg for yahoo stocks). The function returns a 1 col xts dataframe with the 'right' price col.
  if (class(X)[1] != "xts") {
    stop ("X should be an xts dataframe")
  }
  ind = grep("Adjusted", colnames(X))
  if (length(ind) == 1) {
    X = X[, ind] 
  } else if (ncol(X) != 1) {
    stop("X should either have a colname 'Adjusted' (if it's a stock from yahoo) or should only contain one column (price, if it's an exchange rate/metal from oanda)")
  }
  colnames(X) = colname  
  return(X)
}

######################################################################
#                     SPlitTrainTestVal FUNCTION
######################################################################

SplitTrainTestVal = function(train_prop, test_prop, val_prop, X) {
  # Description: splits 1:nrow(X) into a set of (chronologically-ordered) train, test and validation indices
  # Args: X, the dataset; train/test/val_inds: obvious
  # Returns: a list wtih train_inds, test_inds and val_inds
  if (round(sum (train_prop, test_prop, val_prop), 5) != 1) {
    stop ("train_prop, test_prop and val_prop should sum to 1")
  }
  all_inds = 1:nrow(X)
  train_inds = all_inds[1:ceiling(nrow(X) * train_prop)]
  val_inds = rev(rev(all_inds)[1:ceiling(nrow(X) * val_prop)])
  test_inds = all_inds[!all_inds %in% c(train_inds, val_inds)]
  if (length(intersect(train_inds, test_inds)) != 0 |
      length(intersect(train_inds, val_inds)) != 0 |
      length(intersect(val_inds, test_inds)) != 0) {
    stop ("There's overlap betwween train, test and validation indices")
  } 
  return(list("train_inds" = train_inds, "test_inds" = test_inds, 
              "val_inds" = val_inds))
}


######################################################################
#                     LiftFun FUNCTION
######################################################################

LiftFun = function(actual, pred, plots = T) {
  # Description: compute cumulative and noncumulative lift and plot. NOTE COMPUTES BOTH 'UP' AND 'DOWN' LIFT, I.E. WHERE '1' IN ACTUAL (AND THING WE'RE TRYING TO PREDICT IS 'UP' AND 'DOWN', RESPECTIVELY)
  # Args: actual, pred: same args to 'gains' function. NOTE 'pred' PROB SHOULD BE PROB OF 'UP', plots: should we plot
  # REturns: plot of lift when 10, 20 and 50 groups and corresponding data plus bin sizes
  
  warning("Make sure pred is prob of 'up' not prob of 'down")
  # Covnert actual to nuemric (needed for gains function)
  if (!all(actual %in% c("up", "down", NA))) {
    stop("actual should take values 'up', 'down'")
  }
  actual = ifelse(actual == "up", 1, 0)
  
  # Not sure I trust the gains lift function - doesnt seem to aggregate into eg 50 wwhen you specify groups = 50. So, creating my own. Args are same as in gains fun
  ComputeLift = function(actual, pred, bin.size) {
    D = data.frame(actual, pred)
    D$last.obs = c(rep(0, nrow(D) - 1), 1) #record which is last obs
    D = D[order(D$pred, decreasing = T), ] #order by preds
    n_groups = round(nrow(D)/bin.size)
    D$group = cut(1:length(actual), n_groups) #break into 'groups' groups
    
    # compute cumulative lift 
    fun = function(x) { #returns no 1s and no. obs in vector x 
      cbind(sum(x == 1, na.rm = T), length(x))
    }
    A = aggregate(actual ~ group, data = D, fun)
    A = data.frame(A[, 1], A[, 2])
    colnames(A) = c("group", "No_1s", "No_obs")
    model_props = cumsum(A$No_1s)/cumsum(A$No_obs) #cum proportion of '1's up to and including ith bin when ranked by model
    base_prop = sum(A$No_1s, na.rm = T)/sum(A$No_obs, na.rm = T) #base proportion of 1s in whole data
    cum.lifts = model_props/base_prop
    
    # compute non-cumulative lift
    A = aggregate(actual ~ group, data = D, mean, na.rm = T)
    colnames(A) = c("group", "meanOf1s")
    non.cum.lifts = A$meanOf1s/mean(D$actual, na.rm = T)
    
    # Find group in which last obs lies
    last.obs.grp = which(by(D, D$group, function(x) {
      any(x$last.obs == 1, na.rm = T)
    }))
    
    return(list("cum.lifts" = cum.lifts, 
                "noncum.lifts" = non.cum.lifts, 
                "bin.size" = round(nrow(D)/n_groups),
                "last.obs.grp" = last.obs.grp))
  }
  
  #Compute lift for 10, 20 and 50 groups
  up5 = ComputeLift(actual, pred, bin.size = 5)
  up10 = ComputeLift(actual, pred, bin.size = 10)
  up20 = ComputeLift(actual, pred, bin.size = 20)
  down5 = ComputeLift(1 - actual, 1 - pred, bin.size = 5)
  down10 = ComputeLift(1 - actual, 1 - pred, bin.size = 10)
  down20 = ComputeLift(1 - actual, 1 - pred, bin.size = 20)
  
  # Plot
  if (plots == T) {
    
    #extract ylims
    lifts = c(up5, up10, up20, down5, down10, down20)
    lifts = lifts[-which(names(lifts) %in% c("bin.size", "last.obs.grp"))] 
    ylims = c(min(unlist(lifts), na.rm = T), 
              max(unlist(lifts), na.rm = T))
    
    # Simple prelim function as we have a lot of repetition 
    plotfun = function(cumlift, noncumlift, bin.size, last.obs.grp,
                       main, col = "green") {
      plot(1:length(cumlift), cumlift, type = "l",
           col = alpha(col, 0.5), lwd = 2, ylab = "Lift",
           main = main,
           xlab = paste0("Bin (", length(cumlift), " bins total)"),
           ylim = ylims)
      lines(1:length(cumlift), rep(1, length(cumlift)),
            lty = 5, col = "grey")
      points(1:length(noncumlift), noncumlift,
             col = col)
      points(last.obs.grp, noncumlift[last.obs.grp], col = "red",
             pch = 16)
      legend("topright", c("cum. lift", "noncum. lift", "last obs group"),  
             col = c(col, col, "red"), pch = c(45, 1, 1), 
             bty = "n", cex = 0.9)
    }
    
    # Call plotfun for various comobs
    windows(h = 10, w = 15)
    par(mai = c(0.6, 0.82, 0.4, 0.42)) #see par()$mai for defaults
    mat = matrix(c(1, 1, 2, 3, 4, 5, 6, 7), ncol = 2, byrow = T)
    layout(mat, heights = c(0.8, 1.8, 1.8, 1.8))
    plot.new()
    mtext("LIFT", side = 1, cex = 2, font = 2)
    plotfun(cumlift = up20$cum.lifts, 
            noncumlift = up20$noncum.lifts,
            main = "Lift, 1 = 'up', bin size = 20", 
            bin.size = up20$bin.size, last.obs.grp = up20$last.obs.grp)
    plotfun(cumlift = down20$cum.lifts,
            noncumlift = down20$noncum.lifts,
            main = "Lift, 1 = 'down', bin size = 20", 
            bin.size = down20$bin.size, last.obs.grp = down20$last.obs.grp)
    plotfun(cumlift = up10$cum.lifts,
            noncumlift = up10$noncum.lifts,
            main = "Lift, 1 = 'up', bin size = 10", 
            bin.size = up10$bin.size, last.obs.grp = up10$last.obs.grp)
    plotfun(cumlift = down10$cum.lifts,
            noncumlift = down10$noncum.lifts,
            main = "Lift, 1 = 'down', bin size = 10",
            bin.size = down10$bin.size, last.obs.grp = down10$last.obs.grp)
    plotfun(cumlift = up5$cum.lifts,
            noncumlift = up5$noncum.lifts,
            main = "Lift, 1 = 'up', bin size = 5",
            bin.size = up5$bin.size, last.obs.grp = up5$last.obs.grp)
    plotfun(cumlift = down5$cum.lifts, 
            noncumlift = down5$noncum.lifts,
            main = "Lift, 1 = 'down', bin size = 5", 
            bin.size = down5$bin.size, last.obs.grp = down5$last.obs.grp)
  }
  print("NB the function will return cum and non.cum lift and bin sizes if you assign the output to an obj")
  invisible(list("up5" = up5, "up10" = up10, "up20" = up20,
                 "down5" = down5, "down10" = down10, 
                 "down20" = down20))
}


######################################################################
#                     PseudoLiftFun FUNCTION
######################################################################

PseudoLiftFun = function(actual, pred, plots = T) {
  # Description: Just like "LiftFun" but doesnt compute lift but rather somthing in between lift and accuracy. Specifically it computes/plots the cumulative and non-cumulative proportion of "positives" (i.e., up if plotting up, down if plotting down) vs. no. bins. Basically just lift but instead of dividing this proportion by base proportion we leave as is - which also means it's like accuracy, except with accuracy if propensity score was <threshold we'd predict "0"/"negative" - here we just take proportion of "positives" per bin. 
  # Args: actual, pred: same args to 'gains' function. NOTE 'pred' PROB SHOULD BE PROB OF 'UP', plots: should we plot
  # REturns: plot of lift when 10, 20 and 50 groups and corresponding data plus bin sizes
  
  warning("Make sure pred is prob of 'up' not prob of 'down")
  # Covnert actual to nuemric (needed for gains function)
  if (!all(actual %in% c("up", "down", NA))) {
    stop("actual should take values 'up', 'down'")
  }
  actual = ifelse(actual == "up", 1, 0)
  
  # Not sure I trust the gains lift function - doesnt seem to aggregate into eg 50 wwhen you specify groups = 50. So, creating my own. Args are same as in gains fun
  ComputeLift = function(actual, pred, bin.size) {
    D = data.frame(actual, pred)
    D$last.obs = c(rep(0, nrow(D) - 1), 1) #record which is last obs
    D = D[order(D$pred, decreasing = T), ] #order by preds
    n_groups = round(nrow(D)/bin.size)
    D$group = cut(1:length(actual), n_groups) #break into 'groups' groups
    
    # compute cumulative lift 
    fun = function(x) { #returns no 1s and no. obs in vector x 
      cbind(sum(x == 1, na.rm = T), length(x))
    }
    A = aggregate(actual ~ group, data = D, fun)
    A = data.frame(A[, 1], A[, 2])
    colnames(A) = c("group", "No_1s", "No_obs")
    model_props = cumsum(A$No_1s)/cumsum(A$No_obs) #cum proportion of '1's up to and including ith bin when ranked by model
    base_prop = sum(A$No_1s, na.rm = T)/sum(A$No_obs, na.rm = T) #base proportion of 1s in whole data
    cum.lifts = model_props
    
    # compute non-cumulative lift
    A = aggregate(actual ~ group, data = D, mean, na.rm = T)
    colnames(A) = c("group", "meanOf1s")
    non.cum.lifts = A$meanOf1s
    
    # Find group in which last obs lies
    last.obs.grp = which(by(D, D$group, function(x) {
      any(x$last.obs == 1, na.rm = T)
    }))
    
    return(list("cum.lifts" = cum.lifts, 
                "noncum.lifts" = non.cum.lifts, 
                "bin.size" = round(nrow(D)/n_groups),
                "last.obs.grp" = last.obs.grp))
  }
  
  #Compute lift for 10, 20 and 50 groups
  up5 = ComputeLift(actual, pred, bin.size = 5)
  up10 = ComputeLift(actual, pred, bin.size = 10)
  up20 = ComputeLift(actual, pred, bin.size = 20)
  down5 = ComputeLift(1 - actual, 1 - pred, bin.size = 5)
  down10 = ComputeLift(1 - actual, 1 - pred, bin.size = 10)
  down20 = ComputeLift(1 - actual, 1 - pred, bin.size = 20)
  
  # Plot
  if (plots == T) {
    
    #extract ylims and base proportion of positives for plot
    lifts = c(up5, up10, up20, down5, down10, down20)
    lifts = lifts[-which(names(lifts) %in% c("bin.size", "last.obs.grp"))] 
    ylims = c(min(unlist(lifts), na.rm = T), 
              max(unlist(lifts), na.rm = T))
    base_prop = mean(actual, na.rm = T)
    
    # Simple prelim function as we have a lot of repetition 
    plotfun = function(cumlift, noncumlift, bin.size, last.obs.grp,
                       main, col = "orange", base_prop) {
      plot(1:length(cumlift), cumlift, type = "l",
           col = alpha(col, 0.5), lwd = 2, ylab = "Lift",
           main = main,
           xlab = paste0("Bin (", length(cumlift), " bins total)"),
           ylim = ylims)
      lines(1:length(cumlift), rep(base_prop, length(cumlift)),
            lty = 5, col = "grey")
      points(1:length(noncumlift), noncumlift,
             col = col)
      points(last.obs.grp, noncumlift[last.obs.grp], col = "red",
             pch = 16)
      legend("topright", c("cum. lift", "noncum. lift", "last obs group", 
                           "base proportion positives"),  
             col = c(col, col, "red", "grey"), pch = c(45, 1, 1, 45), 
             bty = "n", cex = 0.9)
    }
    
    # Call plotfun for various comobs
    windows(h = 10, w = 15)
    par(mai = c(0.6, 0.82, 0.4, 0.42)) #see par()$mai for defaults
    mat = matrix(c(1, 1, 2, 3, 4, 5, 6, 7), ncol = 2, byrow = T)
    layout(mat, heights = c(0.8, 1.8, 1.8, 1.8))
    plot.new()
    mtext("PROPORTION OF '1s' VS BIN", side = 1, cex = 2, font = 2)
    plotfun(cumlift = up20$cum.lifts, 
            noncumlift = up20$noncum.lifts,
            main = "Proportion of 1s, 1 = 'up', bin size = 20", 
            base_prop = base_prop,
            bin.size = up20$bin.size, last.obs.grp = up20$last.obs.grp)
    plotfun(cumlift = down20$cum.lifts,
            noncumlift = down20$noncum.lifts, base_prop = 1 - base_prop,
            main = "Proportion of 1s, 1 = 'down', bin size = 20", 
            bin.size = down20$bin.size, last.obs.grp = down20$last.obs.grp)
    plotfun(cumlift = up10$cum.lifts,
            noncumlift = up10$noncum.lifts, base_prop = base_prop,
            main = "Proportion of 1s, 1 = 'up', bin size = 10", 
            bin.size = up10$bin.size, last.obs.grp = up10$last.obs.grp)
    plotfun(cumlift = down10$cum.lifts, base_prop = 1 - base_prop,
            noncumlift = down10$noncum.lifts,
            main = "Proportion of 1s, 1 = 'down', bin size = 10",
            bin.size = down10$bin.size, last.obs.grp = down10$last.obs.grp)
    plotfun(cumlift = up5$cum.lifts, base_prop = base_prop,
            noncumlift = up5$noncum.lifts,
            main = "Proportion of 1s, 1 = 'up', bin size = 5",
            bin.size = up5$bin.size, last.obs.grp = up5$last.obs.grp)
    plotfun(cumlift = down5$cum.lifts, base_prop = 1 - base_prop,
            noncumlift = down5$noncum.lifts,
            main = "Proportion of 1s, 1 = 'down', bin size = 5", 
            bin.size = down5$bin.size, last.obs.grp = down5$last.obs.grp)
  }
  print("NB the function will return cum and non.cum lift and bin sizes if you assign the output to an obj")
  invisible(list("up5" = up5, "up10" = up10, "up20" = up20,
                 "down5" = down5, "down10" = down10, 
                 "down20" = down20))
}



######################################################################
#                     AccurFun FUNCTION
######################################################################
# *** NOTE THIS IS BASED HEAVILY OFF LIFTFUN ***


AccurFun = function(actual, pred, thresh = 0.5, plots = T) {
  # Description: compute cumulative and noncumulative accuracy and plot NOTE COMPUTES BOTH 'UP' AND 'DOWN' LIFT, I.E. WHERE '1' IN ACTUAL (AND THING WE'RE TRYING TO PREDICT IS 'UP' AND 'DOWN', RESPECTIVELY)
  # Args: actual, pred: same args to 'gains' function. NOTE 'pred' PROB SHOULD BE PROB OF 'UP', thresh: threshold above which an element of pred is called "1" vs "0", plots: should we plot
  # REturns: plot of accuracy and cumulative accuracy when 10, 20 and 50 groups and corresponding data plus bin sizes.
  
  warning("Make sure pred is prob of 'up' not prob of 'down")
  # Covnert actual to nuemric (needed for gains function)
  if (!all(actual %in% c("up", "down", NA))) {
    stop("actual should take values 'up', 'down'")
  }
  actual = ifelse(actual == "up", 1, 0)
  
  # Function to compute accuracies for data for a given bin size (just like in a lift plot). actual and pred args are same as in gains fun. bin.size is no. obs per 'group', cutoff is thresh above which we assign element of pred to 1 vs 0.
  ComputeAccur = function(actual, pred, bin.size, cutoff = thresh) {
    pred2 = ifelse(pred < cutoff, 0, 1) #convert pred to binary
    D = data.frame(actual, pred, pred2)
    D$last.obs = c(rep(0, nrow(D) - 1), 1) #record which is last obs
    D = D[order(D$pred, decreasing = T), ] #order by preds
    n_groups = round(nrow(D)/bin.size)
    D$group = cut(1:length(actual), n_groups) #break into 'n_groups' groups
    
    # Calculate a) the number we got right and b) the number in the bin for each group, then use these to compute cum and noncum accuracy. Also extract c) the group in which the last obs sits so we can add it to the plot 
    rights = by(D, D$group, function(x) {
      sum(x$actual == x$pred2, na.rm = T)
    })
    
    lengths = by(D, D$group, function(x) {
      sum(!is.na(x$actual))
    })
    last.obs.grp = which(by(D, D$group, function(x) {
      any(x$last.obs == 1, na.rm = T)
    }))
    accurs = as.numeric(rights/lengths)
    sds = sqrt(accurs * (1 - accurs)/bin.size)
    
    return(list("accurs" = accurs, 
                "sds" = sds,
                "bin.size" = bin.size,
                "no.bins" = length(accurs),
                "last.obs.grp" = last.obs.grp))
  }
  
  #Compute noncum and cum accuracy for bin size = 5, 10 & 20
  up5 = ComputeAccur(actual, pred, bin.size = 5, cut = thresh)
  up10 = ComputeAccur(actual, pred, bin.size = 10, cut = thresh)
  up20 = ComputeAccur(actual, pred, bin.size = 20, cut = thresh)
  down5 = ComputeAccur(1-actual, 1-pred, bin.size = 5, cut = thresh)
  down10 = ComputeAccur(1-actual, 1-pred, bin.size = 10, cut = thresh)
  down20 = ComputeAccur(1-actual, 1-pred, bin.size = 20, cut = thresh)
  
  # Plot
  if (plots == T) {
    
    # Compute "base accuracy" (i.e. across all obs) and "model accuracy" (ie overall accuracy of model) for plot
    model.acc = mean(actual == ifelse(pred < thresh, 0, 1), 
                     na.rm = T)
    base.acc = mean(actual, na.rm = T)
    
    # Simple prelim function as we have a lot of repetition 
    plotfun = function(accurs, sds, base.acc, 
                       bin.size, last.obs.grp, main, col = "blue") {
      library(Hmisc) # for error bar function
      library(scales)
      plot(1:length(accurs), accurs, type = "p", 
           ylab = "Accuracy",
           main = main, xlab = paste0("Bin (", length(accurs), 
                                      " bins total)"),
           ylim = c(0, 1))
      cols = rep(col, length(accurs)); cols[last.obs.grp] = "red"
      errbar(x = 1:length(accurs), y = accurs,
             yplus = accurs + 1.645 * sds, #1.645 is z for 90%
             yminus = accurs - 1.645 * sds, 
             add = T, col = cols, errbar.col = alpha(cols, 0.2), lwd = 0.5)
      lines(1:length(accurs), rep(model.acc, length(accurs)),
            lty = 5, col = "grey")
      lines(1:length(accurs), rep(base.acc, length(accurs)),
            lty = 5, col = "lightblue")
      x = 1:length(accurs); l = loess(accurs ~ x)
      lines(1:length(l$fitted), l$fitted, col = alpha(col, 0.5))
      legend("topright", c("accuracy", "accuracy (smoothed)",
                           "overall model accuracy",
                           "base proportion", "last obs group"), 
             col = c(col, col, "grey", "lightblue", "red"), 
             pch = c(1, 45, 45, 45, 1), 
             bty = "n", cex = 0.9)
    }
    
    # Call plotfun for various comobs
    windows(h = 10, w = 15)
    mat = matrix(c(1, 1, 2, 3, 4, 5, 6, 7), ncol = 2, byrow = T)
    par(mai = c(0.6, 0.82, 0.4, 0.42)) #see par()$mai for defaults
    layout(mat, heights = c(0.8, 1.8, 1.8, 1.8))
    plot.new()
    mtext("ACCURACY", side = 3, cex = 2, font = 2)
    mtext("Error bars give a 90% CI", side = 1, cex = 1, font = 3, 
          line = 1)
    plotfun(accurs = up20$accurs,
            sds = up20$sds, base.acc = base.acc,
            main = "Accuracy, 1 = 'up', bin size = 20", 
            bin.size = up20$bin.size, last.obs.grp = up20$last.obs.grp)
    plotfun(accurs = down20$accurs,
            sds = down20$sds, base.acc = 1 - base.acc,
            main = "Accuracy, 1 = 'down', bin size = 20", 
            bin.size = down20$bin.size, last.obs.grp = down20$last.obs.grp)
    plotfun(accurs = up10$accurs, base.acc = base.acc,
            sds = up10$sds,
            main = "Accuracy, 1 = 'up', bin size = 10", 
            bin.size = up10$bin.size, last.obs.grp = up10$last.obs.grp)
    plotfun(accurs = down10$accurs, base.acc = 1 - base.acc,
            sds = down10$sds,
            main = "Accuracy, 1 = 'down', bin size = 10",
            bin.size = down10$bin.size, last.obs.grp = down10$last.obs.grp)
    plotfun(accurs = up5$accurs,  base.acc = base.acc,
            sds = up5$sds,
            main = "Accuracy, 1 = 'up', bin size = 5",
            bin.size = up5$bin.size, last.obs.grp = up5$last.obs.grp)
    plotfun(accurs = down5$accurs, base.acc = 1 - base.acc,
            sds = down5$sds,
            main = "Accuracy, 1 = 'down', bin size = 5", 
            bin.size = down5$bin.size, last.obs.grp = down5$last.obs.grp)
  }
  print("NB the function will return cum and non.cum accuracy and bin sizes if you assign the output to an obj")
  invisible(list("up5" = up5, "up10" = up10, "up20" = up20,
                 "down5" = down5, "down10" = down10, 
                 "down20" = down20))
}


######################################################################
#                     PropScoreSummaryFun FUNCTION
######################################################################

PropScoreSummaryFun = function(pred) {
  # Description: just produces some basic summary plots of propensity score, namely the unsorted values over time (to see if there's any time dependent patterns), the sorted values (helpful for knowing if the bin a particular obs is put in is meaningful eg if all scores are the same its bin will be arbitrary) and the distribution of scores.
  # Args: pred: vector of propensity scores
  # Returns: see description
  
  warning("This code assumes '1' is prob of going up not down (although it only matters for plot labels)")
  
  # Make dataframe of prop scores plus binary indicator for whetehr it's last obs or not (which becomes necessary as we'll sort the data later)
  D = data.frame(pred, c(rep(0, length(pred) - 1), 1)) 
  colnames(D) = c("pred", "lastObs")
  
  # Set plot parameters and add title
  windows(h = 10, w = 10)
  mat = matrix(c(1, 2, 3, 4), ncol = 1, byrow = T)
  par(mai = c(0.6, 0.82, 0.4, 0.42)) #see par()$mai for defaults
  layout(mat, heights = c(0.8, 1.8, 1.8, 1.8))
  plot.new()
  mtext("PROPENSITY SCORES SUMMARY", side = 3, cex = 2, font = 2, line = 1)
  
  # Plot unsorted prop scores
  cols = ifelse(D$lastObs == 1, "red", "black")
  pchs = ifelse(D$lastObs == 1, 16, 1)
  cexs = ifelse(D$lastObs == 1, 2, 0.7)
  plot(1:nrow(D), D$pred, cex = cexs, col = alpha(cols, 0.5),
       main = "Propensity scores (unsorted, 1 = 'up')", 
       xlab = "index", ylab = "score", pch = pchs)
  legend("topright", c("last obs"), col = alpha("red", 0.5), pch = 16,
         bty = "n")
  
  # Plot sorted prop scores
  o = order(D$pred, decreasing = T)
  cols = ifelse(D$lastObs[o] == 1, "red", "black")
  pchs = ifelse(D$lastObs[o] == 1, 16, 1)
  cexs = ifelse(D$lastObs[o] == 1, 2, 0.7)
  plot(1:nrow(D), D$pred[o], cex = cexs, pch = pchs, col = alpha(cols, 0.5),
       main = "Propensity scores (sorted, 1 = 'up')", 
       xlab = "index", ylab = "score")
  legend("topright", c("last obs"), col = alpha("red", 0.5), pch = 16,
         bty = "n")
  
  # Plot hist
  hist(D$pred, freq = F, breaks = 50, main  = "Distribution of propensity scores (1 = 'up')")
  points(x = D$pred[nrow(D)], y = 0, cex = 2, pch = 16, 
         col = alpha("red", 0.5))
  legend("topright", c("last obs"), col = alpha("red", 0.5), pch = 16,
         bty = "n")
}



###########################################################################
#               ProcessStockTwitsDates function
###########################################################################

ProcessStockTwitsDates = function(dates) {
  # Description: takes a vector of times/dates from stocktwits and processes them in a form usable by R. 
  
  # strip leading/trailing \ns & spaces
  # dates = dates2
  dates = gsub("^\n *|\n *$", "", dates) 
  
  # Regex that should match all dates (after stripping leading/training \ns & spaces). Checks (reading left to right) 'starts with capital then two lower case (month) then full stop then whitespcae then one or two numbers (day) then whitespace then either four numbers (year) or nothing (as no year recorded within past 12m of post) then whitespace then at then 1 or 2 numbers, colon, 2 numbers (ie time eg 4:12) then whitespace then am or pm"
  reg = "^[A-Z]{1}[a-z]{2}\\. *[0-9]{1,2} *([0-9]{4}|) *at *[0-9]{1,2}:[0-9]{2} *(AM|PM)$"
  if (length(grep(reg, dates)) != length(dates)) {
    stop ("Not all message dates meet the required regex (the one we're assuming they''re 're saved as in StockTwits")
  }
  
  # Extract day, month, year, hour, min of each post. (NB to extract the first bit wrapped in parentheses (if multiple) then use the "\\1" notation in call to sub() below)
  r_m = "^([A-Z]{1}[a-z]{2})\\. *[0-9]{1,2} *([0-9]{4}|) *at *[0-9]{1,2}:[0-9]{2} *(AM|PM)$"
  r_d = "^[A-Z]{1}[a-z]{2}\\. *([0-9]{1,2}) *([0-9]{4}|) *at *[0-9]{1,2}:[0-9]{2} *(AM|PM)$"
  r_y = "^[A-Z]{1}[a-z]{2}\\. *[0-9]{1,2} *(([0-9]{4}|)) *at *[0-9]{1,2}:[0-9]{2} *(AM|PM)$"
  r_h = "^[A-Z]{1}[a-z]{2}\\. *[0-9]{1,2} *([0-9]{4}|) *at *([0-9]{1,2}):[0-9]{2} *(AM|PM)$"
  r_min = "^[A-Z]{1}[a-z]{2}\\. *[0-9]{1,2} *([0-9]{4}|) *at *[0-9]{1,2}:([0-9]{2}) *(AM|PM)$"
  r_amPm = "^[A-Z]{1}[a-z]{2}\\. *[0-9]{1,2} *([0-9]{4}|) *at *[0-9]{1,2}:[0-9]{2} *((AM|PM))$"
  
  
  # Extract month/day etc
  months = sapply(dates, function(d) {sub(r_m, "\\1", d)})
  days = sapply(dates, function(d) {sub(r_d, "\\1", d)})
  years = sapply(dates, function(d) {sub(r_y, "\\1", d)})
  hours = sapply(dates, function(d) {sub(r_h, "\\2", d)}) #NB have to switch to \\2 as we're now extracting the SECOND bit of the regex thats in parentheses (as year is always in brackets)
  mins = sapply(dates, function(d) {sub(r_min, "\\2", d)})
  amPms = sapply(dates, function(d) {sub(r_amPm, "\\2", d)})
  
  # Make a couple of changes to year and hour: if year was blank make it current year (which is when it's blank); and make hour 24hr and add "0" if its 1 digit. NB this approach to years isnt quite right but we fix below: stocktwits posts no year for posts in last year and year otherwise, so if yaer is blank could be current year or prev year. 
  years[years == ""] = format(Sys.Date(), "%Y")
  hours = ifelse(amPms == "PM" & hours != "12", # convert to 24hr format
                 as.character(as.numeric(hours) + 12), hours)
  hours[amPms == "AM" & hours == "12"] = "00" #12am needs special treatment
  hours = ifelse(nchar(hours) == 1, paste0("0", hours), hours) #eg "7" -> "07"
  
  # Convert dates to R format. Saves as NZST but this is right
  dates = paste(days, months, years, hours, mins, sep = ":")
  dates = strptime(dates, format = "%d:%b:%Y:%H:%M")
  
  # Fix year problem mentioned above, specifically if post was made after today's date last year it will be recorded as the current year (due to year = ifelse..). Correct by making any such date one year earlier
  i = which(dates > dates[1]) #which dates are in the future
  if (length(i) > 0) {
    dates$year[i] = dates$year[i] - 1
  }
  
  if (!all(dates[-1] <= dates[-length(dates)], na.rm = T)) {
    warning ("Something might have gone wrong with converting StockTwits dates/times to R dates/times as the date order (post conversion) isnt stricly chronological. (Note that this can however happen without implying an error e.g. a post from 10.00am can appear after a post from 9.59am)")
  }
  return(dates)
}


##########################################################################
#               GetLastStockTwitsDate FUNCTION
##########################################################################

GetLastStockTwitsDate = function(remDriver) {
  # Description/returns: extracts and returns the last date a message was posted from StockTwits (at least, down as far as we've scrolled).
  #Arg: remDriver: the remote driver created by e.g. rD = rsDriver(browser = "firefox"); remDriver = rD[["client"]]
  
  # Extract last message (which includes date - couldnt figure how to just get date by itself and the usual css selector '.message-date' didnt work if we want to add ":last-child" to get the last element).
  last_child = remDriver$findElement(using = "css selector", 
                                     value = ".messageli:last-child")
  last_child = unlist(last_child$getElementText())
  
  # Regex that should match all dates in last_child. See notes in "ProcessStockTwitsDates()" for how it works 
  reg = "[A-Z]{1}[a-z]{2}\\. *[0-9]{1,2} *([0-9]{4}|) *at *[0-9]{1,2}:[0-9]{2} *(AM|PM)"
  if (length(grep(reg, last_child)) != 1) {
    stop("last_child should contain a date which matches the regex 'reg'")
  }
  
  # Extract the part of the string (the date) that matches the regex. NB this is done by wrapping within parentheses then calling sub with '\\1'.
  reg2 = "^.*([A-Z]{1}[a-z]{2}\\. *[0-9]{1,2} *([0-9]{4}|) *at *[0-9]{1,2}:[0-9]{2} *(AM|PM)).*$"
  last_date = sub(reg2, "\\1", last_child)
  last_date = ProcessStockTwitsDates(last_date) #convert to format R can use
  return(last_date)
}


######################################################################
#                 StockTwitsScraping FUNCTION
######################################################################

StockTwitsScraping = function(sym, oldest_date, out.dir, num2check = 5,
                              browser, max_scroll) {
  # Description: For a given symbol ('sym'), loads its relevant page in stocktwits, scroll down until there's either no more data or we reach the oldest date we want (as specified by "oldest_date") and save in 'out.dir'
  # Args: see description. Only other one is "num2check" which is pretty irrelevant. Basically we want to periodically check if we can scroll further and if we've hit "oldest_date". We do this every num2check scrolls. broswer is same as in call to rsDriver(), max_scroll: most scrolls before we break (can set to arbitrarily large no.)
  # Returns: List with two elements: sym and D, the latter a dateframe holding all the posts, their date and whether they had a bullish/bearish tag. Gets saved into out.dir rather than returned.
  
  library(RSelenium)
  library(rvest)
  
  # Load remote browswer and webpage of interest
  # rD = rsDriver(browser = "firefox")
  gc() #if prev calls to the function crash, sometimes get a 'port still in use' error which stops us reopening RSelenium. gc() closes  
  rD = rsDriver(browser = browser)
  remDr = rD[["client"]]
  url = paste0("https://stocktwits.com/symbol/", sym)
  remDr$navigate(url) 
  remDr$setTimeout(type = "page load", milliseconds = 36000000) #10 hours
  remDr$setTimeout(type = "script", milliseconds = 36000000) #10 hours
  remDr$setTimeout(type = "implicit", milliseconds = 36000000) #10 hours
  
  # Keep scrolling down page, loading new content each time. 
  last_height = 0 #use below to check if we can't scroll any further
  suff = "" #use below for file naming - update if below loop breaks. ACTUALLY I REMOVED THE TRYCATCH WHICH MAKES THIS RELEVANT, BUT LEAVE IT HERE AS DOES NO HARM AND MIGHT FIX THE TRYCATCH
  k = 1 # counter
  ptm = proc.time()
  cat("Scrolling down webpage. Up to scroll ")
  tryCatch({ #Couldnt get this working so commented out (plus at bottom of script). Just wanted to save data if there was an error with the scroll eg if it broke after 4 hours of scrolling wanted that data kept.
    repeat {   
      if (k == max_scroll) {
        cat("\nMax scroll (", max_scroll, ") reached\n")
        break
      }
      cat(k, " ")
      remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
      Sys.sleep(3) #delay by 3sec to give chance to lOAD LOOK HOW TO IMPROVE. 
      
      # Check if we can stop scrolling every num2checkth iteration
      if (k %% num2check == 0) {
        
        # Check if we can't scroll any further (is body height after scrolling same as before?) and break if so
        new_height = remDr$executeScript("return document.body.scrollHeight")
        if(unlist(last_height) == unlist(new_height)) {
          cat("\nNo more data to load\n")
          break
        } else {
          last_height = new_height
        }
        
        # Check if last date in browser is older than oldest date we want and break if so
        last_date = GetLastStockTwitsDate(remDriver = remDr) #NB have to have browser open to work
        if(!all(class(last_date) == c("POSIXlt", "POSIXt"))) {
          stop ("dates should be class: POSIXlt, POSIXt (after processing)")
        }
        if (strftime(last_date, '%Y-%m-%d') <= oldest_date) {
          cat("\nOldest date reached\n")
          break
        }
      }
      k = k + 1
    }
    cat("\nTime taken:", (proc.time() - ptm)["elapsed"], "secs")
  }, error = function(e) { #Next few lines are tail end of tryCatch
    suff <<- "LoopBrokeEarly_"
    warning("\nLoop broke early\n")
  })
  
  tryCatch({ #NB need a trycatch here as if code fails we still need to make sure we close the browswer - else next time we try to call the current function for a new symbol it wont work
    # Extract date of each post and the message content (incl. bullish/bearish tag). NB can take aaaages
    cat("\nExtracting messages and message dates. This may take a while...\n")
    ptm = proc.time()
    messages = read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes(".message-body") %>% html_text()
    dates = read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes(".message-date") %>% html_text()
    dates = ProcessStockTwitsDates(dates) #make dates suitable for R
    cat("\nTime taken:", (proc.time() - ptm)["elapsed"], "secs")
    # messages
    # dates
    
    # Create dataframe to store results 
    D = data.frame(dates, messages, NA)
    colnames(D) = c("Date", "Message", "BullBear")
    D$BullBear[grep("Bullish\n", messages)] = "Bullish"
    D$BullBear[grep("Bearish\n", messages)] = "Bearish"
    
    # Save 
    fname = paste0(sym, "_", suff, gsub("-", "", Sys.Date()), ".RData")
    O = list("sym" = sym, "D" = D)
    save(O, file = file.path(out.dir, fname))
    
  }, error = function(e) {
    cat("\nError extracting dates/messages\n")
  })
  
  # Close Rselenium
  remDr$close()
  rD$server$stop()
  # rD$client$close()
  gc() #see notes by call to gc() at top of function
}


#####################################################################
#             DownloadTFXData FUNCTION
#####################################################################

DownloadTFXData = function(pairs, months, years, out.dir, 
                           skip_if_exists) {
  #Description: downloads the exchange rate data for the specified currency pairs, months and years from True FX into "out.dir". 
  #Args: pairs: a valid currency pair (see 'valid_pairs' for what's valid); months/years: what it sounds like. NB each month must be in e.g. "01", "11" format, out.dir: dir to save in, skip_if_exists: if F, we redownload/extract csv irrespective of whether it already exists. If T, we skip if hte file exists. 
  # REturns: for each combo of pair/month/year, the exchange rate data saved as a zip and csv from True FX.
  
  warning("\n *** IMPORTANT ****\nDON'T THINK THIS CODE WILL RUN UNLESS TRUE FX WEBPAGE IS OPEN AND YOU'RE LOGGED IN\n\n")
  warning("\n *** IMPORTANT ****\nAS PER TFX'S WEBPAGE, ALL TIMES ARE IN GMT. THE TWO COLS ARE BID AND ASK PRICES\n\n")
  library(RCurl) #check if url exists
  
  valid_pairs = c("AUDJPY", "AUDNZD", "AUDUSD", "CADJPY", 
                  "CHFJPY", "EURCHF", "EURGBP", "EURJPY", 
                  "EURUSD", "GBPJPY", "GBPUSD", "NZDUSD", 
                  "USDCAD", "USDCHF", "USDJPY")
  if (!all(pairs %in% valid_pairs)) {
    stop(paste0(pairs[which(!pairs %in% valid_pairs)],
                " is not a valid currency pair"))
  }
  valid_months = c("01", "02", "03", "04", "05", "06", 
                   "07", "08", "09", "10", "11", "12")
  if (!all(months %in% valid_months)) {
    stop ("You havent entred a valid month")
  }
  if (any(as.numeric(years) < 2013)) {
    stop("I've included an error to stop downloading data from before 2013. Prob can back further - just haven't checked whether its available at true fx")
  }
  for (pair in pairs) { #for each pair/month/year, download and unzip the data
    out.dir2 = file.path(out.dir, pair)
    dir.create(out.dir2 , showWarnings = F)
    for (year in years) {
      for (month in months) {
        tryCatch({
          ptm = proc.time()
          
          # Exctract long version of month (needed for url below)
          month2 = switch(month, "01" = "JANUARY", "02" = "FEBRUARY",
                          "03" = "MARCH", "04" = "APRIL", "05" = "MAY",
                          "06" = "JUNE", "07" = "JULY",
                          "08" = "AUGUST", "09" = "SEPTEMBER", 
                          "10" = "OCTOBER", "11" = "NOVEMBER", 
                          "12" = "DECEMBER")
          
          # Download data (if it exists and we havent already )
          datname = paste0(pair, "-", year, "-", month) 
          zip_path = file.path(out.dir2, paste0(datname, ".zip"))
          csv_path = file.path(out.dir2, paste0(datname, ".csv"))
          rdata_path = file.path(out.dir2, paste0(datname, ".RData"))
          cat("Up to ", datname, "\n")
          if (skip_if_exists == T & 
              (paste0(datname, ".RData") %in% list.files(out.dir2))) {
            cat(datname, "wasn't downloaded again as it already exists\n")
            next
          }
          # Two possible versions of the url - we try them both
          url = paste0("http://www.truefx.com/dev/data/",
                       year, "/", month2, "-", year, "/",
                       datname, ".zip")
          url2 = paste0("http://www.truefx.com/dev/data/",
                        year, "/", year, "-", month, "/",
                        datname, ".zip")
          if (url.exists(url)) {
            download.file(url = url, destfile = zip_path) 
            unzip(zip_path, exdir = out.dir2) #unzip
            X = read.csv(csv_path, header = F)
            save(X, file = rdata_path)
            file.remove(zip_path); file.remove(csv_path)
            rm(X); gc()
          } else if (url.exists(url2)) {
            download.file(url = url2, destfile = zip_path) 
            unzip(zip_path, exdir = out.dir2) #unzip
            X = read.csv(csv_path, header = F)
            save(X, file = rdata_path)
            file.remove(zip_path); file.remove(csv_path)
            rm(X); gc()
          } else {
            stop (paste0("The file ", datname,
                         ".zip wasnt found to be downloaded"))
          }
          cat(datname, "took", 
              ((proc.time()-ptm)["elapsed"])/60, "mins\n")
        }, error = function(e) {
          print(paste0("Something went wrong downloading/writing the data for ", paste0(pair, "-", year, "-", month)))
        })
      }
    }
  }
  warning("**** IMPORTANT *****\n\nTHERE ARE OFTEN ERRORS DUE TO (I ASSUME) INTERNET DISCONNECTION WHICH MEAN YOU MAY HAVE TO RUN THE ABOVE MANY TIMES TO SUCCESFFULY DOWNLOAD ALL THE FILES\n\n")
}



#######################################################################################################
#             MakeTimeSlices FUNCTION
#######################################################################################################

MakeTimeSlices = function(nfolds, prop_train, discrete = T) {
  #Description: just calls 'createTimeSlices" function (from caret) but with two modifications: 1) carets function requires that you specify initial window size and horizon size. I want to specify proportion of train data and number of folds instead and choose window/horizon size instead (eg if dataset is 100 rows, prop_train = 0.5 and nfolds = 2, train size will be 50 and horizon 25 (first fold will be 1:50/51:75 train/test; second will be 25:75/76:100 ie we use all data. See caret's page on createTimeSlice for details of exctly what this function does)). 2) IF discrete = T if we were to do the above excercise (set intialWindow = 50 and horizon = 25) carets function would make a gizzillion diff train/test splits - 1:50/51:75; 2:51/52:76....25:75/76/100. We only want the unique folds ie if we specify nfolds = 2 we want 2 not a gizzillion folds (which will take way longer)
  # args: nfolds: number of test/train splits, prop_train: proportion of data to use for training; discrete: if F,  do what caret does in terms of a gizzillion splits (1:50/51:75; 2:51/52:76....25:75/76/100) - which will take forever, so use discrete = T!.
  
  # Calculate initial window/horizon and create time slices
  initialWindow = floor(nrow(X) * prop_train)
  horizon = floor(nrow(X) * prop_train/nfolds)
  slices = createTimeSlices(1:nrow(X), initialWindow = initialWindow, horizon = horizon, fixedWindow = T)
  if (discrete == T) { #remove all those (mostly) duplicated folds as described in "description"
    inds = 1:length(slices$train) #index of each slice
    inds_to_keep = (1:100000)[1:100000 %% length(slices$test[[1]]) == 1] #if each test set is eg 10 obs, keep only 1st, 11th, 21st etc (ie so each test set is unique obs). NB 1000000 is just an aribtrarily big number 
    inds = intersect(inds, inds_to_keep) #subset inds_to_keep to ony inlcude indices in slices
    slices$train = slices$train[inds]
    slices$test = slices$test[inds]
  }
  message("Output from MakeTimeSlices: during resampling for caret, you will have ", nfolds, " pairs of train/test time slices, each with ", length(slices$train[[1]]), "/", length(slices$test[[1]]), " train/test obs")
  return(slices)
}