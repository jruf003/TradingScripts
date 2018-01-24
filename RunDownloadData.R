############################################################################
# Description: runs the DownloadData function from UtilityFuns_vx_yyyymmdd.R. 

# Author and date: Jon Ruffell, 30/12/17
############################################################################

rm(list = ls()); gc()
library(quantmod)

# Specify inputs
base.dir = "~/Desktop/Jon/Trading"
code.dir = file.path(base.dir, "Scripts")
out.dir = file.path(base.dir, "Outputs")
out.dir = file.path(out.dir, "RunDownloadData_Out")
dir.create(out.dir, showWarnings = F)
utility_funs = "UtilityFuns.R"
fname = paste0("L_", gsub("-", "", Sys.Date()), ".RData") #fname for saving

# Create 'syms.df' obj required as input IF running DonwloadData for first time. NB needs to be a 2 col dataframe with colnames 'Symbol' and "SRC". If you want to see how to get ALL oanda symbols, look at first code block within DownloadData function 
stocks = stockSymbols()$Symbol
# stocks = sample(stocks, 500) #>>> JUST FOR A SAMPLE
indices = c("^DJI", "^FTSE", "SPY", "^HSI", "^SSEC", 
            "^IXIC", "^GDAXI", "^FCHI")
#commods = c("XAU/USD", "XAG/USD", "BNO") # gold, silver and brent oil
commods = c("XAU/USD", "XAG/USD") # gold, silver. BNO no longer seems to work??
ERs = paste("USD", c("NZD", "AUD", "GBP", "EUR", "JPY", "HKD", "CNY"), 
            sep = "/")
yahoo_syms = c(stocks, indices); oanda_syms = c(ERs, commods)
syms.df = data.frame("Symbol" = c(yahoo_syms, oanda_syms), 
                     "SRC" = c(rep("yahoo", length(yahoo_syms)), 
                               rep("oanda", length(oanda_syms))))

# Source function and run
source(file.path(code.dir, utility_funs))
ptm = proc.time()
L = DownloadData(syms.df = syms.df, L = NULL, from = "1900-01-01", to = Sys.Date()) #NB from and to are the defaults
proc.time() - ptm

# Save output
save(L, file = file.path(out.dir, fname))

