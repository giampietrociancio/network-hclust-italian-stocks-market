library(tidyverse)
library(tidyquant)
library(reshape2)
library(igraph)
library(hrbrthemes)

# tickers of stocks in FTSE MIB as of June 2023
tickers <- c("A2A.MI", "AMP.MI", "AZM.MI", "BGN.MI", "BMED.MI", "BMPS.MI", "BAMI.MI", "BPE.MI", "CPR.MI", "CNHI.MI",
             "DIA.MI", "ENEL.MI", "ENI.MI", "ERG.MI", "RACE.MI", "FBK.MI", "G.MI", "HER.MI", "IP.MI", "ISP.MI", "INW.MI",
             "IG.MI", "IVG.MI", "LDO.MI", "MB.MI", "MONC.MI", "NEXI.MI", "PIRC.MI", "PST.MI", "PRY.MI", "REC.MI", "SPM.MI",
             "SRG.MI", "STLAM.MI", "STMMI.MI", "TIT.MI", "TEN.MI", "TRN.MI", "UCG.MI", "UNI.MI")


# import all the stock data from 1970 till the end of 2022 and keep only (adjusted) closing prices
stock.prices <- tq_get(tickers, get = "stock.prices", from = "1970-01-01", to = "2022-12-31") %>% select(date, symbol, adjusted)

# remove ".MI" from the stocks ticker
stock.prices$symbol <- sub("\\..*", "", stock.prices$symbol)


#import classification.csv
cls <- read.csv("classification.csv", sep = ";") 