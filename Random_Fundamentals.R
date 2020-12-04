library(tibble)
library(stringr)
library(dplyr)
library(data.table)
library(xts)
library(rvest)
library(timetk)
library(tidyverse)
library(tidyr)
library(convertr)
library(xlsx)
library(plotly)
library(reshape2)
library(knitr)
library(rDEA)

SELECTED_FISCAL_YEAR <- 2018

### Load Data

Fundamental_Data <- read.csv('Desktop/Falcon Management/Firm_Eff_And_MA/ManagerialAbility_20201002.csv')
MA_Info <- read.csv('Desktop/Falcon Management/Firm_Eff_And_MA/SMIU_20201002.csv')
Fundamental_Data <- data.frame(Fundamental_Data)
  
### Format MA_Info

MA_Info <- MA_Info %>% filter( TagName %in% c('docinfo:EntityCommonName', 'docinfo:EntitySectorIndustryClassification',
                                              'docinfo:PrimarySecurityTradingSymbol'))
MA_Info <- data.table(MA_Info)

### Rows to Columns 

MA_Info <- dcast.data.table(MA_Info, ModelID ~ TagName, value.var = 'TagValue', toString)

tibble(MA_Info)
MA_Info$ModelID <- as.factor(MA_Info$ModelID)

### Extract Fundamentals we want

Fundamental_Data <- Fundamental_Data %>% filter(TagName %in% c('perform:OperatingRevenue', 
                                             'perform:FCF',
                                             'perform:EBITDA',
                                             'value:MarketCap',
                                             'utilize:EBITAdjustedROICAverage',
                                             'utilize:ComparableReturnEquityEnding',
                                             'utilize:ComparableReturnAssetsEnding',
                                             'utilize:NOPATReturnOnInvestedCapitalEnding'
                                             ))

Fundamental_Data <- subset(Fundamental_Data, FiscalYear == SELECTED_FISCAL_YEAR )

### Shape the data

Fundamental_Data <- dcast(Fundamental_Data, ModelID + FiscalYear + ActualEstimate ~ TagName, value.var = 'TagReportingValue', fun.aggregate = sum)


Fundamental_Data <- tibble(Fundamental_Data)
Fundamental_Data$ModelID <- as.factor(Fundamental_Data$ModelID)

# Merge Data

Fundamental_Data$Ticker <- MA_Info$`docinfo:PrimarySecurityTradingSymbol`[match(Fundamental_Data$ModelID, MA_Info$ModelID)]
Fundamental_Data$Company <- MA_Info$`docinfo:EntityCommonName`[match(Fundamental_Data$ModelID, MA_Info$ModelID)]
Fundamental_Data$SICS <- MA_Info$`docinfo:EntitySectorIndustryClassification`[match(Fundamental_Data$ModelID, MA_Info$ModelID)]

Fundamental_Data <- Fundamental_Data[,c("Company","Ticker","SICS","ModelID", 'perform:OperatingRevenue', 
                                        'perform:FCF',
                                        'perform:EBITDA',
                                        'value:MarketCap',
                                        'utilize:EBITAdjustedROICAverage',
                                        'utilize:ComparableReturnEquityEnding',
                                        'utilize:ComparableReturnAssetsEnding',
                                        'utilize:NOPATReturnOnInvestedCapitalEnding')]

i <- c("Company","Ticker", "ModelID")

Fundamental_Data[i] <- sapply(Fundamental_Data[i],as.character)

write.csv(Fundamental_Data, 'Desktop/FEMA_Fundamental.csv')
