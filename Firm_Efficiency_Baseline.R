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

Fiscal_Year <- 2021

#Load Data

DEA_Data <- read.csv('Documents/Firm_Efficiency/ManagerialAbility_20201002.csv')
MA_Info <- read.csv('Documents/Firm_Efficiency/SMIU_20201002.csv')
MA_Data <- data.frame(DEA_Data)

#Format MA_Info

MA_Info <- MA_Info %>% filter( TagName %in% c('docinfo:EntityCommonName', 'docinfo:EntitySectorIndustryClassification',
                                              'docinfo:PrimarySecurityTradingSymbol'))
MA_Info <- data.table(MA_Info)

#Rows to Columns

MA_Info <- dcast.data.table(MA_Info, ModelID ~ TagName, value.var = 'TagValue', toString)
tibble(MA_Info)
MA_Info$ModelID <- as.factor(MA_Info$ModelID)

#Format & Select Columns

MA_Data <- MA_Data %>% filter(TagName %in% c('perform:OperatingRevenue', 
                                             'perform:CostRevenue',
                                             'perform:SellingGeneralAdministrative',
                                             'position:SellingGeneralAdministrative5yCapitalization', 
                                             'position:PropertyPlantEquipmentGross',
                                             'position:LeaseContractCommitments', 
                                             'perform:ResearchDevelopmentExpenses', 
                                             'position:Goodwill', 
                                             'position:OtherIntangibleAssets', 
                                             'perform:FCF', 
                                             'value:MarketCap'))

MA_Data$TagReportingValue <- (MA_Data$TagReportingValue/1000000)

#Shape Data

MA_Data <- subset(MA_Data, FiscalYear==Fiscal_Year)

MA_DATA_Shaped <- dcast(MA_Data, ModelID + FiscalYear + FiscalPeriod + ActualEstimate ~ TagName, value.var = 'TagReportingValue', fun.aggregate = sum)
MA_DATA_Shaped <- tibble(MA_DATA_Shaped)

i <- c('perform:OperatingRevenue', 'perform:CostRevenue','perform:SellingGeneralAdministrative',
       'position:SellingGeneralAdministrative5yCapitalization', 'position:PropertyPlantEquipmentGross',
       'position:LeaseContractCommitments', 'perform:ResearchDevelopmentExpenses', 
       'position:Goodwill', 'position:OtherIntangibleAssets', 'perform:FCF', 'ModelID')

MA_DATA_Shaped[i] <- sapply(MA_DATA_Shaped[i],as.integer)


# Merge MA_Info and MA_Data_Shaped

MA_DATA_Shaped$Ticker <- MA_Info$`docinfo:PrimarySecurityTradingSymbol`[match(MA_DATA_Shaped$ModelID, MA_Info$ModelID)]
MA_DATA_Shaped$Company <- MA_Info$`docinfo:EntityCommonName`[match(MA_DATA_Shaped$ModelID, MA_Info$ModelID)]
MA_DATA_Shaped$SICS <- MA_Info$`docinfo:EntitySectorIndustryClassification`[match(MA_DATA_Shaped$ModelID, MA_Info$ModelID)]

n <- c("Company","Ticker")

MA_DATA_Shaped[n] <- sapply(MA_DATA_Shaped[n],as.character)

# Fill SG&A w/ Row Values that equal 0

MA_DATA_Shaped$`position:SellingGeneralAdministrative5yCapitalization` <- ifelse(MA_DATA_Shaped$`position:SellingGeneralAdministrative5yCapitalization` <=0, 
                                                                                 MA_DATA_Shaped$`perform:SellingGeneralAdministrative`, 
                                                                                 MA_DATA_Shaped$`position:SellingGeneralAdministrative5yCapitalization`)

MA_DATA_Shaped<-MA_DATA_Shaped[,c("Company","Ticker","SICS","ModelID","perform:OperatingRevenue","perform:CostRevenue",
                                  "position:SellingGeneralAdministrative5yCapitalization",
                                  "position:PropertyPlantEquipmentGross","position:OtherIntangibleAssets","position:LeaseContractCommitments",
                                  "position:Goodwill","perform:ResearchDevelopmentExpenses", "perform:FCF", 'value:MarketCap')]


#Identify inputs & outputs

output <- select( MA_DATA_Shaped, 'perform:OperatingRevenue')
input <- select (MA_DATA_Shaped, 'perform:CostRevenue', 'position:SellingGeneralAdministrative5yCapitalization', 
                 'position:PropertyPlantEquipmentGross','position:LeaseContractCommitments', 
                 'perform:ResearchDevelopmentExpenses', 'position:Goodwill', 'position:OtherIntangibleAssets')


#RUN PROGRAM 

Program <- dea(XREF = input, YREF = output, X = input[,], Y = output[,], model = "input", RTS = 'variable')
Program

####### Results ########

Result <- cbind(round(Program$thetaOpt, 4), round(Program$lambda, 4))
rownames(Result) <- MA_DATA_Shaped[[2]]
colnames(Result) <- c("Firm Efficiency", rownames(Result))
kable(Result[,])

## Export Data for Backtest in Bloomberg ##


Firm_Efficiency <- (Result[,c(1,2)])

Firm_Efficiency <- data.table(Firm_Efficiency, keep.rownames=TRUE)
colnames(Firm_Efficiency) <- c('Ticker', 'Firm Efficiency', '-')
tibble(Firm_Efficiency)
Firm_Efficiency <- select(Firm_Efficiency, -c('-'))

# Merge and Filter

MA_DATA_Shaped$Firm_Efficiency <- Firm_Efficiency$`Firm Efficiency`[match(MA_DATA_Shaped$Ticker, Firm_Efficiency$Ticker)]
Filtered <- subset(MA_DATA_Shaped, (MA_DATA_Shaped$`perform:FCF` < 0) & (MA_DATA_Shaped$SICS <= 10102050) & (MA_DATA_Shaped$`value:MarketCap` < 50000))

#### NOTE: I did not add filter for Firm Efficiency = 1 here for data analysis purposes        

write.csv(Filtered, 'Desktop/2021_Firm_Efficiency_Baseline.csv')