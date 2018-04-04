# remove seasonality
final <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\final_dataset.csv")

Survey.OECD <- ts(final$Survey.OECD,frequency = 52, start = c(2013,2,8)); survey.oecd <- Survey.OECD - decompose(Survey.OECD)$seasonal
Export <- ts(final$Export,frequency = 52, start = c(2013,2,8)); export <- Export - decompose(Export)$seasonal
Import <- ts(final$Import, frequency = 52, start = c(2013,2,8)); import <- Import - decompose(Import)$seasonal
PMI.Manu <- ts(final$PMI.Manu, frequency = 52, start = c(2013,2,8)); pmi.manu <- PMI.Manu - decompose(PMI.Manu)$seasonal
PMI.NonManu <- ts(final$PMI.NonManu, frequency = 52, start = c(2013,2,8)); pmi.nonmanu <- PMI.NonManu - decompose(PMI.NonManu)$seasonal
M0 <- ts(final$M0, frequency = 52, start = c(2013,2,8)); m0 <- M0 - decompose(M0)$seasonal
M1 <- ts(final$M1, frequency = 52, start = c(2013,2,8)); m1 <- M1 - decompose(M1)$seasonal
Policy.Uncertainty <- ts(final$Policy.Uncertainty, frequency = 52, start = c(2013,2,8)); policy.uncertainty <- Policy.Uncertainty - decompose(Policy.Uncertainty)$seasonal
Manufact.CI <- ts(final$Manufact.CI, frequency = 52, start = c(2013,2,8)); manu.ci <- Manufact.CI - decompose(Manufact.CI)$seasonal
Manufact.Ex <- ts(final$Manufact.Ex, frequency = 52, start = c(2013,2,8)); manu.ex <- Manufact.Ex - decompose(Manufact.Ex)$seasonal
Building.Sale <- ts(final$Building.Sale, frequency = 52, start = c(2013,2,8)); building.sale <- Building.Sale - decompose(Building.Sale)$seasonal
Building.Sale.Rate <- ts(final$Building.Sale.Rate, frequency = 52, start = c(2013,2,8)); building.sale.rate <- Building.Sale.Rate - decompose(Building.Sale.Rate)$seasonal
CPI <- ts(final$CPI, frequency = 52, start = c(2013,2,8)); cpi <- CPI - decompose(CPI)$seasonal
CPI.Residence <- ts(final$CPI.Residence, frequency = 52, start = c(2013,2,8)); cpi.residence <- CPI.Residence - decompose(CPI.Residence)$seasonal
CPI.Transportation <- ts(final$CPI.Transportation, frequency = 52, start = c(2013,2,8)); cpi.transportation <- CPI.Transportation - decompose(CPI.Transportation)$seasonal
CPI.Household <- ts(final$CPI.Household, frequency = 52, start = c(2013,2,8)); cpi.household <- CPI.Household - decompose(CPI.Household)$seasonal
CPI.Health <- ts(final$CPI.Health, frequency = 52, start = c(2013,2,8)); cpi.health <- CPI.Health - decompose(CPI.Health)$seasonal
CPI.Education <- ts(final$CPI.Education, frequency = 52, start = c(2013,2,8)); cpi.education <- CPI.Education - decompose(CPI.Education)$seasonal
SSE <- ts(final$SSE.10D.MA, frequency = 52, start = c(2013,2,8)); sse <- SSE - decompose(SSE)$seasonal
SSECBI <- ts(final$SSECBI.10D.MA, frequency = 52, start = c(2013,2,8)); ssecbi <- SSECBI - decompose(SSECBI)$seasonal
Exchange.Rate <- ts(final$Exchange.Rate, frequency = 52, start = c(2013,2,8)); exchange.rate <- Exchange.Rate - decompose(Exchange.Rate)$seasonal
ETF.Volatility <- ts(final$ETF.Volatility, frequency = 52, start = c(2013,2,8)); etf.volatility <- ETF.Volatility - decompose(ETF.Volatility)$seasonal
Bond.YTM <- ts(final$Bond.YTM, frequency = 52, start = c(2013,2,8)); bond.ytm <- Bond.YTM - decompose(Bond.YTM)$seasonal
CN.GDP <- ts(final$GDP.China, frequency = 52, start = c(2013,2,8)); cn.gdp <- CN.GDP - decompose(CN.GDP)$seasonal
CN.Prime <- ts(final$CN.Prime.Ind.Value, frequency = 52, start = c(2013,2,8)); cn.prime <- CN.Prime - decompose(CN.Prime)$seasonal
CN.Second <- ts(final$CN.Second.Ind.Value, frequency = 52, start = c(2013,2,8)); cn.second <- CN.Second - decompose(CN.Second)$seasonal
CN.Tertiary <- ts(final$CN.Tertiary.Ind.Value, frequency = 52, start = c(2013,2,8)); cn.tertiary <- CN.Tertiary - decompose(CN.Tertiary)$seasonal
US.Inflation <- ts(final$US_INF_RATE, frequency = 52, start = c(2013,2,8)); us.inflation <- US.Inflation - decompose(US.Inflation)$seasonal
US.Trade <- ts(final$US_BAL_TRADE, frequency = 52, start = c(2013,2,8)); us.trade <- US.Trade - decompose(US.Trade)$seasonal
US.GDP <- ts(final$US_GDP, frequency = 52, start = c(2013,2,8)); us.gdp <- US.GDP - decompose(US.GDP)$seasonal
US.Debt <- ts(final$US_FED_DEBT, frequency = 52, start = c(2013,2,8)); us.debt <- US.Debt - decompose(US.Debt)$seasonal
US.Longterm <- ts(final$US_LT_INT, frequency = 52, start = c(2013,2,8)); us.longterm <- US.Longterm - decompose(US.Longterm)$seasonal
US.Shortterm <- ts(final$US_ST_INT, frequency = 52, start = c(2013,2,8)); us.shortterm <- US.Shortterm - decompose(US.Shortterm)$seasonal
FTSE <- ts(final$FTSE.Price, frequency = 52, start = c(2013,2,8)); ftse <- FTSE - decompose(FTSE)$seasonal

weekly <- seq(as.Date("2013-02-08"),as.Date("2018-02-04"),by=7)
final.dataset.noseasonality <- data.frame(weekly, survey.oecd,export,import,
                                          pmi.manu,pmi.nonmanu,m0,m1,
                                          policy.uncertainty,manu.ci,manu.ex,
                                          building.sale,building.sale.rate,cpi,
                                          cpi.residence,cpi.transportation,cpi.household,
                                          cpi.health,cpi.education,sse,ssecbi,exchange.rate,
                                          etf.volatility,bond.ytm,cn.gdp,cn.prime,
                                          cn.second,cn.tertiary,us.inflation,us.trade,
                                          us.gdp,us.debt,us.shortterm,us.longterm,ftse)

write.csv(final.dataset.noseasonality,"C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\final_dataset_nonseasonlity.csv", row.names = F)
