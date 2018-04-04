final <- read.csv("C:\\Users\\Tianxin.Liu\\Dropbox\\DAEN690\\Data\\Data Combination\\final_dataset.csv")

library(scales)
library(car)
library(MASS)

final.normalized <- data.frame(final$Date)
colnames(final.normalized) <- "date"

final.normalized$survey.oecd <- rescale(final$Survey.OECD)
final.normalized$export<- rescale(final$Export)
final.normalized$import <- rescale(final$Import)
final.normalized$pmi.manu <- rescale(final$PMI.Manu)
final.normalized$pmi.nonmanu <- rescale(final$PMI.NonManu)
final.normalized$m0 <- rescale(final$M0)
final.normalized$m1 <- rescale(final$M1)
final.normalized$policy.uncertainty <- rescale(final$Policy.Uncertainty)
final.normalized$manu.ci <- rescale(final$Manufact.CI)
final.normalized$manu.ex <- rescale(final$Manufact.Ex)
final.normalized$building.sale <- rescale(final$Building.Sale)
final.normalized$building.sale.rate <- rescale(final$Building.Sale.Rate)
final.normalized$cpi <- rescale(final$CPI)
final.normalized$cpi.residence <- rescale(final$CPI.Residence)
final.normalized$cpi.transportation <- rescale(final$CPI.Transportation)
final.normalized$cpi.household <- rescale(final$CPI.Household)
final.normalized$cpi.health <- rescale(final$CPI.Health)
final.normalized$cpi.education <- rescale(final$CPI.Education)
final.normalized$sse <- rescale(final$SSE.10D.MA)
final.normalized$ssecbi <- rescale(final$SSECBI.10D.MA)
final.normalized$exchange.rate <- rescale(final$Exchange.Rate)
final.normalized$ETF.volatility <- rescale(final$ETF.Volatility)
final.normalized$bond.ytm <- rescale(final$Bond.YTM)
final.normalized$china.gdp <- rescale(final$GDP.China)
final.normalized$china.prime <- rescale(final$CN.Prime.Ind.Value)
final.normalized$china.second <- rescale(final$CN.Second.Ind.Value)
final.normalized$china.tertairy <- rescale(final$CN.Tertiary.Ind.Value)
final.normalized$us.inflation <- rescale(final$US_INF_RATE)
final.normalized$us.balance.trade <- rescale(final$US_BAL_TRADE)
final.normalized$us.gdp <- rescale(final$US_GDP)
final.normalized$us.debt <- rescale(final$US_FED_DEBT)
final.normalized$us.shortterm <- rescale(final$US_ST_INT)
final.normalized$us.longterm <- rescale(final$US_LT_INT)

final.normalized$ftse <- final$FTSE.Price
final.normalized <- final.normalized[,-1]

fit.1 <- lm(final.normalized.ftse~ ., data=data.frame(final.normalized[,1:5],final.normalized$ftse))
# evaluate collinearity
sqrt(vif(fit.1)) > 2
