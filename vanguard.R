library(PerformanceAnalytics)
library(quantmod)
library(DEoptim)

# Define the list of ETFs
etfs <- list(
  # Dividend and Value Equity ETFs
  DIVIDEND_VALUE = c("VIG", "VYM", "VONG", "VONV", "VTV", "VFVA"),
  
  # Growth Equity ETFs
  GROWTH = c("VUG", "MGC", "MGK", "VOT", "VOE", "VONE", "VOO", "VOOG", "VOOV", "IVOO", "IVOG", "IVOV", "VIOV"),
  
  # Size and Style Equity ETFs
  SIZE_STYLE = c("VB", "VBK", "VBR", "VFMV", "VFMO", "VFMF", "VFQY"),
  
  # Global and Total Market Equity ETFs
  GLOBAL_TOTAL_MARKET = c("VXF", "VEU", "VSS", "VEA", "VXUS", "VT"),
  
  # Emerging Markets Equity ETFs
  EMERGING_MARKETS = c("VWO", "VGK", "VPL", "VNQI", "VUG", "VYM", "VIGI", "VYMI", "VWO", "VONG", "VTWO", "VTWG", "VTWV", "VTHR"),
  
  # ESG (Environmental, Social, and Governance) Equity ETFs
  ESG = c("VSGX", "ESGV", "VV", "VTI", "VTV"),
  
  # U.S. Treasury Bonds
  US_TREASURY = c("VGIT", "EDV", "VGLT", "VGSH"),
  
  # Mortgage-Backed Securities
  MBS = c("VMBS"),
  
  # Corporate Bonds
  CORPORATE_BONDS = c("VCRB", "VPLS", "VCEB", "BIV", "VCIT", "VCLT", "VTC"),
  
  # Total Bond Market ETFs
  TOTAL_BOND_MARKET = c("BND", "BNDX", "BNDW"),
  
  # Intermediate-Term Municipal Bonds
  MUNICIPAL_BONDS = c("VTEI", "VTEB", "VTEC", "VTES"),
  
  # Short-Term and Inflation-Protected Bonds
  SHORT_TERM_INFLATION = c("VTC", "VTIP", "VGSH", "BSV", "VCSH", "VUSB"),
  
  # International Bonds
  INTERNATIONAL_BONDS = c("VWOB")
)

# Load ETF data
e <- new.env()
getSymbols(unlist(etfs), env = e, from = "2021-01-01")

# Extract adjusted monthly prices
aa <- eapply(e, function(x) to.monthly(x, name = names(x)))
port <- do.call(merge, lapply(aa, Ad))
colnames(port) <- gsub(".Adjusted", "", names(port))
port <- ROC(port, type = "discrete")
port[is.na(port)] <- 0

# Group ETFs by sectors
DIVIDEND_VALUE <- port[, etfs$DIVIDEND_VALUE]
DIVIDEND_VALUE <- reclass(coredata(DIVIDEND_VALUE) %*% rep(1/ncol(DIVIDEND_VALUE), ncol(DIVIDEND_VALUE)), match.to = DIVIDEND_VALUE)

GROWTH <- port[, etfs$GROWTH]
GROWTH <- reclass(coredata(GROWTH) %*% rep(1/ncol(GROWTH), ncol(GROWTH)), match.to = GROWTH)

SIZE_STYLE <- port[, etfs$SIZE_STYLE]
SIZE_STYLE <- reclass(coredata(SIZE_STYLE) %*% rep(1/ncol(SIZE_STYLE), ncol(SIZE_STYLE)), match.to = SIZE_STYLE)

GLOBAL_TOTAL_MARKET <- port[, etfs$GLOBAL_TOTAL_MARKET]
GLOBAL_TOTAL_MARKET <- reclass(coredata(GLOBAL_TOTAL_MARKET) %*% rep(1/ncol(GLOBAL_TOTAL_MARKET), ncol(GLOBAL_TOTAL_MARKET)), match.to = GLOBAL_TOTAL_MARKET)

EMERGING_MARKETS <- port[, etfs$EMERGING_MARKETS]
EMERGING_MARKETS <- reclass(coredata(EMERGING_MARKETS) %*% rep(1/ncol(EMERGING_MARKETS), ncol(EMERGING_MARKETS)), match.to = EMERGING_MARKETS)

ESG <- port[, etfs$ESG]
ESG <- reclass(coredata(ESG) %*% rep(1/ncol(ESG), ncol(ESG)), match.to = ESG)

US_TREASURY <- port[, etfs$US_TREASURY]
US_TREASURY <- reclass(coredata(US_TREASURY) %*% rep(1/ncol(US_TREASURY), ncol(US_TREASURY)), match.to = US_TREASURY)

MBS <- port[, etfs$MBS]
MBS <- reclass(coredata(MBS) %*% rep(1/ncol(MBS), ncol(MBS)), match.to = MBS)

CORPORATE_BONDS <- port[, etfs$CORPORATE_BONDS]
CORPORATE_BONDS <- reclass(coredata(CORPORATE_BONDS) %*% rep(1/ncol(CORPORATE_BONDS), ncol(CORPORATE_BONDS)), match.to = CORPORATE_BONDS)

TOTAL_BOND_MARKET <- port[, etfs$TOTAL_BOND_MARKET]
TOTAL_BOND_MARKET <- reclass(coredata(TOTAL_BOND_MARKET) %*% rep(1/ncol(TOTAL_BOND_MARKET), ncol(TOTAL_BOND_MARKET)), match.to = TOTAL_BOND_MARKET)

MUNICIPAL_BONDS <- port[, etfs$MUNICIPAL_BONDS]
MUNICIPAL_BONDS <- reclass(coredata(MUNICIPAL_BONDS) %*% rep(1/ncol(MUNICIPAL_BONDS), ncol(MUNICIPAL_BONDS)), match.to = MUNICIPAL_BONDS)

SHORT_TERM_INFLATION <- port[, etfs$SHORT_TERM_INFLATION]
SHORT_TERM_INFLATION <- reclass(coredata(SHORT_TERM_INFLATION) %*% rep(1/ncol(SHORT_TERM_INFLATION), ncol(SHORT_TERM_INFLATION)), match.to = SHORT_TERM_INFLATION)

INTERNATIONAL_BONDS <- port[, etfs$INTERNATIONAL_BONDS]
INTERNATIONAL_BONDS <- reclass(coredata(INTERNATIONAL_BONDS) %*% rep(1/ncol(INTERNATIONAL_BONDS), ncol(INTERNATIONAL_BONDS)), match.to = INTERNATIONAL_BONDS)

# Merge all sectors
ALL <- merge(DIVIDEND_VALUE, GROWTH, SIZE_STYLE, GLOBAL_TOTAL_MARKET, EMERGING_MARKETS, ESG, US_TREASURY, MBS, CORPORATE_BONDS, TOTAL_BOND_MARKET, MUNICIPAL_BONDS, SHORT_TERM_INFLATION, INTERNATIONAL_BONDS)

# Plot performance summary
charts.PerformanceSummary(ALL, geometric = FALSE, colorset = rich12equal, cex.legend = 0.45)

# Generate 77 random weights
random_weights <- runif(ncol(ALL))
random_weights <- random_weights / sum(random_weights)  # Normalize to sum up to 1

# Display the generated random weights
print(data.frame(Ticker = colnames(ALL), Weight = random_weights))

# Calculate weighted returns
WTD <- as.matrix(ALL) %*% random_weights
WTD <- reclass(WTD, match.to = ALL)

SPY <- ROC(Ad(to.monthly(getSymbols("^GSPC", auto.assign = FALSE, from = "2021-01-01"))), type = "discrete")
SPY[is.na(SPY)] <- 0
colnames(SPY) <- "SPY"
charts.PerformanceSummary(merge(WTD,SPY), geometric = FALSE)

toOptim = function(n) {
  if (length(n) != ncol(ALL)) {
    stop("Number of elements in 'n' must match the number of columns in ALL")
  }
  WTD <- reclass(coredata(ALL) %*% n, match.to = ALL)
  wt.penalty = 100 * (1 - sum(n))^2
  return(-colSums(WTD) + wt.penalty)
}


fnmap_f <- function(x){c(round(x,2))}

r <- DEoptim(toOptim, lower = rep(0.00, ncol(ALL)),upper = rep(0.25, ncol(ALL)),
             control = list(itermax = 10000), fnMap = fnmap_f)

r$optim$bestmem
sum(r$optim$bestmem)
res <- reclass(coredata(ALL) %*% as.numeric(r$optim$bestmem), match.to = ALL)
colnames(res) <- "bestRet"
charts.PerformanceSummary(merge(WTD,SPY,res), geometric = FALSE)

BETAoptim = function(n)
{
  WTD <- reclass(coredata(ALL) %*% c(n[1],n[2],n[3],n[4],n[5],n[6], n[7],n[8],n[9],n[10],n[11],n[12],n[13]), match.to = ALL)
  obj = CAPM.beta(Ra = WTD,Rb=SPY, Rf =0) 
  wt.penalty = 100 * (1 - sum(n))^2
  return(obj + wt.penalty)
}

r2 <- DEoptim(BETAoptim, lower = rep(0.00, ncol(ALL)), upper = rep(0.25, ncol(ALL)),
              control = list(itermax = 10000), fnMap = fnmap_f)

r2$optim$bestmem
sum(r2$optim$bestmem)
res2 <- reclass(coredata(ALL) %*% as.numeric(r2$optim$bestmem), match.to = ALL)
colnames(res) <- "BETA"
charts.PerformanceSummary(merge(WTD,SPY,res, res2), geometric = FALSE)

eqlwt <- reclass(coredata(ALL) %*% rep(1/ncol(ALL), ncol(ALL)), match.to = ALL)
colnames(eqlwt) <- "Equal Weight"
charts.PerformanceSummary(merge(WTD,SPY,res, res2, eqlwt), geometric = FALSE,  cex.legend =0.50)

# Convert 'Date' column to Date format
ALL$Date <- as.Date(ALL$Date)

# Assuming your data has a 'Date' column, you can extract the year from it
ALL$Year <- as.numeric(format(ALL$Date, "%Y"))

# List to store results for each year
yearly_risk_stats <- list()

# Loop through each year starting from 2021
for (year in 2021:2023) {  # Adjust the range as needed
  # Subset data for the current year
  year_data <- ALL[ALL$Year == year, ]
  
  # Calculate excess returns
  excess_returns <- year_data[, !(colnames(year_data) %in% c("Date", "Year"))] - rf_rate
  
  # Calculate Value at Risk (VaR) and Conditional Value at Risk (CVaR)
  VaR <- apply(excess_returns, 2, function(x) quantile(x, 0.05))
  CVaR <- apply(excess_returns, 2, function(x) mean(x[x <= VaR]))
  
  # Calculate Volatility
  volatility <- apply(excess_returns, 2, sd)
  
  # Calculate Average Return
  average_return <- colMeans(excess_returns)
  
  # Calculate Sharpe Ratio
  sharpe_ratio <- (average_return - rf_rate) / volatility
  
  # Calculate Tracking Error
  tracking_error <- sqrt(mean((excess_returns - matrix(rep(beta, nrow(excess_returns)), ncol = ncol(excess_returns), byrow = TRUE) * (market_returns - rf_rate))^2))
  
  # Calculate Beta and Expected Return using CAPM
  beta <- lm(excess_returns ~ market_returns)$coefficients[2]
  expected_market_return <- mean(market_returns)
  expected_return <- rf_rate + beta * (expected_market_return - rf_rate)
  
  # Calculate Information Ratio
  active_returns <- excess_returns - beta * (market_returns - rf_rate)
  information_ratio <- mean(active_returns) / sd(active_returns)
  
  # Calculate R-squared
  model <- lm(excess_returns ~ market_returns)
  r_squared <- summary(model)$r.squared
  
  # Combine results for the current year
  risk_stats <- data.frame(
    Date = as.Date(paste0("01-01-", year)),  # Assuming January 1st of the year
    Sector = colnames(excess_returns),
    VaR = VaR,
    CVaR = CVaR,
    Volatility = volatility,
    Average_Return = average_return,
    Sharpe_Ratio = sharpe_ratio,
    Tracking_Error = tracking_error,
    Beta = beta,
    Expected_Return = expected_return,
    Information_Ratio = information_ratio,
    R_Squared = r_squared
  )
  
  # Append results to the list
  yearly_risk_stats[[as.character(year)]] <- risk_stats
}

# Combine results for all years
all_years_risk_stats <- do.call(rbind, yearly_risk_stats)

# Print the results
print(all_years_risk_stats)

# Save the results to a CSV file
write.csv(ALL, file = "all.csv", row.names = FALSE)


# Load ETF data for the specified time period
e <- new.env()
getSymbols(unlist(etfs), env = e, from = "2021-01-01", to = "2022-01-01")

# Extract adjusted monthly prices
aa <- eapply(e, function(x) to.monthly(x, name = names(x)))
port <- do.call(merge, lapply(aa, Ad))
colnames(port) <- gsub(".Adjusted", "", names(port))
port <- ROC(port, type = "discrete")
port[is.na(port)] <- 0

# Group ETFs by sectors
# (Same grouping and processing as in the original code)

# Merge all sectors
ALL2021 <- merge(DIVIDEND_VALUE, GROWTH, SIZE_STYLE, GLOBAL_TOTAL_MARKET, EMERGING_MARKETS, ESG, US_TREASURY, MBS, CORPORATE_BONDS, TOTAL_BOND_MARKET, MUNICIPAL_BONDS, SHORT_TERM_INFLATION, INTERNATIONAL_BONDS)

# Load ETF data for the specified time period
e <- new.env()
getSymbols(unlist(etfs), env = e, from = "2022-01-01", to = "2023-01-01")

# Extract adjusted monthly prices
aa <- eapply(e, function(x) to.monthly(x, name = names(x)))
port <- do.call(merge, lapply(aa, Ad))
colnames(port) <- gsub(".Adjusted", "", names(port))
port <- ROC(port, type = "discrete")
port[is.na(port)] <- 0

# Group ETFs by sectors
# (Same grouping and processing as in the original code)

# Merge all sectors
ALL2022 <- merge(DIVIDEND_VALUE, GROWTH, SIZE_STYLE, GLOBAL_TOTAL_MARKET, EMERGING_MARKETS, ESG, US_TREASURY, MBS, CORPORATE_BONDS, TOTAL_BOND_MARKET, MUNICIPAL_BONDS, SHORT_TERM_INFLATION, INTERNATIONAL_BONDS)

# Load ETF data for the specified time period
e <- new.env()
getSymbols(unlist(etfs), env = e, from = "2023-01-01", to = "2023-12-01")

# Extract adjusted monthly prices
aa <- eapply(e, function(x) to.monthly(x, name = names(x)))
port <- do.call(merge, lapply(aa, Ad))
colnames(port) <- gsub(".Adjusted", "", names(port))
port <- ROC(port, type = "discrete")
port[is.na(port)] <- 0

# Group ETFs by sectors
# (Same grouping and processing as in the original code)

# Merge all sectors
ALL2023 <- merge(DIVIDEND_VALUE, GROWTH, SIZE_STYLE, GLOBAL_TOTAL_MARKET, EMERGING_MARKETS, ESG, US_TREASURY, MBS, CORPORATE_BONDS, TOTAL_BOND_MARKET, MUNICIPAL_BONDS, SHORT_TERM_INFLATION, INTERNATIONAL_BONDS)
toOptim = function(n) {
  if (length(n) != ncol(ALL2023)) {
    stop("Number of elements in 'n' must match the number of columns in ALL")
  }
  WTD <- reclass(coredata(ALL2023) %*% n, match.to = ALL2023)
  wt.penalty = 100 * (1 - sum(n))^2
  return(-colSums(WTD) + wt.penalty)
}

#######################################################
fnmap_f <- function(x){c(round(x,2))}

r <- DEoptim(toOptim, lower = rep(0.00, ncol(ALL2023)),upper = rep(0.25, ncol(ALL2023)),
             control = list(itermax = 5000), fnMap = fnmap_f)

r$optim$bestmem
sum(r$optim$bestmem)
res <- reclass(coredata(ALL2021) %*% as.numeric(r$optim$bestmem), match.to = ALL2023)
colnames(res) <- "bestRet"
charts.PerformanceSummary(merge(WTD,SPY,res), geometric = FALSE)

BETAoptim = function(n)
{
  WTD <- reclass(coredata(ALL2023) %*% c(n[1],n[2],n[3],n[4],n[5],n[6], n[7],n[8],n[9],n[10],n[11],n[12],n[13]), match.to = ALL2023)
  obj = CAPM.beta(Ra = WTD,Rb=SPY, Rf =0) 
  wt.penalty = 100 * (1 - sum(n))^2
  return(obj + wt.penalty)
}

r2 <- DEoptim(BETAoptim, lower = rep(0.00, ncol(ALL2023)), upper = rep(0.25, ncol(ALL2023)),
              control = list(itermax = 5000), fnMap = fnmap_f)

r2$optim$bestmem
sum(r2$optim$bestmem)
res2 <- reclass(coredata(ALL2023) %*% as.numeric(r2$optim$bestmem), match.to = ALL2023)
colnames(res) <- "BETA"
charts.PerformanceSummary(merge(WTD,SPY,res, res2), geometric = FALSE)
###################################

fnmap_f <- function(x){c(round(x,2))}

r <- DEoptim(toOptim, lower = rep(0.00, ncol(ALL2021)),upper = rep(0.25, ncol(ALL2021)),
             control = list(itermax = 5000), fnMap = fnmap_f)

r$optim$bestmem
sum(r$optim$bestmem)
res <- reclass(coredata(ALL2021) %*% as.numeric(r$optim$bestmem), match.to = ALL2021)
colnames(res) <- "bestRet"
charts.PerformanceSummary(merge(WTD,SPY,res), geometric = FALSE)

BETAoptim = function(n)
{
  WTD <- reclass(coredata(ALL2021) %*% c(n[1],n[2],n[3],n[4],n[5],n[6], n[7],n[8],n[9],n[10],n[11],n[12],n[13]), match.to = ALL2021)
  obj = CAPM.beta(Ra = WTD,Rb=SPY, Rf =0) 
  wt.penalty = 100 * (1 - sum(n))^2
  return(obj + wt.penalty)
}

r2 <- DEoptim(BETAoptim, lower = rep(0.00, ncol(ALL2021)), upper = rep(0.25, ncol(ALL2021)),
              control = list(itermax = 5000), fnMap = fnmap_f)

r2$optim$bestmem
sum(r2$optim$bestmem)
res2 <- reclass(coredata(ALL2021) %*% as.numeric(r2$optim$bestmem), match.to = ALL2021)
colnames(res) <- "BETA"
charts.PerformanceSummary(merge(WTD,SPY,res, res2), geometric = FALSE)

#######################################################

toOptim = function(n) {
  if (length(n) != ncol(ALL2022)) {
    stop("Number of elements in 'n' must match the number of columns in ALL")
  }
  WTD <- reclass(coredata(ALL2022) %*% n, match.to = ALL2022)
  wt.penalty = 100 * (1 - sum(n))^2
  return(-colSums(WTD) + wt.penalty)
}


fnmap_f <- function(x){c(round(x,2))}

r <- DEoptim(toOptim, lower = rep(0.00, ncol(ALL2022)),upper = rep(0.25, ncol(ALL2022)),
             control = list(itermax = 5000), fnMap = fnmap_f)

r$optim$bestmem
sum(r$optim$bestmem)
res <- reclass(coredata(ALL2021) %*% as.numeric(r$optim$bestmem), match.to = ALL2021)
colnames(res) <- "bestRet"
charts.PerformanceSummary(merge(WTD,SPY,res), geometric = FALSE)

BETAoptim = function(n)
{
  WTD <- reclass(coredata(ALL2022) %*% c(n[1],n[2],n[3],n[4],n[5],n[6], n[7],n[8],n[9],n[10],n[11],n[12],n[13]), match.to = ALL2022)
  obj = CAPM.beta(Ra = WTD,Rb=SPY, Rf =0) 
  wt.penalty = 100 * (1 - sum(n))^2
  return(obj + wt.penalty)
}

r2 <- DEoptim(BETAoptim, lower = rep(0.00, ncol(ALL2022)), upper = rep(0.25, ncol(ALL2022)),
              control = list(itermax = 5000), fnMap = fnmap_f)

r2$optim$bestmem
sum(r2$optim$bestmem)
res2 <- reclass(coredata(ALL2022) %*% as.numeric(r2$optim$bestmem), match.to = ALL2022)
colnames(res) <- "BETA"
charts.PerformanceSummary(merge(WTD,SPY,res, res2), geometric = FALSE)