# -------------------------------
# Financial Econ Project - Portfolio Analysis
# Description: Cleaning and building factor-based portfolios for 2005-2025 (Dates Adjustable)
# Author: Andy Giorgio
# Date: April 25, 2025
# -------------------------------

# Load necessary libraries
library(tidyverse)     # For data manipulation and visualization
library(lubridate)     # For handling dates
library(readxl)        # For reading Excel files
library(zoo)           # For time series functionality
library(scales)        # For formatting and plotting
library(tidyr)         # For pivoting and reshaping
library(dplyr)         # For data manipulation
library(ggplot2)       # For making plots and visualizations

# Read in the Excel file
rawData <- read_excel("~/Downloads/FinancialEconProject.xlsx")
glimpse(rawData)
colnames(rawData) <- gsub(" .*", "", colnames(rawData))

# Cleaning empty or unneeded columns
colnames(rawData)[500:520]
colnames(rawData)[1000:1020]
rawData1 <- rawData[, -c(506, 507, 1012, 1013)]

# Reshaping data to be more easy to use for analysis
# Step 1: Extract the second row, which has the factor names
factor_names <- as.character(unlist(rawData1[1, ]))
unique(factor_names)
# Step 2: Clean up factor names
factor_names <- gsub("PX_LAST", "Price", factor_names)
factor_names <- gsub("VOLATILITY_360D", "Volatility", factor_names)
factor_names <- gsub("PE_RATIO", "PE", factor_names)
# Step 3: Combine column names (which include the ticker) with cleaned factor names
new_names <- paste(colnames(rawData1), factor_names, sep = "_")
# Step 4: Assign new names to the columns
colnames(rawData1) <- new_names
# Step 5: Remove the second row from the data (since it is now incorporated into the header)
rawData1 <- rawData1[-1, ]
colnames(rawData1)[1] <- "Date"
#Fixing Dates From Excel
rawData1$Date <- as.Date(as.numeric(rawData1$Date), origin = "1899-12-30")

#Converting to Long Data 
longData <- rawData1 %>%
    pivot_longer(cols = -Date,    # Keep 'date' as is, pivot the rest
                 names_to = "ticker_factor",  # Column for ticker and factor names
                 values_to = "value")  # Values from all factor columns

# Split 'ticker_factor' into 'ticker' and 'factor' columns
longData <- longData %>%
    separate(ticker_factor, into = c("ticker", "factor"), sep = "_")

#Converting to ideal table structuring
idealData <- longData %>%
    pivot_wider(names_from = factor, values_from = value)

# Convert Factors to numeric
idealData$Price <- as.numeric(idealData$Price)
idealData$PE <- as.numeric(idealData$PE)
idealData$Volatility <- as.numeric(idealData$Volatility)


#Momentum Calculations
# Calculate monthly and yearly returns
idealData <- idealData %>%
    group_by(ticker) %>%
    arrange(Date) %>%
    mutate(
        # Calculate 1-month return (lag of 1 month)
        return_1m = (Price / lag(Price, 1) - 1),
        
        # Calculate 1-year return (lag of 12 months)
        return_1y = (Price / lag(Price, 12) - 1)
    )

# Calculate momentum as the difference between 1-year and 1-month returns
idealData <- idealData %>%
    mutate(
        momentum = return_1y - return_1m
    )

#Calculating 3 month returns for re-balancing later
idealData <- idealData %>%
    arrange(ticker, Date) %>%
    group_by(ticker) %>%
    mutate(
        return_3m = (Price / lag(Price, n = 3) - 1)  # 3-month return
    ) %>%
    ungroup()

#Removing 2004
# Filter the data to only include dates after January 1st, 2005
portfolioData <- idealData %>%
    filter(as.Date(Date) >= "2005-01-01")

# Select only the necessary columns: Date, Ticker, Price, PE_Ratio, Volatility, and Momentum
portfolioData <- portfolioData %>%
    select(Date, ticker, Price, PE, Volatility, momentum, return_3m)

# View the final dataset
head(portfolioData)

# Filter the data to include only the years 2012-2017
portfolioData1 <- portfolioData %>%
    filter(Date >= "2005-01-01" & Date <= "2025-12-31")

###########################################
##### Momentum Portfolio Construction #####
###########################################

momentumPortfolio <- portfolioData1 %>%
    filter(!is.na(momentum)) %>%
    select(Date, ticker, Price, momentum, return_3m)

#Set rebalancing dates
rebalance_dates <- unique(as.Date(momentumPortfolio$Date))[seq(1, length(unique(momentumPortfolio$Date)), by = 3)]


portfolio_returns <- vector()  # To store portfolio returns

# Loop through the rebalance dates (except the last one)
for (i in 1:(length(rebalance_dates) - 1)) {
    
    # Get current and next rebalance dates
    rebalance_date <- rebalance_dates[i]
    next_rebalance_date <- rebalance_dates[i + 1]
    
    # Filtering the data for the current rebalance date
    current_data <- momentumPortfolio %>%
        filter(Date == rebalance_date)
    
    # Select the top 20% of companies based on momentum
    top_20_percent <- current_data %>%
        arrange(desc(momentum)) %>% 
        slice_head(n = 100) 
    
    # Get the tickers for the top 20% companies
    top_tickers <- top_20_percent$ticker
    
    # Data for the next period until the next rebalance date for these companies
    next_period_data <- momentumPortfolio %>%
        filter(ticker %in% top_tickers, Date == next_rebalance_date)
    
    # Calculate the return for the selected companies in this period
    next_period_data <- next_period_data %>%
        group_by(ticker) %>%
        mutate(return_period = return_3m) %>%
        ungroup()
    
    # Calculate the portfolio return for this rebalance period
    portfolio_return <- next_period_data %>%
        filter(Date == next_rebalance_date) %>% 
        summarize(
            weighted_return = sum(return_period, na.rm = TRUE) / length(top_tickers)
        )
    
    # Append the portfolio return for this period to the portfolio_returns vector
    portfolio_returns <- c(portfolio_returns, portfolio_return$weighted_return)
}

portfolio_std <- 2*sd(portfolio_returns)
mean_returns <- mean(portfolio_returns)
momentum_annual_returns <- ((1+mean_returns)^4-1)
momentum_sharpe <- ((momentum_annual_returns - 0.0429)/portfolio_std)

# Calculate cumulative returns (for performance tracking)
cumulative_returns <- cumprod(1 + portfolio_returns)

# Data frame for tracking portfolio returns over time
portfolio_performance <- data.frame(
    RebalanceDate = rebalance_dates[-1],  # Remove the first date
    PortfolioReturn = portfolio_returns,
    CumulativeReturn = cumulative_returns
)

# Filter for SPX
spx_data <- momentumPortfolio %>%
    filter(ticker == "SPX") %>%
    select(Date, Price) %>%
    arrange(Date)

# Calculate SPX returns
spx_data <- spx_data %>%
    mutate(
        CumulativeReturn = Price / first(Price),
        Return = (Price / lag(Price)) - 1
    )
spx_mean_return <- 12*mean(spx_data$Return, na.rm = TRUE)
spx_sd_return <- sqrt(12)*sd(spx_data$Return, na.rm = TRUE)
spx_sharpe <- ((spx_mean_return - 0.0429)/spx_sd_return)

# Make sure both start at the same date range (since your portfolio starts later)
aligned_spx <- spx_data %>%
    filter(Date %in% portfolio_performance$RebalanceDate)

# Calculate the compounded annual growth rate (CAGR) for the original portfolio
start_value <- portfolio_performance$CumulativeReturn[1]
end_value <- portfolio_performance$CumulativeReturn[length(portfolio_performance$CumulativeReturn)]
years <- as.numeric(difftime(max(portfolio_performance$RebalanceDate), min(portfolio_performance$RebalanceDate), units = "days")) / 365.25

# Calculate the original CAGR
original_cagr <- (end_value / start_value)^(1 / years) - 1

# Adjust CAGR for 2% survivorship bias
adjusted_cagr <- original_cagr - 0.03

# Apply the adjusted CAGR to calculate the adjusted cumulative returns over time
adjusted_cumulative_return <- start_value * (1 + adjusted_cagr)^(as.numeric(difftime(portfolio_performance$RebalanceDate, min(portfolio_performance$RebalanceDate), units = "days")) / 365.25)

# Create a combined data frame with adjusted returns
combined_returns <- data.frame(
    Date = portfolio_performance$RebalanceDate,
    Portfolio = portfolio_performance$CumulativeReturn,
    SPX = aligned_spx$CumulativeReturn,
    BiasAdjustedPortfolio = adjusted_cumulative_return  # Add the adjusted portfolio return
)

# Turn it into long format for easier plotting
combined_returns_long <- combined_returns %>%
    pivot_longer(cols = c(Portfolio, SPX, BiasAdjustedPortfolio), names_to = "Type", values_to = "CumulativeReturn")

# Now plot it
ggplot(combined_returns_long, aes(x = Date, y = CumulativeReturn, color = Type)) +
    geom_line(size = 1.2) +
    labs(
        title = "Cumulative Returns: Momentum Portfolio vs. S&P 500",
        x = "Date",
        y = "Cumulative Return",
        color = "Portfolio Type"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom"
    )

###############################################
##### End Momentum Portfolio Construction #####
###############################################




########################################
##### Value Portfolio Construction #####
########################################

valuePortfolio <- portfolioData1 %>%
    filter(!is.na(PE)) %>%
    select(Date, ticker, Price, PE, return_3m)

#Set rebalancing dates
rebalance_dates <- unique(as.Date(momentumPortfolio$Date))[seq(1, length(unique(momentumPortfolio$Date)), by = 3)]


portfolio_returns <- vector()  # To store portfolio returns

# Loop through the rebalance dates (except the last one)
for (i in 1:(length(rebalance_dates) - 1)) {
    
    # Get current and next rebalance dates
    rebalance_date <- rebalance_dates[i]
    next_rebalance_date <- rebalance_dates[i + 1]
    
    # Filtering the data for the current rebalance date
    current_data <- valuePortfolio %>%
        filter(Date == rebalance_date)
    
    # Select the bottom 20% of companies based on value
    top_20_percent <- current_data %>%
        arrange(desc(PE)) %>% 
        slice_head(n = 100) 
    
    # Get the tickers for the top 20% companies
    top_tickers <- top_20_percent$ticker
    
    # Data for the next period until the next rebalance date for these companies
    next_period_data <- valuePortfolio %>%
        filter(ticker %in% top_tickers, Date == next_rebalance_date)
    
    # Calculate the return for the selected companies in this period
    next_period_data <- next_period_data %>%
        group_by(ticker) %>%
        mutate(return_period = return_3m) %>%
        ungroup()
    
    # Calculate the portfolio return for this rebalance period
    portfolio_return <- next_period_data %>%
        filter(Date == next_rebalance_date) %>% 
        summarize(
            weighted_return = sum(return_period, na.rm = TRUE) / length(top_tickers)
        )
    
    # Append the portfolio return for this period to the portfolio_returns vector
    portfolio_returns <- c(portfolio_returns, portfolio_return$weighted_return)
}

portfolio_std <- sd(portfolio_returns)
mean_returns <- mean(portfolio_returns)
value_annual_returns <- ((1+mean_returns)^4-1)
value_sharpe <- ((value_annual_returns - 0.0429)/portfolio_std)

# Calculate cumulative returns (for performance tracking)
cumulative_returns <- cumprod(1 + portfolio_returns)

# Data frame for tracking portfolio returns over time
portfolio_performance <- data.frame(
    RebalanceDate = rebalance_dates[-1],  # Remove the first date
    PortfolioReturn = portfolio_returns,
    CumulativeReturn = cumulative_returns
)

# Calculate the compounded annual growth rate (CAGR) for the original portfolio
start_value <- portfolio_performance$CumulativeReturn[1]
end_value <- portfolio_performance$CumulativeReturn[length(portfolio_performance$CumulativeReturn)]
years <- as.numeric(difftime(max(portfolio_performance$RebalanceDate), min(portfolio_performance$RebalanceDate), units = "days")) / 365.25

# Calculate the original CAGR
original_cagr <- (end_value / start_value)^(1 / years) - 1

# Adjust CAGR for 2% survivorship bias
adjusted_cagr <- original_cagr - 0.03

# Apply the adjusted CAGR to calculate the adjusted cumulative returns over time
adjusted_cumulative_return <- start_value * (1 + adjusted_cagr)^(as.numeric(difftime(portfolio_performance$RebalanceDate, min(portfolio_performance$RebalanceDate), units = "days")) / 365.25)

# Create a combined data frame with adjusted returns
combined_returns <- data.frame(
    Date = portfolio_performance$RebalanceDate,
    Portfolio = portfolio_performance$CumulativeReturn,
    SPX = aligned_spx$CumulativeReturn,
    BiasAdjustedPortfolio = adjusted_cumulative_return  # Add the adjusted portfolio return
)

# Turn it into long format for easier plotting
combined_returns_long <- combined_returns %>%
    pivot_longer(cols = c(Portfolio, SPX, BiasAdjustedPortfolio), names_to = "Type", values_to = "CumulativeReturn")

# Now plot it
ggplot(combined_returns_long, aes(x = Date, y = CumulativeReturn, color = Type)) +
    geom_line(size = 1.2) +
    labs(
        title = "Cumulative Returns: Value Portfolio vs. S&P 500",
        x = "Date",
        y = "Cumulative Return",
        color = "Portfolio Type"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom"
    )

############################################
##### End Value Portfolio Construction #####
############################################



#################################################
##### Low Volatility Portfolio Construction #####
#################################################

volatilityPortfolio <- portfolioData %>%
    filter(!is.na(Volatility)) %>%
    select(Date, ticker, Price, Volatility, return_3m)

#Set rebalancing dates
rebalance_dates <- unique(as.Date(momentumPortfolio$Date))[seq(1, length(unique(momentumPortfolio$Date)), by = 3)]


portfolio_returns <- vector()  # To store portfolio returns

# Loop through the rebalance dates (except the last one)
for (i in 1:(length(rebalance_dates) - 1)) {
    
    # Get current and next rebalance dates
    rebalance_date <- rebalance_dates[i]
    next_rebalance_date <- rebalance_dates[i + 1]
    
    # Filtering the data for the current rebalance date
    current_data <- volatilityPortfolio %>%
        filter(Date == rebalance_date)
    
    # Select the top 20% of companies based on volatility
    top_20_percent <- current_data %>%
        arrange(Volatility) %>% 
        slice_head(n = 100) 
    
    # Get the tickers for the top 20% companies
    top_tickers <- top_20_percent$ticker
    
    # Data for the next period until the next rebalance date for these companies
    next_period_data <- valuePortfolio %>%
        filter(ticker %in% top_tickers, Date == next_rebalance_date)
    
    # Calculate the return for the selected companies in this period
    next_period_data <- next_period_data %>%
        group_by(ticker) %>%
        mutate(return_period = return_3m) %>%
        ungroup()
    
    # Calculate the portfolio return for this rebalance period
    portfolio_return <- next_period_data %>%
        filter(Date == next_rebalance_date) %>% 
        summarize(
            weighted_return = sum(return_period, na.rm = TRUE) / length(top_tickers)
        )
    
    # Append the portfolio return for this period to the portfolio_returns vector
    portfolio_returns <- c(portfolio_returns, portfolio_return$weighted_return)
}

portfolio_std <- sd(portfolio_returns)
mean_returns <- mean(portfolio_returns)
low_vol_annual_returns <- ((1+mean_returns)^4-1)
low_vol_sharpe <- ((low_vol_annual_returns - 0.0429)/portfolio_std)


# Calculate cumulative returns (for performance tracking)
cumulative_returns <- cumprod(1 + portfolio_returns)

# Data frame for tracking portfolio returns over time
portfolio_performance <- data.frame(
    RebalanceDate = rebalance_dates[-1],  # Remove the first date
    PortfolioReturn = portfolio_returns,
    CumulativeReturn = cumulative_returns
)

# Calculate the compounded annual growth rate (CAGR) for the original portfolio
start_value <- portfolio_performance$CumulativeReturn[1]
end_value <- portfolio_performance$CumulativeReturn[length(portfolio_performance$CumulativeReturn)]
years <- as.numeric(difftime(max(portfolio_performance$RebalanceDate), min(portfolio_performance$RebalanceDate), units = "days")) / 365.25

# Calculate the original CAGR
original_cagr <- (end_value / start_value)^(1 / years) - 1

# Adjust CAGR for 2% survivorship bias
adjusted_cagr <- original_cagr - 0.03

# Apply the adjusted CAGR to calculate the adjusted cumulative returns over time
adjusted_cumulative_return <- start_value * (1 + adjusted_cagr)^(as.numeric(difftime(portfolio_performance$RebalanceDate, min(portfolio_performance$RebalanceDate), units = "days")) / 365.25)

# Create a combined data frame with adjusted returns
combined_returns <- data.frame(
    Date = portfolio_performance$RebalanceDate,
    Portfolio = portfolio_performance$CumulativeReturn,
    SPX = aligned_spx$CumulativeReturn,
    BiasAdjustedPortfolio = adjusted_cumulative_return  # Add the adjusted portfolio return
)

# Turn it into long format for easier plotting
combined_returns_long <- combined_returns %>%
    pivot_longer(cols = c(Portfolio, SPX), names_to = "Type", values_to = "CumulativeReturn")

# Now plot it
ggplot(combined_returns_long, aes(x = Date, y = CumulativeReturn, color = Type)) +
    geom_line(size = 1.2) +
    labs(
        title = "Cumulative Returns: Low Volatility Portfolio vs. S&P 500",
        x = "Date",
        y = "Cumulative Return",
        color = "Portfolio Type"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom"
    )

#####################################################
##### End Low Volatility Portfolio Construction #####
#####################################################



##################################################
##### High Volatility Portfolio Construction #####
##################################################

volatilityPortfolio <- portfolioData1 %>%
    filter(!is.na(Volatility)) %>%
    select(Date, ticker, Price, Volatility, return_3m)

#Set rebalancing dates
rebalance_dates <- unique(as.Date(momentumPortfolio$Date))[seq(1, length(unique(momentumPortfolio$Date)), by = 3)]


portfolio_returns <- vector()  # To store portfolio returns

# Loop through the rebalance dates (except the last one)
for (i in 1:(length(rebalance_dates) - 1)) {
    
    # Get current and next rebalance dates
    rebalance_date <- rebalance_dates[i]
    next_rebalance_date <- rebalance_dates[i + 1]
    
    # Filtering the data for the current rebalance date
    current_data <- volatilityPortfolio %>%
        filter(Date == rebalance_date)
    
    # Select the top 20% of companies based on volatility
    top_20_percent <- current_data %>%
        arrange(desc(Volatility)) %>% 
        slice_head(n = 100) 
    
    # Get the tickers for the top 20% companies
    top_tickers <- top_20_percent$ticker
    
    # Data for the next period until the next rebalance date for these companies
    next_period_data <- valuePortfolio %>%
        filter(ticker %in% top_tickers, Date == next_rebalance_date)
    
    # Calculate the return for the selected companies in this period
    next_period_data <- next_period_data %>%
        group_by(ticker) %>%
        mutate(return_period = return_3m) %>%
        ungroup()
    
    # Calculate the portfolio return for this rebalance period
    portfolio_return <- next_period_data %>%
        filter(Date == next_rebalance_date) %>% 
        summarize(
            weighted_return = sum(return_period, na.rm = TRUE) / length(top_tickers)
        )
    
    # Append the portfolio return for this period to the portfolio_returns vector
    portfolio_returns <- c(portfolio_returns, portfolio_return$weighted_return)
}

portfolio_std <- sd(portfolio_returns)
mean_returns <- mean(portfolio_returns)
high_vol_annual_returns <- ((1+mean_returns)^4-1)
high_vol_sharpe <- ((high_vol_annual_returns - 0.0429)/portfolio_std)


# Calculate cumulative returns (for performance tracking)
cumulative_returns <- cumprod(1 + portfolio_returns)

# Data frame for tracking portfolio returns over time
portfolio_performance <- data.frame(
    RebalanceDate = rebalance_dates[-1],  # Remove the first date
    PortfolioReturn = portfolio_returns,
    CumulativeReturn = cumulative_returns
)

# Calculate the compounded annual growth rate (CAGR) for the original portfolio
start_value <- portfolio_performance$CumulativeReturn[1]
end_value <- portfolio_performance$CumulativeReturn[length(portfolio_performance$CumulativeReturn)]
years <- as.numeric(difftime(max(portfolio_performance$RebalanceDate), min(portfolio_performance$RebalanceDate), units = "days")) / 365.25

# Calculate the original CAGR
original_cagr <- (end_value / start_value)^(1 / years) - 1

# Adjust CAGR for 2% survivorship bias
adjusted_cagr <- original_cagr - 0.03

# Apply the adjusted CAGR to calculate the adjusted cumulative returns over time
adjusted_cumulative_return <- start_value * (1 + adjusted_cagr)^(as.numeric(difftime(portfolio_performance$RebalanceDate, min(portfolio_performance$RebalanceDate), units = "days")) / 365.25)

# Create a combined data frame with adjusted returns
combined_returns <- data.frame(
    Date = portfolio_performance$RebalanceDate,
    Portfolio = portfolio_performance$CumulativeReturn,
    SPX = aligned_spx$CumulativeReturn,
    BiasAdjustedPortfolio = adjusted_cumulative_return  # Add the adjusted portfolio return
)

# Turn it into long format for easier plotting
combined_returns_long <- combined_returns %>%
    pivot_longer(cols = c(Portfolio, SPX, BiasAdjustedPortfolio), names_to = "Type", values_to = "CumulativeReturn")

# Now plot it
ggplot(combined_returns_long, aes(x = Date, y = CumulativeReturn, color = Type)) +
    geom_line(size = 1.2) +
    labs(
        title = "Cumulative Returns: High Volatility Portfolio vs. S&P 500",
        x = "Date",
        y = "Cumulative Return",
        color = "Portfolio Type"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "bottom"
    )

######################################################
##### End High Volatility Portfolio Construction #####
######################################################
