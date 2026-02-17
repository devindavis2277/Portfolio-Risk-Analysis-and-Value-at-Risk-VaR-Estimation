library(dplyr)
library(haven)
library(purrr)
library(PerformanceAnalytics)
library(ggplot2)

## read in data
dret <- read_sas("C:/Users/Davis/OneDrive/Desktop/dret1519.sas7bdat") %>%
  filter(SHRCD %in% c(10, 11)) %>%
  mutate(
    DATE = as.Date(DATE),
    PRC = abs(PRC)
  )

ff <- read.csv("C:/Users/Davis/OneDrive/Desktop/ffd2619.CSV") %>%
  mutate(
    year = floor(date/10000),
    month = floor((date - year*10000)/100),
    day = date - year*10000 - month*100,
    RM = (RMRF + RF)/100,
    Rf = RF/100
  ) %>%
  select(year, month, day, RM, RMRF, Rf)

# extract stocks
ibm <- dret %>%
  filter(PERMNO == 12490) %>%
  transmute(DATE, IBM_RET = RET, IBM_PRC = PRC)

msft <- dret %>%
  filter(PERMNO == 10107) %>%
  transmute(DATE, MSFT_RET = RET, MSFT_PRC = PRC)

gm <- dret %>%
  filter(PERMNO == 12369) %>%
  transmute(DATE, GM_RET = RET, GM_PRC = PRC)

# merge stocks 
stocks <- reduce(list(ibm, msft, gm), full_join, by = "DATE") %>%
  arrange(DATE)

# portfolio composition (as of Oct 12, 2018) 
shares <- data.frame(
  Ticker = c("IBM", "MSFT_long", "MSFT_short", "GM_short"),
  Shares = c(1.212e6, 3.444e6, -1.872e6, -0.568e6)
)

# prices on Oct 12, 2018
prices <- stocks %>%
  filter(DATE == as.Date("2018-10-12")) %>%
  select(DATE, IBM_PRC, MSFT_PRC, GM_PRC)

# market values
values <- data.frame(
  Asset = c("IBM", "MSFT_net", "GM", "Cash"),
  Position = c(
    shares$Shares[1] * prices$IBM_PRC,
    (shares$Shares[2] + shares$Shares[3]) * prices$MSFT_PRC,
    shares$Shares[4] * prices$GM_PRC,
    5.380e6
  )
)

# total and weights
total_value <- sum(values$Position)
values <- values %>% mutate(Weight = Position / total_value)

cat("---- Portfolio Composition (Oct 12, 2018) ----\n")
print(values)
cat("\n✅ Total Portfolio Value: $", round(total_value, 2), "\n")
cat("✅ Sum of portfolio weights: ", round(sum(values$Weight), 6), "\n\n")

# formatted summary
values_check <- values %>%
  mutate(
    Position_mil = round(Position / 1e6, 3),
    Weight_pct = round(Weight * 100, 2)
  ) %>%
  select(Asset, Position_mil, Weight_pct)
print(values_check)

# ortfolio daily returns 
stocks <- stocks %>%
  mutate(
    Portfolio_RET = (values$Weight[1] * IBM_RET) +
      (values$Weight[2] * MSFT_RET) +
      (values$Weight[3] * GM_RET)
  ) %>%
  filter(DATE <= as.Date("2018-12-18"))

stocks$Portfolio_RET <- as.numeric(stocks$Portfolio_RET)

# --- VaR Calculations (1-day, 5%) ---
VaR_varcov <- - total_value * VaR(stocks$Portfolio_RET, p = 0.95, method = "gaussian")
VaR_hist   <- - total_value * VaR(stocks$Portfolio_RET, p = 0.95, method = "historical")

cat("\n---- Value at Risk (5% level, next trading day) ----\n")
cat("Variance–Covariance (Gaussian) VaR: $", round(VaR_varcov, 2), "\n")
cat("Historical Simulation VaR: $", round(VaR_hist, 2), "\n")

# --- VaR Visualization ---
VaR_cutoff_hist <- quantile(stocks$Portfolio_RET, probs = 0.05, na.rm = TRUE)
VaR_cutoff_norm <- qnorm(0.05, mean = mean(stocks$Portfolio_RET, na.rm = TRUE),
                         sd = sd(stocks$Portfolio_RET, na.rm = TRUE))

plot_data <- data.frame(RET = stocks$Portfolio_RET)

ggplot(plot_data, aes(x = RET)) +
  geom_histogram(binwidth = 0.002, fill = "lightblue", color = "black") +
  geom_vline(xintercept = VaR_cutoff_hist, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = VaR_cutoff_norm, color = "darkgreen", linetype = "dotted", size = 1) +
  annotate("text", x = VaR_cutoff_hist, y = 30,
           label = paste0("Hist 5% VaR = ", round(VaR_cutoff_hist, 4)),
           color = "red", hjust = -0.1) +
  annotate("text", x = VaR_cutoff_norm, y = 40,
           label = paste0("Norm 5% VaR = ", round(VaR_cutoff_norm, 4)),
           color = "darkgreen", hjust = -0.1) +
  labs(
    title = "Distribution of Portfolio Daily Returns",
    subtitle = "Red dashed = Historical 5% VaR | Green dotted = Normal (Parametric) 5% VaR",
    x = "Portfolio Daily Return",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)

