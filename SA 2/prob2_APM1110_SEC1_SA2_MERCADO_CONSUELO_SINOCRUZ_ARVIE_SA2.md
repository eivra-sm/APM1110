# Summative Assessment 2

Authors:
Mercado, Consuelo  B.

Sinocruz, Arvie  M.

Date: MAy 17, 2025

Github Link: **https://github.com/eivra-sm/APM1110/blob/main/SA%202/prob2_APM1110_SEC1_SA2_MERCADO_CONSUELO_SINOCRUZ_ARVIE_SA2.md**

Google Drive Link (for documentation and datasets): **https://drive.google.com/drive/folders/1OP559AJbAJL7jEcspsnIIBlGJG3iYI-0?usp=sharing**

# Part 1
*Find out which probability distribution function best fits Bitcoin’s returns for trading data every minute, from January 1, 2012 to April 15, 2025, for Bitcoin quoted in United States dollars or the BTC/USD pair.*

### Load Libraries

```{r}
library(readr)
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(MASS)
library(goftest)
library(extraDistr)
library(grid)
```

### Read CSV and Prepare Data
```{r}
# Read the dataset
df <- read_csv("btcusd_1-min_data.csv")

# Convert timestamp
df$Timestamp <- as.POSIXct(df$Timestamp, origin = "1970-01-01", tz = "UTC")

# Sort and compute log returns
df <- df %>% arrange(Timestamp)
df$log_return <- c(NA, diff(log(df$Close)))

# Clean log returns
returns <- df$log_return
returns <- returns[is.finite(returns)]  # Remove NA, NaN, Inf, -Inf
returns <- as.numeric(returns)

# Ensure we have enough data
if (length(returns) < 2) stop("Not enough valid return values.")
```

### Sample Data Overview

```{r}
cat("First 10 entries from 2012:\n")
print(head(filter(df, format(Timestamp, "%Y") == "2012"), 10))

cat("\nFirst 10 entries from 2025:\n")
print(head(filter(df, format(Timestamp, "%Y") == "2025"), 10))
```

### Histogram with KDE

```{r}
ggplot(data.frame(returns), aes(x = returns)) +
  geom_histogram(aes(y = after_stat(density)), bins = 200, fill = "skyblue", color = "black") +
  geom_density(color = "red") +
  ggtitle("Bitcoin Minute-by-Minute Returns (2012–2025)") +
  xlab("Log Return") + ylab("Density") +
  theme_minimal()
```

### Fit Distributions

```{r}
# Normal
fit_norm <- fitdist(returns, "norm")
```
```{r}
# Laplace
# Define the negative log-likelihood function for the Laplace distribution
neg_log_lik <- function(params, data) {
  mu <- params[1]
  b <- abs(params[2])  # ensure scale > 0
  -sum(dlaplace(data, mu, b, log = TRUE))
}

# Initial guesses based on data
init_params <- c(mean(returns), sd(returns))

# Fit the Laplace distribution
fit_laplace <- optim(
  par = init_params,
  fn = neg_log_lik,
  data = returns,
  method = "L-BFGS-B",
  lower = c(-Inf, 1e-6),  # scale > 0
  upper = c(Inf, Inf)
)

# Extract estimated parameters
fitted_location <- fit_laplace$par[1]
fitted_scale <- abs(fit_laplace$par[2])

# Output the estimated parameters
cat("Estimated Laplace parameters:\n")
cat("Location:", fitted_location, "\n")
cat("Scale:", fitted_scale, "\n")
```

# Student's t
```{r}
fit_t <- suppressWarnings(
  fitdistr(returns, densfun = "t", start = list(m = mean(returns), s = sd(returns), df = 5))
)
```

### Q-Q Plots

```{r}
# Normal Q-Q
qqnorm(returns, main = "Q-Q Plot: Normal")
qqline(returns, col = "blue")

# Laplace Q-Q
qqplot(
  qlaplace(ppoints(length(returns)), location = fit_laplace$estimate[1], scale = fit_laplace$estimate[2]),
  returns,
  main = "Q-Q Plot: Laplace",
  xlab = "Theoretical",
  ylab = "Sample"
)
abline(0, 1, col = "blue")

# Student's t Q-Q
qt_vals <- qt(ppoints(length(returns)), df = fit_t$estimate["df"])
theoretical_t <- qt_vals * fit_t$estimate["s"] + fit_t$estimate["m"]
qqplot(theoretical_t, returns,
       main = "Q-Q Plot: Student's t", xlab = "Theoretical", ylab = "Sample")
abline(0, 1, col = "blue")
```

### Goodness-of-Fit Tests

```{r}
ks_norm <- ks.test(returns, "pnorm", mean = fit_norm$estimate[1], sd = fit_norm$estimate[2])
ks_laplace <- ks.test(returns, "plaplace", location = fit_laplace$estimate[1], scale = fit_laplace$estimate[2])
ks_t <- ks.test(returns, function(x) pt((x - fit_t$estimate["m"]) / fit_t$estimate["s"], df = fit_t$estimate["df"]))

cat("\n--- Goodness of Fit Results ---\n")
cat("\nNormal Distribution:\n")
print(fit_norm)
print(ks_norm)

cat("\nLaplace Distribution:\n")
print(fit_laplace)
print(ks_laplace)

cat("\nStudent's t Distribution:\n")
print(fit_t)
print(ks_t)
```
## Interpretation/s:
Minute-by-minute returns of Bitcoin exhibit heavy tails and non-normality. Additionally, Normal, Laplace, and Student's t distribution fits indicate that the heavy-tailed t distribution usually provides the best fit to the data. However, Q-Q plots and goodness-of-fit tests suggest that there are more extreme returns than a normal model would suggest. In general, Bitcoin returns are high in variability and with large swings happening often, which emphasizes the necessity of employing heavy-tailed models when assessing risk.


# Part 2
*Test using Shapiro-Wilk normality test the Ethereum returns for trading data every five minutes, from August 7, 2015 to April 15, 2025*
Note: Due to data availability per websites, we have two separate dataset for 2015 to 2020 and 2020 to 2025.

```r
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
```

### Dataset

```r

# First dataset (2015 to 2020)
df1 <- read_csv("ETHUSDT_2015_to_2020.csv", show_col_types = FALSE) %>%
  rename(date = Date, close = Close)

# Second dataset (2020 to 2025)
df2 <- read_csv("ETHUSDT_2020_to_2025.csv", show_col_types = FALSE) %>%
  rename(date = date, close = close)

# Merge and remove NAs
combined_df <- bind_rows(df1, df2) %>%
  filter(!is.na(date)) %>%   
  arrange(date)

# Log returns
combined_df <- combined_df %>%
  mutate(log_return = log(close / lag(close))) %>%
  filter(!is.na(log_return))

set.seed(42)
sample_size <- min(5000, nrow(combined_df))
sample_returns <- sample(combined_df$log_return, sample_size)
```

### Shapiro-Wilk Normality Test
```r
shapiro_test <- shapiro.test(sample_returns)


cat("📊 Shapiro-Wilk Normality Test on ETH/USDT 2015-2025 5-min Returns\n")
cat(sprintf("Test Statistic: %.5f\n", shapiro_test$statistic))
cat(sprintf("P-value: %.5e\n", shapiro_test$p.value))


if (shapiro_test$p.value > 0.05) {
  cat("✅ Returns appear normally distributed (fail to reject H₀).\n")
} else {
  cat("❌ Returns are not normally distributed (reject H₀).\n")
}


```

### Histogram

```r

# Plot histogram of log returns
ggplot(data.frame(log_return = sample_returns), aes(x = log_return)) +
  geom_histogram(binwidth = 0.0005, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Sampled Log Returns (ETH/USDT 2015-2025)",
    x = "Log Return",
    y = "Frequency"
  ) +
  theme_minimal()
```

### Results Interpretation

The normality of Ethereum 5-minute log returns from August 7, 2015 to April 15, 2025 was assessed using two statistical tests:

**Shapiro-Wilk Test** (on a random sample of 5,000 returns):

-   Test Statistic: 0.53694

-   P-value: 1.69021e-78

-   **Interpretation**: The very low p-value indicates a strong rejection of the null hypothesis. This means the sampled Ethereum returns wass  not normally distributed.
