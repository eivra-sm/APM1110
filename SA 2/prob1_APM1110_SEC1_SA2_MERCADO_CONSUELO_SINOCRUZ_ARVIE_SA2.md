
Github Link: **https://github.com/eivra-sm/APM1110/blob/main/SA%202/prob1_APM1110_SEC1_SA2_MERCADO_CONSUELO_SINOCRUZ_ARVIE_SA2.md**

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
