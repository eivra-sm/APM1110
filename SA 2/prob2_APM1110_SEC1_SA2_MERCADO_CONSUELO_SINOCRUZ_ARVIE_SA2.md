Github Link: **https://github.com/eivra-sm/APM1110/blob/main/SA%202/prob2_APM1110_SEC1_SA2_MERCADO_CONSUELO_SINOCRUZ_ARVIE_SA2.md**

Google Drive Link (for documentation and datasets): **https://drive.google.com/drive/folders/1OP559AJbAJL7jEcspsnIIBlGJG3iYI-0?usp=sharing**

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
