---
title: "Formative Assessment 8"
author: "Mercado, C"
date: "2025-04-26"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Problem 1: Signal Analysis
An analogue signal received at a detector, measured in microvolts, is normally distributed with mean $\mu = 200$ and variance $\sigma^2 = 256$.
Thus, the standard deviation $\sigma = \sqrt{256} = 16$.

### (a) What is the probability that the signal will exceed 224 µV?
```r
mu <- 200
sigma <- 16

# Probability that X exceeds 224
prob_a <- pnorm(224, mean = mu, sd = sigma, lower.tail = FALSE)
cat("The probability that the signal exceeds 224 µV is:", round(prob_a, 4), "\n")

```

### (b) What is the probability that it will be between 186 µV and 224 µV?
```r
# Probability that 186 < X < 224
prob_b <- pnorm(224, mean = mu, sd = sigma) - pnorm(186, mean = mu, sd = sigma)
cat("The probability that the signal is between 186 µV and 224 µV is:", round(prob_b, 4), "\n")


```

### (c)  What is the micro voltage below which 25% of the signals will be?

```r
# 25% = 25th percentile (quantile)
quantile_c <- qnorm(0.25, mean = mu, sd = sigma)
cat("The micro volatage is:", round(quantile_c, 4), "\n")
```

### (d) What is the probability that the signal will be less than 240 µV given that it is larger than 210 µV?
```r
# P(X < 240 | X > 210) = (P(210 < X < 240)) / (P(X > 210))
prob_less240 <- pnorm(240, mean = mu, sd = sigma) - pnorm(210, mean = mu, sd = sigma)
p_greater210 <- pnorm(210, mean = mu, sd = sigma, lower.tail = FALSE)

prob_d <- prob_less240 / p_greater210
cat("The conditional probability that the signal is less than 240 µV given it is larger than 210 µV is:", round(prob_d, 4), "\n")
```

### (e) Estimate the interquartile range. 
```r
# IQR = Q3 - Q1
firstq <- qnorm(0.25, mean = mu, sd = sigma)
thirdq <- qnorm(0.75, mean = mu, sd = sigma)
interqr <- thirdq  - firstq
cat("The interquartile range is:", round(interqr, 2), "µV\n")

```

### (f) What is the pobability that the signal will be less than 220 µV given it is larger than 210 µV?

```r
# P(X < 220 | X > 210) = (P(210 < X < 220)) / (P(X > 210))
p_less220 <- pnorm(220, mean = mu, sd = sigma) - pnorm(210, mean = mu, sd = sigma)

prob_f <- p_less220 / p_greater210
cat("The conditional probability that the signal is less than 220 µV given it is larger than 210 µV is:", round(prob_f, 4), "\n")
```
### (g) What is the probability that the signal will be greater than 220 µV given it is greater than 200 µV

```r
# P(X > 220 | X > 200) = (P(X > 220)) / (P(X > 200))
p_greater220 <- pnorm(220, mean = mu, sd = sigma, lower.tail = FALSE)
p_greater200 <- pnorm(200, mean = mu, sd = sigma, lower.tail = FALSE)

prob_g <- p_greater220 / p_greater200
cat("The conditional probability that the signal is greater than 220 µV given it is greater than 200 µV is:", round(prob_g, 4), "\n")
```

# Problem 2: System Downtime Analysis

The amount of downtime (in minutes) is normally distributed with:

- Mean $\mu = 25$
- Variance $\sigma^2 = 144$ $\Rightarrow$ Standard deviation $\sigma = \sqrt{144} = 12$

---

### (a) Obtain bounds which will include 95% of the downtime of all the customers

```r
mu2 <- 25
sigma2 <- 12

# 95% bounds correspond to the 2.5th and 97.5th percentiles
lower <- qnorm(0.025, mean = mu2, sd = sigma2)
upper <- qnorm(0.975, mean = mu2, sd = sigma2)

cat("The 95% bounds for downtime are:", round(lower, 2), "minutes to", round(upper, 2), "minutes.\n")
```

### (b) Obtain the bound above which 10% of the downtime is included
```r
# Find the 90th percentile
percentile_90 <- qnorm(0.90, mean = mu2, sd = sigma2)
cat("The bound above which 10% of the downtime is included is:", round(percentile_90, 2), "minutes.\n")
```
