---
title: "Formative Assessment 8"
author: "Mercado, C"
date: "2025-04-26"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Problem 1: Signal Analysis
An analogue signal received at a detector, measured in microvolts, is normally distributed with mean $\mu = 200$ and variance $\sigma^2 = 256$.
Thus, the standard deviation $\sigma = \sqrt{256} = 16$.

### (a) Probability that the signal exceeds 224 µV
```r
mu <- 200
sigma <- 16

# Probability that X > 224
p_a <- pnorm(224, mean = mu, sd = sigma, lower.tail = FALSE)
p_a

```
### (b) Probability that the signal is between 186 µV and 224 µV
```r
# Probability that 186 < X < 224
p_b <- pnorm(224, mean = mu, sd = sigma) - pnorm(186, mean = mu, sd = sigma)
p_b

```
### (c) Micro voltage below which 25% of the signals will be

```r
# 25th percentile (quantile)
q_c <- qnorm(0.25, mean = mu, sd = sigma)
q_c

```
### (d) Probability that the signal is less than 240 µV given it is larger than 210 µV
```r
# P(X < 240 | X > 210) = (P(210 < X < 240)) / (P(X > 210))
p_210_240 <- pnorm(240, mean = mu, sd = sigma) - pnorm(210, mean = mu, sd = sigma)
p_greater_210 <- pnorm(210, mean = mu, sd = sigma, lower.tail = FALSE)

p_d <- p_210_240 / p_greater_210
p_d
```
### (e) Estimate the interquartile range (IQR)
```r
# IQR = Q3 - Q1
q1 <- qnorm(0.25, mean = mu, sd = sigma)
q3 <- qnorm(0.75, mean = mu, sd = sigma)
iqr <- q3 - q1
iqr

```

### (f) Probability that the signal is less than 220 µV given it is larger than 210 µV
```r
# P(X < 220 | X > 210) = (P(210 < X < 220)) / (P(X > 210))
p_210_220 <- pnorm(220, mean = mu, sd = sigma) - pnorm(210, mean = mu, sd = sigma)

p_f <- p_210_220 / p_greater_210
p_f
```
### (g) Probability that the signal is greater than 220 µV given it is greater than 200 µV

```r
# P(X > 220 | X > 200) = (P(X > 220)) / (P(X > 200))
p_greater_220 <- pnorm(220, mean = mu, sd = sigma, lower.tail = FALSE)
p_greater_200 <- pnorm(200, mean = mu, sd = sigma, lower.tail = FALSE)

p_g <- p_greater_220 / p_greater_200
p_g
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
lower_bound <- qnorm(0.025, mean = mu2, sd = sigma2)
upper_bound <- qnorm(0.975, mean = mu2, sd = sigma2)

c(lower_bound, upper_bound)
```

### (b) Obtain the bound above which 10% of the downtime is included
```r
# Find the 90th percentile
bound_90 <- qnorm(0.90, mean = mu2, sd = sigma2)
bound_90
```
