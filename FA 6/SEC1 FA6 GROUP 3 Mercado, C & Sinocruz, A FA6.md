---
title: "Formative Assessment 6"
author: "Mercado, C & Sinocruz, A"
date: "2025-03-05"
output: pdf_document
---

**github link:**https://github.com/eivra-sm/APM1110/blob/18d745c9e6b0d3744b0bc4ea718035a21391bb69/FA%206/SEC1%20FA6%20GROUP%203%20Mercado%2C%20C%20%26%20Sinocruz%2C%20A%20FA6.md

# I. Geometric Distribution.

### Provide an R code for the geometric distribution. The geometric distribution is a probability distribution that models the number of trials required to achieve the first success in a sequence of Bernoulli trials, where each trial has a constant probability of success.

``` r
# 1. Set the probability of success
p <- 0.2

# 2. Generate 1000 random variables from the geometric distribution
x <- rgeom(1000, p) + 1

# 3.  Calculate basic statistics
mean_x <- mean(x)
var_x <- var(x)
sd_x <- sd(x)

# 4. Print the results with formatted output
cat("Number of trials required to achieve first success:\n")
cat(sprintf("Mean (in 2 decimal places): %.2f\n", mean_x))
cat(sprintf("Variance (in 2 decimal places): %.2f\n", var_x))
cat(sprintf("Standard deviation (in 2 decimal places): %.2f\n", sd_x))

# 5. Plot the histogram
hist(x, breaks = 30, main = "Histogram of Trials to First Success",
     xlab = "Number of Trials", col = "blue", border = "white")
```

![](images/fa6_hist.png)


# II. Hypergeometric Distribution

Consider a plant manufacturing IC chips of which 10% are expected to be defective. The chips are packed in boxes for export. Before transportation, a sample is drawn from each box. Estimate the probability that the sample contains more than 10% defectives, when:

### A sample of 10 is selected from a box of 40
```{r}
# Parameters
N_chips1 <- 40  # Total chips
Defect_1 <- round(0.1 * N_chips1)  # Defective chips (10% of 40)
n_chips1 <- 10  # Sample size
k1 <- floor(0.1 * n_chips1)  # 10% defectives threshold

# Compute probability
prob1 <- 1 - phyper(k1, Defect_1, N_chips1 - Defect_1, n_chips1)
```

```{r}
# Printing the result
cat("The Probability of a sample when 10 is selected from a box of 40:", 
    prob1, "or", sprintf("%.2f%%", prob1 * 100), "\n")
```
    
Therefore, the chances of selecting a sample containing over 10% defective chips out of a batch of 40 chips is approximately 25.6%. This indicates that even in a relatively small population, there is a high probability of finding more defects than anticipated in a small random sample.

### A sample of 10 is selected from a box of 5000

```{r}
# Parameters
N_chips2 <- 5000  # Total chips
Defect_2 <- round(0.1 * N_chips2)  # Defective chips (10% of 40)
n_chips2 <- 10  # Sample size
k2 <- floor(0.1 * n_chips2)  # 10% defectives threshold

# Computing the  probability
prob2 <- 1 - phyper(k2, Defect_2, N_chips2 - Defect_2, n_chips2)

# Printing the result
cat("The probability of a sample when 10 is selected from a box of 5000:", 
    prob2, "or", sprintf("%.2f%%", prob2 * 100), "\n")
```

Therefore, for a much bigger batch of 5000 chips, the chances of obtaining greater than 10% faulty chips in an arbitrary sample of 10 are approximately 26.4%. This value tells us that, even though the population size is large, the chance is still almost the same, supporting the concept that for large populations, the sample behavior is largely determined by the rate of defects and not the number of items.
