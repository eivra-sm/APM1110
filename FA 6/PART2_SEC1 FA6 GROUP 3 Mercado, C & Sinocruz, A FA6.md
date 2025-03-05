# Formative Assessment 6

Author: Mercado, C & Sinocruz, A

Date: 2025-03-05
---

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
cat("The Probability of a sample when 10 is selected from a box of 40:", prob1, "or", sprintf("%.2f%%", prob1 * 100), "\n")
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
cat("The Probability of a sample when 10 is selected from a box of 40:", prob1, "or", sprintf("%.2f%%", prob1 * 100), "\n")
```

Therefore, for a much bigger batch of 5000 chips, the chances of obtaining greater than 10% faulty chips in an arbitrary sample of 10 are approximately 26.4%. This value tells us that, even though the population size is large, the chance is still almost the same, supporting the concept that for large populations, the sample behavior is largely determined by the rate of defects and not the number of items.
