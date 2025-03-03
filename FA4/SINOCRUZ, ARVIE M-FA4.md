# Formative Assessment 4

Author: SINOCRUZ, A.

Date: 2025-03-03

**Github Link:** https://github.com/eivra-sm/APM1110/blob/main/FA4/SINOCRUZ%2C%20ARVIE%20M-FA4.md

# Question 5

A geospatial analysis system has four sensors supplying images. The
percentage of images supplied by each sensor and the percentage of
images relevant to a query are shown in the following table.

```         
                      sensor   % img supplied   % relevant img
                        1             15              50
                        2             20              60
                        3             25              80
                        4             40              85
```

What is the overall percentage of relevant images?

### Calculating the overall percentage of relevant images

``` r
sensor <- c(1,2,3,4)
perc_img_supplied = c(15, 20, 25, 40)
perc_relevant_img = c(50, 60, 80, 85)

overall_percent <- sum(perc_img_supplied * perc_relevant_img) / 100
```

### Printing the result or the answer

``` r
print(paste("The overall percentage of relevant images is ", round(overall_percent, 2), "%"))
```

    ## [1] "The overall percentage of relevant images is 73.5 %"

---
# Question 6

A fair coin is tossed twice. Let E, be the event that both tosses have
the same outcome, that is, E, = (HH, TT). Let E, be the event that the
first toss is a head, that is, E2 = (HH, HT). Let Ez be the event that
the second toss is a head, that is, Ez = (TH, HH). Show that E1, E2, and
E3 are pairwise independent but not mutually independent.

``` r
# Define the sample space
sample_space <- c("HH", "HT", "TH", "TT")

# Define events
E1 <- c("HH", "TT")  # Both tosses are the same
E2 <- c("HH", "HT")  # First toss is heads
E3 <- c("HH", "TH")  # Second toss is heads

# Function to calculate probability
probability <- function(event) length(event) / length(sample_space)

# Compute probabilities
Prob_E1 <- probability(E1)
Prob_E2 <- probability(E2)
Prob_E3 <- probability(E3)

# Compute pairwise intersections
Prob_E1_E2 <- length(intersect(E1, E2)) / length(sample_space)
Prob_E1_E3 <- length(intersect(E1, E3)) / length(sample_space)
Prob_E2_E3 <- length(intersect(E2, E3)) / length(sample_space)

# Compute triple intersection
Prob_E1_E2_E3 <- length(intersect(intersect(E1, E2), E3)) / length(sample_space)

# Check pairwise and mutual independence
pairwise_indep <- c(
  Prob_E1_E2 == Prob_E1 * Prob_E2,
  Prob_E1_E3 == Prob_E1 * Prob_E3,
  Prob_E2_E3 == Prob_E2 * Prob_E3
)

mutual_indep <- Prob_E1_E2_E3 == Prob_E1 * Prob_E2 * Prob_E3
```

### Printing the results or the answer

``` r
cat("Pairwise Independent:", all(pairwise_indep), "\n")
```

    ## Pairwise Independent: TRUE 

``` r
cat("Mutually Independent:", mutual_indep, "\n")
```

    ## Mutually Independent: FALSE

Thus, the events $E_1$, $E_2$, and $E_3$ are **pairwise independent**
but **not mutually independent**, meaning any two events do not affect
each other, but all three together are dependent.
