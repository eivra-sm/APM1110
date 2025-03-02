---
title: "Formative Assessment 4"
author: "Mercado, Consuelo"
date: "2025-03-01"
output:
  pdf_document: default
  html_document: default
---

# Item 5

A geospatial analysis system has four sensors supplying images. The percentage of images supplied by each sensor and the percentage of images relevant to a query are shown in the following table:

```r
library(knitr)
data <- data.frame(
  Sensor = 1:4,
  Images_Supplied = c(15, 20, 25, 40),
  Relevant_Images = c(50, 60, 80, 85)
)

kable(data, caption = "Sensor Image Supply and Relevance")
```


### To solve for the Weighted Average, we use this formula:

```r
overall_percentage <- sum((data$Images_Supplied / 100) * data$Relevant_Images)

overall_percentage
```

### What is the overall percentage of relevant images?

The overall percentage of relevant images is **`r overall_percentage`%**.


# Item 6

A fair coin is tossed twice. Define the following events:
- Let \( E_1 \) be the event that both tosses have the same outcome: \( E_1 = \{HH, TT\} \).
- Let \( E_2 \) be the event that the first toss is a head: \( E_2 = \{HH, HT\} \).
- Let \( E_3 \) be the event that the second toss is a head: \( E_3 = \{TH, HH\} \}.

Show that \( E_1, E_2, \) and \( E_3 \) are pairwise independent but not mutually independent.

### First, let us declare the sample space, sets, probabilities and intersection propobabilities. 

```r
# sample space
events <- c("HH", "HT", "TH", "TT")
prob <- rep(1/4, 4)

# sets
E1 <- c("HH", "TT")
E2 <- c("HH", "HT")
E3 <- c("HH", "TH")

# Compute for the probabilities
P1 <- sum(events %in% E1) / length(events)
P2 <- sum(events %in% E2) / length(events)
P3 <- sum(events %in% E3) / length(events)

# Intersection probabilities
P12 <- sum(events %in% intersect(E1, E2)) / length(events)
P13 <- sum(events %in% intersect(E1, E3)) / length(events)
P23 <- sum(events %in% intersect(E2, E3)) / length(events)
P123 <- sum(events %in% intersect(intersect(E1, E2), E3)) / length(events)
```

### Then, let's check if it is pairwise independent

```r
# expected probabilities
pwise_indep <- list(
  E12 = P1 * P2 == P12,
  E13 = P1 * P3 == P13,
  E23 = P2 * P3 == P23
)
pwise_indep
```

All are true so the events are pairwise independent. 

### Lastly, let's check for its mutual independence,

```r
mutual_indep <- P1 * P2 * P3 == P123
mutual_indep
```

The result is false, therefore, the events are not mutually independent.

### Conclusion

From there, we proved that the events \( E_1, E_2, \) and \( E_3 \) are **pairwise independent but not mutually independent**.



