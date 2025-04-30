Formative Assessment  #8

Author: Sinocruz, Arvie

Date: April 30, 2025

----
**Github Link:** 

## Number 1

An analogue signal received at a detector, measured in microvolts, is
normally distributed with mean of 200 and variance of 256.

**Given:**
Mean (μ) = 200 μV
Variance = 256 → Standard deviation (σ) = sqrt(256) = 16 μV

**(a) What is the probability that the signal will exceed 224 µV?**

```r
prob_a <- 1 - pnorm(224, mean = 200, sd = 16)
cat("The Probability that the signal will exceed 224 µV is", prob_a, "\n")
```

**(b) What is the probability that it will be between 186 and 224 µV?**
```r
prob_b <- pnorm(224, 200, 16) - pnorm(186, 200, 16)
cat("The Probability that it will be between 186 and 224 µV is ", prob_b, "\n")
```

**(c) What is the micro voltage below which 25% of the signals will be?**
```r
quartile_c <- qnorm(0.25, mean = 200, sd = 16)
cat("The micro voltage below which 25% of the signals is", quartile_c, "\n")
```

**(d) What is the probability that the signal will be less than 240 µV, given that it is larger than 210 µV?**
```r 
prob_d <- (pnorm(240, 200, 16) - pnorm(210, 200, 16)) / (1 - pnorm(210, 200, 16))
cat("The probability that the signal will be less than 240 µV is", prob_d, "\n")
```
    
**(e) Estimate the interquartile range.**
```r
quartile1 <- qnorm(0.25, 200, 16)
quartile3 <- qnorm(0.75, 200, 16)
interqr <- quartile3 - quartile1
cat("The estimated interquartile range is", interqr,  "µV\n")
```

**(f) What is the probability that the signal will be less than 220 µV, given that it is larger than 210 µV?**
```r
prob_f <- (pnorm(220, 200, 16) - pnorm(210, 200, 16)) / (1 - pnorm(210, 200, 16))
cat("The probability that the signal will be less than 220 µV is", prob_f, "\n")
```

**(g) If we know that a received signal is greater than 200 µV, what is the probability that it is in fact greater than 220 µV?**
```r
prob_g <- (1 - pnorm(220, 200, 16)) / (1 - pnorm(200, 200, 16))
cat("The conditional probability that the signal is greater than 200 µV is", prob_g, "\n")
```

## Number 2

A manufacturer of a particular type of computer system is interested in
improving its customer support services. As a first step, its marketing
department has been charged with the responsibility of summarizing the
extent of customer problems in terms of system failures. Over a period
of six months, customers were surveyed and the amount of downtime (in
minutes) due to system failures they had experienced during the previous
month was collected. The average downtime was found to be 25 minutes and
a variance of 144. If it can be assumed that downtime is normally
distributed:

**Given:**
Mean (μ) = 25 minutes

Variance = 144 → Standard deviation (σ) = sqrt(144) = 12 minutes

**(a) obtain bounds which will include 95% of the downtime of all the customers; **
```r
lower <- qnorm(0.025, 25, 12)
upper <- qnorm(0.975, 25, 12)

cat("The bound which will include 95% of the downtime of all customers: ", lower,  "minutes to", upper, "minutes \n")
```


**(b) obtain the bound above which 10% of the downtime is included.**
```r
bound_b <- qnorm(0.90, 25, 12)

cat("The bound above  which 10% of the downtime is included: ", bound_b, "minutes \n")
```


