---
title: "Formative Assessment 3"
author: "Mercado, C & Sinocruz, A"
output: pdf_document
---


# Problem 1

A binary communication channel carries data as one of two sets of signals denoted by 0 and 1. Owing to noise, a transmitted 0 is sometimes received as a 1, and a transmitted 1 is sometimes received as a 0. For a given channel, it can be assumed that a transmitted 0 is correctly received with probability 0.95, and a transmitted 1 is correctly received with probability 0.75. Also, 70% of all messages are transmitted as a 0. If a signal is sent, determine the probability that:

(a) a 1 was received;
(b) a 1 was transmitted given than a 1 was received.

```r
# Given Probabilities
message_transmitted_0 <- 0.70

message_transmitted_1 <- 0.30

```

```r
# Given conditional probabilities

prob_received_0 <- 0.95 #P(0 received | 0 sent)

prob_received_1 <- 0.75 #P(1 received | 1 sent)

prob_0_received_1 <- 1 - prob_received_1 #P(0 received | 1 sent)

prob_1_received_0 <- 1 - prob_received_0 #P(1 received | 0 sent)
```

### Printing the results

prob_0_received_1

```         
## [1] 0.25
```

prob_1_received_0

```         
## [1] 0.05
```

## Part (a): Probability that a 1 was received

```r
received <- (prob_1_received_0 * message_transmitted_0) + (prob_received_1 * message_transmitted_1)
```

### Output

received

```         
## [1] 0.26
```

## Part (b): Probability a 1 was transmitted given that a 1 was received

```r
transmitted <- (prob_received_1 * message_transmitted_1)/ received
```

### Output

transmitted

```         
## [1] 0.8653846
```

## Representation using Diagram

```r
barplot(
  c(received, transmitted), 
  names.arg = c("P(1 received)", "P(1 transmitted | 1 received)"),
  ylim = c(0, 1), 
  col = c("lightblue", "purple"),
  main = "Probability in a Binary Communication Channel",
  ylab = "Probability", xlab = "")
```

**Figure 1** *Barplot Representation of Probability in a Binary Communication Channel*

The probability of receiving a 1 (P(1 received)) is 26% (0.26), which means that only 26% of all received signals are 1s. This is largely a result of the occurrence that 0s are received more frequently (70%) and received correctly 95% of the time, making it less likely to receive a 1. Conversely, if a 1 is received, there is an 86.54% chance that it was originally sent as a 1 (P(1 transmitted \| received)), meaning that although noise results in some transmission errors, most received 1s were sent without error. This means that the 0s predominate the received signals due to their faster transmission rate and precision, the channel is comparatively reliable in transmitting 1s upon reception.

----------------------------------------------------------------------------------------------------------------------------

#  Problem 2

## There are three employees working at an IT company: Jane, Amy, and Ava, doing 10%, 30%, and 60% of the programming, respectively. 8% of Jane’s work, 5% of Amy’s work, and just 1% of Ava‘s work is in error. What is the overall percentage of error? If a program is found with an error, who is the most likely person to have written it?

## *First is to declare the employees, their works and percent errors. Then compute for the overall percentage error by multiplying the percentage of the employee's work to their percentage error. Then add all and multiply by 100.*

```r
employees <- c("Jane", "Amy", "Ava")
works <- c(0.10, 0.30, 0.60) # their works
errorpercent <- c(0.08, 0.05, 0.01)   # percentage of their work in error

# Compute overall error percentage
overallpercent <- sum(works * errorpercent)
cat("The overall error percentage is ", overallpercent * 100, "%.\n")

```

## *For the next question, to get the probability of who is the most likely person to have written a program with errors, we use Bayes' Theorem.*

```r
error_per_person <- works * errorpercent
person_per_error <- error_per_person / overallpercent

for (i in 1:length(employees)) {
cat("The probability that", employees[i], "made an error is", round(person_per_error[i] * 
100, 2), "%.\n")
}
```

## *Printing the most likely person to have written a program with errors,*

```r

mostlikely <- employees[which.max(person_per_error)]
cat("The most likely person to have written a program with errors is", mostlikely, ".\n")

```
