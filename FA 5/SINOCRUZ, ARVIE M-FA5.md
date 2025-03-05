# Formative Assessment 5

Author: SINOCRUZ, ARVIE

Date:  2025-03-04

---

# Question 6

An email message can travel through one of three server routes. The
percentage of errors in each of the servers and the percentage of
messages that travel through each route are shown in the following
table. Assume that the servers are independent.

      
                   Percentage of Messages        Percentage of Errors
    server 1                40                             1
    server 2                25                             2
    server 3                35                            1.5


``` {r}
    servers <- c("Server 1", "Server 2", "Server 3")
    prob_perc_mess <- c(0.4, 0.25, 0.35)
    prob_perc_err <- c(0.01, 0.02, 0.015)
```

### (a) What is the probability of receiving an email containing an error?

```{r}
  prob_error <- sum(prob_perc_err * prob_perc_mess)
  cat("The probability of receiving an email containing an error is ", prob_error)
```

        
    ## [1] The probability of receiving an email containing an error is  0.01425


### (b) What is the probability that a message will arrive without error?

``` {r}
  prob_perc_noerr <-  1 - prob_error
  cat("The probability of receiving an email without an error is ", prob_perc_noerr)
```

         
    ## [1] The probability of receiving an email without an error is  0.98575


### (c) If a message arrives without error, what is the probability that it
    was sent through server 1?

``` {r}
  prob_no_perc_error <- 1 - prob_perc_err
  prob_perc_noerror <- (prob_perc_mess * prob_no_perc_error) / prob_perc_noerr
  prob_s1_perc_noerror <- prob_perc_noerror[1]
  
  print(paste("The probability that a message was sent through Server 1 given no error is ", prob_s1_perc_noerror ))
```

         
    ## [1] The probability that a message was sent through Server 1 given no error is  0.40172457519655


# Question 9

A software company surveyed managers to determine the probability that
they would buy a new graphics package that includes three-dimensional
graphics. About 20% of office managers were certain that they would not
buy the package, 70% claimed that they would buy, and the others were
undecided. Of those who said that they would not buy the package, only
10% said that they were interested in upgrading their computer hardware.
Of those interested in buying the graphics package, 40% were also
interested in upgrading their computer hardware, Of those undecided, 20%
were interested in upgrading their computer hardware.

Let A denote the intention of not buying, B the intention of buying, C
the undecided, and G the intention of upgrading the computer hardware.

```{r}
  Prob_A <- 0.20 # Would not buy the package
  Prob_B <- 0.70 # Buying
  Prob_C <- 0.10 # Undecided
  
  Prob_G_A <- 0.10 # P(G | A)
  Prob_G_B <- 0.40 # P(G | B)
  Prob_G_C <- 0.20 # P(G | C)
  
```

### (a) Calculate the probability that a manager chosen at random will not upgrade the computer ardware (P(Gbar)).
  
```{r}
  Prob_G <- (Prob_G_A * Prob_A) + (Prob_G_B * Prob_B) + (Prob_G_C * Prob_C)
  Prob_Gbar <- 1 - Prob_G
  cat("P(Ḡ): ", Prob_Gbar, "\n") # Printing the result
```

    ## [1] P(Ḡ):  0.68


### (b) Prob_A <- 0.20(b) Explain what is meant by the posterior probability of *B* given *G*,  P(B\|G) 

Using Bayes’ Theorem:

```{r}
 Prob_B_given_G <- (Prob_G_B * Prob_B) / Prob_G
 cat("P(B|G):", Prob_B_given_G, "\n") # Printing the result
```

    ## [1] P(B|G):  0.875
    
### (c)  Construct a tree diagram and use it to calculate the following probabilities: P(G), P(B\|G), P(B\|Gbar), P(C\|G), P(Cbar\|Gbar)

```{r}
  # Computing P(B|Gbar)
  Prob_Gbar_A <- 1 - Prob_G_A  
  Prob_Gbar_B <- 1 - Prob_G_B  
  Prob_Gbar_C <- 1 - Prob_G_C 
  
  Prob_Gbar <- (Prob_Gbar_A * Prob_A) + (Prob_Gbar_B * Prob_B) + (Prob_Gbar_C * Prob_C)
  
  Prob_B_given_Gbar <- (Prob_Gbar_B * Prob_B) / Prob_Gbar
  
  # Computing  P(C|G)
  Prob_C_given_G <- (Prob_G_C * Prob_C) / Prob_G
  
  # Computing P(Cbar|Gbar) (not undecided given not upgrading)
  Prob_Cbar_given_Gbar <- (Prob_Gbar_A * Prob_A + Prob_Gbar_B * Prob_B) / Prob_Gbar
```

```{r}
  # Printing the results
  cat("P(G):", Prob_G, "\n")
```

    ## [1] P(G): 0.32
    
```{r}
  # Printing the results
  cat("P(B|G):", Prob_B_given_G, "\n")
```

    ## [1] P(B|G): 0.875 

```{r}
  # Printing the results
  cat("P(B|Ḡ):", Prob_B_given_Gbar, "\n")
```

    ## [1] P(B|Ḡ): 0.6176471 

```{r}
  # Printing the results
  cat("P(C|G):", Prob_C_given_G, "\n")
```

    ## [1] P(C|G): 0.0625 

```{r}
  # Printing the results
  cat("P(Cbar|Gbar):", Prob_Cbar_given_Gbar, "\n")
```

    ## [1] P(Cbar|Gbar): 0.8823529 
    
## Tree Diagram

``` {r}
library(data.tree)
```

    ## Warning: package 'webshot2' was built under R version 4.3.3

``` {r}
library(DiagrammeR)
```

    ## Warning: package 'data.tree' was built under R version 4.3.3
    
```{r}
tree <- Node$new("Managers")

grViz("
digraph DecisionTree {
  
  # Define node styles with colors
  node [shape = oval, style = filled, fontname = Helvetica];

  # Root Node
  Manager [label='Manager', fillcolor=yellow];

  # First Level: Decision Making
  NotBuying [label='Not Buying (0.20)', fillcolor=lightblue];
  Buying [label='Buying (0.70)', fillcolor=lightblue];
  Undecided [label='Undecided (0.10)', fillcolor=lightblue];

  # Second Level: Upgrading Hardware
  Upgrade [label='Interested in upgrading (0.40)', fillcolor=lightcoral];

  # Tree Structure
  Manager -> NotBuying;
  Manager -> Buying;
  Manager -> Undecided;

  Buying -> Upgrade;
}
")
```
![](FA5/tree diagram.png)

# Question 13

A malicious spyware can infect a computer system through the Internet or through email. The spyware comes through the Internet 70% of the time and 30% of the time, it gets in through email. If it enters via the Internet, the anti-virus detector will detect it with probability 0.6, and via email, it is detected with probability 0.8.

```{r}
# Given probabilities
Prob_spywareI <- 0.7  # Probability of Internet source
Prob_spywareE <- 0.3  # Probability of Email source
Prob_detect_given_I <- 0.6  # Detection probability via Internet
Prob_detect_given_E <- 0.8  # Detection probability via Email
```

### (a) What is the probability that this spyware infects the system?

```{r}
# Probability that spyware is NOT detected
Prob_not_detect_given_I <- 1 - Prob_detect_given_I
Prob_not_detect_given_E <- 1 - Prob_detect_given_E

Prob_infection <- (Prob_spywareI * Prob_not_detect_given_I) + (Prob_spywareE * Prob_not_detect_given_E)

cat("The probability of infection:", Prob_infection, "\n")
```

    ## [1] The probability of infection: 0.34 
    
    
### (b) If the spyware is detected, what is the probability that it came through the Internet?

```{r}
# Computing P(Detect) (Total probability of detection)
Prob_detect <- (Prob_detect_given_I * Prob_spywareI) + (Prob_detect_given_E * Prob_spywareE)

# Computing P(Internet | Detect) using Bayes' theorem
Prob_I_given_D <- (Prob_detect_given_I * Prob_spywareI) / Prob_detect

cat("The probability that detected spyware came from the Internet:", Prob_I_given_D, "\n")
```
    ##  [1] The probability that detected spyware came from the Iinternet: 0.6363636


