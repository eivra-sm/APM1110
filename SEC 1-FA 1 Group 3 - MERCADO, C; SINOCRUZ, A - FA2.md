
# Formative Assessment 2

Authors: 

MERCADO, CONSUELO 

SINOCRUZ, ARVIE

2025-02-07
___

**Github Link:** <https://github.com/eivra-sm/APM1110/blob/795e186e2810125d58eb6cab0e4b278585f9366c/SEC%201-FA%201%20Group%203%20-%20MERCADO%2C%20C%3B%20SINOCRUZ%2C%20A%20-%20FA2.md>
___
# Quesstion #1

Use R to illustrate the probability of getting:

(a) a head is 0.5 if a fair coin is tossed repeatedly;

(b) a red card is 0.5 if cards are drawn repeatedly with replacement from an awell-shuffled deck;

(c) an even number is 0.5 if a fair die is rolled repeatedly.

### A. A head is 0.5 if a fair coin is tossed repeatedly.

*First, we need to simulate coin tosses.*

```{r}
set.seed(123)
n <- 10000  # for the number of trials
coin_tosses <- sample(c("H", "T"), size = n, replace = TRUE, prob = c(0.5, 0.5))
```

*Then, compute for the probability of getting a head.*

```{r}
p_head <- sum(coin_tosses == "H") / n
p_head
```
    ## [1] 0.4943
*So, the proportion is 0.4923 or approximately 0.5.*

### B. A red card is 0.5 if cards are drawn repeatedly with replacement from an awell-shuffled deck.

*First, let's simulate again. In a standard deck of cards, there are 26 black cards and 26 red cards.*

```{r}
deck <- c(rep("Red", 26), rep("Black", 26)) 
draws <- sample(deck, size = n, replace = TRUE)
```

*Then let's compute for the probability.*

```{r}
p_red <- sum(draws == "Red") / n
p_red
```
    ## [1] 0.4969
*So, the proportion is 0.4996 or approximately 0.5.*

### C. An even number is 0.5 if a fair die is rolled repeatedly.

*Like what we did in the first two, let's simulate a die roll*

```{r}
die_roll <- sample(1:6, size = n, replace = TRUE)
```

*Then let's calculate the probability.*

```{r}
p_even <- sum(die_roll %% 2 == 0) / n
p_even
```
    ## [1] 0.4943
*So, the proportion is 0.4969 or approximately 0.5.*

### Summary

```{r}
sumtbl <- data.frame(
  Event = c("Coin Toss (Head)", "Drawing a Red Card", "Rolling an Even Number"),
  Theoretical_Probability = c(0.5, 0.5, 0.5),
  Simulated_Probability = c(p_head, p_red, p_even)
)
sumtbl
```
    ##                     Event Theoretical_Probability Simulated_Probability
    ##  1       Coin Toss (Head)                     0.5                0.4943
    ##  2   Drawing  a  Red Card                     0.5                0.4969
    ##  3 Rolling an Even Number                     0.5                0.4969

*From there, we can see that all the simulated probability is very close to 0.5.*
____

# Question #3

An experiment consists of rolling a die. Use R to simulate this experiment 600 times and obtain the relative frequency of each possible outcome. Hence, estimate the probability of getting each of 1, 2, 3, 4, 5, and 6.
___

### Step 1: Simulating 600 rolls of a die

```r
set.seed(123)
```
```r
die_rolls <- sample(1:6, size = 600, replace = TRUE)  
```

### Step 2: Calculating the relative frequency for each outcome
```r
relative_frequency <- table(die_rolls) / 600
```

### *Printing the relative frequencies*
```r
print(relative_frequency)
```

    ## die_rolls
    ##         1           2          3          4          5          6
    ## 0.1766667  0.17000000  0.1600000  0.1533333  0.1666667  0.1733333

### Step 3: Visualizing the relative frequencies with a bar plot
```r
par(mar = c(5, 5, 4, 4))

# Creating the barplot
barplot(relative_frequency, 
        main = "Relative Frequency of Each Die Face After 600 Rolls", 
        xlab = "Die Outcome", 
        ylab = "Relative Frequency", 
        col = "lightgreen", 
        border = "darkgreen", 
        ylim = c(0, 0.2))  

# Adding the horizontal reference line for the expected probability (1/6)
abline(h = 1/6, col = "red", lwd = 2, lty = 2)

# Adding a legend in the top-right corner for easy interpretation
legend("topright", 
       legend = "Expected Probability (1/6)", 
       col = "red", 
       lty = 2, 
       lwd = 2,
       y.intersp = 0.4)

```
![](FA2_Files/barplot.png)

**Figure 1:** *Relative Frequency Distribution of Die Rolls Over 600 Trials*

Figure 1 shows the relative frequency of each six-sided die after 600 rolls. The relative frequencies are compared to the expected probability for each outcome, shown as the red dashed line at $1/6$ or approximately about 0.1667. The observed frequencies of each face change slightly. There are slight positive values for the die faces 1, 2, and 6 and a slight negative value for die faces 3, 4, and 5. In the finite sample size, these minor deviations are within expectations for random fluctuation.

In conclusion, the distribution appears uniform and points neither in the direction of some sort of intrinsic bias in the die nor suggests any such possibility. The random variations from the predicted probability can be attributed to natural randomness; the results fall in line with the assumption of a fair die.


