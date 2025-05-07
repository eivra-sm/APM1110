# Formative Assessment 9

Author: 
  - Borromeo, Elisha Sophia B.
  - Mayo, Zyan Lynn C.
  - Mercado, Consuelo B.
  - Sinocruz, Arvie M.
  - Tagaytay, Gabriel L.
    
Date: April 06, 2025

---



#  Normal Distribution on Campus

Normal Distribution is a concept in statistics that is used to describe how data are symmetrically distributed around the mean (Chen, 2024). Its primary focus is to identify the behavior of data, which was the most common and what is the unusual. There are a lot of real-life applications that study one variable such as test scores, weight, money and etc. For our group presentation, we choose allowance as the variable to study. To assess our ability to apply the concept of normal distribution on campus, the group surveyed 50 IAS students from different years and programs about the amount of their weekly allowance.
```{r}
baon <- c(
  1500, 1500, 1500, 1000, 1500, 1000, 3000, 1500, 4200, 2500,
  1250, 800, 800, 2000, 1500, 1000, 1000, 2000, 2000, 3000,
  3000, 1500, 1500, 1500, 1000, 2000, 1400, 2500, 500, 2000,
  1000, 2500, 1700, 1500, 1000, 2000, 2500, 1000, 1000, 1000,
  1000, 1000, 1200, 1800, 1000, 1000, 1000, 3000, 2500, 1500
)

```

# 1. Dataset

```{r}

baon_df <- data.frame(Student = paste("Student", 1:length(baon)),
                      Weekly_Baon = baon)


knitr::kable(baon_df, caption = "Weekly Allowance of Students (in PHP)")
```
### Explanation

-   500 is the lowest allowance, while 4200 is the highest. The amount spent on transportation and the number of days of classes should also be considered.

-   The average weekly allowance of those 50 students is 1633 pesos.

-   The most common weekly allowance of students is 1500 pesos  



# 2. Organization and Data Analysis

```{r}

mean_baon <- mean(baon)
sd_baon <- sd(baon)

mean_baon
sd_baon

```

# Frequency Table

```{r}

breaks <- seq(0, 5000, by = 500)
baon_cut <- cut(baon, breaks = breaks)


uniqlvl <- unique(baon_cut)

labels <- c("(0, 500]", "(500, 1000]", "(1000, 1500]", "(1500, 2000]", 
            "(2000, 2500]", "(2500, 3000]", "(3000, 3500]", "(3500, 4000]", 
            "(4000, 4500]", "(4500, 5000]")

if (length(uniqlvl) <= length(labels)) {
  baon_cut <- factor(baon_cut, levels = uniqlvl, labels = labels[1:length(uniqlvl)])
}

freqcy_table <- table(baon_cut)

freq_df <- as.data.frame(freqcy_table)
colnames(freq_df) <- c("Baon Range", "Frequency")


knitr::kable(freq_df, caption = "Frequency Distribution of Student Weekly Allowance")

```

# Bar Chart

```{r}
barplot(freqcy_table, 
        col = "lightcoral", 
        main = "Weekly Baon Frequency Distribution", 
        ylab = "Number of Students",
        border = "black",
        las = 2)  # Rotate the x-axis labels for better readability
```

\begin{center}
   {\large \textbf{Allowance Ranges (PHP)}}
\end{center}

# Normal Distribution Plot

```{r}

hist(baon, breaks = seq(0, 5000, by = 500), 
     col = "skyblue", 
     probability = TRUE,
     main = "Histogram with Normal Curve", 
     xlab = "Allowance (PHP)", 
     border = "black")


curve(dnorm(x, mean = mean_baon, sd = sd_baon), 
      col = "red", 
      lwd = 2, 
      add = TRUE)


abline(v = mean_baon, col = "darkgreen",lty = 2)


axis(1, at = seq(0, 5000, by = 500), 
     labels = format(seq(0, 5000, by = 500), 
     scientific = FALSE), las = 1)
```

# Percent of Data

```{r}
within_1sd <- sum(baon >= (mean_baon - sd_baon) & 
                    baon <= (mean_baon + sd_baon)) / length(baon) * 100

within_2sd <- sum(baon >= (mean_baon - 2*sd_baon) & 
                    baon <= (mean_baon + 2*sd_baon)) / length(baon) * 100

within_3sd <- sum(baon >= (mean_baon - 3*sd_baon) & 
                    baon <= (mean_baon + 3*sd_baon)) / length(baon) * 100

within_1sd
within_2sd
within_3sd
```

### Interpretation

The data or the histogram of allowance (baon) of students with an overlaid normal curve indicates that the distribution is **positively skewed**, with the majority of students having lower allowance (baon) values and a few having much higher values. As a result, the distribution is asymmetric, since the tail on the right (towards higher values) is longer. Therefore, there appear to be **outliers**—students who possess a significantly higher allowance (baon) of more than 4000 PHP, which sets them apart from the rest of the data. The distribution indicates that while most students receive a relatively average amount of allowance, some students with significantly higher allowances move the mean to the right. Moreover, the information indicates that **74% of the students lie within one standard deviation**, and **98% lie within two or three**, suggesting that the data is overall bunched up around the mean with some high-value outliers. This data proves helpful in the identification of socioeconomic inequities among students and can inform financial assistance efforts, meal price policy, and policies more generally oriented toward equity on campus.


# Pie Chart Category (Percentage of students with low, medium and high allowance per week)

```{r}
baon_group <- cut(baon, breaks = c(0, 1200, 2000, 5000),
                  labels = c("Low", "Medium", "High"))


group_counts <- table(baon_group)


percentages <- round(group_counts / sum(group_counts) * 100, 2)


labels <- paste(names(group_counts), "\n", group_counts, "(", percentages, "%)", sep = "")


pie(group_counts, labels = labels, 
    col = c("skyblue", "orange", "lightgreen"),
    main = "Student Allowance Category Distribution")
```

### Interpretation

The pie chart indicates the distribution of student allowance (baon) into three levels: **Low (38%), Medium (42%), and High (20%)**. A majority of the students are distributed in the Low and Medium groups, which signifies that higher allowances are not so frequent. The distribution is **asymmetric**, as the values are irregularly distributed with a significantly lower number in the High group, which may be considered an **outlier** in context. This **left-skewed distribution** implies that most of the students receive lower to moderate levels of allowance (baon). These types of data would be useful to school administrators to design affordable menu choices, plan the allocation of funds, and have an appreciation for the student population's economic background.

# Real-life implications

* **Socioeconomic Diversity**  
  -   The PHP 500 to PHP 4200 range of weekly allowance indicates a great disparity in economic capacity among campus students. The fact that it is very broad indicates that while there are those who can very well afford daily necessities and an occasional splurge, there are also those who could hardly make ends meet as simple things such as food or fare. For individuals with lower allowances, money can be a constraining factor in their everyday choices — e.g., missing meals, not engaging in paid school activities, or reducing on basic supplies — which in the long run can influence their performance at school, general health, and the extent of their participation in school activities.  
  
* **Potential for Inequality in Access**  
  -   Lower weekly alowance students might struggle to pay for basic necessities like food, fare, or school materials. These economic pressures can create additional stress and cause students to miss meals, stay home from school because they don't have fare, or fall behind in class because they lack resources. With time, these conditions can translate into lower academic achievement and decreased engagement compared to students who have greater financial room.
  
* **Influence on Social Interactions**  
  -   Students whose weekly allowance is significantly higher or lower than their peers are under muted social pressure, specifically by peer groups like lunch break, field trips, or group expenditures. Students who are on the lower side will not be inclined to attend some activities or limit their choices according to their budget, and students who are higher level may subconsciously exert influence. These differences can, in the long term, influence students' sense of belonging or comfort level within peer groups, and this can have implications for their social interactions and inclusion on campus.  
  

# Recomendations based on the data  

* **Economic Insight for Campus Services**  
  -   Campus enterprises and service establishments — like food stalls, printing services, and event organizers — can utilize this information to better gauge students' purchasing power. With the knowledge that the majority of students are given a small weekly allowance, these vendors can modify their pricing schemes, making their services and products more affordable and within reach. This method not only enables students to budget better but can also enhance customer loyalty and sales, which makes it a win-win for students and campus entrepreneurs alike.

* **Budgeting and Financial Awareness Needs**  
  -   With approximately 80% of the students categorized as "Low" and "Medium" allowance, it is evidently indispensable to encourage financial literacy among students. Educating the students on how to control their allowance however meager can assist them in spending more wisely, minimize wasteful spending, and even form early saving habits. Equipping them with these real-life skills not only benefits them with their everyday budgeting, but also prepares them to meet more significant financial responsibilities in the future.




## References:
Chen, J. (2024, March 13). Normal Distribution. Investopedia. https://www.investopedia.com/terms/n/normaldistribution.asp
