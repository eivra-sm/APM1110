# Summative Assessment 1

Author: "Mercado, C & Sinocruz, A

Date: "2025-03-15"


# Problem 1

A company has three factories producing a product. Factory 1 produces $x_1$ of the product, factory 2 produces $x_2$, and factory 3 produces $x_3$, where: 
$\sum_{i=1}^{3} x_i = 1$.

The defective rates of the products are $y_1, y_2,$ and $y_3$ respectively, where: 
$\sum_{i=1}^{3} y_i = 0.12$.

Write a program that takes user input for $x_i$ and $y_i$ and calculates the probability that a randomly selected product is defective.

### Constraints

-   $0.10 \leq x_i \leq 0.40$ or $10\% \leq x_i \leq 40\%$, and $\sum_{i=1}^{3} x_i = 1$.
-   $0.01 \leq y_i \leq 0.05$ or $1\% \leq y_i \leq 5\%$, and $\sum_{i=1}^{3} y_i = 0.12$.

*Note that the "x" is the factory's production and "y" is the defect rates.*


```{r}
library(shiny)
library(shinythemes)
```


```{r}
# Function to validate inputs
validate_inputs <- function(x, y) {
    if (sum(x) != 1) {
        return("Error: The sum of x1, x2, and x3 must be 1.")
    }
    if (any(x < 0.10 | x > 0.40)) {
        return("Error: Each x value must be between 0.10 and 0.40.")
    }
    if (sum(y) != 0.12) {
        return("Error: The sum of y1, y2, and y3 must be 0.12.")
    }
    if (any(y < 0.01 | y > 0.05)) {
        return("Error: Each y value must be between 0.01 and 0.05.")
    }
    return(TRUE)
}

# Function to calculate probability of selecting a defective product
compute_defective_probability <- function(x, y) {
    probability_defective <- sum(x * y)
    return(probability_defective)
}

# UI Layout
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    tags$head(
        tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&family=Roboto:wght@300;400;700&display=swap');
            body { font-family: 'Poppins', 'Roboto', sans-serif; }
            .well { background-color: #f8f9fa; padding: 15px; border-radius: 10px; }
            h4 { font-weight: 600; }
            .calculator-container { background-color: #e3f2fd; padding: 20px; border-radius: 15px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); }
        "))
    ),
    
    # Title Panel
    titlePanel(tags$h2("Defective Product Probability Calculator", 
                       style = "color: #2C3E50; text-align: center; font-weight: bold;")),
    
    div(class = "calculator-container",
        sidebarLayout(
            sidebarPanel(
                h4("Factory Production Percentages"),
                div(class = "well",
                    numericInput("x1", "x1 (Factory Production %)", value = 0.30, min = 0.10, max = 0.40, step = 0.01),
                    numericInput("x2", "x2 (Factory Production %)", value = 0.30, min = 0.10, max = 0.40, step = 0.01),
                    numericInput("x3", "x3 (Factory Production %)", value = 0.40, min = 0.10, max = 0.40, step = 0.01)
                ),
                
                h4("Defect Rates"),
                div(class = "well",
                    numericInput("y1", "y1 (Defect Rate)", value = 0.04, min = 0.01, max = 0.05, step = 0.01),
                    numericInput("y2", "y2 (Defect Rate)", value = 0.05, min = 0.01, max = 0.05, step = 0.01),
                    numericInput("y3", "y3 (Defect Rate)", value = 0.03, min = 0.01, max = 0.05, step = 0.01)
                ),
                
                actionButton("compute", "Calculate Probability", class = "btn btn-primary")
            ),
            
            mainPanel(
                h4("Results"),
                verbatimTextOutput("result"),
                tags$hr(),
                div(style = "color: red; font-weight: bold;", textOutput("validation_message"))
            )
        )
    )
)

# Server Logic
server <- function(input, output) {
    observeEvent(input$compute, {
        x <- c(input$x1, input$x2, input$x3)
        y <- c(input$y1, input$y2, input$y3)
        
        validation_result <- validate_inputs(x, y)
        
        if (validation_result == TRUE) {
            output$validation_message <- renderText("")
            defective_prob <- compute_defective_probability(x, y)
            output$result <- renderText({
                sprintf("The probability of selecting a defective product is: %.4f", defective_prob)
            })
        } else {
            output$result <- renderText("")
            output$validation_message <- renderText(validation_result)
        }
    })
}
# Run the App
shinyApp(ui = ui, server = server)
```
# Problem 2
```
With your own computing experience, develop a front end to R that allows the user

-   to input the values of a univariate discrete random variable and the associated probabilities to obtain the mean and variance, and

-   to input the values of a bivariate discrete random variable and the associated probabilities and to obtain the marginal and conditional distributions.

Your program should provide a facility to calculate each distribution's mean and variance and plot the pdf and cdf. In each program, do validity checks that the probabilities are in the interval [0, 1], and that they sum to one.

### Code:

```{r}
library(shiny)
library(bslib)

# design for the front end
modern_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#007bff",
  secondary = "#6c757d",
  success = "#28a745",
  base_font = font_google("Poppins"),
  heading_font = font_google("Roboto")
)

# validity check
validity_check <- function(prob) {
  if (any(prob < 0 | prob > 1)) return("Error: Probabilities must be in the interval [0,1].")
  if (abs(sum(prob) - 1) > 1e-6) return("Error: The sum of the probabilities should be 1.")
  return(NULL)
}

# user interface or UI
ui <- fluidPage(
  theme = modern_theme,
  tags$head(
    tags$style(HTML("
      .custom-sidebar {
        background-color: #fdd5df;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }
      .custom-sidebar input, .custom-sidebar textarea {
        margin-bottom: 10px;
      }
    "))
  ),
  tags$div(
    style = "text-align: center; margin-top: 20px;",
    titlePanel("Discrete Probability Distributions")
  ),
  
  tabsetPanel(
    tabPanel("Univariate",
      sidebarLayout(
        sidebarPanel(
          class = "custom-sidebar",
          textInput("univalue", "Enter values (comma-separated):", "1,2,3"),
          textInput("probability_uni", "Enter probabilities (comma-separated):", "0.2,0.5,0.3"),
          actionButton("solve_uni", "Compute")
        ),
        mainPanel(
          verbatimTextOutput("results_uni"),
          plotOutput("pdf_uni"),
          plotOutput("cdf_uni")
        )
      )
    ),
    
    tabPanel("Bivariate Distribution",
      sidebarLayout(
        sidebarPanel(
          class = "custom-sidebar",
          textInput("xvalues", "Enter X values (comma-separated):", "1,2"),
          textInput("yvalues", "Enter Y values (comma-separated):", "1,2"),
          textAreaInput("bivariate_proba", "Enter probabilities matrix (comma-separated per row):", "0.2,0.3\n0.3,0.2"),
          actionButton("solve_bi", "Compute")
        ),
        mainPanel(
          verbatimTextOutput("results_bi"),
          plotOutput("pdf_xplot"),
          plotOutput("pdf_yplot"),
          plotOutput("cdf_xplot"),
          plotOutput("cdf_yplot")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$solve_uni, {
    if (is.null(input$univalue) || is.null(input$probability_uni) || 
        input$univalue == "" || input$probability_uni == "") {
      output$results_uni <- renderText("Error: Please enter valid values and probabilities.")
      return()
    }
    
    values <- unlist(strsplit(trimws(input$univalue), ","))
    probabilities <- unlist(strsplit(trimws(input$probability_uni), ","))
    
    values <- suppressWarnings(as.numeric(values))
    probabilities <- suppressWarnings(as.numeric(probabilities))
    
    if (any(is.na(values)) || any(is.na(probabilities))) {
      output$results_uni <- renderText("Error: Ensure inputs are numeric and correctly formatted.")
      return()
    }
    
    error <- validity_check(probabilities)
    if (!is.null(error)) {
      output$results_uni <- renderText(error)
      return()
    }
    
    mean_x <- sum(values * probabilities)
    uni_var_x <- sum((values - mean_x)^2 * probabilities)
    cdf <- cumsum(probabilities)
    
    output$results_uni <- renderText({
      paste0("Mean: ", round(mean_x, 4), "\nVariance: ", round(uni_var_x, 4))
    })
    
    output$pdf_uni <- renderPlot({
      barplot(probabilities, names.arg = values, main = "Probability Density Function (PDF)",
              col = "lightblue", ylim = c(0, max(probabilities) * 1.2))
    })
    
    output$cdf_uni <- renderPlot({
      plot(values, cdf, type = "s", col = "blue", main = "Cumulative Distribution Function (CDF)",
           xlab = "Values", ylab = "Cumulative Probability")
    })
  })
  
  observeEvent(input$solve_bi, {
    xvalues <- as.numeric(unlist(strsplit(trimws(input$xvalues), ",")))
    yvalues <- as.numeric(unlist(strsplit(trimws(input$yvalues), ",")))
    
    matrix_p_lines <- unlist(strsplit(input$bivariate_proba, "\n"))
    matrix_p_clean <- lapply(matrix_p_lines, function(line) as.numeric(unlist(strsplit(trimws(line), ","))))
    matrix_p <- do.call(rbind, matrix_p_clean)
    
    # Debugging prints
    print("X Values: "); print(xvalues)
    print("Y Values: "); print(yvalues)
    print("Probability Matrix: "); print(matrix_p)
    print("Sum of Probabilities: "); print(sum(matrix_p))

    # Ensure dimensions match
    if (nrow(matrix_p) != length(xvalues) || ncol(matrix_p) != length(yvalues)) {
      output$results_bi <- renderText("Error: Matrix dimensions must match the number of X and Y values.")
      return()
    }

    error <- validity_check(as.vector(matrix_p))
    if (!is.null(error)) {
      output$results_bi <- renderText(error)
      return()
    }
    
    marginprob_of_x <- rowSums(matrix_p)
    marginprob_of_y <- colSums(matrix_p)
    cdf_x <- cumsum(marginprob_of_x)
    cdf_y <- cumsum(marginprob_of_y)
    
    mean_x <- sum(xvalues * marginprob_of_x)
    mean_y <- sum(yvalues * marginprob_of_y)
    bi_var_x <- sum((xvalues - mean_x)^2 * marginprob_of_x)
    bi_var_y <- sum((yvalues - mean_y)^2 * marginprob_of_y)
    
    output$results_bi <- renderText({
      paste0("Mean X: ", round(mean_x, 4), "\nVariance X: ", round(bi_var_x, 4),
             "\nMean Y: ", round(mean_y, 4), "\nVariance Y: ", round(bi_var_y, 4),
             "\nMarginal P(X): ", paste(marginprob_of_x, collapse = ", "),
             "\nMarginal P(Y): ", paste(marginprob_of_y, collapse = ", "))
    })
    
    output$pdf_xplot <- renderPlot({
      barplot(marginprob_of_x, names.arg = xvalues, col = "pink", main = "Marginal P(X)", ylim = c(0, max(marginprob_of_x) * 1.2))
    })
    
    output$pdf_yplot <- renderPlot({
      barplot(marginprob_of_y, names.arg = yvalues, col = "lightgreen", main = "Marginal P(Y)", ylim = c(0, max(marginprob_of_y) * 1.2))
    })
    
    output$cdf_xplot <- renderPlot({
      plot(xvalues, cdf_x, type = "s", col = "red", main = "Cumulative P(X)",
           xlab = "X Values", ylab = "Cumulative Probability")
    })
    
    output$cdf_yplot <- renderPlot({
      plot(yvalues, cdf_y, type = "s", col = "purple", main = "Cumulative P(Y)",
           xlab = "Y Values", ylab = "Cumulative Probability")
    })
  })
}

shinyApp(ui, server)

```

# Problem 3

By generating 10,000 searches in R, carry out a simulation experiment for a search engine going through a list of sites for a given key phrase, until the key phrase is found. You should allow your program to input the probability p that any site will contain the key phrase.

-   Plot the simulated pdf and calculate its mean and variance, and

### Code:

```{r}
library(ggplot2)


set.seed(123)
p <- 0.6
trials <- 10000

# Simulate search process: X follows a Geometric(p) distribution
searches <- rgeom(trials, prob = p) + 1 

# Compute mean and variance
sample_mean <- mean(searches)
variance_simulated <- var(searches)

# Print results
cat("Simulated Mean:", sample_mean, "\n")
cat("Simulated Variance:", variance_simulated, "\n\n")
```

-   Obtain the simulated conditional distribution of searches when three searches have been carried out without success. Calculate its mean and variance, and satisfy yourself that they are equivalent to the simulated distribution of the complete set.

### Code

```{r}
# Plot the simulated probability density function (PDF)
data_frame <- as.data.frame(table(searches) / trials)
colnames(data_frame) <- c("Searches", "Probability")
data_frame$Searches <- as.numeric(as.character(data_frame$Searches))


ggplot(data_frame, aes(x = Searches, y = Probability)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Simulated probability density function (PDF)") +
  xlab("# of searches to first cuccess") +
  ylab("Probability")

# Conditional Distribution when X > 3
filtered_searches <- searches[searches > 3] - 3  
mean_conditional <- mean(filtered_searches)
var_conditional <- var(filtered_searches)

# Markov Memoryless Property Verification
p_x4_given_x3 <- mean(searches == 4) / mean(searches > 3)
p_x5_given_x3 <- mean(searches == 5) / mean(searches > 3)
p_x1 <- mean(searches == 1)
p_x2 <- mean(searches == 2)



cat("Conditional Mean (X > 3):", mean_conditional, "\n")
cat("Conditional Variance (X > 3):", var_conditional, "\n\n")

```

As test data, assume each site has a **60% chance** of containing the key phrase. To verify that the **Markov memoryless property** holds, we obtain estimates of:

**(a)** $P(X = 4 | X > 3)$ and $P(X = 1)$

**(b)** $P(X = 5 | X > 3)$ and $P(X = 2)$where $X$ is the number of searches required to find the first success.

### Code

```{r}
cat("P(X = 4 | X > 3):", p_x4_given_x3, "\n")
cat("P(X = 5 | X > 3):", p_x5_given_x3, "\n")
cat("P(X = 1):", p_x1, "\n")
cat("P(X = 2):", p_x2, "\n")

```
