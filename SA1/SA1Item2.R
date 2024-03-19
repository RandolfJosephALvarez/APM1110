library(shiny)
library(ggplot2)

# UI for univariate random variable analysis
ui_univariate <- fluidPage(
  titlePanel("Univariate Random Variable Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("values_input", "Enter values (comma-separated):"),
      textInput("probabilities_input", "Enter probabilities (comma-separated):"),
      actionButton("calculate_univariate", "Calculate")
    ),
    mainPanel(
      textOutput("mean_output"),
      textOutput("variance_output"),
      plotOutput("pdf_plot"),
      plotOutput("cdf_plot")
    )
  )
)

# Server logic for univariate random variable analysis
server_univariate <- function(input, output) {
  observeEvent(input$calculate_univariate, {
    values <- unlist(strsplit(input$values_input, ","))
    probabilities <- unlist(strsplit(input$probabilities_input, ","))
    
    if (length(values) != length(probabilities)) {
      output$mean_output <- renderText("Error: Values and probabilities must have the same length.")
      output$variance_output <- renderText("")
      output$pdf_plot <- renderPlot({})
      output$cdf_plot <- renderPlot({})
    } else {
      values <- as.numeric(values)
      probabilities <- as.numeric(probabilities)
      
      if (any(probabilities < 0) || any(probabilities > 1) || sum(probabilities) != 1) {
        output$mean_output <- renderText("Error: Probabilities must be in the interval [0, 1] and sum to one.")
        output$variance_output <- renderText("")
        output$pdf_plot <- renderPlot({})
        output$cdf_plot <- renderPlot({})
      } else {
        mean_value <- sum(values * probabilities)
        variance_value <- sum(((values - mean_value) ^ 2) * probabilities)
        
        output$mean_output <- renderText(paste("Mean:", mean_value))
        output$variance_output <- renderText(paste("Variance:", variance_value))
        
        df <- data.frame(Value = values, Probability = probabilities)
        p <- ggplot(df, aes(x = Value, y = Probability)) +
          geom_col(fill = "skyblue") +
          labs(title = "Probability Density Function (pdf)") +
          theme_minimal()
        output$pdf_plot <- renderPlot(p)
        
        df$cdf <- cumsum(df$Probability)
        p <- ggplot(df, aes(x = Value, y = cdf)) +
          geom_step(color = "blue") +
          labs(title = "Cumulative Distribution Function (cdf)") +
          theme_minimal()
        output$cdf_plot <- renderPlot(p)
      }
    }
  })
}

# UI for bivariate random variable analysis
ui_bivariate <- fluidPage(
  titlePanel("Bivariate Random Variable Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("values_x_input", "Enter values for X (comma-separated):"),
      textInput("values_y_input", "Enter values for Y (comma-separated):"),
      textInput("joint_probabilities_input", "Enter joint probabilities (comma-separated):"),
      actionButton("calculate_bivariate", "Calculate")
    ),
    mainPanel(
      textOutput("marginal_output"),
      textOutput("conditional_output"),
      plotOutput("marginal_pdf_plot"),
      plotOutput("conditional_pdf_plot")
    )
  )
)

# Server logic for bivariate random variable analysis
server_bivariate <- function(input, output) {
  observeEvent(input$calculate_bivariate, {
    values_x <- unlist(strsplit(input$values_x_input, ","))
    values_y <- unlist(strsplit(input$values_y_input, ","))
    joint_probabilities <- unlist(strsplit(input$joint_probabilities_input, ","))
    
    if (length(values_x) * length(values_y) != length(joint_probabilities)) {
      output$marginal_output <- renderText("Error: Number of values for X times number of values for Y must match the joint probabilities.")
      output$conditional_output <- renderText("")
      output$marginal_pdf_plot <- renderPlot({})
      output$conditional_pdf_plot <- renderPlot({})
    } else {
      values_x <- as.numeric(values_x)
      values_y <- as.numeric(values_y)
      joint_probabilities <- as.numeric(joint_probabilities)
      
      if (any(joint_probabilities < 0) || any(joint_probabilities > 1) || sum(joint_probabilities) != 1) {
        output$marginal_output <- renderText("Error: Joint probabilities must be in the interval [0, 1] and sum to one.")
        output$conditional_output <- renderText("")
        output$marginal_pdf_plot <- renderPlot({})
        output$conditional_pdf_plot <- renderPlot({})
      } else {
        marginal_x <- colSums(matrix(joint_probabilities, nrow = length(values_x), byrow = TRUE))
        marginal_y <- colSums(matrix(joint_probabilities, ncol = length(values_y)))
        
        conditional_x <- matrix(joint_probabilities, nrow = length(values_x), byrow = TRUE) / marginal_x
        conditional_y <- t(matrix(joint_probabilities, ncol = length(values_y))) / marginal_y
        
        output$marginal_output <- renderText(paste("Marginal Distribution for X:", marginal_x))
        output$conditional_output <- renderText(paste("Conditional Distribution for Y given X:", conditional_x))
        
        df_marginal_x <- data.frame(Value = values_x, Probability = marginal_x)
        p <- ggplot(df_marginal_x, aes(x = Value, y = Probability)) +
          geom_col(fill = "lightgreen") +
          labs(title = "Marginal Probability Density Function for X") +
          theme_minimal()
        output$marginal_pdf_plot <- renderPlot(p)
        
        df_conditional_x <- data.frame(Conditional_Y = values_y, Value_X = rep(values_x, each = length(values_y)), 
                                       Probability = c(conditional_x))
        p <- ggplot(df_conditional_x, aes(x = Conditional_Y, y = Probability, fill = factor(Value_X))) +
          geom_col(position = "dodge") +
          labs(title = "Conditional Probability Density Function for Y given X") +
          theme_minimal()
        output$conditional_pdf_plot <- renderPlot(p)
      }
    }
  })
}

# Combine UI and server into a Shiny app
shinyApp(ui = ui_univariate, server = server_univariate)
shinyApp(ui = ui_bivariate, server = server_bivariate)