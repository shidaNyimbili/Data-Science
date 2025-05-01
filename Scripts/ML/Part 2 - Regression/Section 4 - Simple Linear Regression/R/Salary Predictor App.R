library(shiny)

# Define your trained model (replace with your actual model if needed)
model <- lm(Salary ~ YearsExperience, data = training_set)

# Define UI
ui <- fluidPage(
  titlePanel("Salary Prediction Based on Years of Experience"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("experience", "Enter Years of Experience:", value = 1, min = 0, step = 0.1),
      actionButton("predict", "Predict Salary")
    ),
    
    mainPanel(
      h3("Predicted Salary:"),
      verbatimTextOutput("result"),
      br(),
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  predicted_salary <- eventReactive(input$predict, {
    new_data <- data.frame(YearsExperience = input$experience)
    predict(model, newdata = new_data)
  })
  
  output$result <- renderText({
    if (input$predict > 0) {
      paste0("Estimated Salary: ZMW", round(predicted_salary(), 2))
    }
  })
  
  output$plot <- renderPlot({
    plot(training_set$YearsExperience, training_set$Salary,
         xlab = "Years of Experience", ylab = "Salary",
         main = "Salary vs Years of Experience",
         col = "steelblue", pch = 19)
    abline(model, col = "red", lwd = 2)
    points(input$experience, predicted_salary(), col = "green", pch = 19, cex = 2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)


##shiny::runApp() #Save the script as App.R and run it