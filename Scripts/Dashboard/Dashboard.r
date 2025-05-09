library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(googlesheets4)

# ====== CONFIGURE GOOGLE SHEET ======
sheet_url <- "https://docs.google.com/spreadsheets/d/1xf02UnHJhS8tmHiRugMPj963eZ80n6D85eHDXJlBiMg/edit?usp=sharing"


# ====== UI ======
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "moh_logo.png", height = "40px", style = "margin-right: 10px;"),
      "Zambia Cholera Dashboard"
    )
  ),
  dashboardSidebar(
    width = 250,
    selectInput("province", "Select Province:", choices = NULL),
    tags$hr(),
    tags$div(
      style = "padding: 10px;",
      HTML("<h4>For more information:</h4>
           <a href='https://www.moh.gov.zm/' target='_blank'>Ministry of Health</a><br>
           <a href='https://znphi.co.zm/' target='_blank'>ZNPHI</a><br><br>
           <b>Terms:</b><br>
           1. © 2023 MoH-ZNPHI<br>
           2. Attribution required for reuse.")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box.bg-yellow { background-color: #f4b400 !important; color: white !important; }
        .small-box.bg-green { background-color: #0f9d58 !important; color: white !important; }
        .small-box.bg-orange { background-color: #ff6f00 !important; color: white !important; }
        .content-wrapper, .main-footer { background-color: #f7f7f7; }
        .box-content { height: 400px; overflow: auto; } /* Added height and overflow for both boxes */
      "))
    ),
    fluidRow(
      box(title = "Last Data Update", width = 12, status = "info", solidHeader = TRUE, htmlOutput("update_time"))
    ),
    fluidRow(
      valueBoxOutput("newCases"),
      valueBoxOutput("admissions"),
      valueBoxOutput("deaths"),
      valueBoxOutput("discharged"),
      valueBoxOutput("facilityDeaths"),
      valueBoxOutput("communityDeaths")
    ),
    fluidRow(
      box(title = "National Cholera Update (Last 24hrs)", width = 8, DTOutput("table_data"), class = "box-content"),
      box(title = "Cumulative Statistics: Since Oct 2023", width = 4,
          valueBox(23563, "Total Cases", icon = icon("users"), color = "orange"),
          valueBox(23534, "Discharged", icon = icon("walking"), color = "green"),
          valueBox(741, "Total Deaths", icon = icon("bed"), color = "red"),
          valueBox(358, "Facility Deaths", icon = icon("hospital"), color = "yellow"),
          valueBox(390, "Community Deaths", icon = icon("users"), color = "yellow"),
          class = "box-content"
      )
    ),
    fluidRow(
      box(title = "Facility CFR", width = 4, plotlyOutput("gauge1")),
      box(title = "Community CFR", width = 4, plotlyOutput("gauge2")),
      box(title = "National CFR", width = 4, plotlyOutput("gauge3"))
    )
  )
)

# ====== SERVER ======
server <- function(input, output, session) {
  
  # Reactive poll: refresh Google Sheet data every 5 minutes
  cholera_data <- reactivePoll(
    intervalMillis = 300000,
    session = session,
    checkFunc = function() googlesheets4::sheet_properties(sheet_url)$modifiedTime,
    valueFunc = function() read_sheet(sheet_url)
  )
  
  # Populate province filter dynamically
  observe({
    data <- cholera_data()
    updateSelectInput(session, "province", choices = c("All", unique(data$Province)))
  })
  
  # Filtered data
  filtered_data <- reactive({
    data <- cholera_data()
    if (input$province == "All") return(data)
    data[data$Province == input$province, ]
  })
  
  # Last update time
  output$update_time <- renderUI({
    date_val <- max(cholera_data()$UpdatedDate, na.rm = TRUE)
    tagList(
      tags$h4("Data last updated:"),
      tags$p(format(as.POSIXct(date_val), "%B %d, %Y %H:%M:%S"),
             style = "color:blue; font-weight: bold;")
    )
  })
  
  # Table
  output$table_data <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5))
  })
  
  # Value Boxes
  output$newCases <- renderValueBox({
    valueBox(sum(filtered_data()$NewCases24hrs, na.rm = TRUE), "New Cases", icon = icon("chart-line"), color = "yellow")
  })
  
  output$admissions <- renderValueBox({
    valueBox(sum(filtered_data()$admission24hrs, na.rm = TRUE), "Admissions", icon = icon("procedures"), color = "orange")
  })
  
  output$deaths <- renderValueBox({
    valueBox(sum(filtered_data()$Deaths24hrs, na.rm = TRUE), "Deaths", icon = icon("bed"), color = "red")
  })
  
  output$discharged <- renderValueBox({
    valueBox(sum(filtered_data()$discharge24hrs, na.rm = TRUE), "Discharged", icon = icon("walking"), color = "green")
  })
  
  output$facilityDeaths <- renderValueBox({
    valueBox(sum(filtered_data()$facdeaths24hrs, na.rm = TRUE), "Facility Deaths", icon = icon("hospital"), color = "yellow")
  })
  
  output$communityDeaths <- renderValueBox({
    valueBox(sum(filtered_data()$comdeaths24hrs, na.rm = TRUE), "Community Deaths", icon = icon("users"), color = "yellow")
  })
  
  # Gauges (using calculated values)
  output$gauge1 <- renderPlotly({
    # Calculating Facility CFR based on the data
    facility_cfr <- sum(filtered_data()$facdeaths24hrs, na.rm = TRUE) / sum(filtered_data()$admission24hrs, na.rm = TRUE) * 100
    plot_ly(type = "indicator", mode = "gauge+number", value = facility_cfr,
            gauge = list(axis = list(range = list(NULL, 10))),
            domain = list(x = c(0, 1), y = c(0, 1)))
  })
  
  output$gauge2 <- renderPlotly({
    # Calculating Community CFR based on the data
    community_cfr <- sum(filtered_data()$comdeaths24hrs, na.rm = TRUE) / sum(filtered_data()$admission24hrs, na.rm = TRUE) * 100
    plot_ly(type = "indicator", mode = "gauge+number", value = community_cfr,
            gauge = list(axis = list(range = list(NULL, 10))),
            domain = list(x = c(0, 1), y = c(0, 1)))
  })
  
  output$gauge3 <- renderPlotly({
    # National CFR (use cumulative data)
    national_cfr <- sum(filtered_data()$cumfacdeath, na.rm = TRUE) / sum(filtered_data()$cumcases, na.rm = TRUE) * 100
    plot_ly(type = "indicator", mode = "gauge+number", value = national_cfr,
            gauge = list(axis = list(range = list(NULL, 10))),
            domain = list(x = c(0, 1), y = c(0, 1)))
  })
}

# Run the application
shinyApp(ui, server)
