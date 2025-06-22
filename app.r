library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(janitor)
library(plotly)
library(forecast)

ui <- dashboardPage(
  dashboardHeader(title = "🔥 Million Row-ed Bank Data Analysis 🔥"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Sales Prediction", tabName = "prediction", icon = icon("chart-line")),
      menuItem("Interactive", tabName = "interactive", icon = icon("bolt"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .box {border-radius: 15px;}
      .content {background: #f9f9f9;}
    "))),
    tabItems(
      # Dashboard tab with your main charts
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "Average Transaction Value Daily by Domain (Faceted)",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("avgTransDomain")
                )
              ),
              fluidRow(
                box(title = "Average Transaction Value by City",
                    status = "success", solidHeader = TRUE, width = 6,
                    plotlyOutput("avgTransCity")
                ),
                box(title = "Domain Priority List for Promotion (by Activity)",
                    status = "warning", solidHeader = TRUE, width = 6,
                    tableOutput("domainPriority")
                )
              ),
              fluidRow(
                box(title = "Average Transaction Count by City",
                    status = "info", solidHeader = TRUE, width = 6,
                    plotlyOutput("avgTransCountCity")
                ),
                box(title = "Total Sales Over Time",
                    status = "danger", solidHeader = TRUE, width = 6,
                    plotlyOutput("totalSalesTime")
                )
              ),
              fluidRow(
                box(title = "Top 3 Cities by Total Sales",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("topCities")
                )
              )
      ),
      
      # Sales prediction tab with forecast
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "Sales Value Time Series & 30-Day Forecast",
                    status = "success", solidHeader = TRUE, width = 12,
                    plotOutput("salesForecastPlot")
                )
              )
      ),
      
      # New Interactive tab with your 3 new sections
      tabItem(tabName = "interactive",
              fluidRow(
                box(title = "Random Data Row (Updates every 5 sec)",
                    status = "primary", solidHeader = TRUE, width = 4,
                    uiOutput("randomRowInfo")
                ),
                box(title = "Sale Predicting with Random Inputs (Updates every 5 sec)",
                    status = "warning", solidHeader = TRUE, width = 4,
                    uiOutput("randomPrediction")
                ),
                box(title = "User Input Sale Prediction",
                    status = "success", solidHeader = TRUE, width = 4,
                    numericInput("userTransactionCount", "Enter Transaction Count:", value = 5, min = 1),
                    selectInput("userDomain", "Select Domain:",
                                choices = c("E-commerce", "Banking", "Travel", "Education")),
                    selectInput("userCity", "Select City:",
                                choices = c("New York", "San Francisco", "Chicago", "Boston", "Seattle")),
                    actionButton("predictBtn", "Predict Sale Value"),
                    br(), br(),
                    textOutput("userPrediction")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Sample test data (replace with your actual data)
  set.seed(123)
  sales_data <- data.frame(
    date = seq.Date(Sys.Date() - 364, Sys.Date(), by = "day"),
    domain = sample(c("E-commerce", "Banking", "Travel", "Education"), 365, replace = TRUE),
    city = sample(c("New York", "San Francisco", "Chicago", "Boston", "Seattle"), 365, replace = TRUE),
    transaction_value = runif(365, 50, 1000),
    transaction_count = sample(1:10, 365, replace = TRUE)
  )
  
  sales_data <- clean_names(sales_data)
  
  # Convert domain and city to factors (important for regression)
  sales_data$domain <- factor(sales_data$domain)
  sales_data$city <- factor(sales_data$city)
  
  # Train a simple linear regression model
  model <- lm(transaction_value ~ transaction_count + domain + city, data = sales_data)
  
  # 1. Average Transaction Value Daily by Domain — Faceted
  avg_val_domain <- sales_data %>%
    group_by(date, domain) %>%
    summarise(avg_value = mean(transaction_value), .groups = "drop")
  
  output$avgTransDomain <- renderPlotly({
    p <- ggplot(avg_val_domain, aes(x = date, y = avg_value, color = domain)) +
      geom_line(size = 1) +
      facet_wrap(~domain, scales = "free_y") +
      labs(title = "Avg Transaction Value per Day by Domain (Faceted)",
           x = "Date", y = "Avg Transaction Value ($)") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_color_brewer(palette = "Set2")
    ggplotly(p)
  })
  
  # 2. Average Transaction Value by City
  avg_val_city <- sales_data %>%
    group_by(city) %>%
    summarise(avg_value = mean(transaction_value), .groups = "drop") %>%
    arrange(desc(avg_value))
  
  output$avgTransCity <- renderPlotly({
    p <- ggplot(avg_val_city, aes(x = reorder(city, avg_value), y = avg_value, fill = city)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Avg Transaction Value by City",
           x = "City", y = "Avg Transaction Value ($)") +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal()
    ggplotly(p)
  })
  
  # 3. Domain Priority List based on total transaction count (highest first)
  domain_priority <- sales_data %>%
    group_by(domain) %>%
    summarise(total_transactions = sum(transaction_count), .groups = "drop") %>%
    arrange(desc(total_transactions)) %>%
    mutate(Priority = row_number())
  
  output$domainPriority <- renderTable({
    domain_priority %>%
      select(Priority, domain, total_transactions) %>%
      rename(Domain = domain, `Total Transactions` = total_transactions)
  })
  
  # 4. Average Transaction Count by City
  avg_count_city <- sales_data %>%
    group_by(city) %>%
    summarise(avg_trans_count = mean(transaction_count), .groups = "drop") %>%
    arrange(desc(avg_trans_count))
  
  output$avgTransCountCity <- renderPlotly({
    p <- ggplot(avg_count_city, aes(x = reorder(city, avg_trans_count), y = avg_trans_count, fill = city)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Avg Transaction Count by City",
           x = "City", y = "Avg Transaction Count") +
      scale_fill_brewer(palette = "Paired") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Extra: Total Sales Over Time
  total_sales_time <- sales_data %>%
    group_by(date) %>%
    summarise(total_sales = sum(transaction_value), .groups = "drop")
  
  output$totalSalesTime <- renderPlotly({
    p <- ggplot(total_sales_time, aes(x = date, y = total_sales)) +
      geom_line(color = "firebrick", size = 1) +
      labs(title = "Total Sales Over Time",
           x = "Date", y = "Total Sales ($)") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Extra: Top 3 Cities by Total Sales
  top_cities <- sales_data %>%
    group_by(city) %>%
    summarise(total_sales = sum(transaction_value), .groups = "drop") %>%
    arrange(desc(total_sales)) %>%
    head(3)
  
  output$topCities <- renderPlotly({
    p <- ggplot(top_cities, aes(x = reorder(city, total_sales), y = total_sales, fill = city)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Top 3 Cities by Total Sales",
           x = "City", y = "Total Sales ($)") +
      scale_fill_brewer(palette = "Dark2") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Sales Forecast Plot (ARIMA)
  output$salesForecastPlot <- renderPlot({
    ts_data <- ts(total_sales_time$total_sales, frequency = 7)
    fit <- auto.arima(ts_data)
    fcast <- forecast(fit, h = 30)
    plot(fcast, main = "30-Day Sales Forecast (Simple ARIMA Model)",
         xlab = "Days", ylab = "Sales Value ($)")
  })
  
  # Auto-invalidate every 5 seconds for random row and prediction
  autoInvalidate <- reactiveTimer(5000)
  
  # Reactive random row data
  random_row <- reactive({
    autoInvalidate()
    sales_data[sample(nrow(sales_data), 1), ]
  })
  
  # Display random row info
  output$randomRowInfo <- renderUI({
    row <- random_row()
    tagList(
      p(strong("Date:"), as.character(row$date)),
      p(strong("Domain:"), row$domain),
      p(strong("City:"), row$city),
      p(strong("Transaction Value:"), round(row$transaction_value, 2)),
      p(strong("Transaction Count:"), row$transaction_count)
    )
  })
  
  # Use model to predict sale value for random input row
  output$randomPrediction <- renderUI({
    row <- random_row()
    newdata <- data.frame(
      transaction_count = row$transaction_count,
      domain = factor(row$domain, levels = levels(sales_data$domain)),
      city = factor(row$city, levels = levels(sales_data$city))
    )
    pred_value <- predict(model, newdata)
    tagList(
      p(strong("Transaction Count:"), row$transaction_count),
      p(strong("Domain:"), row$domain),
      p(strong("City:"), row$city),
      p(strong("Predicted Sale Value ($):"), round(pred_value, 2))
    )
  })
  
  # Predict on user input
  observeEvent(input$predictBtn, {
    req(input$userTransactionCount, input$userDomain, input$userCity)
    
    newdata <- data.frame(
      transaction_count = input$userTransactionCount,
      domain = factor(input$userDomain, levels = levels(sales_data$domain)),
      city = factor(input$userCity, levels = levels(sales_data$city))
    )
    
    pred <- predict(model, newdata)
    
    output$userPrediction <- renderText({
      paste0("Predicted Sale Value: $", round(pred, 2))
    })
  })
  
  # Initialize userPrediction empty on load
  output$userPrediction <- renderText({ "" })
  
}

shinyApp(ui, server)
