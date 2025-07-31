library(shiny)
library(DT)
library(shinydashboard)
library(lubridate)

# File paths for CSV
vehicle_file <- "vehicles.csv"
login_file <- "login_history.csv"

# Load or create vehicle data
if (file.exists(vehicle_file)) {
  vehicle_db <- read.csv(vehicle_file, stringsAsFactors = FALSE)
} else {
  vehicle_db <- data.frame(
    ID = numeric(),
    Make = character(),
    Model = character(),
    Year = numeric(),
    Price = numeric(),
    Location = character(),
    Seller = character(),
    stringsAsFactors = FALSE
  )
}

# Load or create login history
if (file.exists(login_file)) {
  login_history <- read.csv(login_file, stringsAsFactors = FALSE)
} else {
  login_history <- data.frame(
    Username = character(),
    Role = character(),
    Time = character(),
    stringsAsFactors = FALSE
  )
}

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = span(icon("car"), "Used Vehicle Portal")),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Login", tabName = "login", icon = icon("sign-in-alt")),
                menuItem("Buyer", tabName = "buyer", icon = icon("search")),
                menuItem("Seller", tabName = "seller", icon = icon("plus-circle")),
                menuItem("Admin", tabName = "admin", icon = icon("user-shield")),
                menuItem("Login History", tabName = "history", icon = icon("history"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem("login",
              fluidRow(
                box(title = tagList(icon("sign-in-alt"), "User Login"), width = 6, solidHeader = TRUE, status = "primary",
                    selectInput("role", "Login as:", choices = c("Buyer", "Seller", "Admin")),
                    textInput("username", "Enter Username"),
                    actionButton("login_btn", "Login", icon = icon("sign-in-alt"))
                )
              )
      ),
      tabItem("buyer",
              fluidRow(
                box(title = tagList(icon("filter"), "Filter Vehicles"), width = 4, status = "info",
                    selectInput("make_filter", "Make:", choices = c("All")),
                    sliderInput("price_filter", "Price Range", min = 0, max = 10000, value = c(1000, 5000))
                ),
                box(title = tagList(icon("car-side"), "Available Vehicles"), width = 8, status = "info",
                    DTOutput("buyer_table")
                )
              )
      ),
      tabItem("seller",
              fluidRow(
                box(title = tagList(icon("plus-circle"), "Add a Vehicle"), width = 6, status = "success",
                    textInput("make", "Make"),
                    textInput("model", "Model"),
                    numericInput("year", "Year", value = 2015),
                    numericInput("price", "Price", value = 3000),
                    textInput("location", "Location"),
                    actionButton("add_vehicle", "Submit", icon = icon("plus"))
                ),
                box(title = tagList(icon("list"), "Your Listings"), width = 6, status = "success",
                    DTOutput("seller_table")
                )
              )
      ),
      tabItem("admin",
              fluidRow(
                box(title = tagList(icon("tools"), "All Listings (Admin View)"), width = 12, status = "danger",
                    DTOutput("admin_table")
                )
              )
      ),
      tabItem("history",
              fluidRow(
                box(title = tagList(icon("clock"), "Login History"), width = 12, status = "warning",
                    DTOutput("login_table")
                )
              )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  user_role <- reactiveVal(NULL)
  username <- reactiveVal(NULL)
  
  vehicle_data <- reactiveVal(vehicle_db)
  login_data <- reactiveVal(login_history)
  
  # Login and redirection
  observeEvent(input$login_btn, {
    role <- input$role
    user <- input$username
    user_role(role)
    username(user)
    
    # Add to login history and save
    current_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    new_entry <- data.frame(
      Username = user,
      Role = role,
      Time = current_time,
      stringsAsFactors = FALSE
    )
    updated_logins <- rbind(login_data(), new_entry)
    login_data(updated_logins)
    write.csv(updated_logins, login_file, row.names = FALSE)
    
    showNotification(paste("Logged in as", role), type = "message")
    updateTabItems(session, "tabs", selected = switch(role, "Buyer" = "buyer", "Seller" = "seller", "Admin" = "admin"))
    
    # Update filter dropdown
    makes <- unique(vehicle_data()$Make)
    updateSelectInput(session, "make_filter", choices = c("All", makes))
  })
  
  # Seller adds vehicle
  observeEvent(input$add_vehicle, {
    new_id <- ifelse(nrow(vehicle_data()) == 0, 1, max(vehicle_data()$ID) + 1)
    new_vehicle <- data.frame(
      ID = new_id,
      Make = input$make,
      Model = input$model,
      Year = input$year,
      Price = input$price,
      Location = input$location,
      Seller = username(),
      stringsAsFactors = FALSE
    )
    updated_vehicles <- rbind(vehicle_data(), new_vehicle)
    vehicle_data(updated_vehicles)
    write.csv(updated_vehicles, vehicle_file, row.names = FALSE)
    
    showNotification("Vehicle added successfully!", type = "message")
    
    # Update make filter if needed
    makes <- unique(updated_vehicles$Make)
    updateSelectInput(session, "make_filter", choices = c("All", makes))
  })
  
  # Buyer filter
  output$buyer_table <- renderDT({
    data <- vehicle_data()
    if (input$make_filter != "All") {
      data <- data[data$Make == input$make_filter, ]
    }
    data <- data[data$Price >= input$price_filter[1] & data$Price <= input$price_filter[2], ]
    datatable(data, options = list(pageLength = 5))
  })
  
  # Seller listings
  output$seller_table <- renderDT({
    data <- vehicle_data()
    user_data <- data[data$Seller == username(), ]
    datatable(user_data, options = list(pageLength = 5))
  })
  
  # Admin view
  output$admin_table <- renderDT({
    datatable(vehicle_data(), options = list(pageLength = 10))
  })
  
  # Login history
  output$login_table <- renderDT({
    datatable(login_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
