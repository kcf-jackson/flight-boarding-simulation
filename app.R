library(shiny)
library(shinyjs)
library(animate)
source("main.R")


# Define UI for application that draws a histogram -----------------------------
ui <- fluidPage(
    useShinyjs(),  # Initialize shinyjs
    tags$style(HTML("
        h2 { font-size: 4rem; }
        p { font-size: 2.5rem; }
        table { font-size: 2.5rem; }
    ")),
    titlePanel("Flight boarding simulator"),
    p("This app simulates flight boarding strategies. Can you figure out which boarding strategy is the most efficient?"),
  
    sidebarLayout(
        sidebarPanel(
          # Section 1: Flight Settings
          wellPanel(
            h4("Flight Settings"),
            sliderInput("rows",
                          "Number of rows:",
                          min = 10,
                          max = 30,
                          value = 20),
            textInput("row_layout", 
                      "Row layout",
                      "3, 3",
                      placeholder = "Integers separated by commas, e.g. '2,3', '3,4,3'.")
          ),
          
          # Section 2: Passenger Settings
          wellPanel(
            h4("Passenger Settings and Boarding Strategy"),
            sliderInput("n_luggage",
                        "Maximum number of luggages:",
                        min = 0,
                        max = 3,
                        value = 2),
            # rangeInput("satisfaction",
            #            "Satisfaction range:",
            #            min = 1,
            #            max = 10,
            #            step = 0.5, 
            #            value = c(1.5, 5)),
            selectInput("strategy", "Boarding strategy", 
                        list("Random" = "random", 
                             "Back-to-front" = "back-to-front", 
                             "Front-to-back" = "front-to-back", 
                             "Window-Middle-Aisle" = "window-middle-aisle"),
                        selected = "Random")
          ),
          
          wellPanel(
            h4("Control"),
            sliderInput("duration",
                        "Time between update (ms)",
                        min = 50,
                        max = 500,
                        value = 350, step = 50),
            actionButton("start", "Start"),
            actionButton("pause", "Play / Pause"),
            actionButton("reset", "Reset")
            # actionButton("mruns", "1000 Runs")
          ),
          width = 3
        ),
        
        mainPanel(
          fluidRow(
            column(
              div(textOutput("time"), style = "font-size: 32px;"),
              animateOutput(),
              width = 10
            ),
            column(
              tableOutput("satisfaction_table"),
              width = 2
            )
          ),
          fluidRow(
            plotOutput("time_histogram")      
          ),
          width = 9
        )
    )
)

# Define server logic required to draw a histogram -----------------------------
server <- function(input, output, session) {
    # Initialisation
    if (exists("device")) detach(device)
    device <- animate$new(WIDTH, HEIGHT, session = session)
    attach(device)
    
    v <- reactiveValues(
      playing = FALSE,
      passenger_list = NULL,
      method = NULL,
      args = NULL,
      time = 0
    )
    
    shinyjs::disable("pause")
    
    
    # Events
    observeEvent(input$rows, {
      num_row <- input$rows
      row_layout <- as.numeric(trimws(strsplit(input$row_layout, ",")[[1]]))
      strategy <- input$strategy
      max_luggage <- input$n_luggage
      init_layout(num_row, row_layout, p_book = 1.0, max_luggage = max_luggage,
                  method = "animate", strategy = strategy)
    })

    observeEvent(input$start, { 
      disable_ui()
      shinyjs::enable("pause")
      shinyjs::enable("reset")
      
      num_row <- input$rows
      row_layout <- as.numeric(trimws(strsplit(input$row_layout, ",")[[1]]))
      strategy <- input$strategy
      max_luggage <- input$n_luggage
      duration <- input$duration      
      
      # Initialize layout and store the required variables in reactiveValues
      args <- init_layout(num_row, row_layout, p_book = 1.0, max_luggage = max_luggage,
                          method = "animate", strategy = strategy)
      v$passenger_list <- args$passenger_list
      v$method <- args$method
      v$args <- args$args
      v$args$duration <- duration
      v$time <- 0
      
      v$playing <- TRUE
    })
    
    observeEvent(input$pause, { 
      v$playing <- !v$playing
    })
    
    observeEvent(input$reset, { 
      v$playing <- FALSE
      enable_ui()
      shinyjs::disable("pause")
      clear()
    })
    
    # A bit slow to run
    # observeEvent(input$mruns, {
    #   output$time_histogram <- renderPlot({
    #     complete_time <- sapply(1:10, function(id) {
    #       res <- simulate(
    #         num_row = input$rows, 
    #         row_layout = as.numeric(trimws(strsplit(input$row_layout, ",")[[1]])), 
    #         p_book = 1.0, 
    #         max_luggage = input$max_luggage,
    #         strategy = input$strategy
    #       )
    #       print(res)
    #       res
    #     })
    #     hist(complete_time, breaks = 20, main = "Time to board 1000 planes", xlab = "Time")
    #   })
    # })
    
    observeEvent(input$duration, {
      v$args$duration <- input$duration
    })
    
    observeEvent(v$time, {
      output$time <- renderText({
        paste("Time:", v$time)
      })
    })
    
    # Simulation update with pausing functionality
    observe({
      invalidateLater(isolate(v$args$duration), session)
      
      if (v$playing) {
        pl <- isolate(v$passenger_list)
        if (any(!pl$seated)) {
          v$time <- isolate(v$time) + 1
          v$passenger_list <- move(pl)
          display_layout(pl, 
                         isolate(v$method), 
                         isolate(v$args))
        } else {
          v$passenger_list <- move(pl)
          display_layout(pl, 
                         isolate(v$method), 
                         isolate(v$args))
          
          # Update table with the customers' satisfaction
          # use the passenger_list to calculate the satisfaction based on the wait_to_seat
          satisfaction <- pl$wait_to_seat
          unhappy <- satisfaction > pl$seat * 2
          okay <- satisfaction > pl$seat * 1.5 & satisfaction <= pl$seat * 2
          happy <- satisfaction <= pl$seat * 1.5 
          satisfaction[unhappy] <- "Unhappy"
          satisfaction[okay] <- "Okay"
          satisfaction[happy] <- "Happy"
          satisfaction <- data.frame(satisfaction)
          output$satisfaction_table <- renderTable({
            table(satisfaction)
          })
          
          v$playing <- FALSE
          enable_ui()
          shinyjs::disable("pause")
        }
      }
    })
}


# Helper functions -------------------------------------------------------------
disable_ui <- function() {
  event_ids <- c("rows", "row_layout", "n_luggage", "strategy", "start", "pause", "reset")
  lapply(event_ids, shinyjs::disable)
}

enable_ui <- function() {
  event_ids <- c("rows", "row_layout", "n_luggage", "strategy", "start", "pause", "reset")
  lapply(event_ids, shinyjs::enable)
}

init_layout <- function(num_row, row_layout, p_book, max_luggage, method, strategy) {
  clear()
  # Set up the simulation (plane layout, passengers, etc.)
  layout <- create_plane_layout(num_row, row_layout)
  
  total_seats <- num_row * sum(row_layout)
  total_passengers <- round(p_book * total_seats)
  ids <- sort(sample(total_seats, total_passengers))
  
  passenger_list <- ids |>
    lapply(function(id) {
      row <- ceiling(id / sum(row_layout))
      seat <- ((id - 1) %% sum(row_layout)) + 1
      adj_seat <- seat + sum(seat > cumsum(row_layout))
      carry_on <- sample(0:max_luggage, 1)
      Passenger(id, row, adj_seat, carry_on, -1)
    }) |>
    do.call(rbind, args = _)
  
  # Line up the passengers in a queue with a boarding strategy
  boarding <- switch(
    strategy,
    "random" = random_boarding,
    "back-to-front" = back_to_front,
    "front-to-back" = front_to_back,
    "window-middle-aisle" = window_middle_aisle 
  )
  passenger_list <- boarding(passenger_list)
  passenger_list$position_x <- - seq_len(total_passengers) + 1
  seat <- ((passenger_list$id - 1) %% sum(row_layout)) + 1
  passenger_list$position_y <- closest_aisle(seat, row_layout)
  
  # Display the layout      
  time <- 0
  args <- environment()
  display_layout(passenger_list, method, args) 
  
  args
}


# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)
