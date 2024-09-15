# Simulation functions ----------------------------------------------------
#' Create a passenger object (dataframe)
#' 
#' @param id A positive integer; the passenger's ID.
#' @param row A positive integer; the row number.
#' @param seat A positive integer; the seat number.
#' @param carry_on A non-negative integer; the number of carry-on luggage.
#' @param group An integer; the group number.
#' 
#' @examples
#' Passenger(1, 1, 1, 0, 1)
#' 
#' @return A data frame
Passenger <- function(id, row, seat, carry_on = 0, group = -1) {
  data.frame(
    # Passenger information
    id = id,
    row = row,
    seat = seat,
    carry_luggage = carry_on,
    group = group,
    
    # State variables
    position_x = 0,
    position_y = 0,
    seated = FALSE,
    stowing_luggage = 0,
    wait_to_board = 0,
    wait_to_seat = 0,
    wait_to_fly = 0
  )
}


#' Create a matrix representing the plane layout
#' 
#' @param num_row A positive integer; the number of rows. 
#' @param row_layout A vector describing the layout of the seats.
#' Some examples are c(3, 4, 3), c(3, 3), c(3, 3), c(2, 4, 2).
#' 
#' @examples 
#' layout <- create_plane_layout(20, c(3, 4, 3))
#' print(layout)
#' 
#' @return A numeric matrix
create_plane_layout <- function(num_row, row_layout) {
  layout <- matrix(
    0, nrow = num_row,
    ncol = sum(row_layout) + (length(row_layout) - 1)  # include the aisles
  )
  
  ind <- head(cumsum(row_layout) + seq_along(row_layout), -1)
  layout[, ind] <- -1  # aisles
  layout
}


#' The main simulation function
#' 
#' @param num_row A positive integer; the number of rows of the plane.
#' @param row_layout A vector describing the layout of the seats.
#' Some examples are c(3, 4, 3), c(3, 3), c(3, 3), c(2, 4, 2).
#' @param p_book A numeric value between 0 and 1; the proportion of booked seats.
#' @param method 'print', 'plot' or 'animate'. 
#' For 'print', the simulation will print out the layout matrix.
#' For 'plot', the simulation will plot the layout matrix with 'base' plot.
#' For 'animate', the simulation will plot the layout matrix with 'animate' plot.
simulate <- function(num_row = 5, row_layout = c(2, 2), p_book = 1.0, 
                     max_luggage = 2, strategy = "random",
                     method = 'print', duration = 250) {
  # Set up the simulation (plane layout, passengers, etc.)
  # 1. Plane layout (number of rows and seats per row)
  layout <- create_plane_layout(num_row, row_layout)
  
  # 2. Passenger list (id, row, seat, carry-on luggage)
  total_seats <- num_row * sum(row_layout)
  total_passengers <- round(p_book * total_seats)
  
  ids <- sort(sample(total_seats, total_passengers))
  passenger_list <- ids |>
    lapply(function(id) {
      row <- ceiling(id / sum(row_layout))
      seat <- ((id - 1) %% sum(row_layout)) + 1
      adj_seat <- seat + sum(seat > cumsum(row_layout))  # adjust for aisle
      carry_on <- sample(0:max_luggage, 1)
      Passenger(id, row, adj_seat, carry_on, -1)
    }) |>
    do.call(rbind, args = _)
  
  # 3. Begin boarding
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
  passenger_list$position_y <- closest_aisle(passenger_list$seat, row_layout)
  # print(passenger_list)
  
  # 4. Begin simulation and display the state with matrix
  time <- 0
  args <- environment()
  # display_layout(passenger_list, method, args)
  while (any(!passenger_list$seated)) {
    time <- time + 1
    passenger_list <- move(passenger_list)
    # display_layout(passenger_list, method, args)
    # Sys.sleep(duration * 1.5/ 1000)
  }
  time
}


#' One-step update of the passengers' positions
#' 
#' @param passengers A data frame of passengers.
#' 
#' @return A data frame 
move <- function(passengers) {
  for (i in 1:nrow(passengers)) {
    passenger <- passengers[i, ]
    
    # Skip if already seated
    if (passenger$seated) {
      passenger$wait_to_fly <- passenger$wait_to_fly + 1
      passengers[i, ] <- passenger
      next
    }
    
    # If the passenger has not reached the row yet, move forward if possible, else wait.
    if (passenger$position_x < passenger$row) {
      next_pos <- next_position(passenger)
      if (!is_occupied(next_pos, passengers)) {
        passenger$position_x <- next_pos$x
        passenger$position_y <- next_pos$y
      } else {
        if (passenger$position_x <= 0) {
          passenger$wait_to_board <- passenger$wait_to_board + 1    
        } else {
          passenger$wait_to_seat <- passenger$wait_to_seat + 1
        }
      }
      passengers[i, ] <- passenger
      next
    }
    
    # If the passenger has reached the row, stow luggage if possible, else seat.
    if (passenger$position_x == passenger$row) {
      if (passenger$stowing_luggage < passenger$carry_luggage) {
        passenger$stowing_luggage <- passenger$stowing_luggage + 1
      } else {
        passenger$position_y <- passenger$seat
        passenger$seated <- TRUE
      }
      passengers[i, ] <- passenger
      next
    }
  }
  
  passengers
}

next_position <- function(passenger) {
  list(x = passenger$position_x + 1, y = passenger$position_y)
}

is_occupied <- function(position, passenger_list) {
  any(passenger_list$position_x == position$x & passenger_list$position_y == position$y)
}

closest_aisle <- Vectorize(
  function(seat, row_layout) {
    rl <- head(row_layout, -1)
    aisle <- cumsum(rl) + 0.5
    ind <- which.min(abs(seat - aisle)) # the n-th aisle
    cumsum(rl + 1)[ind]  # the aisle position
  },
  "seat"
)


# Layout display functions -----------------------------------------------------
#' Display the plane layout with the passengers' positions / occupancy
#' 
#' @param passengers A data frame of passengers.
#' @param method 'print', 'plot' or 'animate'. 
#' For 'print', the simulation will print out the layout matrix.
#' For 'plot', the simulation will plot the layout matrix with 'base' plot.
#' For 'animate', the simulation will plot the layout matrix with 'animate' plot.
#' @param args Additional arguments.
display_layout <- function(passengers, method, args) {  
  if (method == "print") {
    print_layout(passengers, args$layout)
  }
  
  if (method == "plot") {
    plot_layout(passengers)
  }
  
  if (method == "animate") {
    animate_layout(passengers, args)
  } 
}

#' Print the plane layout with the passengers' positions / occupancy
#' 
#' @param passengers A data frame of passengers.
#' @param layout A numeric matrix representing the plane layout.
#' 
#' @return A numeric matrix
print_layout <- function(passengers, layout) {
  ind <- which(passengers$position_x >= 1 & passengers$position_y >= 1)
  for (i in ind) {
    passenger <- passengers[i, ]
    layout[passenger$position_x, passenger$position_y] <- passenger$id
  }
  print(layout)
  layout
}

plot_layout <- function(passengers) {
  stop("Not implemented yet.")
}

# Use 'animate'💫 for Plotting
animate_layout <- function(passenger_list, args) {
  # Destructuring
  num_row <- args$num_row
  row_layout <- args$row_layout
  duration <- args$duration

  id <- passenger_list$id
  row <- passenger_list$row
  seat <- passenger_list$seat
  position_x <- passenger_list$position_x
  position_y <- passenger_list$position_y
  stowing_luggage <- passenger_list$stowing_luggage
  carry_luggage <- passenger_list$carry_luggage
  
  
  # Constants
  num_seat <- sum(row_layout)
  num_space <- num_seat + length(row_layout) - 1
  transition <- list(duration = duration)
  ind <- which(position_x >=0 & position_y >= 0)
  col <- c("lightgreen", "lightblue")[as.numeric(stowing_luggage > 0 & position_y != seat) + 1]
  
  # Handle aspect ratio
  w_unit <- WIDTH / num_row
  h_unit <- HEIGHT / num_space
  size <- min(w_unit, h_unit)
  font_size <- 14 * scale_range(num_row, c(10, 30), c(1.5, 1 / 1.5))
  scale <- 1.35
  if (w_unit < h_unit) {
    par(xlim = c(0, num_row + 1), ylim = c(num_space, 0) * (h_unit / w_unit))
  } else {
    par(xlim = c(0, num_row + 1) * (w_unit / h_unit), ylim = c(num_space, 0))
  }
  
  # Plotting
  points(
    row, 
    seat, 
    id = paste0("seat-", id), 
    pch = "square", 
    col = "black", 
    bg = "lightgray", 
    cex = (size / scale)^2, 
    style = list("stroke-width" = "2px")
  )
  points(
    position_x[ind],
    position_y[ind],
    id = id[ind],
    bg = col[ind],
    cex = 0.8 * (size / scale^2)^2,
    transition = transition,
    style = list("stroke-width" = "0px")
  )
  text(
    position_x[ind],
    position_y[ind] - 0.1,
    id[ind],
    id = paste0("text-id-", id[ind]),
    transition = transition,
    style = list("text-anchor" = "middle", 
                 "dominant-baseline" = "middle",
                 "font-size" = paste0(font_size * 1.5, "px"))
  )
  text(
    position_x[ind] - 0.05,
    position_y[ind] + 0.15,
    paste0("🧳", carry_luggage[ind] - stowing_luggage[ind]),
    id = paste0("text-luggage-", id[ind]),
    transition = transition,
    style = list("text-anchor" = "middle", 
                 "dominant-baseline" = "middle",
                 "font-size" = paste0(font_size, "px"))
  )
}

scale_range <- function(x, input_range, output_range) {
  input_min <- input_range[1]
  input_max <- input_range[2]
  output_min <- output_range[1]
  output_max <- output_range[2]
  slope <- (output_max - output_min) / (input_max - input_min)
  return (slope * (x - input_min) + output_min)
}


# Boarding strategy -------------------------------------------------------
random_boarding <- function(passengers) {
  passengers[sample(nrow(passengers)), ]
}

back_to_front <- function(passengers) {
  passengers[order(passengers$row, decreasing = TRUE), ]
}

front_to_back <- function(passengers) {
  passengers[order(passengers$row), ]
}

back_to_front_with_shuffle <- function(passengers, k = 3) {
  ps <- passengers[order(passengers$row, decreasing = TRUE), ]
  permute_blocks(ps, k)
}

front_to_back_with_shuffle <- function(passengers, k = 3) {
  ps <- passengers[order(passengers$row), ]
  permute_blocks(ps, k)
}

permute_blocks <- function(df, k) {
  n <- nrow(df)
  if (n %% k != 0) {
    stop("The number of rows in the dataframe is not divisible by the number of blocks.")
  }
  
  rows_per_block <- n / k
  blocks <- split(df, rep(1:k, each = rows_per_block))
  permuted_blocks <- lapply(blocks, function(block) {
    block[sample(nrow(block)), ]
  })
  result <- do.call(rbind, permuted_blocks)
  result
}


# window_middle_aisle <- function(passengers) {
#   passengers[order(passengers$seat), ]
# }


# Main simulation -------------------------------------------------------------
WIDTH <- 1500
HEIGHT <- 750
