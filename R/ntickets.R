#' n tickets
#'
#' @param N Number of seats
#' @param gamma Probability of overbooking
#' @param p Probability of showing up
#'
#' @return Displays discrete and normal plots and prints the list of values
#' @export
#'
#' @examples
ntickets <- function(N, gamma, p) {
  # Objective function for discrete case, based off the hint
  obj_discrete <- function(n) { # Where n = # of tickets sold
    1 - gamma - pbinom(N, n, p)
  }

  # Objective function to help plot the objective function, based off the task description
  obj_continuous <- function(n) { # Where n = # of tickets sold
    mean <- n * p # The tickets sold * probability of people showing up, or the average number of people expected to show
    sd <- sqrt(n * p * (1 - p)) # Sqrt of tickets sold * probability of people showing up * probability of people no-showing
    1 - gamma - pnorm(N + 0.5, mean, sd)
  }

  # Find n for discrete case
  nd <- which.min(abs(sapply(N:(2 * N), function(n) obj_discrete(n))))

  # Find n for normal approximation
  nc <- which.min(abs(sapply(N:(2 * N), function(n) obj_continuous(n))))

  # Prints a named list containing nd, nc, N, p and gamma
  print_list <- list(
    nd = N + nd - 1,  # Discrete n (adjusted for the index shift, which we discussed in class)
    nc = N + nc - 1,  # Continuous n (adjusted for the index shift, which we discussed in class)
    N = N,            # User-inputted number of seats
    p = p,            # User-inputted probability of overbooking
    gamma = gamma     # User-inputted probability of showing up
  )

  # Create list of numbers N to 2N, which are the values that n could fall in
  n_vals <- N:(2 * N)

  # layout(matrix(1:2, nrow = 2, ncol = 1)) # This is not necessary when knitting and over-complicates things

  # Get the range of the plots
  obj_discrete_vals   <- sapply(n_vals, obj_discrete)   # A vector of the discrete values
  obj_continuous_vals <- sapply(n_vals, obj_continuous) # A vector of the continuous values

  # Discrete case
  plot(n_vals, obj_discrete_vals,
       type = "o", col = "black", # Have a straight, black line between each point
       ylab = "Objective", xlab = "n", # Labels, as seen in example
       main = paste("Object Vs n to find optimal tickets sold\n(", n_vals[nd],") gamma =", gamma, " N =", N, "discrete"), # Title, as seen in example
       ylim = range(obj_discrete_vals), # Have the y-axis only show the range of the graphed data
       xlim = c(N, N + nd * 3)) # Have the x-axis from N to a bit after the curve

  # Blue dots at all points
  points(n_vals, obj_discrete_vals, pch = 20, lwd = 1, col = "blue")

  # Red lines
  abline(h = 0, col = "red", lwd = 2) # Horizontal on y = 0
  abline(v = n_vals[nd], col = "red", lwd = 2) # Vertical where nd lies on the graph

  # Continuous case
  plot(n_vals, obj_continuous_vals,
       type = "l", col = "black", # Have a curved, black line connecting the points
       ylab = "Objective", xlab = "n", # Labels, as seen in example
       main = paste("Object Vs n to find optimal tickets sold\n(", n_vals[nc],") gamma =", gamma, " N =", N, "continuous"), # Title, as seen in example
       ylim = range(obj_continuous_vals), # Have the y-axis only show the range of the graphed data
       xlim = c(N, N + nc * 3)) # Have the x-axis from N to a bit after the curve

  # Blue lines
  abline(h = 0, col = "blue") # Horizontal on y = 0
  abline(v = n_vals[nc], col = "blue") # Vertical where nc lies on the graph

  # Return the list of values (so it prints to the console)
  return(print_list)
}
