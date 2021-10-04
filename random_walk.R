# AUTHOR: Shade Wilson
# EMAIL: shadew@uw.edu
# DESCRIPTION: The following function is a simulation of the random walk phenomena.
# It uses the runif() funtion to return a uniform random number from 0 to 1, which is then
# scaled up to between 0 and 2pi. The number is then used as an angle, and the coordinate
# changes are calculated using the sin and cos values. random_walk() has three optional
# arguments: time, gradient, and step_size. 

# The time argument specifies how many points you want
# to create, or more generally, how long you want the simulation to run. There's a 1:1 ratio of
# time to points created. 

# The gradient argument is used in graphing the path over time; it specifies what colors to 
# transtion from/to, with the first color in the vector as the first color, and the last as
# the last.

# Lastly, the step_size argument modifies how long each step is. This argument does not effect
# the overall appearance of the graph, only the scale.

library(tidyverse)

random_walk <- function(time = 10000, 
                        gradient = c("red","yellow","green", "lightblue","darkblue"),
                        step_size = 1) {
  # Arguments supplied must be the right type or else the function will fail in the beginning.
  # Time and step_size need to be a numeric, gradient must be a character vector.
  stopifnot(is.numeric(time), is.character(gradient), is.numeric(step_size))
  
  # Creating an index for each step, starting at 1.
  walk <- (1:time)
  
  x <- vector("integer", length = length(walk))
  y <- vector("integer", length = length(walk))
  
  # Preparing the walk datafram to track x and y positions, creating random numbers with runif(),
  # and calculating the changes in x and y (dx, dy) for each random number, modified by
  # the step size.
  walk <- cbind(walk, x, y) %>% 
    as.tibble() %>% 
    mutate(random = runif(time) * 2 * pi,
           dx = sin(random) * step_size,
           dy = cos(random) * step_size)
  
  # For each instance in time, the position of the object is calculated using the position
  # of the object one instance before and the change in position calculated earlier.
  for (i in seq_along(1:(time-1))) {
    walk$x[i + 1] <- walk$x[i] + walk$dx[i + 1]
    walk$y[i + 1] <- walk$y[i] + walk$dy[i + 1]
  }
  
  # Plotting each position in time using geom_path
  plot <- ggplot(walk, aes(x, y, color = walk)) +
    geom_path() + 
    labs(color = "Time") +
    scale_colour_gradientn(colors = gradient) +
    theme_minimal()
  
  print(plot)
}


random_walk()


# Simulating stochastic processes with drift
if (FALSE) {
  library(dplyr)
  library(ggplot2)
  
  rerun(5, rnorm(100)) %>%
    set_names(paste0("sim", 1:5)) %>%
    map(~ accumulate(., ~ .05 + .x + .y)) %>%
    map_dfr(~ tibble(value = .x, step = 1:100), .id = "simulation") %>%
    ggplot(aes(x = step, y = value)) +
    geom_line(aes(color = simulation)) +
    ggtitle("Simulations of a random walk with drift")
}

