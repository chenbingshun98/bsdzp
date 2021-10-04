# Installs gganimate
install.packages("gganimate")
# Loads the library
library(gganimate)
# Other libraries that will be used
library(ggplot2)
library(tidyverse)

# Link where the data are stored
link <- "https://raw.githubusercontent.com/goodekat/presentations/master/2019-isugg-gganimate-spooky/bat-data/bats-subset.csv"

# Access the data and convert id to a factor
bats <- read_csv(link) %>%
  mutate(id = factor(id))
str(bats)

ggplot(
  bats %>% filter(id == 1),
  aes(
    x = longitude,
    y = latitude,
    color = time
  )
) +
  geom_point() +
  scale_color_gradient2(
    midpoint = 6,
    low = "orange",
    mid = "purple",
    high = "black"
  ) +
  gganimate::transition_reveal(time)

bats %>%
  ggplot(aes(
    x = longitude, y = latitude,
    color = time
  )) +
  geom_point() +
  scale_color_gradient2(
    midpoint = 6,
    low = "orange",
    mid = "purple",
    high = "black"
  ) +
  gganimate::transition_states(states = id)

bats %>%
  ggplot(aes(
    x = longitude,
    y = latitude,
    color = time,
    group = id
  )) +
  geom_point() +
  scale_color_gradient2(
    midpoint = 6,
    low = "orange",
    mid = "purple",
    high = "black"
  ) +
  gganimate::transition_states(states = id) +
  labs(title = "Bat {previous_state}") +
  gganimate::transition_states(
    states = id,
    transition_length = 3,
    state_length = 3
  )

# 时间
bats %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_point()

bats %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_point() +
  transition_time(time = time)

time_plot <- bats %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_point() +
  transition_time(time = time)
animate(
  plot = time_plot,
  nframes = 300
)

# Transition: Reveal
bats %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_point() +
  transition_reveal(along = time)

bats %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_path() +
  transition_reveal(along = time)

bats %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_path() +
  geom_point() +
  transition_reveal(along = time)

# History of points can be kept by assigning the along variable to a group
bats %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_path() +
  geom_point(aes(group = time)) +
  transition_reveal(along = time)

# Use the ggimage R package to replace the point with an image
library(ggimage)
bat_image_link <-
  "https://raw.githubusercontent.com/goodekat/presentations/master/2019-isugg-gganimate-spooky/figures/bat-cartoon.png"
bats %>%
  mutate(
    image = bat_image_link
  ) %>%
  filter(id == 1) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_path() +
  ggimage::geom_image(aes(image = image),
    size = 0.3
  ) +
  gganimate::transition_reveal(time)

#
bat_colors <- c("darkorange", "orangered", "violetred", "purple", "black")
bats %>%
  mutate(image = bat_image_link) %>%
  ggplot(aes(
    x = longitude,
    y = latitude,
    group = id,
    color = id
  )) +
  geom_path() +
  ggimage::geom_image(aes(
    image = image,
    size = 0.1
  )) +
  scale_color_manual(values = bat_colors)

bats %>%
  mutate(image = bat_image_link) %>%
  ggplot(aes(x = longitude, y = latitude, group = id, color = id)) +
  geom_path() +
  geom_image(aes(image = image), size = 0.1) +
  scale_color_manual(values = bat_colors) +
  transition_reveal(along = time)

# follow
bats %>%
  mutate(image = bat_image_link) %>%
  filter(id == 3) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_image(aes(image = image),
    size = 0.1
  ) +
  geom_path() +
  transition_reveal(time) +
  view_follow()

bats %>%
  mutate(image = bat_image_link) %>%
  filter(id == 3) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_image(aes(image = image),
    size = 0.1
  ) +
  geom_path() +
  transition_reveal(time) +
  view_step(
    pause_length = 3,
    step_length = 1,
    nsteps = 5
  ) #<<

#
bats %>%
  ggplot(aes(
    x = longitude,
    y = latitude,
    color = time,
    group = id
  )) +
  geom_point() +
  scale_color_gradient2(
    midpoint = 6,
    low = "orange",
    mid = "purple",
    high = "black"
  ) +
  transition_states(states = id) +
  view_step(
    pause_length = 3,
    step_length = 1,
    nsteps = 5
  )

bats %>%
  mutate(image = bat_image_link) %>%
  ggplot(aes(x = longitude, y = latitude, group = id, color = id)) +
  geom_path() +
  geom_image(aes(image = image), size = 0.1) +
  scale_color_manual(values = bat_colors) +
  transition_reveal(along = time) +
  view_follow()

# shadow:wake
bats %>%
  filter(id == 3) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_point() +
  transition_reveal(time) +
  shadow_wake(
    wake_length = 0.1,
    alpha = 0.5
  )

# show:trail
bats %>%
  filter(id == 3) %>%
  ggplot(aes(
    x = longitude,
    y = latitude
  )) +
  geom_point() +
  transition_reveal(time) +
  shadow_trail(
    distance = 0.01,
    alpha = 0.5,
    shape = 2
  )

bats %>%
  mutate(image = bat_image_link) %>%
  ggplot(aes(
    x = longitude,
    y = latitude,
    group = id,
    color = id
  )) +
  geom_point() +
  scale_color_manual(values = bat_colors) +
  transition_reveal(along = time) +
  shadow_wake(wake_length = 0.2, size = 5, alpha = FALSE, colour = "grey92")#没有color
?shadow_wake
