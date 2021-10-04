# Excel Screenshot
library(tidyverse)
library(magick)
library(tesseract)

sack_url <- "https://pbs.twimg.com/media/ErtmQ1PXYAEUQoY?format=jpg&name=900x900"
raw_img <- image_read(sack_url)

image_ggplot(raw_img)

raw_img %>% 
  image_crop(geometry_area(0, 0, 110, 45)) %>% 
  ocr()

#Remove the background and grid
raw_img %>% 
  image_quantize(colorspace = "gray") %>% 
  image_ggplot()

no_grid <- raw_img %>% 
  image_quantize(colorspace = "gray") %>% 
  image_transparent(color = "white", fuzz=20) %>% 
  image_background("white") 

image_ggplot(no_grid)

#
no_grid %>% 
  image_negate() %>% 
  image_ggplot()

no_grid %>%
  image_negate() %>% # negate
  image_morphology(method = "Thinning", kernel = "Rectangle:20x1") %>%
  image_negate() %>% # back to white
  image_ggplot()

# Crop the image
# remove the top 20 pixels
no_grid %>% 
  image_crop(geometry_area(0, 0,110, 45)) %>% 
  image_ggplot()

no_grid_crop <- no_grid %>% 
  image_crop(geometry_area(0, 0,110, 45))

no_grid_crop %>% 
  image_ggplot()

#try ocr
no_grid_crop %>% 
  image_ocr()

num_only <- tesseract::tesseract(
  options = list(tessedit_char_whitelist = c(".0123456789 "))
)

no_grid %>% 
  image_quantize(colorspace = 'gray') %>% 
  image_threshold() %>% 
  image_crop(geometry_area(100, 0, 600, 40)) %>% 
  ocr(engine = num_only) 

combo <- tesseract::tesseract(
  options = list(
    tessedit_char_whitelist = paste0(
      c(letters, LETTERS, " ", ".0123456789 (-)"), collapse = "")
  )
)

raw_text <- no_grid %>%
  image_quantize(colorspace = "gray") %>%
  image_transparent("white", fuzz = 22) %>%
  image_background("white") %>%
  image_threshold() %>%
  image_crop(geometry_area(0, 0, 110, 45)) %>%  
  ocr(engine = combo)

#Make a Tibble
raw_text

raw_tibble <- raw_text %>% 
  str_split(pattern = "\n") %>% 
  unlist() %>%
  tibble(data = .) 

raw_tibble

# Tidy the Tibble

raw_tibble %>% 
  filter(str_length(data) >= 2)  %>%
  separate(
    data, 
    into = c("player", "position", "team", "sacks"), 
    sep = c(" \\(| - |\\) ")
  ) %>% 
  mutate(sacks = as.double(sacks))

#function
scrape_fun <- function(url_in, crop_left, crop_top){
  raw_img <- image_read(url_in) %>% 
    image_quantize(colorspace = 'gray') %>%
    image_transparent("white", fuzz=22) %>% 
    image_background("white") %>%
    image_threshold() %>% 
    image_crop(geometry_area(0, 0, crop_left, crop_top)) 
  
  image_ocr(raw_img) %>% 
    str_c() %>% 
    str_split(pattern = "\n") %>% 
    unlist() %>%
    tibble(data = .) %>% 
    filter(str_length(data) >= 2) %>% 
    separate(
      data, 
      into = c("player", "position", "team", "sacks"), 
      sep = c(" \\(| - |\\) ")
    ) %>% 
    mutate(sacks = as.double(sacks)) %>% 
    mutate(sacks = if_else(sacks >= 20, sacks/10, sacks))
}

# output to tibble
cr_sacks <- tibble(
  url_in = c(
    "https://pbs.twimg.com/media/ErtmQ1PXYAEUQoY?format=jpg&name=900x900",
    "https://pbs.twimg.com/media/ErtmSLlXMAAvylA?format=jpg&name=900x900",
    "https://pbs.twimg.com/media/ErtmTUGW8AEZ6Cy?format=jpg&name=900x900"
  ),
  crop_left = c(110, 95, 95),
  crop_top = c(45, 5, 5)
) %>% 
  pmap_df(scrape_fun)

cr_sacks

cr_sacks %>% 
  mutate(position = if_else(position == "LB", "OLB", position)) %>% 
  ggplot(aes(x = sacks, y = position)) +
  ggridges::geom_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  geom_point(
    data = filter(cr_sacks, player %in% c("Aaron Donald", "T.J. Watt")),
    size = 3
  ) +
  ggridges::theme_ridges() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16),
    plot.caption = element_text(size = 10),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(hjust = 0)
  ) +
  labs(
    x = "\nCreated Sacks", y = "",
    title = "T.J. Watt and A. Donald are both outliers amongst their positions",
    subtitle = "Created Sacks by position",
    caption = "Data: ESPN | Plot: @thomas_mock"
  )

#Example 2
burke <- "https://pbs.twimg.com/media/EpdF4fzW4AEeOvF?format=png&name=small"
raw_img <- image_read(burke)

image_ggplot(raw_img)

# Clean it up
no_grid <- raw_img %>% 
  image_transparent(color = "white", fuzz=20) %>% 
  image_background("white") 

image_ggplot(no_grid)

#
no_grid %>% 
  image_negate() %>% 
  image_ggplot()

no_grid %>% 
  image_negate() %>%
  image_morphology(method = "Thinning", kernel = "Rectangle:20x1") %>% 
  image_negate() %>% 
  image_ggplot()

#Crop the image
# remove the top 20 pixels
no_grid %>% 
  image_crop(geometry_area(0, 0, 0, 20)) %>% 
  image_ggplot()

no_grid_crop <- no_grid %>% 
  image_crop(geometry_area(0, 0, 0, 20))

no_grid_crop %>% 
  image_ggplot()

#try ocr
no_grid_crop %>% 
  image_ocr()

num_only <- tesseract::tesseract(
  options = list(tessedit_char_whitelist = c(".0123456789 "))
)
no_grid %>% 
  image_quantize(colorspace = 'gray') %>% 
  image_threshold() %>% 
  image_crop(geometry_area(80, 0, 80, 20)) %>% 
  ocr(engine = num_only) 

ocr_col1 <- no_grid %>%
  image_crop(geometry_area(80, 0, 80, 20)) %>%
  ocr(engine = num_only) %>%
  str_split(pattern = "\n") %>%
  unlist() %>%
  enframe() %>%
  mutate(value = as.double(value)) %>%
  filter(!is.na(value))

ocr_col1 %>%
  mutate(color = case_when(
    value > 100 ~ "red",
    value > lag(value) ~ "red",
    value > lag(value, n = 3) ~ "red",
    TRUE ~ "black"
  )) %>%
  ggplot(aes(x = name, y = value, color = color)) +
  geom_point(size = 3) +
  scale_color_identity()

ocr_col1 %>% 
  mutate(color = case_when(
    value > 100 ~ "red",
    value > lag(value) ~ "red",
    value > lag(value, n = 3) ~ "red",
    TRUE ~ "black"
  )) %>% 
  mutate(
    value = if_else(value > 100, value/10, value),
    value = if_else(name >= 22, value/10, value)
  ) %>% 
  ggplot(aes(x = name, y = value, color = color)) +
  geom_point(size = 3) +
  scale_color_identity()

#function
img_ocr_fun <- function(trim_width, trim_start, char_num = TRUE) {
  
  num_only <- tesseract::tesseract(
    options = list(tessedit_char_whitelist = c(".0123456789 "))
  )
  
  combo <- tesseract::tesseract(
    options = list(
      tessedit_char_whitelist = paste0(
        c(letters, LETTERS, " ", ".0123456789 "), collapse = "")
    )
  )
  
  
  input_char <- if (isTRUE(char_num)) {
    num_only
  } else {
    combo
  }
  
  no_grid %>%
    image_crop(geometry_area(trim_width, 0, trim_start, 20)) %>%
    ocr(engine = input_char) %>%
    str_split(pattern = "\n") %>%
    unlist() %>%
    enframe() %>%
    select(-name) %>%
    filter(!is.na(value), str_length(value) > 0)
}

c(
  no_grid %>%
    image_crop(geometry_area(80, 0, 0, 20)),
  no_grid %>%
    image_crop(geometry_area(50, 0, 80, 20)),
  no_grid %>%
    image_crop(geometry_area(50, 0, 140, 20)),
  no_grid %>%
    image_crop(geometry_area(50, 0, 210, 20))
) %>%
  image_append() %>%
  image_ggplot()

all_ocr <- list(trim_width = c(80, 50, 50, 50),
                trim_start = c(0, 80, 140, 210),
                char_num = c(FALSE, TRUE, FALSE, TRUE)) %>% 
  pmap(img_ocr_fun) 

data_df <- all_ocr %>% 
  bind_cols() %>% 
  set_names(nm = "team", "win", "lose", "leverage") 

data_df

#clean it up 
data_df %>% 
  mutate(across(win:leverage, ~str_replace(tolower(.x), "s", "5"))) %>% 
  mutate(across(win:leverage, ~str_replace(tolower(.x), "o|a", "0"))) %>% 
  mutate(across(win:leverage, as.double)) %>% 
  mutate(across(win:leverage, ~if_else(.x > 100, .x/10, .x))) %>% 
  mutate(lose = if_else(lose > win, lose/10, lose)) %>% 
  mutate(leverage = win - lose) %>% 
  print(n = 25)

####
course <- "C:/Users/terry/Pictures/table_image.jpg"

raw_image <- image_read(course)

image_ggplot(raw_image)

#clean it up
no_grid <- raw_image %>% 
  image_transparent(color = "white", fuzz=20) %>% 
  image_background("white") 

image_ggplot(no_grid)

no_grid %>% 
  image_negate() %>% 
  image_ggplot()

no_grid %>% 
  image_negate() %>%
  image_morphology(method = "Thinning", kernel = "Rectangle:20x1") %>% 
  image_negate() %>% 
  image_ggplot()

# Crop the image
# remove the top 20 pixels
no_grid %>% 
  image_crop(geometry_area(0, 0, 0, 20)) %>% 
  image_ggplot()
# remove the top 20 pixels
no_grid %>% 
  image_crop(geometry_area(0, 0,250, 250)) %>% 
  image_ggplot()

chinese <- tesseract("chi_sim")
no_grid %>% 
  image_ocr(engine = "chi_sim")
?image_ocr

##
raw_image %>% 
  image_crop(geometry_area(0, 0, 250, 250)) %>% 
  ocr(engine = "chi_sim")

raw_image %>% 
  image_quantize(colorspace = "gray") %>% 
  image_ggplot()

no_grid <- raw_image %>% 
  image_quantize(colorspace = "gray") %>% 
  image_transparent(color = "white", fuzz=20) %>% 
  image_background("white") 

image_ggplot(no_grid)

no_grid %>% 
  image_negate() %>% 
  image_ggplot()

no_grid %>%
  image_negate() %>% # negate
  image_morphology(method = "Thinning", kernel = "Rectangle:20x1") %>%
  image_negate() %>% # back to white
  image_ggplot()

# remove the top 20 pixels
no_grid %>% 
  image_crop(geometry_area(0, 0,250, 250)) %>% 
  image_ggplot()

no_grid_crop <- no_grid %>% 
  image_crop(geometry_area(0, 0,250, 250))

no_grid_crop %>% 
  image_ggplot()

no_grid_crop %>% 
  ocr(engine = chinese)

