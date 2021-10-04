library(tidyverse)
library(gt)
library(gtsummary)
library(tidytuesdayR)
# This works!
#gt(yield_data_wide)

# pipe also works!
tuesdata <- tidytuesdayR::tt_load(2020, 36)
key_crop_yields <- tuesdata$key_crop_yields

country_sel <- c("China", "India", "United States", "Indonesia", "Mexico", "Pakistan")

yield_data <- tuesdata$key_crop_yields %>% 
  janitor::clean_names() %>% 
  rename_with(~str_remove(., "_tonnes_per_hectare")) %>% 
  select(entity:beans, -code) %>% 
  pivot_longer(cols = wheat:beans, names_to = "crop", values_to = "yield") %>% 
  rename(Country = entity)

yield_data %>% 
  gt()

#Add Groups
key_crop_yields %>% 
  head() %>% 
  group_by(Entity) %>% # respects grouping from dplyr
  gt(rowname_col = "crop") 

key_crop_yields %>% 
  head() %>% 
  gt(
    groupname_col = "crop",
    rowname_col = 'Country'
  )

#Adjust appearance
yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>%
  tab_header(
    title = "Crop Yields between 2014 and 2016",
    subtitle = "Countries limited to Asia"
  ) %>% 
  tab_options(
    heading.subtitle.font.size = 12,
    heading.align = "left",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3),
  )

#
yield_data_wide %>% 
  mutate(crop = str_to_title(crop)) %>% 
  group_by(crop) %>% 
  gt(
    rowname_col = "Country"
  ) %>% 
  fmt_number(
    columns = 2:5, # reference cols by position
    decimals = 2 # decrease decimal places
  ) %>% 
  summary_rows(
    groups = TRUE,
    columns = vars(`2014`, `2015`, `2016`), # reference cols by name
    fns = list(
      avg = ~mean(.), # add as many summary stats as you want!
      sd = ~sd(.)
    )
  )

#Add spanners
yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  tab_spanner(
    label = "Yield in Tonnes/Hectare", 
    columns = 2:5
  )

#Add notes and titles
yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  tab_footnote(
    footnote = "Yield in Tonnes/Hectare", 
    locations = cells_column_labels(
      columns = 1:3 # note
    )
  )

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  tab_footnote(
    footnote = "Yield in Tonnes/Hectare", 
    locations = cells_column_labels(
      columns = 1:3 # note
    )
  ) %>% 
  tab_source_note(source_note = "Data: OurWorldInData")

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>%
  tab_header(
    title = md("**Crop Yields between 2014 and 2016**"),
    subtitle = html("<em>Countries limited to Asia</em>")
  )

# Adjust appearance
yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>%
  tab_header(
    title = "Crop Yields between 2014 and 2016",
    subtitle = "Countries limited to Asia"
  ) %>% 
  tab_options(
    heading.subtitle.font.size = 12,
    heading.align = "left",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3),
  )

my_theme <- function(data) {
  tab_options(
    data = data,
    heading.subtitle.font.size = 12,
    heading.align = "left",
    table.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3),
  )
}

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>%
  tab_header(
    title = "Crop Yields between 2014 and 2016",
    subtitle = "Countries limited to Asia"
  ) %>% 
  my_theme()

yield_data_wide %>% 
  head() %>%
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "black", alpha = 0.2),
      cell_borders(
        side = c("left", "right"), 
        color = "black",
        weight = px(2)
      )
    ),
    locations = cells_body(
      columns = vars(crop)
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "red", style = "italic")
    ),
    locations = cells_body(
      columns = 3:5,
      rows = Country == "China"
    )
  ) 

yield_data_wide %>% 
  head() %>%
  gt(
    groupname_col = "crop",
    rowname_col = "Country"
  ) %>% 
  data_color(
    columns = vars(`2014`, `2015`, `2016`),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "ggsci::red_material") %>% as.character(),
      domain = NULL
    )
  )

##
tuesdata <- tidytuesdayR::tt_load(2020, "36")
country_sel <- c("China", "India", "United States", "Indonesia", "Mexico", "Pakistan")

yield_data <- tuesdata$key_crop_yields %>% 
  janitor::clean_names() %>% 
  rename_with(~str_remove(., "_tonnes_per_hectare")) %>% 
  select(entity:beans, -code) %>% 
  pivot_longer(cols = wheat:beans, names_to = "crop", values_to = "yield") %>% 
  rename(Country = entity)

#Rule 1: Offset the Heads from the Body
#The goal here is to clearly separate your column titles from the body of the table. 
# data prep
potato_data <- yield_data %>% 
  filter(Country %in% country_sel, crop == "potatoes", year %in% c(2013:2016)) %>% 
  filter(crop == "potatoes") %>% 
  pivot_wider(names_from = year, values_from = "yield")

potato_data

# Poor Example
potato_tb <- potato_data %>% 
  gt() %>% 
  cols_hide(vars(crop)) %>% 
  opt_table_lines(extent = "none") %>% 
  fmt_number(
    columns = 3:6,
    decimals = 2
  )
potato_tb

#improved 
rule1_good <- potato_tb %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  opt_table_lines(extent = "default") %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(2),
    # table_body.border.bottom.color = "white",
    # table_body.border.bottom.width = px(6)
  ) %>%
  tab_source_note(md("**Table**:@thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))
rule1_good

#Rule 2: Use Subtle Dividers Rather Than Heavy Gridlines
# data prep
rule2_data <- yield_data %>% 
  filter(Country %in% country_sel, crop == "potatoes", year %in% c(2007:2016)) %>% 
  filter(crop == "potatoes") %>% 
  select(-crop) %>% 
  pivot_wider(names_from = year, values_from = "yield") %>% 
  rowwise() %>% 
  mutate(
    avg_07_11 = mean(`2007`:`2011`),
    .before = `2012`
  ) %>% 
  mutate(
    avg_12_16 = mean(`2012`:`2016`)
  ) %>% 
  ungroup()

#Poor Example
rule2_tab1 <- rule2_data %>% 
  gt(
    rowname_col = "Country"
  ) %>% 
  cols_label(
    avg_07_11 = "Avg.",
    avg_12_16 = "Avg."
  ) %>% 
  cols_width(
    1 ~ px(125)
  ) %>% 
  fmt_number(
    columns = 2:last_col()
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "all",
      color = "grey",
      weight = px(1),
      style = "solid"
    ),
    locations = list(
      cells_body(
        everything()
      ),
      cells_column_labels(
        everything()
      )
    )
  ) %>% 
  grand_summary_rows(
    columns = 2:last_col(),
    fns = list(
      "Average" = ~mean(.)
    ),
    formatter = fmt_number
  )
rule2_tab1

rule2_tab2 <- rule2_data %>% 
  add_row(
    rule2_data %>% 
      summarise(
        across(where(is.double),
               list(Average = mean),
               .names = "{col}") 
      ) %>% 
      mutate(Country = "Average")
  ) %>% 
  gt() %>% 
  cols_label(
    avg_07_11 = "Avg.",
    avg_12_16 = "Avg."
  ) %>% 
  fmt_number(
    columns = 2:last_col()
  ) %>% 
  tab_style(
    style = cell_fill(
      color = "lightgrey"
    ),
    locations = list(
      cells_body(
        columns = vars(avg_07_11,avg_12_16)
      ),
      cells_column_labels(
        columns = vars(avg_07_11, avg_12_16)
      )
    )
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(
      columns = everything(),
      rows = Country == "Average"
    )
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black"
  ) %>% 
  tab_source_note(md("**Table**: @thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))

rule2_tab2

#Rule 3: Right-Align Numbers and Heads
rule3_data <- yield_data %>% 
  filter(Country == "United States", year %in% c(2016)) %>% 
  mutate(crop = str_to_title(crop)) %>% 
  pivot_wider(
    names_from = year,
    values_from = "yield"
  ) %>% 
  arrange(crop) %>% 
  select(-Country, Crop = crop)
rule3_data

#Comparison of alignment
rule3_align <- rule3_data %>% 
  mutate(`Center align` = `2016`,
        `Right align` = `2016`) %>% 
  rename(`Left align` = 2) %>% 
  gt() %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>% 
  fmt_number(
    columns = 2:4
  ) %>% 
  cols_align(align = "left",
             columns = 2) %>% 
  cols_align(align = "center",
             columns = 3) %>% 
  cols_align(align = "right",
             columns = 4) %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)
  ) %>% 
  tab_source_note(md("**Table**: @thomas_mock | **Data**: OurWorldInData.org<br>**Inspiration**: @jschwabish"))

rule3_align

#Addendums to alignment
rule3_data_addendum <- yield_data %>% 
  filter(
    Country %in% c("Africa"),
    year >= 2015,
    str_length(crop) == 5
  ) %>% 
  group_by(year) %>% 
  mutate(
    crop = str_to_title(crop),
    max_yield = max(yield),
    `Top Crop` = if_else(yield == max_yield, "Y", "N")
  ) %>% 
  select(Year = year, Crop = crop, `Top Crop`, Yield = yield) %>% 
  ungroup()

rule3_data_addendum %>% 
  gt()

rule3_data_addendum %>% 
  gt() %>% 
  gt::cols_align(
    align = "center",
    columns = vars(`Top Crop`, Crop)
  )

rule3_data_addendum %>% 
  pivot_wider(
    names_from = Year,
    values_from = Yield
    ) %>% 
  gt() %>% 
  gt::cols_align(
    align = "center",
    columns = vars(`Top Crop`)
  )

#Choose fonts carefully
rule3_text <- rule3_data %>% 
  mutate(Karla = `2016`,
         Cabin = `2016`,
         Georgia = `2016`,
         `Fira Mono` = `2016`) %>% 
  rename(Default = 2) %>% 
  gt() %>% 
  tab_style(
    style = list(
      cell_text(font = "Default",
                decorate = "underline"),
      locations = list(
        cells_column_labels(
          vars(Default)
        ),
        cells_body(
          vars(Default)
        )
      )
    )
    )%>% 
      tab_style(
        style = list(
          cell_text(
            font = "Karla",
            decorate = "underline"
          ),
          locations = list(
            cells_column_labels(
              vars(Karla)
            ),
            cells_body(
              vars(Karla)
            )
          )
        )
      ) %>% 
      tab_style(
        style = list(
          cell_text(font = "Cabin", decorate = "underline")
        ),
        locations = list(
          cells_column_labels(
            vars(Cabin)
          ),
          cells_body(vars(Cabin))
        )
      ) %>% 
      tab_style(
        style = list(
          cell_text(font = "Georgia",
                    decorate = "underline")
        ),
        locations = list(
          cells_column_labels(
            vars(Georgia)
          ),
          cells_body(
            vars(Georgia)
          )
        )
      ) %>% 
      tab_style(
        style = list(
          cell_text(font = "Fira Mono", decorate = "underline")
        ),
        locations = list(
          cells_column_labels(
            vars(`Fira Mono`)
          ),
          cells_body(
            vars(`Fira Mono`)
          )
        )
      ) %>% 
      fmt_number(columns = 2:6) %>% 
      tab_spanner(
        label = "Good",
        columns = c(2, 6)
      ) %>% 
      tab_spanner(
        "Bad",
        3:5
      ) %>% 
      tab_options(
        column_labels.border.top.color = "white",
        column_labels.border.top.width = px(3),
        column_labels.border.bottom.color = "black",
        table_body.hlines.color = "white",
        table.border.bottom.color = "white",
        table.border.bottom.width = px(3)
      )
    
    