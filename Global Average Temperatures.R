library(tidyverse)
temperatures <- read_csv("https://pkgstore.datahub.io/core/global-temp/annual_csv/data/a26b154688b061cdd04f1df36e4408be/annual_csv.csv")
glimpse(temperatures, width = 40)

temperatures <- temperatures %>% 
  filter(Source=="GCAG")

# Create the time series
temperatures %>% ggplot(aes(Year,Mean))+
  geom_ribbon(aes(ymax=Mean,ymin=0),
              fill="#EB5286",
              alpha=0.7)+
  geom_line(color="#6F213F")

#Add title, caption and axis labels
ggplot(temperatures, aes(x = Year, y = Mean)) + 
  geom_ribbon(aes(ymax = Mean, ymin = 0), 
              fill = "#EB5286", alpha = 0.7) +
  geom_line(color = "#6F213F") +
  scale_y_continuous(expand = expand_scale(mult = 0)) +
  scale_x_continuous(expand = expand_scale(mult = 0)) +
  labs(
    title = "Global Average Temperature 1880 - 2016",
    subtitle = paste0("Compared to the average temperature ",
                      "from 1951 to 1980, the global temperature\n", 
                      "has risen significantly since 1980."),
    y = "Mean global temperatures",
    caption = paste0("Source: https://datahub.io/core/global-temp\n",
                     "Visualization: Christian Burkhart")
  )

#adjust color
library(showtext)
font.add("Roboto","Roboto-Regular.ttf")  
# Previous time series code goes here
ggplot(temperatures, aes(x = Year, y = Mean)) + 
  geom_ribbon(aes(ymax = Mean, ymin = 0), 
              fill = "#EB5286", alpha = 0.7) +
  geom_line(color = "#6F213F") +
  scale_y_continuous(expand = expand_scale(mult = 0)) +
  scale_x_continuous(expand = expand_scale(mult = 0)) +
  labs(
    title = "Global Average Temperature 1880 - 2016",
    subtitle = paste0("Compared to the average temperature ",
                      "from 1951 to 1980, the global temperature\n", 
                      "has risen significantly since 1980."),
    y = "Mean global temperatures",
    caption = paste0("Source: https://datahub.io/core/global-temp\n",
                     "Visualization: Christian Burkhart")
  ) +
  theme(
    text = element_text(family = "Roboto"),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title = element_text(size = 20, 
                              face = "bold",
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 17, 
                                 margin = margin(b = 25)),
    plot.caption = element_text(size = 12,
                                margin = margin(t = 15)),
    panel.grid.major = element_line(color = "#DAE1E7"),
    panel.background = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title = element_text (size = 15),
    axis.line = element_line(),
    axis.title.y = element_text(margin = margin(r = 10),
                                hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10),
                                hjust = 0.5)
  )


ggplot(temperatures, aes(x = Year, y = Mean)) + 
  geom_ribbon(aes(ymax = Mean, ymin = 0), 
              fill = "#EB5286", alpha = 0.7) +
  geom_line(color = "#6F213F") +
  scale_y_continuous(expand = expand_scale(mult = 0)) +
  scale_x_continuous(expand = expand_scale(mult = 0)) +
  labs(
    title = "Global Average Temperature 1880 - 2016",
    subtitle = paste0("Compared to the average temperature ",
                      "from 1951 to 1980, the global temperature\n", 
                      "has risen significantly since 1980."),
    y = "Mean global temperatures",
    caption = paste0("Source: https://datahub.io/core/global-temp\n",
                     "Visualization: Christian Burkhart")
  ) +
  theme(
    text = element_text(family = "Roboto"),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title = element_text(size = 20, 
                              color = "#22292F",
                              face = "bold",
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 17, 
                                 margin = margin(b = 25)),
    plot.caption = element_text(size = 12,
                                margin = margin(t = 15),
                                color = "#606F7B"),
    panel.grid.major = element_line(color = "#DAE1E7"),
    panel.background = element_blank(),
    axis.text = element_text(size = 12, color = "#22292F"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title = element_text (size = 15),
    axis.line = element_line(color = "#3D4852"),
    axis.title.y = element_text(margin = margin(r = 10),
                                hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10),
                                hjust = 0.5)
  )

ggplot(temperatures, aes(x = Year, y = Mean)) + 
  annotate("segment", x = 1880, xend = 2016,
           y = 0, yend = 0,
           linetype = "dashed",
           color = "#6F213F") +
  annotate("segment", x = 1951, xend = 1951,
           y = -0.5, yend = 1.2,
           linetype = "dashed",
           color = "#451225") +
  annotate("segment", x = 1980, xend = 1980,
           y = -0.5, yend = 1.2,
           linetype = "dashed",
           color = "#451225") +
  annotate("rect",
           fill = "#FA7EA8", alpha = .1, 
           xmin = 1951, xmax = 1980, 
           ymin = -0.5, ymax = 1.2) +
  annotate("text", x = 1953, y = 0.89,
           hjust = 0, color = "#451225",
           size = 3.7,
           label = paste0("The U.S. National Weather\nService ",
                          "uses the three-decade\nperiod from 1951 to 1980\n",
                          "as a baseline value to\nmeasure the global ",
                          "average\ntemperature.")) +
  annotate("text", x = 1892, y = 0.15,
           hjust = 0, size = 3.7,
           label = paste0("The 0 line shows how much warmer or colder\n",
                          "the world was in a particular year ",
                          "compared to\nthe average temperature between ",
                          "1951 to 1980.")) +
  geom_ribbon(aes(ymax = Mean, ymin = 0), 
              fill = "#EB5286", alpha = 0.7) +
  theme(
    text = element_text(family = "Roboto"),
    plot.margin = unit(rep(1, 4), "cm"),
    plot.title = element_text(size = 20, 
                              color = "#22292F",
                              face = "bold",
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 17, 
                                 margin = margin(b = 25)),
    plot.caption = element_text(size = 12,
                                margin = margin(t = 15),
                                color = "#606F7B"),
    panel.grid.major = element_line(color = "#DAE1E7"),
    panel.background = element_blank(),
    axis.text = element_text(size = 12, color = "#22292F"),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.title = element_text (size = 15),
    axis.line = element_line(color = "#3D4852"),
    axis.title.y = element_text(margin = margin(r = 10),
                                hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10),
                                hjust = 0.5)
  )+
  labs(
    title = "Global Average Temperature 1880 - 2016",
    subtitle = paste0("Compared to the average temperature ",
                      "from 1951 to 1980, the global temperature\n", 
                      "has risen significantly since 1980."),
    y = "Mean global temperatures",
    caption = paste0("Source: https://datahub.io/core/global-temp\n",
                     "Visualization: Christian Burkhart")
  )



