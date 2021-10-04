#text
library(tidyverse)
df <- data.frame(x = 1, y = 3:1, family = c("sans", "serif", "mono"))
ggplot(df, aes(x, y)) + 
  geom_text(aes(label = family, family = family))

df <- data.frame(trt = c("a", "b", "c"), resp = c(1.2, 3.4, 2.5))
ggplot(df, aes(resp, trt)) + 
  geom_point() + 
  geom_text(aes(label = paste0("(", resp, ")")), nudge_y = -0.25) + 
  xlim(1, 3.6)

ggplot(mpg, aes(displ, hwy)) + 
  geom_text(aes(label = model)) + 
  xlim(1, 8)
ggplot(mpg, aes(displ, hwy)) + 
  geom_text(aes(label = model), check_overlap = TRUE) + 
  xlim(1, 8)

label <- data.frame(
  waiting = c(55, 80), 
  eruptions = c(2, 4.3), 
  label = c("peak one", "peak two")
)

ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_tile(aes(fill = density)) + 
  geom_label(data = label, aes(label = label))

mini_mpg <- mpg[sample(nrow(mpg), 20),]
ggplot(mpg, aes(displ, hwy)) + geom_point(colour = "red") + 
  ggrepel::geom_text_repel(data = mini_mpg, aes(label = class))

presidential <- subset(presidential, start > economics$date[1])

presidential %>% filter(start>economics$date[1])

ggplot(economics) + 
  geom_rect(
    aes(xmin = start, xmax = end, fill = party), 
    ymin = -Inf, ymax = Inf, alpha = 0.2, 
    data = presidential
  ) + 
  geom_vline(
    aes(xintercept = as.numeric(start)), 
    data = presidential,
    colour = "grey50", alpha = 0.5
  ) + 
  geom_text(
    aes(x = start, y = 2500, label = name), 
    data = presidential, 
    size = 3, vjust = 0, hjust = 0, nudge_x = 50
  ) + 
  geom_line(aes(date, unemploy)) + 
  scale_fill_manual(values = c("blue", "red")) +
  xlab("date") + 
  ylab("unemployment")
#> Warning: ggrepel: 13 unlabeled data points (too many overlaps). Consider
#> increasing max.overlaps

yrng <- range(economics$unemploy)
xrng <- range(economics$date)
caption <- paste(strwrap("Unemployment rates in the US have 
  varied a lot over the years", 40), collapse = "\n")

ggplot(economics, aes(date, unemploy)) + 
  geom_line() + 
  geom_text(
    aes(x, y, label = caption), 
    data = data.frame(x = xrng[1], y = yrng[2], caption = caption), 
    hjust = 0, vjust = 1, size = 4
  )
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(
    data = filter(mpg, manufacturer == "subaru"), 
    colour = "orange",
    size = 3
  ) +
  geom_point() 
p

p + 
  annotate(
    geom = "curve", x = 4, y = 35, xend = 2.65, yend = 27, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 4.1, y = 35, label = "subaru", hjust = "left")


library(showtext)
## Add fonts that are available on Windows
font_add("heiti", "simhei.ttf")
font_add("constan", "constan.ttf", italic = "constani.ttf")
font_add("SimSun",regular = "simsun.ttc")

library(ggplot2)
p = ggplot(NULL, aes(x = 1, y = 1)) + ylim(0.8, 1.2) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank()) +
  annotate("text", 1, 1.1, family = "SimSun", size = 15,
           label = "\u4F60\u597D\uFF0C\u4E16\u754C") +
  annotate("text", 1, 0.9, label = 'Chinese for "Hello, world!"',
           family = "constan", fontface = "italic", size = 12)

## Automatically use showtext for new devices
showtext_auto()

## On-screen device
x11()
print(p)
dev.off()

## PDF device
pdf("showtext-example-3.pdf", 7, 4)
print(p)
dev.off()

## PNG device
ggsave("showtext-example-4.png", width = 7, height = 4, dpi = 96)

## Turn off if no longer needed
showtext_auto(FALSE)
