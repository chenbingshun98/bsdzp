data("mtcars")  # load data

mpg %>% 
  group_by(manufacturer) %>% 
  summarise(Mileage = mean(cty)) %>% 
  mutate(Make = fct_reorder(manufacturer, Mileage)) %>% 
  ggplot(aes(x = Make, y = Mileage))+
  geom_point(size = 4, color = "tomato3")+
  geom_segment(aes(x = Make,
                   xend = Make,
                   y = 0,
                   yend = Mileage))+
  geom_text(color = "purple",
            size = 4,
            vjust = -0.8,
            aes(label = sprintf("%0.1f",
                                round(Mileage,
                                      digits = 2))))+
  labs(title = "Lollopop Chart",
       subtitle = "Make Vs Avg.Mileage")+
  ylim(0,27)+
  theme(axis.title.x = element_text(angle = 65,
                                    vjust = 0.7,
                                    color = "tomato3"))+
  coord_flip()+theme_classic()
        

#Dumbbell Plot
install.packages("ggalt")
library(ggalt)

health <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/health.csv")
health$Area <- factor(health$Area, levels=as.character(health$Area))
ggplot(health, aes(x=pct_2013, xend=pct_2014, y=Area)) + 
  geom_dumbbell()

#decoration
library(showtext)
showtext_auto()
ggplot(health,
       aes(x = pct_2013,
           xend = pct_2014,
           y = Area))+
  geom_segment(aes(x = pct_2013,
                   xend = pct_2014,
                   y = Area,
                   yend = Area),
               color = "#b2b2b2", size = 1.5)+
  geom_dumbbell(color = "light blue",
                size_x = 3.5,
                size_xend = 3.5,
                colour_x = "#edae52",
                colour_xend = "#9fb059")+
  labs(x = NULL, y = NULL,
       title = "Dumbbell Chart",
       subtitle = "Pct Change: 2013 vs 2014")+
  geom_text(color = "black", size = 2, hjust = -0.5,
            aes(x = pct_2013, label = pct_2013))+
  geom_text(aes(x = pct_2014, label = pct_2014),
            color = "black", size = 2, hjust = 1.5)

#treemap
library(treemapify)  #for geom_treemap()

#Read formatted data from web
proglangs <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/proglanguages.csv")

ggplot(proglangs, aes(area=value, fill=parent, subgroup=parent)) + 
  geom_treemap()

#decoration
ggplot(proglangs, aes(area=value, fill=parent, subgroup=parent)) + 
  geom_treemap()+
  #main group bordering
  geom_treemap_subgroup_border()+
  #subgroup heading in white
  geom_treemap_subgroup_text(color="white")+
  #all other group text in black
  geom_treemap_text(aes(label=id), color="black")+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Dark2")

#nnormal distribution1
all_mean <- 100
all_sd <- 30
my_score <- 157

dd <- rnorm(n=100000, mean=all_mean, sd=all_sd)
z <- (my_score - all_mean)/all_sd
pc <- round(100*(pnorm(z)), digits=0)
t1 <- paste0(as.character(pc),"th percentile")

p33 <- all_mean + (qnorm(0.3333) * all_sd)
p67 <- all_mean + (qnorm(0.6667) * all_sd)

ggplot() +
  geom_density(aes(dd)) +
  geom_vline(aes(xintercept=my_score), colour="red") +
  geom_label(aes(x=my_score, y=0.001, label=t1)) +
  geom_label(aes(x=my_score, y=0.01, label=as.character(my_score))) +
  
  geom_segment(aes(x=0,   y=0.014, xend=p33, yend=0.014), colour="blue") +
  geom_segment(aes(x=p33, y=0.014, xend=p67, yend=0.014), colour="orange") +
  geom_segment(aes(x=p67, y=0.014, xend=200, yend=0.014), colour="darkgreen") +
  
  geom_text(aes(x=p33/2, y=0.0145, label="Novice"), colour="blue") +
  geom_text(aes(x=all_mean, y=0.0145, label="Intermediate"), colour="orange") +
  geom_text(aes(x=200-(200-p67)/2, y=0.0145, label="Advanced"), colour="darkgreen") +
  ylim(0, 0.015) +
  labs(x="Score", y="Frequency") +
  theme(legend.position='none')

#normal distribution2
library(ggplot2)
all_mean <- 100
all_sd <- 30
my_score <- 157

dd <- function(x) { dnorm(x, mean=all_mean, sd=all_sd) }
z <- (my_score - all_mean)/all_sd
pc <- round(100*(pnorm(z)), digits=0)
t1 <- paste0(as.character(pc),"th percentile")

p33 <- all_mean + (qnorm(0.3333) * all_sd)
p67 <- all_mean + (qnorm(0.6667) * all_sd)

ggplot(data.frame(x=c(0, 180)), aes(x=x)) +
  stat_function(fun=dd) +
  geom_vline(aes(xintercept=my_score), colour="red") +
  geom_label(aes(x=my_score, y=0.001, label=t1)) +
  geom_label(aes(x=my_score, y=0.01, label=as.character(my_score))) +
  
  geom_segment(aes(x=0,   y=0.014, xend=p33, yend=0.014), colour="blue") +
  geom_segment(aes(x=p33, y=0.014, xend=p67, yend=0.014), colour="orange") +
  geom_segment(aes(x=p67, y=0.014, xend=200, yend=0.014), colour="darkgreen") +
  
  geom_text(aes(x=p33/2, y=0.0145, label="Novice"), colour="blue") +
  geom_text(aes(x=all_mean, y=0.0145, label="Intermediate"), colour="orange") +
  geom_text(aes(x=200-(200-p67)/2, y=0.0145, label="Advanced"), colour="darkgreen") +
  ylim(0, 0.015) +
  labs(x="Score", y="Frequency") +
  theme(legend.position='none')+
  ggthemes::theme_wsj()
