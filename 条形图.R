library(vcd)
#基本
ggplot(data=ToothGrowth, mapping=aes(x=dose))+
  geom_bar(stat="count")

mytable <- with(Arthritis,table(Improved))
df <- as.data.frame(mytable)

ggplot(data=df, mapping=aes(x=Improved,y=Freq))+
  geom_bar(stat="identity")

#修改条形图的图形属性
ggplot(data=Arthritis, mapping=aes(x=Improved))+
  geom_bar(stat="count",width=0.5, color='red',fill='steelblue')

ggplot(data=Arthritis, mapping=aes(x=Improved))+
  geom_bar(stat="count",width=0.5, color='red',fill='steelblue')+
  geom_text(stat='count',aes(label= ..count..), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

#报错
ggplot(data=Arthritis, mapping=aes(x=Improved))+
  geom_bar(stat="count",width=0.5, color='red',fill='steelblue')+
  geom_text(stat='count',aes(label= count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

ggplot(data=Arthritis, mapping=aes(x=Improved,fill=Improved))+
  geom_bar(stat="count",width=0.5)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

#修改图例的位置
p <- ggplot(data=Arthritis, mapping=aes(x=Improved,fill=Improved))+
  geom_bar(stat="count",width=0.5)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

p + theme(legend.position="top")
p + theme(legend.position="bottom")
# Remove legend
p + theme(legend.position="none")

#修改条形图的顺序
p <- ggplot(data=Arthritis, mapping=aes(x=Improved,fill=Improved))+
  geom_bar(stat="count",width=0.5)+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  geom_text(stat='count',aes(label=..count..), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

p + scale_x_discrete(limits=c("Marked","Some", "None"))

#包含分组的条形图
#堆叠
ggplot(data=Arthritis, mapping=aes(x=Improved,fill=Sex))+
  geom_bar(stat="count",width=0.5,position='stack')+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  geom_text(stat='count',aes(label=..count..), color="white", size=3.5,position=position_stack(0.5))+
  theme_minimal()

#并行
y_max <- max(aggregate(ID~Improved+Sex,data=Arthritis,length)$ID)

ggplot(data=Arthritis, mapping=aes(x=Improved,fill=Sex))+
  geom_bar(stat="count",width=0.5,position='dodge')+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  ylim(0,y_max+5)+
  geom_text(stat='count',aes(label=..count..), color="black", size=3.5,position=position_dodge(0.5),vjust=-0.5)+
  theme_minimal()

#按照比例堆叠
ggplot(data=Arthritis, mapping=aes(x=Improved,fill=Sex))+
  geom_bar(stat="count",width=0.5,position='fill')+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  geom_text(stat='count',aes(label=..count..), color="white", size=3.5,position=position_fill(0.5))+
  theme_minimal()

#百分比
ggplot(data=Arthritis, mapping=aes(x=Improved,fill=Sex))+
  geom_bar(stat="count",width=0.5,position='fill')+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  geom_text(stat='count',aes(label=scales::percent(..count../sum(..count..)))
            , color="white", size=3.5,position=position_fill(0.5))+
  theme_minimal()

library("ggplot2")
library("dplyr")
library("scales")

#win.graph(width=6, height=5,pointsize=8)

#data
df <- data.frame(
  rate_cut=rep(c("0 Change", "0 - 10", "10 - 20", "20 - 30", "30 - 40","40 - 50", "50 - 60", "60 - 70","70 - 80", "80 - 90", "90 - 100", ">100"),2)
  ,freq=c(1,3,5,7,9,11,51,61,71,13,17,9,
          5,7,9,11,15,19,61,81,93,17,21,13)
  ,product=c(rep('ProductA',12),rep('ProductB',12))
)

#set order
labels_order <- c("0 Change", "0 - 10", "10 - 20", "20 - 30", "30 - 40","40 - 50", "50 - 60", "60 - 70","70 - 80", "80 - 90", "90 - 100", ">100")

#set plot text
plot_legend <- c("Product A", "Product B")
plot_title <- paste0("Increase % Distribution")
annotate_title <-"Top % Increase"
annotate_prefix_1 <-"Product A = "
annotate_prefix_2 <-"Product B = "

df_sum <- df %>% 
  group_by(product) %>%
  summarize(sumFreq=sum(freq))%>%
  ungroup()%>%
  select(product,sumFreq)

df <- merge(df,df_sum,by.x = 'product',by.y='product')
df <- within(df,{rate <- round(freq/sumFreq,digits=4)*100})
df <- subset(df,select=c(product,rate_cut,rate))

#set order
df$rate_cut <- factor(df$rate_cut,levels=labels_order,ordered = TRUE)
df <- df[order(df$product,df$rate_cut),]

#set position
annotate.y <- ceiling(max(round(df$rate,digits = 0))/4*2.5)
text.offset <- max(round(df$rate,digits = 0))/25

annotation <- df %>%
  mutate(indicator = ifelse(substr(rate_cut,1,2) %in% c("70","80","90",'>1'),'top','increase' )) %>%
  filter(indicator=='top') %>%
  dplyr::group_by(product) %>%
  dplyr::summarise(total = sum(rate)) %>%
  select(product, total)

mytheme <- theme_classic() + 
  theme(
    panel.background = element_blank(),
    strip.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "gray95"),
    axis.ticks = element_blank(),
    text = element_text(family = "sans"),
    axis.title = element_text(color = "gray30", size = 12),
    axis.text = element_text(size = 10, color = "gray30"),
    plot.title = element_text(size = 14, hjust = .5, color = "gray30"),
    strip.text = element_text(color = "gray30", size = 12),
    axis.line.y = element_line(size=1,linetype = 'dotted'),
    axis.line.x = element_blank(),
    axis.text.x = element_text(vjust = 0),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    legend.position = c(0.7, 0.9),
    legend.text = element_text(color = "gray30")
  )

##ggplot
ggplot(df,aes(x=rate_cut, y=rate)) + 
  geom_bar(stat = "identity", aes(fill = product), position = "dodge", width = 0.5) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#00188F","#00BCF2")
                    ,breaks = c("ProductA","ProductB")
                    ,labels = plot_legend
                    ,name = "") +
  geom_text(data = df
            , aes(label = comma(rate), y = rate +text.offset, color = product)
            ,position = position_dodge(width =1)
            , size = 3) + 
  scale_color_manual(values = c("#00BCF2", "#00188F"), guide = FALSE) +
  annotate("text", x = 3, y = annotate.y, hjust = 0, color = "gray30", label = annotate_title) + 
  annotate("text", x = 2.5, y = annotate.y, hjust = 0, color = "gray30", label = paste0(annotate_prefix_1, annotation$total[1])) + 
  annotate("text", x = 2, y = annotate.y, hjust = 0, color = "gray30", label = paste0(annotate_prefix_2, annotation$total[2])) + 
  labs(x="Increase Percentage",y="Percent of freq",title=plot_title) +
  mytheme + 
  coord_flip()

#添加标注
mpg %>% 
  group_by(class, drv) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(class, count))+
  geom_col(aes(fill = drv),
           position = position_dodge2(preserve = 'single'))+
  geom_text(aes(label = count),
            position = position_dodge2(width = 0.9,
                                       preserve = 'single'),
            vjust = -0.2,
            hjust = 0.5)

#堆叠的例子
mpg %>%
  group_by(class, drv) %>% 
  summarise(count = n()) %>% 
  mutate(cumcount = cumsum(count)) %>% 
  ggplot(aes(class, count))+
  geom_col(aes(fill = drv),
           position = position_stack(reverse = T))+
  geom_text(aes(label = cumcount),
            position = position_stack(),
            vjust = 0.5,
            hjust = 0.5)

mpg %>%
  group_by(class, drv) %>% 
  summarise(count = n()) %>% 
  mutate(cumcount = cumsum(count),
         midcount = cumcount - count/2) %>% 
  ggplot(aes(class, count))+
  geom_col(aes(fill = drv),
           position = position_stack(reverse = T))+
  geom_text(aes(y = midcount,
                label = cumcount),
            #position = position_stack(),
            hjust = 0.5)

df <- tibble(
  gene = factor(paste0("gene_",rep(1:16, 2)),
                levels = paste0("gene_", 16:1)),
  stat = c(seq(-10, -100, -10),
           seq(-90, -40, 10),
           seq(10, 100, 10),
           seq(90, 40, -10)),
  direct = rep(c("down", "up"), each = 16)
)

df

ggplot(df,
       aes(gene, stat,
           fill = direct))+
  geom_col()+
  coord_flip()+
  scale_y_continuous(breaks = seq(-100, 100, 20),
                     labels = c(seq(100, 0, -20),
                                seq(20, 100, 20)))

#偏差图
df <- tibble(
  gene = factor(paste0("gene_", rep(1:20)),
                levels = paste0("gene_", 20:1)),
  stat = c(seq(100, 10, -10),
           seq(-10, -100, -10)),
  direct = factor(rep(c("down", "up"), each = 10),
                  levels = c("up", "down"))
)

df

ggplot(df,
       aes(gene, stat,
           fill = direct))+
  geom_col()+
  coord_flip()
