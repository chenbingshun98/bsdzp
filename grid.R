library(grid)
grid.text(label = "Let's us begin!")

grid.circle(
  x = seq(0.1, 0.9, length = 100),
  y = 0.5 + 0.4 * sin(seq(0, 2 * pi, length = 100)),
  r = abs(0.1 * cos(seq(0, 2 * pi, length = 100)))
)

unit(1, "cm")
unit(1:4, "mm")

unit(1:4, c('npc', 'mm', 'native', 'lines'))

unit(1:4,"mm")[1] - unit(1:4, "mm")[4]

unit(1, "npc") - unit(1:4, "mm")
max(unit(1:4, c('npc', 'mm', 'native', 'lines')))

#对于字符串及对象长度坐标系统
unit(1, "strwidth", "some text")

unit(1, "grobwidth", textGrob("some text"))
