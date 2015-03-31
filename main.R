print("helloworld")

library(ggplot2)

dat <- read.csv('dataset1.csv', sep = '\t')
datFixed <- dat
datFixed$U <- dat$U - .003
g <- ggplot(datFixed, aes(x = A, y = U)) +
  geom_point()+
  geom_line() +
  scale_x_log10(breaks = c(5,10,15,20,25,30,35))
  #theme_bw()
plot(g)
ggsave(file="laser-curve.png")


