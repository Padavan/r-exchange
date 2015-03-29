print("helloworld")

#require(ggplot2)
library(ggplot2)

dat <- read.csv('dataset1.csv', sep = '\t')

g <- ggplot(dat, aes(x = A, y = U)) +
  geom_point()+
  geom_line() +
  scale_x_log10(breaks = c(5,10,15,20,25,30,35)) +
  theme_bw()

plot(g)
