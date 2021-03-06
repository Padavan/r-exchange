print("helloworld")

library(ggplot2)

<<<<<<< HEAD
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

=======
# dat <- read.csv('dataset1.csv', sep = '\t')
# 
# g <- ggplot(dat, aes(x = A, y = U)) +
#   geom_point()+
#   geom_line() +
#   scale_x_log10(breaks = c(5,10,15,20,25,30,35)) +
#   theme_bw()
# 
# plot(g)

dat2 <- read.csv('dataset2.csv', sep = '\t')
dat3 <- read.csv('dataset3.csv', sep = '\t')
dat3 <- as.data.frame(dat3)
colnames(dat3) <- c('lambda', 'U')

dat2$fac <- as.factor('two') #change 'two to the name you need for legend
dat3$fac <- as.factor('three')

datFinal <- rbind.data.frame(dat2, dat3) #final DF for plotting, values separated by factor 

# your nice loop
datFinalFixed <- datFinal

for (i in 1:length(datFinalFixed$U)) {
  if (datFinalFixed$U[i] < .003) {
    datFinalFixed$U[i] <- 0
  }
  else {
    datFinalFixed$U[i] <- datFinalFixed$U[i] - .003
  }
}

g <- ggplot(data = datFinal, aes(x = lambda, y = U)) + 
  geom_line(aes(colour = fac)) +
  geom_point(aes(colour = fac)) +
  scale_colour_discrete(name = 'Name your legend', l = 40) + 
  labs(list(x = 'x-axis descr.', y = 'y-axis descr')) +
  theme_bw()

plot(g)
>>>>>>> e70b95d4533d83065bf9ffaab8bb99280eddb859

