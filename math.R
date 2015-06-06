library(ggplot2)

#Data
n_n <- 1E16 #m^-3
p_p <- 5E17 #m^-3
N_d <- n_n
N_a <- p_p

##Constants
k_eV <- 8.6E-05 #eV/K
k_B <- 1.38E-23 #J/K
temperature <- 300 #K
epsilon_0 <- 8.85E-12 #1
#epsilon_0 <- 1
epsilon <- 12.9 ##диэлектрическая проницаемость статическая — 12,9
e <- 1.6E-19 ##Кл
hbar <- 6.6225E-34

E_g=1.42 #eV
#Подвижность
nu_n <- 0.85 # m^2/V*s
nu_p <- 0.04 #  m^2/V*s
#Mass
m <- 9.1E-31 #kg
m_n <- 0.067*m
m_p <- 0.082*m
#Время жизни носителей
tau_n <- 1E-8 #s
tau_p <- 1E-8 #s
tau_0 <- tau_n

##Part I


N_c <- 2*((2*pi*m_n*k_B*temperature)/hbar**2)**(3/2)
N_v <- 2*((2*pi*m_p*k_B*temperature)/hbar**2)**(3/2)

n_0 <- sqrt(N_c*N_v)*exp(-E_g/(2*k_eV*temperature))

phi_0 <- k_B*temperature*log(n_n*p_p/n_0^2)
phi_0V <- phi_0/e

E_fv <- k_ev*temperature*log(N_v/N_a)
E_fc <- k_ev*temperature*log(N_c/N_d)


ksi_sum <- sqrt(2*epsilon*epsilon_0*phi_0*(n_n+p_p)/(n_n*p_p*e^2))

phi_n <-phi_0*p_p/(n_n+p_p)

phi_p <-phi_0*n_n/(n_n+p_p)

ksi_n <- ksi_sum/(1+phi_p/phi_n)
ksi_p <- ksi_sum/(1+phi_n/phi_p)

#PART 2 - volt ampere characteristics

D_p <- nu_p*k_B*temperature/e
D_n <- nu_n*k_B*temperature/e

L_n <- sqrt(tau_n*D_n)
L_p <- sqrt(tau_p*D_p)

n_p=n_0**2/p_p
p_n=n_0**2/n_n



fun_vac1 <- function(x) {e*(D_n*n_p/L_n + D_p*p_n/L_p)*(exp(e*x/(k_B*temperature))-1)}
qplot(c(-0.5, 0.5), stat = "function", fun = fun_vac1, geom = "line")

#PART 3 - volt pharade characteristics

S <- 1E-06 #cm
U_k <- phi_0/e
fun_vac2 <- function(x) {(N_a+N_d)/(e*epsilon*epsilon_0*N_a*N_d*S^2)*(U_k-x)}
qplot(c(-2, 2), stat = "function", fun = fun_vac2, geom = "line")

#PART 4 - recombination currents
fun_j_r1 <- function(x) {n_0*k_B*temperature/(tau_0*sqrt(phi_0-e*x))*
    sqrt((2*e*epsilon*epsilon_0*(N_a+N_d))/(N_a*N_d))*
    (exp(e*x/(k_B*temperature))-1)/(exp(e*x/(2*k_B*temperature))+1)}
qplot(c(0, 1.5), stat = "function", fun = fun_j_r1, geom = "line")

fun_j_r2 <- function(x) {n_0/(2*tau_0)*
    sqrt((2*e*epsilon*epsilon_0*(N_a+N_d))/((N_a*N_d)))*
    sqrt(phi_0-e*x)}
qplot(c(-1.5, 1.5), stat = "function", fun = fun_j_r2, geom = "line")

#PART 5 - high injection 


#conditions of high injection
k_eV*temperature*log((n_n/n_0)**2+1)
k_eV*temperature*log((p_p/n_0)**2+1)

fun_j_r3 <- function(x) {2*e*(D_p*n_0)/(L_p)*(exp(e*x/(2*k_B*temperature))-p_n/n_0)}
qplot(c(-1, 0.5), stat = "function", fun = fun_j_r3, geom = "line")

fun_j_r4 <- function(x) {2*e*(D_n*n_0)/(L_n)*(exp(e*x/(2*k_B*temperature))-n_p/n_0)}
qplot(c(0, 0.5), stat = "function", fun = fun_j_r4, geom = "line")

fun_j_r5 <- function(x) {2*e*n_0*(D_p/L_p-D_n/L_n)*exp(e*x/(3*k_B*temperature))}
qplot(c(0, 0.5), stat = "function", fun = fun_j_r5, geom = "line")

#PART 6 -  


plot.range1 <- data.frame(x=c(0, 0.5), Functions = factor(1))
plot.range2 <- data.frame(x=c(0.5, 0.71), Functions = factor(2))
plot.range3 <- data.frame(x=c(0.71, 0.73),   Functions = factor(3))

xint=0.5
xint2=0.7
df <- data.frame(xint,xint2)
g <- ggplot(NULL, aes(x=x, colour=Functions)) +
  stat_function(data = plot.range1, fun = fun_vac1) +
  stat_function(data = plot.range2, fun = fun_j_r4 ) +
  stat_function(data = plot.range3, fun = fun_j_r5 ) +
  #scale_colour_manual(values = c("red", "green"), labels = c("log(1+x)", "x")) +
  geom_vline(data=df, xintercept=xint, linetype=4, colour="black") +
  geom_vline(data=df, xintercept=xint2, linetype=4, colour="black") +
  theme(axis.title.y=element_blank())
plot(g)
