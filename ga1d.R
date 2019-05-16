library(GA)
f <- function(x)  (x^2+x)*cos(x)
lbound <- -10; ubound <- 10
curve(f, from = lbound, to = ubound, n = 1000)

GA <- ga(type = "real-valued"
         , fitness = f
         , lower = c(th = lbound)
         , upper = ubound
         , maxiter = 20
         , popSize = 10)
summary(GA)
GA@solution

points(x = GA@solution, y = f(GA@solution)
       , pch = 19, col = "red")
plot(GA)


defaultControl<- gaControl()
sumbin <- function(x) sum(x)
sqsumbin <- function(x) sum(x)^2
D <- 8 # dimension
set.seed(1)
GA <- ga(type = "binary"
         , fitness = sqsumbin
         , nBits = D
         , maxiter = 2000
         , popSize = 1000
         , pmutation = 0.01
         , pcrossover = 0.8
         , elitism = 100
         , selection = "gabin_lrSelection")
plot(GA)
