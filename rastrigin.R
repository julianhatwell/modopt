library(lattice)
library(latticeExtra)

# define the objective function in 1D
d1 = function(x){
  return(x^2 - 10*(cos(2*pi*x)) + 10)
}

rastrig <- function(vec) {
  # if(class(vec) != "numeric" || any(abs(vec) > 5.12)) {
  #   stop("Search domain is numeric -5.12 <= x <= 5.12")
  # }
  sum(sapply(vec, d1))
}

gradient <- function(vec) {
  part_deriv <- function(x) {
    2 * x + 10 * (sin(2 * pi * x) * (2 * pi))
  }
  part_deriv_out <- sapply(vec, part_deriv)
  return(part_deriv_out)
}

objFun <- function(vec) {
  sapply(vec, rastrig)
}

#Plot the objective function
plot_obj <- function(title, from=0, to=4){
  #Plot the objective function
  curve(objFun(x), from=from, to=to, main=title)
  #Plot crossing lines so they intersect at the minimum
  abline(v = 0, col = "red")
}

plot_iterations <- function(x){
  points(x, objFun(x), type="b", col="green")
}

# gradient descent method to find the minimum
gdescent_dims <- function(x, alpha, epsilon, iter){  
  # create a vector to contain all xs for all the steps
  x.All = matrix(NA, nrow = iter, ncol = length(x))
  x.All[1, ] = x
  
  g = rep(1, length(x))
  i = 1
  while(i < iter && abs(g) > epsilon){
    g = gradient(x)
    #To check the gradients developing. Comment it if you do not wish to see this ouput.  
    print(paste("solution = ",paste(x, collapse = ","), "; gradient =", g))
  
    i = i + 1
    x = x - alpha * g
    x.All[i,] = x
  }
  
  # print result and plot all xs for every iteration
  print(paste("The minimum of f(x) = ", objFun(x), " at position x = ", paste(x, collapse = ","), "; found in ",i-1," iterations.",sep = ""))
  return (x.All[1:(i-1),])
}

search_domain <- seq(-5.12, 5.12, length.out = 100)
grd <- expand.grid(x1 = search_domain
                   , x2 = search_domain)
grd$y <- sapply(1:nrow(grd)
                , function(n) {
                  rastrig(as.vector(t(grd[n,1:2])))
                })

gd_params <- list(c(1, 2), 0.001
                  , 10E-13
                  , 1000
                  , -5.12, 5.12)
names(gd_params) <- c("x", "alpha", "epsilon", "iter", "from", "to")

x.All <- gdescent_dims(gd_params[["x"]]
                  , gd_params[["alpha"]]
                  , gd_params[["epsilon"]]
                  , gd_params[["iter"]])

x.All <- as.data.frame(x.All)
names(x.All) <- c("x1", "x2")

levelplot(y~x1*x2, data=grd) +
  as.layer(xyplot(x2~x1, data=x.All
                  , type="b"))


