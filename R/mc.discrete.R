mc.discrete <- function(p,k,n){
  #------------------------------------Error Handler:--------------------------------------------#
  # ---------------------------------------------------------------------------------------------#
  # x is a vector of numbers                                                                     #
  # n is the number of points to be used for plotting the density function. default is 500       #
  # method is a string that either contains "naive" or "kernel", defaults to "naive"             #
  # from is a number for where to start plot. If none chosen, calculates based on sd(x)          #
  # to is a number for where to end plot. If none chosen, calculates based on sd(x)              #
  #----------------------------------------------------------------------------------------------#
  #p is a matrix with ncol=nrow
  #k is a single number. it has to be less than p(nrow)
  #n is a single number.
  #---------------------------------------End Error Handler--------------------------------------#
  width <- dim(p)[2]
  road <- rep(0,n)
  for(i in 1:n){
    point <- FALSE
    j <- 1
    a <- runif(1)
    while(point == FALSE){
      if(a <= p[k,j]){
        point = TRUE
        road[i] = j
        k = j
      } else{
        a <- a - p[k,j]
        j <- j + 1
      }
    }
  }
  stationary <- rep(0,width)
  for(i in 1:width){
    stationary[i] = sum(road == i)/n
  }
  return(stationary)
}

p <- matrix(c(0.2,0.3,0,0,0,0.4,0.7,0,0.5,0,0,0,0,0.7,0,0,0,0,0,0,0.5,0.9,0.25,0,0,0,0,0.1,0.5,0.4,0.1,0,0,0,0.25,0.2),nrow=6,ncol=6)
k <- 3
n <- 10000
