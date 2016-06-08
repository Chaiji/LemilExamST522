
#' @title Beta Simulator
#' @export
#' @return Returns a vector of numbers to be Beta(a,b) distribution, using the Metropolis-Hastings sampler.
#' @usage Beta.sim(n=1337,a=2.5,b=5.5,initVal=0.5)
#' @keywords Beta uniform metropolis hastings sampler distribution simulation iteration
#' @description This function takes 0 to 4 arguments to use the Metropolis-Hastings sampler to generate Beta distributions.
#' If no arguments are used, 1337 iterations are used to generate a Beta(2.5,5.5) distribution using uniform(0,1) distribution.
#' The function also always prints the rejection rate.
#' @param n is a natural number that chooses the number of iterations for the simulation.
#' @param a is a non-negative number that chooses the first parameter of the Beta distribution.
#' @param b is a non-negative number that chooses the second parameter of the Beta distribution.
#' @param initVal is a single number between 0 and 1 that chooses the starting value for the simulation.
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' Beta.sim()
#' Beta.sim(n=200)

Beta.sim <- function(n = 1337, a = 2.5, b = 5.5, initVal = 0.5){
  #------------------------------------Error Handler:--------------------------------------#
  # ---------------------------------------------------------------------------------------#
  # 'n' must be numeric, natural number greater than 1.                                    #
  # 'x, y' must be numerical vectors.                                                      #
  # 'plot' must be a boolean valued.                                                       #
  #----------------------------------------------------------------------------------------#
  
  if(is.matrix(n) || is.list(n) || length(n)!=1){ #Check that n is a single value
    stop("'n' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(n)){ #checks if n is a number
    stop("'n' has to be a positive integer")
  } else if(n < 1 || n != round(n)){ #checks if n is a natural number
    stop("'n' has to be a positive integer")
  }
  if(is.matrix(a) || is.list(a) || length(a)!=1){ #Check that a is a single value
    stop("'a' has to be a positive number; length=1, non-list, non-matrix")
  } else if(!is.numeric(a)){ #checks if a is a number
    stop("'a' has to be a positive number")
  } else if(a < 0){ #checks if a is a positive number
    stop("'a' has to be a positive number")
  }
  if(is.matrix(b) || is.list(b) || length(b)!=1){ #Check that b is a single value
    stop("'b' has to be a positive number; length=1, non-list, non-matrix")
  } else if(!is.numeric(b)){ #checks if b is a number
    stop("'b' has to be a positive number")
  } else if(b < 0){ #checks if b is a positive number
    stop("'b' has to be a positive number")
  }
  if(is.matrix(initVal) || is.list(initVal) || length(initVal)!=1){ #Check that initVal is a single value
    stop("'initVal' has to be a number between 0 and 1; length=1, non-list, non-matrix")
  } else if(!is.numeric(initVal)){ #checks if initVal is a number
    stop("'initVal' has to be a number between 0 and 1")
  } else if(initVal <= 0 || initVal >= 1){ #checks if initVal is a natural number
    stop("'initVal' has to be a number between 0 and 1")
  }
  #---------------------------------------End Error Handler---------------------------------#
  B <- c(initVal) # start value.
  rate = 0        # rejection counter
  va <- matrix(runif((n-1)*2),nrow=n-1,ncol=2) #We need (n-1)*2 random variables
  Y <- va[,1] #To make it easier to read, we split them to Y and P
  P <- va[,2]
  for(i in 2:n){
    test <- min(dbeta(Y[i-1],a,b)/dbeta(B[i-1],a,b),1)  # Acceptance probability
    B[i] <- if(P[i-1] < test){Y[i-1] }else{ B[i-1] }
    rate = rate + (P[i-1] > test)
  }
  print(paste("The rejection rate is",rate/n))
  return(B)
}