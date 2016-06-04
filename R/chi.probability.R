
#' @title Calculation the Chi Squared probability.
#' @export
#' @return returns the probability that the \code{Chi} is greater than \code{x} (area under the distribution which is greater than \code{x}).
#' @usage chi.probability(x, df = 5, n = 1000)
#' @keywords distribution chi Monte Carlo simulation Estimation
#' @description \code{chi.probability} computes the probability that the \code{Chi} of a given degree of freedom is greater than a specified value \code{x}.
#' If \code{x} is a vector, the function will return a vector of probabilities of the components of \code{x}.
#' @param x specifies the value that should be evaluated in the Chi Square distribution.
#' @param df specifies degrees of freedom in the Chi Square distribution.
#' @param n specifies number of random numbers for evaluating the distribution.
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' chi.probability(x= 5, df = 5, n = 1000)


chi.probability <- function(x, df = 5, n = 1000){

  #------------------------------------Error Handler:--------------------------------------#
  # ---------------------------------------------------------------------------------------#
  # 'n' and 'df' must be numeric, natural number greater than 1.                           #
  # 'x' must be a numerical vector.                                                        #
  #----------------------------------------------------------------------------------------#
  if(is.matrix(n) || is.list(n) || length(n)!=1){ #Check that n is a single value
    stop("'n' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(n)){ #checks if n is a number
    stop("'n' has to be a positive integer")
  } else if(n < 1 || n != round(n)){ #checks if n is a natural number
    stop("'n' has to be a positive integer")
  }
  if(is.matrix(df) || is.list(df) || length(df)!=1){ #Check that df is a single value
    stop("'df' has to be an integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(df)){ #checks if df is a number
    stop("'df' has to be a positive integer")
  } else if(df < 1){ #checks if df is a positive integer
    stop("'df' has to be a positive integer")
  }
  if(is.matrix(x) || is.list(x) || !is.numeric(x)){ #Check that x is a vector of numbers(can be any length)
    stop("'x' has to be an integer; non-list, non-matrix")
  }
  #---------------------------------------End Error Handler---------------------------------#
  lx <- length(x) #Save the length of x for later

  storage <- matrix(rep(0,lx*n), nrow = lx) #Generate a matrix so rows are used for multiple different chi probabilities(if x is longer than 1. Defensive)
  Pr <- rep(0,length(x)) #Create Pr to save results in.

  for(k in 1:lx){ #For every different x, we create values for Pr.
    for(i in 1:n){ #We want n numbers for each different x.
      storage[k,i]<- sum(rnorm(df)^2) #Chi values
    }
  }

  for(j in 1: lx){ #For every different x, we create a different P-value
    Pr[j] <- length(storage[j,][storage[j,] > x[j]])/n #We get P(chi^2 > x)
  }
  return(Pr) #Returns the p-value
}
