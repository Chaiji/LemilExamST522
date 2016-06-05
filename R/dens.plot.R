
#' @title Density Plotter
#' @export
#' @return Returns a density plot over the input data x, with information calculated depending on other inputs, or lack thereof.
#' @usage dens.plot(x,n=500,method="naive",from,to)
#' @keywords estimate density silverman struges sturges bandwidth data plot naive kernel gaussian
#' @description This function takes between 1 and 5 arguments to plot the density of data, using the dens.estimator function.
#' @param x is data given as a vector of numbers, to estimate density from.
#' @param n is a positive integer that specifies the number of points to be used for plotting the density function. Defaults to 500.
#' @param method is an argument that is either "naive" or "kernel", deciding the method of estimating the density. Defaults to "naive".
#' @param from is a number that specifies where the plot should start. Defaults to min(x)-sd(x)/3
#' @param to is a number that specifies where the plot should end. Defaults to max(x)+sd(x)/3
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' dens.plot(cars$speed)
#' dens.plot(cars$speed,method="kernel")
#' dens.plot(cars$speed,n=512,method="naive", from=10, to=20)

dens.plot <- function(x,n=500,method="naive",from,to){
  #------------------------------------Error Handler:--------------------------------------------#
  # ---------------------------------------------------------------------------------------------#
  # x is a vector of numbers                                                                     #
  # n is the number of points to be used for plotting the density function. default is 500       #
  # method is a string that either contains "naive" or "kernel", defaults to "naive"             #
  # from is a number for where to start plot. If none chosen, calculates based on sd(x)          #
  # to is a number for where to end plot. If none chosen, calculates based on sd(x)              #
  #----------------------------------------------------------------------------------------------#
  if(is.list(x) || !is.numeric(x) || is.matrix(x) || !is.vector(x)   ){
    stop(" The given data 'x' must be a numerical vector.")
  }
  if(is.matrix(n) || is.list(n) || length(n)!=1){ #Check that n is a single value
    stop("'n' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(n)){ #checks if n is a number
    stop("'n' has to be a positive integer")
  } else if(n < 1 || n != round(n)){ #checks if n is a natural number
    stop("'n' has to be a positive integer")
  }
  if(method != "naive" && method != "kernel"){
    stop("The given argument 'method' has to be either 'naive' or 'kernel'")
  }
  if(!missing(from)){
    if(is.matrix(from) || is.list(from) || length(from)!=1){ #Check that n is a single value
      stop("'from' has to be a number; length=1, non-list, non-matrix")
    } else if(!is.numeric(n)){ #checks if n is a number
      stop("'from' has to be a number")
    }
  }
  if(!missing(to)){
    if(is.matrix(to) || is.list(to) || length(to)!=1){ #Check that n is a single value
      stop("'to' has to be a number; length=1, non-list, non-matrix")
    } else if(!is.numeric(to)){ #checks if n is a number
      stop("'to' has to be a number")
    }
  }
  #---------------------------------------End Error Handler--------------------------------------#
  if(missing(from)){ #From is chosen as minimum of x, minus standard deviation/3
    from <- min(x) - sd(x)/3
  }
  if(missing(to)){ #To is chosen as maxmimum of x, plus standard deviation/3
    to <- max(x) + sd(x)/3
  }
  if(from > to){ #If somehow from is greater than to, flip them. Defensive measure!
    temp <- to
    to <- from
    from <- temp
  }
  intervalX <- seq(from,to,(diff(c(from,to))/n))
  if(method=="naive"){
    intervalY <- dens.estimator(x,d=intervalX,method="naive")
    plot(intervalX,intervalY, main="Naive estimator", xlab="points", ylab="density", type="l")
  } else if(method=="kernel"){
    intervalY <- dens.estimator(x,d=intervalX,method="kernel")
    plot(intervalX,intervalY, main="Gaussian estimator", xlab="points", ylab="density", type="l")
  }
}
