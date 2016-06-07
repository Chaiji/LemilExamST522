
#' @title Density Estimator
#' @export
#' @return Depending on input, either returns density on specific points d, or returns a bandwidth as well as a table of densities estimated
#' at points on data given as: minimum, 1st quantile, median, mean, 3rd quantile, and maximum of x.
#' @usage dens.estimator(x,d,h,method="naive")
#' @keywords estimate density silverman struges sturges bandwidth data kernel gaussian naive
#' @description This function takes between 1 and 4 arguments to estimate density of data.
#' @param x is data given as a vector of numbers, to estimate density from.
#' @param d is a number or a vector of numbers, that specifies what points density should be returned for. If nothing is chosen, a table of
#' relevant values are given: Min, 1st quantile, median, mean, 3rd quantile and max of the input data x.
#' @param h is a positive number that specifies the bandwidth used for the density estimation. If nothing is chosen, h is estimated
#' using either Struges method or Silverman's method, depending on the method parameter.
#' @param method is an argument that is either "naive" or "kernel", deciding the method of estimating the density. Defaults to "naive".
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' dens.estimator(cars$speed)
#' dens.estimator(cars$speed,method="kernel")
#' dens.estimator(cars$speed,10,3,method="naive")

dens.estimator <- function(x,d,h,method="naive"){
  #------------------------------------Error Handler:--------------------------------------------#
  # ---------------------------------------------------------------------------------------------#
  # x is a vector of numbers                                                                     #
  # d is a numeric value, can be a vector                                                        #
  # h is a positive numeric value, if no input, use Struges or Silvermans depending on method    #
  # method is a string that either contains "naive" or "kernel", defaults to "naive"             #
  #----------------------------------------------------------------------------------------------#
  if(is.list(x) || !is.numeric(x) || is.matrix(x) || !is.vector(x)   ){
    stop(" The given data 'x' must be a numerical vector.")
  }
  if(!missing(d)){
    if(is.list(d) || !is.numeric(d) || is.matrix(d) || !is.vector(d)   ){
      stop(" The given point(s) 'd' must be a number or a vector of numbers.")
    }
  }
  if(!missing(h)){
    if(is.list(h) || !is.numeric(h) || is.matrix(h) || length(h) != 1 || h < 0){
      stop(" The given bandwidth 'h' must be a single positive number.")
    }
  }
  if(method != "naive" && method != "kernel"){
    stop("The given argument 'method' has to be either 'naive' or 'kernel'")
  }
  #---------------------------------------End Error Handler--------------------------------------#
  if(missing(d)){
    d <- c(min(x),quantile(x,0.25,names=FALSE),median(x),mean(x),quantile(x,0.75,names=FALSE),max(x))
    tableplease <- TRUE #If user did not choose d, we use a table. This boolean value decides that.
  } else{
    tableplease <- FALSE #If user chooses d, output is exactly the result of evaluating d.
  }
  n <- length(x) #n is the amount of values in x. This information is needed regardless of estimator
  f <- rep(0,length(d))
  if(method == "naive"){ #Naive estimator
    if(missing(h)){ #If h not given, but method is naive.
      h <- diff(range(x)/(1+log2(n))) #h is chosen based on Sturges method of choosing bandwidth.
    }
    w <- matrix(rep(0,length(x)*length(d)),nrow=length(d),ncol=length(x)) #Allows calculating multiple values for d simultaneously using a matrix
    for (j in 1:length(d)){ #We need to check a row of values for every value in d.
      for(i in 1:length(x)){ #We need to check every x.
        if(abs((d[j] - x[i])/h) < 1){ #Easier to calculate each w beforehand and then sum them later
          w[j,i] <- 1/2
        }
      }
    }
    for(i in 1:length(d)){
      f[i] <- (1/(n*h)) * sum(w[i,]) #f are calculated
    }
  } else if(method =="kernel"){
    if(missing(h)){ #If h not given, but method is kernel
      h <- ((4*sd(x)^5)/(3*n))^(1/5) #h is chosen based on Silvermans suggestion for choosing bandwidth for Gaussian functions.
    }
    K <- matrix(rep(0,length(x)*length(d)),nrow=length(d),ncol=length(x))
    for(j in 1:length(d)){
      for(i in 1:length(x)){ #Easier to calculate each K beforehand and then sum them later
        u <- (d[j] - x[i])/h
        K[j,i] <- (1/(sqrt(2*pi)))*exp(-(1/2)*u^2)
      }
    }
    for(i in 1:length(d)){ #f are calculated
      f[i] <- (1/(n*h))*sum(K[i,])
    }
  }
  if(tableplease == TRUE){ #If user did not choose d, return a table of information.
    fr <- data.frame(x=d, y=f, row.names=c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max."))
    return(list("BandWidth" = h, "Data" = fr))
  } else{
    return(f)
  }
}
