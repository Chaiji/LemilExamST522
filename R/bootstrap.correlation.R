
#' @title Bootstrap Estimation of Correlation
#' @export
#' @return Returns a list containing: the estimated correlation of the two given vectors, the bootstrap estimated correlation of the data, as well as the standard error, bias and the 95 percent confidence interval.
#' @usage bootstrap.correlation(n=200, x , y, plot = FALSE)
#' @keywords bootstrap estimation correlation standard error bias confidence interval
#' @description This function takes as arguments the number of bootstrap replicates \code{n},
#' \code{x} and \code{y} which are two numerical vectors / data, then bootstrap from \code{x ,y}, and calculate the correlation of these data for each resampling n times.
#' If \code{plot} is chosen to be TRUE, then the function plots the histogram, as well as the 95% confidence interval for the
#' estimates.
#' @import graphics stats
#' @param n is a positive integer that specifies how many bootstrap replicates should be used for estimation.
#' @param x is a vector of numeric values to be used with y to estimate correlation and bootstrap estimates.
#' @param y is a vector of numeric values to be used with x to estimate correlation and bootstrap estimates. y has to be of the same length as x.
#' @param plot is a boolean value that specifies whether a plot should be created or not
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' bootstrap.correlation(n=2000,cars$speed,cars$dist, plot=TRUE)


bootstrap.correlation <- function(n = 200, x , y, plot = FALSE){

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
  if(is.list(y) || is.list(x) || !is.numeric(x) || !is.numeric(y) || is.matrix(x+y) || !is.vector(x+y)){
    stop(" The given data 'x' or 'y' must be numerical vectors.")
  }
  if(length(x) != length(y)){ #We check if x and y has same length
      stop("'x' and 'y' has to be of the same length.")
  }
  if(!is.logical(plot)|| length(plot) != 1){
    stop("'plot' must be a single logical value.")
  }
  #---------------------------------------End Error Handler---------------------------------#
  correlation <- cor(x,y) #First get the true correlation
  N <- length(x) #we set N
  storage <- numeric(n) #We make space for n correlation estimates
  for (i in 1:n){
    j <- sample(1:N, size = N, replace = TRUE) #This sample is just a list of "slots" we want to take from in x and y
    xSample <- x[j] #Sample from x
    ySample <- y[j] #Sample from y
    storage[i] <- cor(xSample,ySample) #Estimate the correlation
  }
  BootstrapCor <- mean(storage) #We get the mean of the estimates
  se <- sd(storage) #standard deviation of correlations is in fact standard error
  bias <- mean(storage) - correlation #Estimate bias
  alpha <- 0.05 #alpha is set for a 95% confidence interval
  value <- round(qnorm(1-alpha/2),digits=2) #"value" is z
  CI <- c(BootstrapCor - value*se, BootstrapCor + value*se) #Confidence interval given in a vector
  #We save a list of returned values to easily work with if user is interested in specific values
  returnedList <- list("TrueCor" = correlation, "BSCor" = BootstrapCor, "SE" = se, "Bias" = bias, "CI" = CI)
  if(plot == TRUE){ #If user wants a histogram.
    #Breaks are set in the smallest possible interval to optimize presentation
    hist(storage, breaks=seq(round(min(storage),digits=2)-0.01,round(max(storage),digits=2)+0.01,0.01), main="Histogram of correlations", xlab="correlations")
    abline(v = correlation, col="red") #True correlation
    abline(v = CI[1], col="blue", lty=2) #Confidence interval 1
    abline(v = CI[2], col="blue", lty=2) #Confidence interval 2
  }
  return(returnedList) #Finally, the list is returned.
}
