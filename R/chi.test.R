#' @export
#' @return returns the Chi-squared goodness of fit value, calculated using Pearson's Chi-squared test
#' @title Chi-squared Goodness of Fit test
#' @usage chi.test(x,p)
#'
#' @keywords distribution chi squared pearson goodness fit value
#'
#' @description This function takes 1 or 2 arguments to calculate the chi squared goodness of fit value from these arguments.
#'
#' @param x is a categorical variable vector of numbers, counting number of each incident.
#' @param p is an optional vector of probabilities. If unused, replaced by a uniformly distributed vector of probabilities.
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' chi.test(c(1,2,3),c(0.25,0.6,0.15))

chi.test <- function(x,p){
  #------------------------------------Error Handler:--------------------------------------#
  # ---------------------------------------------------------------------------------------#
  # 'x' must be a vector of positive numbers                                               #
  # 'p' must be a vector of probabilities. If not chosen, program decides uniform          #
  #----------------------------------------------------------------------------------------#
  #First we check if x is a vector of numbers
  if(!is.vector(x) || is.matrix(x) || is.list(x) || !is.numeric(x)){
    stop("'x' has to be a vector of numbers.(Non list, non matrix)")
  }
  if(missing(p)){ #If p is not used, probability is uniform
    p <- rep((1/length(x)),length(x))
  } else if(!is.vector(p) || is.matrix(p) || is.list(p) || !is.numeric(p)){
    stop("'p' has to be a vector of numbers.(Non list, non matrix)") #If p exist, check it is a vector of numbers
  } else if(length(p) != length(x)){
    stop("'p' and 'x' has to be of the same length")
  }
  if(sum(p) != 1){ #Warn the user if the total probability does not equal to 1.
    warning("Sum of p is not equal to 1, this may cause incorrect calculations.")
  }
  if(length(p[p<0])>0){ #Warn the user if they added negative probabilities
    stop("p contains negative values, this will cause incorrect calculations.")
  }
  if(length(x[x<0])>0){ #Warn the user if they added negative values to x
    stop("x contains negative values, this will cause incorrect calculations.")
  }
  #---------------------------------------End Error Handler---------------------------------#
  n <- sum(x) #We need to know how many there are in total for Pearson's Chi-squared statistic
  return(sum(((x - n*p)^2)/(n*p))) #Formula to calculate Pearson's Chi-squared statistic
}
