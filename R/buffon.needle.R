#' @export
#' @return returns pi estimated by simulating Buffon's experiment.
#' @title Buffon's needle
#' @usage buffon.needle(n=10, l=1, d=1)
#'
#' @keywords buffon needle simulation pi estimation
#'
#' @description This function takes 3 arguments to simulate Buffon's experiment to estimate pi.
#'
#' @param n is a positive integer value that specifies how many needles that are tossed in the simulation.
#' @param l is a positive number that specifies the length of the needles tossed in the simulation.
#' @param d is a positive number that specifies the distance between the stripes on the floor in the simulation.
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' buffon.needle(n=3408, l=2.5,d=3)

buffon.needle <- function(n = 10,l = 1,d = 1){
  #------------------------------------Error Handler:--------------------------------------#
  # ---------------------------------------------------------------------------------------#
  # 'n' must be numeric, natural number greater than 1.                                    #
  # 'l' and 'd' must be numeric, positive numbers.                                         #
  #----------------------------------------------------------------------------------------#
  if(is.matrix(n) || is.list(n) || length(n)!=1){ #Check that n is a single value
    stop("'n' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(n)){ #checks if n is a number
    stop("'n' has to be a positive integer")
  } else if(n < 1 || n != round(n)){ #checks if n is a natural number
    stop("'n' has to be a positive integer")
  }
  if(is.matrix(l) || is.list(l) || length(l)!=1){ #Check that l is a single value
    stop("'l' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(l)){ #checks if l is a number
    stop("'l' has to be a positive number")
  } else if(l != abs(l)){ #checks if l is a positive number
    stop("'l' has to be a positive number")
  }
  if(is.matrix(d) || is.list(d) || length(d)!=1){ #Check that d is a single value
    stop("'d' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(d)){ #checks if l is a number
    stop("'d' has to be a positive number")
  } else if(d != abs(d)){ #checks if l is a positive number
    stop("'d' has to be a positive number")
  }
  #---------------------------------------End Error Handler---------------------------------#
  U <- matrix(runif(n*2),nrow=n,ncol=2) #We need 2 random variables for every needle. We throw n needles.
  #Calling the runif function as few times as possible appears to minimize simulation time.
  return((2*l*n)/(d*sum(U[,1]*d < l*sin(U[,2]*(pi/2))))) #Number of hits are calculated directly in the return.
  #U[,1] contains n random numbers. U[,2] contains n random numbers.
}
