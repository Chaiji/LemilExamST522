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
  h = 0                                      #Start with 0 hits
  for(i in 1:n){
    U <- runif(3)                            #Need 3 random variables for x, y and angle respectively
    x <- d + 4*d*U[1]                        #This makes some empty space around the playing field
    y <- d + 4*d*U[2]                        #same as above
    Y <- l * sin(2*pi*U[3])                  #Determines the "height" of needle
    Y2 = y %% d + Y                          #Used for determining a hit or miss
    if(Y2 >= d || Y2 <= 0){                  #If Y2 is within area 0<Y2<d, then needle is between 2 parallel lines, but not hitting any
      h = h + 1                              #If Y2 hits a parallel line, count it
    }
  }
  return((2*l*n)/(d*h))                      #Finally, return the estimated pi
}
