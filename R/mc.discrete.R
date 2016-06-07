
#' @title Markov Chain Simulator
#' @export
#' @return Returns a vector of values corresponding to the stationary probability for each state in the given transition probability matrix.
#' @usage mc.discrete(p,k,n)
#' @keywords simulation markov chain transition matrix probability
#' @description This function takes 3 arguments to simulate the stationary probabilities from a transition probability matrix.
#' @param p is a transition probability matrix for Markov Chain simulation.
#' @param k is a single integer number that states which state the simulation starts at.
#' @param n is a single natural number that decides how many simulation steps is to be done.
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' mc.discrete(matrix(c(0,0,0,0.4,0.7,0.5,0.6,0.3,0.5),nrow=3,ncol=3),1,100)

mc.discrete <- function(p,k,n){
  #------------------------------------Error Handler:--------------------------------------------#
  # ---------------------------------------------------------------------------------------------#
  # p is a transition probability matrix                                                         #
  # k is an integer indicating the initial state of the chain                                    #
  # n is a natural number indicating the number of simulated steps                               #
  #----------------------------------------------------------------------------------------------#
  #p is a matrix with ncol=nrow
  if(is.list(p) || !is.numeric(p) || !is.matrix(p) || is.vector(p)){
    stop("The given data 'p' must be a numerical matrix.")
  } else if(dim(p)[1] != dim(p)[2]){
    stop("The given data 'p' must have the same number of columns and rows.")
  } else if(sum(p) != dim(p)[1]){
    stop("The given data 'p' must have summed probability 1 in every row.")
  }
  if(is.matrix(k) || is.list(k) || length(k)!=1){ #Check that k is a single value
    stop("'k' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(k)){ #checks if n is a number
    stop("'k' has to be a positive integer")
  } else if(k < 1 || n != round(n) || k > dim(p)[2]){ #checks if k is a natural number inside the transition probability matrix
    stop("'k' has to be a positive integer between 1 and the width/height of the transition probability matrix")
  }
  if(is.matrix(n) || is.list(n) || length(n)!=1){ #Check that n is a single value
    stop("'n' has to be a positive integer; length=1, non-list, non-matrix")
  } else if(!is.numeric(n)){ #checks if n is a number
    stop("'n' has to be a positive integer")
  } else if(n < 1 || n != round(n)){ #checks if n is a natural number
    stop("'n' has to be a positive integer")
  }
  #---------------------------------------End Error Handler--------------------------------------#
  width <- dim(p)[2] #Lets us know what the highest number to expect.
  road <- rep(0,n) #Saves space for the amount of steps we take.
  va <- runif(n) #We need a random number for each step.
  for(i in 1:n){ #We need to take n steps.
    point <- FALSE #Used to check if we found a point
    j <- 1 #We start from 1, when checking probabilities.
    while(point == FALSE){ #We have to check up to "width" to get a point.
      if(va[i] <= p[k,j]){ #If the random number corresponds to the probability of that state
        point = TRUE #We found a point
        road[i] = j #Save it on the "road"
        k = j #Set our new starting state as the state j we found.
      } else{ #If the random number is not correct
        va[i] <- va[i] - p[k,j] #We subtract the probability so that it corresponds to the rest.
        j <- j + 1 #We check the next probability state.
      }
    }
  }
  stationary <- rep(0,width) #We need "width" numbers to be returned
  for(i in 1:width){ #We sum the amount of times we were at each state, and divide by number of steps.
    stationary[i] = sum(road == i)/n
  }
  return(stationary) #We return a vector that corresponds to stationary probability of each state in numerical order.
}
