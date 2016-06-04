
#' @title Linear Model summarizer
#' @export
#' @return Returns a list containing: a vector of residuals, a single value for Residual Standard Error, single value for R-squared,
#' single value for adjusted R-squared, single value for F-statistic, single value for degrees of freedom, single value for p-value,
#' And a data.frame of coefficients(Estimates, standard error, t-value, PR(>|t|))
#' @usage linear.model(formula)
#' @keywords estimate standard error t p value df freedom degrees residual linear model summary lm
#' @description This function takes as argument a formula with a certain syntax, dependent~predictor1+predictor2+...+predictorN,
#' And returns a list of information including residuals and coefficients(Estimate, standard error, t-value, Pr(>|t|)) in data.frames,
#' as well as the residual standard error, degrees of freedom, Multiple R-squared, Adjusted R-squared, F-statistic and the p-value.
#' @param formula is an argument of the type 'language' that apply the syntax: dependent~predictor1+predictor2+...+predictorN
#' @author Nguyen Khanh Le Ho & Emil H. Andersen \cr
#' Department of Mathematics and Computer Science (IMADA) \cr
#' University of Southern Denmark, Denmark \cr
#' \email{emila14@student.sdu.dk} \cr
#' \email{ngho14@student.sdu.dk} \cr
#' @examples
#' linear.model(cars$speed~cars$dist)
#' linear.model(cars$speed~cars$dist+I(cars$dist^2))
#' linear.model(cars$speed~cars$dist+I(cars$dist^2)+I(cars$dist*cars$dist*cars$dist))

linear.model <- function(formula){
  #------------------------------------Error Handler:--------------------------------------#
  # ---------------------------------------------------------------------------------------#
  # 'formula' has to be of the type 'language'                                             #
  #----------------------------------------------------------------------------------------#
  #We only check that 'formula' is of the correct type, because this type of input is very
  #hard to work with, we don't know what else to check for.
  if(typeof(formula) != "language"){
    stop("Your input must be presented in the language type, see documentation for examples")
  }
  #---------------------------------------End Error Handler---------------------------------#
  call <- match.call() #This one is saved for the summary at the end of the function
  mf <- match.call() #This is used for setting up our matrix. This is equal to the exact call
  mf[[1L]] <- quote(stats::model.frame) #Change the called function in the call to stats::model.frame. Evaluating this gives us what we need
  mf <- eval(mf, parent.frame()) #This evaluates our model frame using information from the parent environment.

  #Most calculations below are done based on chapter 5 of "A modern approach to regression with R"
  #Page referrences are to that book.
  width <- dim(mf)[2] #Save the information about the dimensions of the matrix. This is needed for later. In the regression book, width=p-1
  height <- dim(mf)[1] #Also saved for later. height = n
  Y <- mf[,1] #Y is the dependent variable. Page 131 in book.
  X <- rep(1,height) #X is a matrix that contains a column of 1s, and then columns of the other data. Page 131 in book.

  for(i in 2:width){
    X <- cbind(X,mf[,i]) #The other variables are added. Page 131 in book.
  }

  bEst <- as.vector(solve((t(X)%*%X))%*%t(X)%*%Y) #bEst is the estimated betas. Page 132 formula (5.6). Solve inverts a matrix, t() transposes a matrix, %*% is matrix multiplication.
  Yhat <- X%*%bEst #Estimated Y's, so caleld Yhat. Page 132 formula (5.7)
  residuals <- as.vector(Y - Yhat) #Estimated error, i.e. residuals, so called ehat. Page 132 formula (5.8)

  RSE <- sd(residuals) #Residual standard error is equal to the standard deviation of the residuals

  RSS <- sum((residuals)^2) #Least squares estimates, needed for Rsquare. Page 135 top.
  SST <- sum((Y - mean(Y))^2) #Sum of squares of Y, needed for Rsquare. Page 135 bottom.
  Rsquare <- 1 - (RSS/SST) #Multiple R-squared calculated using RSS and SST. Page 136 Note 1.
  RsquareAdj <- 1 - (RSS/(height-width-2))/(SST/(height-1)) #Adjusted R-squared, using formula from book. Reminder: height = n, width = p - 1. Page 137 Note 1.

  SSreg <- sum((Yhat - mean(Y))^2) #Page 135 bottom. Used to calculate the F-statistic
  Fstat <- (SSreg/(width-1))/(RSS/(height-width-2)) #Formula to calculate F-statistic. Page 136 middle.

  df <- height - width #degrees of freedom is generally the amount of data minus the amount of different parameters.

  pval <- 1 - pf(Fstat,width-1,height-width-2) #To calculate the p-value, we use F-statistics, since we already have the Fstat.

  SS <- (1/(height - width - 2))*sum((residuals)^2) #Error variance, S^2. Page 135 top.
  SE <- as.vector(sqrt(diag(SS*solve(t(X)%*%X)))) #Standard Error. Page 134 Var(betahat|X), replace sigma^2 by SS. Note that Diag is used because all outliers are irrelevant. Book does not explain why
  Tval <- bEst/SE #Page 135, middle. t-value, note that beta_i is 0, and so isnt in the equation

  Pr <- 1-pt(abs(Tval),df) #Pr(>|t|) values. Calculated directly as it is described, using t distribution.

  #Making the table for residuals, based on how summary() would do it.
  res <- data.frame("Min" = min(residuals),"Q1"=quantile(residuals,0.25,names=FALSE), "Median" = median(residuals), "Q3"=quantile(residuals,0.75,names=FALSE), "Max" = max(residuals), row.names='')

  #Getting the names of the things for coefficient table
  y <- toString(call[2]) #Gets the entire argument as a string
  y2 <- strsplit(y, c("+"), fixed = TRUE) #Splits into parts based on +
  y3 <- unique(y2[[1]]) #Makes it into a vector of strings, that contains unique instances in it.
  y4 <- strsplit(y3[1], c(" "), fixed = TRUE) #splits the first part of the argument, namely "dependent ~ predictor1"
  if(length(y3) > 2){ #Depending on how many more predictors are chosen after the first, we piece them together accordingly in a vector.
    y3 <- y3[2:length(y3)] #If there is more than 1.
  } else if(length(y3) == 2){
    y3 <- y3[2] #If there is exactly 1 more.
  } else{
    y3 <- c() #If there is only 1 predictor after ~, then y3 is no longer needed.
  }
  y4 <- unique(y4[[1]]) #We turn this into a vector of strings
  y4 <- y4[3] #We are not interested in the dependent variable or the ~, so we remove them
  y5 <- c(y4,y3) #We combine y4 and y3.
  y5 <- trimws(y5) #We trim away whitespace in the strings.

  #Coefficient table names
  coefNames <- c("(Intercept)") #The first coefficient name is always (intercept)
  for(i in 1:length(bEst)-1){ #we know that we need names for length(bEst)-1 coefficients.
    coefNames <- c(coefNames,y5[i]) #Name vector is created properly
  }
  if(length(y5) > length(bEst)-1){ #If we did not use all the names, it means there is something wrong with the input. User is notified.
    message("Some inputs were ignored, please try the I() function, and see if this helps. Examples can be found in the help file.")
  }
  coef <- data.frame("Estimate" = bEst, "Std.Error" = SE, "t value" = Tval, "Pr.t" = Pr, row.names=coefNames) #The coefficients are saved as a data.frame. We chose this over a table as we found this easier to work with.
  print(list("Call" = call, "Residuals" = res, "Coefficients" = coef)) #Prints a list containing the call, the residuals and the coefficients just like summary()
  print(paste0("Residual standard error: ",format(RSE,digits=4)," on ",df," degrees of freedom")) #Prints extra information, namely RSE with 4 digits, as well as degrees of freedom.
  print(paste0("Multiple R-squared: ",format(Rsquare,digits=4), ", Adjusted R-squared: ", format(RsquareAdj,digits=4))) #Extra information, R^2 and R_Adj^2
  print(paste0("F-statistic: ", format(Fstat,digits=4), ", p-value: ", format(pval,3))) #Extra information, F-statistic and p-value
  #Returns residuals, RSE, R^2, R_Adj^2, F-statistic, degrees of freedom, p-value as well as the coefficient data.frame. This is done using invisible so it does NOT print it unless user explicitly asks for it later.
  return(invisible(list("residuals" = residuals,"RSE" = RSE, "Rsquared" = Rsquare, "RsquaredAdj" = RsquareAdj, "Fstat" = Fstat, "DF" = df, "pval" = pval, "coefficients" = coef)))
}
