library(xlsx)

# algorithm constant parameters
minDimVal<- -100                   # min value of every dimension
maxDimVal <- 100                   # max value of every dimension
eps <- 10^-8                       # error values smaller than eps are considered as 0
mVectorLen <- c(10, 30, 50)          # vector length
mPopulationCount <- 150            # population count
F_Cr_params <- c(0.25, 0.5, 0.75)  # testable values for F and Cr parameters

# returns mean vector from rows in P
select <- function(P) {
  ans <- numeric()
  mi <- nrow(P)
  L <- ncol(P)
  for (i in 1:L) {
    mean <- mean(P[1:mi,i])
    ans <- c(ans, mean)
  }
  return(ans)
}

# gets 'count' unique rows from P
getSample <- function(P, count) {
  mi <- nrow(P)
  indices <- sample(1:mi, count, replace=F)
  return(P[indices,])
}

# returns result of crossover operation of elements Pi and Mi using parameter rc
crossover <- function(Pi, Mi, cr) {
  L <- length(Pi)
  d <- sample(1:L, 1)
  O <- numeric()
  for (j in 1:L) {
    rand <- runif(1, 0, 1)
    if (rand < cr || j == d) {
      O <- c(O, Mi[j])
    }
    else {
      O <- c(O, Pi[j])
    }
  }
  return(O)
}

# returns vector whose evaluation function value is lower
tournament <- function(q, Pi, Oi) {
  PiVal <- q(Pi)
  OiVal <- q(Oi)
  if (PiVal > OiVal) {
    return(Oi)
  }
  else {
    return(Pi)
  }
}

# generates random matrix with 'cols' columns and 'rows' rows, every value is in range [-minVal, maxVal]
generateRandomMatrix <- function(cols, rows, minVal, maxVal) {
  return(matrix(runif(cols*rows, minVal, maxVal), ncol=cols, nrow=rows))
}

# returns best value of evaluation function from all rows of matrix P
getBestResult <- function(q, P) {
  min(apply(P, 1, q))
}

# differential evolution algorithm with mean vector, returns minimum of q function
DEalgorithm <- function(q, P, FParam, cr, expectedValue) {
  t <- 0
  mi <- nrow(P)
  L <- ncol(P)
  iterations <- 10 * L
  stop <- FALSE
  newP <- P
  minVal <- numeric()
  
  cat("expectedValue=",expectedValue,"\n")
  while(!stop) {
    Pj <- select(P)
    for (i in 1:mi) {
      samples <- getSample(P, 2)
      Pk <- samples[1,]
      Pl <- samples[2,]
      M <- Pj + FParam * (Pk-Pl)
      O <- crossover(P[i,], M, cr)
      newP[i,] <- tournament(q, P[i,], O)
      
      t <- t + 2
    }
    P <- newP
    minVal <- getBestResult(q, P)
    error <- abs(minVal - expectedValue)
    
    if(t >= iterations || error < eps) {
      stop <- TRUE
    }
  }
  return(minVal)
}

# classical differential evolution algorithm, returns minimum of q function
classicalDEalgorithm <- function(q, P, FParam, cr, expectedValue) {
  t <- 0
  mi <- nrow(P)
  L <- ncol(P)
  iterations <- 10 * L
  stop <- FALSE
  newP <- P
  minVal <- numeric()
  cat("expectedValue=",expectedValue,"\n")
  while(!stop) {
    for (i in 1:mi) {
      samples <- getSample(P, 3)
      Pj <- samples[1,]
      Pk <- samples[2,]
      Pl <- samples[3,]
      M <- Pj + FParam * (Pk-Pl)
      O <- crossover(P[i,], M, cr)
      newP[i,] <- tournament(q, P[i,], O)
      
      t <- t + 2
    }
    P <- newP
    minVal <- getBestResult(q, P)
    error <- abs(minVal - expectedValue)
    if(t >= iterations || error < eps) {
      stop <- TRUE
    }
  }
  return(minVal)
}

mainFunction <- function() {
  # populate expected values for cec2013 functions
  expectedValue1 <- seq(from = -1500, to = -100, by = 100)
  expectedValue2 <- seq(from = 100, to = 1400, by = 100)
  expectedValue <- c(expectedValue1, expectedValue2)
  
  finalTable <- data.frame(Expected=double(28),
                   Max=double(28),
                   MaxClassic=double(28),
                   Min=double(28),
                   MinClassic=double(28),
                   Median=double(28),
                   MedianClassic=double(28),
                   Mean=double(28),
                   MeanClassic=double(28),
                   Std=double(28),
                   StdClassic=double(28))
    
  # Run each test for different F and Cr parameters...
  for(FParam in F_Cr_params) {
    for(Cr in F_Cr_params) {
      # ...and for different vecotr lenght values....
      for(L in mVectorLen) {
        # ...and for different test functions
        for(i in 1:28) {
          # Function with which we test evolutional algorithm
          q <- function(x) {
            return(cec2013::cec2013(i, x))
          }
          results <- vector(mode="numeric", length=0)
          results2 <- vector(mode="numeric", length=0)
          # ...21 times.
          for(j in 1:21) {
            cat(j,". F=",FParam,", Cr=",Cr,", L=",L,", Func=",i,"\n", sep="")
            
            P <- generateRandomMatrix(L, mPopulationCount, minDimVal, maxDimVal)
            val <- DEalgorithm(q, P, FParam, Cr, expectedValue[i])
            val2 <- classicalDEalgorithm(q, P, FParam, Cr, expectedValue[i])
            
            results[j] <- val
            results2[j] <- val2
          } # for(j)
          minValue <- min(results)
          minValue2 <- min(results2)
          maxValue <- max(results)
          maxValue2 <- max(results)
          medianValue <- median(results)
          medianValue2 <- median(results2)
          meanValue <- mean(results)
          meanValue2 <- mean(results2)
          stdValue <- sd(results)
          stdValue2 <- sd(results2)
          
          finalTable$Expected[i] <- expectedValue[i]
          finalTable$Max[i] <- maxValue
          finalTable$MaxClassic[i] <- maxValue2
          finalTable$Min[i] <- minValue
          finalTable$MinClassic[i] <- minValue2
          finalTable$Median[i] <- medianValue
          finalTable$MedianClassic[i] <- medianValue2
          finalTable$Mean[i] <- meanValue
          finalTable$MeanClassic[i] <- meanValue2
          finalTable$Std[i] <- stdValue
          finalTable$StdClassic[i] <- stdValue2
        } # for(i)
        
        # write results to xls file
        fileName <- paste("table_","F_",FParam,"_Cr_",Cr,"_L_",L,".xlsx", sep="")
        write.xlsx(finalTable, fileName)
      } # for(L)
    } # for(Cr)
  } # for(FParam)
}

mainFunction()
