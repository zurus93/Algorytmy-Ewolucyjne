# algorithm constant parameters
minDimVal<- -100      # min value of every dimension
maxDimVal <- 100      # max value of every dimension
eps <- 10^-8          # error values smaller than eps are considered as 0

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
  cat("q = ", PiVal,"\n")
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
DEalgorithm <- function(q, P, F, cr, expectedValue) {
  t <- 0
  mi <- nrow(P)
  L <- ncol(P)
  iterations <- 10000 * L
  stop <- FALSE
  newP <- P
  minVal <- numeric()
  
  while(!stop) {
    for (i in 1:mi) {
      Pj <- select(P)
      samples <- getSample(P, 2)
      Pk <- samples[1,]
      Pl <- samples[2,]
      M <- Pj + F * (Pk-Pl)
      O <- crossover(P[i,], M, cr)
      newP[i,] <- tournament(q, P[i,], O)
    }
    P <- newP
    minVal <- getBestResult(q, P)
    error <- abs(minVal - expectedValue)
    t <- t + 1
    if(t >= maxIterations || error < eps) {
      stop <- TRUE
    }
  }
  return(minVal)
}
# classical differential evolution algorithm, returns minimum of q function
classicalDEalgorithm <- function(q, P, F, cr, expectedValue) {
  t <- 0
  mi <- nrow(P)
  L <- ncol(P)
  iterations <- 10000 * L
  stop <- FALSE
  newP <- P
  minVal <- numeric()
  
  while(!stop) {
    for (i in 1:mi) {
      samples <- getSample(P, 3)
      Pj <- samples[1,]
      Pk <- samples[2,]
      Pl <- samples[3,]
      M <- Pj + F * (Pk-Pl)
      O <- crossover(P[i,], M, cr)
      newP[i,] <- tournament(q, P[i,], O)
    }
    P <- newP
    minVal <- getBestResult(q, P)
    error <- abs(minVal - expectedValue)
    t <- t + 1
    if(t >= maxIterations || error < eps) {
      stop <- TRUE
    }
  }
  return(minVal)
}

vectorLen <- 5
populationCount <- 100
expectedValue <- 0
F <- 0.5
cr <- 0.5
q <- function(x) {
  return(sum(x^2))
}
P <- generateRandomMatrix(vectorLen, populationCount, minDimVal, maxDimVal)
val <- DEalgorithm(q, P, F, cr, expectedValue)
val2 <- classicalDEalgorithm(q, P, F, cr, expectedValue)
print("Oczekkiwane minimum: 0")
cat("Uzyskane minimum: ", val)
cat("Uzyskane minimum: ", val2)
s
