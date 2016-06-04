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
getSample <- function(P, count) {
  mi <- nrow(P)
  indices <- sample(1:mi, count, replace=F)
  return(P[indices,])
}
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
q <- function(v) {
  return(mean(v)*mean(v))
}
tournament <- function(Pi, Oi) {
  PiVal <- q(Pi)
  OiVal <- q(Oi)
  if (PiVal > OiVal) {
    return(Oi)
  }
  else {
    return(Pi)
  }
}
generateRandomMatrix <- function(cols, rows, maxVal, minVal) {
  return(matrix(runif(cols*rows, minVal, maxVal), ncol=cols, nrow=rows))
}

DEalgorithm <- function(P, maxIterations, F, cr) {
  t <- 0
  stop <- FALSE
  mi <- nrow(P)
  newP = P
  
  while(!stop) {
    #print(t)
    for (i in 1:mi) {
      Pj <- select(P)
      samples = getSample(P, 2)
      Pk <- samples[1,]
      Pl <- samples[2,]
      M <- Pj + F * (Pk-Pl)
      O <- crossover(P[i,], M, cr)
      newP[i,] <- tournament(P[i,], O)
    }
    P <- newP
    
    t <- t + 1
    if(t >= maxIterations) {
      stop <- TRUE
    }
  }
  return(P)
}

# test algorytmu dla funkcji ewaluacyjnej q bedacej kwadratem sredniej wszystkich elementow wektora. Minimum, do ktorego dazymy to 0.
cols <- 5
rows <- 5
min <- -100
max <- 100
iterations <- 200
F <- 0.5
cr <- 0.5
v <- generateRandomMatrix(cols, rows, max, min)
P <- DEalgorithm(v, iterations, F, cr)
print("Oczekkiwane minimum: 0")
cat("Uzyskane minimum: ", min(q(P)))
