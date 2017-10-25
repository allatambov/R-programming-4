########################### HW2 solution (advanced) ###########################

# load packages

library(MASS)
library(Matrix)

# function

fcorrs <- function(n_obs, means_vect, corrs_vect){
  
  if (! all(abs(corrs_vect) <= 1)) {
    cat("The vector of correlation coefficients contains inappropriate values.")
  }
  
  else {
    m1 <- matrix(, length(means_vect), length(means_vect))
    m1[lower.tri(m1)] <- c(corrs_vect)
    diag(m1) <- 1
    m1 <- pmax(m1, t(m1), na.rm = TRUE)

    if (det(m1) < 0){
      new <- nearPD(m1, corr = TRUE)
      m1 <- new$mat 
      cor_list <- m1[lower.tri(m1)]
      cat("Matrix of correlations is not positive definite. 
          Trying to find the nearest positive definite matrix. \n")
      cat("Old values of correlation coeffs:", corrs_vect, "\n")
      cat("New values of correlation coeffs:", cor_list, "\n")
      cat("Frobenius norm of matrix difference:", new$normF, "\n")}
    
      mvrnorm(n_obs, mu = means_vect, Sigma = m1, empirical = TRUE)
  }}

# tests

F <- fcorrs(n_obs = 10, means_vect = c(2, 5, 7), corrs_vect = c(0.3, 0.6, -1.8))
F <- fcorrs(n_obs = 10, means_vect = c(2, 5, 7), corrs_vect = c(0.3, 0.6, 0.8))
F <- fcorrs(n_obs = 10, means_vect = c(2, 5, 7), corrs_vect = c(0.3, 0.6, -0.8))

# comments

# all - checks whether all values in a vector are TRUE
# check the condition abs(corrs_vect) <= 1 and negate using !
# so ! all(abs(corrs_vect) <= 1) will return TRUE 
# if at least one coefficient is not in [-1,1]

# lower.tri - lower triangle, elements below the main diagonal of a matrix
# insert coefficients into the lower triangle
# diag <- 1 - insert ones into the main diagonal
# pmax(m1, t(m1), na.rm = TRUE - make the matrix symmetrical 

# nearPD - the nearest positive matrix
# new$normF - Frobenius norm (authomatically computed by R)

