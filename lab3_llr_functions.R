library(reshape2)

llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

make_weight_matrix <- function(z, x, omega) {
  r <- abs(x-z)/omega
  w <- ifelse(abs(r) < 1, (1-abs(r)^3)^3, 0)
  return(diag(w))
}

make_predictor_matrix <- function(x) {
  X <- cbind(1,x)
  return(X)
}

data(french_fries)
french_fries = french_fries[complete.cases(french_fries),]
z = seq(0, 15, length.out = 100)
fits = llr(z = z, x = french_fries$potato, y = french_fries$buttery, omega = 50)
plot(z, fits)

# tets
# terst2 