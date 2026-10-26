x <- c(0, 1, 2, 3)
y <- c(1, 2, 5, 10)

interpolasi_approx <- function(x0) {
  i <- findInterval(x0, x)
  
  y[i] + (y[i+1] - y[i]) / (x[i+1] - x[i]) * (x0 - x[i])
}

interpolasi_approx(1.5)=
  