interpolasi_linear <- function(x0, y0, x1, y1, x){
x<- c(0, 1, 2, 3)
f<- c(1, 2, 5, 10)

y<-y0+
  (y0-y1)/(x1-x0)*(x-x0)

  return(y)
}

interpolasi_linear(
  x0 = 1,
  y0 = 2,
  x1 = 3,
  y1 = 6,
  x = 2
)

x <- c(1, 2, 4, 5)
y <- c(2, 3, 5, 8)

approx(
  x,
  y,
  xout = 3
)


x <- c(1, 2, 5, 7)
y <- c(2, 3, 6, 8)

approx(
  x,
  y,
  xout = 6
)
