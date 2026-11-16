f <- function(x) x^2

a <- 0
b <- 2

midpoint <- (b - a) * f((a+b) / 2)

trapezoid <- (b - a) / 2 * (f(a) + f(b))

h <- (b - a ) / 2
simpson13 <- h / 3 * (f(a) + 4*f((a + b) / 2 ) + f(b))

c(midpoint = midpoint,
  trapezoid = trapezoid,
  simpson13 = simpson13)


trapezoid_comp <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, length.out = n + 1)
  y <- f(x)
  
  h/2 * (y[1] + 2*sum(y[2:n]) + y[n+1])
}

f <- function(x) x^2

trapezoid_comp(f, 0, 2, 4)



trapezoid_comp <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, length.out = n + 1)
  y <- f(x)
  
  h/2 * (y[1] + 2*sum(y[2:n]) + y[n+1])
}

f <- function(x) x^2

trapezoid_comp(f, 0, 2, 12)






simpson13_comp <- function(f, a, b, n) {
  
  if (n <= 0 || n %% 2 != 0) {
    stop("n harus berupa bilangan bulat positif dan genap.")
  }
  
  h <- (b - a) / n
  x <- seq(a, b, length.out = n + 1)
  
  sum_odd <- sum(f(x[seq(2, n, by = 2)]))
  
  sum_even <- if (n >= 4) {
    sum(f(x[seq(3, n - 1, by = 2)]))
  } else {
    0
  }
  
  (h / 3) * (
    f(a) + f (b) +
    4 * sum_odd +
    2 * sum_even 
  )
}

simpson13_comp(f, 0, 2, 4)