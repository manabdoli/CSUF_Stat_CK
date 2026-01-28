# --- A small stand-in "Fingers" dataset (self-contained) ---------------------
set.seed(42)
n <- 60

Fingers <- data.frame(
  Gender = sample(c("Female", "Male"), n, replace = TRUE),
  Thumb  = round(rnorm(n, mean = 5.8, sd = 0.6), 2),
  Index  = round(rnorm(n, mean = 7.0, sd = 0.7), 2),
  Middle = round(rnorm(n, mean = 7.7, sd = 0.7), 2),
  Ring   = round(rnorm(n, mean = 7.2, sd = 0.7), 2),
  Pinkie = round(rnorm(n, mean = 5.4, sd = 0.6), 2),
  SSLast = sample(c(0:9, NA), n, replace = TRUE, prob = c(rep(0.095, 10), 0.05))
)
