set.seed.rainpool <- function(seed) {
  .Call("_rainpool_set_seed_rainpool", seed)
}

rbinom.rainpool <- function(n, size, prob) {
  if (length(n) == 1) {
    n = rep(0, n);
  }
  df <- data.frame(n=n, size=size, prob=prob)
  .Call("_rainpool_rbinom_rainpool", df$n, df$size, df$prob)
}
