set.seed.rainpool <- function(seed) {
  .Call("_rainpool_set_seed_rainpool", seed)
}

rbinom.rainpool <- function(n, size, prob) {
  if (length(n) == 0) {
    return(numeric(0))
  }
  .Call("_rainpool_rbinom_rainpool", n, size, prob)
}
