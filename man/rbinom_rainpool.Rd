\name{rbinom.rainpool}
\alias{rbinom.rainpool}
\title{Binomial Random Variate Generation}
\usage{
rbinom.rainpool(n, size, prob)
}
\arguments{
  \item{n}{number of observations. If `length(n) > 1', the length is
          taken to be the number required.}
  \item{size}{number of trials (zero or more). }
  \item{prob}{probability of success on each trial.}
}
\description{
Yield n pseudorandom numbers from Binom(size, prob) with the Rainpool algorithm.
}
\examples{
goodness.of.fit.test <- function(x, size0, prob0) {
  x.count <- rep(0, size0 + 1)
  x.table <- table(x)
  x.count[as.numeric((names(x.table))) + 1] <- unname(x.table)
  
  # merge bins with less than 5 observations
  E <- length(x) * dbinom(0:size0, size0, prob0)
  E.lesser <- (E < 5)
  x.count <- c(x.count[!E.lesser], sum(x.count[E.lesser]))
  E <- c(E[!E.lesser], sum(E[E.lesser]))
  
  chisq.test(x.count, p = E/sum(E), simulate.p.value = TRUE)
}

n <- 1E5
size <- 64
prob <- 0.5

if (require(microbenchmark, quietly = TRUE)) {
  tm <- microbenchmark(
    native <- rbinom(n, size, prob),
    rainpool <- rbinom.rainpool(n, size, prob))
  boxplot(tm)
} else {
  native <- rbinom(n, size, prob)
  rainpool <- rbinom.rainpool(n)
}

\dontrun{
  goodness.of.fit.test(native, size, prob)
  goodness.of.fit.test(rainpool, size, prob)
}
}
