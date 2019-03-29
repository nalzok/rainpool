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
size <- 64
prob <- 0.5

result.rainpool <- rbinom.rainpool(1E3, size, prob)
result.native <- rbinom(1E3, size, prob)

goodness.of.fit.test <- function(x, size0, prob0) {
  x.count <- rep(0, size0 + 1)
  x.table <- table(x)
  x.count[as.numeric((names(x.table)))] <- unname(x.table)
  E <- length(x) * dbinom(0:size0, size0, prob0)

  # merge all slots with expectation < 5 into E[size0 + 2]
  E.lesser <- (E < 5)
  E[size0 + 2] <- sum(E[E.lesser])
  x.count[size0 + 2] <- sum(x.count[E.lesser])
  x.count[E.lesser] <- E[E.lesser]

  Q <- sum((x.count - E) ^ 2 / E)
  df <- size0
  1 - pchisq(Q, df)
}

goodness.of.fit.test(result.rainpool, size, prob)
goodness.of.fit.test(result.native, size, prob)
}
