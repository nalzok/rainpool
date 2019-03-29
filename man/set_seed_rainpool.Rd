\name{set.seed.rainpool}
\alias{set.seed.rainpool}
\title{Binomial Random Variate Generation}
\usage{
set.seed.rainpool(seed)
}
\arguments{
  \item{seed}{A vector to be used as the seed of pseudorandom number generators for `rbinom.rainpool'.}
}
\description{
Set the seed used by the internal PRNG of `rbinom.rainpool' to `seed'.
}
\examples{
set.seed.rainpool(c(47, 11))
set.seed.rainpool(42)
}
