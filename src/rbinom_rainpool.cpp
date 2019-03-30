#include <stdio.h>
#include <stdint.h>
#include <math.h>
#include <Rcpp.h>
// [[Rcpp::depends(dqrng)]]
#include <dqrng.h>
#include <xoshiro.h>
#include <convert_seed.h>
// [[Rcpp::plugins(cpp11)]]

namespace {
  dqrng::xoshiro256plus rng{};
}

Rcpp::NumericVector rbinom_rainpool(Rcpp::NumericVector n,
                                    Rcpp::NumericVector size,
                                    Rcpp::NumericVector p);
int rbinom_rainpool_scalar(int size, double p);
int rbinom_rainpool_01(int size);
int64_t get_random();

// [[Rcpp::export]]
void set_seed_rainpool(Rcpp::IntegerVector seed) {
  rng.seed(dqrng::convert_seed<uint64_t>(seed));
}

// [[Rcpp::export]]
Rcpp::NumericVector rbinom_rainpool(Rcpp::NumericVector n,
                                    Rcpp::NumericVector size,
                                    Rcpp::NumericVector prob) {
  int rep_times = n.size() > 1 ? n.size() : n[0];
  Rcpp::NumericVector result(rep_times);
  
  if (size.size() < rep_times) {
    size = Rcpp::rep_len(size, rep_times);
  }
  if (prob.size() < rep_times) {
    prob = Rcpp::rep_len(prob, rep_times);
  }

  for (int i = 0; i < rep_times; i += 1) {
    if(i % 1000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    result[i] = rbinom_rainpool_scalar(size[i], prob[i]);
  }

  return result;
}

int rbinom_rainpool_scalar(int size, double p) {
    if (size < 0 || std::isnan(p) || p < 0 || p > 1) {
        return -1;
    } else if (size == 0 || p == 0) {
        return 0;
    } else if (p == 1) {
        return size;
    }

    if (p < 0.5) {
        return rbinom_rainpool_01(rbinom_rainpool_scalar(size, 2 * p));
    } else if (p > 0.5) {
        return size - rbinom_rainpool_scalar(size, 1 - p);
    } else {
        return rbinom_rainpool_01(size);
    }
}

int rbinom_rainpool_01(int size) {
  if (!size) {
    return 0;
  }

  int result = 0;
  while (size >= 64) {
    result += __builtin_popcountll(rng());
    size -= 64;
  }

  if (size) {
    result += __builtin_popcountll(rng() & ((1LLU << size) - 1));
  }

  return result;
}
