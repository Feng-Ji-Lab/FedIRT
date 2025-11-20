# Federated Graded Response Model Estimation Function

Implements a federated learning approach for the estimation of the
graded response model parameters, enabling collaborative parameter
estimation across distributed datasets while ensuring individual data
source privacy.

## Usage

``` r
fedirt_gpcm(J, M, logL_entry, g_logL_entry)
```

## Arguments

- J:

  An integer indicating the number of items in the IRT model across all
  sites. This number should be consistent for all response matrices
  provided.

- M:

  An integer vector indicating the maximum level (number of categories
  minus one) for each item across all sites, which determines the total
  number of step difficulties to estimate for the graded response model.

- logL_entry:

  A function that calculates the sum of log-likelihoods for the response
  matrices across all sites. This function is crucial for evaluating the
  fit of the model at each iteration.

- g_logL_entry:

  A function that computes the aggregated gradient of the log-likelihood
  across all participating entities.

## Value

A list containing the following components from the federated graded
model estimation:

- `par`: Numeric vector of model's fitted parameters including item
  discrimination (a) and item difficulty (b) parameters.

- `value`: The optimization objective function's value at the found
  solution, typically the log-likelihood.

- `counts`: Named integer vector with counts of function evaluations and
  gradient evaluations during optimization.

- `convergence`: Integer code indicating the optimization's convergence
  status (0 indicates successful convergence).

- `message`: Message from optimizer about optimization process, NULL if
  no message is available.

- `loglik`: The calculated log-likelihood of the fitted model, identical
  to the 'value' element when the objective function is log-likelihood.

- `a`: Numeric vector of estimated item discrimination parameters.

- `b`: Numeric vector of estimated item difficulty parameters.

## Details

The function adopts a federated learning framework to perform estimation
of item step difficulties and individual ability levels in an IRT graded
response model without needing to pool the data into one centralized
dataset. The estimator follows an iterative optimization procedure
consisisting of local computations, information sharing with a central
aggregator, and updating of the global parameters.

## References

Muraki, E. (1992). A generalized partial credit model: Application of an
EM algorithm. *Applied Psychological Measurement*, 16(2), 159–176.
[doi:10.1177/014662169201600206](https://doi.org/10.1177/014662169201600206)
