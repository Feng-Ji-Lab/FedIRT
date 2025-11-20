# Aggregate Log-Likelihood Function for Federated Learning

Computes the sum of log-likelihoods across multiple schools in a
federated learning setting. The function `logL_entry` aggregates
contribution of each school's log-likelihood to the overall model. It is
designed to be used within the optimization process of `fedirt`.

## Usage

``` r
logL_entry(ps)
```

## Arguments

- ps:

  A parameter vector consisting of item parameters; it should include
  both discrimination (a) and difficulty (b) parameters.

## Value

The sum of log-likelihoods as a single numeric value, representing the
likelihood of the entire federated dataset under the current model's
parameters.

## Details

In a federated learning context, each school computes its log-likelihood
locally. The `logL_entry` function is responsible for aggregating these
values. Users are expected to provide an implementation for
`getlogL_from_index`, which should include network requests to retrieve
log-likelihoods calculated by each school, or for simplified prototyping
purposes, could directly use a `logL` function to compute likelihoods
locally.
