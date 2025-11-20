# Federated 2PL model

This function implements a federated learning approach to estimate the
parameters of the 2PL IRT model. It allows for collaborative estimation
across multiple datasets, while maintaining the privacy of each
individual data source. The federated 2PL model is particularly useful
in contexts where data sharing might be limited due to privacy concerns
or logistical constraints.

## Usage

``` r
fedirt_2PL(J, logL_entry, g_logL_entry)
```

## Arguments

- J:

  An integer indicating the number of items in the IRT model across all
  sites. This number should be consistent for all response matrices
  provided.

- logL_entry:

  A function that calculates the sum of log-likelihoods for the response
  matrices across all sites. This function is crucial for evaluating the
  fit of the model at each iteration.

- g_logL_entry:

  A function that computes the aggregated gradient of the log-likelihood
  across all participating entities.

## Value

A list containing the following components from the federated 2PL model
estimation:

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

- `person`: List containing person-related estimates with elements:

  - `a`: Vector of discrimination parameters (same as top-level 'a').

  - `b`: Vector of difficulty parameters (same as top-level 'b').

  - `ability`: List of numeric vectors with person abilities per site.

  - `site`: Numeric vector of abilities or locations specific to each
    site.

  - `person`: List of numeric vectors of person abilities minus site
    ability.

## Details

The algorithm leverages federated learning techniques to estimate shared
item parameters and individual ability levels without requiring the raw
data to be combined into a single dataset. The estimation procedure is
composed of several steps, including initialization, local computations
at each data source, communication of summary statistics to a central
server, and global parameter updates. This cycle is repeated until
convergence criteria are met and the global parameters stabilize.

Regarding the input parameters, 'J' is the number of items across all
sites, which should be consistent and known in advance. The 'logL_entry'
parameter should be a function that computes the log-likelihood of the
observed responses given the current model parameters. Likewise,
'g_logL_entry' is expected to be a function that computes the gradient
of the log-likelihood with respect to the model parameters to inform the
optimization process during parameter estimation.
