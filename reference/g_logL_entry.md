# Aggregated Gradient of Log-Likelihood for Federated Learning

Calculates the sum of the gradients of the log-likelihood with respect
to item discrimination (a) and difficulty (b) parameters across all
schools participating in a federated learning process. The function
`g_logL_entry` is a critical component in the gradient-based
optimization process within `fedirt`.

## Usage

``` r
g_logL_entry(ps)
```

## Arguments

- ps:

  A numeric vector including the model's current estimates for the item
  parameters, organized consecutively with discrimination parameters
  followed by difficulty parameters.

## Value

A matrix where the first half of rows corresponds to the aggregated
gradient with respect to item discrimination parameters and the second
half corresponds to the aggregated gradient with respect to item
difficulty parameters.

## Details

The function aggregates the gradients computed locally at each school.
The cumulative gradient is then used in the optimization algorithm to
update the model parameters. Each school should implement the function
`get_g_logL_from_index` which computes the gradients of log-likelihood
locally. This function needs to be aligned with the federated learning
framework, typically involving network communication to retrieve the
gradient information.

In simplified scenarios, or during initial testing and development,
users can substitute the network communication with a direct call to a
local `g_logL` function that computes the gradient of log-likelihood.
