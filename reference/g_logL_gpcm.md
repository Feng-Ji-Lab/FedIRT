# Gradient of Log-Likelihood for the federated graded Model

Calculates the gradients of the log-likelihood function with respect to
the item discrimination (a) and difficulty (b) parameters for the graded
IRT model. This computation is vital for optimizing the item parameters
via gradient-based optimization algorithms.

## Usage

``` r
g_logL_gpcm(a, b, data, q = 21, lower_bound = -3, upper_bound = 3)
```

## Arguments

- a:

  Numeric vector of item discrimination parameters in the graded model.

- b:

  Numeric vector of item difficulty parameters in the graded model.

- data:

  The matrix of observed item responses, with individuals in rows and
  items in columns.

- q:

  The number of Gaussian quadrature points for numerical integration
  (default is 21).

- lower_bound:

  The lower bound for Gaussian quadrature integration (default is -3).

- upper_bound:

  The upper bound for Gaussian quadrature integration (default is 3).

## Value

A list containing two elements: the gradient vector with respect to item
discrimination parameters ('a') and the gradient vector with respect to
item difficulty parameters ('b').

## Details

The function approximates the partial derivatives by utilizing Gaussian
quadrature for numerical integration. Memoization techniques are used to
cache intermediate results, which is crucial for efficient computation
because it avoids redundant calculations. This can significantly speed
up iterative algorithms, particularly in the context of large datasets.
