# Gradient of Log-Likelihood for the federated 2PL Model

Calculates the gradients of the log-likelihood function with respect to
the item discrimination (a) and difficulty (b) parameters for the
Two-Parameter Logistic (2PL) Item Response Theory (IRT) model. This
computation is vital for optimizing the item parameters via
gradient-based optimization algorithms.

## Usage

``` r
g_logL(a, b, data, q = 21, lower_bound = -3, upper_bound = 3)
```

## Arguments

- a:

  Numeric vector of item discrimination parameters in the 2PL model.

- b:

  Numeric vector of item difficulty parameters in the 2PL model.

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

The partial gradient for each parameter is: \$\$ \frac{ \partial l_k} {
\partial \alpha_j } = \sum\limits\_{n=1}^{q} \sum\limits\_{i=1}^{N_k}
(V\_{ik}(n) - \beta_j) \[ r\_{ijnk} - m\_{ink} P_j(V\_{ik}(n))\] \$\$
\$\$ \frac{ \partial l_k} { \partial \beta_j } =
(-\alpha_j)\sum\limits\_{n=1}^{q} \sum\limits\_{i=1}^{N_k} \[
r\_{ijnk} - m\_{ink} P_j(V\_{ik}(n))\] \$\$
