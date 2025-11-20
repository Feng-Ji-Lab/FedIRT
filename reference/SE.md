# Federated IRT SE

Calculates Standard Error(SE) for FedIRT models.

## Usage

``` r
SE(fedresult)
```

## Arguments

- fedresult:

  fedirt result object

## Value

An array of standard errors for all parameters.

## Details

Input is the object of fedirt class.

## Examples

``` r
# turn input data to a list
inputdata = list(as.matrix(example_data_2PL))
# Call fedirt() function, and use 2PL model
fedresult = fedirt(inputdata, model_name = "2PL")
# get SE result
SEresult = SE(fedresult)
```
