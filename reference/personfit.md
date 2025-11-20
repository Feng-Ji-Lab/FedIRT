# Federated IRT person fit

personfit calculates the Zh values, infit and outfit statistics. The
returned object is a list.

## Usage

``` r
personfit(fedresult)
```

## Arguments

- fedresult:

  fedirt result object

## Value

a list of person fit in each school.

## Details

Input is the object of fedirt class.

## Examples

``` r
# turn input data to a list
inputdata = list(as.matrix(example_data_2PL))
# Call fedirt() function, and use 2PL model
fedresult = fedirt(inputdata, model_name = "2PL")
personfitResult = personfit(fedresult)
```
