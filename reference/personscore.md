# Federated IRT person score

This function calculates persons' ability.

## Usage

``` r
personscore(fedresult)
```

## Arguments

- fedresult:

  fedirt result object

## Value

a list of person score in each school.

## Details

Input is the object of fedirt class.

## Examples

``` r
# turn input data to a list
inputdata = list(as.matrix(example_data_2PL))
# Call fedirt() function, and use 2PL model
fedresult = fedirt(inputdata, model_name = "2PL")
personscoreResult = personscore(fedresult)
```
