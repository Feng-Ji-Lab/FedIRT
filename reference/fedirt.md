# Federated IRT model

This function combines all types of algorithm of federated IRT models.
It inputs a dataset and return the estimated IRT parameters.

## Usage

``` r
fedirt(inputdata, model_name = "2PL", school_effect = FALSE, federated = "Avg")
```

## Arguments

- inputdata:

  A list of all responding matrices.

- model_name:

  The name of the model you want to use. Can be "1PL" "2PL" or "graded".
  "1PL" refers to Rasch Model, "2PL" refers to two-parameter logistic
  model, "graded" refers to graded model.

- school_effect:

  A bool parameter, TRUE refers to considering the school effect as a
  fixed effect. Default is FALSE.

- federated:

  The federated learning method. Default is "Avg", meaning using
  Federated Average. Can also be "Med", meaning Federated Median.

## Value

Corresponding model result as a list.

## Details

Input is a list of responding matrices from each school, every
responding matrix is one site's data.

## Examples

``` r
if (FALSE) { # \dontrun{
# turn input data to a list
inputdata = list(as.matrix(example_data_2PL))
# Call fedirt() function, and use 2PL model with school effect as a fixed effect
fedresult = fedirt(inputdata, model_name = "2PL",school_effect = TRUE)

# turn input data to a list
inputdata = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
# Call fedirt() function, and use graded model
fedresult = fedirt(inputdata, model_name = "graded")
} # }
```
