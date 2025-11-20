# Federated IRT model

This function combines all types of algorithm of federated IRT models.
It inputs a dataframe and return the estimated IRT parameters.

## Usage

``` r
fedirt_file(
  inputdata,
  model_name = "2PL",
  school_effect = FALSE,
  federated = "Avg",
  colname = "site"
)
```

## Arguments

- inputdata:

  A dataframe.

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

- colname:

  Column name indicating the school.

## Value

Corresponding model result as a list.

## Details

Input is a dataframe from each school with a column indicating the
school name.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- read.csv("dataset.csv", header = TRUE)
fedresult <- fedirt_file(data, model_name = "2PL")
} # }
```
