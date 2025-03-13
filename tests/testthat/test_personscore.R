library(FedIRT)
library(testthat)
test_that('test person score', {
  inputdata2 = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
  fedresult2 = fedirt(inputdata2)
  personscoreResult = personscore(fedresult2)
  expect_equal(personscoreResult[[1]][1:5],c(-1.18,-0.55, -0.89,-0.20,0.38), tolerance = 1e-2)
})
