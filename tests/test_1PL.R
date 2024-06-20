library(FedIRT)
library(testthat)
test_that('test 1PL', {

  inputdata1 = list(as.matrix(example_data_2PL))
  fedresult1 = fedirt(inputdata1, model_name = "1PL")

  inputdata2 = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
  fedresult2 = fedirt(inputdata2, model_name = "1PL")

  expect_equal(fedresult1[['a']],fedresult2[['a']])
  expect_equal(fedresult1[['b']],fedresult2[['b']])
  expect_equal(fedresult1[['loglik']],fedresult2[['loglik']])

  expect_equal(fedresult1[['a']], c(1,1,1,1,1,1,1,1,1,1),tolerance = 1e-2)
  expect_equal(fedresult1[['b']],c(-0.95588779, -1.24097452, -0.59950125, -0.44607217, -1.88997545, -1.27879273 ,-1.16695639 ,-0.75761610, -0.11846449,-0.08904234),tolerance = 1e-2)
  expect_equal(fedresult1[['loglik']],-957, tolerance = 1e-2)

})
