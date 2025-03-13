rm(list = ls())
devtools::load_all(".")


inputdata = list(as.matrix(example_data_2PL))
fedresult = fedirt_2PL_data(inputdata)
inputdata = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
fedresult = fedirt_2PL_data(inputdata)

inputdata1 = list(as.matrix(example_data_2PL))
fedresult1 = fedirt(inputdata1)

inputdata2 = list(as.matrix(example_data_2PL_1), as.matrix(example_data_2PL_2))
fedresult2 = fedirt(inputdata2)

expect_equal(fedresult1[['a']],fedresult2[['a']])
expect_equal(fedresult1[['b']],fedresult2[['b']])
expect_equal(fedresult1[['loglik']],fedresult2[['loglik']])

expect_equal(fedresult1[['a']], c(0.6630576,0.2394203,2.1177645,0.8156689,0.4185483,0.4195789,0.5181838,0.6657132,0.8049487,0.9141963),tolerance = 1e-2)
expect_equal(fedresult1[['b']],c(-1.34716966, -4.51079294, -0.40097569, -0.52843935, -4.05022564, -2.71823636, -2.04151169, -1.06254190, -0.13873640, -0.09320751),tolerance = 1e-2)
expect_equal(fedresult1[['loglik']],-957, tolerance = 1e-2)

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



inputdata = list(as.matrix(example_data_2PL))
fedresult = fedirt_2PL_median_data(inputdata)


inputdata1 = list(as.matrix(example_data_graded))
fedresult1 = fedirt(inputdata1, model_name = "graded")

inputdata2 = list(as.matrix(example_data_graded_and_binary))
fedresult2 = fedirt(inputdata2, model_name = "graded")

expect_equal(fedresult1[['a']], c(0.024972889, -0.007298666, -0.223922728, 0.156573173, 0.102766762, 0.144576141, 0.401820170, 0.486887577, -1.168820108, -0.022503904), tolerance = 1e-2)
expect_equal(fedresult1[['b']], c(-22.21787663, 19.08124691, -13.81391282, 24.14087650, 31.14376750, -57.92830183, -2.66167695, 1.08326015, 0.13474588, 0.45497353, -3.88610679, 0.93674953, 1.63321670, 0.38833013, 5.43326143, 0.11280220, 2.71317052, -1.47064982, -0.55715906, 1.28507736, -0.49458765, 0.07102033, 0.18266666, -0.10059896, -14.60979492, 6.89391374), tolerance = 1e-2)

expect_equal(fedresult2[['a']], c(0.7554907, 0.7392413, 1.0495204, 0.4792658, 0.2184830, 0.8692939, 1.0245426, 0.8757308), tolerance = 1e-2)
expect_equal(fedresult2[['b']], c(-1.87984274, -1.35199484, -0.05149482, -0.56571184, -1.78542786, -2.58587414, -4.56223809, -0.84794542, -0.02826932, 0.23145341), tolerance = 1e-2)
