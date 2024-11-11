library(testthat)
library(EquationMaxwell)

test_that("Champ électrique est calculé correctement", {
  expect_equal(champ_electrique(1e-6, 0.1), 8.99e4, tolerance = 1e-2)  # Test de la fonction champ_electrique
})
