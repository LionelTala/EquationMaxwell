library(testthat)
library(EquationMaxwell)

test_that("Visualisation du champ électrique fonctionne", {
  expect_s3_class(visualiser_champ_electrique(1e-6, seq(0.01, 1, by = 0.01)), "gg")  # Test de la visualisation du champ électrique
})

test_that("Visualisation du champ magnétique fonctionne", {
  expect_s3_class(visualiser_champ_magnetique(10, seq(0.01, 1, by = 0.01)), "gg")  # Test de la visualisation du champ magnétique
})

test_that("Visualisation de l'énergie d'une onde électromagnétique fonctionne", {
  expect_s3_class(visualiser_onde_electromagnetique(1e5, 1e-6), "gg")  # Test de la visualisation de l'énergie
})
