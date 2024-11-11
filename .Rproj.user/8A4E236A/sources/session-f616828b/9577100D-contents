#' Calcule le champ magnétique autour d'un fil droit
#'
#' Cette fonction calcule le champ magnétique créé par un courant passant dans un fil droit à une distance donnée.
#'
#' @param I Courant en ampères (A)
#' @param r Distance en mètres (m) du fil
#' @return Champ magnétique en teslas (T)
#' @examples
#' champ_magnetique(10, 0.1)  # Exemple d'utilisation
champ_magnetique <- function(I, r) {
  mu_0 <- 4 * pi * 1e-7  # Perméabilité du vide en N/A²
  B <- (mu_0 * I) / (2 * pi * r)  # Calcul du champ magnétique
  return(B)  # Retourne le résultat
}
