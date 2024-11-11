#' Calcule le champ électrique d'une charge ponctuelle
#'
#' Cette fonction calcule le champ électrique généré par une charge ponctuelle à une distance donnée.
#'
#' @param q Charge en coulombs (C)
#' @param r Distance en mètres (m) de la charge
#' @return Champ électrique en volts par mètre (V/m)
#' @examples
#' champ_electrique(1e-6, 0.1)  # Exemple d'utilisation
champ_electrique <- function(q, r) {
  k <- 8.99e9  # Constante de Coulomb en N m²/C²
  E <- k * q / (r^2)  # Calcul du champ électrique
  return(E)  # Retourne le résultat
}
