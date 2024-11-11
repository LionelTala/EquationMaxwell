#' Calcule l'énergie d'une onde électromagnétique
#'
#' Cette fonction calcule l'énergie d'une onde électromagnétique en fonction du champ électrique et du champ magnétique.
#'
#' @param E Champ électrique en volts par mètre (V/m)
#' @param B Champ magnétique en teslas (T)
#' @return Énergie en joules par mètre cube (J/m³)
#' @examples
#' onde_electromagnetique(1e5, 1e-6)  # Exemple d'utilisation
onde_electromagnetique <- function(E, B) {
  u <- (1/2) * (8.854e-12 * E^2 + (1/(4 * pi * 1e-7)) * B^2)  # Formule d'énergie
  return(u)  # Retourne le résultat
}
