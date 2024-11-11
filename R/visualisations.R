library(ggplot2)

#' Visualise le champ électrique en fonction de la distance
#'
#' Cette fonction génère un graphique du champ électrique en fonction de la distance d'une charge ponctuelle.
#'
#' @param q Charge en coulombs (C)
#' @param r_range Plage de distances en mètres (m)
#' @return Un graphique ggplot2
#' @examples
#' visualiser_champ_electrique(1e-6, seq(0.01, 1, by = 0.01))  # Exemple d'utilisation
visualiser_champ_electrique <- function(q, r_range) {
  E_values <- sapply(r_range, champ_electrique, q = q)
  data <- data.frame(Distance = r_range, Champ_Electrique = E_values)

  ggplot(data, aes(x = Distance, y = Champ_Electrique)) +
    geom_area(fill = "lightblue", alpha = 0.5) +  # Zone sous la courbe
    geom_line(color = "blue", size = 1.2) +  # Ligne pour le champ électrique
    labs(title = "Champ Électrique d'une Charge Ponctuelle",
         subtitle = paste("Charge =", q, "C"),
         x = "Distance (m)",
         y = "Champ Électrique (V/m)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_blank())
}

#' Visualise le champ magnétique en fonction de la distance
#'
#' Cette fonction génère un graphique du champ magnétique en fonction de la distance d'un fil droit.
#'
#' @param I Courant en ampères (A)
#' @param r_range Plage de distances en mètres (m)
#' @return Un graphique ggplot2
#' @examples
#' visualiser_champ_magnetique(10, seq(0.01, 1, by = 0.01))  # Exemple d'utilisation
visualiser_champ_magnetique <- function(I, r_range) {
  B_values <- sapply(r_range, champ_magnetique, I = I)
  data <- data.frame(Distance = r_range, Champ_Magnetique = B_values)

  ggplot(data, aes(x = Distance, y = Champ_Magnetique)) +
    geom_col(fill = "lightcoral", alpha = 0.5) +  # Barres pour le champ magnétique
    geom_point(color = "red", size = 3) +  # Points pour les valeurs
    labs(title = "Champ Magnétique autour d'un Fil Droit",
         subtitle = paste("Courant =", I, "A"),
         x = "Distance (m)",
         y = "Champ Magnétique (T)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_blank())
}

#' Visualise l'énergie d'une onde électromagnétique
#'
#' Cette fonction génère un graphique de l'énergie d'une onde électromagnétique en fonction du champ électrique.
#'
#' @param E Champ électrique en volts par mètre (V/m)
#' @param B Champ magnétique en teslas (T)
#' @return Un graphique ggplot2
#' @examples
#' visualiser_onde_electromagnetique(1e5, 1e-6)  # Exemple d'utilisation
visualiser_onde_electromagnetique <- function(E, B) {
  u <- onde_electromagnetique(E, B)
  data <- data.frame(E = E, B = B, Energie = u)

  ggplot(data, aes(x = E, y = Energie)) +
    geom_line(color = "green", size = 1) +  # Ligne pour l'énergie
    geom_point(color = "darkgreen", size = 4, shape = 21, fill = "white") +  # Points avec un contour
    labs(title = "Énergie d'une Onde Électromagnétique",
         subtitle = paste("Champ Magnétique =", B, "T"),
         x = "Champ Électrique (V/m)",
         y = "Énergie (J/m³)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_blank())
}





















#' Visualise le champ électrique et le champ magnétique en fonction du temps
#'
#' Cette fonction génère un graphique montrant les variations du champ électrique et du champ magnétique dans le temps.
#'
#' @param E Champ électrique maximal en volts par mètre (V/m)
#' @param B Champ magnétique maximal en teslas (T)
#' @param t_range Plage de temps en secondes (s)
#' @return Un graphique ggplot2
#' @examples
#' visualiser_champs_temps(1e5, 1e-6, seq(0, 2*pi, by = 0.1))  # Exemple d'utilisation
visualiser_champs_temps <- function(E, B, t_range) {
  # Création des données pour les champs électrique et magnétique
  data <- data.frame(
    Temps = t_range,
    Champ_Electrique = E * (1 + sin(t_range)),  # Variation du champ électrique
    Champ_Magnetique = B * (1 + sin(t_range + pi/2))  # Décalage de phase pour le champ magnétique
  )

  ggplot(data, aes(x = Temps)) +
    geom_ribbon(aes(ymin = 0, ymax = Champ_Electrique), fill = "lightblue", alpha = 0.5) +  # Zone pour le champ électrique
    geom_ribbon(aes(ymin = 0, ymax = Champ_Magnetique), fill = "salmon", alpha = 0.3) +  # Zone pour le champ magnétique
    geom_line(aes(y = Champ_Electrique), color = "blue", size = 1.2) +  # Ligne pour le champ électrique
    geom_line(aes(y = Champ_Magnetique), color = "red", size = 1.2) +  # Ligne pour le champ magnétique
    labs(title = "Variation des Champs Électrique et Magnétique dans le Temps",
         x = "Temps (s)",
         y = "Amplitude (V/m ou T)",
         subtitle = "Champ Électrique (bleu) et Champ Magnétique (rouge)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_blank())
}

#' Visualise les champs électrique et magnétique en fonction de la position
#'
#' Cette fonction génère un graphique montrant les champs électrique et magnétique en fonction de la position.
#'
#' @param E Champ électrique maximal en volts par mètre (V/m)
#' @param B Champ magnétique maximal en teslas (T)
#' @param x_range Plage de positions en mètres (m)
#' @return Un graphique ggplot2
#' @examples
#' visualiser_onde_position(1e5, 1e-6, seq(0, 10, by = 0.1))  # Exemple d'utilisation
visualiser_onde_position <- function(E, B, x_range) {
  # Création des données pour les champs électrique et magnétique
  data <- data.frame(
    Position = x_range,
    Champ_Electrique = E * (1 + sin(2 * pi * x_range)),  # Variation du champ électrique
    Champ_Magnetique = B * (1 + sin(2 * pi * x_range + pi/2))  # Décalage de phase pour le champ magnétique
  )

  ggplot(data, aes(x = Position)) +
    geom_bar(aes(y = Champ_Electrique), stat = "identity", fill = "blue", alpha = 0.5, position = "dodge") +  # Barres pour le champ électrique
    geom_bar(aes(y = Champ_Magnetique), stat = "identity", fill = "red", alpha = 0.3, position = "dodge") +  # Barres pour le champ magnétique
    labs(title = "Champs Électrique et Magnétique en Fonction de la Position",
         x = "Position (m)",
         y = "Amplitude (V/m ou T)",
         subtitle = "Champ Électrique (bleu) et Champ Magnétique (rouge)") +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_blank())
}
