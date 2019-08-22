#' Histogramme
#'
#' @param dfx Data.frame
#' @param varz Variable numérique
#' @param taille Largeur de chaque colonne (10 par défaut)
#' @param titre Titre du graphique ("" par défaut)
#' @param titx Titre des x ("" par défaut)
#' @param tity Titre des y ("n" par défaut)
#'
#' @import stats
#' @import ggplot2
#' @import viridis
#' @import dplyr
#'
#' @example histoPh(cars,speed,taille=3,titre="ggg",titx="aa", tity="bb")
#'
#' @return graphique
#' @export
histoPh <- function(dfx, varz, taille = 10, titre = "", titx = "", tity = "n"){
  varx <- enquo(varz)
  gg <- ggplot(dfx) +
    aes(x = !! varx) +
    geom_histogram(binwidth = taille, color = "black", fill = 'lightblue') +
    labs(title = titre,
         x = titx,
         y = tity
    ) +
    theme_light() +
    theme(plot.title = element_text(size=14, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.position ="none"
    ) +
    scale_colour_viridis_d()
  plot(gg)
}
