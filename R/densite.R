#' Courbe de Densité
#'
#' @param dfx Data.frame
#' @param varx Variable numérique
#' @param vary Groupes à représenter ("1" par défaut, un seul groupe)
#' @param titre titre du graphique ("" par défaut)
#' @param titx titre de l'axe des x ("" par defaut)
#'
#' @import stats
#' @import ggplot2
#' @import viridis
#' @import dplyr
#'
#' @return graphique
#' @export
densitePh <- function(dfx, varx, vary = "1", titre = "",titx = ""){
  varx <- enquo(varx)
  vary <- enquo(vary)
  gg <- ggplot(dfx) +
    aes(x = !!varx, color = !!vary, fill = !!vary) +
    geom_density(alpha = 0.6) +
    labs(title = titre,
         x = titx
    ) +
    theme_light() +
    theme(plot.title = element_text(size=16, face="bold"),
          axis.title.x = element_text(size=14, face="bold"),
          axis.title.y = element_text(size=14, face="bold"),
          axis.text.x = element_text(size=14, face="bold"),
          axis.text.y = element_text(size=14, face="bold")
    ) +
  scale_fill_viridis_d(option = "magma") +
  scale_colour_viridis_d(option = "magma") +
  if (vary[[2]] == "1")
  {theme(legend.position = "none")}
  else
  {theme(legend.position = "right")}
  plot(gg)
}
