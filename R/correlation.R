#' Graphique de correlation, plusieurs groupes
#'
#' Trace le graphique en point avec la courbe de moyene mobile ou la droite de regression
#' @param dfx Data frame à étudier
#' @param xx Variable en x
#' @param yy Variabe en y
#' @param groupe Groupes a deiis
#' @param titre Titre Principal
#' @param titx Titre de l'axe des X
#' @param tity Titre de l'axe des Y
#' @param moymob Trace la moyenne mobile + IC (TRUE par defaut)
#' @param dtcor Trace la droite de regression + IC (FALSE par defaut)
#'
#' @import stats
#' @import ggplot2
#' @import viridis
#' @import dplyr
#'
#' @return un graphique
#' @export
#'
#' @examples cormultiph(iris,Sepal.Length,Petal.Length, Species, titx="Pétales",
#'                      tity ="Pistil", titre = "Grand titre")
#'
cormultiph <- function(dfx, xx, yy, groupe, titre = "", titx = "", tity = "", moymob = TRUE, dtcor = FALSE){
    x <- enquo(xx)
    y <- enquo(yy)
    z <- enquo(groupe)
    xa <- pull(dfx, !!x)
    ya <- pull(dfx, !!y)
    mx <- (max(xa) - min(xa)) * 0.9 + min(xa)
    my <- (max(ya) - min(ya)) * 1.1 + min(ya)
    ct <- cor.test(xa,ya)
    ctp <- ct$p.value
    if (ctp < 0.001) {
        ppm <- paste0("p <0,0001")
    }
    else {
        ppm <- paste0("p = ",signif(ctp,3))
    }
    #
    gg <- ggplot(dfx) +
          aes(x = !!x, y = !!y, color = !!z) +
          geom_jitter()
      if (moymob == TRUE){
            gg <- gg + geom_smooth(method = "loess")
            }
      if (dtcor == TRUE){
            gg <- gg + geom_smooth(method = "lm")
            }
       gg <- gg +
      labs(title = titre,
           x = titx,
           y = tity
      ) +
      theme_light() +
      theme(plot.title = element_text(size=14, face="bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.position = "right"
      ) +
      scale_colour_viridis_d()
    plot(gg)
}
