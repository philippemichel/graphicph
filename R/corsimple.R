#' Graphique de correlation simple
#'
#' Trace le graphique en point avec la courbe de moyene mobile ou la droite de regression, affiche le p.
#'
#' @param dfx Data frame à étudier
#' @param xx Variable en x
#' @param yy Variabe en y
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
#' @examples corsimpleph(iris,Sepal.Length,Petal.Length, titx="Pétales",
#'                       tity ="Pistil", titre = "Grand titre")
#'
corsimpleph <- function(dfx, xx, yy, titre = "", titx = "", tity = "", moymob = TRUE, dtcor = FALSE){
    x <- enquo(xx)
    y <- enquo(yy)
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
          aes(x = !!x, y = !!y) +
          geom_jitter()
      if (moymob == TRUE){
            gg <- gg + geom_smooth(method = "loess")
            }
      if (dtcor == TRUE){
            gg <- gg + geom_smooth(method = "lm")
            }
       gg <- gg + geom_text(label = ppm, x = mx, y = my, size = 5) +
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
            legend.position = "none"
      ) +
      scale_colour_viridis_d()
    plot(gg)
}
