#' Box-plot
#'
#' @param dfx Data.frame
#' @param xx valeur catégorielle
#' @param yy valeur numérique
#' @param titre Titre principal (vide par defaut)
#' @param titx Titre de l'axe des x (vide par defaut)
#' @param tity Titre de l'axe des y (vide par defaut)
#'
#' @import stats
#' @import ggplot2
#' @import viridis
#' @import dplyr
#'
#' @return graphique
#' @export
graphboxph <- function(dfx, xx, yy, titre = "",titx = "", tity = ""){
    x <- enquo(xx)
    y <- enquo(yy)
    xa <- pull(dfx, !!x)
    ya <- pull(dfx, !!y)
    bb <- data.frame(xa,ya)
    bb <- na.omit(bb)
    mx <- length(levels(xa))-0.5
    my <- (max(ya)-min(ya))*0.9+min(ya)
    ct <- anova(lm(ya~xa))
    ctp <- ct[[5]][1]
    if (ctp < 0.001){
        ppm <- paste0("p <0,0001")
    }
    else {
        ppm <- paste0("p = ",signif(ctp,2))
    }
    gg <- ggplot(bb) +
        aes(x = xa, y = ya, fill = xa) +
        geom_boxplot(varwidth = TRUE, na.rm = TRUE) +
        # geom_jitter(width=0.2) +
      labs(title = titre,
           x = titx,
           y = tity
      ) +
      theme_light() +
      theme(plot.title = element_text(size=16, face="bold"),
            axis.title.x = element_text(size=14, face="bold"),
            axis.title.y = element_text(size=14, face="bold"),
            axis.text.x = element_text(size=14, face="bold"),
            axis.text.y = element_text(size=14, face="bold"),
            legend.position ="none"
      ) +
      scale_fill_viridis_d(option = "magma")
    plot(gg)
}
