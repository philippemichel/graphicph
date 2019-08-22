#' Pyramide des âges
#'
#' @param dfx data.frame
#' @param age variable age "age" par défaut
#' @param sexe variable "sexe" sexe par défaut
#'
#' @importf stats
#' @import ggplot2
#' @import RColorBrewer
#' @import dplyr
#'
#' @return graphique
#' @export
pyramidph <- function(dfx, age, sexe){
  age <- enquo(age)
  sexe <- enquo(sexe)
  gg <-  ggplot(dfx) +
    aes(x=!!age, fill=!!sexe) +
    geom_bar(data=subset(dfx,sexe=="F"),aes(y=..count..*(-1))) +
    geom_bar(data=subset(dfx,sexe=="H")) +
    scale_fill_manual(values = c("pink","blue")) +
    scale_y_discrete(limits=c(-100,0,100),labels=c("F",0,"H")) +
    coord_flip() +
    labs(y = "ans") +
    theme_light() +
    theme(plot.title = element_blank(), # pas de titre
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=16, face="bold"),
          axis.text.x  = element_text(size=16, face="bold"),
          axis.text.y  = element_text(size=16, face="bold"),
          legend.position = "none"
    )
  plot(gg)
}
