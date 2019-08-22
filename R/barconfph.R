#' Barplot simple avec les IC a partir d'une donnee discrete.
#'
#' Trace un graphique en barre avec les IC à 95 % des  pourcentages d'une variable fcatorielle.
#' Calcul des intervalles de confiance par transformation angulaire.
#'
#' @param dfx un data.frame
#' @param lig une variable factorielle
#' @param titre Titre du graphique (vide par defaut)
#' @param titx Titre de l'axe des x (vide par defaut)
#' @param tity Titre de l'axe des y (pourcent par defaut)
#' @param seuil Marge d'erreur en % (95 par défaut)
#' @param desc Classification descendante des valeurs (TRUE par default)
#' @param angle  Niveaux en x inscrit avec un angle (FALSE par défaut)
#'
#' @return un graphique
#'
#' @import stats
#' @import ggplot2
#' @import viridis
#' @import dplyr
#'
#' @export
barconfPh <- function(dfx, lig,
                      titre = "", titx ="", tity="%",
                      seuil = 95, desc = TRUE, angle = FALSE){
  varx <- enquo(lig)
  lig <- pull(dfx,!!varx)
  if (is.factor(lig) == FALSE){
    return("Erreur : Variable non factorielle")
  }
  ligx <- c()
  tlig <- table(lig)
  ll <- length(tlig)
  slig <- sum(tlig)
  for (i in 1:ll){
    # print(tlig[[i]])
    ligp <- tangangPh(tlig[[i]],slig, seuil)
    ligx <- rbind(ligx,ligp)
  }
  ligx <- as.data.frame(ligx)
  names(ligx) <- c("pc", "binf", "bsup")
  # print(lig)
  ll <- levels(lig)
  ligx$nom <- factor(ll,as.character(ll))
  #return(ligx)
  if (angle)
  {ang <-  60
   hh <-  1}
  else
  {ang <-  0
  hh <-  0.5}
  #
  if (desc){
    ligx <- ligx %>%
      mutate(nom = fct_reorder(nom,desc(pc)))
  }
  gg <- ligx %>%
    ggplot(aes(x= nom, y = pc, fill= nom)) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = binf, ymax = bsup),
                  width = 0.8,
                  size = 0.5) +
    labs(title = titre,
         x = titx,
         y = tity
    ) +
    theme_light() +
    scale_fill_brewer(palette = "Blues") +
    theme(plot.title = element_text(size=16, face = "bold"),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          axis.text.x = element_text(size=14,angle = ang, hjust = hh),
          axis.text.y = element_text(size=14),
          legend.position ="none"
    )# +
   # scale_fill_viridis_d()
  plot(gg)
}



