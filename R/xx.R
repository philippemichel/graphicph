xx<- function(dfx, lig, titre = "",
              titx ="", tity="%", seuil = 95, desc = 1){
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
  ligx$nom <- levels(lig)
  #return(ligx)
  if (length(levels(lig))>5)
    {ang = 60}
  else
    {ang = 0}
  #
  if (desc == 1){
  ligx <- ligx %>%
    mutate(nom = fct_reorder(nom,desc(pc)))
  }
  gg <- ligx %>%
      ggplot(aes(x= nom, y = pc, fill = nom)) +
      geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin = binf, ymax = bsup),
                    width = 0.8,
                    size = 0.5) +
     labs(title = titre,
           x = titx,
           y = tity
      ) +
     theme_light() +
      theme(plot.title = element_text(size=16, face="bold"),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14),
            axis.text.x = element_text(size=14,angle=ang, hjust =1),
            axis.text.y = element_text(size=14),
            legend.position ="none"
      ) +
      scale_fill_viridis_d()
  plot(gg)
}
