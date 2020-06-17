#' Calcul des IC par transformation angulaire
#'
#' Pour un nb  de cas et un nb total, calcule l'intervalle de confiance au seuil voulu.
#' 
#' @param nn nb de cas
#' @param tot total
#' @param seuil : marge d'erreur exprimée en percent (95 par défaut)
#'
#' @return un  vecteur : pourcentage, borne inf, borne sup
#' 
#' @example tangangPh(nn = 45, total = 100, seuil = 95)
#'
#' @export
tangangPh <- function(nn,tot,seuil = 95){
  if (seuil > 100 | seuil < 0)
  {return ("Erreur dans la marge de l'IC")}
  else{
  seu <- 1-(1-seuil/100)/2
  pp <- nn/tot
  sp <- qnorm(seu)/(2*sqrt(tot))
  if (floor(pp*100) == 0){
    binf <- 0
  }
  else {
  binf <- sin(asin(sqrt(pp-1/(2*tot)))-sp)^2*100
  }
  if (ceiling(pp*100) == 100){
    bsup <- 100
  }
  else{
  bsup <- sin(asin(sqrt(pp+1/(2*tot)))+sp)^2*100
  }
  return (c(pp=pp*100,binf=binf,bsup=bsup))
  }
}
