checkSP <- function(sp){

  mustHave <- c("NombreCie", "NombreCom", "NombreIng", "NombreFAO", "TallaMin", "ArtePesca")

  if(class(sp) != "list" || !all(is.element(names(sp), mustHave))){
    stop("'sp' debe ser una lista con los objetos '", paste(mustHave, collapse = "', '"), "'.")
  }

  return(TRUE)
}
