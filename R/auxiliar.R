checkSP <- function(sp){

  mustHave <- c("NombreCom", "NombreCie", "NombreIng", "Unidad", "TallaMin", "TipoMedicion")

  if(is.vector(sp) && length(sp) == 1 && class(sp) == "character"){
    index <- match(tolower(sp), tolower(speciesData$NombreCom))

    if(is.na(index)){
      stop("EL nombre ingresado para la especie es incorrecto o no se encuentra en la lista predeterminada.")
    }

    sp <- as.list(speciesData[index,])
  }else if(class(sp) != "list" || !all(is.element(names(sp), mustHave))){
    stop("'sp' debe ser una lista con los objetos '", paste(mustHave, collapse = "', '"), "'.")
  }

  return(sp)
}

readSegFile <- function(file, ...){
  output <- read.csv(file = file, ...)

  monthVector <- tolower(substr(output$month, 1, 3))
  for(i in seq_along(month.abb_spanish)){

    index <- which(is.element(monthVector, substr(tolower(month.abb_spanish[i]), 1, 3)))

    if(length(index) < 1){
      next
    }else{
      output$month[index] <- rep(substr(tolower(month.abb)[i], 1, 3), length(index))
    }
  }

  output$month <- paste0(toupper(substr(output$month, 1, 1)), tolower(substr(output$month, 2, 99)))

  dateVector <- with(output, as.Date(paste(year, word2month(month), 15, sep = "-")))

  dateRange <- seq.Date(range(dateVector)[1], range(dateVector)[2], by = "month")
  dateRange <- dateRange[!is.element(dateRange, unique(dateVector))]

  newData <- as.data.frame(mat.or.vec(nr = length(dateRange), nc = ncol(output)))
  colnames(newData) <- colnames(output)

  newData$year <- as.numeric(substr(dateRange, 1, 4))
  newData$month <- month2word(substr(dateRange, 6, 7))
  newData$day <- rep(15, length(dateRange))

  output <- rbind.data.frame(output, newData, stringsAsFactors = FALSE)
  output <- output[order(output$year, word2month(output$month), output$day),]

  return(output)
}

word2month <- function(x){
  x <- seq_along(month.abb)[match(tolower(x), tolower(month.abb))]

  return(x)
}

month2word <- function(x){
  x <- month.abb[match(as.numeric(x), 1:12)]

  return(x)
}
