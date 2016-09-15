#' @import R.utils
#' @import graphics
#'
#' @title Herramientas para la generación de reportes de captura, esfuerzo y CPUE
#'
#' @author Josymar Torrejón-Magallanes, \email{jotorrejon@imarpe.gob.pe}
#' @name RepSeg-package
#' @description Herramientas para la generación de reportes de captura, esfuerzo y CPUE.
#' @aliases RepSeg-package RepSeg
#' @docType package
#' @keywords reportes, seguimiento, captura, esfuerzo, CPUE
#' @exportClass landings
#' @exportClass effort
#' @exportClass cpue
NULL

#' getData
#'
#' @param file Nombre del archivo con información de captura y cpue.
#' @param type Definir si se desea obtener estadísticos de Desembarques (\code{landings}),
#' Esfuerzo (\code{effort}) o CPUE (\code{cpue}).
#' @param toTons Para convertir los valores a toneladas (\code{TRUE} or \code{FALSE}).
#' Solo para desembarques (\code{landings}) y cpue (\code{cpue}).
#' @param ... Argumentos extras.
#'
#' @details Esta función retornará un objeto de clase \code{landings}, \code{effort} o
#' \code{cpue}, dependiendo de lo seleccionado en el argumento \code{type}. La manera
#' cómo se generarán las figuras dependerá de cada clase.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' getData(file = "file.csv", type = "effort")
#' }
getData <-  function(file, type, toTons, ...){
  output <-  switch(tolower(type),
                    landings = .getLandingsData(file = file, toTons = toTons, ...),
                    effort   = .getEffortData(file = file, ...),
                    cpue     = .getCPUEData(file = file, toTons = toTons, ...),
                    read.csv(file = file, ...))
  return(output)
}

#' Función que construye el reporte de seguimiento y lo exporta en formato pdf.
#'
#' @param desembarque Objeto de clase \code{landings} de donde se obtendrán los datos de
#' desembarque.
#' @param filename Archivo en donde se guardará el pdf. Si es \code{NULL}, se creará un archivo temporal
#' que se mostrará al final.
#' @param openAtEnd ¿Desea abrir el archivo al final del proceso? Si \code{filename = NULL}, este parámetro
#' no se tomará en cuenta.
#' @param paper Argumento pasado desde la función \code{\link{pdf}}.
#' @param width Argumento pasado desde la función \code{\link{pdf}}.
#' @param height Argumento pasado desde la función \code{\link{pdf}}.
#' @param sp Argumento con información sobre especie. Si es \code{NULL}, se tomará la especie incluída
#' en el argumento \code{desembarque}. Ver detalles abajo.
#' @param ... Argumentos extra pasados a la función \code{pdf}.
#'
#' @details El argumento \code{sp} puede ser indicado de dos maneras: 1. Si es dejado como \code{NULL} (por
#' defecto), se tomará desde la sección \code{info} dentro del objeto \code{desembarque}; 2. si el usuario
#' desea indicar manualmente la información de la especie, puede hacerlo mediante una lista cuyos elementos
#' sean NombreCie (nombre científico), NombreCom (nombre común), NombreIng (nombre en inglés), NombreFAO
#' (nombre FAO), TallaMin (talla mínima) y ArtePesca (arte de pesca). Un ejemplo de cómo crear una lista
#' con información de especies es mostrado abajo, en los ejemplos.
#'
#' @export
#' @examples
#' \dontrun{
#' desembarque <- getData(file = "file.csv", type = "effort")
#' makeReport(desembarque)
#' }
#'
#' sp <- list(NombreCie = "Engraulis ringens", NombreCom = "anchoveta", NombreIng = "anchovy",
#'            NombreFAO = "anchoveta", TallaMin = "12 cm", ArtePesca = "Red de cerco")
makeReport <- function(desembarque, filename = NULL, openAtEnd = TRUE,
                       paper = "USr", width = 10, height = 10, sp = NULL, ...){
  x <- getTable(desembarque)

  if(is.null(filename)){
    filename <- paste0(tempfile(), ".pdf")
  }

  checkSP(sp)

  pdf(file = filename, paper = paper, width = width, height = height, ...)

  layoutMatrix <- c(1, 1, 2, 2,
                    3, 3, 4, 4,
                    5, 5, 6, 6)
  layoutMatrix <- matrix(layoutMatrix, nrow = 3, byrow = TRUE)
  layout(mat = layoutMatrix)

  # IMARPE logo
  par(mar = c(3, 0, 0, 0))
  plot(1, 1, xlim = c(0, 1), ylim = c(0, 1), pch = NA, axes = FALSE, xlab = NA, ylab = NA)
  rasterImage(imarpeLogo, 0.65, 0.1, 1, 0.9)

  # Header
  delay <- 0.01

  par(mar = c(0, 5, 0, 5), xaxs = "i", yaxs = "i")
  plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = c(0, 1), ylim = c(0, 8))

  text(x = 0.5 - delay, y = 7.5, labels = "Nombre científico:", adj = 1, cex = 1.2, font = 2)
  text(x = 0.5 + delay, y = 7.5, labels = sp$NombreCie, adj = 0, cex = 1.2, font = 3)

  text(x = 0.5 - delay, y = 6.5, labels = "Nombre común:", adj = 1, cex = 1.2, font = 2)
  text(x = 0.5 + delay, y = 6.5, labels = sp$NombreCom, adj = 0, cex = 1.2, font = 1)

  text(x = 0.5 - delay, y = 5.5, labels = "Nombre en inglés:", adj = 1, cex = 1.2, font = 2)
  text(x = 0.5 + delay, y = 5.5, labels = sp$NombreIng, adj = 0, cex = 1.2, font = 1)

  text(x = 0.5 - delay, y = 4.5, labels = "Nombre FAO:", adj = 1, cex = 1.2, font = 2)
  text(x = 0.5 + delay, y = 4.5, labels = sp$NombreFAO, adj = 0, cex = 1.2, font = 1)

  text(x = 0.5 - delay, y = 3.0, labels = "Talla mínima de captura:", adj = 1, cex = 1.2, font = 2)
  text(x = 0.5 + delay, y = 3.0, labels = sp$TallaMin, adj = 0, cex = 1.2, font = 1)

  text(x = 0.5 - delay, y = 2.0, labels = "Arte de pesca:", adj = 1, cex = 1.2, font = 2)
  text(x = 0.5 + delay, y = 2.0, labels = sp$ArtePesca, adj = 0, cex = 1.2, font = 1)

  # Plot 1: Landing table
  xlim <- c(0, 1)
  ylim <- c(0, 14)

  par(xaxs = "i", yaxs = "i", mar = c(2, 7, 3, 7))
  plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)

  tableSep <- 1/(ncol(x))
  tableSep <- seq(from = 0, by = tableSep, length.out = ncol(x)) + tableSep/2

  text(x = tableSep, y = c(13.5, 13.5), labels = colnames(x), font = 2)
  mtext(text = "Desembarques", side = 3, line = 2)

  for(i in seq(nrow(x), 1)){
    text(x = tableSep, y = c(13.5, 13.5) - i, labels = x[i,])
  }

  abline(h = c(13, 1), lwd = 2)
  box(lwd = 2)

  # Plot 2: Landin
  par(mar = c(4, 4, 3, 1))
  plot(desembarque, time = "month", main = "Desembarques")

  # Plot 3
  par(mar = c(4, 4, 4, 1))
  plot(esfuerzo, time = "month", main = "Esfuerzo pesquero")

  # Plot 4
  par(mar = c(4, 4, 4, 1))
  plot(cpue, time = "month", main = "CPUE (viaje)")

  dev.off()

  if(is.null(filename) | isTRUE(openAtEnd)){
    file.show(filename)
  }

  return(invisible())
}

