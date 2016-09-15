#' @import R.utils
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
#' @param file Nombre del archivo coninformación de captura y cpue.
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

#' Función que construye el reporte de seguiumiento
#'
#' @param desembarque Objeto de clase \code{landings} de donde se obtendrán los datos de
#' desembarque.
#' @param filename Archivo en donde se guardará el pdf. Si es \code{NULL}, se creará un archivo temporal
#' que se mostrará al final.
#' @param openAtEnd ¿Desea abrir el archivo al final del proceso? Si \code{filename = NULL}, este parámetro
#' no se tomará en cuenta.
#' @param ... Argumentos extra pasados a la función \code{pdf}.
#'
#' @export
#' @examples
#' \dontrun{
#' desembarque <- getData(file = "file.csv", type = "effort")
#' makeReport(desembarque)
#' }
makeReport <- function(desembarque, filename = NULL, openAtEnd = TRUE,
                       paper = "a4r", width = 10, height = 10, ...){
  x <- getTable(desembarque)

  if(is.null(filename)){
    filename <- paste0(tempfile(), ".pdf")
  }

  pdf(file = filename, paper = paper, width = width, height = height, ...)
  par(mfrow = c(2, 2))

  # Plot 1
  xlim <- c(0, 1)
  ylim <- c(0, 14)

  par(xaxs = "i", yaxs = "i", mar = c(0, 1, 3, 1))
  plot(1, 1, pch = NA, axes = FALSE, xlab = NA, ylab = NA, xlim = xlim, ylim = ylim)

  text(x = c(0.25, 0.75), y = c(13.5, 13.5), labels = colnames(x), font = 2)
  mtext(text = "Desembarques", side = 3, line = 2)

  for(i in seq(nrow(x), 1)){
    text(x = c(0.25, 0.75), y = c(13.5, 13.5) - i, labels = x[i,])
  }

  abline(h = c(13, 1), lwd = 2)
  box(lwd = 2)

  # Plot 2
  par(mar = c(4, 4, 1, 1))
  plot(desembarque, time = "month", main = "Desembarques")

  # Plot 3
  par(mar = c(4, 4, 1, 1))
  plot(esfuerzo, time = "month", main = "Esfuerzo pesquero")

  # Plot 4
  par(mar = c(4, 4, 1, 1))
  plot(cpue, time = "month", main = "CPUE (viaje)")

  dev.off()

  if(is.null(filename) | isTRUE(openAtEnd)){
    file.show(filename)
  }

  return(invisible())
}

