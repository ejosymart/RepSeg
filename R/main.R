#' getData
#'
#' @param file Dirección del archivo con información de capturas.
#' @param type Definir si se desea obtener estadísticos de Desembarques (\code{landings}),
#' Esfuerzo (\code{effort}) o CPUE (\code{cpue}).
#' @param toTons ¿Desea tranformar los valores a toneladas? (\code{TRUE} or \code{FALSE}).
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
#' getData(file = "D:/datosperico.csv", type = "effort")
#' }
getData = function(file, type, toTons, ...){
  output = switch(tolower(type),
                  landings = .getLandingsData(file = file, toTons = toTons, ...),
                  effort   = .getEffortData(file = file, ...),
                  cpue     = .getCPUEData(file = file, toTons = toTons, ...),
                  read.csv(file = file, ...))
  return(output)
}
