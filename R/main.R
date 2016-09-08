#' getData
#'
#' @param file Direcci�n del archivo con informaci�n de capturas.
#' @param type Definir si se desea obtener estad�sticos de Desembarques (\code{landings}),
#' Esfuerzo (\code{effort}) o CPUE (\code{cpue}).
#' @param toTons �Desea tranformar los valores a toneladas? (\code{TRUE} or \code{FALSE}).
#' @param ... Argumentos extras.
#'
#' @details Esta funci�n retornar� un objeto de clase \code{landings}, \code{effort} o
#' \code{cpue}, dependiendo de lo seleccionado en el argumento \code{type}. La manera
#' c�mo se generar�n las figuras depender� de cada clase.
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