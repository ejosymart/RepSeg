#' @title Método \code{print} para objetos de clase \code{landings}.
#'
#' @param x Objeto de clase \code{landings}.
#' @param ... Argumentos extra.
#'
#' @return Información básica en la base de datos: Número de registros, meses, años, número de puertos.
#' @export
#'
#' @method print landings
print.landings <- function(x, ...){
  print(x$info)

  return(invisible())
}

#' @title Método \code{summary} para objetos de clase \code{landings}.
#'
#' @param object Objeto de clase \code{landings}.
#' @param ... Argumentos extra.
#' @return Desembarques diarios, mensual, anual y por puertos.
#' @export
#'
#' @method summary landings
summary.landings <-  function(object,...) {

  output <-  list()

  output$sumPorts <- .getSumPorts.landings(object)
  output$ports    <- .getPorts.landings(object)
  output$months   <- .getMonth.landings(object)
  output$years    <- .getYear.landing(object)

  class(output) <- "summary.landings"
  return(output)
}

#' @title Método \code{print} para objetos de clase \code{summary.landings}.
#'
#' @param x Objeto de clase \code{summary.landings}.
#' @param ... Argumentos extra.
#'
#' @export
#'
#' @method print summary.landings
print.summary.landings <- function(x, ...) {

  x2 <- x; class(x2) <- 'landings'

  cat("\nDesembarque por dia:\n\n")
  print(x$sumPorts)

  cat("\nDesembarque por puertos (solo positivos):\n\n")
  print(x$ports[x$ports$Desembarque>0, ,drop=FALSE])

  cat("\nDesembarque mensual:\n\n")
  print(t(x$months),...)

  cat("\nDesembarque anual:\n\n")
  print(x$years,...)

  return(invisible())
}

#' @title Método \code{plot} para objetos de clase \code{landings}.
#'
#' @param x Objeto de clase \code{landings}.
#' @param time Argumento que hace referencia al paso de tiempo para ser graficados,
#' \code{"day"}, \code{"month"}, \code{"year"}.
#' @param ... Argumentos extra.
#'
#' @export
#'
#' @method plot landings
plot.landings <- function(x, time = NULL, ...) {

  if(is.null(time)) time = "day"
  switch(time,
         day   = .plotDays.landings(x=x, ...),
         month = .plotMonths.landings(x=x, ...),
         year  = .plotYears.landings(x=x, ...),
         port  = .plotPort.landings(x=x, ...)
  )
  return(invisible())
}
