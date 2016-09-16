#' @title Método \code{print} para objetos de clase \code{effort}.
#'
#' @param x Objeto de clase \code{effort}.
#' @param ... Argumentos extra.
#'
#' @return Información básica en la base de datos: Número de registros, meses, años, número de puertos.
#' @export
#'
#' @method print effort
print.effort <- function(x, ...){
  print(x$info)

  return(invisible())
}



#' @title Método \code{summary} para objetos de clase \code{effort}.
#'
#' @param object Objeto de clase \code{effort}.
#' @param ... Argumentos extra.
#' @return Esfuerzo diario, mensual, anual y por puertos.
#'
#' @export
#'
#' @method summary effort
summary.effort <- function(object,...) {

  output <- list()

  output$sumPorts <- .getSumPorts.effort(object)
  output$ports    <- .getPorts.effort(object)
  output$months   <- .getMonth.effort(object)
  output$years    <- .getYear.effort(object)

  class(output) <- "summary.effort"
  return(output)
}

#' @title Método \code{print} para objetos de clase \code{summary.effort}.
#'
#' @param x Objeto de clase \code{summary.effort}.
#' @param ... Argumentos extra.
#'
#' @export
#'
#' @method print summary.effort
print.summary.effort <- function(x, ...) {

  x2 <- x; class(x2) <- 'effort'

  cat("\nEsfuerzo por dia:\n\n")

  print(x$sumPorts)

  cat("\nEsfuerzo por puertos (solo positivos):\n\n")

  print(x$ports[x$ports$Effort>0, ,drop=FALSE])

  cat("\nEsfuerzo mensual:\n\n")
  print(t(x$months),...)

  cat("\nEsfuerzo anual:\n\n")
  print(x$years,...)

  return(invisible(x))

}

#' @title Método \code{plot} para objetos de clase \code{effort}.
#'
#' @param x Objeto de clase \code{effort}.
#' @param time Argumento que hace referencia al paso de tiempo para ser graficados,
#' \code{"day"}, \code{"month"}, \code{"year"}.
#' @param ... Argumentos extra.
#'
#' @export
#'
#' @method plot effort
plot.effort <- function(x, time=NULL, ...) {

  if(is.null(time)) time = "day"
  switch(time,
         day   = .plotDays.effort(x=x, ...),
         month = .plotMonths.effort(x=x, ...),
         year  = .plotYears.effort(x=x, ...),
         port  = .plotPort.effort(x=x, ...)
  )
  return(invisible())
}
