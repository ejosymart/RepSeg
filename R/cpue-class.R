#' @title M�todo \code{summary} para objetos de clase \code{cpue}.
#'
#' @param object Objeto de clase \code{cpue}.
#' @param ... Argumentos extra.
#' @return CPUE diario, mensual, anual y por puertos.
#' @export
#'
#' @method summary cpue
summary.cpue <- function(object,...) {

  output <- list()

  output$day      <- object$data
  output$ports    <- object$dataPort
  output$months   <- .getMonth.cpue(object)
  output$years    <- .getYear.cpue(object)

  class(output)   <- "summary.cpue"
  return(output)

}

#' @title M�todo \code{print} para objetos de clase \code{summary.cpue}.
#'
#' @param x Objeto de clase \code{summary.cpue}.
#' @param ... Argumentos extra.
#'
#' @export
#'
#' @method print summary.cpue
print.summary.cpue <- function(x, ...) {

  x2 <- x; class(x2) <- 'cpue'

  cat("\nCPUE diario:\n\n")
  print(x$day,...)

  cat("\nCPUE por puertos (solo positivos):\n\n")
  print(x$ports[x$ports>0, ,drop=FALSE])

  cat("\nCPUE mensual:\n\n")
  print(t(x$months),...)

  cat("\nAnnual CPUE:\n\n")
  print(x$years,...)

  return(invisible(x))

}

#' @title M�todo \code{plot} para objetos de clase \code{cpue}.
#'
#' @param x Objeto de clase \code{cpue}.
#' @param time Argumento que hace referencia al paso de tiempo para ser graficados,
#' \code{"day"}, \code{"month"}, \code{"year"}.
#' @param ... Argumentos extra.
#'
#' @export
#'
#' @method plot cpue
plot.cpue <- function(x, time = NULL, ...) {

  if(is.null(time)) time = "day"
  switch(time,
         day   = .plotDays.cpue(x=x, ...),
         month = .plotMonths.cpue(x=x, ...),
         year  = .plotYears.cpue(x=x, ...),
         port  = .plotPort.cpue(x=x, ...)
  )
  return(invisible())
}
