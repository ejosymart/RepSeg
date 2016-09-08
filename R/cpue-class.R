


summary.cpue = function(object,...) {

  output = list()

  output$day      = object$data
  output$ports    = object$dataPort
  output$months   = .getMonth.cpue(object)
  output$years    = .getYear.cpue(object)

  class(output) = "summary.cpue"
  return(output)

}


print.summary.cpue = function(x, ...) {

  x2=x; class(x2)='cpue'

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


plot.cpue = function(x, time=NULL, ...) {

  if(is.null(time)) time="day"
  switch(time,
         day   = .plotDays.cpue(x=x, ...),
         month = .plotMonths.cpue(x=x, ...),
         year  = .plotYears.cpue(x=x, ...),
         port  = .plotPort.cpue(x=x, ...)
  )
  return(invisible())
}
