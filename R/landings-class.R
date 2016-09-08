print.landings <- function(x, ...){
  print(x$info)

  return(invisible())
}

summary.landings = function(object,...) {

  output = list()

  output$sumPorts = .getSumPorts.landings(object)
  output$ports    = .getPorts.landings(object)
  output$months   = .getMonth.landings(object)
  output$years    = .getYear.landing(object)

  class(output) = "summary.landings"
  return(output)
}

print.summary.landings = function(x, ...) {

  x2=x; class(x2)='landings'

  cat("\nDesembarque por dia:\n\n")
  print(x$sumPorts)

  cat("\nDesembarque por puertos (solo positivos):\n\n")
  print(x$ports[x$ports$Desembarque>0, ,drop=FALSE])

  cat("\nDesembarque mensual:\n\n")
  print(t(x$months),...)

  cat("\nDesembarque anual:\n\n")
  print(x$years,...)

  return(invisible(x))

}


plot.landings = function(x, time=NULL, ...) {

  if(is.null(time)) time="day"
  switch(time,
         day   = .plotDays.landings(x=x, ...),
         month = .plotMonths.landings(x=x, ...),
         year  = .plotYears.landings(x=x, ...),
         port  = .plotPort.landings(x=x, ...)
  )
  return(invisible())
}
