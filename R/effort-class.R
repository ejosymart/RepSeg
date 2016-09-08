.getEffortData = function(file=file, toTons = T, ...) {

  out = read.csv(file = file, na.strings = "", stringsAsFactors = FALSE)
  outEffort = out[seq(5, length(colnames(out)), by = 2)]
  out = data.frame(out[seq(1,3)], outEffort)

  ports = out[seq(4, length(colnames(out)))]
  namesPorts = vector()
  for(i in seq_len(ncol(ports))){
    namesP = strsplit(x = names(ports), split = "_")[[i]][1]
    namesPorts = c(namesPorts, namesP)
  }

  info = list(file=file, records=nrow(out),
              months = length(rle(out$month)$values),
              years  = length(unique(out$year)),
              ports  = length(namesPorts))

  output = list(data=out, info=info)
  class(output) = c("effort")
  return(output)
}


summary.effort = function(object,...) {

  output = list()

  output$sumPorts = .getSumPorts.effort(object)
  output$ports    = .getPorts.effort(object)
  output$months   = .getMonth.effort(object)
  output$years    = .getYear.effort(object)

  class(output) = "summary.effort"
  return(output)
}

print.summary.effort = function(x, ...) {

  x2=x; class(x2)='effort'

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


plot.effort = function(x, time=NULL, ...) {

  if(is.null(time)) time="day"
  switch(time,
         day   = .plotDays.effort(x=x, ...),
         month = .plotMonths.effort(x=x, ...),
         year  = .plotYears.effort(x=x, ...),
         port  = .plotPort.effort(x=x, ...)
  )
  return(invisible())
}
