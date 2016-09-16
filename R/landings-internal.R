.getLandingsData <- function(file = file, toTons = TRUE, sp, ...) {

  out <- readSegFile(file = file, na.strings = "", stringsAsFactors = FALSE)
  out[is.na(out)] <-  0

  out1 <- out[c(1:3)]
  out2 <- out[seq(4, length(colnames(out)), by = 2)]/ifelse(isTRUE(toTons), 1000, 1)
  out  <- data.frame(out1, out2)

  ports <- out[seq(4, length(colnames(out)))]
  namesPorts <- vector()
  for(i in seq_len(ncol(ports))){
    namesP <- strsplit(x = names(ports), split = "_")[[i]][1]
    namesPorts <- c(namesPorts, namesP)
  }

  info <- list(file    =  file,
               records = nrow(out),
               months  = length(rle(out$month)$values),
               years   = length(unique(out$year)),
               ports   = length(namesPorts),
               sp      = sp)

  output <- list(data = out, info = info)
  class(output) <- c("landings")
  return(output)
}

.getSumPorts.landings <-  function (object, ...) {

  datos <- object$data
  ports <- cbind(datos[, seq(4, length(colnames(datos)))])

  tabla <- data.frame(datos[, c(1:3)], apply(ports, 1, sum))
  colnames(tabla) <- c("year", "month", "day", "Ports")

  return(tabla)
}

.getPorts.landings <- function (object, ...) {

  datos <- object$data

  ports <- datos[seq(4, length(colnames(datos)))]
  namesPorts <- vector()
  for(i in seq_len(ncol(ports))){
    namesP <- strsplit(x = names(ports), split = "_")[[i]][1]
    namesPorts <- c(namesPorts, namesP)
  }

  tabla <- data.frame(apply(ports,2,sum), row.names = NULL)
  colnames(tabla) <- "Desembarque"
  rownames(tabla) <- capitalize(namesPorts)

  return(tabla)
}

.getMonth.landings <- function (object, ...) {

  datos <- object$data

  ports  <- datos[seq(4, length(colnames(datos)))]
  months <- unique(datos$month)
  years  <- unique(datos$year)

  datos <- data.frame(datos[, c(1:3)], apply(ports, 1, sum))
  colnames(datos) <- c("year", "month", "day", "Ports")

  tabla <- tapply(datos$Ports, list(datos$month, datos$year),
                 sum, na.remove = FALSE)

  monthsTable <- row.names(tabla)
  sortMonth   <- sort(match(monthsTable, months), decreasing = FALSE)
  order       <- months[sortMonth]

  tabla <- data.frame(tabla[order, ])
  rownames(tabla) <- months
  colnames(tabla) <- years

  return(tabla)
}

.getYear.landing <- function (object, ...) {

  datos <- object$data

  ports <- datos[seq(4, length(colnames(datos)))]

  datos <- data.frame(datos[, c(1:3)], apply(ports, 1, sum))
  colnames(datos) <- c("year", "month", "day", "Ports")

  years <- unique(datos$year)

  tabla <- tapply(datos$Ports, list(datos$year), sum, na.remove=FALSE)
  tabla <- data.frame(tabla)
  colnames(tabla) <- "Desembarque"
  rownames(tabla) <- years

  return(tabla)
}


.trimData.landings <- function(x, start, end) {
  datos <- x$data
  months <- tolower(datos$month)
  monthsPosition <- unique(months)
  dataDate <- paste(datos$year, "-", match(months, tolower(month.abb)), "-", datos$day, sep = "")
  SumPorts <- .getSumPorts.landings(x)
  data <- data.frame(dataDate, SumPorts)

  data$date <- as.Date(as.character(data$dataDate), format = "%Y-%m-%d")
  data <- subset(data, data$date >= as.Date(start) & data$date <= as.Date(end))

  dataDate <- strptime(data$date, format = "%Y-%m-%d")
  years <- format(dataDate, "%Y")
  months <- data$month
  days <- format(dataDate, "%d")

  tabla <- data.frame(years, months, days, data$Ports)
  names(tabla) <- c("year", "month", "day", "Ports")

  return(tabla)
}


.plotDays.landings <- function (x, start=NULL, end=NULL, main=NULL, xlab=NULL, ylab=NULL, col = "blue",
                                daysToPlot = c(1,8,15,22), cex.axis = 0.8, ...) {
  if(is.null(start) & is.null(end)){
    months <- tolower(x$data$month)
    monthsPosition <- unique(months)
    dataDate <- as.Date(as.character(paste(x$data$year,"-",match(months, tolower(month.abb)), "-",x$data$day,sep="")),format="%Y-%m-%d")
    start<-min(dataDate)
    end<-max(dataDate)
  }
  datos <- .trimData.landings(x, start=start, end=end)
  days <- paste0(as.character(datos$day),"-",capitalize(as.character(datos$month)))
  daysToPlot <- which(as.numeric(datos$day) %in% daysToPlot)
  daysToPlot <- days[daysToPlot]

  days[! days %in% daysToPlot] <- NA

  if(is.null(main)) main="Desembarques Diarios"
  if(is.null(ylab)) ylab="Toneladas"
  barplot(datos$Ports, main=main, xlab=NA,
          ylab=ylab, col=col, names.arg = FALSE,
          ylim=c(0,max(datos$Ports)*1.2), cex.names=0.7, axes=FALSE, ...)
  AxisDate <- seq(0.7, by=1.2, length.out=length(days))
  NonNa =! is.na(days)
  axis(1, at=AxisDate[NonNa], labels=days[NonNa], las=2,cex.axis=cex.axis)
  axis(2, las=2, cex.axis=0.7)
  box()

  return(invisible())

}


.plotMonths.landings <- function (x, main=NULL, xlab=NULL, ylab=NULL, col = "blue", cex.axis = 0.8, ...) {

  datos <- .getMonth.landings(x)
  years <- as.numeric(colnames(datos))
  monthPlot <- NULL
  for(i in 1:length(years)){
    monthPort <- datos[,i]
    monthPlot <- c(monthPlot, monthPort)
  }
  monthPlot <- monthPlot[!is.na(monthPlot)]
  namesMonth <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  namesMonthPlot  <- capitalize(rep(namesMonth, length.out = length(monthPlot)))

  if(is.null(main)) main="Desembarques Mensuales"
  if(is.null(ylab)) ylab="Toneladas"
  barplot(monthPlot, main=main,
          xlab=NA, ylab=ylab, col=col, names.arg=FALSE,
          ylim=c(0, max(monthPlot)*1.2), cex.names=0.7, axes=FALSE, ...)
  axis(1, at=seq(0.7, by=1.2, length.out=length(monthPlot)), labels=namesMonthPlot,
       las=1, cex.axis=cex.axis, line=0)
  axis(1, at=seq(0.7,by=1.2, length.out=length(monthPlot)),
       labels=rep(years,each=12)[1:length(monthPlot)], las=1, cex.axis=cex.axis, line=1, tick=FALSE)
  axis(2, las=2, cex.axis=0.8)
  box()

  return(invisible())

}


.plotYears.landings <- function (x, main=NULL, xlab=NULL, ylab=NULL, col = "blue", cex.axis = 0.8, ...) {

  datos <- .getYear.landing(x)
  years <- as.numeric(rownames(datos))

  if(is.null(main)) main="Desembarques Anuales"
  if(is.null(ylab)) ylab="Toneladas"
  barplot(datos$Desembarque, main=main, xlab=NA,
          ylab=ylab, col=col, names.arg=FALSE,
          ylim=c(0,max(datos)*1.2), cex.names=0.7, axes=FALSE, ...)
  axis(1, at=seq(0.7, by=1.2, length.out=length(years)), labels=years, las=1,
       cex.axis=cex.axis)
  axis(2, las=2, cex.axis=0.8)
  box()

  return(invisible())

}


.trimPort.landings <- function(x, start, end, puerto) {
  datos <- x$data
  months <- tolower(datos$month)
  monthsPosition <- unique(months)
  dataDate <- paste(datos$year, "-", match(months, tolower(month.abb)), "-", datos$day, sep = "")

  ports <- datos[seq(4, length(colnames(datos)))]
  namesPorts <- vector()
  for(i in seq_len(ncol(ports))){
    namesP <- strsplit(x = names(ports), split = "_")[[i]][1]
    namesPorts <- c(namesPorts, namesP)
  }
  colnames(ports) <- namesPorts

  data <- data.frame(dataDate, ports)
  data$date <- as.Date(as.character(data$dataDate), format="%Y-%m-%d")
  data <- subset(data, data$date >= as.Date(start) & data$date <= as.Date(end))
  data <- data[, c("date", puerto)]

  dataDate <- strptime(data$date, format="%Y-%m-%d")
  years <- format(dataDate, "%Y")
  months <- format(dataDate, "%m")
  days <- format(dataDate, "%d")

  tabla <- data.frame(years, months, days, data[, puerto])
  names(tabla) <- c("year", "month", "day", puerto)

  return(tabla)
}


.plotPort.landings <- function (x, start=NULL, end=NULL, main=NULL, xlab=NULL, ylab=NULL, puerto=NULL, col = "blue", ...) {
  if(is.null(start) & is.null(end)){
    months <- tolower(x$data$month)
    monthsPosition <- unique(months)
    dataDate <- as.Date(as.character(paste(x$data$year,"-",match(months, tolower(month.abb)), "-",x$data$day,sep="")),format="%Y-%m-%d")
    start<-min(dataDate)
    end<-max(dataDate)
  }
  datos <- .trimPort.landings(x, start=start, end=end, puerto = puerto)
  days <- paste0(as.character(datos$day),"-",capitalize(as.character(datos$month)))
  daysToPlot <- c(1,8,15,22) #dias que seran ploteados
  daysToPlot <- which(as.numeric(datos$day) %in% daysToPlot) #posicion de los dias que seran ploteados
  daysToPlot <- days[daysToPlot] #formato de los dias que seran ploteados

  days[! days %in% daysToPlot] <- NA

  if(is.null(main)) main="Desembarques Diarios"
  if(is.null(xlab)) xlab="D\u{ED}a"
  if(is.null(ylab)) ylab="Toneladas"
  barplot(datos[,4], main=main, xlab=xlab,
          ylab=ylab, col=col, names.arg = FALSE,
          ylim=c(0,max(datos[,4])*1.2), cex.names=0.7, axes=FALSE, sub = toupper(puerto))
  AxisDate <- seq(0.7, by=1.2, length.out=length(days))
  NonNa=!is.na(days)
  axis(1, at=AxisDate[NonNa], labels=days[NonNa], las=2,cex.axis=0.7)
  axis(2, las=2, cex.axis=0.7)
  box()

  return(invisible())
}
