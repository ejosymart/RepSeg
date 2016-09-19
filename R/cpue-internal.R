.getCPUEData <- function(file=file, toTons = TRUE, sp, tipoEsfuerzo, ...) {

  out       <- readSegFile(file = file, na.strings = "", stringsAsFactors = FALSE)
  outCatch  <- out[seq(4, length(colnames(out)), by = 2)]/ifelse(isTRUE(toTons), 1000, 1)
  outEffort <- out[seq(5, length(colnames(out)), by = 2)]
  catch     <- rowSums(outCatch, na.rm = TRUE)
  effort    <- rowSums(outEffort, na.rm = TRUE)
  cpue      <- catch/effort
  cpue[is.infinite(cpue)] <- NA
  out       <- data.frame(out[seq(1,3)], catch, effort, cpue)
  out[is.na(out)] <- 0

  #ByPortDay
  cpuePortDay <- outCatch/outEffort
  ports <- names(cpuePortDay)
  outPortDay <- data.frame(cpuePortDay)
  outPortDay[is.na(outPortDay)] <- 0

  namesPorts <- vector()
  for(i in seq_len(length(ports))){
    namesP <- strsplit(x = ports, split = "_")[[i]][1]
    namesPorts <- c(namesPorts, namesP)
  }
  colnames(outPortDay) <- capitalize(namesPorts)
  outPortDay <- data.frame(out[seq(1,3)], outPortDay)

  #ByPort
  catchPort  <- colSums(outCatch, na.rm = TRUE)
  effortPort <- colSums(outEffort, na.rm = TRUE)
  cpuePort <- catchPort/effortPort
  cpuePort[is.infinite(cpuePort)] <- NA

  outPort <- data.frame(cpuePort)
  outPort[is.na(outPort)] <- 0
  rownames(outPort) <- capitalize(namesPorts)

  info <- list(file        = file,
               records     = nrow(out),
               months      = length(rle(out$month)$values),
               years       = length(unique(out$year)),
               sp          = sp,
               effort_type = tipoEsfuerzo)

  output <- list(data = out, dataPortDay = outPortDay, dataPort = outPort, info = info)

  class(output) = c("cpue")

  return(output)
}

.getMonth.cpue <- function (object, ...) {

  datos <- object$data
  months <- unique(datos$month)
  years <- unique(datos$year)

  datosCatch <- tapply(datos$catch, list(datos$month, datos$year), sum, na.remove=FALSE)
  datosEffort <- tapply(datos$effort, list(datos$month, datos$year), sum, na.remove=FALSE)
  tabla <- datosCatch/datosEffort
  tabla[is.na(tabla)] <- 0

  monthsTable <- row.names(tabla)
  sortMonth <- sort(match(monthsTable, months), decreasing=FALSE)
  order <- months[sortMonth]

  tabla <- data.frame(tabla[order, ])
  rownames(tabla) <- months
  colnames(tabla) <- years

  return(tabla)
}

.getYear.cpue <- function (object, ...) {

  datos <- object$data
  years <- unique(datos$year)

  datosCatch  <- tapply(datos$catch, list(datos$year), sum, na.remove=FALSE)
  datosEffort <- tapply(datos$effort, list(datos$year), sum, na.remove=FALSE)
  tabla       <- datosCatch/datosEffort
  tabla       <- data.frame(tabla)
  colnames(tabla) <- "CPUE"
  rownames(tabla) <- years

  return(tabla)
}

.trimData.cpue <- function(x, start, end) {
  datos  <- x$data
  months <- tolower(datos$month)
  monthsPosition <- unique(months)
  dataDate <- paste(datos$year,"-",match(months, tolower(month.abb)), "-",datos$day,sep="")

  data <- data.frame(dataDate, datos)

  data$date <- as.Date(as.character(data$dataDate), format = "%Y-%m-%d")
  data <- subset(data, data$date >= as.Date(start) & data$date <= as.Date(end))

  dataDate <- strptime(data$date, format="%Y-%m-%d")
  years    <- format(dataDate, "%Y")
  months   <- data$month
  days     <- format(dataDate, "%d")

  tabla        <- data.frame(years, months, days, data$cpue)
  names(tabla) <- c("year", "month", "day", "cpue")

  return(tabla)
}

.plotDays.cpue <- function(x, start = NULL, end = NULL, main = NULL, xlab = NULL, ylab = NULL, col = "blue",
                            daysToPlot = c(1,8,15,22), cex.axis = 0.8, ...) {
  if(is.null(start) & is.null(end)){
    months <- tolower(x$data$month)
    monthsPosition <- unique(months)
    dataDate <- as.Date(as.character(paste(x$data$year,"-",match(months,tolower(month.abb)), "-",x$data$day,sep="")),format="%Y-%m-%d")
    start <- min(dataDate)
    end   <- max(dataDate)
  }
  datos <- .trimData.cpue(x, start=start, end=end)
  days  <- paste0(as.character(datos$day),"-",capitalize(as.character(datos$month)))
  daysToPlot <- which(as.numeric(datos$day) %in% daysToPlot) #posicion de los dias que seran ploteados
  daysToPlot <- days[daysToPlot] #formato de los dias que seran ploteados

  days[! days %in% daysToPlot] <- NA

  if(is.null(main)) main="CPUE Diario"
  if(is.null(ylab)) ylab=""
  barplot(datos$cpue, main=main, xlab=NA,
          ylab=ylab, col=col, names.arg = FALSE,
          ylim=c(0,max(datos$cpue)*1.2), cex.names=0.7, axes=FALSE, ...)
  AxisDate <- seq(0.7, by=1.2, length.out=length(days))
  NonNa =! is.na(days)
  axis(1, at=AxisDate[NonNa], labels=days[NonNa], las=2,cex.axis=cex.axis)
  axis(2, las=2, cex.axis=0.7)
  box()

  return(invisible())

}

.plotMonths.cpue <- function (x, main=NULL, xlab=NULL, ylab=NULL, col = "blue", cex.axis = 0.8, ...) {

  datos <- .getMonth.cpue(x)
  years <- as.numeric(colnames(datos))
  monthPlot <-  NULL
  for(i in 1:length(years) ) {
    monthPort <-datos[,i]
    monthPlot <- c(monthPlot, monthPort)
  }
  monthPlot <- monthPlot[!is.na(monthPlot)]
  namesMonth <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  namesMonthPlot  <- capitalize(rep(namesMonth, length.out = length(monthPlot)))

  if(is.null(main)) main="CPUE Mensual"
  if(is.null(ylab)) ylab=""
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

.plotYears.cpue <- function (x, main=NULL, xlab=NULL, ylab=NULL, col = "blue", cex.axis = 0.8, ...) {

  datos <- .getYear.cpue(x)
  years <- as.numeric(rownames(datos))

  if(is.null(main)) main="CPUE Anual"
  if(is.null(ylab)) ylab=""
  barplot(datos$CPUE, main=main, xlab=NA,
          ylab=ylab, col=col, names.arg=FALSE,
          ylim=c(0,max(datos)*1.2), cex.names=0.7, axes=FALSE, ...)
  axis(1, at=seq(0.7, by=1.2, length.out=length(years)), labels=years, las=1,
       cex.axis=cex.axis)
  axis(2, las=2, cex.axis=0.8)
  box()

  return(invisible())

}



.trimPort.cpue <- function(x, start, end, puerto) {
  datos    <- x$dataPortDay
  colnames(datos) <- tolower(colnames(datos))
  months   <- datos$month
  monthsPosition <- unique(months)
  dataDate <- paste(datos$year,"-",match(months, tolower(month.abb)), "-",datos$day,sep="")
  ports    <- datos[seq(4, length(colnames(datos)))]

  data <- data.frame(dataDate, ports)

  data$date <- as.Date(as.character(data$dataDate), format="%Y-%m-%d")
  data <- subset(data, data$date >= as.Date(start) & data$date <= as.Date(end))

  dataDate <- strptime(data$date, format="%Y-%m-%d")
  years    <- format(dataDate, "%Y")
  months   <- format(dataDate, "%m")
  days     <- format(dataDate, "%d")

  tabla <- data.frame(years, months, days, data[, puerto])
  names(tabla) <- c("year", "month", "day", puerto)

  return(tabla)
}

.plotPort.cpue <- function (x, start=NULL, end=NULL, main=NULL, xlab=NULL, ylab=NULL, puerto=NULL, col = "blue", ...) {
  if(is.null(start) & is.null(end)){
    months <- tolower(x$dataPortDay$month)
    monthsPosition <- unique(months)
    dataDate <- as.Date(as.character(paste(x$dataPortDay$year,"-",match(months,tolower(month.abb)), "-",x$dataPortDay$day,sep="")),format="%Y-%m-%d")
    start <- min(dataDate)
    end <- max(dataDate)
  }
  datos <- .trimPort.cpue(x, start=start, end=end, puerto=puerto)
  days <- paste0(as.character(datos$day),"-",capitalize(as.character(datos$month)))
  daysToPlot <- c(1,8,15,22)
  daysToPlot <- which(as.numeric(datos$day) %in% daysToPlot)
  daysToPlot <- days[daysToPlot]

  days[! days %in% daysToPlot] <- NA

  if(is.null(main)) main="CPUE Diario"
  if(is.null(xlab)) xlab="D\u{ED}a"
  if(is.null(ylab)) ylab="
  "
  barplot(datos[, 4], main=main, xlab=xlab,
          ylab=ylab, col=col, names.arg = FALSE,
          ylim=c(0,max(datos[, 4])*1.2), cex.names=0.7, axes=FALSE, sub = toupper(puerto))
  AxisDate=seq(0.7, by=1.2, length.out=length(days))
  NonNa =! is.na(days)
  axis(1, at=AxisDate[NonNa], labels=days[NonNa], las=2,cex.axis=0.7)
  axis(2, las=2, cex.axis=0.7)
  box()

  return(invisible())

}
