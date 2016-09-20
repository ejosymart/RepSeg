.monthToNumber <- function(x){
  index <- match(x, month.abb)
  return(index)
}

.numberToMonth <- function(x){
  index <- match(x, 1:12)
  return(month.name_spanish[index])
}


.getCalendar <- function(year = NULL, ...){
  ndays   <- NULL
  nmonths <- NULL
  nyears  <- NULL
  for(j in seq_along(year)){
    for(i in 1:12){
      fecha   <- paste(year[j], "-", i, "-01", sep = "")
      xdays   <- 1:monthDays(as.Date(fecha))
      ndays   <- c(ndays, xdays)
      xmonth  <- monthDays(as.Date(fecha))
      ymonth  <- rep(i, xmonth)
      nmonths <- c(nmonths, ymonth)
    }
    xyears <- yearDays(as.Date(fecha))
    yyears <- rep(year[j], xyears)
    nyears <- c(nyears, yyears)
  }
  return(data.frame(anho = nyears, mes = nmonths, dia = ndays))
}


convertBase <- function(file = file, sp, tipo, nameFileOut,...){
  base           <- read.csv(file = file, na.strings = "")
  colnames(base) <- tolower(colnames(base))
  base           <- base[base$especie == sp & base$tipo_flota == tipo, ]

  catch  <- aggregate(captura ~ dia + mes + anho + puerto, data = base, FUN = sum)
  effort <- aggregate(puerto ~ dia + mes + anho + puerto, data = base, FUN = table)

  effort1 <- effort[, 1:3]
  effort2 <- as.data.frame(effort$puerto)
  effort  <- data.frame(effort1, effort2)
  effort  <- effort[, which(!apply(effort, 2, FUN = function(x){all(x == 0)}))]

  puerto  <- unique(catch$puerto)

  baseCatch <- list()
  for(i in seq_along(puerto)){
    catchPort      <- catch[catch$puerto == puerto[i], ]
    newBase        <- .getCalendar(unique(catch$anho))
    catchPort      <- merge(newBase, catchPort, all = TRUE)
    Date           <- catchPort[, 1:3]
    Date$mes       <- tolower(month.abb[Date$mes])
    colnames(Date) <- c("year", "month", "day")
    baseCatch[[i]] <- data.frame(catchPort[, "captura"])
    colnames(baseCatch[[i]]) <- c(paste(as.character(puerto[i]),"_c", sep = ""))
    frameCatch     <- do.call("cbind", baseCatch)
    frameEffort    <- merge(newBase, effort, all = TRUE)
    frameEffort    <- cbind(frameEffort[, -c(1:3)])
    colnames(frameEffort) <- paste(as.character(puerto),"_e", sep = "")
    catcheffort    <- data.frame(frameCatch, frameEffort)
    catcheffort    <- catcheffort[, order(names(catcheffort))]
    output         <- data.frame(Date, catcheffort)
    output[is.na(output)] <- 0
    out            <- write.csv(x = output, nameFileOut, row.names = F)
  }

  return(list(base = base, newbase = output))
}


#' Función para obtener tablas resumen
#'
#' @param data Objeto de clase \code{landings}, \code{effort} o \code{cpue} desde donde
#' se desea obtener la tabla resumen.
#'
#' @export
getTable <- function(data){
  data_summary <- summary(data)
  dataTable    <- data_summary[[3]]

  outTable     <- rbind(round(dataTable, 0), total = round(colSums(dataTable), 0))
  row.names(outTable) <- c(month.name_spanish, "Total (t)")

  if(class(data) == "effort"){
    row.names(outTable)[13] = "Total"
  }else{
    outTable
  }

  return(outTable)
}


#' Función para obtener tablas resumen mostrando las estadisticas quincenales dle último mes
#'
#' @param Objeto de clase \code{landings} o \code{effort} desde donde
#' se desea obtener la tabla resumen.
#' @export
getTable2 <- function(data){
  if (!inherits(data, c("landings", "effort")))
    stop("Usar solo con objetos de clase 'landings' o 'effort'")
  data_summary  <- summary(data)
  sumdays       <- data_summary[[1]]
  sumdays$month <- .monthToNumber(sumdays$month)
  today         <- strsplit(date(), " ")
  actualYear    <- as.numeric(today[[1]][5])
  actualMonth   <- .monthToNumber(today[[1]][2])
  actualDay     <- as.numeric(today[[1]][3])
  byMonth       <- sumdays[which(sumdays$year == actualYear & sumdays$month == actualMonth & sumdays$day <= actualDay), ]

  if(nrow(byMonth) > 15){
    byMonthfirst   <- head(byMonth, 15)
    byMonthlast    <- tail(byMonth, nrow(byMonth) - 15)
    tab1           <- data.frame(month = c(paste(byMonthfirst$day[1], "-", rev(byMonthfirst$day)[1], .numberToMonth(byMonthfirst$month[1]), sep = " "),
                                           paste(byMonthlast$day[1], "-", rev(byMonthlast$day)[1], .numberToMonth(byMonthfirst$month[1]), sep = " ")),
                                 Ports = c(sum(byMonthfirst$Ports), sum(byMonthlast$Ports)))
  }else{
    byMonthfirst  <- head(byMonth, 15)
    tab1          <- data.frame(month = paste(byMonthfirst$day[1], "-", rev(byMonthfirst$day)[1], .numberToMonth(byMonthfirst$month[1]), sep = " "),
                                Ports = c(sum(byMonthfirst$Ports)))
  }

  byMonth2       <- sumdays[which(sumdays$year == actualYear & sumdays$month < actualMonth), ]
  tab2           <- aggregate(Ports ~ month + year, byMonth2, sum)
  tab2$month     <- .numberToMonth(tab2$month)
  tab2           <- tab2[, c(1,3)]

  outTable <- rbind(tab2, tab1)
  colnames(outTable) <- c("Mes", actualYear)
  return(outTable)
}
