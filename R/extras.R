.getCalendar = function(year = NULL, ...){
  ndays   = NULL
  nmonths = NULL
  nyears  = NULL
  for(j in seq_along(year)){
    for(i in 1:12){
      fecha   = paste(year[j], "-", i, "-01", sep = "")
      xdays   = 1:monthDays(as.Date(fecha))
      ndays   = c(ndays, xdays)
      xmonth  = monthDays(as.Date(fecha))
      ymonth  = rep(i, xmonth)
      nmonths = c(nmonths, ymonth)
    }
    xyears = yearDays(as.Date(fecha))
    yyears = rep(year[j], xyears)
    nyears = c(nyears, yyears)
  }
  return(data.frame(anho = nyears, mes = nmonths, dia = ndays))
}


convertBase = function(file = file, sp, tipo, nameFileOut,...){
  base = read.csv(file = file, na.strings = "")
  colnames(base) = tolower(colnames(base))
  base = base[base$especie == sp & base$tipo_flota == tipo, ]
  base$embarcaciones[is.na(base$embarcaciones)] = 1
  
  if(mean(base$embarcaciones) == 1){
    catch  = aggregate(captura ~ dia + mes + anho + puerto, data = base, FUN = sum)
    effort = aggregate(puerto ~ dia + mes + anho + puerto, data = base, FUN = table)
    
    effort1 = effort[, 1:3]
    effort2 = as.data.frame(effort$puerto)
    effort  = data.frame(effort1, effort2)
    effort  = effort[,which(!apply(effort,2,FUN = function(x){all(x == 0)}))]
    
    puerto = unique(catch$puerto) 
    
    baseCatch = list()
    for(i in seq_along(puerto)){
      catchPort = catch[catch$puerto == puerto[i], ]
      newBase = .getCalendar(unique(catch$anho))
      catchPort = merge(newBase, catchPort, all=TRUE)
      Date = catchPort[, 1:3]
      Date$mes = tolower(month.abb[Date$mes])
      colnames(Date) = c("year", "month", "day")
      baseCatch[[i]] = data.frame(catchPort[, "captura"])
      colnames(baseCatch[[i]]) = c(paste(as.character(puerto[i]),"_c", sep = ""))
      frameCatch = do.call("cbind", baseCatch)
      frameEffort = merge(newBase, effort, all = TRUE)
      frameEffort = cbind(frameEffort[, -c(1:3)])
      colnames(frameEffort) = paste(as.character(puerto),"_e", sep = "")
      catcheffort = data.frame(frameCatch, frameEffort)
      catcheffort = catcheffort[, order(names(catcheffort))]
      output = data.frame(Date, catcheffort)
      output[is.na(output)] = 0
      out = write.csv(x = output, nameFileOut, row.names = F)
    }
  }else{
    base = base[rep(row.names(base), base$embarcaciones), (1:ncol(base))]
    base$captura = base$captura/base$embarcaciones
    
    catch  = aggregate(captura ~ dia + mes + anho + puerto, data = base, FUN = sum)
    effort = aggregate(puerto ~ dia + mes + anho + puerto, data = base, FUN = table)
    
    effort1 = effort[, 1:3]
    effort2 = as.data.frame(effort$puerto)
    effort  = data.frame(effort1, effort2)
    effort  = effort[,which(!apply(effort,2,FUN = function(x){all(x == 0)}))]
    
    puerto = unique(catch$puerto) 
    
    baseCatch = list()
    for(i in seq_along(puerto)){
      catchPort = catch[catch$puerto == puerto[i], ]
      newBase = .getCalendar(unique(catch$anho))
      catchPort = merge(newBase, catchPort, all=TRUE)
      Date = catchPort[, 1:3]
      Date$mes = tolower(month.abb[Date$mes])
      colnames(Date) = c("year", "month", "day")
      baseCatch[[i]] = data.frame(catchPort[, "captura"])
      colnames(baseCatch[[i]]) = c(paste(as.character(puerto[i]),"_c", sep = ""))
      frameCatch = do.call("cbind", baseCatch)
      frameEffort = merge(newBase, effort, all = TRUE)
      frameEffort = cbind(frameEffort[, -c(1:3)])
      colnames(frameEffort) = paste(as.character(puerto),"_e", sep = "")
      catcheffort = data.frame(frameCatch, frameEffort)
      catcheffort = catcheffort[, order(names(catcheffort))]
      output = data.frame(Date, catcheffort)
      output[is.na(output)] = 0
      out = write.csv(x = output, nameFileOut, row.names = F)
    }
  }

  return(list(base=base, newbase = output))
}
