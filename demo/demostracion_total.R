require(RepSeg)

# Definición de parámetros ------------------------------------------------

# Dirección de archivo de entrada
fileCPUE <- "../desembarque_esfuerzo.csv" #contiene la base de datos de desembarque y esfuerzo.

fileDesembarque <- "../desembarque.csv" #contiene la base de datos de desembarque.

# Indicar la especie de la que proviene la información
sp   <- "merluza"

# Indicar el tipo de esfuerzo usado
tipoEsfuerzo <- "Capacidad de bodega"


# Análisis ----------------------------------------------------------------

#Lectura de datos
desembarque <- getData(file = fileDesembarque, sp = sp, type = "landings", tipoEsfuerzo = tipoEsfuerzo)

esfuerzo    <- getData(file = fileCPUE, sp = sp, type = "effort", tipoEsfuerzo = tipoEsfuerzo)

cpue        <- getData(file = fileCPUE, sp = sp, type = "cpue", tipoEsfuerzo = tipoEsfuerzo)

# Generación del reporte
makeReport(desembarque, esfuerzo, cpue, time = "month", cex.axis = 0.5)
