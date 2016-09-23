
# Definición de parámetros ------------------------------------------------

# Dirección de archivo de entrada
file <- "../data_ejemplo.csv"

# Indicar la especie de la que proviene la información
sp   <- "caballa"

# Indicar el tipo de esfuerzo usado
tipoEsfuerzo <- "Capacidad de bodega"


# Análisis ----------------------------------------------------------------

require(RepSeg)

#Lectura de datos
desembarque <- getData(file = file, sp = sp, type = "landings", tipoEsfuerzo = tipoEsfuerzo)

esfuerzo   <- getData(file = file, sp = sp, type = "effort", tipoEsfuerzo = tipoEsfuerzo)

cpue       <- getData(file = file, sp = sp, type = "cpue", tipoEsfuerzo = tipoEsfuerzo)

# Generación del reporte
makeReport(desembarque, esfuerzo, cpue, time = "month", cex.axis = 0.5)
