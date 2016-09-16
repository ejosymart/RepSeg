
# Definición de parámetros ------------------------------------------------

# Dirección de archivo de entrada
file <- "../data_ejemplo.csv"

# Indicar la especie de la que proviene la información
sp <- "jurel"

# Indicar el tipo de esfuerzo usado
tipoEsfuerzo <- "viaje"


# Análisis ----------------------------------------------------------------

require(RepSeg)

#Lectura de datos
desembarque <- getData(file = file, sp = sp, type = "landings", tipoEsfuerzo = tipoEsfuerzo)

esfuerzo    <- getData(file = file, sp = sp, type = "effort", tipoEsfuerzo = tipoEsfuerzo)

cpue        <- getData(file = file, sp = sp, type = "cpue", tipoEsfuerzo = tipoEsfuerzo)

# Generación de tablas
tablaDesembarque <- getTable(desembarque)

tablaEsfuerzo <- getTable(esfuerzo)

tablaCPUE <- getTable(cpue)

# Generación de gráficos
plot(desembarque, time = "month")

plot(esfuerzo, time = "month")

plot(cpue, time = "month")

# Obtención de información importante
summary(desembarque)

summary(esfuerzo)

summary(cpue)

# Generación del reporte
makeReport(desembarque, esfuerzo, cpue, time = "month", cex.axis = 0.8)
