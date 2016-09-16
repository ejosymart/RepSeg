
# Definición de parámetros ------------------------------------------------

# Dirección de archivo de entrada
file <- "data/data_ejemplo2.csv"

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
plot(desembarque, time = "day")

plot(esfuerzo, time = "day")

plot(cpue, time = "day")

# Obtención de información importante
summary(desembarque)

summary(esfuerzo)

summary(cpue)

# Generación del reporte
makeReport(desembarque, esfuerzo, cpue, time = "month", cex.axis = 0.8)
