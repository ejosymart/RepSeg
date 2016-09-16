
# Definición de parámetros ------------------------------------------------

# Dirección de archivo de entrada
file <- "data/data_ejemplo.csv"

# Indicar la especie de la que proviene la información
sp <- "jurel"


# Análisis ----------------------------------------------------------------

require(RepSeg)

#Lectura de datos
desembarque <- getData(file = file, sp = sp, type = "landings")

esfuerzo    <- getData(file = file, sp = sp, type = "effort")

cpue        <- getData(file = file, sp = sp, type = "cpue")

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
makeReport(x = desembarque)

# Generación del reporte para una especie no incluída en la base predeterminada
sp <- list(NombreCie = "Engraulis ringens", NombreCom = "anchoveta", NombreIng = "anchovy",
           TallaMin = "12", Unidad = "cm", TipoMedicion = "LT")

makeReport(x = desembarque, sp = sp)
