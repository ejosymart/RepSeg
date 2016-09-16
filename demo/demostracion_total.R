
# Definici�n de par�metros ------------------------------------------------

# Direcci�n de archivo de entrada
file <- "data/data_ejemplo.csv"

# Indicar la especie de la que proviene la informaci�n
sp <- "jurel"


# An�lisis ----------------------------------------------------------------

require(RepSeg)

#Lectura de datos
desembarque <- getData(file = file, sp = sp, type = "landings")

esfuerzo    <- getData(file = file, sp = sp, type = "effort")

cpue        <- getData(file = file, sp = sp, type = "cpue")

# Generaci�n de tablas
tablaDesembarque <- getTable(desembarque)

tablaEsfuerzo <- getTable(esfuerzo)

tablaCPUE <- getTable(cpue)

# Generaci�n de gr�ficos
plot(desembarque, time = "day")

plot(esfuerzo, time = "day")

plot(cpue, time = "day")

# Obtenci�n de informaci�n importante
summary(desembarque)

summary(esfuerzo)

summary(cpue)

# Generaci�n del reporte
makeReport(x = desembarque)

# Generaci�n del reporte para una especie no inclu�da en la base predeterminada
sp <- list(NombreCie = "Engraulis ringens", NombreCom = "anchoveta", NombreIng = "anchovy",
           TallaMin = "12", Unidad = "cm", TipoMedicion = "LT")

makeReport(x = desembarque, sp = sp)