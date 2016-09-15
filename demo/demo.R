rm(list = ls())
file = "../../reportePerico2/data/perico2.csv"

#Lectura de datos
desembarque = getData(file, type = "landings", toTons = TRUE)

esfuerzo = getData(file, type = "effort")

cpue = getData(file, type = "cpue", toTons = TRUE)

#Tablas
getTable(desembarque)

getTablaCuota(desembarque, cuota = 10000)

#Gráficos
plot(desembarque, time = "month", main = "")

plot(esfuerzo, time = "month", main = "")

plot(cpue, time = "month", main = "")

sp <- list("Engraulis ringens", "anchoveta", "anchovy", "anchoveta", "12 cm", "Red de cerco")
names(sp) <- c("NombreCie", "NombreCom", "NombreIng", "NombreFAO", "TallaMin", "ArtePesca")

makeReport(desembarque, sp = sp)
