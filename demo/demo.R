rm(list = ls())

file = "data_ejemplo.csv"
sp <- "anchoveta"

#Lectura de datos
desembarque = getData(file, type = "landings", toTons = TRUE, sp = sp)

esfuerzo = getData(file, type = "effort", sp = sp)

cpue = getData(file, type = "cpue", toTons = TRUE, sp = sp)

#Tablas
getTable(desembarque)

getTable(esfuerzo)

#Gráficos
plot(desembarque, time = "month", main = "")

plot(esfuerzo, time = "month", main = "")

plot(cpue, time = "month", main = "")

# sp <- list("Engraulis ringens", "anchoveta", "anchovy", "anchoveta", "12 cm", "Red de cerco")
# names(sp) <- c("NombreCie", "NombreCom", "NombreIng", "NombreFAO", "TallaMin", "ArtePesca")
makeReport(x = desembarque)
