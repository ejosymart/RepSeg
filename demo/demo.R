rm(list = ls())

file = "data_ejemplo.csv"

#Lectura de datos
desembarque = getData(file, type = "landings", toTons = TRUE)

esfuerzo = getData(file, type = "effort")

cpue = getData(file, type = "cpue", toTons = TRUE)

#Tablas
getTable(desembarque)

#Gráficos
plot(desembarque, time = "month", main = "")

plot(esfuerzo, time = "month", main = "")

plot(cpue, time = "month", main = "")

makeReport(desembarque = desembarque)
