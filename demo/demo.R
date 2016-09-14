rm(list = ls())
file = "data_ejemplo.csv"

#Lectura de datos
desembarque = getData(file, type = "landings", toTons = TRUE)

esfuerzo = getData(file, type = "effort")

#Tablas
getTable(desembarque)

getTable(esfuerzo)

getTablaCuota(desembarque, cuota = 10000)

#Gráficos
plot(desembarque, time = "month", main = "")
