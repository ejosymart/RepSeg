rm(list = ls())
file = "D:/NvTomus InDemonic/Trabajos/JTorrejon/2016/reportePerico2/data/perico2.csv"

#Lectura de datos
desembarque = getData(file, type = "landings", toTons = TRUE)
esfuerzo = getData(file, type = "effort")
cpue = getData(file, type = "cpue", toTons = TRUE)

#Resumen de datos
summary(desembarque)
summary(esfuerzo)
summary(cpue)


#Gráficos
plot(desembarque)
plot(desembarque, start = "2016-02-15", end = "2016-03-05")
plot(desembarque, time = "month")
plot(desembarque, time = "year")
plot(desembarque, time = "port", puerto = "ilo")
plot(desembarque, time = "port", puerto = "pucusana", start = "2016-02-15", end = "2016-03-05")


plot(esfuerzo)
plot(esfuerzo, start = "2016-02-15", end = "2016-03-05")
plot(esfuerzo, time = "month")
plot(esfuerzo, time = "year")
plot(esfuerzo, time = "port", puerto = "pucusana")
plot(esfuerzo, time = "port", puerto = "pucusana", start = "2016-02-15", end = "2016-03-05")


plot(cpue)
plot(cpue, start = "2016-02-15", end = "2016-03-05")
plot(cpue, time = "month")
plot(cpue, time = "year")
plot(cpue, time = "port", puerto = "paita")
plot(cpue, time = "port", puerto = "pucusana")
plot(cpue, time = "port", puerto = "ilo")
plot(cpue, time = "port", puerto = "paita", start = "2016-02-15", end = "2016-03-05")
plot(cpue, time = "port", puerto = "pucusana", start = "2016-02-15", end = "2016-03-05")
plot(cpue, time = "port", puerto = "ilo", start = "2016-02-15", end = "2016-03-05")

