# A continuación  encontrará la secuencia de órdenes ejecutables para seguir paso 
# a paso el flujo de procesos documentado en "Primeros pasos en R: ejercicio práctico
#
# 
# Siéntase libre de modificar, cambiar, y probar nuevas sentencias para jugar con
# el conjunto de datos que ha servido como excusa para la elaboración de este 
# pequeño ejercicio. 

# Fecha de elaboración: Febrero 03 de 2021 - John Fredy Valbuena Lozano


######################################
## Lectura de datos en formato .csv ##
######################################

# Librería necesaria para ello
library(readr)
library(here)
# Definir directorios de trabajo para lectura y escritura de datos
rutas<-list(input=c(here("Datos/Eventos_Minas_Antipersonal_en_Colombia.csv")),
            output=c(here("Resultados/Ejemplo1.csv")))
# conectar los datos
datos<-read_csv(rutas$input)

# Identificar qué tipo de archivo es
typeof(datos)

# Identificar nombre de columnas contenidas en los datos
names(datos)

# Departamento con mayor ocurrencia de estos fenómenos
table(datos$DEPARTAMENTO)

# Año y frecuencia de ocurrencia, eventos  minas antipersona
table(datos$AÑO)

# Almacenar esta información en un objeto aparte
FrecA<-table(datos$AÑO)

# Convertir a data.frame
FrecA<-as.data.frame(FrecA)

###########################################
# Nuevas columnas en un conjunto de datos #
###########################################

# Frecuencia acumulada
FrecA$FrecAcum<-cumsum(FrecA$Freq)

# Porcentaje de ocurrencia por año
FrecA$Porcent<-prop.table(FrecA$Freq)*100

# Porcentaje acumulado de ocurrencia por año
FrecA$PorcAcum<-cumsum(FrecA$Porcent)

# Ver conjunto de datos
print(FrecA)

# Cambio de nombre a columnas específicas
names(FrecA)[c(1,2)]<-c("Año","Frecuencia")


#################################
# Graficar un conjunto de datos #
#################################

#Ejemplo 01: Histograma rápido
hist(datos$AÑO)

#Ejemplo 02: Histograma

hist(datos$AÑO,
     breaks = 31, # Número de barras en las que se presenta la información
     main = "Frecuencia de minas antipersona por año", # Título de histograma
     xlab =   "Año", # Etiqueta para eje X
     ylab = "Frecuencia", # Etiqueta para eje Y
     col = "deepskyblue4", # Color de barras
     border = "black") # Color de borde de barras
# Lineas de ubicación sobre gráfico
grid(col = "gray50") 

# Crear columna agrupando registros
FrecA$Agrupados[FrecA$PorcAcum<=40]<-"Registros menores al 40%"
FrecA$Agrupados[FrecA$PorcAcum>=40]<-"Registros mayores al 40%"

# Unir tablas a partir de columna "Año"
datosPorcAcum<-merge(datos,FrecA,  # Conjuntos de datos a unir 
                     by.x = "AÑO", # Nombre de columna llave en objeto "datos"
                     by.y = "Año") # Nombre de columna llave en objeto "FrecA"

# Identificar columnas de "FrecA" que no aportan información sobre porcentaje acumulado ni agrupación
names(datosPorcAcum)

# Eliminar columnas 15 a 17
datosPorcAcum<-datosPorcAcum[,-c(15:17)]

names(datosPorcAcum)

# Librería a implementar
library(ggplot2)
# Histograma de frecuencias diferenciando datos agrupados
ggplot(datosPorcAcum, aes(x=AÑO, fill=Agrupados, color=Agrupados)) +
  geom_histogram(position="identity", alpha=0.5)+
  scale_fill_brewer()

######################################
# Representar datos sobre el espacio #
######################################

# Librería
#En caso de no tener la librería instalada use la sentencia: install.packages("leaflet")  

library(leaflet)

# Filtrar datos por agrupación
datos_xy<-datosPorcAcum[datosPorcAcum$Agrupados=="Registros menores al 40%",]

# Representar datos seleccionados en un mapa dinámico
leaflet() %>% # Entorno general de mapa dinámico
  addTiles() %>% # Adherir mapa base, por defecto Open Street Maps
  addMarkers(
    datos_xy$LONGITUD_CABECERA,datos_xy$LATITUD_CABECERA, # Ubicar según coordenadas 
    clusterOptions = markerClusterOptions(), # Agrupar registros en clusters
    popup = datos_xy$TIPO_EVENTO) # Ventana emergente al click con información sobre tipo de evento

# Tipos de evento
print(table(datos_xy$TIPO_EVENTO))

# Exportar datos tabulares a un formato legible por excel
dir.create("Resultados")
write_csv2(FrecA, file = rutas$output) # guarda un archivo .csv legible en excel
