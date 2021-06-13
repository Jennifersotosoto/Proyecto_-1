#Proyecto de procesamiento
#Datos de la estacion meteorolica de Liberia 

# Cargar lass librerias necesarias para el trabajo 
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(gridExtra)

# Cargar los datos que se van a usar
inp <- read.csv("liberia_datos_climaticos.csv", sep= ",", na.strings = "", dec=",")

#Visualizar las primeras 5 filas
head(inp)

#Revisar cuantas filas y cuantas columnas tiene el archivo
dim(inp)

#Limpiar el archivo de N/A
inp[!complete.cases(inp),]
inp<- na.omit(inp) 

#Crear un data frame
inp_N<- na.omit(inp) 

#Asegurarse que no existan N/A

inp_N[!complete.cases(inp_N),]


# Definir las variables a numeros 
inp_N$Temperatura..Celsius.<-as.numeric(inp$Temperatura..Celsius.)
inp_N$HumedadRelativa....<-as.numeric(inp$HumedadRelativa....)
inp_N$VelocidadViento..m.s.<-as.numeric(inp$VelocidadViento..m.s.)
inp_N$Lluvia..mm.<-as.numeric(inp$Lluvia..mm.)
inp_N$Irradiacion..W.m2. <-as.numeric(inp$Irradiacion..W.m2.)
inp_N$EvapoTranspiracion..mm.<-as.numeric(inp$EvapoTranspiracion..mm.)


#ReconFiarmar el data frame
str(inp_N)

#Renombrar las variables 
inp_ren<- inp_N %>%
  rename(Fecha=Date,
         Temperatura= Temperatura..Celsius.,
         Humedad= HumedadRelativa....,
         Viento= VelocidadViento..m.s.,
         Lluvia= Lluvia..mm.,
         Irradiación= Irradiacion..W.m2.,
         Evaporación= EvapoTranspiracion..mm.
         )

#Visualización  de las variables individualmente
# Temperatura 

ggplot(inp_ren,aes(x= Temperatura))+
  geom_histogram (binwidth= 1,
                  color = "red",
                  fill = "green") +
  facet_grid()+
  ggtitle("La temperatura en Liberia") +
  xlab("Rango") +
  ylab("Celsius")+
  theme_ipsum()

#Humedad Relativa 
ggplot(inp_ren,aes (x= Humedad))+
  geom_histogram (binwidth = 2,
                  color = "blue",
                  fill = "sky blue") +
  facet_grid()+
  ggtitle("Humedad en Liberia") +
  xlab("Rango") +
  ylab("Porcentaje")+
theme_ipsum()

#Velocidad del viento
ggplot(inp_ren, aes(x=Viento))+
  geom_histogram (binwidth = 2,
                  color = "red",
                  fill = "orange") +
  facet_grid()+
  ggtitle("Velocidad del viento en Liberia") +
  xlab("Rango") +
  ylab("m.s")+
  theme_ipsum()

#Lluvia
ggplot(inp_ren,aes(x= Lluvia))+
  geom_histogram (binwidth = 10,
                  color = "blue",
                  fill = "white") +
  facet_grid()+
  ggtitle("Lluvias en Liberia") +
  xlab("Rango") +
  ylab("m.m")+
  theme_ipsum()

ggplot(inp_ren,aes(x= Irradiación))+
  
  geom_histogram (binwidth = 9 ,
                  color = "red",
                  fill = "green") +
  facet_grid()+
  ggtitle(" Irradiación en Liberia") +
  xlab("Rago") +
  ylab("m.m")+
theme_ipsum()


ggplot(inp_ren,aes(x= Evaporación))+
  geom_histogram (binwidth= 2 ,
                  color = "black",
                  fill = "sky blue") +
  facet_grid()+
  ggtitle("La evapotranspiración en Liberia") +
  xlab(" Evapotranspiración") +
  ylab("Frecuencia")+
theme_ipsum()

#Graficos juntos
Temp<-ggplot(inp_ren,aes(x= Temperatura))+
  geom_histogram (binwidth= 1,
                  color = "red",
                  fill = "green") +
  facet_grid()+
  ggtitle("La temperatura en Liberia") +
  xlab("Rango") +
  ylab("Celsius")+
  theme_ipsum()

#Humedad Relativa 
Hume<-ggplot(inp_ren,aes (x= Humedad))+
  geom_histogram (binwidth = 2,
                  color = "blue",
                  fill = "sky blue") +
  facet_grid()+
  ggtitle("Humedad en Liberia") +
  xlab("Rango") +
  ylab("Porcentaje")+
  theme_ipsum()

#Velocidad del viento
vie<-ggplot(inp_ren, aes(x=Viento))+
  geom_histogram (binwidth = 2,
                  color = "red",
                  fill = "orange") +
  facet_grid()+
  ggtitle("Velocidad del viento en Liberia") +
  xlab("Rango") +
  ylab("m.s")+
  theme_ipsum()

#Lluvia
LlU<- ggplot(inp_ren,aes(x= Lluvia))+
  geom_histogram (binwidth = 10,
                  color = "blue",
                  fill = "white") +
  facet_grid()+
  ggtitle("Lluvias en Liberia") +
  xlab("Rango") +
  ylab("m.m")+
  theme_ipsum()

Irra<- ggplot(inp_ren,aes(x= Irradiación))+
  
  geom_histogram (binwidth = 9 ,
                  color = "red",
                  fill = "yellow") +
  facet_grid()+
  ggtitle(" Irradiación en Liberia") +
  xlab("Rago") +
  ylab("m.m")+
  theme_ipsum()


Evap<- ggplot(inp_ren,aes(x= Evaporación))+
  geom_histogram (binwidth= 2 ,
                  color = "black",
                  fill = "sky blue") +
  facet_grid()+
  ggtitle("La evapotranspiración en Liberia") +
  xlab("Rango") +
  ylab("m.m")+
  theme_ipsum()

#Unir todos los graficos
grid.arrange(Temp, Hume, vie, LlU, Irra, Evap)

#Promediar las variables por mes

Datos_Prome<-inp_ren%>%
  select(Fecha, 
         Temperatura,
         Humedad, 
         Viento,
         Lluvia, 
         Irradiación,
         Evaporación)%>%
  mutate(Fecha= as.Date(Fecha, format= "%d/%m%/Y"))%>%
  group_by(Fecha= format(Fecha, "%m"))%>%
  summarise(Temperatura=mean(Temperatura), 
            Humedad= mean(Humedad),
            Viento= mean(Viento),
            Lluvia=mean(Lluvia),
            Irradiación= mean(Irradiación),
            Evaporación=mean(Evaporación),
            )
#Preparar las graficas en linea

MMQ_Tem<- ggplot(Datos_Prome, aes(x=Fecha, y= Temperatura, group= 1))+
  geom_line(color="yellow")+
  xlab(" El promedio de la Temperatura por mes en Liberia")+
ylab("°C")

MMQ_Hume<- ggplot(Datos_Prome, aes(x=Fecha, y= Humedad, group= 1))+
  geom_line(color="brown")+
  xlab("El promedio de la humedad por mes en Liberia")+
  ylab("%")

MMQ_Velo<- ggplot(Datos_Prome, aes(x=Fecha, y= Viento, group= 1))+
  geom_line(color=" sky blue")+
  xlab("El promedio de la velocidad del viento por mes en Liberia")+
  ylab("m/s")

MMQ_Llu<- ggplot(Datos_Prome, aes(x=Fecha, y= Lluvia, group= 1))+
  geom_line(color="purple")+
  xlab("El promedio de la lluvia por mes en Liberia")+
  ylab("m.m")

MMQ_Irra<- ggplot(Datos_Prome, aes(x=Fecha, y= Irradiación, group= 1))+
  geom_line(color="red")+
  xlab(" El promedio de la Irradiación por mes en Liberia")+
  ylab("w.m2")

MMQ_Evapo<- ggplot(Datos_Prome, aes(x=Fecha, y= Evaporación, group= 1))+
  geom_line(color="green")+
  xlab(" El promedio de la evapotranspiración por mes en Liberia")+
  ylab("m.m")


#Mostrar los 6 paneles 

grid.arrange(MMQ_Tem, MMQ_Hume, MMQ_Velo, MMQ_Llu, MMQ_Irra, MMQ_Evapo, nrow= 6, ncol=1)

#Graficos de nubes de puntos 
Nb_Tem<- ggplot(inp_ren, aes(x=Fecha, y= Temperatura, group= 1))+
  geom_point(color= "green")+
  xlab("Fechas variable")+
  ylab("°C")

Nb_hum<- ggplot(inp_ren, aes(x=Fecha, y= Humedad, group= 1))+
  geom_point(color= "green")+
  xlab("Fechas variable")+
  ylab("%")
Nb_vie<- ggplot(inp_ren, aes(x=Fecha, y= Viento, group= 1))+
  geom_point(color= "green")+
  xlab("Fechas variable de viento")+
  ylab("m.s")

Nb_llu<- ggplot(inp_ren, aes(x=Fecha, y= Lluvia, group= 1))+
  geom_point(color= "green")+
  xlab("Fechas variable de lluvia")+
  ylab("mm")

Nb_irra<- ggplot(inp_ren, aes(x=Fecha, y= Irradiación, group= 1))+
  geom_point(color= "green")+
  xlab("Fechas variable de irradiacion")+
  ylab("w.m2")

Nb_eva<- ggplot(inp_ren, aes(x=Fecha, y= Evaporación, group= 1))+
  geom_point(color= "green")+
  xlab("Fechas variable")+
  ylab("mm")
grid.arrange(Nb_Tem, Nb_hum, Nb_vie, Nb_llu, Nb_irra, Nb_eva, nrow=6, ncol=1)
