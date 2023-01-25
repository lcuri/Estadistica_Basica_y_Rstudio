##creando vectores
#-------------------------------------------------------------------------------------------------------------"
v<-c(1,2,3,4,5,6)
v2<-c("rojo","amarillo","verde","azul","blanco",NA)

##uniendo vectores y conviertiendo en data frame
base2<-data.frame(
  v,v2)
cbind(v,v2)
##uniendo vectores sin convertir en vectores
base<- cbind(v2,v)
base<-as.data.frame(base) ##convirtiendo en data frame

##adicionando nombres a las columnas
colnames(base)<-c("numeros","colores")
colnames(base)##consultando el nombre de las columnas
#-------------------------------------------------------------------------------------------------------------"

#OPERACIONES ARITMETICAS EN RSTUDIO
2+3 #SUMA
2-3 #RESTA
2*4 #MULTIPLICACION
2/5 #DIVISION
2**3 #POTENCIA
#.------------------------------------------------------------------------------------------------------------

##OPERACIONES LOGICOS
x<-1 #asignando valor a x
y<-2
x>3 #mayor
x<=5 #menor iguak
x==6 #igual
x=y #se le asigna el valor de y a x 
X!=6 #para consultar si es diferente x es diferente de 6???
(x>2)&(y<8) #2 función logica si x es mayor que 2 e y es menor que 2 ...compara ambos y da un resultado. FALSO
(x>2)|(y<8) #Si alguno de los 2 es verdadero, resultado verdadero.
!(x>3) #negacion a un resultado...si su resultado sale falso, el ! lo niega y es verdadero.
#--------------------------------------------------------------------------------------------------------------

#FUNCIONES
min(5,9,6,7,8)
max(5,8,9,2)
sum(2,5,8,7)
log(5) #logaritmo
exp(8) #exponencial
sqrt(8) #raiz cuadrada

seq(from=5, to=12, by=1) #crea una secuencia de numeros 
rep(c(2,8), times=c(9,9)) #repeticiones 

#--------------------------------------------------------------------------------------------------------------
#IMPORTACION DE DATOS DATAFRAMES desde su ruta
##opcion1
base<-read.csv("D:/Carpeta personal/1. Profesión/5. Diplomado Estadistica Aplicada/3.Módulos/1.  Procedimientos estadísticos/2. introduccion a la probabilidad/2. Práctica/3. ejercicios resueltos/1. analisis Univariado/datosbmi.csv")
base3<-read.table("D:/Carpeta personal/1. Profesión/5. Diplomado Estadistica Aplicada/3.Módulos/1.  Procedimientos estadísticos/1. conceptos basicos de estadistica/2. Práctica/2. Ejercicios propuestos/1. Ejercicio_propuesto_1/USArrests.txt",
header = T, sep = ",", dec = ".") ##header = tiene encabezado? = true o false, sep= columnas separado por comas, decimales separado por punto

##paquete requerido para excel
library(tidyverse)
base2<-readxl::read_excel("D:/Carpeta personal/1. Profesión/5. Diplomado Estadistica Aplicada/3.Módulos/1.  Procedimientos estadísticos/1. conceptos basicos de estadistica/2. Práctica/2. Ejercicios propuestos/1. Ejercicio_propuesto_1/USArrests.xlsx")

#opcion2: uso de faichus
setwd("D:/Carpeta personal/1. Profesión/5. Diplomado Estadistica Aplicada/3.Módulos/1.  Procedimientos estadísticos/2. introduccion a la probabilidad/2. Práctica/3. ejercicios resueltos/1. analisis Univariado")
base4<-read.csv("datosbmi.csv")
setwd("D:/Carpeta personal/1. Profesión/5. Diplomado Estadistica Aplicada/3.Módulos/1.  Procedimientos estadísticos/1. conceptos basicos de estadistica/2. Práctica/2. Ejercicios propuestos/1. Ejercicio_propuesto_1")
base5<-read.table("USArrests.txt",
header = T, sep = ",", dec = ".")

#opcion 3 directorio automatico
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##se coloca el directorio de trabajo donde el archivo r(r desde cero) se encuentre 
base5<-read.csv("prueba1.csv")
#remover dataset
remove(base4)

#guardar un archivo plano
write.csv(base4,"prueba5.csv") #lo guardó en el directorio establecido en setwd
#..--------------------------------------------------------------------------------------------------------------
##EXPLORACION DE MIS DATASETS
head(base) #muestra los 5 primeros datos
tail(base) #muestra los 5 ultimos datos
nrow(base) #numeros de filas
ncol(base) #numero de columnas
dim(base) #cantidad de filas y columnas
str(base) #tipo de datos de cada variable/y muestra de datos
str(base$sexo)
view(base) #muestra la tabla
unique(base$bmi) ##cantidad unica de datos de a variable
base$bmi ##muestra los datos de la variable
#FILTRO DE COLUMNAS
base[1:100,c(1,3)] ##subset o filtrar columnas del DF y las filas del 1 a 100
base[,c("bmi","sexo")] ##filtrar por nombre de la columna

#FILTRO DE FILAS
base[c(1,2,3,4,5),] #muestra las primeras 5 filas y todas las columnas
base[c(1:100),] ##muestra las 100 primeras filas
base[base$sexo==0,] #filtrar el data set en funcion a la variable sexo, solo cero
##usando la funcion subset
base7<-subset(base,bmi>25 & sexo==0) #usando la funcion subset con multiples condiciones (funcion Y)
subset(base,bmi>25|sexo==0) ##subset diciendo si cumple una funcion ó cumple la otra función(funcion ó).
#guardar el resultado en un archivo csv plano
write.csv(subset(base,bmi>25 & sexo==1),"ejemplo1.csv",row.names = FALSE)
save(base8,file= "prueba2.RData")
#__________________________________________________________________________________________________________-
#CONVIRTIENDO NUMERICO A FACTOR
str(iris$Species)

as.factor(iris$Species) #convierte de caracter a factor (especifica los niveles como un numero finito, se usa
#cuando los niveles ya estan definidas en tu variable
levels(iris$Species)
str(iris$Species)

#___________________________________________________________________________________________________________________---
##EXPLORACION BASICO DE DATOS Y AGRUPACIÓN

head(base) #primeros datos
dim(base) #cantidad de filas y columnas
str(base) #tipo de datos
base$sexo<-factor(base$sexo,levels = c(0,1),labels= c("mujer","hombre")) #da nombre a las categorias
table(base$sexo) #separa cantidad por categorias
#darle un porcentaje del total
prop<-table(base$sexo)
prop.table(prop)
#opcion 2 de darle porcentaje al total
table(base$sexo)/nrow(base)
#redondear decimales
round(table(base$sexo)/nrow(base),2)#2 decimales

#tabla de distribucion de frecuencias mas completo
library(summarytools)
freq(base$sexo) #solo sirve para datos cualitativos


#tabla de distriucion de frecuencia sin contar na ni identificar tipo de dato
library(DescTools)
Freq(base$sexo) #funciona para cualitativos y cuantitativos
sturges<-1+3.3*log(nrow(base)) #se particiona los numeros de levels según la regla de sturges
Freq(base$bmi, sturges) 
#________________________________________________________________________________________________________________
#GRAFICOS BASICOS
plot(base$sexo) #grafico de barras básico

graf1<-barplot(table(base$sexo), #gráfico de barras mas elaborado
        xlab = "Género",
        ylab = "Cantidad",
        main = "Proporción de personas por género")

#guardar el grafico como foto
jpeg("grafico1.jpeg") #signa el nombre
barplot(table(base$sexo), #gráfico de barras mas elaborado
        xlab = "Género",
        ylab = "Cantidad",
        main = "Proporción de personas por género")
dev.off() #termina la instruccion de guardar

#grafico de barras como proporcion
proporcion<-table(base$sexo)#sacar la cantidad de cada level
porcentaje <-prop.table(proporcion) #sacar la proporcion de los levels
#Grafico
grafico1<-barplot(porcentaje, 
                  ylim = c(0,1), #coloca el rango de la proporcion de 0 a 1
                  names.arg = "", #elimina las etiquetas 
                  xlab = "Género", #nombre a x
                  ylab = "Cantidad", #nombre a y
                  main = "Proporción de personas por género", #titulo
                  col = c("blue4","darkmagenta")) #colores
text(x=grafico1, y = porcentaje+0.1,
     labels = round(table(base$sexo)/nrow(base),2)) #se puede colocar la proporcion o cantidad total(quitar nrow para cantidad)
     legend("topright",legend = c("mujer","hombre"), #topright para que la leyenda vaya al costado
           col=c("blue4","darkmagenta"), pch = 20, bty="n") #agrega la leyenda/pch=tipo de figura/bty=n negrita
#___________________________________________________________________________________________________________________
#USO DE LIBRERIA GGPLOT
library(ggplot2)
#grafico con cantidad (count)     
ggplot(base, aes(x=sexo, #x identifica la variable que deseas usar para la gráfica
       fill=sexo))+ #etiqueta del grafico 
       geom_bar()#hace el grafico

#grafico con proporciones (%)
ggplot(base, aes(x=sexo, #x identifica la variable que deseas usar para la gráfica
      fill=sexo))+ #etiqueta del grafico 
      geom_bar(aes(y=(..count..)/sum(..count..)))+ #hace el grafico como proporcion
      ylab("Propoción")+ #coloca la etiqueta a y
      labs(title="proproción \n por género")+ #coloca el titulo de grafico, pero a una esquina (\n hace 1 fila más)
      theme(plot.title = element_text(hjust = 0.5))+ #coloca el titulo al centro
      xlab("") + #quita nombre del eje x
      theme(axis.text.x = element_blank(), #quita las etiquetas de las barras del eje x
            axis.ticks.x = element_blank())


#otra forma
prop_df<-data.frame(porcentaje) #almacena como dataframe la proporcion hecha de la variable sexo (linea 157)
names(prop_df)<-c("clase","frecuencia") #cambia de nombres a los encabezados

ggplot(prop_df, aes(x=clase, y=frecuencia, #x identifica la variable que deseas usar para la gráfica
                 fill=clase))+ #etiqueta del grafico 
                 geom_bar(stat = "identity") +#hace el grafico

                ylab("Propoción")+ #coloca la etiqueta a y
                labs(title="proproción \n por género")+ #coloca el titulo de grafico, pero a una esquina (\n hace 1 fila más)
                theme(plot.title = element_text(hjust = 0.5))+ #coloca el titulo al centro
                xlab("") + #quita nombre del eje x
                theme(axis.text.x = element_blank(), #quita las etiquetas de las barras del eje x
                axis.ticks.x = element_blank())
#-------------------------------------------------------------------------------------------------------------------
#GRÁFICOS CIRCULARES
cantidad1<-table(base$sexo) #sacar cantidad de cada nivel
porcentaje1<-prop.table(cantidad1) #sacar la proporcion de cada nivel
pie(porcentaje1,
    main = "proporción")

#usando ggplot
        ggplot(base, aes(x="", y=sexo, #x identifica la variable que deseas usar para la gráfica
              fill=sexo))+ #etiqueta del grafico 
              geom_col()+#hace el grafico de columna
              coord_polar(theta = "y") #convertir el grafico de columna a circular
#usando la tabla
        ggplot(prop_df, aes(x="", y=frecuencia, #x identifica la variable que deseas usar para la gráfica
               fill=clase))+ #etiqueta del grafico
               geom_col()+#hace el grafico
               coord_polar(theta = "y")+ #convertir el grafico de columna a circular
               geom_text(aes(label = round(frecuencia,2)*100), #colocar la cantidad por cada tipo (saca de la frecuencia la cantidad, considera 2 decimales y lo multiplica por 100)
                         position = position_stack(vjust = 0.5))+ # coloca el resultado en el medio
               theme_void() #elimina las cosas que no e configurado(preconfiguradas)
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#--------------------------------ESTADISTICA DESCRIPTIVA------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
#___________________________________________________________________________________________________________________              
                                       #TABLAS con variables cuantitativas
table(base$sexo) #la función table no sirve para cuantitativos
PLOT
        
library(DescTools)
freq(base$sexo) #tabla mas elaborada para datos cualitativos
Freq(base$bmi) #tabla mas elaborada para datos cuantitativos

sturges<-1+3.3*log(nrow(base),10)
Freq(base$bmi,ceiling(sturges)) #calculando sturges y redondeando hacia adelante
Freq(base$bmi,breaks =nclass.Sturges(base$bmi)) #usando sturges como función de rstudio

#_________________________________________________________________________________________________________________
                                          #medidas de tendencia central
median(base$bmi) #mediana
mean(base$bmi) #promedio
min(base$bmi) #minimo 
max(base$bmi) #maximo

#medidas de dispersión
#rango
max(base$bmi) - min(base$bmi) #maximo - minimo
#forma 2
range(base$bmi) #rangos max y min
range(base$bmi)[2]-range(base$bmi)[1] #el 2 y 1 hace que reste la segunda columna con la primera

#rango intercuartil
IQR(base$bmi) #rango intercuartil 

#varianza y desv
var(base$bmi)
sd(base$bmi)

#medidas de posicion
quantile(base$bmi, c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), type=2) #cuantiles, especificando la cantidad de posiciones que deseo, tipo 2 es el standard(forma de calculo)

#asimetria

#pearson
3*(mean(base$bmi)-median(base$bmi))/sd(base$bmi)

#Fisher
fisher.asi = function(x) mean((x-mean(x))^3)/sd(x)^3 #formula de fisher (dato menos media conjunto de datos al cubo ENTRE LA CANTIDAD DE DATOS O MEDIA) entre desv standar del dato al cubo
                              
fisher.asi = function(x) mean((x-mean(x,na.rm = T))^3, na.rm = T)/sd(x,na.rm = T)^3 #colocamos para que no considere NA

fisher.asi(base$bmi)

#RESUMEN DE DATOS
summary(base$bmi)
#____________________________________________________________________________________________________________________
#graficos para datos cuantitativos discretos

#paquete base
#generando datos
x<-data.frame(c(1,2,3,4,5), c(10,20,30,20,10))
colnames(x)<-c("num_accidentes","frecuencia")

#grafico basico
plot(x$num_accidentes,x$frecuencia,type="h") #grafico de bastones especificando x y y type el tipo de grafico

plot(x$num_accidentes,x$frecuencia/sum(x$frecuencia),type="h", #grafico de bastones en proporciones
    ylim = c(0,1), #limite de eje y de 0 a 1
    main = "gráfico de bastones", #titulo
    xlab = "Número de accidentes", #nombre eje x
    ylab = "Frecuencia") #nombre eje y

#grafico usando ggplot
library(ggplot2)
ggplot(x,aes(x=num_accidentes, y=frecuencia))+
      geom_point()+
      geom_segment(aes(x=num_accidentes,xend=num_accidentes, y=0,yend=frecuencia))+
      labs(x = "Número de accidentes",
           title = "Gráfico de bastones para el anillo de aros")

#HISTROGRAMAS
#con el programa base
hist(base$bmi, breaks = 10, #hist no es eficiente por que no te permite definir el numero de barras con sturges
     main = "IMC del hijo")
#forma como calcula la cantidad de clases en el histograma base y en ggplot, usa sturges pero lo fuerza para tener enteros.
breaks1<- pretty(range(base$bmi), 
                 nclass.Sturges(base$bmi),
                 min.n = 1)

#grafico considerando proporciones
grafic<-hist(base$bmi, breaks = 10, #hist no es eficiente por que no te permite definir el numero de barras con sturges
     main = "grafico de distribucion del hijo",
     prob = T,
     ylab = "FRECUENCIA",
     xlab = "IMC del hijo")
grafic #si guardo el grafico, este me da mucha información como las marcas de clase, cntidad por columna, etc.

#HISTOGRMAS USANDO GGPLOT
#cantidad
ggplot(base,aes(x=bmi))+ #llama los datos con que se trabajará
      geom_histogram(color = "darkblue", fill = "white")+ #tipo de grafico y colores
      labs(title = "IMC de distribucion de hijos",x ="IMC del hijo", y="frecuencia") #titulos

#densidad
ggplot(base,aes(x=bmi))+ #llama los datos con que se trabajará
  geom_histogram(aes(y= ..density..),color = "darkblue", fill = "white")+ #tipo de grafico y colores. Se aumeta la densidad
  labs(title = "IMC de distribucion de hijos",x ="IMC del hijo", y="frecuencia")+ #titulos
   theme(plot.title = element_text(hjust = 0.5)) #centra el texto


#grafico de cajas paquete base
box<-boxplot(base$bmi,
             horizontal = T,
             xlab = "IMC DE HIJOS",
             main = "DIAGRAMA DE CAJAS IMC")
        
box #si guardo el grafico, me brinda los datos tambien, no solo el grafico      

#grafico de cajas usando ggplot

ggp1<- ggplot(base, aes(x=bmi))+
              geom_boxplot(fill = "lightblue", outlier.color = "red",
                           outlier.shape = 1, #tipo del outlier
                           outlier.size =  2, #tamaño del outlier
                           #notch = TRUE #diseño
                             ) +
              labs(title = "DIAGRAMA DE CAJAS", x = "IMC de hijo")+ #coloca los nombres
              theme(plot.title = element_text(hjust = 0.5))+ #centra el titulo
              theme(axis.text.y = element_blank(), #elimina el eje y
                    axis.ticks.y = element_blank() ) #elimina las rayas del eje y

#tomando datos que estan fuera del digrama de cajas
windows() #activar ventana indepiendiente para graficos 

box$out
base[base$bmi>=min(box$out),]
base[base$bmi%in%(box$out),] 

#frecuencia acumulada
 facum = ecdf(base$bmi)
 plot(facum,
      main = "función de probabilidad acumulada IMC del hijo",
      xlab = "IMC del hijo",
      col = "darkblue")
 
#________________________________________________________________________________________________________________
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::DISTRIBUCIONES DE LOS DATOS:::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#________________________________________________________________________________________________________________
#FUNCIONES EN R PARA DISTRIBUCIÓN NORMAL
# Calcular la densidad acumulada
pnorm(-Inf,mean = 0, sd = 1) 
pnorm(0,mean = 0, sd = 1) #halla el funcion acumulada de x cuando sea <= a 0(área) 50% o 0.5
pnorm(Inf,mean = 0, sd = 1) 
#calcular el cuartil
qnorm(0.5,mean = 0, sd = 1) #dame el quantil 50 o la mediana 
#ejercicio
pnorm(10,mean = 10, sd = 1) #en que cuantil se encuentra el 10 ..en el cuantil 50 (mediana)
qnorm(0.975,mean =10, sd = 1) #dame el cuantil 97.5%, cuanto vale

#ejercicio 2
set.seed(25)
muestra<-rnorm(30,mean = 0,sd = 1)
mean(muestra)
sd(muestra)

pnorm(0,mean = -0.1847830, sd = 1.064394) #cual es la funcion acumulada cuando el x <= 0. Es 0.5689 ó 56.89%
qnorm(0.5,mean =-0.1847830, sd = 1.064394) #dame el valor de la variable cuando el cuantil sea igual a 50% (mediana)

#ejercicio3
#se tiene un objeto que viaja a una velocidad V en m/s con
media = 25
varianza = 4.195^2
desviacion = sqrt(varianza)
#hallar vo cuando V, tal que : 0.05 = P(V>VO)
qnorm(0.95, mean = media, sd = desviacion) #= el percentir o la funcion acumulada del 95% de los datos se encuentra en 31.90.
#comprobando el 5% de datos se encuentra a partir del 31.9
1- pnorm(31.90016, mean=media, sd=desviacion)


#__________________________________________________________________________________________________________
#SIMULANDO UNA DATA CON MEDIA 15 Y DESVIACION 2

set.seed(667)
temperatura<-rnorm(1000,mean = 15,sd =2)#generando datos
datos<-data.frame(temperatura)#guardando datos en un dataframe
mean(datos$temperatura)
sd(datos$temperatura)

#graficando datos
hist(datos$temperatura, probability = T)
     curve(dnorm(x, mean = 14.99417,sd=2.032153), add = T, col = "red") #grafica una curva, add agrega encima del anterior de color azul

#usando ggplo
library(ggplot2)
ggplot(datos, aes(x=temperatura))+
        geom_histogram(aes(y=..density..), fill="white", alpha = 1, size =1, col="blue")+      
        stat_function(fun=dnorm, color = "violetred4", size =1.8,
                      args = list(mean = 15, sd = 2))
#--------------------------------------------------------------------------------------------------------------------
#comparacion de medidas estadisticas
hist(datos$temperatura, probability = T)
curve(dnorm(x, mean = 14.99417,sd=2.032153), add = T, col = "red")

deciles<-(0:10)/10 #crear 10 valores divididos entre 10
empirico<-quantile(datos$temperatura,deciles)     
teorico <-qnorm(deciles,mean=15,sd=2) #generame los 10 cuantiles

rbind(empirico, teorico) #uniendo

plot( empirico,teorico, ylim=c(10,20), xlim=c(10,20))
abline(0,1, col="red") #traza una linea de 45° intercepto 0, pendiente 1

#usando ggplot 
datadeciles<-data.frame(cbind(empirico,teorico)) #almacenando en un dataframe

ggplot(datadeciles, aes(x=teorico, y=empirico))+
         geom_point()+
         geom_abline(intercept = 0, slope = 1, color = "red")+
         ggtitle("comparacion de deciles")+
         theme(plot.title= element_text(hjust = 0.5))

#estandarizando los valores teoricos
#al estandarizar los valores la forma de la recta no cambia
estandarizados<-qnorm(deciles, mean=0,sd=1)
datadeciles2<-data.frame(cbind(empirico,estandarizados))

ggplot(datadeciles2,aes(y=empirico, x=estandarizados))+
       geom_point()+
        geom_abline(intercept = 15, slope = 2, color = "red")+
        ggtitle("comparacion de deciles")+
        theme(plot.title= element_text(hjust = 0.5))

#################################################################################################################
#ggplot

qqnorm(datos$temperatura) #grafica los datos con los teoricos standarizados y empiricos tal cual
qqline(y=datos$temperatura, col = "red")

ggplot(datos, aes(sample=temperatura))+
       stat_qq(distribuction = stats::qnorm)+
       stat_qq_line(distribution = stats::qnorm, col = "red")+
       theme_bw()


####################################################################################################################

#PRUEBAS ESTADISTICAS
#shapiro
shapiro.test(datos$temperatura)
#prueba de kolmogorov
ks.test(datos$temperatura, "pnorm", mean = mean(datos$temperatura), sd= sd(datos$temperatura))
#en ambos se aceptan la hipotesis nula por que el pvalue es mayor a 0.05 ; la variable sigue una normal

#___________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________
####################################################################################################################
####################################################################################################################
#____________________________________INFERENCIA ESTADÍSTICA_________________________________________________________
#___________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________

peajes<-read.csv("D:/Carpeta personal/1. Profesión/5. Diplomado Estadistica Aplicada/3.Módulos/1.  Procedimientos estadísticos/5. pruebas de hipotesis/2. Práctica/L5 Scripts y datos-20221006/datos_peajes.csv")

totalvehiculos<-peajes$miles_vehiculos #usando la variable total de vehiculos
hist(totalvehiculos) #histograma para ver el comportamiento de la variable
box<-boxplot(totalvehiculos, horizontal = T) #diagrama de cajas/ a simple vista la variable no tiene un comportamiento normal
mean(totalvehiculos) #media de la variable

#ejemplo para evidenciar el que el comportamiento de la población es similar a la muestra
#ejemplo1
set.seed(123)
muestra1<-sample(totalvehiculos,50)
hist(muestra1)
mean(muestra1)
#ejemplo2
set.seed(2)
muestra2<-sample(totalvehiculos,50)
hist(muestra2)
mean(muestra2)
#al sacar las 2 muestras se puede demostrar que las medias y el comportamiento de 2 muestras de la misma población no
#van a ser la misma, existiendo ya así un margen de error.
#____________________________________________________________________
#obteniendo 5000 muestras de tamaño 10
coleccion_medias_1 <- rep(NA,5000) #creando 5000 espacios vacios donde se almacenarán las medias de las muestras

for(i in 1:5000) { #repirte el procedimiento 5000 veces
         muestra5000_10<-sample(totalvehiculos,10) #generando una muestra de 50
        coleccion_medias_1[i] <- mean(muestra5000_10) #sacando la media de cada muestra
}
#viendo el comportamiento de las 5000 muestras/ 
hist(coleccion_medias_1) #ahora la media de las muestras se distribuyen como una normal como lo indica el teorema del limite central
boxplot(coleccion_medias_1, horizontal = T)
mean(coleccion_medias_1)
#____________________________________________________________________
#obteniendo 5000 muestras de tamaño 50
coleccion_medias_2 <- rep(NA,5000) #creando 5000 espacios vacios donde se almacenarán las medias de las muestras

for(i in 1:5000) { #repirte el procedimiento 5000 veces
  muestra5000_50<-sample(totalvehiculos,50) #generando una muestra de 50
  coleccion_medias_2[i] <- mean(muestra5000_50) #sacando la media de cada muestra
}
#viendo el comportamiento de las 5000 muestras/ 
hist(coleccion_medias_2) #ahora la media de las muestras se distribuyen como una normal como lo indica el teorema del limite central
boxplot(coleccion_medias_2, horizontal = T)
mean(coleccion_medias_2)
#____________________________________________________________________
#obteniendo 5000 muestras de tamaño 100
coleccion_medias_3 <- rep(NA,5000) #creando 5000 espacios vacios donde se almacenarán las medias de las muestras

for(i in 1:5000) { #repirte el procedimiento 5000 veces
  muestra5000_100<-sample(totalvehiculos,100) #generando una muestra de 50
  coleccion_medias_3[i] <- mean(muestra5000_100) #sacando la media de cada muestra
}
#viendo el comportamiento de las 5000 muestras/ 
hist(coleccion_medias_3) #ahora la media de las muestras se distribuyen como una normal como lo indica el teorema del limite central
boxplot(coleccion_medias_3, horizontal = T)
mean(coleccion_medias_3)

#comparando las muestras con diferentes n de muestras

par(mfrow = c(3,1))
xlimits <- range(coleccion_medias_1)
hist(coleccion_medias_1, breaks = 20, xlim = xlimits) 
hist(coleccion_medias_2, breaks = 20, xlim = xlimits) 
hist(coleccion_medias_3, breaks = 20, xlim = xlimits) 

#al comparar las 3 muestras se puede observar la teoria del limite central y que a mayor la cantidad de los datos de la muestra los datos tienden a una normal.

#____________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________
#____________________________________________________________________________________________________________________

#INTERVALOS DE CONFIANZA

set.seed(411)
muestra_tot_vehiculos<-sample(totalvehiculos,60)
hist(muestra_tot_vehiculos)
boxplot(muestra_tot_vehiculos, horizontal = T)
mean(muestra_tot_vehiculos)

#estimacion por intervalos
media<- mean(muestra_tot_vehiculos) #media
t<- qt(0.975,59) # qt es una distribucion similar a la normal qnorm - hallando el cuartil 97.5, 60-1 grados de libertad
se <- sd(muestra_tot_vehiculos) / sqrt(60)  #error estandar o dispersion de datos
Lim_inf <- media - t*se
Lim_sup <- media + t*se
 #al 95% de confianza podemos afirmar que la media se encuentra entre los intervalos 795.71 y 1376.94 
#forma rapida
ic1<-t.test(muestra_tot_vehiculos)

#graficarlo
res_ic1 <- data.frame(t(c(ic1$estimate, ic1$conf.int))) #almacenar en un df los vaores
names(res_ic1)<-c("puntual", "li","ls") #darle nombres a las columnas
library(ggplot2) #llamar a la libreria
ggplot(res_ic1, aes(x="", y= puntual, colour = 1))+ #indicar donde ira el punto
      geom_point(size =3) + ylim(600,1600)+ #parametros del punto
      geom_errorbar(aes(ymax =ls,ymin =li), width = 0.2)+ #parametros de las barras
      ylab("total de vehiculos")+ #titulo al eje y
      xlab(element_blank())+ #alimina los rangos de x
      ggtitle("INTERVALOS DE CONFIANZA")+
      theme(legend.position = 'none')+
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
#si bien es cierto la muestra no tiene a una normal, pero como es mayor a 30 el numero de datos, tiende a una normal.

#INTERVALOS DE CONFIANZA PARA LA PROPORCION
#se tiene una muestra de 300 personas, de las cuales 100 sufren de depresión. PROBAR SI LA depresion es = 0.3
prop.test(100,300)
#el intervalo de confianza al 95% de confianza tiene una proporcion entre 0.28 y 0.39 

#EJERCICIO2
#se asume que la proporcion de medicos que ganan mas de 5000 soles es de 0.6. 
#en la encuesta del 2015 se observa que 1176 de 2219 medicos ganan mas de 5000.
#¿Segun los datos el minsa se equivoca o tiene una afirmacion correcta?

prop.test(1176,2219) #el minsa se equivoca por que ganan menos
#_________________________________________________________________________________________________________________
#__________________________________________________________________________________________________________________
#__________________________________________________________________________________________________________________
#__________________________________________________________________________________________________________________
#PRUEBAS DE HIPOTESIS
#Suponga que la media de vehiculos en el 2018 fue de 900
#probar si se a mantenido la media 
#ho=900
#hi=/900

t.test(muestra_tot_vehiculos, mu=900, alternative = "two.sided")



