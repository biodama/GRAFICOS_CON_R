###############################################################################
## De qplot a ggplot2
##############################################################################
rm(list=ls())
gc()


setwd("/Users/celiatalavan/Desktop/curso CTG/")
load("data/datos.tablas.des.RData")

library("ggplot2")
library("data.table")


# Distribución de una variable (continua)
## Histograma

qplot(edad,data= datos)

ggplot(data=datos)+
  geom_histogram(aes(x=edad))

## Histograma dividido
qplot(edad,data= datos,facets = ~ sexo)

ggplot(data=datos)+
  geom_histogram(aes(x=edad))+
  facet_grid(~sexo)

## **Ejercicio:** Crea un histograma de la variable peso según el nivel de estudio de la base de datos.
ggplot(data=datos)+
  geom_histogram(aes(x=peso))+
  facet_grid(~sexo)

## Histograma (numero de intervalos)

qplot(edad,data= datos,bins=8,color=I("skyblue3"),fill= I("red"))


ggplot(data=datos)+
  geom_histogram(aes(x=edad),bins = 8,color="skyblue3",fill="red")


## Histograma (relleno)

qplot(edad,data=datos,bins=8,fill=sexo)

ggplot(data=datos)+
  geom_histogram(aes(x=edad,fill=sexo),bins = 8)

#**Ejercicio:** Crea un histograma de la variable peso según si son diabeticos o no de la base de datos.

qplot(peso,data=datos,bins=8,fill=diabetes)

ggplot(data=datos)+
  geom_histogram(aes(x=peso,fill=diabetes))


## Piechart

#No se puede hacer un piechat con qplot está en desuso


tabla<-table(datos$nivel.estudios)
porcentaje <- prop.table(tabla) * 100

df_porcentaje <- data.frame(
  categoria = names(porcentaje),
  porcentaje = as.numeric(porcentaje)
)

qplot(x = "", y = porcentaje, fill = categoria, data = df_porcentaje,
      geom = "bar", stat = "identity", width = 1)

ggplot(df_porcentaje, aes(x = "", y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(title = "Ejemplo de gráfico circular (pie chart)") +
  theme_void(base_size = 14) +  # elimina ejes y fondo
  theme(legend.position = "right")



ggplot(df_porcentaje, aes(x = "", y = porcentaje, fill = categoria)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = porcentaje),
  position = position_stack(vjust = 0.5),  # coloca etiquetas centradas
  color = "white", size = 5) +
  labs(title = "Ejemplo de gráfico circular (pie chart)") +
  theme_void() +
  theme(legend.position = "right")


# Relación y Correlación de dos variables

## Scatter plot

qplot(peso,altura,data=datos,xlab="peso (kg)")

ggplot(data=datos,aes(x=peso,y=altura))+
  geom_point()+labs(x="peso (kg)")


## Scatter plot ajuste


qplot(peso,altura,data=subset(datos,datos$sexo=="Hombre"),xlab="peso(kg)",geom="smooth",
      main="Peso vs Altura en Hombres")

ggplot(data=subset(datos,datos$sexo=="Hombre"),aes(x=peso,y=altura))+
  geom_smooth()+labs(x="peso (kg)",title = "Peso vs Altura en Hombres")


#  **Ejercicio:** Estudia la relación entre el peso y la altura de los diabéticos aplicando una tendencia suavizada.

qplot(peso,altura,data=subset(datos,datos$diabet=="Si"),xlab="peso(kg)",geom="smooth",main="Peso vs Altura en Diabeticos")

ggplot(data=subset(datos,datos$diabetes=="Si"),aes(x=peso,y=altura))+
  geom_smooth()+labs(x="peso (kg)",title = "Peso vs Altura en Hombres")

  ## Scatter plot ajuste estratificado
  


qplot(peso, altura, data = datos, facets = .~factor(sexo), color = factor(sexo))+ geom_smooth(method = "lm") 
# metodo linear modeling, regresion lineal


ggplot(data=datos,aes(x=peso,y=altura,color=sexo))+
  geom_point()+
  geom_smooth(method = "lm")+labs(x="peso (kg)",title = "Peso vs Altura en Hombres")+
  facet_grid(~sexo)




## Scatter plot line + punto



qplot(peso, altura, data=datos, geom=c("point", "line"))

ggplot(data=datos,aes(x=peso,y=altura))+
  geom_point()+geom_line()

## Scatter plot line + identificación


qplot(peso, altura, data = subset(datos,datos$sexo=="Hombre"), label = ID, 
      geom=c("point", "text"),
      hjust=0, vjust=0)

ggplot(data = subset(datos,datos$sexo=="Hombre"),aes(x=peso,y=altura))+
  geom_point()+
  geom_text(aes(label=ID),vjust = 0,hjust=0)




## Jittering

qplot(edad,nivel.estudios,data=datos)

ggplot(data=datos,aes(x=edad,y=nivel.estudios))+
  geom_point()


## Jittering

qplot(edad,nivel.estudios,data=datos,alpha=I(0.3),size=I(1),geom="jitter")

ggplot(data=datos,aes(x=edad,y=nivel.estudios))+
  geom_jitter(alpha=0.3,size=1)



# **Ejercicio:** Estudia la relación entre el peso y la diabetes aplicando mostrando un diagrama de dispersión evitando el solapamiento.

qplot(peso,diabetes,data=datos,alpha=I(0.5),size=I(0.5),geom="jitter")

ggplot(data=datos,aes(x=peso,y=diabetes))+
  geom_jitter(alpha=0.5,size=0.5)
  
## EJERCICIOS


# **1. Representa la distribución de la variable edad mediante un histograma con 8 intervalos (bins).**
#   
#   - Cambia el color del borde de las barras a azul.
# 
# - Usa un relleno de color gris claro para el interior de las barras.
# 
# 

qplot(edad, data = datos, bins = 8, color = I("blue"), fill = I("grey80"))

ggplot(data=datos,aes(x=edad))+geom_histogram(color="blue",fill="grey80",bins=8)



#DUDA
ggplot(data=datos,aes(x=edad))+geom_bar(color="blue",fill="grey80")

# 
# **2. Construye un gráfico de barras de la variable diabetes.**
#   
#  - Dibuja primero el gráfico con las frecuencias absolutas (número de personas con y sin diabetes).
# 
# - Asigna un color diferente a cada categoría de diabetes.

qplot(diabetes, data = datos)            
qplot(diabetes, data = datos, fill=diabetes) 


ggplot(data=datos,aes(x=diabetes,fill=diabetes))+
  geom_bar()
# 
# **3. Crea un diagrama de cajas (boxplot) para analizar cómo varía el peso según el nivel de estudio de las personas.**
#   
#   - El color del contorno de las cajas debe ser negro.
# 
# - Rellena cada caja con un color distinto dependiendo del nivel de estudio.
# 
# - Añade un título descriptivo al gráfico.
# 
qplot(nivel.estudios, peso, data=datos, geom="boxplot", fill=nivel.estudios, color=I("black")
      ,main="Nivel educativos vs Peso (Kg)")

ggplot(data=datos,aes(x=nivel.estudios,y=peso,fill=nivel.estudios))+
  geom_boxplot(color="black")+labs(title="Nivel educativos vs Peso (Kg)")

# 
# **4. Representa en un gráfico de dispersión (scatter plot) la relación entre la altura y el peso de los individuos.**
#   
#   - Dibuja los puntos en color rojo con un tamaño mayor que el predeterminado.
# 
# - Añade una curva suavizada que muestre la tendencia general entre ambas variables.
qplot(altura, peso, data=datos, geom=c("point"), color=I("red"), size=I(3))

ggplot(data=datos,aes(x=altura,y=peso))+
  geom_point(color="red",size=3)

qplot(altura, peso, data=datos, geom=c("point","smooth"), color=I("red"), size=I(3))
#Con qplot() no se puede controlar colores de capas individuales tan fácilmente, porque aplica los mismos argumentos estéticos a todo.
# Para cambiar el color ya tendríamos que ir haciendolo por capas como el ggplot

ggplot(data=datos,aes(x=altura,y=peso))+
  geom_point(color="red")+geom_smooth(color="red")
# 
# **5. Queremos visualizar la relación entre la edad y el nivel de estudio, teniendo en cuenta también la presencia de cáncer.**
#   
#   - Utiliza un gráfico con jittering para evitar la superposición de los puntos.
# 
# - Ajusta el jitter para que los puntos se dispersen un poco en el eje X, pero no se desplacen en el eje Y.
# 
# - Colorea los puntos según si la persona tiene cáncer de mama y utiliza una forma distinta de los puntos para distinguir a quienes tienen cáncer de próstata.
# 

#No fuciona
qplot(nivel.estudios, edad, data=datos[!is.na(datos$cancer.mama),],color=cancer.mama,shape=cancer.mama, geom="jitter")

#Mirar porque estó no funciona
ggplot(data=datos,aes(x=nivel.estudios,y=edad),color=cancer.mama,shape=cancer.mama)+
  geom_jitter()


📌 Resumen rápido:
  Función	Uso principal	Qué hace por defecto
geom_bar()	Categóricas	Cuenta las observaciones (stat="count")
geom_histogram()	Numéricas	Agrupa en bins e histograma
geom_col()	Datos resumidos	Usa valores de y directamente (stat="identity")
