#Instalacion de las librerias necesarias
install.packages("corrplot")
install.packages("ggplot2")
library(corrplot)
library(ggplot2)
# 1. Importamos los datos:
# Definimos los nombres de las columnas manualmente
nombres <- c("Sex", "Length", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell", "Rings")

# Código para leer el archivo local
# Ponermos "header = FALSE" porque el archivo no tiene títulos en la primera fila
# Usamos col.names para asignar los nombres que hemos definido antes
datos <- read.csv("abalone.data", header = FALSE, col.names = nombres)

# Tabla A: Objetivo + Sexo (Cualitativa)
tabla_sexo <- datos[, c("Sex", "Shucked")]

# Tabla B: Objetivo + Diámetro
tabla_diametro <- datos[, c("Diameter", "Shucked")]

# Tabla C: Objetivo + Anillos
tabla_anillos <- datos[, c("Rings", "Shucked")]

# Tabla D: Objetivo + Peso Vísceras
tabla_visceras <- datos[, c("Viscera", "Shucked")]

#Comprobamos que está todo bien usando head con alguna de la tablas
head(tabla_diametro)


#---- grafica de las tablas por separado ----

# relacion con respecto al sexo
boxplot(Shucked ~ Sex, data = tabla_sexo,
        main = "Distribución del magro de la carne según el sexo",
        xlab = "Sexo (I: infante, M: macho, F: hembra)",
        ylab = "Peso de la carne (g)",
        col = c("lightpink", "lightblue", "lightgreen"))

# relacion con respecto al diametro

plot(tabla_diametro$Diameter, tabla_diametro$Shucked,
     main = "Relación lineal: diámetro-peso de la carne",
     xlab = "Diámetro (mm)",
     ylab = "Peso de la carne (g)",
     col = "steelblue",
     pch = 20)

# relacion con respecto a los anillos

plot(tabla_anillos$Rings, tabla_anillos$Shucked,
     main = "Curva de crecimiento: anillos-peso de la carne",
     xlab = "Número de anillos",
     ylab = "Peso de la carne (g)",
     col = "darkorange", pch = 18)

# ----- comparacion total del conjunto -------

columnas_num <- datos [, -1]
matriz_cor <- cor(columnas_num)
# Matriz de correlación con cada una de las variables
corrplot(matriz_cor,
         method = "ellipse",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         title = "Matriz de correlación de variables físicas")

# grafico de dispersion multiple (diametro-shucked) separado por sexo

ggplot(datos, aes(x = Diameter, y= Shucked, color = Sex)) + geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) + labs(title = "Interacción: diámetro y peso de la carne por sexo", x = "Diámetro (mm)", y = "Peso de la Carne (g)") + theme_minimal()
