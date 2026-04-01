#Instalacion de las librerias necesarias
install.packages("corrplot")
install.packages("ggplot2")

# Importacion de las librerias necesarias
library(corrplot)
library(ggplot2)

# 1. Importamos los datos:
# Definimos los nombres de las columnas manualmente
nombres <- c("Sex", "Length", "Diameter", "Height", "Whole", "Shucked",
             "Viscera", "Shell", "Rings")

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


#---- COMPARACIONES DE LA CARNE RESPECTO AL RESTO DE VARIABLES ESCOGIDAS ----

# Comparación respecto al sexo
# 1. BOXPLOT
boxplot(Shucked ~ Sex, data = tabla_sexo,
        main = "Distribución del magro de la carne según el sexo",
        xlab = "Sexo (I: infante, M: macho, F: hembra)",
        ylab = "Peso de la carne (g)",
        col = c("lightpink", "lightblue", "lightgreen"))
# 2. CONTEO
ggplot(datos, aes(x = Sex, fill = Sex)) +
  geom_bar(color = "blue", alpha = 0.8) +
  labs(title = "Cantidad de Ejemplares por Sexo",
       x = "Sexo",
       y = "Número de Abulones") + 
  theme_classic()

# Comparación respecto al diámetro
# 1. SCATTER PLOT
plot(tabla_diametro$Diameter, tabla_diametro$Shucked,
     main = "Relación lineal: diámetro-peso de la carne",
     xlab = "Diámetro (mm)",
     ylab = "Peso de la carne (g)",
     col = "steelblue",
     pch = 20)

# 2. GRÁFICO DE HEXÁGONOS
ggplot(datos, aes(x = Diameter, y = Shucked)) +
  geom_hex(bins = 45) + #Esto divide el gráfico en 30 hex
  scale_fill_viridis_c() + #Escala de colores daltónicos
  labs(title = "Densidad: Diámetro - Carne Magra",
       x = "Diámetro (mm)",
       y = "Peso de la carne (g)",
       fill = "Frecuencia") +
  theme_classic()


# Comparación respecto a los anillos
#1. CURVA DE CRECIMIENTO
plot(tabla_anillos$Rings, tabla_anillos$Shucked,
     main = "Curva de crecimiento: anillos-peso de la carne",
     xlab = "Número de anillos",
     ylab = "Peso de la carne (g)",
     col = "darkorange", pch = 18)

#2. POLÍGONO DE FRECUENCIAS
ggplot(datos, aes(x = Shucked, color = Sex)) +
  geom_freqpoly(bins = 30, linewidth = 1.2) +
  labs(title = "Polígono de Frecuencias Peso - Sexo",
       x = "Peso de la carne (g)",
       y = "Frecuencia de los Abulones",
       color = "Sexo") + 
  theme_classic()

#3. POLÍGONOS DE DENSIDAD

ggplot(datos, aes(x = Shucked, y = Shucked)) +
  geom_density_2d_filled(alpha = 0.7) + # Color en los polígonos
  geom_density_2d(color = "black", linewidth = 0.1) + # Contorno
  labs(title = "Mapa de Densidad: Edad - Peso",
       x = "Número de anillos",
       y = "Peso de la Carne (g)",
       fill = "Nivel de \nConcentración") + # \n para salto de línea
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) # Para que el eje X se lea mejor

# ----- COMPARACIÓN TOTAL DEL CONJUNTO -------

columnas_num <- datos [, -1]
matriz_cor <- cor(columnas_num)

# MATRIZ DE CORRELACIÓN DE LAS VARIABLES
corrplot(matriz_cor,
         method = "ellipse",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         title = "Matriz de correlación de variables físicas")

# 1. SCATTER PLOT (DIÁMETRO - SHUCKED) RESPECTO AL SEXO

  ggplot(datos, aes(x = Diameter, y= Shucked, color = Sex)) + 
    geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) + 
    labs(title = "Interacción: diámetro y peso de la carne por sexo",
         x = "Diámetro (mm)", y = "Peso de la Carne (g)") + theme_minimal()
  
# 2. SCATTER PLOT (DÍAMETRO - SHUCKED) EN 3 PANELES
  ggplot(datos, aes(x = Diameter, y = Shucked, color = Sex)) +
    geom_point(alpha = 0.4, size = 1.5) + #Puntos transparentes
    geom_smooth(method = "lm", color = "black", se = FALSE, lwd = 1) +#Tendencia
    facet_wrap(~Sex) + # Separación en 3 paneles
    labs(title = "Relación Diámetro - Peso con respecto al sexo",
         x = "Diámetro (mm)",
         y = "Peso de la Carne (g)") +
    theme_bw() + 
    theme(legend.position = "none")
  
  
  
#-------- MEDIDAS DE CENTRALIZACIÓN Y DE DISPERSIÓN ---------
  
# Resumen de cuartiles
summary(datos$Shucked)
  
# Varianza
var_peso <- var(datos$Shucked)

# Desviación típica
desviacion_peso <- sd(datos$Shucked)

# Imprimir las medidas
cat("Desviación típica: ", desviacion_peso, "\nVarianza: ", var_peso)


#--------- AJUSTE DE DISTRIBUCIÓN -----------

# 1. Ajuste a una normal ----- No se parece demasiado
qqnorm(datos$Shucked, main = "Ajuste normal del peso de la carne")
qqline(datos$Shucked, col = "red", lwd = 2)

# 2. Ajuste a una lognormal ------ Se parece aún menos
qqnorm(log(datos$Shucked), main = "Ajuste lognormal")
qqline(log(datos$Shucked), col = "blue", lwd = 2)

# 3. Ajuste a una Gamma --- Se parece bastante

media <- mean(datos$Shucked)
varianza <- var(datos$Shucked)

# Variables para comprobar si se ajusta a una distribución gamma
escala <- varianza/media
forma <- media/escala

cat("Forma: ", forma, "Escala: ", escala) # Imprimir los datos

# Creamos unos cuantiles teóricos de una distribución gamma
n <- length(datos$Shucked)
probabilidades <- (1:n - 0.5) / n
cuantiles_teoricos <- qgamma(probabilidades, shape = forma, scale = escala)

plot(cuantiles_teoricos, sort(datos$Shucked),
     main = "Ajuste a distribución Gamma",
     xlab = "Cuantiles Teóricos (Gamma)",
     ylab = "Cuantiles Reales (Peso Shucked)",
     pch = 20,
     color = "darkgreen")

#Línea donde se supone que debería estar si se ajusta a una Gamma
abline(0, 1, col = "red", lwd = 2)

# --------------------- INTERVALO DE CONFIANZA ------------------------
# Investigando en internet encontré que t.test es el que se suele usar
int_conf <- t.test(datos$Shucked, conf.level = 0.95)

cat("La media estimada es: ", media, "\n")
cat("Intervalo de Confianza (95%) para la Media: [", int_conf$conf.int[1], ",", int_conf$conf.int[2], "]\n")
