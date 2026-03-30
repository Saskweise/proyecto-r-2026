# 1. Importamos los datos:
# Definimos los nombres de las columnas manualmente
colnames(datos) <- c("Sex", "Lenght", "Diameter", "Height", "Whole", "Shucked", "Viscera", "Shell", "Rings")

# Código para leer el archivo local
# Ponermos "header = FALSE" porque el archivo no tiene títulos en la primera fila
# Usamos col.names para asignar los nombres que hemos definido antes
datos <- read.csv("abalone.data", header = FALSE, col.names = nombres)

# 2. Análisis Descriptivo: Tablas: Hacemos una tabla para cada variable predictora frente a la variable objetivo
# Ver las primeras filas
head(datos,100)

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
