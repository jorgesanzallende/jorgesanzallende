# B.3. Análisis Parcial Triádico (PTA) y Co-Inercia Temporal (COSTATIS)
library(KTensorGraphs)
library(readxl)

# Lectura de datos con rutas relativas
datos_est_raw <- read_excel("../data/raw/BASE_1_ESTRUCTURAL_NUM.xlsx")
datos_eco_raw <- read_excel("../data/raw/BASE_2_ECONOMICA_NUM.xlsx")

# Estructuración de los tensores tridimensionales
X <- aperm(array(as.matrix(datos_est_raw),dim=c(11,17,11)),c(2,3,1))
Y <- aperm(array(as.matrix(datos_eco_raw),dim=c(17,13,4)),c(1,3,2))

# Etiquetado de los elementos del tensor X (Estructural)
dimnames(X)[[1]]<- c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", 
                     "Cantabria", "Castilla-La Mancha", "Castilla y León", "Cataluña", 
                     "Extremadura", "Galicia", "Madrid", "Murcia", "Navarra", "País Vasco",
                     "La Rioja", "Comunidad Valenciana")

dimnames(X)[[2]]<- c("EXP_INTENSIVA_CEBO", "EXP_EXTENSIVA_LECHE", "EXP_EXTENSIVA_CARNE", 
                     "EXP_MIXTA", "EXP_OTRAS", "CENSO_<12_MESES", 
                     "CENSO_12_24_MESES", "CENSO_>24_MESES", "VACAS_NODRIZAS", 
                     "VACAS_LECHE", "TAMAÑO_MEDIO_EXPLOTACION")

dimnames(X)[[3]]<- c("2021_S1", "2021_S2", "2022_S1", "2022_S2", "2023_S1", 
                     "2023_S2", "2024_S1", "2024_S2", "2025_S1", "2025_S2", "2026_S1")

# Etiquetado de los elementos del tensor Y (Económico)
dimnames(Y)[[1]]<- dimnames(X)[[1]]

dimnames(Y)[[2]]<- c("Valor de Producción", "Subvenciones", "Producción de carne", "Precio Medio")

dimnames(Y)[[3]]<- c("2011", "2012", "2013", "2014", "2015", "2016",
                     "2017", "2018", "2019", "2020", "2021", "2022", "2023")

# Definición de parámetros gráficos (Paletas de colores)
coloresX1 <- c("blue", "red", "green", "orange", "purple", "brown", "pink", "gray",
               "cyan", "magenta", "darkblue", "darkred", "darkgreen", "navy", "maroon",
               "turquoise", "black")

coloresX2 <- c("darkred", "darkblue", "darkgreen", "darkorange", "darkgray", "blue",
               "dodgerblue", "navy", "forestgreen", "steelblue", "purple")

coloresY1 <- coloresX1
coloresY2 <- c("darkred", "darkblue", "darkgreen", "darkorange")

# Ejecución de los algoritmos de análisis multivariante k-tablas
PTA(X, dimX=1, dimY=2, norm = TRUE, coloresf=coloresX1, coloresc=coloresX2)
PTA(Y, dimX=1, dimY=2, norm = TRUE, coloresf=coloresY1, coloresc=coloresY2)

COSTATIS(X, Y, dimX=1, dimY=2, coloresf=coloresX1, coloresc1=coloresX2, coloresc2=coloresY2, norm=TRUE)
