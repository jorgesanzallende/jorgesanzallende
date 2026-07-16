# B.2. Análisis HJ-Biplot y Clúster Espacial (Dimensión de Sostenibilidad)
library(readxl)

# Carga de datos con ruta relativa
base3 <- read_excel("../data/raw/BASE_3_SOSTENIBILIDAD.xlsx")
b3_num <- base3[, 3:ncol(base3)]

# Estandarización y Descomposición en Valores Singulares (DVS)
X_escalada <- scale(b3_num, center = TRUE, scale = TRUE)
dvs <- svd(X_escalada)

# Cálculo de marcadores
J_individuos <- dvs$u %*% diag(dvs$d) 
H_variables <- dvs$v %*% diag(dvs$d) 

ccaa_ejes <- J_individuos[, 1:2]
var_ejes <- H_variables[, 1:2]

rownames(ccaa_ejes) <- base3$CCAA
rownames(var_ejes) <- colnames(b3_num)

# Definición de parámetros gráficos
coloresX1 <- c("blue", "red", "green", "orange", "purple", "brown", "pink", "gray",
               "cyan", "magenta", "darkblue", "darkred", "darkgreen", "navy", "maroon",
               "turquoise", "black")
colores_ccaa <- coloresX1 
colores_var <- c("darkred", "darkblue", "darkgreen", "darkorange", "purple")

limites_x <- range(c(ccaa_ejes[,1], var_ejes[,1])) * 1.3 
limites_y <- range(c(ccaa_ejes[,2], var_ejes[,2])) * 1.3

# Primer gráfico: HJ-Biplot de Sostenibilidad
plot(0, 0, type = "n", xlim = limites_x, ylim = limites_y,
     xlab = "Eje 1 (61,02%)", ylab = "Eje 2 (20,17%)",
     main = "HJ-Biplot - Dimensión de Sostenibilidad",
     panel.first = grid(col = "gray90", lty = "solid")) 

abline(h = 0, v = 0, col = "gray60", lty = 2)
points(ccaa_ejes[,1], ccaa_ejes[,2], col = colores_ccaa, pch = 19, cex = 1.3)
text(ccaa_ejes[,1], ccaa_ejes[,2], labels = rownames(ccaa_ejes), 
     pos = 3, col = colores_ccaa, cex = 1.1, font = 2)
arrows(0, 0, var_ejes[,1], var_ejes[,2], col = colores_var, length = 0.1, lwd = 2)

# Ajuste condicional para evitar solapamiento de etiquetas en "SAU" y "SUPERFICIE"
coords_texto_var <- var_ejes
idx_sau <- grep("SAU", rownames(coords_texto_var))
idx_sup <- grep("SUPERFICIE", rownames(coords_texto_var))

if(length(idx_sup) > 0) coords_texto_var[idx_sup, 2] <- coords_texto_var[idx_sup, 2] + 0.15
if(length(idx_sau) > 0) coords_texto_var[idx_sau, 2] <- coords_texto_var[idx_sau, 2] - 0.15

text(coords_texto_var[,1], coords_texto_var[,2], labels = rownames(coords_texto_var), 
     pos = 4, col = colores_var, cex = 1.2, font = 2)

# Algoritmo de agrupación (K-Means Espacial)
grupos <- rep(2, nrow(ccaa_ejes))
ccaa_grupo_azul <- c("ANDALUCIA", "ARAGON", "CASTILLA Y LEON", "CASTILLA LA MANCHA", "CATALUÑA", "EXTREMADURA")
grupos[toupper(base3$CCAA) %in% ccaa_grupo_azul] <- 1

# Cálculo de centroides de grupo
centro_g1 <- colMeans(ccaa_ejes[grupos == 1, , drop = FALSE])
centro_g2 <- colMeans(ccaa_ejes[grupos == 2, , drop = FALSE])
centros <- rbind(centro_g1, centro_g2)

colores_grupos <- c("deepskyblue", "red")
colores_puntos <- colores_grupos[grupos]

# Segundo gráfico: HJ-Biplot + Agrupamiento Clúster (Sostenibilidad)
plot(0, 0, type = "n", xlim = limites_x, ylim = limites_y,
     xlab = "Eje 1 (61,02%)", ylab = "Eje 2 (20,17%)", 
     main = "HJ-Biplot + Agrupamiento Clúster (Sostenibilidad)",
     panel.first = grid(col = "gray95", lty = "solid"))

abline(h = 0, v = 0, col = "gray60", lty = 2)

# Dibujar segmentos desde el centroide a cada punto del clúster
for (i in 1:nrow(ccaa_ejes)) {
  g_actual <- grupos[i] 
  segments(x0 = centros[g_actual, 1], y0 = centros[g_actual, 2], 
           x1 = ccaa_ejes[i, 1], y1 = ccaa_ejes[i, 2], 
           col = colores_puntos[i], lty = 1, lwd = 1) 
}

points(ccaa_ejes[,1], ccaa_ejes[,2], col = colores_puntos, pch = 19, cex = 1.3)
text(ccaa_ejes[,1], ccaa_ejes[,2], labels = rownames(ccaa_ejes), 
     pos = 3, col = colores_puntos, cex = 1.1, font = 2)
arrows(0, 0, var_ejes[,1], var_ejes[,2], col = "black", length = 0.1, lwd = 2)

# Ajuste condicional para evitar solapamientos en K-Means
coords_texto_var_cl <- var_ejes
idx_sau_cl <- grep("SAU", rownames(coords_texto_var_cl))
idx_sup_cl <- grep("SUPERFICIE", rownames(coords_texto_var_cl))

if(length(idx_sup_cl) > 0) coords_texto_var_cl[idx_sup_cl, 2] <- coords_texto_var_cl[idx_sup_cl, 2] + 0.15
if(length(idx_sau_cl) > 0) coords_texto_var_cl[idx_sau_cl, 2] <- coords_texto_var_cl[idx_sau_cl, 2] - 0.15

text(coords_texto_var_cl[,1], coords_texto_var_cl[,2], labels = rownames(coords_texto_var_cl), 
     pos = 4, col = "black", cex = 1.2, font = 2)
