# B.1. Análisis Descriptivo y Correlaciones de Spearman
library(readxl)
library(corrplot)

# Lectura de datos con rutas relativas
base1 <- read_excel("../data/raw/BASE_1_ESTRUCTURAL.xlsx")
base2 <- read_excel("../data/raw/BASE_2_ECONOMICA.xlsx")
base3 <- read_excel("../data/raw/BASE_3_SOSTENIBILIDAD.xlsx")

if("AÑO" %in% names(base1)) base1$AÑO <- NULL
if("AÑO" %in% names(base2)) base2$AÑO <- NULL
if("AÑO" %in% names(base3)) base3$AÑO <- NULL

# Función para generar tablas de estadística descriptiva
generar_tabla <- function(df) {
  df_num <- df[, sapply(df, is.numeric)]
  
  res <- data.frame(
    Variable = colnames(df_num),
    Media = round(sapply(df_num, mean, na.rm=TRUE), 4),
    Mediana = round(sapply(df_num, median, na.rm=TRUE), 4),
    Desv.Tipica = round(sapply(df_num, sd, na.rm=TRUE), 4),
    Minimo = round(sapply(df_num, min, na.rm=TRUE), 4),
    Maximo = round(sapply(df_num, max, na.rm=TRUE), 4)
  )
  return(res)
}

# Escritura de resultados en data/processed/
write.csv(generar_tabla(base1), "../data/processed/TABLA_Descriptivos_Base1.csv", row.names=FALSE)
write.csv(generar_tabla(base2), "../data/processed/TABLA_Descriptivos_Base2.csv", row.names=FALSE)
write.csv(generar_tabla(base3), "../data/processed/TABLA_Descriptivos_Base3.csv", row.names=FALSE)

write.csv(round(cor(base1[, sapply(base1, is.numeric)], use="pairwise.complete.obs", 
                    method="spearman"), 4), "../data/processed/TABLA_Correlaciones_Base1_Spearman.csv")
write.csv(round(cor(base2[, sapply(base2, is.numeric)], use="pairwise.complete.obs", 
                    method="spearman"), 4), "../data/processed/TABLA_Correlaciones_Base2_Spearman.csv")
write.csv(round(cor(base3[, sapply(base3, is.numeric)], use="pairwise.complete.obs", 
                    method="spearman"), 4), "../data/processed/TABLA_Correlaciones_Base3_Spearman.csv")

# Generación de gráficos de correlación
corrplot(cor(base1[, sapply(base1, is.numeric)], use="pairwise.complete.obs", 
             method="spearman"), 
         method="color", type="upper", addCoef.col="black", tl.cex=0.7, number.cex=0.6, 
         title="Estructural (Spearman)", mar=c(0,0,1,0))

corrplot(cor(base2[, sapply(base2, is.numeric)], use="pairwise.complete.obs", 
             method="spearman"), 
         method="color", type="upper", addCoef.col="black", tl.cex=0.8, number.cex=0.8, 
         title="Económica (Spearman)", mar=c(0,0,1,0))

corrplot(cor(base3[, sapply(base3, is.numeric)], use="pairwise.complete.obs", 
             method="spearman"), 
         method="color", type="upper", addCoef.col="black", tl.cex=0.8, number.cex=0.8, 
         title="Sostenibilidad (Spearman)", mar=c(0,0,1,0))
