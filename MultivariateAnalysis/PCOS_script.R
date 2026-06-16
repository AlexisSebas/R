#Empezamos limpiando nuestro ambiente

rm(list = ls(all.names = TRUE))

# Configuración global de los bloques de código (chunk's)
knitr::opts_chunk$set(
  echo = FALSE,           
  fig.align = "center",  
  fig.dim = c(7, 6),     
  fig.pos = "H",         
  message = FALSE,       
  warning = FALSE,       
  error = FALSE)         
  
# Librerías utilizadas
library(ca)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(knitr)
library(readxl)
library(dplyr)
library(MASS)
library(caret)
library(pROC)
library(glmnet)
library(cluster)
library(DescTools)
library(reshape2)
library(patchwork)
library(psych)
library(randomForest)

library(gridExtra)
library(rpart)
library(rpart.plot)

library(biotools)
library(MVN)
library(kableExtra)
library(nortest)




data1 <- read_xlsx("C:/Users/USER/Downloads/PCOS.xlsx", sheet = 2)

# summary(data1)
# str(data1)

# Renombramos variable
colnames(data1)[colnames(data1) == "PCOS (Y/N)"] <- "PCOS"



# Listas de variables 
vars <- list(
  target      = "PCOS",
  numericas   = c("Age (yrs)", "BMI", "FSH/LH", "AMH(ng/mL)", "Vit D3 (ng/mL)", 
                  "TSH (mIU/L)", "PRL(ng/mL)", "Total_Follicle"),
  categoricas = c("Pregnant(Y/N)", "Weight gain(Y/N)", "hair growth(Y/N)",
                  "Skin darkening (Y/N)", "Hair loss(Y/N)", "Pimples(Y/N)",
                  "Fast food (Y/N)", "Reg.Exercise(Y/N)", "Blood Group", "Cycle(R/I)")
)

vars$numericas   <- intersect(vars$numericas, names(data1))
vars$categoricas <- intersect(vars$categoricas, names(data1))

cols_eliminar <- c("No. of aborptions", "Weight (Kg)", "Height(Cm)",
                   "FSH(mIU/mL)", "LH(mIU/mL)", "Hip(inch)", "Waist(inch)",
                   "Follicle No. (L)", "Follicle No. (R)")

data1 <- data1 %>%
  mutate(`AMH(ng/mL)` = as.numeric(as.character(`AMH(ng/mL)`))) %>%
  # Convertimos las categóricas a factor
  mutate(across(all_of(vars$categoricas), ~ as.factor(.x))) %>%
  
  # Filtramos el dato de ciclo con 5
  
  filter(`Cycle(R/I)` != "5") %>%
  
  mutate(
    
    # Para "Cycle(R/I)"  se convierte a factor
    `Cycle(R/I)` = dplyr::recode(`Cycle(R/I)`,"2" = "Cycle:R", "4" = "Cycle:I") %>% droplevels(),
    `Blood Group` = dplyr::recode(`Blood Group`,
                           "11" = "A+", "12" = "A-", "13" = "B+", "14" = "B-",
                           "15" = "O+", "16" = "O-", "17" = "AB+", "18" = "AB-")
  ) %>%
  
  # Creamos variable binaria de abortos y  la de folículos totales
  mutate(
    
    #Dado que la variable repecto al número de abortos resulta bastante conflictiva, por su enorme cantidad de ceros y poco número de abortos, parece pertinente mejor trtarla como var cat
    `Aborptions(Y/N)` = as.factor(ifelse(`No. of aborptions` < 1, "0", "1")),
    Total_Follicle = `Follicle No. (L)` + `Follicle No. (R)`
  ) %>%
  
  # Eliminamos columnas redundantes como la de foliculos y otras correlacionadas
  dplyr::select(-any_of(cols_eliminar))

# Actualizamos la lista de categóricas tras crear Aborptions(Y/N)
vars$categoricas <- c(vars$categoricas, "Aborptions(Y/N)")

# Revisamos estructura final de las variables
# str(data1)




# summary(data1)

##  Datos muy raros:
#FSH/LH : max 1372
#Vit D3 (ng/mL) : max 6014

## Altos y raros pero posibles
#AMH(ng/mL)  max 66
#TSH (mIU/L) : max 65
#PRL(ng/mL) : 128.24

# Identificamos valores atípicos que podrían afectar el análisis 

cat("FSH/LH           : max", max(data1$`FSH/LH`, na.rm = TRUE), "\n")
cat("Vit D3 (ng/mL)   : max", max(data1$`Vit D3 (ng/mL)`, na.rm = TRUE), "\n")

cat("AMH(ng/mL)       : max", max(data1$`AMH(ng/mL)`, na.rm = TRUE), "\n")
cat("TSH (mIU/L)      : max", max(data1$`TSH (mIU/L)`, na.rm = TRUE), "\n")
cat("PRL(ng/mL)       : max", max(data1$`PRL(ng/mL)`, na.rm = TRUE), "\n")

# Boxplots 
par(mfrow = c(1, 3))  
boxplot(data1$`TSH (mIU/L)`, main = "TSH (mIU/L)", col = "lightpink")
boxplot(data1$`PRL(ng/mL)`, main = "PRL (ng/mL)", col = "lightcyan")
boxplot(data1$`AMH(ng/mL)`, main = "AMH (ng/mL)", col = "lavender")



# Boxplots de los outliers que se van a omitir
fun_boxplot <- function(data, variable, titulo, outliers_idx, outliers_val) {
  df_plot <- data.frame(
    valor = data[[variable]],
    es_outlier = ifelse(seq_len(nrow(data)) %in% outliers_idx, "Error de medición", "Normal")
  )
  
  ggplot(df_plot, aes(x = "", y = valor)) +
    geom_boxplot(fill = "#AED6F1", color = "#1A5276", 
                 alpha = 0.9, outlier.shape = NA, width = 0.4,
                 size = 1) +
    # Outliers normales
    geom_point(data = subset(df_plot, es_outlier == "Normal" & 
                               (valor > quantile(df_plot$valor, 0.95, na.rm = TRUE) |
                                valor < quantile(df_plot$valor, 0.05, na.rm = TRUE))),
               aes(x = "", y = valor), 
               color = "#EC7063", size = 3, shape = 17, alpha = 0.7) +
    # Outliers a eliminar
    geom_point(data = subset(df_plot, es_outlier == "Error de medición"),
               aes(x = "", y = valor), 
               color = "#943126", size = 4, shape = 17) +
    geom_text(data = subset(df_plot, es_outlier == "Error de medición"),
              aes(x = "", y = valor, label = round(valor, 1)),
              color = "#943126", size = 3.5, hjust = -0.3, fontface = "bold") +
    labs(
      title = titulo,
      subtitle = "Valores atípicos (errores de medición)",
      x = NULL, y = "Valor"
    ) +
    theme_minimal() +
    theme(

      plot.title = element_text(hjust = 0.5, face = "bold", size = 13, 
                                color = "#1A5276", 
                                margin = ggplot2::margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#5499C7",
                                   margin = ggplot2::margin(b = 10)),
      axis.title.y = element_text(size = 11, face = "bold", color = "#1A5276"),
      axis.text.y = element_text(size = 10, color = "#5499C7"),
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = ggplot2::margin(15, 20, 15, 15)
    )
}

bp1 <- fun_boxplot(data1, "FSH/LH", "Relación FSH/LH", 
                   outliers_idx = 330, outliers_val = 1372.826)
bp2 <- fun_boxplot(data1, "Vit D3 (ng/mL)", "Vitamina D3 (ng/mL)", 
                   outliers_idx = c(192, 196), outliers_val = c(6014.86, 5418.6))

bp_final <- bp1 + bp2 + 
  plot_annotation(
    title = "Detección de Valores Atípicos",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", 
                                            size = 15, color = "#1A5276"))
  )
print(bp_final)

# Eliminamos observaciones con errores de medición

# data1[330,] # FSH/LH = 1372.826
# data1[192,] # Vit D3 (ng/mL) = 6014.86
# data1[196,] # Vit D3 (ng/mL) = 5418.6

data1 <- data1 %>% slice(-c(330, 192, 196))

# Eliminamos NAs restantes ya que son muy pocos 

# cat("NAs antes de omisión:", sum(is.na(data1)), "\n")
data1 <- na.omit(data1)
# cat("NAs después de omisión:", sum(is.na(data1)), "\n")



# Numéricas 
data_num <- data1 %>% 
  dplyr::select(where(is.numeric), -all_of(vars$target))

# Categóricas 
data_cat <- data1 %>% 
  dplyr::select(where(is.factor))
vars$numericas   <- names(data_num)
vars$categoricas <- names(data_cat)



tabla <- data.frame(
  Variable = c(
    "PCOS..Y.N.", "Age..yrs.", "BMI", "Blood.Group",
    "Pulse.rate.bpm.", "RR..breaths.min.", "Hb..g.dl.",
    "Cycle.length.days.", "Marraige.Status..Yrs.", "FSH.LH",
    "Waist.Hip.Ratio", "TSH..mIU.L.", "AMH.ng.mL.",
    "PRL.ng.mL.", "Vit.D3..ng.mL.", "PRG.ng.mL.",
    "RBS.mg.dl.", "BP._Systolic..mmHg.", "BP._Diastolic..mmHg.",
    "Total_Follicle", "Avg.F.size..L...mm.", "Avg.F.size..R...mm.",
    "Endometrium..mm.", "Pregnant.Y.N.", "Cycle.R.I.",
    "Weight.gain.Y.N.", "hair.growth.Y.N.", "Skin.darkening..Y.N.",
    "Hair.loss.Y.N.", "Pimples.Y.N.", "Fast.food..Y.N.",
    "Reg.Exercise.Y.N.", "Abortions.Y.N."
  ),

  Descripcion = c(
    "Diagnóstico de Síndrome de Ovario Poliquístico (0 = No, 1 = Sí)",
    "Edad de la paciente (años)",
    "Índice de Masa Corporal (relación peso/talla)",
    "Grupo sanguíneo codificado",
    "Frecuencia cardíaca en reposo (lat/min)",
    "Frecuencia respiratoria (resp/min)",
    "Nivel de hemoglobina (g/dL)",
    "Duración del sangrado menstrual (días)",
    "Años transcurridos desde el matrimonio",
    "Relación entre hormona foliculoestimulante (FSH) y hormona luteinizante (LH)",
    "Índice cintura-cadera",
    "Hormona estimulante de la tiroides (TSH, mIU/L)",
    "Hormona antimülleriana (ng/mL)",
    "Nivel de prolactina (ng/mL)",
    "Concentración de vitamina D3 (ng/mL)",
    "Nivel de progesterona (ng/mL)",
    "Glucosa en sangre en momentos aleatorios (mg/dL)",
    "Presión arterial sistólica (mmHg)",
    "Presión arterial diastólica (mmHg)",
    "Número total de folículos en ambos ovarios",
    "Tamaño promedio de folículos en ovario izquierdo (mm)",
    "Tamaño promedio de folículos en ovario derecho (mm)",
    "Grosor del endometrio (mm)",
    "¿La paciente está embarazada? (0 = No, 1 = Sí)",
    "¿Hay regularidad del ciclo menstrual? (R = Regular, I = Irregular)",
    "¿Hay aumento de peso? (0 = No, 1 = Sí)",
    "¿Hay hirsutismo o crecimiento excesivo de vello? (0 = No, 1 = Sí)",
    "¿Hay Oscurecimiento de la piel? (0 = No, 1 = Sí)",
    "¿Hay caída de cabello o alopecia? (0 = No, 1 = Sí)",
    "¿Hay alta presencia de acné? (0 = No, 1 = Sí)",
    "¿Se consume frecuentemente comida rápida? (0 = No, 1 = Sí)",
    "¿Se realiza ejercicio frecuentemente? (0 = No, 1 = Sí)",
    "Indica si la paciente ha abortado (0 = No, 1 = Sí)"
  ),

  Tipo_de_Información = c(
    "Diagnóstico clínico", "Demográfica", "Antropométrica / Metabólica",
    "Información clínica general", "Signos vitales", "Signos vitales",
    "Hematológica", "Ginecológica / Antecedentes obstétricos",
    "Demográfica", "Hormonal",
    "Antropométrica / Metabólica", "Hormonal",
    "Hormonal / Ovárica", "Hormonal", "Metabólica / Nutricional",
    "Hormonal / Antecedentes obstétricos", "Metabólica", "Cardiovascular",
    "Cardiovascular", "Ginecológica / Ovárica",
    "Ginecológica / Ovárica", "Ginecológica / Ovárica",
    "Ginecológica / Ovárica", "Antecedentes obstétricos",
    "Ginecológica / Antecedentes obstétricos", "Clínica / Metabólica",
    "Clínica / Hiperandrogenismo", "Clínica / Metabólica",
    "Clínica / Hiperandrogenismo", "Clínica / Hiperandrogenismo",
    "Estilo de vida", "Estilo de vida",
    "Antecedentes obstétricos"
  )
)

kable(tabla,
      format = "latex",
      booktabs = TRUE,
      escape = TRUE,
      align = "l") %>%
  kable_styling(latex_options = c("HOLD_position")) %>%
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "8cm") %>%
  column_spec(3, width = "4cm")




# Matriz de Pearson
mat_corr <- cor(data_num, use = "pairwise.complete.obs", method = "pearson")
mat_corr_melted <- melt(mat_corr)
colnames(mat_corr_melted) <- c("Variable1", "Variable2", "Correlación")

ggplot(mat_corr_melted, aes(x = Variable1, y = Variable2, fill = Correlación)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", mid = "#f7f7f7", high = "cyan3", 
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  geom_text(aes(label = sprintf("%.2f", Correlación)), size = 3, color = "black") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Matriz de correlaciones", x = NULL, y = NULL)

# Tabla de correlaciones 
corr_filtrada <- as.data.frame(mat_corr_melted) %>%
  filter(abs(Correlación) >= 0.30, Variable1 != Variable2) %>%
  arrange(desc(abs(Correlación)))



# PCA 
# Detecta multicolinealidad entre variables clínicas numéricas y permite reducción de ruido
# Excluimos PCOS y escalamos para dar peso equitativo a todas las variables

pca_res <- prcomp(data_num, scale. = TRUE, center = TRUE)

fviz_screeplot(pca_res, addlabels = TRUE, ylim = c(0, 20),
               barfill = "#AED6F1",       
               barcolor = "#1A5276",      
               linecolor = "#943126",     
               main = "Varianza Explicada (PCA)",
               xlab = "Componente Principal",
               ylab = "Porcentaje de Varianza Explicada") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, 
                              color = "#1A5276",
                              margin = ggplot2::margin(b = 10)),
    axis.title = element_text(size = 11, face = "bold", color = "#1A5276"),
    axis.text = element_text(size = 10, color = "#5499C7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


fviz_pca_var(pca_res, col.var = "contrib", gradient.cols = c("#5499C7","#1A5276","#943126")) +
  labs(title = "Contribución de variables al PCA")



data_fa <- scale(data_num)
# Determinamos el número de factores (prueba de Bartlett)
set.seed(123)

fa_par <- fa.parallel(data_fa, fa = "fa", n.iter = 100, show.legend = FALSE, plot = FALSE)

eigen_obs <- fa_par$fa.values      # Eigenvalores observados
eigen_sim <- fa_par$fa.sim         # Eigenvalores simulados
n_vars <- length(eigen_obs)

df_fa <- data.frame(
  Factor = 1:n_vars,
  Observado = eigen_obs,
  Simulado = eigen_sim
)

ggplot(df_fa, aes(x = Factor)) +
  geom_line(aes(y = Simulado, color = "Simulado"), 
            size = 1.2, linetype = "dashed") +
  geom_point(aes(y = Simulado, color = "Simulado"), 
             size = 3, shape = 17) +
  geom_line(aes(y = Observado, color = "Observado"), 
            size = 1.5) +
  geom_point(aes(y = Observado, color = "Observado"), 
             size = 3.5, shape = 16) +
  scale_color_manual(
    values = c("Observado" = "#1A5276", "Simulado" = "#943126"),
    name = "Eigenvalores",
    labels = c("Observado" = "Datos reales", "Simulado" = "Datos aleatorios")
  ) +
  labs(
    title = "Análisis Paralelo para Selección de Factores",
    subtitle = "Comparación de eigenvalores observados vs. simulados",
    x = "Número de Factores",
    y = "Eigenvalor"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, 
                              color = "#1A5276",
                              margin = ggplot2::margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#5499C7",
                                 margin = ggplot2::margin(b = 10)),
    axis.title = element_text(size = 11, face = "bold", color = "#1A5276"),
    axis.text = element_text(size = 10, color = "#5499C7"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#F4F9FD", size = 0.3),
    legend.title = element_text(size = 10, face = "bold", color = "#1A5276"),
    legend.text = element_text(size = 9, color = "#5499C7"),
    legend.position = c(0.85, 0.85),
    legend.background = element_rect(fill = "white", color = "#AED6F1", size = 0.5),
    plot.margin = ggplot2::margin(15, 20, 15, 15)
  )



fa_varimax <- fa(
  data_fa,
  nfactors = 4,
  rotate = "varimax",
  fm = "ml"
)

loadings_df <- as.data.frame(unclass(fa_varimax$loadings))

kable(
  round(loadings_df, 3),
  caption = "Cargas factoriales obtenidas mediante análisis factorial con rotación Varimax."
)

# Gráfico 
fa.diagram(fa_varimax, simple = TRUE, cut = 0.3)


# Scores factoriales
scores_fa <- factor.scores(data_fa, fa_varimax)$scores

grupo_pcos <- as.character(data1$"PCOS")
colores <- ifelse(grupo_pcos == "1", "#943126", "#5499C7")  

par(mar = c(5, 5, 4, 2) + 0.1, cex.lab = 1.1, cex.axis = 0.9)

# Gráfico
plot(scores_fa[, 1], scores_fa[, 2],
     col = colores, pch = 19, 
     cex = 1.1, xlab = "Factor 1", 
     ylab = "Factor 2",
     main = "Proyección de Pacientes en Factores Rotados (FA)",
     sub = "Análisis Factorial con rotación Varimax",
     cex.main = 1.3, cex.sub = 0.9,
     col.main = "#1A5276", col.lab = "#1A5276",
     col.axis = "#5499C7")

abline(h = 0, v = 0, lty = 2, col = "#AED6F1")

legend("topright", 
       legend = c("Sin PCOS", "Con PCOS"),
       col = c("#5499C7", "#943126"), 
       pch = 19,
       pt.cex = 1.2,
       bg = "white",
       box.col = "#AED6F1",
       text.col = "#1A5276",
       cex = 0.9)
par(mar = c(5, 4, 4, 2) + 0.1)



# Distancias 
#para numéricos: euclidean, manhattan, minkowski, mahalanobis, maximum
# para mixtos: goer, podani (aunque no lo vimos... y podríamos omitirlo)

data_num_scaled <- scale(data_num)

# Distancia Euclidiana (solo numéricas)
dist_euclid <- dist(data_num_scaled, method = "euclidean")
mds_euclid <- cmdscale(dist_euclid, k = 2)

# Distancia Manhattan (solo numéricas)
dist_manhattan <- dist(data_num_scaled, method = "manhattan")
mds_manhattan <- cmdscale(dist_manhattan, k = 2)

# Distancia de Gower 
vars_cat_sin_pcos <- setdiff(vars$categoricas, vars$target)
data_mds_gower <- data1[, intersect(c(vars$numericas, vars_cat_sin_pcos), names(data1))]
dist_gower <- daisy(data_mds_gower, metric = "gower")
mds_gower <- cmdscale(dist_gower, k = 2)


plot_mds <- function(mds_res, title, subtitle = "") {
  data.frame(
    MDS1 = mds_res[,1], 
    MDS2 = mds_res[,2], 
    PCOS = factor(data1$PCOS, levels = c("0", "1"), labels = c("Sin PCOS", "Con PCOS"))
  ) %>%  
    ggplot(aes(x = MDS1, y = MDS2, color = PCOS)) +
    geom_point(alpha = 0.7, size = 2.2, shape = 19) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#AED6F1", size = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#AED6F1", size = 0.5) +
    scale_color_manual(
      values = c("Sin PCOS" = "#5499C7", "Con PCOS" = "#943126"),
      name = "Diagnóstico"
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Dimensión 1",
      y = "Dimensión 2"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, 
                                color = "#1A5276",
                                margin = ggplot2::margin(b = 3)),
      plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#5499C7",
                                   margin = ggplot2::margin(b = 8)),
      axis.title = element_text(size = 10, face = "bold", color = "#1A5276"),
      axis.text = element_text(size = 9, color = "#5499C7"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#F4F9FD", size = 0.3),
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

p1 <- plot_mds(mds_euclid, "Distancia Euclidiana", "Solo variables numéricas")
p2 <- plot_mds(mds_manhattan, "Distancia Manhattan", "Solo variables numéricas")
p3 <- plot_mds(mds_gower, "Distancia de Gower", "Variables mixtas")

p_final <- p1 + p2 + p3 + 
  plot_layout(ncol = 3, guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold", color = "#1A5276"),
    legend.text = element_text(size = 9, color = "#5499C7"),
    legend.background = element_rect(fill = "white", color = "#AED6F1", size = 0.5),
    legend.box.margin = ggplot2::margin(5, 0, 0, 0)
  ) &
  plot_annotation(
    title = "Comparación de Métricas de Distancia para MDS",
    subtitle = "Análisis de Escalamiento Multidimensional",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14, 
                                color = "#1A5276",
                                margin = ggplot2::margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#5499C7")
    )
  )

print(p_final)



modelo_MCA <- FactoMineR::MCA(data_cat, ncp = 5, graph = FALSE)

# individuos en el MCA
factoextra::fviz_mca_ind(
  modelo_MCA,
  col.ind = "cos2",                      
  gradient.cols = c("cyan","#1A5276","red3"), 
  repel = TRUE,                          
  ggtheme = ggplot2::theme_minimal(),
  alpha.ind = "cos2"                    
) +
  labs(
    title = "Proyección de pacientes en el MCA",
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
 



# MCA
factoextra::fviz_mca_var(modelo_MCA, col.var = "cos2", 
             gradient.cols = c("cyan","#1A5276","red"),
             repel = TRUE, ggtheme = ggplot2::theme_minimal()) +
  labs(title = "Asociaciones entre categorías categóricas (MCA)")




knitr::kable(modelo_MCA$eig[1:5, ], digits = 3, 
      col.names = c("Valor propio", "% Inercia", "% Acumulado"),
      caption = "Dimensión vs. Varianza Categórica Capturada")

factoextra::fviz_contrib(modelo_MCA, choice = "var", axes = 1, top = 10) +
  labs(title = "Contribución de categorías a Dimensión 1 (MCA)")




# Detección de observasión rara en los antecedentes de aborto
# fviz_mca_ind(modelo_MCA, select.ind = list(name = 100), repel = TRUE, col.ind = "red")

# data1[100,] (no. abortos = 100) | Pinta morado y se coloca del lado derecho
# Esta obs corresponde a una paciente que no posee el padecimiento

# Viendo que tiene cierto grado de contribución en la construcción de las dim. podría parecer que la fértilidad tiene cierto impacto el no tener la condición. Sin embargo, leyendo literatura médica esta suposición se ha puesto en tela de juicio y si comparamos las proporciones de los grupos respecto a las variables de fertilidad vemos que no refleja esta suposición que se tiene sobre la fertilidad. Igualmente, si observamos detenidamente el gráfico de asociaciones entre categorías cat podemos notar que la variable "Aborptions(Y/N)" contribuye muy poco a la construcción de las dim. 

# Separamos las obs sin SOP
#SOP0x = data1 %>% filter(PCOS=='0') %>% dplyr::select(-PCOS)
# Separamos las obs con SOP
#SOP1x = data1 %>% filter(PCOS=='1') %>% dplyr::select(-PCOS)




set.seed(123)  
train_idx <- caret::createDataPartition(data1$`PCOS`, p = 0.8, list = FALSE)
train <- data1[train_idx, ]
test  <- data1[-train_idx, ]


cat("Distribución de PCOS en Train:", table(train$PCOS), "\n")
cat("Distribución de PCOS en Test :", table(test$PCOS), "\n")

# Niveles de los factores en el train y test
var_predictoras <- setdiff(vars$categoricas, vars$target)
for(v in var_predictoras) {
  if(v %in% names(train) && v %in% names(test)) {
    test[[v]] <- factor(test[[v]], levels = levels(train[[v]]))
    nas <- is.na(test[[v]])
    if(any(nas)) test[[v]][nas] <- names(sort(table(train[[v]]), decreasing = TRUE))[1]
  }
}




# Esta parte del código viene del repositorio del maestro  tal cual 
# Función que grafica histograma, qqplot, box plot y la prueba de Anderson-Darling.

univariateNormalityPlots <- function(X, test = "ad", ncol = 2) {

  # Asegurar que solo variables numéricas entren
  X <- X[, sapply(X, is.numeric), drop = FALSE]
  p <- ncol(X)
  var_names <- colnames(X)

  plots_list <- list()

  for(i in seq_len(p)) {
    x <- X[, i]
    # quitar NA por seguridad
    x <- x[!is.na(x)]

    df <- data.frame(x = x)
    ## Histograma
    h <- ggplot(df, aes(x)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 15,
                     fill = "skyblue3",
                     color = "black",
                     alpha = 0.6) +
      stat_function(fun = dnorm,
                    args = list(mean = mean(x), sd = sd(x)),
                    color = "darkred",
                    linewidth = 1) +
      theme_minimal(base_size = 10) +
      labs(title = paste0(var_names[i], " - Histogram"))

    ## QQ plot
    q <- ggplot(df, aes(sample = x)) +
      stat_qq(size = 0.8) +
      stat_qq_line(color = "red") +
      theme_minimal(base_size = 10) +
      labs(title = paste0(var_names[i], " - Q-Q Plot"))

    ## Test de normalidad
    p_val <- switch(test,
                    ad = nortest::ad.test(x)$p.value,
                    cvm = nortest::cvm.test(x)$p.value,
                    lillie = nortest::lillie.test(x)$p.value,
                    sf = nortest::sf.test(x)$p.value)
    label <- paste0(
      toupper(test),
      " p-value = ",
      signif(p_val, 4)
    )

    pv <- ggplot() +
      annotate("text", x = 0, y = 0, label = label, size = 5) +
      theme_void()

    ## Layout por variable (compacto)
    plots_list[[i]] <- (h | q) / pv
  }

  ## Ajustepara no saturar en una sola pantalla
  final_plot <- wrap_plots(plots_list, ncol = ncol)

  print(final_plot)

  invisible(final_plot)
}

# Función que grafica el qqplot de la forma cuadrática de una distribución normal
# contra los cuantiles de una distribución chi-cuadrado con p grados de libertad.

quadraticFormPlot = function(X, estimate = TRUE, mu = NULL, Sigma = NULL){
  
  library(ggplot2)
  
  n <- nrow(X)
  p <- ncol(X)
  
  if(estimate){
    mu <- colMeans(X)
    Sigma <- cov(X)
  }
  
  ## Mahalanobis distances
  D2 <- mahalanobis(X, mu, Sigma)
  
  ## theoretical quantiles
  probs <- ppoints(n)
  theo <- qchisq(probs, df = p)
  
  ## empirical quantiles
  emp <- sort(D2)
  
  df <- data.frame(theo, emp)
  
  q <- ggplot(df, aes(x = theo, y = emp)) +
    geom_point(size = 1) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    theme_minimal() +
    labs(x = expression(chi[p]^2~"theoretical quantiles"),
         y = "Empirical Mahalanobis distances",
         title = "Chi-square Q-Q Plot")
  
  print(q)
}




#df para pruebas
data_mod <- data.frame(PCOS = data1$PCOS, data_num)

# Separamos las obs sin SOP
SOP0 = data_mod %>% filter(PCOS=='0') %>% dplyr::select(-PCOS)

# Separamos las obs con SOP
SOP1 = data_mod %>% filter(PCOS=='1') %>% dplyr::select(-PCOS)

## Pila de pruebas de norm para ambos grupos

#mvn(SOP0, mvnTest="hz")$multivariateNormality
#mvn(SOP0, mvnTest="royston")$multivariateNormality
#mvn(SOP0, mvnTest="mardia")$multivariateNormality

#mvn(SOP1, mvnTest="hz")$multivariateNormality
#mvn(SOP1, mvnTest="royston")$multivariateNormality
#mvn(SOP1, mvnTest="mardia")$multivariateNormality

#univariateNormalityPlots(SOP0[, 1:6])
#univariateNormalityPlots(SOP0[, 7:12])
#univariateNormalityPlots(SOP0[, 13:18])

#univariateNormalityPlots(SOP1[, 1:6])
#univariateNormalityPlots(SOP1[, 7:12])
#univariateNormalityPlots(SOP1[, 13:18])

#quadraticFormPlot(SOP1)
#quadraticFormPlot(SOP0)

# Prueba de igualdad de matrices de cov

#X <- data_mod %>% dplyr::select(-PCOS)
#g <- factor(data_mod$PCOS)
#boxM(X, g)

## No se superan pruebas




# Preparamos los datos para LDA/QDA (solo numéricas)
cols_lda <- c(vars$numericas, vars$target)
train_data <- train %>% dplyr::select(all_of(cols_lda))
test_data  <- test  %>% dplyr::select(all_of(cols_lda))


target_levels <- c("0", "1")
train_data$PCOS <- factor(as.character(train_data$PCOS), levels = target_levels)
test_data$PCOS  <- factor(as.character(test_data$PCOS),  levels = target_levels)

# Ajuste
modelo_lda <- lda(as.formula(paste(vars$target, "~ .")), data = train_data)
modelo_qda <- qda(as.formula(paste(vars$target, "~ .")), data = train_data)

# Predicciones
pred_lda <- predict(modelo_lda, newdata = test_data)
pred_qda <- predict(modelo_qda, newdata = test_data)

# Alineamos niveles de predicciones con la referencia
pred_lda_class <- factor(as.character(pred_lda$class), levels = target_levels)
pred_qda_class <- factor(as.character(pred_qda$class), levels = target_levels)

# Evaluación
cm_lda <- confusionMatrix(pred_lda_class, test_data$PCOS, positive = "1")
cm_qda <- confusionMatrix(pred_qda_class, test_data$PCOS, positive = "1")

# cat("\n--- Métricas LDA ---\n")
# print(cm_lda$byClass[c("Sensitivity", "Specificity")])
# cat("Accuracy:", round(cm_lda$overall["Accuracy"], 3), "\n")

# cat("\n--- Métricas QDA ---\n")
# print(cm_qda$byClass[c("Sensitivity", "Specificity")])
# cat("Accuracy:", round(cm_qda$overall["Accuracy"], 3), "\n")




# Esta tabla tiene los mismos resultados que los que se obtendrían arriba pero es para mejorar la presentación 
tabla_metricas <- data.frame(
  Modelo = c("LDA", "QDA"),
  Sensibilidad = c(
    cm_lda$byClass["Sensitivity"],
    cm_qda$byClass["Sensitivity"]
  ),
  Especificidad = c(
    cm_lda$byClass["Specificity"],
    cm_qda$byClass["Specificity"]
  ),
  Accuracy = c(
    cm_lda$overall["Accuracy"],
    cm_qda$overall["Accuracy"]
  )
) |>
  mutate(
    across(-Modelo, ~ sprintf("%.1f%%", .x * 100))
  )

kable(
  tabla_metricas,
  caption = "Métricas de desempeño para los modelos LDA y QDA",
  align = c("l", "c", "c", "c"),
  booktabs = TRUE,
  format = "latex"
) |>
  kable_styling(
    latex_options = c("HOLD_position", "striped", "scale_down"),
    stripe_color = "#F4F9FD", 
    font_size = 11
  ) |>
  row_spec(0, background = "#1A5276", color = "white", bold = TRUE)



# Densidad del discriminante lineal (LDA)

lda_scores_train <- predict(modelo_lda, newdata = train_data)$x
df_lda_scores <- data.frame(LD1 = lda_scores_train, PCOS = train_data$PCOS)

ggplot(df_lda_scores, aes(x = LD1, fill = PCOS)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("0" = "#2166ac", "1" = "#b2182b"),
                    labels = c("Sin PCOS", "Con PCOS"), name = "PCOS") +
  labs(title = "Distribución del Discriminante Lineal (LDA)",
       x = "Puntuación LD1", y = "Densidad") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# Función auxiliar para asegurar que los factores sean corretcos en confusionMatrix
factores<- function(pred, ref, positive="1") {
  lv <- c("0", "1")
  pred_f <- factor(as.character(pred), levels = lv)
  ref_f  <- factor(as.character(ref),  levels = lv)
  confusionMatrix(pred_f, ref_f, positive = positive)
}
# arbol representativo 

tree_simple <- rpart(PCOS ~ ., data = train[, c(vars$numericas, setdiff(vars$categoricas, "PCOS"), "PCOS")], 
                     method = "class", 
                     control = rpart.control(maxdepth = 4, minsplit = 20))
par(mar = c(1,1,2,1))
rpart.plot(tree_simple, 
           main = "Árbol de Decisión Representativo (profundidad=4)",
           type = 4, extra = 101, fallen.leaves = TRUE,
           box.palette = c("lightgrey", "#5499C7"),
           shadow.col = "gray", nn = TRUE)
par(mar = c(5.1, 4.1, 4.1, 2.1)) # Restaurar márgenes




# Preparamos predictores, excluimos la variable objetivo
predictores_train <- train %>% dplyr::select(-all_of(vars$target))
predictores_test  <- test  %>% dplyr::select(-all_of(vars$target))


train$PCOS <- factor(as.character(train$PCOS), levels = c("0", "1"))
test$PCOS  <- factor(as.character(test$PCOS), levels = c("0", "1"))


# Entrenamiento de Random Forest
set.seed(123)
rf_model <- randomForest(
  x = predictores_train,
  y = train$PCOS,
  ntree = 500,               # Número de árboles
  mtry = floor(sqrt(ncol(predictores_train))), # Variables probadas por nodo 
  importance = TRUE          
)


# Predicciones
pred_rf <- predict(rf_model, newdata = predictores_test)
prob_rf <- predict(rf_model, newdata = predictores_test, type = "prob")[, "1"]

# print(rf_model)   <- ES AQUI DONDE SE OBTIENEN LOS DATOS PARA MI MATRIZ DE CONFUSION PARA MI TRAIN

# Predicciones
pred_rf <- predict(rf_model, newdata = predictores_test)
prob_rf <- predict(rf_model, newdata = predictores_test, type = "prob")[, "1"]


target_levels <- c("0", "1")
pred_rf_factor <- factor(as.character(pred_rf), levels = target_levels)
ref_rf_factor  <- factor(as.character(test$PCOS), levels = target_levels)

# Matriz de confusión y métricas
cm_rf <- confusionMatrix(pred_rf_factor, ref_rf_factor, positive = "1")

# MATRIZ PARA MI TRAIN... 
cm_oob <- rf_model$confusion
cm_counts <- cm_oob[, 1:2]

cm_df <- data.frame(
  Reference = factor(rep(rownames(cm_counts), each = ncol(cm_counts)), 
                     levels = c("0", "1")),
  Prediction = factor(rep(colnames(cm_counts), times = nrow(cm_counts)), 
                      levels = c("0", "1")),
  Freq = as.vector(t(as.matrix(cm_counts)))
)

# Calculamos las métricas
#           Predicho 0  Predicho 1
# Real 0        TN           FP
# Real 1        FN           VP

TN <- cm_counts[1, 1]  # Real 0, Predicho 0
FP <- cm_counts[1, 2]  # Real 0, Predicho 1
FN <- cm_counts[2, 1]  # Real 1, Predicho 0
VP <- cm_counts[2, 2]  # Real 1, Predicho 1

accuracy <- (TN + VP) / (TN + FP + FN + VP)
sensitivity <- VP / (VP + FN)  # Tasa de verdaderos positivos
specificity <- TN / (TN + FP)  # Tasa de verdaderos negativos

# Obtenemos error OOB del modelo
oob_error <- rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]


ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 4.5) +
  scale_fill_gradient(low = "white", high = "#2166ac", name = "Pacientes") +
  labs(
    title = "Matriz de Confusión OOB (Entrenamiento)",
    subtitle = sprintf("Error OOB: %.2f%% | Exactitud: %.3f | Sensibilidad: %.3f | Especificidad: %.3f", 
                       oob_error * 100, accuracy, sensitivity, specificity),
    x = "Real",
    y = "Predicho"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9))


# Matriz de confusión para mi test 

pred_rf_test <- predict(rf_model, newdata = predictores_test)
cm_rf_viz <- factores(pred_rf, test$PCOS, positive = "1")

cm_df <- as.data.frame(cm_rf_viz$table)
colnames(cm_df) <- c("Predicho", "Real", "Count")

p_cm <- ggplot(cm_df, aes(x = Predicho, y = Real, fill = Count)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Count), size = 4.5, color = "black") +
  scale_fill_gradient(low = "#f7f7f7" , high = "#b2182b", name = "Pacientes") +
  labs(title = "Matriz de Confusión (Conjunto de Prueba)",
       subtitle = paste("Exactitud:", round(cm_rf_viz$overall["Accuracy"], 3), 
                       "| Sensibilidad:", round(cm_rf_viz$byClass["Sensitivity"], 3),
                       "| Especificidad:", round(cm_rf_viz$byClass["Specificity"], 3))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 11, face = "bold"))
print(p_cm)



auc_rf <- auc(roc(test$PCOS, prob_rf, levels = c("0", "1")))

metricas_rf <- data.frame(
  Métrica = c("Accuracy", "Sensitivity", "Specificity", "AUC"),
  Valor = round(c(
    cm_rf$overall["Accuracy"],
    cm_rf$byClass["Sensitivity"],
    cm_rf$byClass["Specificity"],
    auc_rf
  ), 3)
)

# print(metricas_rf, row.names = FALSE)

# Curva ROC
roc_rf <- roc(test$PCOS, prob_rf, levels = c("0", "1"))
roc_data <- data.frame(
  specificity = roc_rf$specificities,
  sensitivity = roc_rf$sensitivities
)

ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_area(fill = "#AED6F1", alpha = 0.35) +
  geom_line(color = "#1A5276", size = 1.3) +
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "#943126", size = 0.9, alpha = 0.7) +
  annotate("text", x = 0.65, y = 0.25,
           label = paste("AUC =", round(auc_rf, 3)),
           size = 5.5, fontface = "bold", color = "#1A5276",
           hjust = 0.5) +
  labs(
    title = "Curva ROC - Random Forest",
    subtitle = paste("Evaluación en conjunto de prueba (AUC =", round(auc_rf, 3), ")"),
    x = "Especificidad ",
    y = "Sensibilidad "
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  coord_equal() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, 
                              color = "#1A5276",
                              margin = ggplot2::margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#5499C7",
                                 margin = ggplot2::margin(b = 10)),
    axis.title = element_text(size = 11, face = "bold", color = "#1A5276"),
    axis.text = element_text(size = 10, color = "#5499C7"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#F4F9FD", size = 0.3),
    plot.margin = ggplot2::margin(15, 20, 15, 15)
  )


hist(prob_rf)


# Partial Dependence Plots (Efecto marginal de las variables)

# Se excluye de doc pero se podría usar en expo para int RF

# Función para calcular PDP de clasificación
calcular_pdp <- function(model, var_name, train_data, target_col, n_pts = 30) {
  vals <- seq(min(train_data[[var_name]], na.rm=TRUE), max(train_data[[var_name]], na.rm=TRUE), length.out = n_pts)
  probs <- numeric(length(vals))
  grid_base <- train_data[, setdiff(names(train_data), target_col), drop=FALSE]
  
  for(i in seq_along(vals)) {
    grid_temp <- grid_base
    grid_temp[[var_name]] <- vals[i]
    preds <- predict(model, newdata = grid_temp, type = "prob")[, "1"]
    probs[i] <- mean(preds, na.rm = TRUE)
  }
  invisible(data.frame(Value = vals, Probability = probs)) 
}

vars_pdp <- c("AMH(ng/mL)", "BMI", "FSH/LH", "Age (yrs)")
vars_pdp <- intersect(vars_pdp, vars$numericas)

pdp_plots <- lapply(vars_pdp, function(var) {
  df_pdp <- calcular_pdp(rf_model, var, train, vars$target)
  
  ggplot(df_pdp, aes(x = Value, y = Probability)) +
    geom_line(color = "#b2182b", linewidth = 1.2) +
    geom_point(size = 1) +
    labs(title = paste("Efecto marginal de", var), x = var, y = "P(PCOS=1)") +
    theme_minimal() + theme(plot.title = element_text(size = 10, hjust = 0.5))
})

do.call(grid.arrange, c(pdp_plots, ncol = 2))




# Distribución de probabilidades 

df_prob_dist <- data.frame(
  Probability = prob_rf,
  Real_Class = factor(test$PCOS, levels = c("0", "1"), labels = c("Sin PCOS", "Con PCOS"))
)

p_prob_dist <- ggplot(df_prob_dist, aes(x = Probability, fill = Real_Class)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("Sin PCOS" = "#2166ac", "Con PCOS" = "#b2182b")) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  labs(title = "Distribución de Probabilidades Predichas",
       subtitle = "Separación entre clases reales (línea punteada = umbral 0.5)",
       x = "Probabilidad Predicha de PCOS", y = "Densidad") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_prob_dist)





imp_data <- as.data.frame(importance(rf_model, type = 1))
imp_data$Variable <- rownames(imp_data)
imp_data <- imp_data %>% arrange(desc(MeanDecreaseAccuracy)) %>% head(10)

p_imp <- ggplot(imp_data, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_col(fill = "#2166ac") +
  coord_flip() +
  labs(title = "10 Variables Más Importantes",
       x = "", y = "Importancia") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p_imp)
