
# --------------------------------------------------------------------# 
# ███╗   ███╗██████╗ ██╗   ██╗██╗      ██████╗         ██████╗        #
# ████╗ ████║██╔══██╗██║   ██║██║     ██╔═══██╗██╗    ██╔═████╗       #
# ██╔████╔██║██║  ██║██║   ██║██║     ██║   ██║╚═╝    ██║██╔██║       #
# ██║╚██╔╝██║██║  ██║██║   ██║██║     ██║   ██║██╗    ████╔╝██║       #
# ██║ ╚═╝ ██║██████╔╝╚██████╔╝███████╗╚██████╔╝╚═╝    ╚██████╔╝       #
# ╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚══════╝ ╚═════╝         ╚═════╝        #
# --------------------------------------------------------------------# 
#       CARGA DE DATOSLIBRERÍA Y SELECCION DE VARIABLES               #
# --------------------------------------------------------------------# 

# ------------------------- #
#       Carga de datos      #
# ------------------------- #



#-- Establecer directorio de trabajo y cargar datos
setwd("D:/OneDrive - CGIAR/Desktop/TDG/");source("Códigos y salidas/Funciones base.R")

#setwd("C:/Users/kmili/Desktop/Todo/TDG/Código");source("Funciones base.R")
Base <-read_dta("Datos/Base_unida [ARROZ Y MAÍZ PARA TDG].dta")
Base$F2[Base$F2 == -99] <- NA
# Se identificar los prodcutores que no pudieron identificarse directamente, o que son familiares de tal y se lograron mapear.
Base$Condición_P_2 <- ifelse(rowSums(!is.na(Base[, c("Edad_P", "Genero_P", "Escolaridad_P_Cat")])) > 0,1,0)

tabla_promedios <- Base %>%
  group_by(A1_std) %>%
  summarise(
    n = n(),
    promedio_adop = mean(Adop_Pronósticos, na.rm = TRUE)
  ) %>%
  ungroup()
# ------------------------- ## select()------------------------- #
#   Dejar variables a usar  #
# ------------------------- #


# Creando nueva KEY ya que entre dfs pueden haber repretidas
Base <- Base %>% mutate(sufijo = case_when(Origen_Datos == "Arroz CAS"   ~ "AC",Origen_Datos == "Arroz LECRA" ~ "AL",
Origen_Datos == "Maíz CAS"    ~ "MC",Origen_Datos == "Maíz LECRA"  ~ "ML",TRUE ~ "XX"),New_Key = paste0(ID_Productor, "_", sufijo))

# Crear variable de cultivo y año 

Base <- Base %>% mutate(Cultivo = case_when(Origen_Datos %in% c("Arroz LECRA", "Arroz CAS") ~ "Arroz",Origen_Datos %in% c("Maíz LECRA", "Maíz CAS") ~ "Maíz",
TRUE ~ NA_character_),Año = case_when(Origen_Datos %in% c("Maíz LECRA", "Arroz LECRA") ~ "2020",Origen_Datos %in% c("Maíz CAS", "Arroz CAS") ~ "2024",
TRUE ~ NA_character_))         

# Pasando cantidades vendidas = 0 como NA
# Base <- Base %>%mutate(F14 = ifelse(F14 == 0, NA, F14),F17 = ifelse(F17 == 0, NA, F17))

# - Selecionando variables
Base_F <- Base %>% select(ID_Productor,New_Key,Adop_Pronósticos,A1_std, Región, Edad_P, Genero_P, Escolaridad_P, Escolaridad_P_Cat, Etnia_P, F2, HH_size ,Num_15,Num_15_28,Num_Adultos,Num_Adultos_Mayores,share_15_28 ,share_60plus,Educación_Hogar,M1, F7_M,
F11, F12, F14,F15, F16,F17, Irrigation_system, L1, K1, org_member, received_training, J4,E1, P1, P3, P5, variety_age_group,E2,E1_A,E1_B,E1_D,E1_E,E1_F,Distancia_Final,Distancia_via,N1_A, N1_B, N1_C, N1_D, N1_E, N2_A, N2_B, N2_C, N2_D, N2_E, N2_F, N2_G, N2_H,Origen_Datos,Cultivo,Condición_P,Condición_P_2,Año)

# - Dejando otras como dicotomicas:

# Género (METER POSTERIORMENTE)
# Base_F <- Base_F %>% mutate(Genero_P = case_when(Genero_P == 1 ~ 1,Genero_P == 2 ~ 0,TRUE ~ NA_real_))

# # Pasando a factor las variaibles con más de dos niveles:
# Base_F <- Base_F %>%mutate(across(c(Región, Escolaridad_P_Cat, Etnia_P), haven::as_factor))


# ------------------------- #
#   CREAR ÍNDICE Y FIGURA   #
# ------------------------- #


Indice_Activo_DF <- crear_indice_MCA(df = Base_F %>% select(New_Key,N1_A, N1_B, N1_C, N1_D, N1_E, N2_A, N2_B, N2_C, N2_D, N2_E, N2_F, N2_G, N2_H) %>% na.omit(),id_var = "New_Key",
vars_mca =c("N1_A", "N1_B", "N1_C", "N1_D", "N1_E", "N2_A", "N2_B", "N2_C", "N2_D", "N2_E", "N2_F", "N2_G", "N2_H"),
nombre_indice = "índice_Activos",path_pesos = "TDG/Tablas y figuras/Pesos_Indicador.xlsx")

# Unir indice al df completo
Base_F <-Base_F %>%  left_join(Indice_Activo_DF %>% select(New_Key,índice_Activos)) %>% filter(Condición_P==1 | Condición_P_2==1)

# ---- Personalizar figura de MCA ----

# Definir MCA y disciconario dedescripciones
DF_MCA <- Indice_Activo_DF %>%select(-c(New_Key, índice_Activos)) %>%  mutate(across(everything(), as.factor));MCA_Fig <- MCA(DF_MCA, graph = FALSE)

diccionario <- c("N1_A" = "Bomba de agua","N1_B" = "Reservorio","N1_C" = "Tractor","N1_D" = "Moto","N1_E" = "Automovil","N2_A" = "Televisión",
                 "N2_B" = "Celular inteligente","N2_C" = "computador","N2_D" = "Radio/equipo de sonido","N2_E" = "Sanitario","N2_F" = "Agua potable","N2_G" = "Electricidad",
                 "N2_H" = "Acceso a internet")

# Definir labels con diccionario

labels_originales <- rownames(MCA_Fig$var$coord)
labels_nuevas <- sapply(labels_originales, function(x) {partes <- strsplit(x, "_(?=[01]$)", perl = TRUE)[[1]];var <- partes[1]
nivel <- partes[2];nombre <- if (!is.null(diccionario[[var]])) diccionario[[var]] else var; if (nivel == "1") {paste0(nombre, ": Sí")
} else if (nivel == "0") {paste0(nombre, ": No")} else {paste0(nombre, ": ", nivel)}})

# Definir df para graficar
coords <- as.data.frame(MCA_Fig$var$coord);coords$Etiqueta <- labels_nuevas

# - Figura

coords$Nivel <- ifelse(grepl(": Sí$", coords$Etiqueta), "Sí", "No")

# Crear etiquetas HTML con color para "Sí" y "No"
coords <- coords %>%mutate(ParteVar = sub(":.*", "", Etiqueta),ParteNivel = sub(".*: ", "", Etiqueta),ColorHTML = ifelse(
  ParteNivel == "Sí",paste0(ParteVar, ": <span style='color:#6375de;'>", ParteNivel, "</span>"),paste0(ParteVar, ": <span style='color:#bf616a;'>", ParteNivel, "</span>")
))

# Ajuste vertical (Sí un poco arriba, No un poco abajo)
coords <- coords %>%mutate(AjusteY = ifelse(Nivel == "Sí", `Dim 2` + 0.06, `Dim 2` + 0.06))

# Figura final
pdf("Códigos y salidas/Tablas y figuras/Figura_indice.pdf",width = 16,height = 10)

ggplot(coords, aes(x = `Dim 1`, y = `Dim 2`, fill = Nivel)) +geom_hline(yintercept = 0, color = "#5f6971", linetype = "dashed") +
geom_vline(xintercept = 0, color = "#5f6971", linetype = "dashed") + geom_point(shape = 21, color = "black", size = 4, stroke = 0.9) +
geom_richtext(aes(x = `Dim 1`, y = AjusteY, label = ColorHTML),fill = NA, label.color = NA,size = 5,family = "Times",
show.legend = FALSE) +scale_fill_manual(values = c("Sí" = "#6375de", "No" = "#bf616a")) +
theme_mine(base_family = "Times") + labs(title = "",x = paste0("Dim 1 (", round(MCA_Fig$eig[1,2], 1), "%)"),y = paste0("Dim 2 (", round(MCA_Fig$eig[2,2], 1), "%)")) + theme(legend.position = "none")

dev.off()


# --------------------------------------------------------------------# 
# ███╗   ███╗██████╗ ██╗   ██╗██╗      ██████╗         ██╗            #
# ████╗ ████║██╔══██╗██║   ██║██║     ██╔═══██╗██╗    ███║            #
# ██╔████╔██║██║  ██║██║   ██║██║     ██║   ██║╚═╝    ╚██║            #
# ██║╚██╔╝██║██║  ██║██║   ██║██║     ██║   ██║██╗     ██║            #
# ██║ ╚═╝ ██║██████╔╝╚██████╔╝███████╗╚██████╔╝╚═╝     ██║            #
# ╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚══════╝ ╚═════╝         ╚═╝            #
# --------------------------------------------------------------------# 
#           REVISIÓN DE NAS Y SELECCIÓN DE VARIABLES                   #
# --------------------------------------------------------------------# 

# - Añadir labels de variables recien creadas
labels_DF <- c(índice_Activos="índice de activos",Cultivo="Cultivo")
for (var in names(labels_DF)) {if (var %in% names(Base_F)) {labelled::var_label(Base_F[[var]]) <- labels_DF[var]}}


# -------------------------------- #
#   SELECCIÓN DE VARIABLES         #
# -------------------------------- #


# - Se estraen las desciprtivas de cada variables por pronóstico y diferentes log ratios de la regresión
Base_F_Descriptivas <- Base_F %>% select(ID_Productor,New_Key,Adop_Pronósticos, Región, Edad_P, Genero_P,Etnia_P, Escolaridad_P_Cat, F2, HH_size,Educación_Hogar,M1, F7_M,
F11, F12,F16,F17, Irrigation_system, L1, K1, org_member, received_training, J4,E1, P1, P3,E2,E1_A,E1_B,E1_D,E1_E,E1_F,índice_Activos,Cultivo,Año)

Base_F_Descriptivas <- Base_F_Descriptivas %>% mutate(across(c(Adop_Pronósticos,Región,Genero_P,Etnia_P, Escolaridad_P_Cat,M1, F7_M,F11,F12,Irrigation_system,L1,K1,org_member,
received_training,J4,E1,E2,P1,P3,E1_A,E1_B,E1_D,E1_E,E1_F,Año), haven::as_factor))

predictors <- setdiff(names(Base_F_Descriptivas), c("Adop_Pronósticos","New_Key","ID_Productor"));explanatory = predictors;dependent = "Adop_Pronósticos"

# - Generar tabla en formato LaTeX para guardar en el informe
Latex_Base_F_Descriptivas <- Base_F_Descriptivas %>% finalfit(dependent, explanatory) %>% kable(format = "latex",row.names = FALSE,
align = c("l","l","c","c","c","c"),booktabs = TRUE,caption = "Tabla descriptiva de adopción de pronósticos",escape = FALSE) %>% kable_styling(latex_options = c("scale_down", "hold_position"))

cat(Latex_Base_F_Descriptivas, file = "Códigos y salidas/Tablas y figuras/Tabla_Descipción_variables.tex") # Guardar

# Se eliminan variables: M1,E1_A,E1_B,E1_D,E1_E,E1_F,P1, P3, los % de personas por rango de edad se eliminan por muchos ceros
# -------------------------------- #
#   REVISIÓN DE NAS BASE COMPLETA  #
# -------------------------------- #

# - Selecionar variables a usar
Base_F_2 <- Base_F %>% select(ID_Productor,New_Key,Adop_Pronósticos, Región, Edad_P, Genero_P, Escolaridad_P_Cat, F2, HH_size,Educación_Hogar, F7_M,
F11, F12,F16,F17, Irrigation_system, L1, K1, org_member, received_training, J4,E1,E2,índice_Activos,Cultivo,Año)

# - reporte de NAS
Reporte_NA <- miss(Base_F_2,id_var="New_Key");NA_Observaciones <-  Reporte_NA$Reporte_ID

# - Revisando y borrando registros con variables  de más 4 NA (DOS CASOS)
Id_borar <- NA_Observaciones %>% filter(N_NA>4)

# Dejar base sin registros con más de 4 NA
Base_F_2_Impu.1 <- Base_F_2 %>% filter(!New_Key %in% Id_borar$ID) 

# Repore de frecuencia de NA sin esos registros comunes:
Arbol.na <- naclus(Base_F_2_Impu.1);plot(Arbol.na)

#  ----- DENDOGRAMA DE NAS SIMULTANEOS ----- #

var_labels <- sapply(Base_F_2_Impu.1 %>% select(-c(ID_Productor,New_Key)), function(x) {lab <- attr(x, "label");if (is.null(lab)) NA else lab})
var_labels[is.na(var_labels)] <- names(Base_F_2_Impu.1 %>% select(-c(ID_Productor,New_Key)))[is.na(var_labels)]

Arbol.na <- naclus(Base_F_2_Impu.1 %>% select(-c(ID_Productor,New_Key)))

par(cex.axis = 1.4,cex.lab  = 1.4,family= "Times");plot(Arbol.na,labels = var_labels,main   = "Patrones de ausencia conjunta de variables",ylab   = "Proporción de Nas",cex    = 1.4)

# - Segundo reporte de NAs
Reporte_NA_2 <- miss(Base_F_2_Impu.1,id_var="New_Key");NA_Observaciones_2 <-  Reporte_NA$Reporte_ID


# --- CREAR TABLA DE REPORTE DE NA por n y %

df_na <- data.frame(variable = names(Reporte_NA_2$Var.n),n_na     = as.numeric(Reporte_NA_2$Var.n),p_na     = as.numeric(Reporte_NA_2$Var.p) * 100)
df_na <- df_na %>%  filter(n_na > 0)
df_na$label <- sapply(df_na$variable, function(v){lab <- var_label(Base_F_Descriptivas[[v]]);if (is.null(lab) || lab == "") return(v);return(as.character(lab))})

df_na_final <- df_na %>% select(label, n_na, p_na) %>% arrange(desc(p_na)) %>% kable(format = "latex",row.names = FALSE,
align = c("c","c","c"),booktabs = TRUE,caption = "Tabla descriptiva de adopción de pronósticos",escape = FALSE) %>% 
kable_styling(latex_options = c("scale_down", "hold_position"))


# CONCLUSIÓN:

# Elimino casos con más de  4 variables como NA, tienen 5 7 8 y 10 NA en variables claves, son 4 casos
# los borro porque no parece pertinente imputar con ellos y 4 registros no cambiará NADA

# -------------------------------------------------------------- #
#  REVISIÓN DE VARIABLES Y  NA CON BASE DE REGISTROS ELIMINADOS  #
# -------------------------------------------------------------- #

# Transformación de las cantidades vendidas
Base_F_2_Impu.1 <- Base_F_2_Impu.1 %>%mutate(F17_log = log(F17 + 1)) %>% select(-F17)

# Figura de F17 vs F17log+1

# #Base_F_2_Impu.1 <- Base_F_2_Impu.1 %>%mutate(F17_log = log(F17 + 1))
#
# df_plot <- Base_F_2_Impu.1 %>%select(F17, F17_log) %>%pivot_longer(cols = everything(),
# names_to = "Variable",values_to = "Valor") %>%mutate(
# Variable = factor(Variable,levels = c("F17", "F17_log"),
# labels = c("Cantidad vendida (Ton)", "Log(Cantidad vendida + 1)")))

# stats <- df_plot %>%group_by(Variable) %>%summarise(
# min = min(Valor, na.rm = TRUE),mean = mean(Valor, na.rm = TRUE),max = max(Valor, na.rm = TRUE),.groups = "drop") %>%
# mutate(label = paste0("Mín: ", round(min, 2), "\n","Prom: ", round(mean, 2), "\n","Máx: ", round(max, 2)))
# 
# 
# ggplot(df_plot, aes(x = Valor)) +geom_density(aes(y = after_stat(density)),fill = "#707788",alpha = 0.45,
# color = "#40506d",linewidth = 1,adjust = 1,outline.type = "upper"   ) +facet_wrap(~ Variable,scales = "free",nrow = 1
# ) +geom_text(data = stats,aes(x = Inf,y = Inf,label = label),hjust = 1.1,vjust = 1.1,size = 6,family = "Times"
# ) +labs(x = NULL,y = "Densidad" ) +theme_minimal(base_family = "Times") +theme(panel.background = element_rect(fill = "white", colour = NA),
# plot.background  = element_rect(fill = "white", colour = NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
# strip.placement = "outside",strip.background = element_rect(fill = "#939EA6",color = "black",linewidth = 0.8),
# strip.text = element_text(size = 16,family = "Times"),panel.border = element_rect(color = "black",
# fill = NA,linewidth = 0.8),axis.text = element_text(size = 12),axis.title.y = element_text(size = 0))

# --- Revisando NA con otras variables por tablas y descriptivas:

# Se pasan las categóricas como tal sólo para visualización de las tablas
Base_F_2_Impu.1_CAT <- Base_F_2_Impu.1 %>% mutate(across(c(Adop_Pronósticos,Región,Genero_P, Escolaridad_P_Cat, F7_M,F11,F12,Irrigation_system,L1,K1,org_member,received_training,J4,E1,E2,Año), haven::as_factor))

# -  Tabla de variables por: Escolaridad_P_Cat
predictors <- setdiff(names(Base_F_2_Impu.1), c("Escolaridad_P_Cat","New_Key","ID_Productor"));explanatory = predictors;dependent = "Escolaridad_P_Cat"
Tabla_NA_Escolaridad <- Base_F_2_Impu.1_CAT %>% missing_compare(dependent, explanatory)


# -  Tabla de variables por: F17
predictors <- setdiff(names(Base_F_2_Impu.1), c("F17_log","New_Key","ID_Productor"));explanatory = predictors;dependent = "F17_log"
Tabla_NA_F17 <- Base_F_2_Impu.1_CAT %>% missing_compare(dependent, explanatory)


# -  Tabla de variables por: F16
predictors <- setdiff(names(Base_F_2_Impu.1), c("F16","New_Key","ID_Productor"));explanatory = predictors;dependent = "F16"
Tabla_NA_F16 <- Base_F_2_Impu.1_CAT %>% missing_compare(dependent, explanatory)


# - CART por: Escolaridad_P_Cat 
Base_F_2_Impu.1$NA_Escolaridad_P_Cat <- as.integer(is.na(Base_F_2_Impu.1$Escolaridad_P_Cat));predictors <- setdiff(names(Base_F_2_Impu.1), c("Escolaridad_P_Cat","New_Key","ID_Productor")) 

# Modelo
CART_Escolaridad_P_Cat <- rpart(NA_Escolaridad_P_Cat ~ ., data = Base_F_2_Impu.1[, c(predictors, 
"NA_Escolaridad_P_Cat")], method = "class",control = rpart.control(minbucket = 5, minsplit = 15, cp = 0.001))

# Df de importancia
Importancia_Escolaridad_P_Cat <- get_importance_with_labels(CART_Escolaridad_P_Cat, Base_F_2_Impu.1)

# - CART por: F17  
Base_F_2_Impu.1$NA_F17  <- as.integer(is.na(Base_F_2_Impu.1$F17_log));predictors <- setdiff(names(Base_F_2_Impu.1), c("F17_log","New_Key","ID_Productor","NA_Escolaridad_P_Cat")) 

# Modelo
CART_F17 <- rpart(NA_F17 ~ ., data = Base_F_2_Impu.1[, c(predictors, 
"NA_F17")], method = "class",control = rpart.control(minbucket = 5, minsplit = 15, cp = 0.001))

# Df de importancia
Importancia_F17 <- get_importance_with_labels(CART_F17, Base_F_2_Impu.1)


# - CART por: F16  
Base_F_2_Impu.1$NA_F16  <- as.integer(is.na(Base_F_2_Impu.1$F16));predictors <- setdiff(names(Base_F_2_Impu.1), c("F16","New_Key","ID_Productor","NA_Escolaridad_P_Cat","NA_F17")) 

# Modelo
CART_F16 <- rpart(NA_F16 ~ ., data = Base_F_2_Impu.1[, c(predictors, 
"NA_F16")], method = "class",control = rpart.control(minbucket = 5, minsplit = 15, cp = 0.001))

# Df de importancia
Importancia_F16 <- get_importance_with_labels(CART_F16, Base_F_2_Impu.1)


# Eliminar varaibles_NA del df

Base_F_2_Impu.1 <- Base_F_2_Impu.1 %>% select(-c(NA_Escolaridad_P_Cat,NA_F17,NA_F16))

# CONCLUSIÓN:
# Si bien es posible que los NA sean por errores del instrumento (MCAR), en las varaibles con mayor n de NA
# Se pueden encontrar relaciones entre el faltante y algunas varaibles, por lo tanto es posible asumir MAR 
# e utilizar imputación multiples para estas. Como las restantes tienen tan pocos NA <38, la revision de patrones
# puedes ser compleja, además, bajo la socuposición de que son fallos del insturmento, es coherente incluirlos
# en la imputación multiple

# -------------------------------------------------------------- #
#                     IMPUTACIÓN                                 #
# -------------------------------------------------------------- #

# Dejar factores como tal solo para imputación
Base_F_2_Impu.1_CAT_2 <- Base_F_2_Impu.1 %>% mutate(across(c(Adop_Pronósticos,Región,Genero_P, Escolaridad_P_Cat, F7_M,F11,F12,Irrigation_system,L1,K1,org_member,received_training,J4,E1,E2,Año), haven::as_factor))

# 1) Establecer parámetros a la imputacion, metodos y matriz del método

Metodo_imputación <- mice(Base_F_2_Impu.1_CAT_2, m = 1, maxit = 0)
         
Matriz_predicción <- Metodo_imputación$predictorMatrix
Método <- Metodo_imputación$method

# 2) Excluir variables que no se usaran en la imputación: IDs

Matriz_predicción["ID_Productor", ] <- 0
Matriz_predicción[, "ID_Productor"] <- 0
Matriz_predicción["New_Key", ] <- 0
Matriz_predicción[, "New_Key"] <- 0

# 3) Ejecutar imputación
Base_F_2_Impu.2 <- mice(Base_F_2_Impu.1_CAT_2, m = 12, method  = Método, predictorMatrix = Matriz_predicción, maxit = 20, seed = 1)


# 4) Evaluando calidad de imputaciones

# - Figura de Densidades var continuas

vars <- c("F16", "F17_log", "Educación_Hogar")

p <- densityplot(Base_F_2_Impu.2,as.formula(paste("~", paste(vars, collapse = "+"))),strip = strip.custom(
factor.levels = c("Rendimiento","Log (Venta + 1)","Años promedio de educación del hogar"),bg = "#939EA6",par.strip.text = list(
fontfamily = "Times",cex = 1.4)),layout = c(length(vars), 1),plot.points = FALSE,col = c("#bf616a", rep("#40506d", 12)),
lwd = c(3, rep(1, 12)),ylab = "Densidad",par.settings = list(axis.text = list(fontfamily = "Times", cex = 1.3),
par.xlab.text = list(fontfamily = "Times", cex = 1.4),par.ylab.text = list(fontfamily = "Times", cex = 1.4),
par.main.text = list(fontfamily = "Times", cex = 1.5)))

print(p)

grid.text(c("Datos originales", "Datos imputados"),x = unit(c(0.435, 0.585), "npc"),y = unit(0.035, "npc"),
gp = gpar(fontfamily = "Times",fontsize = 15))
grid.lines(x = unit(c(0.36, 0.38), "npc"),y = unit(0.035, "npc"),gp = gpar(col = "#bf616a", lwd = 3))
grid.lines(x = unit(c(0.51, 0.53), "npc"),y = unit(0.035, "npc"),gp = gpar(col = "#40506d", lwd = 1))


# Guardar df imputado para cilo de RF:

saveRDS(Base_F_2_Impu.2,"Códigos y salidas/Datos/Base_F_2_Impu.2.rds")

# --------------------------------------------------------------------# 
#                                                                     #        
# ███╗   ███╗ ██████╗ ██████╗ ██╗   ██╗██╗      ██████╗     ██████╗   #
# ████╗ ████║██╔═══██╗██╔══██╗██║   ██║██║     ██╔═══██╗    ╚════██╗  #
# ██╔████╔██║██║   ██║██║  ██║██║   ██║██║     ██║   ██║     █████╔╝  #
# ██║╚██╔╝██║██║   ██║██║  ██║██║   ██║██║     ██║   ██║    ██╔═══╝   #
# ██║ ╚═╝ ██║╚██████╔╝██████╔╝╚██████╔╝███████╗╚██████╔╝    ███████╗  #
# ╚═╝     ╚═╝ ╚═════╝ ╚═════╝  ╚═════╝ ╚══════╝ ╚═════╝     ╚══════╝  #
# --------------------------------------------------------------------# 
#                       MODELADO LOGÍSTICO                            #
# --------------------------------------------------------------------# 

# --------------------------------------------------------------------------- #
#                               Primer modelo ejecutado                       #
# --------------------------------------------------------------------------- #
#                                 📝  NOTAS:                                  #
# 1. Se ajusta el modelo para las 12 imputaciones con 22 covaraibles
# 2. Se exluye año
# --------------------------------------------------------------------------- #


Modelo_logit.1.Imput <- with(data = Base_F_2_Impu.2,expr = glm(Adop_Pronósticos ~ Región+Edad_P+Genero_P+Escolaridad_P_Cat+F2+HH_size+Educación_Hogar+F7_M+F11+F12+F16+F17_log+Irrigation_system+L1+K1+              
+org_member+received_training+J4+E1+E2+índice_Activos+Cultivo+Año,family = binomial(link = "logit")))

resumen <- summary(pool(Modelo_logit.1.Imput));resumen_df <- as.data.frame(resumen)

# Crear tabla LaTeX
tabla_latex <- xtable(resumen_df %>% select(-c(df,statistic)),caption = "Resultados modelo Logit con imputación múltiple",label = "tab:logit_pool",include.rownames = FALSE)



# ------------------------------------------------- #
#     Modelo final considerado: SIN AÑO             #
# ------------------------------------------------- #

# -- Ajuste
Modelo_logit.1.Imput.final <- with(data = Base_F_2_Impu.2,
expr = glm(Adop_Pronósticos ~ Región+Edad_P+Genero_P+Escolaridad_P_Cat+F2+HH_size+Educación_Hogar+F7_M+F11+F12+F16+F17_log+Irrigation_system+L1+K1+              
+org_member+received_training+J4+E1+E2+índice_Activos+Cultivo,family = binomial(link = "logit")))

D3(Modelo_logit.1.Imput, Modelo_logit.1.Imput.final)

# Df de mapeo de nombres:
term <- c("(Intercept)","RegiónLlanos","RegiónAndina","Edad_P","Genero_PMujer","Escolaridad_P_CatSecundaria","Escolaridad_P_CatTerciaría","F2","HH_size","Educación_Hogar","F7_MSí","F11Sí","F12Sí","F16","F17_log","Irrigation_systemSí","L1Sí","K1Sí","org_memberSí","received_trainingSí","J4Sí","E1Sí","E2Sí","índice_Activos","CultivoMaíz")
Variable <- c("(Intercept)","Región: Llanos","Región: Andina","Edad del productor","Género: Mujer","Escolaridad del P: Secundaria","Escolaridad del P: Terciaría","Experiencia del productor","Tamaño del hogar","Años promedio de educación del hogar","Siembra mecanizada: Sí","Semilla certificada: Sí","Pérdida de cosecha: Sí","Rendimiento","$Log(Venta+1)$","Riego: Sí","Crédito: Sí","Otros ingresos : Sí","Miembro de alguna organización: Sí","Recibió capacitaciones: Sí","Asistió a (MTA): Sí","Afectaciones: Sí","Estrategias:Sí","Índice de activos","Cultivo: Maíz")
Df_labels <- data.frame(term  ,Variable)

# -- Tabla de resultados del modelo final:

# Resumen con FMI
pool_final <- pool(Modelo_logit.1.Imput.final)
resumen_pool <- summary(pool(Modelo_logit.1.Imput.final), conf.int = TRUE) %>% left_join(pool_final$pooled %>% select(c(term,fmi)))

# Construir tabla final con redondeo
tabla_final <- resumen_pool %>%mutate(Beta = round(estimate, 2),SE  = round(std.error, 2),CI_inf = round(conf.low, 2),CI_sup = round(conf.high, 2),
p.value= round(p.value, 2),OR = round(exp(estimate), 2),FMI = round(fmi, 2))%>% left_join(Df_labels) %>%select(Variable,Beta,SE,CI_inf,CI_sup,p.value,OR,FMI)

tabla_final_latex <- xtable(tabla_final);print(tabla_final_latex, include.rownames = FALSE)

# ---------------------------------------- #
#   Diagnóstico y desempeño del modelo     #
# ---------------------------------------- #

# ------------
# M. Desempeño
# ------------

Base.imput.2.long<- complete(Base_F_2_Impu.2,action = "long",include = FALSE)

# Indicadores de validación pooled
Result.pool <- pool_performance(data = Base.imput.2.long,formula = Adop_Pronósticos ~ Región + Edad_P + Genero_P +Escolaridad_P_Cat + F2 + HH_size + Educación_Hogar +
F7_M + F11 + F12 + F16 + F17_log + Irrigation_system +L1 + K1 + org_member + received_training + J4 +E1 + E2 + índice_Activos + Cultivo,
nimp = 12,impvar    = ".imp",model_type = "binomial",cal.plot  = TRUE,plot.method = "mean",groups_cal = 10)


# ------------
# M. Linealidad
# ------------


# Fórmula del modelo
form_modelo <- Adop_Pronósticos ~ Región + Edad_P + Genero_P +Escolaridad_P_Cat + F2 + HH_size + Educación_Hogar +
F7_M + F11 + F12 + F16 + F17_log +Irrigation_system + L1 + K1 +org_member + received_training +J4 + E1 + E2 + índice_Activos + Cultivo

# Bases de residuos parciales para cada categoria
df_Edad <- get_partial_resid_MI(mids_obj = Base_F_2_Impu.2,formula_model = form_modelo,varname = "Edad_P")
df_F2 <- get_partial_resid_MI(mids_obj = Base_F_2_Impu.2,formula_model = form_modelo,varname = "F2")
df_HH_size <- get_partial_resid_MI(mids_obj = Base_F_2_Impu.2,formula_model = form_modelo,varname = "HH_size")
df_Educación_Hogar <- get_partial_resid_MI(mids_obj = Base_F_2_Impu.2,formula_model = form_modelo,varname = "Educación_Hogar")
df_F16 <- get_partial_resid_MI(mids_obj = Base_F_2_Impu.2,formula_model = form_modelo,varname = "F16")
df_F17_log<- get_partial_resid_MI(mids_obj = Base_F_2_Impu.2,formula_model = form_modelo,varname = "F17_log")
df_índice_Activos<- get_partial_resid_MI(mids_obj = Base_F_2_Impu.2,formula_model = form_modelo,varname = "índice_Activos")


green_teal_12 <- c("#1F2E2B",  "#263934","#2E4440","#344B47", "#3D5852","#47655E",
                   "#51726A","#5C8077","#688E84","#769D92","#87ADA2","#9ABDB3")

shape_values <- c(0,1,2,3,4,5,6,7,8,9,10,11)

# Lista de dataframes
lista_dfs <- list("Edad del productor" = df_Edad,"Experiencia del prodcutor" = df_F2,"Tamaño del hogar" = df_HH_size,
"Educación del hogar" = df_Educación_Hogar,"Rendimiento" = df_F16,"Log(Venta)" = df_F17_log,"Índice de activos" = df_índice_Activos)

# Crear todos los gráficos
plots <- mapply(plot_partial,df = lista_dfs,x_label = names(lista_dfs),SIMPLIFY = FALSE)

# Unir en figura final con leyenda común
figura_final <- wrap_plots(plots, ncol = 3) +plot_layout(guides = "collect") &theme(legend.position = "bottom")


# ------------
# M. VIF
# ------------

# Extraer lista de modelos individuales
modelos_lista <- Modelo_logit.1.Imput.final$analyses

# Calcular VIF por cada imputación
tabla_vif <- map2_dfr(modelos_lista,seq_along(modelos_lista),
~{vif_vals <- vif(.x)
if (is.matrix(vif_vals)) {vif_vals <- vif_vals[, "GVIF^(1/(2*Df))"]}

data.frame(Imputacion = .y,Variable = names(vif_vals),VIF = round(as.numeric(vif_vals),2))})

tabla_vif_ancha <- tabla_vif %>%pivot_wider(names_from = Imputacion, values_from = VIF)

tabla_vif_ancha_F <- tabla_vif_ancha %>%left_join(Df_labels, by = c("Variable" = "term")) %>%select(-Variable)
tabla_vif_ancha_F <- tabla_vif_ancha_F[, c("Variable.y", setdiff(names(tabla_vif_ancha_F), "Variable.y"))]

kable(tabla_vif_ancha_F, format = "latex", booktabs = TRUE,caption = "Variance Inflation Factors by Imputation") %>%
kable_styling(latex_options = c("hold_position"))


# ------------
# P. INFLUYENTES
# ------------

# Se crea un df  con las medidas hat y cook a cada modelo
df_influencia <- map2_dfr(modelos_lista,seq_along(modelos_lista), ~{data.frame(obs = 1:length(hatvalues(.x)),
hat = hatvalues(.x),cook = cooks.distance(.x),imputacion = factor(.y))})


# Se crea una medida de los valores hat y cook estandarizados que proprcione los puntos de mayor lejania conjunta
df_influencia <- df_influencia %>% group_by(imputacion) %>% mutate(hat_z = scale(hat)[,1],cook_z = 
scale(cook)[,1],dist_infl = sqrt(hat_z^2 + cook_z^2)) %>%ungroup()
top_influ <- df_influencia %>%group_by(imputacion) %>%slice_max(dist_infl, n = 7) %>%ungroup()


# Gráfico final para todas las imputaciones:
ggplot(df_influencia, aes(hat, cook)) +geom_point(alpha = 1,color="#263749") +geom_point(data = top_influ, size = 2.5, color = "#5C8077") +
geom_text(data = top_influ, aes(label = obs), vjust = -0.7, color = "black") +facet_wrap(~imputacion, ncol = 3) +
labs(x = "Leverage (Valores Hat)",y = "Distancia de Cook") +coord_cartesian(ylim = c(0, max(df_influencia$cook) * 1.1)) +
theme(axis.title = element_text(family = "Times"),axis.text  = element_text(family = "Times"),strip.background = element_rect(fill = "#939EA6", color = "black"),
strip.text = element_text(family = "Times",size = 14,face = "bold",margin = margin(t = 1, b = 1)  ),axis.title.y = element_text(
family = "Times",margin = margin(r = 12)),panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# -- Modelo sin puntos influyentes -- #

# Se crean las posiciones a eliminar
ids_remove <- c(38,1214,11,1465,274,242,668)

# Se deja la base imputada original en forma long
Base_long <- complete(Base_F_2_Impu.2, action = "long", include = TRUE)

# Se remueven los puntos y se convierte en un objeto MICE
Base_long_filtrada <- Base_long %>%filter(!.id %in% ids_remove)
Base_imput_filtrada <- as.mids(Base_long_filtrada)

# Se ajusat el modelo
Modelo_logit_sin_infl <- with(data = Base_imput_filtrada,expr = glm(
Adop_Pronósticos ~ Región + Edad_P + Genero_P + Escolaridad_P_Cat + F2 +HH_size + Educación_Hogar + F7_M + F11 + F12 + F16 + F17_log +
Irrigation_system + L1 + K1 + org_member + received_training +J4 + E1 + E2 + índice_Activos + Cultivo,family = binomial(link = "logit")))

# -- Se crea una tabla que compara ambos modelos: --#

# Se crean los resumenes de ambos modelos
resumen_full <- summary(pool(Modelo_logit.1.Imput.final)) %>%select(term, estimate, std.error, p.value) %>% dplyr::rename(Beta_full = estimate,SE_full= std.error,p_full = p.value)
resumen_sin <- summary(pool(Modelo_logit_sin_infl)) %>%select(term, estimate, std.error, p.value) %>% dplyr::rename(Beta_sin = estimate,SE_sin   = std.error,p_sin    = p.value)

# Se unen y se pasan sus valores a los labes para tabla
tabla_comp <- resumen_full %>%left_join(resumen_sin, by = "term")
tabla_comp_final <- tabla_comp %>%left_join(Df_labels, by = "term") %>%select(Variable,Beta_full, SE_full, p_full,Beta_sin,  SE_sin,  p_sin)

# Se redondean
tabla_comp_final <- tabla_comp_final %>%mutate(across(where(is.numeric), ~round(.x, 3)))

# Se genera en formato latex:
tabla_latex_comp <- xtable(tabla_comp_final,
caption = "Comparación de estimaciones entre el modelo completo y el modelo sin observaciones influyentes")


# ---------------------------------------- #
#           Sobreajuste del modelo         #
# ---------------------------------------- #

# Se procesan las bases imputadas y original, para que las categorícas sean numericas y las dicotomicas 0 y 1
Base.imput.2.long.process <- Procesar.cv.imput(Base.imput.2.long)
Base_F_2_Impu.1.process <- Procesar.cv.imput(Base_F_2_Impu.1_CAT_2)

# Se define el modelo psfmi_lr con datos imputados antes de la cv
Model.pool.cv <- psfmi_lr(data = Base.imput.2.long.process,formula = Adop_Pronósticos ~ factor(Región) + Edad_P + Genero_P +factor(Escolaridad_P_Cat) + F2 + HH_size + Educación_Hogar +
F7_M + F11 + F12 + F16 + F17_log + Irrigation_system +L1 + K1 + org_member+ received_training + J4 +E1 + E2 + índice_Activos + Cultivo ,nimp = 12,impvar  = ".imp",p.crit  = 1,method="D1")

# Se ejecuta la validacion cruzada en combinacion con MICE
Pool.cv<- psfmi_perform(Model.pool.cv, val_method = "cv_MI", data_orig = Base_F_2_Impu.1.process, folds=10,nimp_cv = 12, p.crit=1,cal.plot=TRUE,miceImp=miceImp,printFlag = FALSE)

# Pasando tabla de CV a formato Latex:
tabla_latex_Pool.cv <- xtable(Pool.cv$pool_stats,caption = "Desempeño del modelo con CV")


# -------------------------------------------------- #
# Evaluación de las predicciones y la calsificación  #
# ---------------------------------------------------#

# Se calculan las predicciones para cala modelo de conjunto imputado
pred_list <- lapply(1:Base_F_2_Impu.2$m, function(d) {datos_d <- complete(Base_F_2_Impu.2, d);modelo_d <- glm(Adop_Pronósticos ~ Región + Edad_P + Genero_P +
Escolaridad_P_Cat + F2 + HH_size +Educación_Hogar + F7_M + F11 + F12 +F16 + F17_log + Irrigation_system +
L1 + K1 + org_member + received_training +J4 + E1 + E2 + índice_Activos + Cultivo,data = datos_d,family = binomial)
predict(modelo_d, type = "response")})

# Se calculan las predicciones promedio
p_prom <- Reduce("+", pred_list) / length(pred_list)

# ------------
# Bier score
# ------------

y_real <- complete(Base_F_2_Impu.2, 1)$Adop_Pronósticos
y <- ifelse(y_real == "Sí", 1, 0)
brier <- mean((y - p_prom)^2)

# ------------
# Calibration plot
# ------------


cal_data <- data.frame(y = y,p = p_prom)

# Agrupar en deciles de riesgo
cal_plot <- cal_data %>%mutate(decile = ntile(p, 10)) %>%group_by(decile) %>%summarise(pred_mean = mean(p),obs_mean = mean(y))

ggplot(cal_plot, aes(x = pred_mean, y = obs_mean)) +geom_point(size = 3, color = "#3d4756") +geom_line(color = "#3d4756") +
geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#0072FF") +labs(x = "Probabilidad predicha final",
y = "Frecuencia observada") +theme_mine() +theme(text = element_text(family = "Times"))

# ------------
# Clasificación
# ------------

y_real <- complete(Base_F_2_Impu.2, 1)$Adop_Pronósticos
roc_pooled <- roc(y_real, p_prom)

youden <- pROC::coords(roc_pooled,x = "best",best.method = "youden",ret = c("threshold", "sensitivity", "specificity"))

# --------------------------------------------------------------------# 
# ███╗   ███╗ ██████╗ ██████╗ ██╗   ██╗██╗      ██████╗     ██████╗   #
# ████╗ ████║██╔═══██╗██╔══██╗██║   ██║██║     ██╔═══██╗    ╚════██╗  #
# ██╔████╔██║██║   ██║██║  ██║██║   ██║██║     ██║   ██║     █████╔╝  #
# ██║╚██╔╝██║██║   ██║██║  ██║██║   ██║██║     ██║   ██║     ╚═══██╗  #
# ██║ ╚═╝ ██║╚██████╔╝██████╔╝╚██████╔╝███████╗╚██████╔╝    ██████╔╝  #
# ╚═╝     ╚═╝ ╚═════╝ ╚═════╝  ╚═════╝ ╚══════╝ ╚═════╝     ╚═════╝   #
# --------------------------------------------------------------------# 
#                       MODELO RANDOM FOREST                          #
# --------------------------------------------------------------------# 

# ====================================================================#
#   MODELO RANDOM FOREST: PRIMERA PARTE, VALIDACION CRUZADA ANIDADA   #
# ====================================================================#
#                     Código ejecutado en Servidor CIAT               #
#                         Tiempo aproximado: 25 Hr                    #
# --------------------------------------------------------------------#


#-- Se establece la semilla para lo folds  y se crean los 10 folds o particiones--#
set.seed(123);folds_outer <- createFolds(Base_F_2_Impu.2$data$Adop_Pronósticos, k = 10)

# Fórmula final del modelo
Fórmula_Model <- Adop_Pronósticos ~ Región + Edad_P + Genero_P + Escolaridad_P_Cat +F2 + HH_size + Educación_Hogar + F7_M + F11 + F12 + F16 + F17_log +
Irrigation_system + L1 + K1 + org_member + received_training + J4 + E1 + E2 + índice_Activos + Cultivo

# Listas para guardar resultados
resultados_lista <- list();modelos_lista<-list();probs_lista<-list()

# Espacio de hiperparámetros
espacio <- list(num.trees = c(100, 200, 300, 500, 750, 1000),mtry= c(2, 4, 6, 9, 12, 15, 18, 20),
min.node.size = c(1, 5, 10, 20, 50, 75, 100, 150),max.depth = c(5, 10, 15, 20, 30, 40, 50, 70, 80, 0))


# - - - - - - - - - - - - #
# CICLO EXTERNO DE LA NCV #
# - - - - - - - - - - - - #

for (k in seq_along(folds_outer)) {
  
  cat("\n=========== FOLD EXTERNO:", k, "===========\n")
  
  test_idx <- folds_outer[[k]]
  
  # se guarda la Y real de test ANTES de ignorarla-- #
  y_test_real <- data_raw$Adop_Pronósticos[test_idx]
  
  # se deja para el df de test Y como si fuera NA -- #
  data_fold <- data_raw
  data_fold$Adop_Pronósticos[test_idx] <- NA
  
  # Se exclenyen los datos de validacion en la imputacion de prueba
  ignore_vec <- rep(FALSE, nrow(data_fold))
  ignore_vec[test_idx] <- TRUE
  
  imp_k <- mice(data = data_fold, m = Base_F_2_Impu.2$m, ignore = ignore_vec,
                method = method_base, predictorMatrix = Base_F_2_Impu.2$predictorMatrix,
                maxit = 10, seed = 1, printFlag = FALSE)
  
  for (i in 1:imp_k$m) {
    
    cat("  Imputación:", i, "\n")
    
    data_i <- complete(imp_k, i)
    
    # --  restaurar la Y real de test, descartando la que mice haya inventado -- #
    
    data_i$Adop_Pronósticos[test_idx] <- y_test_real
    data_i$Adop_Pronósticos <- factor(data_i$Adop_Pronósticos, levels = c("No", "Sí"))
    
    train_outer <- data_i[-test_idx, ]
    test_outer  <- data_i[test_idx,  ]
    
    y_train_num <- as.numeric(train_outer$Adop_Pronósticos == "Sí")
    y_test_num  <- as.numeric(test_outer$Adop_Pronósticos  == "Sí")
    
    # brier SCORE DE REFERENCIA (Para el BSS)
    brier_null_train <- mean(y_train_num) * (1 - mean(y_train_num))
    brier_null_test  <- mean(y_test_num)  * (1 - mean(y_test_num))
    
    # Folds separados para grid search y para OOF de calibración
    set.seed(10 + k)
    inner_folds_tuning <- createFolds(train_outer$Adop_Pronósticos, k = 10)
    inner_folds_cal    <- createFolds(train_outer$Adop_Pronósticos, k = 10)
    
    # Generar grid aleatorio de tamaño 120
    set.seed(42 + k * 100 + i)
    grid <- data.frame(
      num.trees     = sample(espacio$num.trees,     120, replace = TRUE),
      mtry          = sample(espacio$mtry,          120, replace = TRUE),
      min.node.size = sample(espacio$min.node.size, 120, replace = TRUE),
      max.depth     = sample(espacio$max.depth,     120, replace = TRUE)
    ) %>% distinct()
    
    grid$AUC_val_inner   <- NA
    grid$AUC_train_inner <- NA
    grid$BSS_val_inner   <- NA
    grid$Brier_val_inner <- NA
    grid$THR_inner       <- NA
    
    # - - - - - - - - - - - - #
    # CICLO INTERNO DE LA NCV #
    # - - - - - - - - - - - - #
    for(g in 1:nrow(grid)){
      
      # Vectores vacios para los resultados del ciclo interno
      aucs_val  <- c();aucs_train <- c();bss_vals <- c();briers_val <- c();thresholds <- c()

      for(f in inner_folds_tuning){
        
        # generar conjuntos internos
        val_idx         <- f
        train_idx_inner <- setdiff(1:nrow(train_outer), val_idx)
        train_inner     <- train_outer[train_idx_inner, ]
        val_inner       <- train_outer[val_idx, ]
        
        y_val_num_in <- as.numeric(val_inner$Adop_Pronósticos   == "Sí")
        y_tr_num_in  <- as.numeric(train_inner$Adop_Pronósticos == "Sí")
        
        # BSS interno
        brier_null_in <- mean(y_tr_num_in) * (1 - mean(y_tr_num_in))
        
        # Ejecucción del entrenamiento del modelo
        rf_tmp <- ranger(formula= Fórmula_Model,data= train_inner,num.trees= grid$num.trees[g],
        mtry= grid$mtry[g],min.node.size = grid$min.node.size[g],max.depth= grid$max.depth[g],probability= TRUE)
        
        # Predicciones internas
        p_val   <- predict(rf_tmp, val_inner)$predictions[,   "Sí"]
        p_tr_in <- predict(rf_tmp, train_inner)$predictions[, "Sí"]
        
        # AUC interno
        roc_val    <- roc(val_inner$Adop_Pronósticos, p_val, quiet = TRUE)
        aucs_val   <- c(aucs_val,   as.numeric(auc(roc_val)))
        aucs_train <- c(aucs_train, as.numeric(auc(train_inner$Adop_Pronósticos,p_tr_in, quiet = TRUE)))
        
        # calibración interna
        cal_in    <- calibrar_isotonica(p_tr_in, y_tr_num_in, p_val)
        p_val_cal <- cal_in$p_cal
        
        # Bier
        brier_cal_in <- mean((p_val_cal - y_val_num_in)^2)
        bss_in       <- 1 - (brier_cal_in / brier_null_in)
        
        bss_vals   <- c(bss_vals,   bss_in)
        briers_val <- c(briers_val, brier_cal_in)
        
        # umbral de Youden sobre predicciones calibradas
        roc_cal    <- roc(val_inner$Adop_Pronósticos, p_val_cal, quiet = TRUE)
        coords_all <- pROC::coords(roc_cal, x = "all",ret = c("threshold","sensitivity","specificity"),transpose = FALSE)
        youden     <- coords_all$sensitivity + coords_all$specificity - 1
        thresholds <- c(thresholds, coords_all$threshold[which.max(youden)])
      }
      
      grid$AUC_val_inner[g]   <- mean(aucs_val,   na.rm = TRUE)
      grid$AUC_train_inner[g] <- mean(aucs_train, na.rm = TRUE)
      grid$BSS_val_inner[g]   <- mean(bss_vals,   na.rm = TRUE)
      grid$Brier_val_inner[g] <- mean(briers_val, na.rm = TRUE)
      grid$THR_inner[g]       <- mean(thresholds, na.rm = TRUE)
    }
    
    # Selecionar los mejores hiperparámetros para ese fold según AUC
    best <- grid[which.max(grid$AUC_val_inner), ]
    
    # - - - - - - - - - - - - - - - - - - - - - #
    # AJUSTE MODELO EXTERNO O FINAL POR FOLD    # 
    # - - - - - - - - - - - - - - - - - - - - - #
    
    rf_final <- ranger(formula = Fórmula_Model,data= train_outer,num.trees = best$num.trees,mtry = best$mtry,min.node.size = best$min.node.size,
    max.depth = best$max.depth,probability = TRUE,importance = "permutation")
    
    # - - - - - - - - - - - - - - - - - - - - - #
    # CALIBRACIÓN MODELO FINAL (FOLD EXTERNO)   # 
    # - - - - - - - - - - - - - - - - - - - - - #
    
    cat("  Generando OOF para calibración isotónica...\n")
    
    p_oof_train <- rep(NA, nrow(train_outer))
    
    for(f in inner_folds_cal) {
      val_idx_cal   <- f
      train_idx_cal <- setdiff(1:nrow(train_outer), val_idx_cal)
      
      rf_cal_tmp <- ranger(formula = Fórmula_Model,
      data= train_outer[train_idx_cal, ],num.trees = best$num.trees,mtry= best$mtry,
      min.node.size = best$min.node.size,max.depth= best$max.depth,probability   = TRUE)
      
      p_oof_train[val_idx_cal] <- predict(rf_cal_tmp,train_outer[val_idx_cal, ])$predictions[, "Sí"]
    }
    
    # - - - - - - - - - - - - - - - - - - - - -- - - - - - - #
    # Generar métricas finales de entrenamiento y validación # 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    # prediciones finales o externas
    p_test  <- predict(rf_final, test_outer)$predictions[,  "Sí"]
    p_train <- predict(rf_final, train_outer)$predictions[, "Sí"]
    
    # Métricas sin calibrar
    auc_test        <- as.numeric(auc(test_outer$Adop_Pronósticos,  p_test))
    auc_train_outer <- as.numeric(auc(train_outer$Adop_Pronósticos, p_train))
    brier_test_raw  <- mean((p_test  - y_test_num)^2)
    brier_train_raw <- mean((p_train - y_train_num)^2)
    bss_test_raw    <- 1 - (brier_test_raw / brier_null_test)
    
    # calibración final o externa
    cal_result <- calibrar_isotonica(p_oof_train, y_train_num, p_test)
    p_test_cal <- cal_result$p_cal
    
    # Métricas calibradas
    auc_test_cal   <- as.numeric(auc(test_outer$Adop_Pronósticos, p_test_cal))
    brier_test_cal <- mean((p_test_cal - y_test_num)^2)
    bss_test_cal   <- 1 - (brier_test_cal / brier_null_test)
    
    # - - - - - - - - - - - - - - - - - - - - -- - - - - - - #
    # Clasificación: se genera la sencibilidad y especificidad
    # con prediciones calbiradas y sin calibrar              # 
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - -#
    
    # P calibradas
    p_oof_cal <- calibrar_isotonica(p_oof_train, y_train_num, p_oof_train)$p_cal
    
    # Generar mejor umbral por youden
    roc_oof    <- roc(train_outer$Adop_Pronósticos, p_oof_cal, quiet = TRUE)
    coords_oof <- pROC::coords(roc_oof, x = "all",ret = c("threshold","sensitivity","specificity"),transpose = FALSE)
    youden_oof <- coords_oof$sensitivity + coords_oof$specificity - 1
    thr_final  <- coords_oof$threshold[which.max(youden_oof)]
    
    # Clasificación sin y con calibración
    pred_raw <- factor(ifelse(p_test     >= best$THR_inner, "Sí", "No"), levels = c("No","Sí"))
    pred_cal <- factor(ifelse(p_test_cal >= thr_final,      "Sí", "No"), levels = c("No","Sí"))
    
    cm_raw <- confusionMatrix(pred_raw, test_outer$Adop_Pronósticos, positive = "Sí")
    cm_cal <- confusionMatrix(pred_cal, test_outer$Adop_Pronósticos, positive = "Sí")
    
    # - - - - - - - - - - - - - - - - - -- -#
    # Guardando resultados finales por FOLD # 
    # - - - - - - - - - - - - - - - - - -- -#
    
    
    # -- GUARDANDO MÉTRICAS -- #
    resultados_lista[[length(resultados_lista) + 1]] <- data.frame(Fold= k,Imputacion = i,AUC_Train_Inner= best$AUC_train_inner,
    AUC_Val_Inner= best$AUC_val_inner,AUC_Train_Outer= auc_train_outer,AUC_Test_Outer= auc_test,AUC_Test_Cal= auc_test_cal,
    BSS_Val_Inner= best$BSS_val_inner,BSS_Test_Raw= bss_test_raw,BSS_Test_Cal= bss_test_cal,Brier_Val_Inner= best$Brier_val_inner,
    Brier_Test_Raw= brier_test_raw,Brier_Test_Cal= brier_test_cal,Sens_Raw  = cm_raw$byClass["Sensitivity"],Spec_Raw  = cm_raw$byClass["Specificity"],
    Sens_Cal= cm_cal$byClass["Sensitivity"],Spec_Cal  = cm_cal$byClass["Specificity"],Threshold_Raw = best$THR_inner,Threshold_Cal = thr_final,
    num.trees = best$num.trees,mtry = best$mtry,nodesize = best$min.node.size,max.depth = best$max.depth)
    
    # -- GUARDANDO PROBABILIDADES CALIBRADAS FINALES -- #
    
    probs_lista[[length(probs_lista) + 1]] <- data.frame(Fold= k,
    Imputacion = i,y_real= y_test_num, p_raw= p_test,p_cal = p_test_cal)
    
    # -- GUARDANDO MODELOS FINALES -- #
    
    clave <- paste0("fold", k, "_imp", i)
    modelos_lista[[clave]] <- list(modelo  = rf_final,iso_fit = cal_result$iso_fit,p_oof_train = p_oof_train,
    train_data = train_outer,test_data = test_outer,threshold_raw = best$THR_inner,threshold_cal = thr_final,
    fold = k,imputacion = i)}}

# -- Generar resultados en finales y guardar
resultados_df <- bind_rows(resultados_lista);probs_df <- bind_rows(probs_lista)
saveRDS(resultados_lista, "Codigos y salidas/Resultados Modelos ML/RF_resultados_lista.rds")
saveRDS(modelos_lista,    "Codigos y salidas/Resultados Modelos ML/RF_modelos_lista.rds")
saveRDS(resultados_df,    "Codigos y salidas/Resultados Modelos ML/RF_resultados_df.rds")
saveRDS(probs_df,         "Codigos y salidas/Resultados Modelos ML/RF_probs_calibracion.rds")

# ====================================================================#
#       MODELO RANDOM FOREST: SEGUNDA PARTE, AJUSTE FINAL             #
# ====================================================================#
#                     Código ejecutado en Servidor CIAT               #
#                         Tiempo aproximado: NA                       #
# --------------------------------------------------------------------#

# Vector explicito de varaibles
vars_pred <- c("Región","Edad_P","Genero_P","Escolaridad_P_Cat","F2","HH_size","Educación_Hogar","F7_M","F11","F12","F16","F17_log",
"Irrigation_system","L1","K1","org_member","received_training","J4","E1","E2","índice_Activos","Cultivo")

# numero de imputacioens oroginales
M <- Base_F_2_Impu.2$m
y_all <- ifelse(Base_F_2_Impu.2$data$Adop_Pronósticos == "Sí", 1, 0)

# Grilla del modelo final
set.seed(42)
grid_final <- data.frame(num.trees= sample(espacio$num.trees,120, replace = TRUE),mtry = sample(espacio$mtry, 120, replace = TRUE),
min.node.size = sample(espacio$min.node.size, 120, replace = TRUE),max.depth = sample(espacio$max.depth,120, replace = TRUE)) %>% distinct()

cat("Configuraciones en grilla final:", nrow(grid_final), "\n")

# - - - - - - - -  #
# CV AJUSTE FINAL  # 
# - - - - - - - -  #

set.seed(10)
folds_tuning_final <- createFolds(y_all, k = 10)

grid_final$AUC_val<- NA;grid_final$AUC_train <- NA;grid_final$Brier_val <- NA

# --- INICIO DEL CILO POR FOLD E IMPUTACIÓN --- #
for (g in 1:nrow(grid_final)) {

  cat("Grilla", g, "/", nrow(grid_final), "\n")
  aucs_val <- c();aucs_train <- c();briers_val <- c()
  
  for (f in folds_tuning_final) {
    val_idx   <- f;train_idx <- setdiff(seq_along(y_all), val_idx)
    
    # -- Promediar AUC sobre las M imputaciones para  fold i
    aucs_val_m   <- c();aucs_train_m <- c();briers_val_m <- c()
    
    for (m in seq_len(M)) {
      datos_m <- complete(Base_F_2_Impu.2, m)
      datos_m$Adop_Pronósticos <- factor(datos_m$Adop_Pronósticos,levels = c("No", "Sí"))
      
      rf_g <- ranger(formula= Fórmula_Model,
      data= datos_m[train_idx, ],num.trees= grid_final$num.trees[g],
      mtry= grid_final$mtry[g],min.node.size = grid_final$min.node.size[g],
      max.depth= grid_final$max.depth[g],probability= TRUE,seed= 42 + g + m)
      
      p_val_m   <- predict(rf_g, data = datos_m[val_idx,   ])$predictions[, "Sí"]
      p_train_m <- predict(rf_g, data = datos_m[train_idx, ])$predictions[, "Sí"]
      
      # Calibración isotónica dentro del fold 
      ord_in    <- order(p_train_m)
      iso_in    <- gpava(p_train_m[ord_in], y_all[train_idx][ord_in])
      p_val_cal <- approx(x = p_train_m[ord_in], y = iso_in$x,xout = p_val_m, method = "linear", rule = 2)$y
      p_val_cal <- pmax(0, pmin(1, p_val_cal))
      
      aucs_val_m   <- c(aucs_val_m,   as.numeric(auc(y_all[val_idx],   p_val_m,   quiet = TRUE)))
      aucs_train_m <- c(aucs_train_m, as.numeric(auc(y_all[train_idx], p_train_m, quiet = TRUE)))
      briers_val_m <- c(briers_val_m, mean((p_val_cal - y_all[val_idx])^2))
    }
    aucs_val <- c(aucs_val,mean(aucs_val_m));aucs_train <- c(aucs_train, mean(aucs_train_m));briers_val <- c(briers_val, mean(briers_val_m))
  }
  grid_final$AUC_val[g] <- mean(aucs_val,   na.rm = TRUE);grid_final$AUC_train[g] <- mean(aucs_train, na.rm = TRUE);grid_final$Brier_val[g] <- mean(briers_val, na.rm = TRUE)
}

# Mejor grilla para el modelo final según CV simple
HP_final <- grid_final %>%arrange(desc(AUC_val)) %>%slice(1)

# - - - - - - - - - - - - - - #
#   ENTRENO DEL AJUSTE FINAL  # 
# - - - - - - - - - - - - - - #

modelos_finales <- vector("list", M)

for (m in seq_len(M)) {
  
  cat("Entrenando modelo final RF — imputación", m, "de", M, "\n")
  
  datos_m <- complete(Base_F_2_Impu.2, m);datos_m$Adop_Pronósticos <- factor(datos_m$Adop_Pronósticos, levels = c("No","Sí"))
  
  set.seed(2024 + m)
  modelos_finales[[m]] <- ranger(formula= Fórmula_Model,data = datos_m,num.trees = HP_final$num.trees,mtry= HP_final$mtry,
  min.node.size = HP_final$min.node.size, max.depth = HP_final$max.depth,probability= TRUE,importance= "permutation")
}

saveRDS(modelos_finales, "Codigos y salidas/Resultados Modelos ML/RF_modelos_finales_M.rds")

# - - - - - - - - - - - - - - #
# CALIBRACIÓN DEL MODELO FINAL# 
# - - - - - - - - - - - - - - #

calibradores_rf <- vector("list", M)
p_oof_list      <- vector("list", M)

set.seed(20)
folds_cal_final <- createFolds(y_all, k = 10)  

for (m in seq_len(M)) {
  
  cat("Calibración isotónica — imputación", m, "de", M, "\n")
  
  datos_m <- complete(Base_F_2_Impu.2, m);datos_m$Adop_Pronósticos <- factor(datos_m$Adop_Pronósticos, levels = c("No","Sí"))
  p_oof_m <- rep(NA, nrow(datos_m))
  
  for (f in folds_cal_final) {
    
    train_c <- setdiff(seq_len(nrow(datos_m)), f)
    val_c   <- f
    
    set.seed(2024 + m)
    rf_cal_tmp <- ranger(formula = Fórmula_Model,data = datos_m[train_c, ],num.trees  = HP_final$num.trees,
    mtry  = HP_final$mtry,min.node.size = HP_final$min.node.size,max.depth = HP_final$max.depth,probability = TRUE)
    p_oof_m[val_c] <- predict(rf_cal_tmp, data = datos_m[val_c, ])$predictions[, "Sí"]
  }
  
  # Ajuste isotónico POR IMPUTACIÓN
  ord_m  <- order(p_oof_m);iso_m  <- gpava(p_oof_m[ord_m], y_all[ord_m])
  p_oof_ord_m <- p_oof_m[ord_m];iso_x_m <- iso_m$x
  
  calibradores_rf[[m]] <- local({p_ord <- p_oof_ord_m;iso_x <- iso_x_m
  function(p_new) {p_cal <- approx(x = p_ord, y = iso_x, xout = p_new,method = "linear", rule = 2)$y
  pmax(0, pmin(1, p_cal))}});p_oof_list[[m]] <- p_oof_m
}

# - - - - - - - - - - - - - - #
#   CÁLCULO DE VALORES SHAP   # 
# - - - - - - - - - - - - - - #


shap_matrices <- vector("list", M)
pfun <- function(object, newdata) {predict(object, data = newdata)$predictions[, "Sí"]}

# Ciclo para valores por imputación
for (m in seq_len(M)) {
  cat("Calculando SHAP — imputación", m, "de", M, "\n")
  
  datos_m <- complete(Base_F_2_Impu.2, m)
  datos_m$Adop_Pronósticos <- factor(datos_m$Adop_Pronósticos,levels = c("No","Sí"))
  X_m <- datos_m[, vars_pred]
  
  shap_m <- explain(object= modelos_finales[[m]],X = X_m,pred_wrapper = pfun,nsim = 50)
  shap_matrices[[m]] <- as.matrix(shap_m)
}

# Promediar los shap de las M imputaciones
shap_mean_rf <- Reduce("+", shap_matrices) / M

# Se generan los valores SHPA finales| OJO: En los gráficos, los x_j de referencia del eje vertical son los de la primera imputación (es un paroximación)
datos_ref  <- complete(Base_F_2_Impu.2, 1);X_ref  <- datos_ref[, vars_pred]
colnames(shap_mean_rf) <- vars_pred;sv_rf_final <- shapviz(shap_mean_rf, X = X_ref)
saveRDS(sv_rf_final, "Codigos y salidas/Resultados Modelos ML/RF_shapviz.rds")


# ====================================================================#
#   MODELO RANDOM FOREST: TERCERA PARTE, RESULTADOS DEL REPORTE       #
# ====================================================================#

# - - - - - - - - - - - - - - - - - - - - -- - - - - - - #
#         RESULTADOS MODELOS FINALES RF: SHAP       #
# - - - - - - - - - - - - - - - - - - - - -- - - - - - - #

# Se cargan los resultados de NCV
NCV_RF <- readRDS("Códigos y salidas/Resultados Modelos ML/RF_resultados_df.rds")

# Se Cargan los resultados del modelo final producidos por el servidor CIAT:
SHAP_values_RF <- readRDS("Códigos y salidas/Resultados Modelos ML/Salidas modelos finales/RF_shapviz.rds")

# Se cargan los HP del modelo final: 
HP_RF <- readRDS("Códigos y salidas/Resultados Modelos ML/Salidas modelos finales/RF_HP_final.rds")


# ----- RESULTADOS MODELOS NCV RF ----- #

# Ordenar segun el diccionario
orden_metricas <- names(labels_metricas)

# -- Tabla de resultados NCV para RF por fold y general

Tabla_NVC_RF <- NCV_RF %>%select(Fold, Imputacion, all_of(vars_metricas)) %>%pivot_longer(cols = all_of(vars_metricas),
names_to = "Metrica",values_to = "Valor") %>%group_by(Fold, Metrica) %>%summarise(Media = mean(Valor, na.rm = TRUE),SD= sd(Valor, na.rm = TRUE),
.groups = "drop")

# Columna General 
Tabla_NVC_RF_General <- Tabla_NVC_RF %>%group_by(Metrica) %>%summarise(Media_Gen = mean(Media, na.rm = TRUE),SD_Gen = sd(Media, na.rm = TRUE), 
.groups = "drop") %>%mutate(General = linebreak(sprintf("%.3f", Media_Gen), align = "c")) %>%  select(Metrica, General)

# Tabla final
Tabla_NVC_RF <- Tabla_NVC_RF %>%mutate(Resultado = linebreak(sprintf("%.3f\n(%.3f)", Media, SD), align = "c"),Metrica = factor(Metrica, levels = names(labels_metricas))
) %>% arrange(Metrica) %>%mutate(Metrica = labels_metricas[as.character(Metrica)]) %>%select(Metrica, Fold, Resultado) %>%pivot_wider(names_from = Fold,values_from = Resultado,
names_prefix = "Fold ") %>%left_join(Tabla_NVC_RF_General %>% mutate(Metrica = labels_metricas[Metrica]),by = "Metrica")

# Tabla en latex
kbl(Tabla_NVC_RF,format = "latex",booktabs = TRUE,align = c("l", rep("c", ncol(Tabla_NVC_RF) - 1)),
caption = "Resultados promedio por fold externo (12 imputaciones por fold) para el modelo Random Forest y su desviación estándar entre paréntesis.",
label = "tab:ncv_rf_folds",escape = FALSE ) %>%kable_styling(latex_options = c("hold_position", "scale_down"))

# - - - - - - - - - - - - - - - - - - - -#
# FIGURA DE CONTRUBICIONES POR PRODUCTOR
# - - - - - - - - - - - - - - - - -- - - #


#- Se genera la figura génerica del paquete shapviz
SHAP_RF_G_BEE <- sv_importance(SHAP_values_RF,kind = "beeswarm",show_numbers = FALSE,max_display = 24)

dic_labels <- c(Región= "Región",Edad_P="Edad del productor",Genero_P="Género",Escolaridad_P_Cat="Escolaridad del P",
F2="Experiencia del productor",HH_size="Tamaño del hogar",Educación_Hogar="Educación promedio hogar",F7_M="Siembra mecanizada",
F11="Semilla certificada",F12="Pérdida de cosecha",F16="Rendimiento",F17_log="Log(Ventas + 1)",Irrigation_system="Riego",
L1="Crédito",K1="Otros ingresos",org_member="Miembro de alguna organización",received_training="Recibió capacitación",
J4="Asistió a (MTA)",E1="Afectaciones",E2="Estrategias",índice_Activos="Índice de activos",Cultivo="Cultivo")

# -- Gráfica de las variables continuas -- #

vars_continuas <- c("Edad_P", "F2", "HH_size", "Educación_Hogar","F16", "F17_log", "índice_Activos")

SHAP_bee_cont <- SHAP_values_RF[, vars_continuas]
SHAP_RF_BEE_CONT <- sv_importance(SHAP_bee_cont,kind= "beeswarm",show_numbers = FALSE,max_display = length(vars_continuas)
) +scale_colour_gradientn(colours = c(col_bajo, col_medio, col_alto),limits = c(0, 1),breaks = c(0, 1),labels = c("Bajo", "Alto"),
name= NULL,guide= guide_colorbar(direction = "vertical",barwidth  = unit(0.4, "cm"),barheight = unit(4, "cm"),ticks = FALSE,
label.theme = element_text(family = "Times", size = 11, colour = "black"))) +scale_y_discrete(labels = function(x) ifelse(x %in% names(dic_labels), dic_labels[x], x)
) +geom_vline(xintercept = 0, colour = "#888888",linewidth = 0.5, linetype = "dashed") +scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
expand = expansion(mult = 0.05)) +labs(x = "Valor SHAP (← reduce adopción | aumenta adopción →)", y = NULL) +theme_mine(base_size = 12, base_family = "Times") +
theme(legend.position    = "right",axis.text.y= element_text(size = 14),axis.text.x  = element_text(size = 14),panel.grid.major.y = element_blank())

# -- Gráfica de las variables categóricas -- #

# Paleta por categóricas
paletas_cat <- list(Región = c("Caribe" = "#C97B6E","Andina" = "#5E86A1","Llanos" = "#6BAE8A"),
Escolaridad_P_Cat = c("Primaria o menos" = "#C97B6E","Secundaria"= "#5E86A1","Terciaría"= "#6BAE8A"),
Genero_P = c("Hombre" = "#C97B6E","Mujer"  = "#5E86A1"),F7_M = c("No" = "#C97B6E", "Sí" = "#5E86A1"),
F11  = c("No" = "#C97B6E", "Sí" = "#5E86A1"),F12  = c("No" = "#C97B6E", "Sí" = "#5E86A1"),
Irrigation_system = c("No" = "#C97B6E", "Sí" = "#5E86A1"),L1 = c("No" = "#C97B6E", "Sí" = "#5E86A1"),
K1= c("No" = "#C97B6E", "Sí" = "#5E86A1"),org_member = c("No" = "#C97B6E", "Sí" = "#5E86A1"),
received_training = c("No" = "#C97B6E", "Sí" = "#5E86A1"),J4   = c("No" = "#C97B6E", "Sí" = "#5E86A1"),
E1= c("No" = "#C97B6E", "Sí" = "#5E86A1"),E2= c("No" = "#C97B6E", "Sí" = "#5E86A1"),Cultivo= c("Maíz" = "#C97B6E", "Arroz" = "#5E86A1"))

# Se extrae el valor SHAP y las categorias del df original
shap_mat <- get_shap_values(SHAP_values_RF)
feat_mat <- get_feature_values(SHAP_values_RF)

# Generar todas las figuras
vars_cat_todas <- names(paletas_cat);plots_cat <- lapply(vars_cat_todas, plot_shap_cat)
names(plots_cat) <- vars_cat_todas

# Unir las n figuras
SHAP_RF_BEE_CAT <- wrap_plots(plots_cat, ncol = 3) +plot_annotation(caption = "Valor SHAP (← reduce adopción | aumenta adopción →)",
theme = theme(plot.caption = element_text(family = "Times", size = 14,hjust = 0.5, margin = margin(t = 0))))


# - - - - - - - - - - - - - - - - - - - - - #
# FIGURA DE SHAP VS COVARAIBLES CONTINUAS
# - - - - - - - - - - - - - - - - - - - - - #

# Variables continuas para los gráficos SHAP vs covaraibles:
VAR_Continuas <- c("Edad_P", "F2", "HH_size", "Educación_Hogar", "F16", "F17_log", "índice_Activos")

# Generar los 7 gráficos
Figuras_Depen_SHAP_RF <- lapply(VAR_Continuas,function(v) {plot_dependence(var  = v,shap = SHAP_values_RF)});names(Figuras_Depen_SHAP_RF) <- VAR_Continuas
Figuras_Depen_SHAP_RF <- wrap_plots(Figuras_Depen_SHAP_RF, ncol = 3) # Unificar las figuras

# --------------------------------------
#     TABLA DE CONTRIBUCIONES |SHAP|
# --------------------------------------

# Extraer matriz SHAP y pasar a df
shap_mat_rf <- get_shap_values(SHAP_values_RF);shap_imp_rf <- data.frame(Variable = colnames(shap_mat_rf),Mean_ABS_SHAP = colMeans(abs(shap_mat_rf)))
shap_imp_rf$Variable <- ifelse(shap_imp_rf$Variable %in% names(dic_labels),dic_labels[shap_imp_rf$Variable],shap_imp_rf$Variable)

# ordenar
shap_imp_rf <- shap_imp_rf %>%arrange(desc(Mean_ABS_SHAP));rownames(shap_imp_rf) <- NULL
shap_imp_rf$Mean_ABS_SHAP <- round(shap_imp_rf$Mean_ABS_SHAP,4)

# Pasar a latex
SHAP_table_latex_RF <- kbl(shap_imp_rf,format = "latex",booktabs = TRUE,align = c("l", "c"),col.names = c("Variable", "$|SHAP|$ medio"),
caption = "Importancia global promedio basada en valores absolutos SHAP para el modelo Random Forest.",label = "tab:shap_rf"
) %>% kable_styling(latex_options = c("hold_position"))


# --------------------------------------------------------------------# 
# ███╗   ███╗ ██████╗ ██████╗ ██╗   ██╗██╗      ██████╗     ██╗  ██╗  #
# ████╗ ████║██╔═══██╗██╔══██╗██║   ██║██║     ██╔═══██╗    ██║  ██║  #
# ██╔████╔██║██║   ██║██║  ██║██║   ██║██║     ██║   ██║    ███████║  #
# ██║╚██╔╝██║██║   ██║██║  ██║██║   ██║██║     ██║   ██║    ╚════██║  #
# ██║ ╚═╝ ██║╚██████╔╝██████╔╝╚██████╔╝███████╗╚██████╔╝         ██║  #
# ╚═╝     ╚═╝ ╚═════╝ ╚═════╝  ╚═════╝ ╚══════╝ ╚═════╝          ╚═╝  #
# --------------------------------------------------------------------# 
#                         MODELO XGBOOT                               #
# --------------------------------------------------------------------# 


# ====================================================================#
#       MODELO XGBOOT: PRIMERA PARTE, VALIDACION CRUZADA ANIDADA      #
# ====================================================================#
#                     Código ejecutado en Servidor CIAT               #
#                         Tiempo aproximado: 15 Hr                    #
# --------------------------------------------------------------------#

# - - - - - - - - - - - - - - - - - - - - - #
#       GENERANDO BASE AL MODELO XGBOOST    #
# - - - - - - - - - - - - - - - - - - - - - #

# Selecionar varaibles y pasar Y a numerica
data_full <- Base_F_2_Impu.2$data %>%dplyr::select(Adop_Pronósticos, Región, Edad_P, Genero_P, Escolaridad_P_Cat,F2, HH_size, Educación_Hogar, F7_M, F11, F12, F16, F17_log,
Irrigation_system, L1, K1, org_member, received_training,J4, E1, E2, índice_Activos, Cultivo)

data_full$Adop_Pronósticos <- ifelse(data_full$Adop_Pronósticos == "Sí", 1, 0)

# Generando indicadoras (dicotómicas) por varaibles categoricas
rec <- recipe(Adop_Pronósticos ~ ., data = data_full) %>%step_unknown(all_nominal_predictors()) %>%step_dummy(all_nominal_predictors())
prep_rec  <- prep(rec, retain = TRUE);data_proc <- bake(prep_rec, new_data = NULL)

# Entradas finales
y_all <- data_proc$Adop_Pronósticos
X_all <- data_proc %>%select(-c(Adop_Pronósticos, Genero_P_Prefiere.no.contestar)) %>%as.matrix()


# Creando CV
folds_outer <- createFolds(y_all, k = 10)

# Crendo grilla
espacio <- list(eta= c(0.001, 0.003, 0.01, 0.03, 0.05, 0.1),max_depth= c(2, 3, 4, 5, 6, 8, 10),subsample= c(0.5, 0.6, 0.7, 0.85, 1.0),
colsample_bytree = c(0.4, 0.6, 0.7, 0.85, 1.0),min_child_weight = c(1, 3, 5, 10, 20, 30),gamma= c(0, 0.5, 1, 2, 5, 10),lambda= c(0.1, 0.5, 1, 2, 5, 10, 20),
alpha= c(0, 0.01, 0.1, 0.5, 1, 5))

# Para temprana
nrounds_max       <- 3000
early_stop_rounds <- 100

# listas vacias de resultados
resultados_lista <- list()
modelos_lista    <- list()
probs_lista      <- list()

for(k in seq_along(folds_outer)){
  
  
  cat("\n=========== FOLD EXTERNO:", k, "===========\n")
  
  # Crear FOLDS
  
  test_idx <- folds_outer[[k]]
  X_train <- X_all[-test_idx, ]
  y_train <- y_all[-test_idx]
  X_test  <- X_all[test_idx, ]
  y_test  <- y_all[test_idx]
  
  set.seed(10 + k)
  inner_folds_tuning <- createFolds(y_train, k = 10)
  inner_folds_cal    <- createFolds(y_train, k = 10)
  
  set.seed(42 + k * 100)
  grid <- data.frame(eta  = sample(espacio$eta,120, replace = TRUE),max_depth = sample(espacio$max_depth, 120, replace = TRUE),
  subsample = sample(espacio$subsample, 120, replace = TRUE),colsample_bytree = sample(espacio$colsample_bytree, 120, replace = TRUE),
  min_child_weight = sample(espacio$min_child_weight, 120, replace = TRUE),gamma = sample(espacio$gamma,120, replace = TRUE),
  lambda = sample(espacio$lambda,120, replace = TRUE), alpha = sample(espacio$alpha,120, replace = TRUE)) %>% distinct()
  
  grid$AUC_val_inner  <- NA;grid$AUC_train_inner  <- NA;grid$THR_inner  <- NA
  grid$nrounds_optimo <- NA;grid$Brier_val_inner  <- NA;grid$Brier_train_inner <- NA
  
  # - - - - - - - - - - - - #
  # CICLO INTERNO DE LA NCV #
  # - - - - - - - - - - - - #
  
  for(g in 1:nrow(grid)){
    
    cat("    Grid:", g, "/", nrow(grid), "\n")
    
    aucs_val <- c();aucs_train <- c();thresholds<- c();nrounds_found <- c()
    briers_val <- c();briers_train <- c()
    
    for(f in inner_folds_tuning){
      
      val_idx_inner   <- f
      train_idx_inner <- setdiff(1:length(y_train), val_idx_inner)
      
      X_tr  <- X_train[train_idx_inner, ];y_tr  <- y_train[train_idx_inner]
      X_val <- X_train[val_idx_inner, ];y_val <- y_train[val_idx_inner]
      
      dtrain_in <- xgb.DMatrix(data = X_tr,  label = y_tr,  missing = NA)
      dval_in   <- xgb.DMatrix(data = X_val, label = y_val, missing = NA)
      
      params <- list(objective= "binary:logistic",eval_metric= "auc",eta= grid$eta[g],max_depth= grid$max_depth[g],
      subsample= grid$subsample[g],colsample_bytree = grid$colsample_bytree[g],min_child_weight = grid$min_child_weight[g],
      gamma= grid$gamma[g],lambda= grid$lambda[g],alpha= grid$alpha[g])
      
      # Entrenar modelo interno para hiperparámetros
      xgb_tmp <- xgb.train(params= params,data  = dtrain_in,nrounds = nrounds_max,
      watchlist = list(val = dval_in),early_stopping_rounds = early_stop_rounds,verbose  = 0)
      
      # -- Calcular resutlados -- #
      nrounds_found <- c(nrounds_found, xgb_tmp$best_iteration)
      
      p_val   <- predict(xgb_tmp, dval_in)
      p_tr_in <- predict(xgb_tmp, dtrain_in)
      
      aucs_val   <- c(aucs_val,   as.numeric(auc(y_val, p_val,   quiet = TRUE)))
      aucs_train <- c(aucs_train, as.numeric(auc(y_tr,  p_tr_in, quiet = TRUE)))
      
      cal_in    <- calibrar_isotonica(p_tr_in, y_tr, p_val) # Se calbira internamente
      p_val_cal <- cal_in$p_cal
      
      briers_val   <- c(briers_val,   mean((p_val_cal - y_val)^2))
      briers_train <- c(briers_train, mean((p_tr_in   - y_tr)^2))
      
      roc_cal    <- roc(y_val, p_val_cal, quiet = TRUE)
      coords_all <- pROC::coords(roc_cal, x = "all",ret = c("threshold","sensitivity","specificity"),transpose = FALSE)
      youden     <- coords_all$sensitivity + coords_all$specificity - 1
      thresholds <- c(thresholds, coords_all$threshold[which.max(youden)])
    }
    
    grid$AUC_val_inner[g]     <- mean(aucs_val,      na.rm = TRUE)
    grid$AUC_train_inner[g]   <- mean(aucs_train,    na.rm = TRUE)
    grid$THR_inner[g]         <- mean(thresholds,    na.rm = TRUE)
    grid$nrounds_optimo[g]    <- round(mean(nrounds_found, na.rm = TRUE))
    grid$Brier_val_inner[g]   <- mean(briers_val,    na.rm = TRUE)
    grid$Brier_train_inner[g] <- mean(briers_train,  na.rm = TRUE)
  }
  
  # - - - - - - - - - - - - #
  # CICLO EXTERNO DE LA NCV #
  # - - - - - - - - - - - - #
  
  # Mejor grilla
  best <- grid[which.max(grid$AUC_val_inner), ]
  dtrain_full <- xgb.DMatrix(data = X_train, label = y_train, missing = NA)
  dtest   <- xgb.DMatrix(data = X_test,  label = y_test,  missing = NA)
  
  params_final <- list(objective  = "binary:logistic",eval_metric = "auc",eta= best$eta,max_depth= best$max_depth,
  subsample= best$subsample,colsample_bytree = best$colsample_bytree,min_child_weight = best$min_child_weight,
  gamma= best$gamma,lambda= best$lambda,alpha= best$alpha)
  
  # Entrenar el modelo final
  model_final <- xgb.train(params = params_final,data = dtrain_full,nrounds = best$nrounds_optimo,verbose = 0)
  
  # - - - - - - - - - - - - - - - - #
  #   CALIBRACION DE TRAIN Y VAL    #
  # - - - - - - - - - - - - - - - - #
  
  cat("  Generando OOF para calibración isotónica...\n")
  p_oof_train <- rep(NA, length(y_train))
  
  # Ciclo de calbiración por fols externos
  for(f in inner_folds_cal) {val_idx_cal  <- ftrain_idx_cal <- setdiff(1:length(y_train), val_idx_cal)
    dtrain_cal <- xgb.DMatrix(data  = X_train[train_idx_cal, ],label = y_train[train_idx_cal], missing = NA)
    dval_cal   <- xgb.DMatrix(data  = X_train[val_idx_cal, ],label = y_train[val_idx_cal],   missing = NA)
    xgb_cal_tmp <- xgb.train(params = params_final,data  = dtrain_cal,nrounds = best$nrounds_optimo,verbose = 0)
    p_oof_train[val_idx_cal] <- predict(xgb_cal_tmp, dval_cal)
  }
  
  p_test  <- predict(model_final, dtest)
  p_train <- predict(model_final, dtrain_full)
  
  # - - - - - - - - - - - #
  #   MÉÞRICAS FINALES    #
  # - - - - - - - - - - - #
  
  auc_test        <- as.numeric(auc(y_test,  p_test))
  auc_train_outer <- as.numeric(auc(y_train, p_train))
  brier_null_test <- mean(y_test) * (1 - mean(y_test))
  brier_test_raw  <- mean((p_test  - y_test)^2)
  brier_train_raw <- mean((p_train - y_train)^2)
  bss_test_raw    <- 1 - (brier_test_raw / brier_null_test)
  
  cal_result <- calibrar_isotonica(p_oof_train, y_train, p_test)
  p_test_cal <- cal_result$p_cal
  
  auc_test_cal   <- as.numeric(auc(y_test, p_test_cal))
  brier_test_cal <- mean((p_test_cal - y_test)^2)
  bss_test_cal   <- 1 - (brier_test_cal / brier_null_test)
  
  p_oof_cal  <- calibrar_isotonica(p_oof_train, y_train, p_oof_train)$p_cal
  
  roc_oof    <- roc(y_train, p_oof_cal, quiet = TRUE)
  coords_oof <- pROC::coords(roc_oof, x = "all",ret = c("threshold","sensitivity","specificity"),transpose = FALSE)
  youden_oof <- coords_oof$sensitivity + coords_oof$specificity - 1
  thr_final  <- coords_oof$threshold[which.max(youden_oof)]
  
  pred_raw <- factor(ifelse(p_test     >= best$THR_inner, 1, 0), levels = c(0,1))
  pred_cal <- factor(ifelse(p_test_cal >= thr_final,      1, 0), levels = c(0,1))
  
  cm_raw <- confusionMatrix(pred_raw, factor(y_test, levels = c(0,1)), positive = "1")
  cm_cal <- confusionMatrix(pred_cal, factor(y_test, levels = c(0,1)), positive = "1")
  
  resultados_lista[[length(resultados_lista) + 1]] <- data.frame(
  Fold  = k,AUC_Train_Inner   = best$AUC_train_inner,AUC_Val_Inner = best$AUC_val_inner,AUC_Train_Outer = auc_train_outer,
  AUC_Test_Outer = auc_test,AUC_Test_Cal = auc_test_cal,BSS_Test_Raw = bss_test_raw,BSS_Test_Cal = bss_test_cal,
  Brier_Train_Inner = best$Brier_train_inner,Brier_Val_Inner = best$Brier_val_inner,Brier_Train_Outer = brier_train_raw,
  Brier_Test_Raw  = brier_test_raw,Brier_Test_Cal= brier_test_cal,Sens_Raw= cm_raw$byClass["Sensitivity"],
  Spec_Raw= cm_raw$byClass["Specificity"],Sens_Cal= cm_cal$byClass["Sensitivity"],Spec_Cal= cm_cal$byClass["Specificity"],
  Threshold_Raw  = best$THR_inner,Threshold_Cal  = thr_final,nrounds_optimo = best$nrounds_optimo,eta = best$eta,
  max_depth = best$max_depth,subsample  = best$subsample,colsample_bytree = best$colsample_bytree,min_child_weight = best$min_child_weight,
  gamma = best$gamma,lambda= best$lambda,alpha  = best$alpha)
  
  probs_lista[[k]] <- data.frame(Fold  = k,y_real = y_test,p_raw  = p_test,p_cal  = p_test_cal)
  
clave <- paste0("fold", k)
modelos_lista[[clave]] <- list(modelo = model_final,
iso_fit  = cal_result$iso_fit,p_oof_train  = p_oof_train,prep_recipe  = prep_rec,X_train  = X_train,
X_test = X_test,y_train = y_train,y_test = y_test,threshold_raw = best$THR_inner,
threshold_cal = thr_final,nrounds_optim = best$nrounds_optimo,fold  = k)}

resultados_df <- bind_rows(resultados_lista)
probs_df      <- bind_rows(probs_lista)

saveRDS(resultados_lista, "Codigos y salidas/Resultados Modelos ML/XGB_resultados_lista.rds")
saveRDS(modelos_lista,    "Codigos y salidas/Resultados Modelos ML/XGB_modelos_lista.rds")
saveRDS(resultados_df,    "Codigos y salidas/Resultados Modelos ML/XGB_resultados_df.rds")
saveRDS(probs_df,         "Codigos y salidas/Resultados Modelos ML/XGB_probs_calibracion.rds")


# ====================================================================#
#       MODELO XGBOOT: SEGUNDA PARTE, VALIDACION CRUZADA ANIDADA      #
# ====================================================================#
#                     Código ejecutado en Servidor CIAT               #
#                         Tiempo aproximado: NA Hr                    #
# --------------------------------------------------------------------#

# - - - - - - - -  #
# CV AJUSTE FINAL  # 
# - - - - - - - -  #

set.seed(10)
folds_tuning_final <- createFolds(y_all, k = 10)

grid_final$AUC_val<- NA;grid_final$AUC_train <- NA;grid_final$nrounds <- NA;grid_final$Brier_val <- NA

for (g in 1:nrow(grid_final)) {
  
  cat("Grilla", g, "/", nrow(grid_final), "\n")
  aucs_val<- c();aucs_train <- c();nrounds_found <- c();briers_val <- c()
  for (f in folds_tuning_final) {
    
    val_idx   <- f
    train_idx <- setdiff(seq_along(y_all), val_idx)
  
    dtrain_g <- xgb.DMatrix(data  = X_all[train_idx, ],label = y_all[train_idx],missing = NA)
    dval_g <- xgb.DMatrix(data  = X_all[val_idx, ],label = y_all[val_idx],missing = NA)
    
    params_g <- list(objective = "binary:logistic",eval_metric  = "auc",eta = grid_final$eta[g],
    max_depth = grid_final$max_depth[g],subsample = grid_final$subsample[g],colsample_bytree = grid_final$colsample_bytree[g],
    min_child_weight = grid_final$min_child_weight[g],gamma = grid_final$gamma[g],lambda=grid_final$lambda[g],alpha =grid_final$alpha[g])
    
    xgb_g <- xgb.train(params = params_g,data= dtrain_g,nrounds = nrounds_max,watchlist = list(val = dval_g),
    early_stopping_rounds = early_stop_rounds,verbose = 0)
    
    p_val   <- predict(xgb_g, dval_g);p_train <- predict(xgb_g, dtrain_g)
    
    # Calibración isotónica dentro del fold para Brier
    cal_g     <- calibrar_isotonica(p_train, y_all[train_idx], p_val)
    p_val_cal <- cal_g$p_cal
    
    aucs_val      <- c(aucs_val,as.numeric(auc(y_all[val_idx],   p_val,   quiet = TRUE)))
    aucs_train    <- c(aucs_train,as.numeric(auc(y_all[train_idx], p_train, quiet = TRUE)))
    nrounds_found <- c(nrounds_found, xgb_g$best_iteration)
    briers_val    <- c(briers_val,mean((p_val_cal - y_all[val_idx])^2))
  }
  
  grid_final$AUC_val[g] <- mean(aucs_val, na.rm = TRUE);grid_final$AUC_train[g] <- mean(aucs_train,na.rm = TRUE)
  grid_final$nrounds[g] <- round(mean(nrounds_found, na.rm = TRUE));grid_final$Brier_val[g] <- mean(briers_val,na.rm = TRUE)
}

HP_final <- grid_final %>%arrange(desc(AUC_val)) %>%slice(1)

# - - - - - - - - - - - -  #
# AJUSTE DEL MODLEO FINAL  # 
# - - - - - - - - - - - -  #

# Seleción mejores hiperparámetros
params_final <- list(objective = "binary:logistic", eval_metric = "auc",eta = HP_final$eta,max_depth = HP_final$max_depth,
subsample = HP_final$subsample,colsample_bytree = HP_final$colsample_bytree,min_child_weight = HP_final$min_child_weight,
gamma = HP_final$gamma,lambda= HP_final$lambda,alpha = HP_final$alpha)

# Ajuste final
set.seed(2024)
modelo_xgb_final <- xgb.train(params= params_final,data = dtrain_all,nrounds = HP_final$nrounds,verbose = 1)


# - - - - - - - - - - - - - - - -  #
#   CALIBRACIÓN DEL MODELO FINAL   # 
# - - - - - - - - - - - - - - - -  #

cat("\nGenerando OOF para calibración isotónica...\n")

set.seed(20)
folds_cal_final <- createFolds(y_all, k = 10)

p_oof <- rep(NA, length(y_all))

# -- CICLO POR FOLD -- #

for (f in seq_along(folds_cal_final)) {
  
  val_idx   <- folds_cal_final[[f]]
  train_idx <- setdiff(seq_along(y_all), val_idx)
  
  dtrain_c <- xgb.DMatrix(data  = X_all[train_idx, ],label = y_all[train_idx],missing = NA)
  dval_c <- xgb.DMatrix(data  = X_all[val_idx, ],label = y_all[val_idx],missing = NA)
  xgb_cal <- xgb.train(params  = params_final,data    = dtrain_c,nrounds = HP_final$nrounds,verbose = 0)
  
  p_oof[val_idx] <- predict(xgb_cal, dval_c)
}

# Ajuste isotónico
ord        <- order(p_oof)
p_oof_ord  <- p_oof[ord]
y_ord      <- y_all[ord]
iso_final  <- gpava(p_oof_ord, y_ord)

# Calibración
calibrar_final <- function(p_new) {p_cal <- approx(x = iso_final_df$p_oof_ord,y = iso_final_df$iso_final$x,
xout = p_new,method = "linear",rule  = 2)$ypmax(0, pmin(1, p_cal))}


# - - - - - - - - --  #
#      VALORES SHAP   # 
# - - - - - - - - -   #

shap_matrix <- predict(modelo_xgb_final,newdata = dtrain_all,predcontrib = TRUE)

# Separar SHAP 
shap_vals <- shap_matrix[, -ncol(shap_matrix)]
bias   <- shap_matrix[, ncol(shap_matrix)]
colnames(shap_vals) <- colnames(X_all)

# Calcular y guardar SHAP
logodds_pred <- predict(modelo_xgb_final, dtrain_all, outputmargin = TRUE)
sv_final <- shapviz(shap_vals, X = as.data.frame(X_all), baseline = mean(bias))
saveRDS(sv_final, "Codigos y salidas/Resultados Modelos ML/XGB_shapviz.rds")


# ==============================================================#
#   MODELO XGBOOST: TERCERA PARTE, RESULTADOS DEL REPORTE       #
# ==============================================================#

# - - - - - - - - - - - - - - - -#
#       RESULTADOS NCV XGB       #
# - - - - - - - - - - - - - - -  #

NCV_XGB_P <- readRDS("Códigos y salidas/Resultados Modelos ML/XGB_probs_calibracion.rds")

# Contruir agrupaciones por deciles
DF_P_Cal <- NCV_XGB_P %>%mutate(bin = cut(p_raw,breaks = seq(0, 1, by = 0.1),include.lowest = TRUE)) %>%group_by(bin) %>%
summarise(mean_raw = mean(p_raw),mean_cal = mean(p_cal),obs_rate = mean(y_real),n = n(),.groups  = "drop")

#  Se crea leyenda personalizada
DF_P_Cal_Long <- bind_rows(DF_P_Cal %>%transmute(prob= mean_raw,obs= obs_rate,n=n,Modelo = "Sin calibración"),DF_P_Cal %>%
transmute(prob= mean_cal,obs= obs_rate,n= n,Modelo = "Calibración isotónica"))

# Colores de calibración
cols_cal <- c("Sin calibración" = "#3d4756","Calibración isotónica" = "#AB6F6F" )

# Figura final
Calibracion_XGB <- ggplot(DF_P_Cal_Long,aes(x = prob,y = obs,colour = Modelo))+geom_abline(slope = 1,intercept = 0,
linetype = "dashed",colour = "#0072FF",linewidth = 0.5) +geom_line(linewidth = 0.9,alpha = 0.9) +
geom_point(size = 2.8,alpha = 0.9) +scale_colour_manual(values = cols_cal,name   = NULL) +scale_x_continuous(
labels = scales::percent_format(accuracy = 1),limits = c(0, 1),expand = expansion(mult = 0.01)) +
scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits = c(0, 1),expand = expansion(mult = 0.01)
) +labs(x = "Probabilidad predicha",y = "Frecuencia observada") +theme_mine(base_size = 13, base_family = "Times") +
theme(legend.position = c(0.78, 0.2),legend.background = element_rect(fill = ggplot2::alpha("white", 0.85), colour = NA),
legend.text = element_text(size = 12,colour = "#2F2F2F"),axis.text.x = element_text(size = 12),axis.text.y = element_text(size = 12),
axis.title.x = element_text(size = 14,margin = margin(t = 8)),axis.title.y = element_text(size = 14,margin = margin(r = 8)),
panel.grid.major = element_blank(),panel.grid.minor = element_blank())

# ----- RESULTADOS MODELOS NCV RF ----- #

# Diccioanrio de métricas para mostrar en documento
labels_metricas <- c("AUC_Train_Outer" = "AUC (Entrenamiento)","AUC_Test_Outer"  = "AUC (Prueba)",
"AUC_Test_Cal"  = "AUC (Prueba con calibración)","BSS_Test_Raw"  = "Brier Score escalado (Prueba)",
"BSS_Test_Cal"  = "Brier Score escalado (Prueba con calibración)","Brier_Test_Raw"  = "Brier Score (Prueba)",
"Brier_Test_Cal"= "Brier Score (Prueba con calibración)","Sens_Raw"  = "Sensibilidad (Prueba)",
"Spec_Raw"= "Especificidad (Prueba)","Sens_Cal"= "Sensibilidad (Prueba con calibración)",
"Spec_Cal"= "Especificidad (Prueba con calibración)","Threshold_Raw"   = "Umbral de clasificación",
"Threshold_Cal"   = "Umbral de clasificación (Con calibración)")

vars_metricas <- c("AUC_Train_Outer", "AUC_Test_Outer", "AUC_Test_Cal","BSS_Test_Raw", "BSS_Test_Cal","Brier_Test_Raw", "Brier_Test_Cal",
"Sens_Raw", "Spec_Raw","Sens_Cal", "Spec_Cal","Threshold_Raw", "Threshold_Cal")


NCV_XG <- readRDS("Códigos y salidas/Resultados Modelos ML/XGB_resultados_df.rds")

# -- Tabla de resultados NCV para RF por fold y general
Tabla_NVC_XG <- NCV_XG %>%select(Fold, all_of(vars_metricas)) %>%pivot_longer(cols = all_of(vars_metricas),names_to  = "Metrica",
values_to = "Valor") %>%mutate(Metrica = factor(Metrica, levels = names(labels_metricas))) %>%arrange(Metrica)

# Columna General 
Tabla_NVC_XG_General <- Tabla_NVC_XG %>%group_by(Metrica) %>%summarise(Media_Gen = mean(Valor, na.rm = TRUE),
SD_Gen = sd(Valor,   na.rm = TRUE),.groups   = "drop") %>%mutate(General = sprintf("%.3f", Media_Gen)) %>%
select(Metrica, General)

#Tabla final
Tabla_NVC_XG_Final <- Tabla_NVC_XG %>%mutate(Resultado = sprintf("%.3f", Valor),Metrica = labels_metricas[as.character(Metrica)]
) %>%select(Metrica, Fold, Resultado) %>%pivot_wider(names_from  = Fold,values_from = Resultado,names_prefix = "Fold") %>%
left_join(Tabla_NVC_XG_General %>%mutate(Metrica = labels_metricas[as.character(Metrica)]),by = "Metrica" )

# Tabla LaTeX
kbl(Tabla_NVC_XG_Final,format = "latex",booktabs  = TRUE,align = c("l", rep("c", ncol(Tabla_NVC_XG_Final) - 1)),
caption= "Resultados por fold externo para el modelo XGBoost y promedio general con desviación estándar entre paréntesis.",
label = "tab:ncv_xg_folds", escape = FALSE) %>% kable_styling(latex_options = c("hold_position", "scale_down"))


#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#              RESULTADOS MODELOS FINALES XGB: SHAP                #
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #


# Se Cargan los resultados del modelo final producidos por el servidor CIAT:
SHAP_values_XGB <- readRDS("Códigos y salidas/Resultados Modelos ML/Salidas modelos finales/XGB_shapviz.rds")

# Se cargan los HP del modelo final: 
HP_XGB <- readRDS("Códigos y salidas/Resultados Modelos ML/Salidas modelos finales/XGB_HP_final.rds")


# -- Diccionario de variables para los modelos de ML -- #

dic_labels <- c("Edad_P" = "Edad del productor","F2" = "Experiencia del productor","HH_size" = "Tamaño del hogar","Educación_Hogar" = "Educación promedio hogar",
"F16" = "Rendimiento","F17_log" = "Log(Ventas + 1)","índice_Activos" = "Índice de activos","Región_Llanos" = "Región: Llanos","Región_Andina" = "Región: Andina",
"Genero_P_Mujer" = "Género: Mujer","Escolaridad_P_Cat_Secundaria" = "Escolaridad del P: Secundaria","Escolaridad_P_Cat_Terciaría" = "Escolaridad del P: Terciaria","F7_M_Sí" = "Siembra mecanizada: Sí",
"F11_Sí" = "Semilla certificada: Sí","F12_Sí" = "Pérdida de cosecha: Sí","Irrigation_system_Sí" = "Riego: Sí","L1_Sí" = "Crédito: Sí","K1_Sí" = "Otros ingresos: Sí",
"org_member_Sí" = "Miembro de alguna organización: Sí","received_training_Sí" = "Recibió capacitaciones: Sí","J4_Sí" = "Asistió a (MTA): Sí","E1_Sí" = "Afectaciones: Sí",
"E2_Sí" = "Estrategias: Sí","Cultivo_Maíz" = "Cultivo: Maíz")


# Colores para SHAP V.
col_bajo  <- "#BC7D73";col_medio <- "#E8E4DC";col_alto<- "#3A627D"

#  - - - - - - - - - - - - - - - - - - - #
# FIGURA DE CONTRUBICIONES POR PRODUCTOR #
#  - - - - - - - - - - - - - - - - - - - #

#- Se genera la figura génerica del paquete shapviz
SHAP_XGB_G_BEE <- sv_importance(SHAP_values_XGB,kind = "beeswarm",show_numbers = FALSE,max_display = 24)

# -Se mejora la figura genéria
SHAP_XGB_F_BEE <- SHAP_XGB_G_BEE +scale_colour_gradientn(colours = c(col_bajo, col_medio, col_alto),limits  = c(0, 1),breaks  = c(0, 1),
labels= c("Bajo", "Alto"),name= NULL,guide = guide_colorbar(direction = "vertical",barwidth = unit(0.4, "cm"),barheight = unit(4,"cm"),
ticks = FALSE, label.theme  = element_text(family = "Times",size = 11,colour = "black"))) +scale_y_discrete(
labels = function(x) ifelse(x %in% names(dic_labels), dic_labels[x], x)) +geom_vline(xintercept = 0,colour = "#888888",
linewidth = 0.5,linetype = "dashed") +scale_x_continuous(labels = scales::number_format(accuracy = 0.01),expand = expansion(mult = 0.05)) +
labs(x = "Valor SHAP (← reduce adopción | aumenta adopción →)",y = NULL) +theme_mine(base_size = 12, base_family = "Times") +
theme(legend.position = "right",plot.title = element_text(face = "bold", size = 15, hjust = 0),plot.subtitle = element_text(
size = 12,colour = "black",hjust = 0,margin = margin(b = 8)),axis.text.y = element_text(size = 14),axis.text.x = element_text(size = 14),
panel.grid.major.y = element_blank())

#  - - - - - - - - - - - - - - - - - - - #
# FIGURA DE SHAP VS COVARAIBLES CONTINUAS
#  - - - - - - - - - - - - - - - - - - - #


# Variables continuas para los gráficos SHAP vs covaraibles:
VAR_Continuas <- c("Edad_P", "F2", "HH_size", "Educación_Hogar", "F16", "F17_log", "índice_Activos")

# Generar los 7 gráficos
Figuras_Depen_SHAP <- lapply(VAR_Continuas,function(v) {plot_dependence(var  = v,shap = SHAP_values_XGB)});names(Figuras_Depen_SHAP) <- VAR_Continuas
Figuras_Depen_SHAP <- wrap_plots(Figuras_Depen_SHAP, ncol = 3) # Unificar las figuras

#  - - - - - - - - - - - - - - - - - - - #
#     TABLA DE CONTRIBUCIONES |SHAP|
#  - - - - - - - - - - - - - - - - - - - #

# Extraer matriz SHAP y pasar a df
shap_mat_xgb <- get_shap_values(SHAP_values_XGB)
shap_imp_xgb <- data.frame(Variable = colnames(shap_mat_xgb),Mean_ABS_SHAP = colMeans(abs(shap_mat_xgb))) 

# Aplicar labels
shap_imp_xgb$Variable <- ifelse(shap_imp_xgb$Variable %in% names(dic_labels),dic_labels[shap_imp_xgb$Variable],shap_imp_xgb$Variable)
shap_imp_xgb <- shap_imp_xgb %>%arrange(desc(Mean_ABS_SHAP));rownames(shap_imp_xgb)=NULL
shap_imp_xgb$Mean_ABS_SHAP <- round(shap_imp_xgb$Mean_ABS_SHAP, 4);shap_imp_xgb <- shap_imp_xgb %>%filter(!grepl("unknown", Variable, ignore.case = TRUE))

# Extraer tabla formato latex:

SHAP_table_latex <- kbl(shap_imp_xgb,format = "latex",booktabs = TRUE,align = c("l", "c"),col.names = c("Variable", "$|SHAP|$ medio"),
caption = "Importancia global promedio basada en valores absolutos SHAP para el modelo XGBoost.",
label = "tab:shap_xgb") %>%kable_styling(latex_options = c("hold_position"))







