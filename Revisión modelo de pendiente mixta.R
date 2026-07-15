
# --------------------------------------------------------------------# 
# ███╗   ███╗██████╗ ██╗   ██╗██╗      ██████╗         ██████╗        #
# ████╗ ████║██╔══██╗██║   ██║██║     ██╔═══██╗██╗    ██╔═████╗       #
# ██╔████╔██║██║  ██║██║   ██║██║     ██║   ██║╚═╝    ██║██╔██║       #
# ██║╚██╔╝██║██║  ██║██║   ██║██║     ██║   ██║██╗    ████╔╝██║       #
# ██║ ╚═╝ ██║██████╔╝╚██████╔╝███████╗╚██████╔╝╚═╝    ╚██████╔╝       #
# ╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚══════╝ ╚═════╝         ╚═════╝        #
# --------------------------------------------------------------------# 
# CARGA DE DATOS LIBRERÍA Y CÓDIGO OROGINAL DEL AJUSTE LOGÍSTICO      #
# --------------------------------------------------------------------# 
# !!NOTA:!! La primera parte  de este código o el MÓDULO 0 contiene
# todo el código necesario para el ajuste del modelo logistico. Como se
# hace uso de la imputacion multiple por mice, para el modelo de pendiente mixta
# es menester eliminar la varaible A1_std y remplazarla por los departamentos
# inclusive en el proceso de imputación, por eso se replica todo el código
# con la differencia de quirtar region y añadir A1_std
# --------------------------------------------------------------------# 


# ------------------------- #
#       Carga de datos      #
# ------------------------- #


#-- Establecer directorio de trabajo y cargar datos
setwd("C:/Users/kmili/OneDrive - CGIAR/Desktop/TDG/");source("Códigos y salidas/Funciones base.R")

#setwd("C:/Users/kmili/Desktop/Todo/TDG/Código");source("Funciones base.R")
Base <-read_dta("Datos/Base_unida [ARROZ Y MAÍZ PARA TDG].dta")
Base$F2[Base$F2 == -99] <- NA
# Se identificar los prodcutores que no pudieron identificarse directamente, o que son familiares de tal y se lograron mapear.
Base$Condición_P_2 <- ifelse(rowSums(!is.na(Base[, c("Edad_P", "Genero_P", "Escolaridad_P_Cat")])) > 0,1,0)

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
Base_F <- Base %>% select(ID_Productor,New_Key,Adop_Pronósticos,A1_std, A1_std, Edad_P, Genero_P, Escolaridad_P, Escolaridad_P_Cat, Etnia_P, F2, HH_size ,Num_15,Num_15_28,Num_Adultos,Num_Adultos_Mayores,share_15_28 ,share_60plus,Educación_Hogar,M1, F7_M,
F11, F12, F14,F15, F16,F17, Irrigation_system, L1, K1, org_member, received_training, J4,E1, P1, P3, P5, variety_age_group,E2,E1_A,E1_B,E1_D,E1_E,E1_F,Distancia_Final,Distancia_via,N1_A, N1_B, N1_C, N1_D, N1_E, N2_A, N2_B, N2_C, N2_D, N2_E, N2_F, N2_G, N2_H,Origen_Datos,Cultivo,Condición_P,Condición_P_2,Año)


Indice_Activo_DF <- crear_indice_MCA(df = Base_F %>% select(New_Key,N1_A, N1_B, N1_C, N1_D, N1_E, N2_A, N2_B, N2_C, N2_D, N2_E, N2_F, N2_G, N2_H) %>% na.omit(),id_var = "New_Key",
vars_mca =c("N1_A", "N1_B", "N1_C", "N1_D", "N1_E", "N2_A", "N2_B", "N2_C", "N2_D", "N2_E", "N2_F", "N2_G", "N2_H"),
nombre_indice = "índice_Activos",path_pesos = "TDG/Tablas y figuras/Pesos_Indicador.xlsx")

# Unir indice al df completo
Base_F <-Base_F %>%  left_join(Indice_Activo_DF %>% select(New_Key,índice_Activos)) %>% filter(Condición_P==1 | Condición_P_2==1)



# - Añadir labels de variables recien creadas
labels_DF <- c(índice_Activos="índice de activos",Cultivo="Cultivo")
for (var in names(labels_DF)) {if (var %in% names(Base_F)) {labelled::var_label(Base_F[[var]]) <- labels_DF[var]}}


# -------------------------------- #
#   SELECCIÓN DE VARIABLES         #
# -------------------------------- #

# - Selecionar variables a usar
Base_F_2 <- Base_F %>% select(ID_Productor,New_Key,Adop_Pronósticos, A1_std, Edad_P, Genero_P, Escolaridad_P_Cat, F2, HH_size,Educación_Hogar, F7_M,
F11, F12,F16,F17, Irrigation_system, L1, K1, org_member, received_training, J4,E1,E2,índice_Activos,Cultivo,Año)

# - reporte de NAS
Reporte_NA <- miss(Base_F_2,id_var="New_Key");NA_Observaciones <-  Reporte_NA$Reporte_ID

# - Revisando y borrando registros con variables  de más 4 NA (DOS CASOS)
Id_borar <- NA_Observaciones %>% filter(N_NA>4)

# Dejar base sin registros con más de 4 NA
Base_F_2_Impu.1 <- Base_F_2 %>% filter(!New_Key %in% Id_borar$ID) 


# -------------------------------------------------------------- #
#  REVISIÓN DE VARIABLES Y  NA CON BASE DE REGISTROS ELIMINADOS  #
# -------------------------------------------------------------- #

# Transformación de las cantidades vendidas
Base_F_2_Impu.1 <- Base_F_2_Impu.1 %>%mutate(F17_log = log(F17 + 1)) %>% select(-F17)

# Se pasan las categóricas como tal sólo para visualización de las tablas
Base_F_2_Impu.1_CAT <- Base_F_2_Impu.1 %>% mutate(across(c(Adop_Pronósticos,A1_std,Genero_P, Escolaridad_P_Cat, F7_M,F11,F12,Irrigation_system,L1,K1,org_member,received_training,J4,E1,E2,Año), haven::as_factor))

# -------------------------------------------------------------- #
#                     IMPUTACIÓN                                 #
# -------------------------------------------------------------- #

# Dejar factores como tal solo para imputación
Base_F_2_Impu.1_CAT_2 <- Base_F_2_Impu.1 %>% mutate(across(c(Adop_Pronósticos,A1_std,Genero_P, Escolaridad_P_Cat, F7_M,F11,F12,Irrigation_system,L1,K1,org_member,received_training,J4,E1,E2,Año), haven::as_factor))

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


# --------------------------------------------------------------------# 
# ███╗   ███╗██████╗ ██╗   ██╗██╗      ██████╗         ██╗            #
# ████╗ ████║██╔══██╗██║   ██║██║     ██╔═══██╗██╗    ███║            #
# ██╔████╔██║██║  ██║██║   ██║██║     ██║   ██║╚═╝    ╚██║            #
# ██║╚██╔╝██║██║  ██║██║   ██║██║     ██║   ██║██╗     ██║            #
# ██║ ╚═╝ ██║██████╔╝╚██████╔╝███████╗╚██████╔╝╚═╝     ██║            #
# ╚═╝     ╚═╝╚═════╝  ╚═════╝ ╚══════╝ ╚═════╝         ╚═╝            #
# --------------------------------------------------------------------# 
#           AJUSTE DEL MODELO CON PENDIENTE MIXTA                     #
# --------------------------------------------------------------------# 

# Se carga la tabla del modelo lgositco final para comparación
Tabla_Logistico_Final <- readRDS("Códigos y salidas/Datos/Tabla_Logistico_Final.rds")

# Se ajusta el modelo mixto con MICE
Modelo_Mixto <- with(Base_F_2_Impu.2,glmer(Adop_Pronósticos ~ Edad_P + Genero_P + Escolaridad_P_Cat + F2 + HH_size +Educación_Hogar + F7_M + F11 + F12 + F16 + F17_log + Irrigation_system + 
L1 + K1 + org_member + received_training + J4 + E1 + E2 +índice_Activos + Cultivo + (1 | A1_std),family = binomial))

# Se poolean las 12 imputaciones por rublin
Modelo_mixto_consolidado <- pool(Modelo_Mixto)

# Se saca el resumen estadistico
summary(Modelo_mixto_consolidado)

# Df de mapeo de nombres (igual que el modelo orginal)
term <- c("(Intercept)","RegiónLlanos","RegiónAndina","Edad_P","Genero_PMujer","Escolaridad_P_CatSecundaria","Escolaridad_P_CatTerciaría","F2","HH_size","Educación_Hogar","F7_MSí","F11Sí","F12Sí","F16","F17_log","Irrigation_systemSí","L1Sí","K1Sí","org_memberSí","received_trainingSí","J4Sí","E1Sí","E2Sí","índice_Activos","CultivoMaíz")
Variable <- c("(Intercept)","Región: Llanos","Región: Andina","Edad del productor","Género: Mujer","Escolaridad del P: Secundaria","Escolaridad del P: Terciaría","Experiencia del productor","Tamaño del hogar","Años promedio de educación del hogar","Siembra mecanizada: Sí","Semilla certificada: Sí","Pérdida de cosecha: Sí","Rendimiento","$Log(Venta+1)$","Riego: Sí","Crédito: Sí","Otros ingresos : Sí","Miembro de alguna organización: Sí","Recibió capacitaciones: Sí","Asistió a (MTA): Sí","Afectaciones: Sí","Estrategias:Sí","Índice de activos","Cultivo: Maíz")
Df_labels <- data.frame(term  ,Variable)


#  resumen del modelo mixto como df para comparar con el modelo logistico final
Resumen_Modelo_mixto_consolidado <- summary(Modelo_mixto_consolidado) %>%as.data.frame() %>%mutate(Beta_M  = round(estimate,   2),SE_M    = round(std.error,  2),p.value_M = round(p.value,  2)
) %>%select(term, Beta_M, SE_M, p.value_M)

# Unir con tabla logístico final 
Tabla_comparativa_mixto <- Tabla_Logistico_Final %>%left_join(Df_labels, by = "Variable") %>%       
left_join(Resumen_Modelo_mixto_consolidado, by = "term") %>%  mutate(D_Beta = round(abs(Beta   - Beta_M),  2),
D_SE  = round(abs(SE - SE_M),2),D_p= round(abs(p.value - p.value_M), 2)) %>%select(Variable,Beta,Beta_M,SE,SE_M,p.value,p.value_M,D_Beta, D_SE, D_p)

# Tabla final en latex
kbl(Tabla_comparativa_mixto,format = "latex",booktabs  = TRUE,escape  = FALSE,align = c("l", rep("c", ncol(Tabla_comparativa_mixto) - 1)),
col.names = c("Variable","$\\hat{\\beta}_{L}$", "$\\hat{\\beta}_{M}$","$\\widehat{SE}_{L}$", "$\\widehat{SE}_{M}$","$p_{L}$", "$p_{M}$",
"$|\\hat{\\beta}_{L} - \\hat{\\beta}_{M}|$","$|\\widehat{SE}_{L} - \\widehat{SE}_{M}|$","$|p_{L} - p_{M}|$"),
caption = paste0("Comparación de estimaciones entre el modelo logístico ($L$) y el modelo ","logístico mixto con intercepto aleatorio por municipio ($M$). ",
"Las diferencias se expresan en valor absoluto."),label = "tab:comp_logit_mixto") %>%add_header_above(c(" " = 1,"Coeficientes" = 2,"Errores est. " = 2,
"Valores $p$"   = 2,"Dif. absoluta" = 3),escape = FALSE) %>%footnote(general = "Subíndices: $L$ = modelo logístico; $M$ = modelo mixto con intercepto aleatorio por municipio.",
escape  = FALSE,general_title = "Nota:") %>% kable_styling(latex_options = c("hold_position", "scale_down"))
