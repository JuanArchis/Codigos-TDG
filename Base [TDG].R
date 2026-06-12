# -----------------------------------------------------------# 
#                 📑 CREAR BASE PARA EL TDG 📑               #
# -----------------------------------------------------------# 


# ~~~~~~~~~~~~~~~~~~~~~~~~~ #
#       Carga de datos      #
# ~~~~~~~~~~~~~~~~~~~~~~~~~ #

#-- Establecer directorio de trabajo y cargar datos
setwd("D:/OneDrive - CGIAR/Desktop/Maíz (Estudio institucional)");source("LECRA/Código/Funciones base.R")

#-- Cargar bases de modelos de diana
MAÍZ_LECRA_MODEL <- read_dta("Bases_Finales_Diana/MAÍZ_LECRA_MODEL_v2.dta") %>% mutate(Origen_Datos="Maíz LECRA",ID_Productor=ID_PRODUCTOR)
MAÍZ_CAS_MODEL <- read_dta("Bases_Finales_Diana/MAÍZ_CAS_MODEL.dta") %>% mutate(Origen_Datos="Maíz CAS",ID_Productor=nuevo_id)
ARROZ_LECRA_MODEL <- read_dta("Bases_Finales_Diana/ARROZ_LECRA_MODEL.dta") %>% mutate(Origen_Datos="Arroz LECRA",ID_Productor=id_productor)
ARROZ_CAS_MODEL <- read_dta("Bases_Finales_Diana/ARROZ_CAS_MODEL.dta") %>% mutate(Origen_Datos="Arroz CAS",ID_Productor=nuevo_id)


x <- MAÍZ_CAS_MODEL %>% filter(is.na(Rendimiento_new))
table(haven::as_factor(x$a15))




# Cambiar algunos nombrees para facilitar unión

names(MAÍZ_CAS_MODEL)[26] <- "Genero_P"
ARROZ_CAS_MODEL <- ARROZ_CAS_MODEL %>% mutate(Etnia_P_A=NA_character_)

# estandarizar y crear varaibles de A_7_opcional

MAÍZ_LECRA_MODEL <- MAÍZ_LECRA_MODEL %>% mutate(Condición_P = A_7 == 1 | A_7_opc == 1)
ARROZ_LECRA_MODEL <- ARROZ_LECRA_MODEL %>% mutate(Condición_P=a_7 == 1 | a_7_opc == 1)
MAÍZ_CAS_MODEL <- MAÍZ_CAS_MODEL %>% mutate(Condición_P= a7==1)
ARROZ_CAS_MODEL <- ARROZ_CAS_MODEL %>% mutate(Condición_P= a7==4)

#-- Cargar bases de MAÍZ Y ARROZ UNIDO

ARROZ_UNIDO <- read_dta("Bases_Finales_Unidas/ARROZ_UNIDO.dta")
MAIZ_UNIDO <- read_dta("Bases_Finales_Unidas/MAIZ_UNIDO.dta")

# ----------------------------------- #
#  Crear variables y estandarización  #
# ----------------------------------- #


# --- ESTANDARIZAR DEPARTAMENTO

ARROZ_UNIDO <- ARROZ_UNIDO %>%mutate(depto = as.character(haven::as_factor(A1)),depto = ifelse(depto == "CORDOBA", "CÓRDOBA", depto))
MAIZ_UNIDO  <- MAIZ_UNIDO  %>% mutate(depto = as.character(haven::as_factor(A1)))

# Crear diccionario de etiuqetas
dicc <- tibble(depto = unique(c(ARROZ_UNIDO$depto, MAIZ_UNIDO$depto))) %>%arrange(depto) %>%mutate(cod = row_number())

# Aplicar nuevos numeros y etiquetas
ARROZ_UNIDO <- ARROZ_UNIDO %>%left_join(dicc, by = "depto") %>%mutate(cod = as.integer(cod),A1_std = labelled(cod, labels = setNames(dicc$cod, dicc$depto)))
MAIZ_UNIDO <- MAIZ_UNIDO %>%left_join(dicc, by = "depto") %>%mutate(cod = as.integer(cod),A1_std = labelled(cod, labels = setNames(dicc$cod, dicc$depto)))


# --- SE CREARN VARIABLES CATEGÓRICAS DE ESCOLARIDAD SEGÚN CATEGORÍAS DE DIANA:
# Se dejan categorías: 1 "Primary or less"  2 "Secondary" 3 "Tertiary"

# LECRA:
MAÍZ_LECRA_MODEL <- MAÍZ_LECRA_MODEL %>% mutate(Escolaridad_P_Cat = case_when(Edad_P <= 5 ~ 1,Edad_P >= 6 & Edad_P <= 12 ~ 2,Edad_P >= 13 ~ 3,TRUE ~ NA_real_))
ARROZ_LECRA_MODEL <- ARROZ_LECRA_MODEL %>% mutate(Escolaridad_P_Cat = case_when(Edad_P <= 5 ~ 1,Edad_P >= 6 & Edad_P <= 12 ~ 2,Edad_P >= 13 ~ 3,TRUE ~ NA_real_))

# CAS:
MAÍZ_CAS_MODEL <- MAÍZ_CAS_MODEL %>%mutate(Escolaridad_P_Cat = case_when(Escolaridad_P_Cat %in% c(1, 2, 3) ~ 1,Escolaridad_P_Cat %in% c(4, 5) ~ 2,
Escolaridad_P_Cat %in% c(6, 7, 8) ~ 3,TRUE ~ NA_real_))

ARROZ_CAS_MODEL <- ARROZ_CAS_MODEL %>%mutate(Escolaridad_P_Cat = case_when(Escolaridad_P_Cat %in% c(1, 4) ~ 1,Escolaridad_P_Cat %in% c(2, 3) ~ 2,
Escolaridad_P_Cat %in% c(6, 7, 8) ~ 3,TRUE ~ NA_real_))

# --- SE ESTANDARIZAN LAS CATEGORÍAS DE ETNIA EN LOS 4 DFS

# LECRA:

MAÍZ_LECRA_MODEL <- MAÍZ_LECRA_MODEL %>% mutate(Etnia_P = case_when(Etnia_P_A == "Mestizo/a" ~ 4,Etnia_P_A == "Blanco/a" ~ 5,Etnia_P_A == "No sabe" ~ NA,TRUE ~ Etnia_P))
ARROZ_LECRA_MODEL <- ARROZ_LECRA_MODEL %>% mutate(Etnia_P = case_when(Etnia_P_A == "Mestizo/a" ~ 4,Etnia_P_A == "Blanco/a" ~ 5,Etnia_P_A == "No sabe" ~ NA,TRUE ~ Etnia_P))

# CAS:

MAÍZ_CAS_MODEL <- MAÍZ_CAS_MODEL %>% mutate(Etnia_P = case_when(Etnia_P == 1 ~ 1,Etnia_P == 2 ~ 4,Etnia_P %in% c(3, 4) ~ 2,Etnia_P == 5 ~ 3,Etnia_P == 6 ~ 99,TRUE ~ Etnia_P))
ARROZ_CAS_MODEL <- ARROZ_CAS_MODEL %>% mutate(Etnia_P = case_when(Etnia_P %in% c(1, 4) ~ 2,Etnia_P == 2 ~ 1,Etnia_P == 5 ~ 3,Etnia_P == 3 ~ 4,TRUE ~ Etnia_P))

# --- CREAR VARIABLE DE SIEMBRA MECANIZADA

ARROZ_UNIDO <- ARROZ_UNIDO %>% mutate(F7_M = if_else(F7 %in% c(1, 6), 1, 0))
MAIZ_UNIDO <- MAIZ_UNIDO %>% mutate(F7_M = if_else(F7 %in% c(2, 3), 1, 0))


# --- CERTIFICACIÓN DE SEMILLA COMO DICOTÓMICA

MAIZ_UNIDO <- MAIZ_UNIDO %>%mutate(F11 = if_else(F11 == 2, 0, F11))
ARROZ_UNIDO <- ARROZ_UNIDO %>%mutate(F11 = if_else(F11 == 1, 1, 0))


# ----------------------------------- #
#    Seleccionar variables a unir     #
# ----------------------------------- #


# ---- Selecionar varaibles de df unidos: "F14", "F15", "F16", "F17",

Variables_DF_Unidos <- c("ID_Productor", "A1_std", "F2", "A12", "M1", "D1", "F2", "F11", "F12", "F18", "N1_A", "N1_B", "N1_C", "N1_D", "N1_E", 
"N2_A", "N2_B", "N2_C", "N2_D", "N2_E", "N2_F", "N2_G", "N2_H", "C1_GENDER", "C2_GENDER", "C3_GENDER", "C4_GENDER", "C5_GENDER", "C6_GENDER","C7_GENDER", "C8_GENDER", "C9_GENDER", "C10_GENDER", "C11_GENDER", 
"C12_GENDER", "C13_GENDER", "C14_GENDER", "C15_GENDER","L1", "L2", "L3", "K1", "K2", "E1", "E2", "E1_A", "E1_B", "E1_D", "E1_E", "E1_F", "P1", "P3", "P4", "P6","Adop_Pronósticos","J4","Origen_Datos","F7_M","P5","F5")

MAIZ_UNIDO_F <-  MAIZ_UNIDO %>% select(Variables_DF_Unidos)
ARROZ_UNIDO_F <-  ARROZ_UNIDO %>% select(Variables_DF_Unidos)


# ---- Selecionar varaibles de df individuales

# Orden: 
# Área cosechada en HA	F14
# Cosecha en TON	F15
# Rendimiento en TON/HA	F16
# Cantidad vendida en TON	F17


Variables_DF_Separados <- c("ID_Productor","Condición_P","Edad_P","Genero_P","Escolaridad_P","Etnia_P","Etnia_P_A","HH_size","Num_15","Num_15_28","Num_Adultos",
"Num_Adultos_Mayores","share_15_28","share_60plus","Educación_Hogar","Irrigation_system","org_member","received_training","variety_age_group","Origen_Datos","Escolaridad_P_Cat",
"Distancia_Final","Distancia_via")
 
MAÍZ_LECRA_MODEL_F <- MAÍZ_LECRA_MODEL %>% select(Variables_DF_Separados,AREA_COSECHADA_HA_F21,COSECHA_TON,RENDIMIENTO_TON_HA,Venta_TON)
MAÍZ_CAS_MODEL_F <- MAÍZ_CAS_MODEL %>% select(Variables_DF_Separados,Maize_area_main_plot_harvested,Cantidad_TON,Yield,Venta_TON)
ARROZ_LECRA_MODEL_F <- ARROZ_LECRA_MODEL %>% select(Variables_DF_Separados,f_19,f20_cosecha_ton,rendimiento_ton_ha,f_22_ton)
ARROZ_CAS_MODEL_F <- ARROZ_CAS_MODEL %>% select(Variables_DF_Separados,y4_ha,y5_ton,rendimiento_ton_ha,y9_toneladas)


# -- Cambiar el nombre de las últimas 4:

# Función para renombrar las últimas 4
renombrar_ultimas_4 <- function(df){ultimas <- tail(names(df), 4);names(df)[ (ncol(df)-3):ncol(df) ] <- c("F14", "F15", "F16", "F17");return(df)}

# Aplicar a todos los df
MAÍZ_LECRA_MODEL_F  <- renombrar_ultimas_4(MAÍZ_LECRA_MODEL_F)
MAÍZ_CAS_MODEL_F    <- renombrar_ultimas_4(MAÍZ_CAS_MODEL_F)
ARROZ_LECRA_MODEL_F <- renombrar_ultimas_4(ARROZ_LECRA_MODEL_F)
ARROZ_CAS_MODEL_F   <- renombrar_ultimas_4(ARROZ_CAS_MODEL_F)

# ----------------------------------- #
#           Crear df unidos           #
# ----------------------------------- #

# Unir dfs
Dfs_SEPARADOS <- bind_rows(MAÍZ_LECRA_MODEL_F,MAÍZ_CAS_MODEL_F,ARROZ_LECRA_MODEL_F,ARROZ_CAS_MODEL_F)
Dfs_UNIDOS <- bind_rows(MAIZ_UNIDO_F,ARROZ_UNIDO_F)

# Crear df final
DF_Final <- Dfs_UNIDOS %>% left_join(Dfs_SEPARADOS,by = c("ID_Productor", "Origen_Datos"))


# -------------------------------------------------------- #
#     Crear otras varaibles para el  df ya unido           #
# -------------------------------------------------------- #


# --- SE CREAN VARIABLES DE REGIÓN SEGÚN CLASIFICACIONES DE DIANA:

# 1: Córdoba, BOLIVAR,CESAR,LA GUAJIRA,SUCRE
# 2: Meta,ARAUCA,CASANARE,
# 3: Tolima,Quindío, Risaralda, Valle del Cauca,ANTIOQUIA,HUILA,NORTE DE SANTANDER

DF_Final <- DF_Final %>%mutate(Región = case_when(A1_std %in% c(3, 5, 6, 8, 13) ~ 1,A1_std %in% c(2, 4, 9) ~ 2,A1_std %in% c(1, 7, 10, 11, 12, 14, 15) ~ 3,  TRUE ~ NA_real_))

# --- SE CREA VARAIBLE DE TASA DE DEPENDENCIA (# P menores de 15 o mayores de 60 / # Personas entre 15 y 60)
DF_Final <- DF_Final %>% mutate(Tasa_DP = (Num_15 + Num_Adultos_Mayores) / (Num_15_28 + Num_Adultos))

# --- ORGANIZAR DF FINAL

Variables_F <- c("ID_Productor","A1_std","Región","Condición_P","Edad_P","Genero_P","Escolaridad_P","Escolaridad_P_Cat","Etnia_P","Etnia_P_A","F2","HH_size","Num_15","Num_15_28","Num_Adultos",
"Num_Adultos_Mayores","share_15_28","share_60plus","Educación_Hogar","A12","M1","Tasa_DP","D1","F2","F5","F7_M","F11","F12","F14","F15","F16","F17","F18","Irrigation_system",
"N1_A","N1_B","N1_C","N1_D","N1_E","N2_A","N2_B","N2_C","N2_D","N2_E","N2_F","N2_G","N2_H","C1_GENDER","C2_GENDER","C3_GENDER","C4_GENDER","C5_GENDER","C6_GENDER","C7_GENDER",
"C8_GENDER","C9_GENDER","C10_GENDER","C11_GENDER","C12_GENDER","C13_GENDER","C14_GENDER","C15_GENDER","L1","L2","L3","K1","K2","org_member","received_training","Adop_Pronósticos",
"J4","E1","E2","E1_A","E1_B","E1_D","E1_E","E1_F","P1","P3","P4","P5","P6","variety_age_group","Distancia_Final","Distancia_via","Origen_Datos")

DF_Final <- DF_Final %>% select(Variables_F)

# -------------------------------------------------------- #
#           Revisión de NA que pueden ser = 0              #
# -------------------------------------------------------- #

# Revisar NA por variables
#miss(DF_Final)

# A12: ¿Cuánto se demora en llegar a esa finca?
# Está condicionada a si la finca y casa están separadas, entonces si están juntas las dejo como = 0 en CÓDIGO DE UNIFICACIÓN.


# E1_i: Los Na por eventos adversos se dejan com = 0 aun si no presentó ningún

DF_Final <- DF_Final %>% mutate(
across(c(E1_A, E1_B, E1_D, E1_E, E1_F),~ if_else(E1 == 0, 0, .x)))

# E2: Recordar en CAS no estan condicionada y en LECRA SÍ

# Pasar E2 = 0 si es NA
DF_Final <- DF_Final %>%mutate(E2 = ifelse(is.na(E2), 0, E2))

# -------------------------------------------------------- #
#           Labels y etiquetas de varaibles                #
# -------------------------------------------------------- #

# --- ETIQUETAS

labels_DF <- c(
  ID_Productor = "ID del productor (Key)",
  A1_std = "Departamento",
  Región = "Región",
  Edad_P = "Edad del productor",
  Condición_P="Indica si es productor",
  Genero_P = "Género del productor",
  Escolaridad_P = "Escolaridad del productor",
  Escolaridad_P_Cat = "Escolaridad del productor",
  Etnia_P = "Etnia del productor",
  Etnia_P_A = "Otra [Etnia del productor]",
  F2 = "Experiencia del productor",
  F5="Área sembrada en HA",
  HH_size = "Tamaño del hogar",
  Num_15 = "Personas < 15 años",
  Num_15_28 = "Número de personas entre 15 y 28 años en el hogar",
  Num_Adultos = "Número de personas entre 29 y 60 años en el hogar",
  Num_Adultos_Mayores = "Número de personas mayores de 60 años en el hogar",
  share_15_28 = "% del hogar de 15–28 años",
  share_60plus = "% del hogar con 60+ años",
  Educación_Hogar = "Años promedio de educación del hogar",
  A12 = "¿Cuánto se demora en llegar a esa finca?",
  M1 = "Migración",
  Tasa_DP = "Tasa de dependencia",
  D1 = "Área total de la finca en hectáreas (ha)",
  F7_M = "Siembra mecanizada",
  F11 = "Semilla certificada",
  F12 = "Pérdida de cosecha",
  F14 = "Área cosechada en hectáreas (ha)",
  F15 = "Cosecha en toneladas",
  F16 = "Rendimiento",
  F17 = "Venta (TON)",
  F18 = "Precio de venta por tonelada",
  Irrigation_system = "Riego",
  N1_A = "¿Tiene bomba de agua?",
  N1_B = "¿Tiene reservorio?",
  N1_C = "¿Tiene tractor?",
  N1_D = "¿Tiene moto?",
  N1_E = "¿Tiene automóvil?",
  N2_A = "¿Tiene televisión?",
  N2_B = "¿Tiene celular inteligente?",
  N2_C = "¿Tiene computador?",
  N2_D = "¿Tiene radio/equipo de sonido?",
  N2_E = "¿Tiene servicio sanitario/inodoro?",
  N2_F = "¿Tiene servicio de agua potable?",
  N2_G = "¿Tiene electricidad?",
  N2_H = "¿Tiene acceso a internet?",
  C1_GENDER = "Quién toma la decisión en: Selección de lote para siembra",
  C2_GENDER = "Quién toma la decisión en: Preparación de terreno",
  C3_GENDER = "Quién toma la decisión en: Fecha de siembra",
  C4_GENDER = "Quién toma la decisión en: Manejo agronómico del cultivo (en general)",
  C5_GENDER = "Quién toma la decisión en: Fecha de cosecha",
  C6_GENDER = "Quién toma la decisión en: Adopción de nuevas prácticas y tecnología",
  C7_GENDER = "Quién toma la decisión en: Alquiler y/o adquisición de nueva maquinaria",
  C8_GENDER = "Quién toma la decisión en: Asistencia a capacitaciones",
  C9_GENDER = "Quién toma la decisión en: Solicitar asistencia técnica",
  C10_GENDER = "Quién toma la decisión en: Comercialización y/o venta",
  C11_GENDER = "Quién toma la decisión en: Uso de ingresos por ventas de arroz",
  C12_GENDER = "Quién toma la decisión en: Uso de otros ingresos del hogar",
  C13_GENDER = "Quién toma la decisión en: Tipo de alimentos que se consumen en el hogar",
  C14_GENDER = "Quién toma la decisión en: Educación de los niños",
  C15_GENDER = "Quién toma la decisión en: Solicitud de financiamiento o crédito",
  L1 = "Crédito",
  L2 = "¿Le aprobaron el préstamo?",
  L3 = "¿Qué porcentaje del crédito lo usó para su cultivo?",
  K1 = "Otros ingresos",
  K2 = "Número de ingresos extra recibidos por el hogar",
  org_member = "Miembro de alguna organización",
  received_training = "Recibió apacitaciones",
  Adop_Pronósticos = "Adopta pronósticos",
  J4 = "Asistió a (MTA)",
  E1 = "Afectaciones",
  E2 = "Estrategias",
  E1_A = "Inundaciones",
  E1_B = "Sequías",
  E1_D = "Lluvias irregulares",
  E1_E = "Vientos fuertes",
  E1_F = "Incendios o quemas",
  P1 = "Plagas y/o enfermedades ",
  P3 = "Químicos para maleza",
  P4 = "¿Cuántas veces controló maleza con insumos?",
  P6 = "¿Cuántas veces abonó/fertilizó?",
  variety_age_group = "Nueva variedad según (WAVA threshold)",
  Distancia_Final = "Distancia del productor a estación [KM]",
  Distancia_via = "Distancia del productor a vía tipo 1 o 3 [KM]"
  )

# --- LABELS


# E2
DF_Final <- DF_Final %>% mutate(across(c(E2),~ labelled(.x, labels = c("Sí"=1,"No"=0))))

# Región
DF_Final <- DF_Final %>% mutate(across(c(Región),~ labelled(.x, labels = c("Caribe"=1,"Llanos"=2,"Andina"=3))))

# Escolaridad_P_Cat
DF_Final <- DF_Final %>% mutate(across(c(Escolaridad_P_Cat),~ labelled(.x, labels = c("Primaria o menos"=1,"Secundaria"=2,"Terciaría"=3))))

# F7_M
DF_Final <- DF_Final %>% mutate(across(c(F7_M),~ labelled(.x, labels = c("Sí"=1,"No"=0))))

# F11
DF_Final <- DF_Final %>% mutate(across(c(F11),~ labelled(.x, labels = c("Sí"=1,"No"=0))))

# Otras que estaban en inglés
DF_Final <- DF_Final %>% mutate(across(c(Irrigation_system),~ labelled(.x, labels = c("Sí"=1,"No"=0))))
DF_Final <- DF_Final %>% mutate(across(c(org_member),~ labelled(.x, labels = c("Sí"=1,"No"=0))))
DF_Final <- DF_Final %>% mutate(across(c(received_training),~ labelled(.x, labels = c("Sí"=1,"No"=0))))


# Etnia_P
DF_Final <- DF_Final %>% mutate(across(c(Etnia_P),~ labelled(.x,labels = c(
"Indígena" = 1,"Negro(a), mulato(a), afrodescendiente, afrocolombiano(a)" = 2,"Ningún grupo étnico" = 3,"Mestizo" = 4,"Blanco" = 5,"Otro, especifique" = 99))))

for (var in names(labels_DF)) {if (var %in% names(DF_Final)) {labelled::var_label(DF_Final[[var]]) <- labels_DF[var]}}

write_dta(DF_Final, "TDG/Base_unida [ARROZ Y MAÍZ PARA TDG].dta")








