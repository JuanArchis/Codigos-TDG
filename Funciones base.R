# -------------------------------------------------------------------------------------#
#        Fuciones base: Funciones creadas que facilitan los reportes para CIAT         #
# -------------------------------------------------------------------------------------#


# -----------------------------------------------#
#      Función para instalar librerias           #
# -----------------------------------------------#


# Instalar pacman si no está instalado
if (!require("pacman")) install.packages("pacman")
# Cargar todas las librerías con pacman
pacman::p_load(
ggplot2, reshape2,shapviz, ggbeeswarm,forcats, patchwork,xtable, RM.weights,tidytext,sysfonts, purrr,ggspatial, lessR, webr, haven, dplyr, tidyr, homals, FactoMineR, moments, car, stringr, fuzzyjoin,sf,rnaturalearthdata,rnaturalearth, 
stringdist, writexl, readxl, tibble, latex2exp, RColorBrewer, gridExtra, grid,psych,scales,ggpie, openxlsx, ggh4x, Gifi, Cairo,hunspell,moments,devEMF,ggrepel,showtext,extrafont,patchwork,ranger,isotone,Matrix,recipes,
readr,psfmi,readstata13,lubridate,labelled,sjlabelled,geosphere,pROC,pscl,ResourceSelection,boot,ggtext,xfun,caret,glmnet,rms,riskRegression,rpart,mice,naniar,VIM,Hmisc,finalfit,kableExtra,treeshap,fastshap,
)


# -----------------------------------------------#
#      Función para identificar atípicos         #
# -----------------------------------------------#


detectar_atipicos <- function(df, key_var, num_var, remover_atipicos = FALSE, limites_personalizados = NULL, excluir_ceros = FALSE) {
  
  # Filtrar valores si excluir_ceros es TRUE
  df_filtrado <- if (excluir_ceros) df %>% filter(.data[[num_var]] != 0) else df
  
  # Calcular estadísticas básicas
  stats <- df_filtrado %>%
    summarise(
      Media = mean(.data[[num_var]], na.rm = TRUE),
      Mediana = median(.data[[num_var]], na.rm = TRUE),
      Minimo = min(.data[[num_var]], na.rm = TRUE),
      Maximo = max(.data[[num_var]], na.rm = TRUE),
      Desviacion = sd(.data[[num_var]], na.rm = TRUE),
      CV = Desviacion / Media,  # Coeficiente de variación
      Asimetria = skewness(.data[[num_var]], na.rm = TRUE),  # Asimetría
      Curtosis = kurtosis(.data[[num_var]], na.rm = TRUE),  # Curtosis
      Q1 = quantile(.data[[num_var]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[num_var]], 0.75, na.rm = TRUE)
    ) %>%
    mutate(
      IQR_val = Q3 - Q1,
      Limite_Inferior = Q1 - 1.5 * IQR_val,
      Limite_Superior = Q3 + 1.5 * IQR_val
    ) %>%
    select(Media, Mediana, Minimo, Maximo, CV, Asimetria, Curtosis, 
           Q1, Q3, IQR_val, Limite_Inferior, Limite_Superior)
  
  # Filtrar valores atípicos
  df_atipicos <- df %>%
    filter(.data[[num_var]] < stats$Limite_Inferior | .data[[num_var]] > stats$Limite_Superior) %>%
    select(all_of(key_var), all_of(num_var))
  
  # Determinar límites a usar
  if (!is.null(limites_personalizados) && length(limites_personalizados) == 2) {
    nuevo_limite_inferior <- limites_personalizados[1]
    nuevo_limite_superior <- limites_personalizados[2]
  } else {
    nuevo_limite_inferior <- stats$Limite_Inferior
    nuevo_limite_superior <- stats$Limite_Superior
  }
  
  # Si remover_atipicos es TRUE, usar los límites seleccionados para asignar NA
  if (remover_atipicos) {
    df <- df %>%
      mutate(!!num_var := ifelse(.data[[num_var]] < nuevo_limite_inferior | .data[[num_var]] > nuevo_limite_superior, NA, .data[[num_var]]))
    
    # Recalcular estadísticas después de eliminar/modificar los valores atípicos
    stats_sin_atipicos <- df %>%
      summarise(
        Media = mean(.data[[num_var]], na.rm = TRUE),
        Mediana = median(.data[[num_var]], na.rm = TRUE),
        Minimo = min(.data[[num_var]], na.rm = TRUE),
        Maximo = max(.data[[num_var]], na.rm = TRUE),
        Desviacion = sd(.data[[num_var]], na.rm = TRUE),
        CV = Desviacion / Media,  # Coeficiente de variación
        Asimetria = skewness(.data[[num_var]], na.rm = TRUE),  # Asimetría
        Curtosis = kurtosis(.data[[num_var]], na.rm = TRUE),  # Curtosis
        Q1 = quantile(.data[[num_var]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[num_var]], 0.75, na.rm = TRUE)
      )
  } else {
    stats_sin_atipicos <- NULL
  }
  
  # Devolver lista con valores atípicos, resumen y, si aplica, el dataframe modificado y su resumen
  return(list(
    atipicos = df_atipicos,
    resumen = stats,
    datos_modificados = if (remover_atipicos) df else NULL,
    resumen_sin_atipicos = stats_sin_atipicos
  ))
}


# -----------------------------------------------#
#      Función para identificar NAS              #
# -----------------------------------------------#



miss <- function(Datos, plot = TRUE, id_var = NULL){  
  
  # Si se especifica un id_var, lo guardo y lo quito temporalmente del análisis
  if(!is.null(id_var)){
    if(!(id_var %in% names(Datos))){
      stop("El id_var no existe en el dataset.")
    }
    ID <- Datos[[id_var]]
    Datos_noID <- Datos[ , setdiff(names(Datos), id_var), drop = FALSE]
  } else {
    ID <- NULL
    Datos_noID <- Datos
  }
  
  n  <- nrow(Datos_noID)
  p  <- ncol(Datos_noID)
  names.obs <- rownames(Datos_noID)
  
  # Cálculo de faltantes
  nobs.comp <- sum(complete.cases(Datos_noID))
  Obs.comp  <- which(complete.cases(Datos_noID))
  nobs.miss <- sum(!complete.cases(Datos_noID))
  Obs.miss  <- which(!complete.cases(Datos_noID))
  
  Datos.NA <- is.na(Datos_noID)
  Var_Num  <- sort(colSums(Datos.NA), decreasing = TRUE)
  Var_per  <- round(Var_Num/n, 3)
  
  Obs_Num  <- rowSums(Datos.NA)
  names(Obs_Num) <- names.obs
  Obs_Num  <- sort(Obs_Num, decreasing = TRUE)
  Obs_per  <- round(Obs_Num/p, 3)
  
  # --- NUEVO: salida opcional ---
  if(!is.null(ID)){
    Reporte_por_ID <- data.frame(
      ID = ID,
      Porcentaje_NA = round(rowSums(Datos.NA)/p, 3),
      N_NA=rowSums(Datos.NA)
    )
  } else {
    Reporte_por_ID <- NULL
  }
  
  # Lista de salida
  lista <- list(
    n.row = n,
    n.col = p,
    n.comp = nobs.comp,
    Obs.comp = Obs.comp,
    n.miss = nobs.miss,
    Obs.miss = Obs.miss,
    Var.n = Var_Num,
    Var.p = Var_per,
    Obs.n = Obs_Num,
    Obs.per = Obs_per,
    Reporte_ID = Reporte_por_ID  # <-- añadido
  )
  
  # Gráficos
  if(plot){
    windows(height = 10, width = 15)
    par(mfrow = c(1, 2))
    
    coord <- barplot(Var_per, plot = FALSE)
    barplot(Var_per, xaxt="n", horiz=TRUE, yaxt="n",
            xlim=c(-0.2,1), ylim=c(0,max(coord)+1),
            main="% datos faltantes por variable")
    axis(2, at=coord, labels=names(Var_per), cex.axis=0.5, pos=0, las=2)
    axis(1, seq(0,1,0.2), seq(0,1,0.2), pos=0)
    
    coord <- barplot(Obs_per, plot = FALSE)
    barplot(Obs_per, xaxt="n", horiz=TRUE, yaxt="n",
            xlim=c(-0.2,1), ylim=c(0,max(coord)+1),
            main="% datos faltantes por registro")
    axis(2, at=coord, labels=names(Obs_per), cex.axis=0.5, pos=0, las=2)
    axis(1, seq(0,1,0.2), seq(0,1,0.2))
  }
  
  return(invisible(lista))
}






# Función para encontrar valores atípicos
find_outliers <- function(df, var_name) {
  # Calcular el rango intercuartil (IQR)
  Q1 <- quantile(df[[var_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[var_name]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  # Definir los límites para los valores atípicos
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Filtrar los valores atípicos
  outliers <- df %>%
    filter(df[[var_name]] < lower_bound | df[[var_name]] > upper_bound) %>%
    select(var_name)
  
  return(outliers)
}


find_outliers_by_category <- function(df, var_name, category_name) {
  
  # Asegurarse de que las variables sean columnas del dataframe
  var_name <- rlang::ensym(var_name)
  category_name <- rlang::ensym(category_name)
  
  # Aplicar el método IQR por cada categoría
  outliers <- df %>%
    group_by(!!category_name) %>%
    mutate(
      Q1 = quantile(!!var_name, 0.25, na.rm = TRUE),
      Q3 = quantile(!!var_name, 0.75, na.rm = TRUE),
      IQR_value = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR_value,
      upper_bound = Q3 + 1.5 * IQR_value
    ) %>%
    filter(!!var_name < lower_bound | !!var_name > upper_bound) %>%
    select(!!category_name, !!var_name) %>%
    distinct()
  
  return(outliers)
}


evaluate_category <- function(data, categoria, column_prefix) {
  letras <- strsplit(categoria, NULL)[[1]]  # Divide la cadena en letras individuales
  
  for (letra in letras) {
    column_name <- paste0(column_prefix, "_", letra)
    
    data <- data %>%
      mutate(!!column_name := as.numeric(grepl(letra, !!sym(column_prefix))))
  }
  
  return(data)
}



evaluate_category_exact <- function(data, categoria, column_prefix) {
  # Convertir la categoría en un vector de valores únicos (si hay múltiples valores separados por comas o espacios)
  valores <- unlist(strsplit(categoria, split = "[, ]+")) # Separa por coma o espacio
  
  # Iterar sobre cada valor en la lista de valores
  for (valor in valores) {
    # Construir el nombre de la columna basado en el prefijo y el valor
    column_name <- paste0(column_prefix, "_", valor)
    
    # Crear una nueva columna para cada valor
    data <- data %>%
      mutate(!!column_name := as.numeric(!!sym(column_prefix) == valor)) %>% # 1 si coincide, 0 si no
      group_by(A01) %>%
      mutate(!!column_name := ifelse(any(!!sym(column_name) == 1), 1, 0)) %>%
      ungroup()
  }
  
  return(data)
}



# -----------------------------------------------#
#      Función para crear medias de tablas      #
# ----------------------------------------------#


# Función para generar la tabla de medias, transponerla y exportarla a Excel
generate_summary_table_transposed <- function(data, vars, group_var, output_file) {
  # Calcular las medias por el grupo especificado (en este caso group_var)
  summary_table <- data %>%
    group_by({{group_var}}) %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"), .groups = 'drop')
  
  # Asegurarse de que la variable de agrupación sea de tipo character
  summary_table <- summary_table %>%
    mutate({{group_var}} := as.character({{group_var}}))
  
  # Calcular las medias generales (sin agrupar)
  global_means <- data %>%
    summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}")) %>%
    mutate({{group_var}} := "Total")  # Añadir una fila para el total sin distinguir el grupo
  
  # Asegurarse de que la variable de agrupación sea de tipo character en el total
  global_means <- global_means %>%
    mutate({{group_var}} := as.character({{group_var}}))
  
  # Combinar ambos resultados
  result_table <- bind_rows(summary_table, global_means)
  
  # Transponer la tabla para que las medias sean filas
  result_table_transposed <- result_table %>%
    pivot_longer(cols = starts_with("mean_"), 
                 names_to = "variable", 
                 values_to = "mean_value") %>%
    pivot_wider(names_from = {{group_var}}, values_from = mean_value)
  
  # Exportar la tabla transpuesta a un archivo Excel
  write_xlsx(result_table_transposed, output_file)
  
  return(result_table_transposed)
}



#--------------------------------------------------------------------------#
# Función para distintigir los ID que se repiten pero son idf productores  #
#--------------------------------------------------------------------------#


ajustar_repetidos <- function(df, columna, valores) {
  df <- df %>% 
    group_by(!!sym(columna)) %>% 
    mutate(!!sym(columna) := ifelse((!!sym(columna) %in% valores) & row_number() > 1, 
                                    paste0(!!sym(columna), "_1"), 
                                    !!sym(columna))) %>%
    ungroup()
  return(df)
}



#--------------------------------------------------------------------------#
#                         Función del ®áfico perzonalizados                #
#--------------------------------------------------------------------------#

theme_mine <- function(base_size = 10, base_family = "Times") {
  # Starts with theme_grey and then modify some parts
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 13),
      strip.text.y = element_text(size = 13,color = "#3b4144"),
      axis.text.x = element_text(size=13),
      axis.text.y = element_text(size=13,hjust=1),
      axis.ticks =  element_line(colour = "black"), 
      axis.title.x= element_text(size=15,margin = margin(r = 10)),
      axis.title.y= element_text(size=15,angle=90,margin = margin(r = 10)),
      panel.background = element_blank(), 
      panel.border =element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.margin = unit(0.5, "lines"), 
      plot.background = element_blank(), 
      plot.margin = unit(c(2, 2, 2, 2), "lines"),
      axis.line.x = element_line(color="black", size = 0.6),
      axis.line.y = element_line(color="black", size = 0.6),
      legend.title = element_text(size = 13), # Tamaño del título de la leyenda
      legend.text = element_text(size = 13)  # Tamaño de las etiquetas de la leyenda
    )
}



#--------------------------------------------------------------------------#
#       FUNCIÓN BASE PARA CREAR TABLA E NEL INFORME FINAL COPARATIVO       #
#--------------------------------------------------------------------------#


generar_tabla_resumen_simple <- function(df, vars_promedio, vars_porcentaje, 
                                         var_periodo, diccionario, output_path,
                                         var_secundaria = NULL) {
  
  # Crear columna con var_periodo y var_secundaria (si existe)
  if (!is.null(var_secundaria)) {
    df <- df %>%
      mutate(Periodo = !!sym(var_periodo),
             Secundaria = !!sym(var_secundaria))
  } else {
    df <- df %>%
      mutate(Periodo = !!sym(var_periodo))
  }
  
  # Variables para group_by según si hay variable secundaria
  grupos <- if (!is.null(var_secundaria)) c("Periodo", "Secundaria") else "Periodo"
  
  # ------------------------- #
  # CALCULAR N POR PARTICIÓN  #
  # ------------------------- #
  tabla_n <- df %>%
    group_by(across(all_of(grupos))) %>%
    summarise(valor = n(), .groups = "drop") %>%
    mutate(variable = "n")  # etiqueta fija para conteo
  
  # ------------------------- #
  #         PROMEDIOS         #
  # ------------------------- #
  tabla_promedios <- df %>%
    summarise(across(all_of(vars_promedio), mean, na.rm = TRUE, .names = "{.col}"),
              .by = all_of(grupos)) %>%
    pivot_longer(-all_of(grupos), names_to = "variable", values_to = "valor")
  
  # ------------------------- #
  #       PORCENTAJES         #
  # ------------------------- #
  tabla_porcentajes <- lapply(vars_porcentaje, function(var) {
    
    df %>%
      mutate(valor_cat = haven::as_factor(.data[[var]])) %>%
      filter(!is.na(valor_cat)) %>%
      count(across(all_of(grupos)), valor_cat) %>%
      group_by(across(all_of(grupos))) %>%
      mutate(valor = n / sum(n)) %>%
      ungroup() %>%
      mutate(variable = paste0(var, " - ", valor_cat)) %>%
      select(all_of(grupos), variable, valor)
    
  }) %>% bind_rows()
  
  # ------------------------- #
  # Unir todo (incluyendo n)  #
  # ------------------------- #
  tabla_final <- bind_rows(tabla_n, tabla_promedios, tabla_porcentajes)
  
  # Pivotar: hacer nombres con var_periodo y var_secundaria si existe
  if (!is.null(var_secundaria)) {
    tabla_final <- tabla_final %>%
      unite("Periodo_Secundaria", all_of(grupos), sep = " | ") %>%
      pivot_wider(names_from = Periodo_Secundaria, values_from = valor)
  } else {
    tabla_final <- tabla_final %>%
      pivot_wider(names_from = Periodo, values_from = valor)
  }
  
  # -------------------------------------------------- #
  # Reemplazar nombres por diccionario (excepto "n")   #
  # -------------------------------------------------- #
  tabla_final <- tabla_final %>%
    rowwise() %>%
    mutate(variable = if (variable != "n") {
      base_var <- strsplit(variable, " - ")[[1]][1]
      etiqueta <- diccionario$descripcion[diccionario$variable == base_var]
      if (grepl(" - ", variable)) {
        categoria <- sub(".* - ", "", variable)
        paste0(etiqueta, " - ", categoria)
      } else {
        etiqueta
      }
    } else {
      "n"
    }) %>%
    ungroup()
  
  # Guardar en Excel
  if (!is.null(output_path)) {
    write_xlsx(tabla_final, output_path)
  }
  
  return(tabla_final)
}


# --------------------------------------- #
# Función para procesa eventos y medidas  #
# --------------------------------------- #


procesar_eventos <- function(df, cols, año, origen) {
  df %>%
    pivot_longer(cols = all_of(cols),
                 names_to = "Variable",
                 values_to = "Porcentaje") %>%
    group_by(Variable) %>%
    summarise(Media = mean(Porcentaje, na.rm = TRUE) * 100,
              .groups = "drop") %>%
    mutate(Año = año,
           Origen_Datos = origen)
}


procesar_adaptacion <- function(df, cols, año, origen) {
  df %>%
    summarise(across(all_of(cols), ~mean(.x, na.rm = TRUE) )) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Media") %>%
    mutate(Año = año, Origen_Datos = origen)
}



# --------------------------#
# Función para MCA GENÉRICO #
# ------------------------- #


crear_indice_MCA <- function(df, id_var, vars_mca, nombre_indice = "Indice_MCA",
                             invertir = FALSE, path_pesos = NULL) {
  
  # Seleccionar variables
  df_mca <- df %>%
    select(all_of(c(id_var, vars_mca)))
  
  # Quitar la variable ID para el MCA
  df_sin_id <- df_mca %>% select(-all_of(id_var))
  
  # Filtrar casos completos
  completos <- complete.cases(df_sin_id)
  df_completos <- df_sin_id[completos, ] %>%
    mutate(across(everything(), as.factor))
  
  # --- MCA ---
  res.mca <- MCA(df_completos, graph = TRUE)
  
  # Calcular el índice
  coords <- res.mca$var$coord[, 1]
  eig1 <- res.mca$eig[1, 1]
  pesos <- coords / sqrt(eig1)
  
  # Invertir si se solicita
  if (invertir) pesos <- -pesos
  
  # Guardar archivo de pesos SI se especifica path
  if (!is.null(path_pesos)) {
    write.xlsx(as.data.frame(pesos),
               file = path_pesos,
               rowNames = TRUE)
  }
  
  # Matriz disyuntiva
  Z <- res.mca$call$Xtot
  indice_mca <- as.numeric(as.matrix(Z) %*% pesos)
  
  # Normalizar
  indice_norm <- scales::rescale(indice_mca, to = c(0, 1))
  
  # Reconstruir el índice en la base original
  df_mca[[nombre_indice]] <- NA
  df_mca[[nombre_indice]][completos] <- indice_norm
  
  return(df_mca)
}


# ----------------------------------------------------------#
# Función para generar área y % dep roductores por variedad #
# ----------------------------------------------------------#


# --- Función para calcular top por % de productores ---
calc_top <- function(data, n_top = 30, col_name) {
  data %>%
    group_by(Variedad_Total) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate("{col_name}" := n / sum(n) * 100) %>%
    arrange(desc(.data[[col_name]])) %>%
    slice_head(n = n_top) %>%
    select(-n)
}

# --- Función para calcular top por área sembrada ---
calc_top_area <- function(data, var_area, n_top = 30, col_name) {
  data %>%
    group_by(Variedad_Total) %>%
    summarise(area_total = sum(.data[[var_area]], na.rm = TRUE), .groups = "drop") %>%
    mutate("{col_name}" := area_total / sum(area_total) * 100) %>%
    arrange(desc(.data[[col_name]])) %>%
    slice_head(n = n_top) %>%
    select(Variedad_Total, all_of(col_name))
}


# ----------------------------------------------------------#
# Función para generar medias condicionales a una categórica #
# ----------------------------------------------------------#



resumir_media <- function(df, var_grupo, vars, anio) {
  df %>%
    mutate(grupo = haven::as_factor({{ var_grupo }})) %>%
    group_by(grupo) %>%
    summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    bind_rows(
      df %>%
        summarise(across(all_of(vars), ~mean(.x, na.rm = TRUE))) %>%
        mutate(grupo = "General")
    ) %>%
    mutate(Año = anio)
}



# ----------------------------------------------------------#
#           Función para generar conteo de radios           #
# ----------------------------------------------------------#

contar_estaciones_radio <- function(df_productores,
                                    df_estaciones,
                                    id_col_prod,
                                    lat_col_prod,
                                    lon_col_prod,
                                    lat_col_est,
                                    lon_col_est,
                                    radios_km) {
  # --- Convertir estaciones a objeto sf ---
  estaciones_sf <- df_estaciones %>%
    st_as_sf(coords = c(lon_col_est, lat_col_est), crs = 4326) %>%
    st_transform(9377)
  
  # --- Filtrar productores con coordenadas válidas ---
  productores_validos <- df_productores %>%
    filter(!is.na(.data[[lat_col_prod]]) & !is.na(.data[[lon_col_prod]]))
  
  # --- Convertir productores válidos a sf ---
  productores_sf <- productores_validos %>%
    st_as_sf(coords = c(lon_col_prod, lat_col_prod), crs = 4326) %>%
    st_transform(9377)
  
  # --- Crear buffers (por cada radio en km) ---
  buffers <- lapply(radios_km, function(r) st_buffer(productores_sf, dist = r * 1000))
  names(buffers) <- paste0("r", radios_km, "km")
  
  # --- Calcular el número de estaciones dentro de cada buffer ---
  conteos <- tibble(!!id_col_prod := productores_validos[[id_col_prod]])
  
  for (r in radios_km) {
    conteos[[paste0("estaciones_", r, "km")]] <-
      lengths(st_intersects(buffers[[paste0("r", r, "km")]], estaciones_sf))
  }
  
  # --- Devuelve solo ID + columnas de conteos ---
  return(conteos)
}

# --------- FUNCIÓN PARA ORGANIZACIONES (sólo cambia el texto de la variable)
contar_org_radio <- function(df_productores,
                                    df_estaciones,
                                    id_col_prod,
                                    lat_col_prod,
                                    lon_col_prod,
                                    lat_col_est,
                                    lon_col_est,
                                    radios_km) {
  # --- Convertir estaciones a objeto sf ---
  estaciones_sf <- df_estaciones %>%
    st_as_sf(coords = c(lon_col_est, lat_col_est), crs = 4326) %>%
    st_transform(9377)
  
  # --- Filtrar productores con coordenadas válidas ---
  productores_validos <- df_productores %>%
    filter(!is.na(.data[[lat_col_prod]]) & !is.na(.data[[lon_col_prod]]))
  
  # --- Convertir productores válidos a sf ---
  productores_sf <- productores_validos %>%
    st_as_sf(coords = c(lon_col_prod, lat_col_prod), crs = 4326) %>%
    st_transform(9377)
  
  # --- Crear buffers (por cada radio en km) ---
  buffers <- lapply(radios_km, function(r) st_buffer(productores_sf, dist = r * 1000))
  names(buffers) <- paste0("r", radios_km, "km")
  
  # --- Calcular el número de estaciones dentro de cada buffer ---
  conteos <- tibble(!!id_col_prod := productores_validos[[id_col_prod]])
  
  for (r in radios_km) {
    conteos[[paste0("organizaciones_", r, "km")]] <-
      lengths(st_intersects(buffers[[paste0("r", r, "km")]], estaciones_sf))
  }
  
  # --- Devuelve solo ID + columnas de conteos ---
  return(conteos)
}


# ----------------------------------------------------------#
#   Función crear df de importancia con base en un CART     #
# ----------------------------------------------------------#


get_importance_with_labels <- function(rpart_model, data) {
  
  # 1. Importancias del árbol
  imp <- rpart_model$variable.importance
  
  # 2. Extraer labels del dataframe
  labels <- sapply(data, function(x) {
    lab <- attr(x, "label")
    if (is.null(lab)) return(NA)
    if (length(lab) > 1) lab <- lab[1]
    return(as.character(lab))
  })
  
  # 3. Crear tabla final
  df <- data.frame(
    variable   = names(imp),
    label      = labels[names(imp)],
    importance = as.numeric(imp),
    row.names = NULL
  )
  
  # 4. Ordenar por importancia descendente
  df <- df[order(-df$importance), ]
  
  return(df)
}



# ------------------ AGREGAR TIMES ROMAN COMO FUENTE PARA FIGURAS ---------------- # 

font_add("Times", "C:/Windows/Fonts/times.ttf")
font_add("Times Bold", "C:/Windows/Fonts/timesbd.ttf")

showtext_auto()


# ----------------------------------------------------------#
#   Función para procesar datos antes de usar psfmi_lr      #
# ----------------------------------------------------------#


Procesar.cv.imput <- function(df) {
  
  df[] <- lapply(names(df), function(var) {
    
    v <- df[[var]]
    
    if (is.factor(v) || is.character(v)) {
      
      v_char <- as.character(v)
      n_levels <- length(unique(v_char[!is.na(v_char)]))
      
      
      if (var == "Genero_P") {
        return(ifelse(v_char == "Hombre", 1, 0))
      }
      
      if (var == "Cultivo") {
        return(ifelse(v_char == "Maíz", 1, 0))
      }
      
      
      if (n_levels == 2 && all(unique(v_char) %in% c("Sí", "No"))) {
        return(ifelse(v_char == "Sí", 1, 0))
      }
      
      
      if (n_levels > 2) {
        return(as.numeric(factor(v_char)))
      }
      
      
      if (n_levels == 2) {
        return(as.numeric(factor(v_char)) - 1)
      }
      
    }
    
    return(v)
  })
  
  return(as.data.frame(df))
}

# ----------------------------------------------------------#
#   Funcion para crear DF que genera residios parciales     #
# ----------------------------------------------------------#


get_partial_resid_MI <- function(mids_obj, formula_model, varname){
  
  # Extraer datasets imputados
  imp_list <- complete(mids_obj, action = "all")
  nimps <- length(imp_list)
  
  # Iterar sobre imputaciones
  partial_list <- lapply(seq_len(nimps), function(i){
    
    data_i <- imp_list[[i]]
    
    # Ajustar modelo en esa imputación
    mod_i <- glm(formula_model,
                 family = binomial(link = "logit"),
                 data = data_i)
    
    # Extraer coeficiente de la variable
    beta_i <- coef(mod_i)[varname]
    
    if(is.na(beta_i)){
      stop(paste("La variable", varname, "no está en el modelo."))
    }
    
    # Calcular residuo parcial
    partial_resid_i <- residuals(mod_i, type = "working") +
      beta_i * data_i[[varname]]
    
    # Construir df
    data.frame(
      x = data_i[[varname]],
      partial_resid = partial_resid_i,
      imp = i
    )
  })
  
  # Unir todo en formato largo
  partial_long <- do.call(rbind, partial_list)
  
  return(partial_long)
}


# ----------------------------------------------------------#
#   Funcion para crear figuras de residuos parciales         #
# ----------------------------------------------------------#

plot_partial <- function(df, x_label){
  ggplot(df, aes(x = x, y = partial_resid, color = factor(imp))) +
    geom_point(alpha = 1, size = 2) +
    geom_smooth(method = "loess",
                se = FALSE,
                aes(group = imp),
                linewidth = 0.8,
                alpha = 1,
                color = "#6c778d") +
    scale_color_manual(values = green_teal_12) +
    theme_mine(base_family = "Times") +
    theme(
      axis.title = element_text(size = 14),
      axis.text  = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 11)
    ) +
    labs(x = x_label,
         y = "Residuo parcial",
         color = "Imputación:  ")+guides(color = guide_legend(nrow = 1),override.aes = list(size = 4))
}


# ----------------------------------------------------------#
#   Funcion para crear figuras de SHAP X Covaraibles        #
# ----------------------------------------------------------#


plot_dependence <- function(var, shap) {
  
  p <- sv_dependence(
    shap,
    v = var,
    color_var = NULL
  )
  
  # Color
  p$layers[[1]]$aes_params$colour <- "#485966"
  
  p +
    labs(
      x = dic_labels[var],
      y = paste0("SHAP — ", dic_labels[var])
    ) +
    geom_hline(
      yintercept = 0,
      colour = "#888888",
      linewidth = 0.5,
      linetype = "dashed"
    ) +
    theme_mine(base_size = 12, base_family = "Times") +
    theme(
      axis.text.x  = element_text(size = 12),
      axis.text.y  = element_text(size = 12),
      axis.title.x = element_text(size = 13),
      axis.title.y = element_text(size = 13)
    )
}


# ----------------------------------------------------------#
#   Funcion para crear figuras de SHAP categóricas          #
# ----------------------------------------------------------#


plot_shap_cat <- function(var) {df <- data.frame(shap= shap_mat[, var],categoria = as.character(feat_mat[, var]))

etiqueta <- ifelse(var %in% names(dic_labels), dic_labels[var], var)

ggplot(df, aes(x = shap, y = etiqueta, colour = categoria)) +
  geom_beeswarm(
    cex      = 0.8,
    size     = 1.2,
    priority = "density"
  ) +
  scale_colour_manual(
    values = paletas_cat[[var]],
    name   = NULL,
    guide  = guide_legend(
      label.theme = element_text(
        family = "Times", size = 13, colour = "black"
      )
    )
  ) +
  geom_vline(
    xintercept = 0, colour = "#888888",
    linewidth  = 0.5, linetype = "dashed"
  ) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01),
    expand = expansion(mult = 0.05)
  ) +
  labs(x = NULL, y = NULL) +
  theme_mine(base_size = 11, base_family = "Times") +
  theme(
    legend.position = "right",
    axis.text.x     = element_text(size = 11),
    axis.text.y     = element_text(size = 14),
    panel.grid.major.y = element_blank()
  )
}


# ----------------------------------------------------------#
#           FUNCIÓN DE CALIBRACIÓN ISOTÓNICA                #
# ----------------------------------------------------------#

calibrar_isotonica <- function(p_oof, y_num, p_test) {
  
  # Ordenar por probabilidad predicha 
  ord     <- order(p_oof)
  p_ord   <- p_oof[ord]
  y_ord   <- y_num[ord]
  
  # Ajuste isotónico 
  iso_fit <- gpava(p_ord, y_ord)
  
  # Para nuevos puntos: interpolación lineal
  p_cal <- approx(
    x      = p_ord,
    y      = iso_fit$x,       
    xout   = p_test,
    method = "linear",
    rule   = 2                 
  )$y
  
  p_cal <- pmax(0, pmin(1, p_cal))
  
  return(list(p_cal = p_cal, iso_fit = iso_fit, p_oof_ord = p_ord))
}
