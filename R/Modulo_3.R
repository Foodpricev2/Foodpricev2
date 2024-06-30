
Modulo_3 <- function(Data_income_household,Model_CoCA,Model_CoNA,Model_CoRD) {


  #-------------------------------------------------#
  #  Validación de parámetros de la función 3      #
  #-------------------------------------------------#
  validar_parametros <- function(Data_income_household, Model_CoCA, Model_CoNA, Model_CoRD) {

    # Validar Data_income_household
    if (!is.data.frame(Data_income_household)) {
      stop("Data_income_household debe ser un data.frame")
    }

    columnas_esperadas <- c("deciles", "income", "Total_persons_household", "per_capita_income", "food_income_per_capita")
    columnas_faltantes <- setdiff(columnas_esperadas, names(Data_income_household))

    if (length(columnas_faltantes) > 0) {
      stop("Data_income_household le faltan las siguientes columnas: ", paste(columnas_faltantes, collapse = ", "))
    }

    # Validar Model_CoCA
    if (!is.data.frame(Model_CoCA) ||
        !all(c("per_capita_year", "per_capita_month") %in% names(Model_CoCA))) {
      stop("Model_CoCA debe ser un data.frame con las columnas 'per_capita_year' y 'per_capita_month'")
    }

    # Validar Model_CoNA
    if (!is.data.frame(Model_CoNA) ||
        !all(c("per_capita_year", "per_capita_month") %in% names(Model_CoNA))) {
      stop("Model_CoNA debe ser un data.frame con las columnas 'per_capita_year' y 'per_capita_month'")
    }

    # Validar Model_CoRD
    if (!is.data.frame(Model_CoRD) ||
        !all(c("per_capita_year", "per_capita_month") %in% names(Model_CoRD))) {
      stop("Model_CoRD debe ser un data.frame con las columnas 'per_capita_year' y 'per_capita_month'")
    }
  }

  #-------------------------------------------------#
   #                CODIGO                           #
  #-------------------------------------------------#



Sys.sleep(1);cat("Módulo 1: Cálculo de indicadores de asequibilidad")

  deciles_grupos = c("Decil 1", "Decil 2",
                     "Decil 3", "Decil 4",
                     "Decil 5", "Decil 6",
                     "Decil 7", "Decil 8",
                     "Decil 9", "Decil 10")

outcome_1_list = list()
length(outcome_1_list) = 10

z <- as.numeric(levels(as.factor(Model_CoCA$per_capita_year )))

outcome_1_list <- lapply(deciles_grupos, function(decile) {
  # Filtrar Data_income_household una vez para el decil actual
  df_y <- Data_income_household %>% filter(deciles %in% decile)

  # Crear dummy vectorizado
  df_y$dummy <- ifelse(df_y$food_income_per_capita_year < z, 1, 0)

  # Filtrar df_y para obtener solo filas donde dummy es 1
  df_z <- df_y %>% filter(dummy == 1)

  # Calcular brecha relativa y su cuadrado
  df_z$brecha_rel <- (z - df_z$food_income_per_capita_year) / z
  df_z$brecha_rel_sqr <- df_z$brecha_rel^2

  # Calcular los índices
  N <- nrow(df_y)
  rate <- (nrow(df_z) / N) * 100
  gap <- sum(df_z$brecha_rel) / N
  severity <- sum(df_z$brecha_rel_sqr) / N

  # Crear el dataframe de salida
  df_w <- data.frame(deciles = decile, rate = rate, gap = gap, severity = severity)

  return(df_w)
})

# Asignar nombres a la lista de salida
names(outcome_1_list) <- deciles_grupos

calculate_outcome <- function(dataset, model, deciles_grupos) {
  z <- as.numeric(levels(as.factor(model$per_capita_year )))
  outcome_list <- list()

  for (j in 1:length(deciles_grupos)) {
    df_y <- dataset %>% filter(deciles %in% deciles_grupos[j])

    # Crear dummy vectorizado
    df_y$dummy <- ifelse(df_y$food_income_per_capita_year < z, 1, 0)

    df_z <- df_y %>% filter(dummy == 1)

    df_z$brecha_rel <- (z - df_z$food_income_per_capita_year) / z
    df_z$brecha_rel_sqr <- df_z$brecha_rel^2

    N <- nrow(df_y)
    rate <- (nrow(df_z) / N) * 100
    gap <- sum(df_z$brecha_rel) / N
    severity <- sum(df_z$brecha_rel_sqr) / N

    df_w <- data.frame(deciles = deciles_grupos[j], rate = rate, gap = gap, severity = severity)

    outcome_list[[j]] <- df_w
  }

  names(outcome_list) <- deciles_grupos
  return(outcome_list)
}

# Calcular resultados para los tres escenarios
outcome_1_list <- calculate_outcome(Data_income_household, Model_CoCA, deciles_grupos)
outcome_2_list <- calculate_outcome(Data_income_household, Model_CoNA, deciles_grupos)
outcome_3_list <- calculate_outcome(Data_income_household, Model_CoRD, deciles_grupos)

# Combinar resultados para cada escenario
poverty_1_outcome <- do.call(rbind, outcome_1_list)
poverty_2_outcome <- do.call(rbind, outcome_2_list)
poverty_3_outcome <- do.call(rbind, outcome_3_list)



# Agregar resultados finales en un DF
poverty_1_outcome <- poverty_1_outcome %>%
  mutate(model = "CoCA")

poverty_2_outcome <- poverty_2_outcome %>%
  mutate(model = "CoNA")

poverty_3_outcome <- poverty_3_outcome %>%
  mutate(model = "CoRD")

# Unir los dataframes en uno solo
poverty_outcome <- bind_rows(poverty_1_outcome, poverty_2_outcome, poverty_3_outcome)


#--------------------------------------------------#
# Razones costo m?nimo e ingreso en alimentación   #
#--------------------------------------------------#


# Hallar el ingreso promedio por decil
deciles_grupos = c("Decil 1", "Decil 2",
                   "Decil 3", "Decil 4",
                   "Decil 5", "Decil 6",
                   "Decil 7", "Decil 8",
                   "Decil 9", "Decil 10")


mean_income = data.frame(deciles_grupos)
mean_income$ingreso_prom = NA
mean_income$size_prom = NA
mean_income$n = NA
mean_income$min_ing_pc = NA
mean_income$max_ing_pc = NA
mean_income$ing_per_capita_prom = NA

mean_income$share = NA

mean_income$food = NA

mean_income$min_food_pc = NA
mean_income$max_food_pc = NA
mean_income$food_per_capita_prom = NA



# NOTA: LOS PROMEDIOS FUERON CALCULADOS CON PREVIA EXPANSIÓN
for (k in 1:length(deciles_grupos)) {
  df = data.frame()
  df = Data_income_household  %>% filter(deciles  %in% deciles_grupos[k])
  y_1 = which(mean_income$deciles_grupos == deciles_grupos[k])


  mean_income$ingreso_prom[y_1] = mean(df$income)

  mean_income$size_prom[y_1] = mean(df$Total_persons_household)

  mean_income$n[y_1] = nrow(df)

  mean_income$min_ing_pc[y_1] = min(df$per_capita_income)

  mean_income$max_ing_pc[y_1] = max(df$per_capita_income)

  mean_income$ing_per_capita_prom[y_1] = mean(df$per_capita_income)

  mean_income$share[y_1] = as.numeric(levels(as.factor(df$share)))

  mean_income$food[y_1] = mean(df$food_income)

  mean_income$min_food_pc[y_1] = min(df$food_income_per_capita)
  mean_income$max_food_pc[y_1] = max(df$food_income_per_capita)
  mean_income$food_per_capita_prom[y_1] = mean(df$food_income_per_capita)

}

mean_income_deciles = mean_income


new_names_mean_income <- c(
  "deciles",
  "average_income",
  "average_size",
  "n",
  "min_income_per_capita",
  "max_income_per_capita",
  "average_income_per_capita",
  "share",
  "food_income",
  "min_food_per_capita",
  "max_food_per_capita",
  "average_food_income_per_capita"
)

# Cambiar los nombres de las columnas
names(mean_income_deciles) <- new_names_mean_income

umbral_1 =as.numeric(levels(as.factor(Model_CoCA$per_capita_month)))
umbral_2 =as.numeric(levels(as.factor(Model_CoNA$per_capita_month)))
umbral_3 =as.numeric(levels(as.factor(Model_CoRD$per_capita_month)))

mean_income_food = mean_income_deciles[c("deciles", "average_food_income_per_capita")]

mean_income_food$umbral_1 = umbral_1
mean_income_food$umbral_2= umbral_2
mean_income_food$umbral_3= umbral_3

mean_income_food$ratio_1 = mean_income_food$umbral_1/mean_income_food$average_food_income_per_capita
mean_income_food$ratio_2 = mean_income_food$umbral_2/mean_income_food$average_food_income_per_capita
mean_income_food$ratio_3 = mean_income_food$umbral_3/mean_income_food$average_food_income_per_capita

names(mean_income_food)= c("decile_groups", "food_per_capita_avg", "threshold_1", "threshold_2", "threshold_3", "ratio_1", "ratio_2", "ratio_3")

#-----------------------------#
# FIN    DEL MÓDULO 6 ORGINAL # FALTA SIMPLIFICAR Y GENERALIZAR
#-----------------------------#

# Guardando las salidas como lista

Resultados=list(poverty_outcome,mean_income_food);names(Resultados)=c("Poverty_outcome","Mean_income_food")

# retorno

Sys.sleep(1);cat("     Finalizado ✓ \n")

return(invisible(Resultados))
}

x_3=Modulo_3(Data_income_household=x_1,Model_CoCA=x,Model_CoNA=x_2$Model_CoNA,Model_CoRD=x_2$Model_CoRD)



x=data.frame(per_capita_year=2, per_capita_month=5)






