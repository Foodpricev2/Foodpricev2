HCost <- function(Month = NULL, Year = NULL, City = NULL, Household, Data = NULL, ERR = NULL, EER_LL = NULL, UL = NULL, Serv = NULL, Diverse = NULL) {

  #-------------------------------------------------#
  #  Validación de parámetros de la función         #
  #-------------------------------------------------#

  # Validar tipo y estructura del dataframe Household
  validar_household_type <- function(household_type) {
    if (!is.data.frame(household_type)) {
      stop("Household debe ser un dataframe.")
    }

    if (ncol(household_type) != 3) {
      stop("Household debe tener exactamente 3 columnas.")
    }

    if (!all(c("Person", "Sex", "Demo_Group") %in% colnames(household_type))) {
      stop("Household debe tener las columnas Person, Sex y Demo_Group.")
    }

    if (!all(sapply(household_type$Person, is.numeric))) {
      stop("La columna Person de Household debe ser numérica.")
    }

    if (!all(household_type$Sex %in% c(0, 1))) {
      stop("La columna Sex de Household debe contener solo valores 0 o 1.")
    }

    if (!all(sapply(household_type$Demo_Group, is.character))) {
      stop("La columna Demo_Group de Household debe contener solo texto.")
    }
  }

  validar_household_type(Household)

  # Función para validar parámetros
  validar_parametros <- function(parametro, tipo, rango = NULL) {
    if (missing(parametro)) {
      stop("Parámetro faltante: ", deparse(substitute(parametro)))
    }

    tipo_funcion <- switch(tipo,
                           "numeric" = is.numeric,
                           "character" = is.character,
                           "list" = is.list,
                           "vector" = function(x) is.vector(x) || is.data.frame(x),
                           "default" = function(x) FALSE)

    if (!tipo_funcion(parametro)) {
      stop(paste("El parámetro", deparse(substitute(parametro)), "debe ser de tipo", tipo))
    }

    if (!is.null(rango) && !is.infinite(rango[1]) && !is.infinite(rango[2])) {
      if (parametro < rango[1] || parametro > rango[2]) {
        stop(paste("El parámetro", deparse(substitute(parametro)), "debe estar en el rango", rango[1], "-", rango[2]))
      }
    }
  }

  # Validar parámetros de entrada Month, Year y City si Data es NULL
  if (is.null(Data)) {
    validar_parametros(Month, "numeric", c(1, 12))
    validar_parametros(Year, "numeric", c(2022, 2023))
    validar_parametros(City, "character")

    cat("Se utilizará la función DataCol del paquete Foodprice para estimaciones.\n")
    Data_mes_año <- Foodprice::DataCol(Month = Month, Year = Year, City = City)
  } else {
    Data_mes_año <- Data
  }

  # Ejecutar modelo CoCA si ERR no es NULL
  if (!is.null(ERR)) {
    cat("Ejecutando modelo CoCA.\n")
    modelo_1 <- Foodprice::CoCA(data = Data_mes_año, EER = ERR)$cost
    model_dieta_1 <- merge(Household, modelo_1[c("Demo_Group", "Sex", "cost_day")],
                           by = c("Demo_Group", "Sex"),
                           all.x = TRUE, all.y = FALSE)
    model_dieta_1$hogar_total <- sum(as.numeric(model_dieta_1$cost_day))
    model_dieta_1$per_capita <- model_dieta_1$hogar_total / nrow(model_dieta_1)
    model_dieta_1$per_capita_year <- model_dieta_1$per_capita * 365
    model_dieta_1$per_capita_month <- model_dieta_1$per_capita * 30
  } else {
    cat("No se ejecutará la parte correspondiente al modelo CoCA porque ERR es NULL.\n")
    model_dieta_1 <- NULL
  }

  # Ejecutar modelo CoNA si EER_LL y UL no son NULL
  if (!is.null(EER_LL) && !is.null(UL)) {
    cat("Ejecutando modelo CoNA.\n")
    modelo_2 <- Foodprice::CoNA(data = Data_mes_año, EER_LL = EER_LL, UL = UL)$cost
    model_dieta_2 <- merge(Household, modelo_2[c("Demo_Group", "Sex", "cost_day")],
                           by = c("Demo_Group", "Sex"),
                           all.x = TRUE, all.y = FALSE)
    model_dieta_2$hogar_total <- sum(as.numeric(model_dieta_2$cost_day))
    model_dieta_2$per_capita <- model_dieta_2$hogar_total / nrow(model_dieta_2)
    model_dieta_2$per_capita_year <- model_dieta_2$per_capita * 365
    model_dieta_2$per_capita_month <- model_dieta_2$per_capita * 30
  } else {
    cat("No se ejecutará la parte correspondiente al modelo CoNA porque EER_LL y/o UL son NULL.\n")
    model_dieta_2 <- NULL
  }

  # Ejecutar modelo CoRD si Serv y Diverse no son NULL
  if (!is.null(Serv) && !is.null(Diverse)) {
    cat("Ejecutando modelo CoRD.\n")
    modelo_3 <- Foodprice::CoRD(data = Data_mes_año, diverse = Diverse, serv = Serv)$cost

    # Aplicar mapeo de grupos demográficos si Data es NULL
    if (is.null(Data)) {

      mapeo_grupos <- list(
        ">70 años" = "> 70 años",
        "1 a 3 años" = "1-3 años",
        "14 a 18 años" = "14-18 años",
        "19 a 30 años" = "19-30 años",
        "31 a 50 años" = "31-50 años",
        "4 a 8 años" = "5 -8 años",
        "51 a 70 años" = "51-70 años",
        "9 a 13 años" = "9-13 años",
        "gestantes < 18 años" = "Gestantes 14-18 años",
        "gestantes 19 a 30 años" = "Gestantes 19-30 años",
        "gestantes 31 a 50 años" = "Gestantes 31-50 años",
        "lactantes < 18 años" = "Lactantes 14-18 años",
        "lactantes 19 a 30 años" = "Lactantes 19-30 años",
        "lactantes 31 a 50 años" = "Lactantes 31-50 años"
      )

      aplicar_mapeo <- function(df, mapeo) {
        df$Demo_Group <- mapeo[df$Demo_Group]
        return(df)
      }

      Household_mod_3 <- aplicar_mapeo(Household, mapeo_grupos)

    } else {
      Household_mod_3 <- Household
    }

    model_dieta_3 <- merge(Household_mod_3, modelo_3[c("Demo_Group", "Sex", "cost_day")],
                           by = c("Demo_Group", "Sex"),
                           all.x = TRUE, all.y = FALSE)
    model_dieta_3$hogar_total <- sum(as.numeric(model_dieta_3$cost_day))
    model_dieta_3$per_capita <- model_dieta_3$hogar_total / nrow(model_dieta_3)
    model_dieta_3$per_capita_year <- model_dieta_3$per_capita * 365
    model_dieta_3$per_capita_month <- model_dieta_3$per_capita * 30
  } else {
    cat("No se ejecutará la parte correspondiente al modelo CoRD porque Serv y/o Diverse son NULL.\n")
    model_dieta_3 <- NULL
  }

  # Cambiar nombres de columnas
  new_names_model_dieta <- c(
    "Demo_Group",
    "Sex",
    "Person",
    "cost_day",
    "total_household",
    "per_capita",
    "per_capita_year",
    "per_capita_month"
  )

  # Asignar nombres a los modelos y eliminar NULL
  resultado <- list(Model_CoCA = model_dieta_1, Model_CoNA = model_dieta_2, Model_CoRD = model_dieta_3)
  resultado <- resultado[!sapply(resultado, is.null)]  # Eliminar modelos NULL

  # Cambiar nombres de columnas
  for (i in seq_along(resultado)) {
    names(resultado[[i]]) <- new_names_model_dieta
  }

  cat("Proceso completado.\n")

  return(resultado)
}








