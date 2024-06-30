\name{Afford}
\docType{package}
\alias{Afford}
\title{Cálculo de Indicadores de Asequibilidad de Dietas de Costo Mínimo}

\usage{
Afford(
  Hexpense,
  Model_CoCA = NULL,
  Model_CoNA = NULL,
  Model_CoRD = NULL
)
}

\description{
La función Afford calcula indicadores de asequibilidad para tres tipos de dietas de costo mínimo, utilizando los resultados generados por la función HCost y los datos de gasto de hogares proporcionados en el data frame Hexpense. Hexpense debe contener las siguientes columnas: `id_hogar` (identificador único del hogar), `ung` (número de miembros del hogar), `income` (ingreso mensual total del hogar) y `food_exp` (gasto mensual en alimentos y bebidas no alcohólicas). Por otro lado, HCost es el resultado de la función HCost y debe contener información detallada sobre los modelos CoCA, CoNA y CoRD estimados para cada hogar representativo, incluyendo la composición por sexo y grupos demográficos de los miembros del hogar.
}

\examples{
# Ejemplo de uso:
datos <- Foodpricev2::IncomeCol(Mes=1, Año=2022, Ciudad="Cali")

# Cargar datos previamente calculados
datos_hcost <- HCost(Year = 2022, City = "Cali", Month = 6, Household = Household, ERR = EER, EER_LL = EER_LL, UL = UL, Serv = serv, Diverse = diverse)

# Calcular indicadores de asequibilidad
resultados <- Afford(Hexpense = datos_h, Model_CoCA = datos_hcost$Model_CoCA, Model_CoNA = datos_hcost$Model_CoNA, Model_CoRD = datos_hcost$Model_CoRD)
}

\arguments{
  \item{Hexpense:}{Data frame con los datos de gasto de hogares, incluyendo `id_hogar`, `ung`, `income` y `food_exp`.}
  \item{Model_CoCA:}{Data frame opcional con los resultados del modelo CoCA generado por la función HCost.}
  \item{Model_CoNA:}{Data frame opcional con los resultados del modelo CoNA generado por la función HCost.}
  \item{Model_CoRD:}{Data frame opcional con los resultados del modelo CoRD generado por la función HCost.}
}

\details{
La función Afford combina los datos de gasto de hogares (Hexpense) con los resultados de los modelos CoCA, CoNA y CoRD (si están disponibles) para calcular indicadores de asequibilidad. Estos indicadores ayudan a evaluar la capacidad de los hogares para alcanzar dietas de costo mínimo recomendadas, considerando variables como el ingreso total, el gasto en alimentos y el tamaño del hogar.
}

\value{
Devuelve una lista con dos elementos:
\itemize{
  \item `Poverty_outcome`: Resultados de los indicadores de asequibilidad calculados para los modelos CoCA, CoNA y CoRD, incluyendo tasas, brechas y severidades por decil.
  \item `Mean_income_food`: Promedios y ratios de ingreso y gasto en alimentos por decil, junto con los umbrales calculados para cada tipo de dieta.
}
}

\note{
Es esencial asegurarse de que los datos proporcionados a la función Afford estén correctamente estructurados según lo especificado para Hexpense y HCost, tal como se describe en la documentación de Foodprice v.1.0.0.
}
