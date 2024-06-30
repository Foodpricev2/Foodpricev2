\name{HCost}
\docType{package}
\alias{HCost}
\title{Estimación de Métricas de Costo Mínimo para Dietas en Hogares}

\usage{
HCost(
  Month = NULL,
  Year = NULL,
  City = NULL,
  Household,
  Data = NULL,
  ERR = NULL,
  EER_LL = NULL,
  UL = NULL,
  Serv = NULL,
  Diverse = NULL
)
}

\description{
Función diseñada para estimar métricas basadas en tres tipos de dietas de costo mínimo para un hogar representativo, utilizando datos proporcionados por el paquete Foodprice. Esta función integra las funciones CoCA(), CoNA() y CoRD() del paquete Foodprice v.1.0.0 (González et al., 2024), unificándolas en un solo proceso mediante los parámetros especificados.
}

\examples{
# Ejemplo de uso :

# cargando datos de Foodprice
library(Foodprice)

datos_h=HCost <- Foodpricev2::HCost(Year = 2022, City = "Cali", Month=6,Household=Household, ERR = EER, EER_LL = EER_LL, UL = UL, Serv = serv,Diverse=diverse)}
head(datos_h)

}

\arguments{
  \item{Month:}{Valor numérico que indica el mes para el cual se realizará la estimación. Debe ser un valor entre 1 y 12.}
  \item{Year:}{Valor numérico que indica el año para el cual se realizará la estimación. Debe estar comprendido entre 2022 y 2023.}
  \item{City:}{Cadena de caracteres que especifica la ciudad para la cual se realizará la estimación. Actualmente, el paquete funciona para las 13 principales ciudades y áreas metropolitanas del país: Bogotá D.C., Medellín A.M., Cali A.M., Barranquilla A.M., Bucaramanga A.M., Manizales A.M., Pereira A.M., Cúcuta A.M., Pasto, Ibagué, Montería, Cartagena y Villavicencio.}
  \item{Household:}{Data frame con tres columnas que definen la composición del hogar representativo según sexo y edad de sus miembros. La columna `Person` indica el identificador de la persona, `Sex` especifica el sexo de la persona (0=Hombre, 1=Mujer), y `Demo_Group` define el grupo demográfico al que pertenece, con los mismos niveles utilizados en las salidas de los modelos CoCA, CoNA y CoRD para una implementación correcta (consulte la documentación del paquete Foodprice para más información).}
  \item{Data:}{Data frame que contiene información detallada sobre alimentos locales disponibles, incluyendo nombre, grupo/subgrupo, tamaño de porción comestible, precios, contenido nutricional, etc.}
  \item{ERR:}{Data frame con requerimientos de energía estimados para cada grupo demográfico del hogar representativo.}
  \item{EER_LL:}{Data frame que incluye niveles mínimos de ingesta requeridos para nutrientes específicos según grupo demográfico.}
  \item{UL:}{Data frame que especifica niveles máximos de ingesta recomendados para nutrientes según grupo demográfico.}
  \item{Serv:}{Data frame con recomendaciones de porciones diferenciadas por grupos de alimentos y demográficos dentro del hogar representativo.}
  \item{Diverse:}{Data frame que indica el número de alimentos seleccionados por grupo de alimentos según la estructura definida en Data.}
}

\details{
La función requiere únicamente el parámetro obligatorio `Household`. Si dispone de datos con estimaciones de alimentos, grupos y composición nutricional, puede utilizarlos mediante el parámetro `Data`. Para usar estimaciones específicas de Colombia utilizando la función `DataCol()` del paquete Foodprice, simplemente ingrese los parámetros `Month`, `Year` y `City`. Además, la función es flexible y permite calcular solo los modelos pertinentes:

\begin{itemize}
  \item Si omite el parámetro `ERR`, la función no calculará el costo de la dieta CoCA.
  \item Si omite alguno de los parámetros `EER_LL` o `UL`, la función no calculará el costo de la dieta CoNA.
  \item Si omite alguno de los parámetros `Serv` o `Diverse`, la función no calculará el costo de la dieta CoRD.
\end{itemize}
}

\value{
Devuelve una lista con data frames (dependiendo del número de modelos que requiera ejecutar): Model_CoCA, Model_CoNA, Model_CoRD. En general, los anteriores data frames tienen las siguientes columnas:
\itemize{
  \item cost_day: Costo diario estimado para la dieta.
  \item total_household: Costo total estimado para el hogar.
  \item per_capita: Costo per cápita estimado para la dieta.
  \item per_capita_year: Costo anual per cápita estimado para la dieta.
  \item per_capita_month: Costo mensual per cápita estimado para la dieta.
}
}

\note{
Es crucial asegurarse de proporcionar correctamente todos los parámetros necesarios para la función `HCost`, según la estructura y formatos detallados en la documentación de Foodprice v.1.0.0.
}
