\name{IncomeCol}
\docType{package}
\alias{IncomeCol}
\title{Cálculo de Ingresos a partir de Datos GEIH}

\usage{
IncomeCol(
  Month,
  Year,
  City
)
}

\description{
Función diseñada para calcular los ingresos actuales de individuos basándose en datos de la Encuesta Integrada de Hogares (GEIH). Además, utiliza información de la Encuesta de Calidad de Vida (ECV) para calcular las proporciones del ingreso destinadas al gasto en alimentos y bebidas no alcohólicas, diferenciadas por niveles de ingreso.

}

\examples{
# Ejemplo de uso :

datos <- Foodpricev2::IncomeCol(Month=1, Year=2022, City="Cali")
head(datos_ingresos)

}

\arguments{
  \item{Mes:}{Valor numérico que indica el período mensual para el cual se desean calcular los ingresos. Debe ser un valor entre 1 y 12.}
  \item{Año:}{Valor numérico que indica el año de los datos a cargar. Debe estar comprendido entre 2022 y 2023.}
  \item{Ciudad:}{Cadena de caracteres que especifica la ciudad para la cual se calcularán los ingresos. Actualmente, el paquete funciona para las 13 principales ciudades y áreas metropolitanas del país, a saber: Bogotá D.C., Medellín A.M., Cali A.M., Barranquilla A.M., Bucaramanga A.M., Manizales A.M., Pereira A.M., Cúcuta A.M., Pasto, Ibagué, Montería, Cartagena y Villavicencio.}
}


\details{
La función carga en total seis módulos. El módulo 0 se encarga de descargar y cargar las librerías necesarias. El módulo 1 se encarga de descargar y cargar los datos de la Encuesta Integrada de Hogares (GEIH) para el mes y año asignados. El módulo 2 aplica un algoritmo para el cálculo de los ingresos corrientes mensuales de las unidades de gasto. El módulo 3 aplica factores de expansión a los hogares. El módulo 4 descarga y carga los datos de la Encuesta de Calidad de Vida (ECV), con la cual se calcularán las proporciones del gasto en alimentos y bebidas no alcohólicas. Finalmente, el módulo 5 concluye con los cálculos de los ingresos.

}

\value{

Devuelve un conjunto de datos con las siguientes columnas:

\itemize{
  \item deciles: Factor que indica el decil al que pertenece cada fila o registro.
  \item household_id: Identificador único del hogar.
  \item ung: Tamaño del hogar.
  \item income: Ingresos totales del hogar.
  \item per_capita_income: Ingresos per cápita calculados como `income / ung`.
  \item share: Proporción de ingresos destinada a la alimentación.
  \item food_exp: Gasto en alimentación calculado como `income * share`.
  \item food_exp_per_capita: Gasto per cápita en alimentación calculado como `food_exp / ung`.
  \item food_exp_per_capita_year: Gasto anual per cápita en alimentación calculado como `food_exp_per_capita * 12`.
}
}


\note{
Es pertinente asegurarse de especificar correctamente los parámetros de entrada (Mes, Año, Ciudad) para cargar los datos relevantes de GEIH.
}
