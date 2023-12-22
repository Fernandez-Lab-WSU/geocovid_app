#' bsas: Partidos de Buenos Aires
#'
#' Dataset de clase sf con los polígonos de los partidos de Buenos Aires y 
#' las coordenadas de la bounding box de cada uno. Ver el repositorio de 
#' GeoCovid BsAs para mas detalle de estas Variables
#'
#' @format ## `bsas`
#' Este 
#' \describe{
#'   \item{partido}{Nombre del partido de prov de Buenos Aires}
#'   \item{lat1}{Latitud}
#'   \item{lat2}{Latitud}
#'   \item{lng1}{Longitud}
#'   \item{lng2}{Longitud}
#'   \item{geom}{sfc_POLYGON}
#' }
"bsas"

#' mini_data_sisa_deploy: Extrato del dataset de casos de COVID-19
#'
#' Ver el repositorio de GeoCovid BsAs para mas detalle de estas Variables
#' 
#' @format ## `mini_data_sisa_deploy`
#' Una extracto de la abse de datos con 2230 filas y 4 columnas:
#' \describe{
#'   \item{id_evento_caso}{ID}
#'   \item{residencia_provincia_nombre}{Provincia}
#'   \item{residencia_departamento_nombre}{Departamento, partido}
#'   \item{fecha_enfermo}{Fecha reporte caso COVID-19}
#'   }
"mini_data_sisa_deploy"

#' px_baires: Valores promedio de movilidad ciudadana por partido
#'
#' Ver el repositorio de GeoCovid BsAs para mas detalle de estas Variables.
#' Se esta empleando solo un extracto de la base de datos para este dataset de 
#' ejempplo.
#' 
#' @format ## `px_baires`
#' Una extracto de la abse de datos con 4080 filas y 11 columnas:
#' \describe{
#'   \item{fecha}{ID}
#'   \item{locacion}{Provincia}
#'   \item{tipo_de_raster}{Cambio porcentual prepandemia (pc) o semanal (7dpc)}
#'   \item{partido}{Partido o departamento}
#'   \item{noche_0}{Promedio para el raster de la noche}
#'   \item{mañana_8}{Promedio para el raster de la mañana}
#'   \item{tarde_16}{Promedio para el raster de la tarde}
#'   \item{px_mean_dianoche}{Promedio para los rasters de mañana y tarde}
#'   \item{criterio}{Vuelve variable categorica px_mean_dianoche}
#'   \item{criterio_noche}{Vuelve variable categorica noche_0}
#'   \item{geom}{sfc_POLYGON}
#'   }
"px_baires"