#' Elementos de interfaz de usuario de la barra flotante del tab de Mapa BsAs
#'
#' @param id Module name
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan.
#'
#' @param id Module name
#' @return Barra flotante del tab Mapa Buenos Aires.
#' 
#' @export
FechaMomentoUI <- function(id, base_raster) {
  
  ns <- NS(id)
  
  momento_del_dia <- as.list(unique(base::unique(base_raster$hora)))
  names(momento_del_dia) <- unique(base::unique(base_raster$momento))
  
  shiny::tagList(
    h4("Cliquea en el mapa"),
    shiny::radioButtons(ns("basemap"),
                        label = 'Mapa Base:',
                        choices = c("Relieve" = 'relieve',
                                    "Calles" = 'calles'),
                        selected = 'relieve',
                        inline = TRUE),
    shiny::radioButtons(ns("area"),
                        label = paste('Selecciona AMBA para visualizar datos",
                                      "con mayor resolucion en ese area'),
                        choices = c("prov. de Buenos Aires" = 'baires',
                                    "AMBA" = 'amba'),
                        selected = 'baires',
                        inline = TRUE),
    shinyjs::hidden(shiny::dateInput(ns("fechas"),
                                     label = "Fecha",
                                     min = "2020-05-09", #min(base::unique(base_raster$fecha)),
                                     max = "2020-05-14", #max(base::unique(base_raster$fecha))
                                     value = "2020-05-09",
                                     language = "es",
                                     format = "yyyy-mm-dd")),
    shinyjs::hidden(shiny::radioButtons(ns('porcentaje'),
                                        label = 'Cambio porcentual',
                                        choices = c("Prepandemia" = "pc",
                                                    "Semanal" = "7dpc"),
                                        inline = TRUE,
                                        selected = "pc")),
    shinyjs::hidden(
      shiny::radioButtons(ns("momento"),
                          label = "Momento del día",
                          choices = unique(base_raster$momento),
                          inline = TRUE,
                          selected = unique(base_raster$momento)[1])),
    shinyjs::hidden(
      p(id = 'barra_transparencia',
        "Opciones de visualizacion del mapa"),
      shiny::sliderInput(ns("opacity"),
                         label = "Transparencia",
                         min = 0,
                         max = 1,
                         value = 0.5,
                         ticks = FALSE))
    
  )
}

#' Convierte el archivo en raster en base a las elecciones del usuario
#'
#' @param id Module name
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan.
#' @param mapa_zoom Valor de zoom que retorna leaflet en base al uso del mapa.
#'
#' @return Una serie de variables extraidas del dataframe base_raster para
#' el raster que eligio el usuario y representan el area, la opacidad, 
#' el raster mismo (imagen) y basemap
#' @export
FechaMomento_Server <- function(id,
                                base_raster,
                                mapa_zoom){
  moduleServer(id,
               session = getDefaultReactiveDomain(),
               function(input, output, session){
                 
                 
                 imagen <- shiny::reactive({
                   
                   
                   # selecciono un solo dia y tiempo, ya que estoy probando
                   raster_data <-  base_raster |>
                     dplyr::filter(.data$fecha == as.Date(input$fechas,
                                                    origin = "1970-01-01"),
                                   .data$tipo_de_raster == input$porcentaje,
                                   .data$momento == input$momento,
                                   .data$locacion == input$area)
                   
                   # leo el raster
                   terra::rast(paste0('data/rasters/', raster_data$value))
                 })
                 
                 # revelo la barra de transparencia cuando el zoom es mayor a 6
                 shiny::observeEvent(mapa_zoom(),{
                   if(mapa_zoom() <= 6){
                     shinyjs::hide("opacity")
                     shinyjs::hide("porcentaje")
                     shinyjs::hide("momento")
                     shinyjs::hide("fechas")
                   }else{
                     shinyjs::show("opacity")
                     shinyjs::show("porcentaje")
                     shinyjs::show("momento")
                     shinyjs::show("fechas")
                   }
                 })
                 
                 return(
                   list(
                     area = reactive({ input$area }),
                     opacity = reactive({ input$opacity }),
                     imagen = imagen,
                     basemap = reactive({ input$basemap }) #?
                   )
                 )
                 
               })}