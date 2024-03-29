#' IU: Mapa raster por partido
#' 
#' @description
#' Este mapa se encuentra en el tab 2, "por partido" de GeoCovid app
#' 
#' @param id Module name
#' 
#' @return Mapa leaflet en la IU
#' @export
MapaPartido_UI <- function(id) {

  ns <- NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(
                 leaflet::leafletOutput(ns("mapa_partido"),
                                        height = 350

                                       ),
                 type = 2,
                 color = 'lightgrey',
                 color.background = 'white')
       )
}

#' Servidor: Mapa raster por partido
#' 
#' @description
#' Este mapa se encuentra en el tab 2, "por partido" de GeoCovid app
#'
#' @param id Module name
#' @param amba_reducido_names String. Vector con los nombres de los partidos 
#' que conforman el AMBA.
#' @param base_raster Dataframe que lista todos los rasters y desagrega en 
#' sus columnas características de interes, como si son rasters de 
#' AMBA o Buenos Aires, si el cambio porcentual es semanal o prepandemia 
#' o el momento del día que representan. 
#' @param fecha Fecha seleccionada.
#' @param bsas Dataset de clase sf con los partidos de Buenos Aires.
#' @param area El raster puede corresponder a Buenos Aires provincia o a AMBA.
#' @param part Partido de la provincia de Buenos Aires, seleccionado en otro 
#' módulo.
#' @param momento_dia Entero. Hora del dia representada en el raster, puede ser 
#' 0am, 8 am o 4 pm.
#' @param tipo_de_raster String. Si el raster corresponde a el cambio porcentual 
#' prepandemia (pc) o semanal (7dpc).
#' @param opacidad Double. Valor de opacidad del raster. 
#'
#' @return Mapa raster por partido creado con leaflet.
#' @export
MapaPartido_Server <-  function(id,
                                amba_reducido_names,
                                base_raster, 
                                bsas_comunas,
                                area, 
                                fecha,
                                tipo_de_raster, 
                                opacidad, part,
                                momento_dia) {

   moduleServer(
    id,
    session = getDefaultReactiveDomain(),
    function(input, output, session) {



      # Esta imagen generada no es la misma que en FechaMomento
      # Estos modulos se van a reusar en disntos momentos del dio
      # FUTURE: Podria transformar esto en un modulo menor dentro de este modulo.

      imagen <- shiny::reactive({

        # Agrego un dia por default para que renderice si no hay otras fechas.
        if(is.null(fecha())){

          formatted_date <- format('2020-05-03', format = "%Y-%m-%d")


        }else{

   formatted_date <- formatted_date(fecha())  }

        # selecciono un solo dia y tiempo, ya que estoy probando
        raster_data <-  base_raster |>
          dplyr::filter(.data$fecha == as.Date(formatted_date,
                                         origin = "1970-01-01"),
                        .data$tipo_de_raster == tipo_de_raster(),
                        .data$momento == momento_dia, # es un valor no reactivo
                        .data$locacion == area()
                       )

      terra::rast(paste0(system.file("rasters", package = 'geocovidapp'),
                         "/",
                         raster_data$value))

      })

      filter_partido <- shiny::reactive({

        if(part() %in% amba_reducido_names){ # amba

          # ver Partidos_Input.R
          amba <-  dplyr::filter(bsas_comunas,
                                 .data$partido %in% amba_reducido_names)

          sf::st_as_sf(base::subset(amba, 
                                    partido == part()))

        }else if(!(part() %in% amba_reducido_names)){ #baires

          # ver Partidos_Input.R
          prov <-  dplyr::filter(bsas,
                                 !.data$partido %in% amba_reducido_names)

          # recorto por poligono
          sf::st_as_sf(base::subset(prov,
                              partido == part()))


        }
      })


output$mapa_partido <-  leaflet::renderLeaflet({


pal <- leaflet::colorBin(palette = c("#0000FF", "#0040FF",
                                     "#0080FF", "#00BFFF",
                                     "#00FFFF", "#FFFFFF",
                                     "#FFCC00", "#FF9900",
                                     "#FF6600", "#FF3300",
                                     "#FF0000"),
                         bins = c(50, 40, 30 , 20 , 10, 1, -1,
                                  -10, -20, -30, -40, -50),
                         na.color = "transparent")

bbx <- sf::st_bbox(filter_partido())

        # mapa mañana
        leaf_map <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) |>
          leaflet::addTiles() |>
          leaflet::addRasterImage(shiny::isolate(imagen()),
                                  colors = pal,
                                  opacity = shiny::isolate(opacidad()),
                                  group = "basic",
                                  layerId = "raster") |>
          leaflet::addPolygons(data = shiny::isolate(filter_partido())[, 'geom'],
                               label = shiny::isolate(filter_partido())[, 'partido'],
                               layerId = shiny::isolate(filter_partido())[, 'partido'],
                               color = "black",
                               fillColor = "transparent",
                               weight = 1,
                               stroke = TRUE,
                               fillOpacity = 0.1,
                               smoothFactor = 0.5,
                               group = "basic") |>
          leaflet::fitBounds(lng1 = bbx$xmin[[1]],
                            lat1= bbx$ymin[[1]],
                            lng2 = bbx$xmax[[1]],
                            lat2 = bbx$ymax[[1]])


        leaf_map
})


shiny::observe({


  pal <- leaflet::colorBin(palette = c("#0000FF", "#0040FF",
                                       "#0080FF", "#00BFFF",
                                       "#00FFFF", "#FFFFFF",
                                       "#FFCC00", "#FF9900",
                                       "#FF6600", "#FF3300",
                                       "#FF0000"),
                           bins = c(50, 40, 30 , 20 , 10, 1, -1,
                                    -10, -20, -30, -40, -50),
                           na.color = "transparent")


  leaflet::leafletProxy("mapa_partido")  |>
    leaflet::addRasterImage(imagen(),
                            colors = pal,
                            opacity = opacidad(),
                            group = "basic",
                            layerId = "raster") |>
     leaflet::clearShapes() |>
    leaflet::addPolygons(data = filter_partido()[, 'geom'],
                         label = filter_partido()[, 'partido'],
                         layerId = filter_partido()[, 'partido'],
                         color = "black",
                         fillColor = "transparent",
                         weight = 1,
                         stroke = TRUE,
                         fillOpacity = 0.1,
                         smoothFactor = 0.5,
                         group = "basic") |>
    leaflet::fitBounds(
                        lng1 = filter_partido()$lng1,
                        lat1 = filter_partido()$lat1,
                        lng2 = filter_partido()$lng2,
                        lat2 = filter_partido()$lat2)


})

})}