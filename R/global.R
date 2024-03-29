
# Para chequar la reactividad, descomentar
# library(reactlog)
# reactlog_enable()

# Cargo datasets ------

amba_reducido_names <- c('Almirante Brown',
                         'Avellaneda',
                         'Berazategui',
                         paste('Comuna', 1:15), # CABA
                         'Esteban Echeverría', 'Escobar', 'Ezeiza',
                         'Florencio Varela',
                         'General San Martín',
                         'Hurlingham',
                         'Ituzaingó',
                         'José C. Paz',
                         'La Matanza',  'Lanús', 'Lomas de Zamora',
                         'Malvinas Argentinas', 'Merlo', 'Moreno', 'Morón',
                         'Quilmes', 'Pilar', 'Presidente Perón',
                         'San Fernando', 'San Isidro', 'San Miguel',
                         'Tigre', 'Tres de Febrero',
                         'Vicente López')

amba_caba <- c('Almirante Brown',
               'Avellaneda',
               'Berazategui',
               'Capital Federal', # CABA
               'Esteban Echeverría', 'Escobar', 'Ezeiza',
               'Florencio Varela',
               'General San Martín',
               'Hurlingham',
               'Ituzaingó',
               'José C. Paz',
               'La Matanza',  'Lanús', 'Lomas de Zamora',
               'Malvinas Argentinas', 'Merlo', 'Moreno', 'Morón',
               'Quilmes', 'Pilar', 'Presidente Perón',
               'San Fernando', 'San Isidro', 'San Miguel',
               'Tigre', 'Tres de Febrero',
               'Vicente López')



# PRIMERA VERSION
# Esta version de la app no tiene conectados todos los datos.
# mini_data_sisa <- data_sisa |> filter(fecha_enfermo >= as.Date(min(base_raster$fecha)),
#                                       fecha_enfermo <= as.Date(max(base_raster$fecha)))
#
# write_csv(mini_data_sisa, "mini_data_sisa_deploy.csv")

# data_sisa <- readr::read_csv("data/mini_data_sisa_deploy.csv") |>
#   dplyr::filter(fecha_enfermo >= "2020-05-09" &
#                   "2020-05-14" >= fecha_enfermo) |>
#   dplyr::mutate(residencia_departamento_nombre = stringr::str_to_title(residencia_departamento_nombre),
#                 residencia_departamento_nombre = stringr::str_remove(residencia_departamento_nombre, "0(?!$)"))


# Lista todos los archivos en la carpeta
all_files <- list.files("inst/rasters/")

# Fintra los archivos que terminan con '.tif'
base_raster <- all_files[grep(".tif$",
                              basename(all_files))]  |>
  tibble::as_tibble() |>
  tidyr::separate(value,
                  into = c('locacion',
                           'tipo_de_raster',
                           'fecha',
                           'hora'),
                  sep = '_',
                  remove = FALSE) |>
  dplyr::mutate(fecha = as.Date(fecha),
                hora = as.numeric(stringr::str_sub(hora, end= -4))) |>
  dplyr::mutate(momento = dplyr::case_when(hora == 0 ~ "noche",
                                           hora == 8 ~ "mañana",
                                           hora == 16 ~ "tarde"))



# Permite leer el directorio imagenes dentro de www/'
shiny::addResourcePath(prefix = "imagenes",
                directoryPath = system.file("www/imagenes",
                                            package = "geocovidapp"))

