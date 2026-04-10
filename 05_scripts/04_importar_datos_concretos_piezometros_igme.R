# 05 leer datos concretos piezómetros (por masa de agua)


# Cargar librerías -------------------------------------------------------------
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)

# Leer nombres archivos --------------------------------------------------------
nombres_xlsx_ruta <- list.files(
  path = "03_datos_brutos/datos_piezometros/campo_dalias_sierra_gador", 
  pattern = ".xlsx",
  full.names = T) # Obtener ruta completa

nombres_xlsx <- list.files(
  path = "03_datos_brutos/datos_piezometros/campo_dalias_sierra_gador",
  pattern = ".xlsx",
  full.names = F) |> # Obtener nombre del archivo
  str_remove_all(pattern = ".xlsx") |> 
  str_remove_all(pattern = "AGPZ_")



# Crear bucle que lea todos los archivos ---------------------------------------
l_datos <- rep(list(NA),length(nombres_xlsx_ruta))  # Inicializar lista

for (i in 1:length(nombres_xlsx_ruta)) {
  df <- read_xlsx(nombres_xlsx_ruta[i], skip=1) # Leer cada dataframe y guardar como objeto
  df <- df[,-c(1)] # Eliminar las columnas 1 (info repetida fecha)
  colnames(df) <- make_clean_names(colnames(df)) # Tomar como nombres de las variables la fila 2 y pasarlos a formato r
  df <- type.convert(df, as.is = TRUE) # Volver a inferir el tipo de variables
  l_datos[[i]] <- df
  names(l_datos)[[i]] <- nombres_xlsx[i]
}  # Crear una lista en la que cada elemento sea un dataframe y el nombre sea el del archivo



# Pasar lista a tibble ---------------------------------------------------------
datos <- 
  enframe(l_datos, name="id_piezometro", value="contenido") |> 
  unnest_wider(contenido) |> 
  unnest(c(fecha_2, profundidad_del_agua_m, cota_punto, nivel_piezometrico_m_s_n_m,
           tipo_surgencia)) 


# HASTA AQUÍ ESTÁ BIEN ----------------------------------------------------------

# Filtrar los piezómetros que tengan más de una medida por año --------------------
datos$fecha_2 <- as.Date(datos$fecha_2, format = "%d/%m/%y") # Establecer formato fecha
datos |> 
  group_by(id_piezometro) |> 
  summarise(fecha_2 = n()) |> 
  filter(fecha_2 > 1) |> 
  ungroup()
datos <- datos |> 
  mutate(anno = year(fecha_2)) 

 
  group_by(id_piezometro, anno) |> 
  summarise(cota_np_media = mean(nivel_piezometrico_m_s_n_m, na.rm=T)) |> 
  print(n = 61)

  
  
# Exportar archivo -------------------------------------------------------------
write.csv(datos, "04_datos_procesados/piezometros/base_piezometros_igme.csv")