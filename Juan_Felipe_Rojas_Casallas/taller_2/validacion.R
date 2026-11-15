# validacion.R - validacion con pointblank
# Juan Felipe Rojas - 2016325

library(pointblank)

validar_productos <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(valido = FALSE, datos = df, reporte = "Sin datos"))
  }

  agent <- create_agent(tbl = df, actions = action_levels(warn_at = 0.1, stop_at = 0.5)) |>
    col_exists(columns = c("comercio_id", "nombre_producto", "precio", "fecha_hora_consulta")) |>
    col_vals_not_null(columns = c("comercio_id", "nombre_producto", "fecha_hora_consulta")) |>
    col_vals_gt(columns = precio, value = 0, na_pass = TRUE) |>
    col_vals_not_null(columns = producto_id_comercio) |>
    rows_distinct(columns = c("comercio_id", "producto_id_comercio")) |>
    interrogate()

  n_pass <- sum(agent$validation_set$all_passed, na.rm = TRUE)
  n_total <- nrow(agent$validation_set)
  reporte <- sprintf("Validacion: %d/%d reglas OK (%d filas)", n_pass, n_total, nrow(df))

  datos_ok <- df[!is.na(df$nombre_producto) & nchar(df$nombre_producto) > 0, ]
  datos_ok <- datos_ok[!duplicated(datos_ok[, c("comercio_id", "producto_id_comercio")]), ]

  list(
    valido = n_pass >= (n_total - 1),
    datos = datos_ok,
    reporte = reporte,
    n_original = nrow(df),
    n_valido = nrow(datos_ok),
    n_rechazado = nrow(df) - nrow(datos_ok)
  )
}

validar_comparables <- function(df, comercio_id) {
  if (is.null(df) || nrow(df) == 0) return(list(ok = FALSE, n = 0))
  comp <- df[df$comparable == 1 & !is.na(df$precio), ]
  n <- nrow(comp)
  list(ok = n >= 8, n = n)
}

validar_texto_input <- function(txt, max_len = 200) {
  if (is.null(txt) || is.na(txt)) return("")
  txt <- as.character(txt)
  txt <- substr(txt, 1, max_len)
  txt <- str_remove_all(txt, "[;'\"\\\\]")
  str_squish(txt)
}

validar_rango_precio <- function(min_val, max_val) {
  min_val <- suppressWarnings(as.numeric(min_val))
  max_val <- suppressWarnings(as.numeric(max_val))
  if (is.na(min_val) || min_val < 0) min_val <- 0
  if (is.na(max_val) || max_val < 0) max_val <- Inf
  if (min_val > max_val) {
    tmp <- min_val; min_val <- max_val; max_val <- tmp
  }
  list(min = min_val, max = max_val)
}
