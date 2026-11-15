# scraper.R - funciones de scraping
# Juan Felipe Rojas - 2016325

library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(purrr)

VTEX_BATCH <- 50

tiendas <- list(
  la_rebaja = list(
    id = "la_rebaja",
    nombre = "La Rebaja Virtual",
    base_url = "https://www.larebajavirtual.com",
    paths = c("cuidado-personal/cuidado-corporal/repelentes"),
    search_terms = c("repelente", "insecticida")
  ),
  exito = list(
    id = "exito",
    nombre = "Exito",
    base_url = "https://www.exito.com",
    paths = c(
      "cuidado-personal/proteccion-solar-y-repelentes/repelentes",
      "mercado/aseo-del-hogar/ambientadores-e-insecticidas"
    ),
    search_terms = NULL
  ),
  jumbo = list(
    id = "jumbo",
    nombre = "Jumbo Colombia",
    base_url = "https://www.jumbocolombia.com",
    paths = c(
      "supermercado/belleza/repelentes",
      "supermercado/aseo-de-hogar/insecticidas"
    ),
    search_terms = NULL
  )
)

# los 9 productos que encontre en las 3 tiendas
comparables_ref <- tibble(
  id = paste0("P0", 1:9),
  nombre = c(
    "Stay Off Spray Adulto 120ml", "Stay Off Amazonic Spray 120ml",
    "Autan Crema Family 200g", "Autan Aerosol 177ml",
    "Nopikex Liquido 120ml", "Nopikex Barra 50g",
    "Bacterion Family Spray 150ml", "Stay Off Ninos Spray 120ml",
    "Bacterion Kids Crema 120g"
  )
)

`%||%` <- function(x, y) if (is.null(x)) y else x

colombia_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S-05:00", tz = "America/Bogota")
}

gen_run_id <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S", tz = "America/Bogota")
}

# guardar json crudo en raw/
guardar_raw <- function(texto, store_id, run_id, cat_path, pag) {
  dir_raw <- file.path("raw", store_id, run_id)
  if (!dir.exists(dir_raw)) dir.create(dir_raw, recursive = TRUE)
  slug <- str_replace_all(cat_path, "[/?=&%]", "_")
  slug <- str_replace_all(slug, "_+", "_") |> str_remove("^_") |> str_remove("_$")
  fp <- file.path(dir_raw, sprintf("%s_p%02d.json", slug, pag))
  writeLines(texto, fp, useBytes = TRUE)
  fp
}

# cargar json del disco
cargar_raw <- function(store_id, run_id, cat_path, pag) {
  slug <- str_replace_all(cat_path, "[/?=&%]", "_")
  slug <- str_replace_all(slug, "_+", "_") |> str_remove("^_") |> str_remove("_$")
  fp <- file.path("raw", store_id, run_id, sprintf("%s_p%02d.json", slug, pag))
  if (file.exists(fp)) paste(readLines(fp, warn = FALSE), collapse = "\n") else NULL
}

ultimo_run <- function(store_id) {
  d <- file.path("raw", store_id)
  if (!dir.exists(d)) return(NULL)
  runs <- sort(list.dirs(d, full.names = FALSE, recursive = FALSE), decreasing = TRUE)
  if (length(runs) > 0) runs[1] else NULL
}

# consulta a la api vtex con reintentos
fetch_vtex <- function(base_url, path, from, to) {
  # armar la url segun si es busqueda o categoria
  if (startsWith(path, "?ft=")) {
    url <- sprintf("%s/api/catalog_system/pub/products/search/%s&_from=%d&_to=%d",
                   base_url, path, from, to)
  } else {
    url <- sprintf("%s/api/catalog_system/pub/products/search/%s?_from=%d&_to=%d",
                   base_url, path, from, to)
  }

  resp <- NULL
  for (intento in 1:3) {
    resp <- tryCatch(
      request(url) |>
        req_headers(Accept = "application/json", `Content-Type` = "application/json") |>
        req_user_agent("ComparadorPrecios/2.0 (Academico)") |>
        req_timeout(seconds = 60) |>
        req_error(is_error = function(resp) FALSE) |>
        req_perform(),
      error = function(e) { message("  Error conexion: ", e$message); NULL }
    )
    if (!is.null(resp) && resp_status(resp) %in% c(200L, 206L)) break
    if (intento < 3) { message("  Reintentando..."); Sys.sleep(3) }
  }

  if (is.null(resp) || !(resp_status(resp) %in% c(200L, 206L))) {
    return(list(text = NULL, total = 0))
  }

  # sacar el total del header resources
  total <- 0
  rh <- resp_header(resp, "resources")
  if (!is.null(rh)) {
    partes <- str_split_1(rh, "/")
    if (length(partes) == 2) total <- as.integer(partes[2])
  }
  list(text = resp_body_string(resp), total = total)
}


# recorre todas las paginas de una categoria
fetch_todos <- function(base_url, path, store_id, run_id, offline = FALSE) {
  message("  Cat: ", path)
  productos <- list()
  pag <- 1
  desde <- 0

  repeat {
    hasta <- desde + VTEX_BATCH - 1
    if (offline) {
      json_txt <- cargar_raw(store_id, run_id, path, pag)
      if (is.null(json_txt)) break
    } else {
      res <- fetch_vtex(base_url, path, desde, hasta)
      json_txt <- res$text
      if (is.null(json_txt)) break
      if (res$total > 0 && pag == 1) message("    Total: ", res$total)
      guardar_raw(json_txt, store_id, run_id, path, pag)
      # pausa entre peticiones
      Sys.sleep(runif(1, 1, 2.5))
    }

    prods <- tryCatch(fromJSON(json_txt, simplifyDataFrame = FALSE),
                      error = function(e) list())
    if (length(prods) == 0) break
    productos <- c(productos, prods)
    message("    Pag ", pag, ": ", length(prods), " (acum: ", length(productos), ")")
    if (length(prods) < VTEX_BATCH) break
    desde <- desde + VTEX_BATCH
    pag <- pag + 1
  }
  productos
}

# sacar contenido numerico del nombre (ej: "120ml" -> 120, "ml")
extraer_contenido <- function(txt) {
  if (is.na(txt) || is.null(txt) || nchar(txt) == 0)
    return(list(valor = NA_real_, unidad = NA_character_))
  m <- str_match(txt, "(\\d+\\.?\\d*)\\s*(ml|mL|ML|g|G|gr|grs|GR|GRS|kg|KG|mg|MG|lt|LT|l|L|cc|CC|oz|OZ)")
  if (!is.na(m[1,1])) {
    u <- tolower(m[1,3])
    u <- switch(u, "grs" = "g", "gr" = "g", "lt" = "l", "cc" = "ml", u)
    return(list(valor = as.numeric(m[1,2]), unidad = u))
  }
  list(valor = NA_real_, unidad = NA_character_)
}

# ojo: la categoria de ambientadores trae de todo, toca filtrar
es_repelente <- function(nombre, marca) {
  n <- tolower(nombre %||% "")
  m <- tolower(marca %||% "")
  # lo que NO es repelente (sale mezclado en aseo del hogar)
  excluir <- c("ambientador", "aromatiz", "desodorante.*(ambiental|hogar)",
    "difusor", "vela.*(arom|perfum)", "incienso",
    "shampoo", "collar.*(pulga|garrapata)", "raticida",
    "limpia", "jabon", "detergente", "suavizante", "blanqueador",
    "desinfectan", "cloro", "lava(platos|dora)", "cera")
  incluir <- c("repelente", "insecticida", "mata.?insect", "mata.?mosqu",
    "mata.?cucarach", "antimosquit", "anti.?mosquit",
    "espiral.*(mosqu|insect)", "citronela",
    "aerosol.*(insect|cucarach|mosqu|hormig)")
  marcas_ok <- c("\\braid\\b", "\\bbaygon\\b", "stay.?off", "\\bnopikex\\b",
    "\\bautan\\b", "\\bbacterion\\b", "\\boff!?\\b")
  if (str_detect(n, paste0(excluir, collapse = "|"))) return(FALSE)
  if (str_detect(n, paste0(incluir, collapse = "|"))) return(TRUE)
  if (str_detect(m, paste0(marcas_ok, collapse = "|"))) return(TRUE)
  if (str_detect(n, paste0(marcas_ok, collapse = "|"))) return(TRUE)
  FALSE
}

# parsear la respuesta de vtex a un dataframe
parsear_productos <- function(lista, comercio_id, comercio_nombre, fecha, filtrar = FALSE) {
  filas <- map(lista, function(p) {
    tryCatch({
      precio <- NA_real_; precio_reg <- NA_real_; sku <- NA_character_
      disponible <- "no_observado"

      if (length(p$items) > 0) {
        item <- p$items[[1]]
        sku <- as.character(item$itemId %||% NA)
        if (length(item$sellers) > 0) {
          oferta <- item$sellers[[1]]$commertialOffer
          if (!is.null(oferta)) {
            precio <- as.numeric(oferta$Price %||% NA)
            precio_reg <- as.numeric(oferta$ListPrice %||% NA)
            # precio 0 no es precio real
            if (!is.na(precio) && precio == 0) precio <- NA_real_
            if (!is.na(precio_reg) && precio_reg == 0) precio_reg <- NA_real_
            qty <- as.numeric(oferta$AvailableQuantity %||% 0)
            disponible <- if (!is.na(qty) && qty > 0 && !is.na(precio)) "disponible" else "agotado"
          }
        }
      }

      # extraer contenido y presentacion
      cont_val <- NA_real_; cont_ud <- NA_character_; pres <- NA_character_
      cv <- p[["Cantidadunidadesmedida"]]
      if (!is.null(cv) && length(cv) > 0) cont_val <- as.numeric(cv[[1]])
      um <- p[["Unidadmedida"]]
      if (!is.null(um) && length(um) > 0) {
        cont_ud <- tolower(um[[1]])
        cont_ud <- switch(cont_ud,
          "mililitro"="ml", "mililitros"="ml", "gramo"="g", "gramos"="g",
          "kilogramo"="kg", "litro"="l", "litros"="l", cont_ud)
      }
      pum <- p[["Presentacionunidadmedida"]]
      if (!is.null(pum) && length(pum) > 0) pres <- pum[[1]]
      if (is.na(cont_val) || is.na(cont_ud)) {
        ext <- extraer_contenido(pres %||% (p$productName %||% ""))
        if (is.na(cont_val)) cont_val <- ext$valor
        if (is.na(cont_ud)) cont_ud <- ext$unidad
        if (is.na(cont_val) || is.na(cont_ud)) {
          ext2 <- extraer_contenido(p$productName %||% "")
          if (is.na(cont_val)) cont_val <- ext2$valor
          if (is.na(cont_ud)) cont_ud <- ext2$unidad
        }
      }
      if (is.na(pres)) pres <- p$productName %||% NA_character_

      subcat <- NA_character_
      if (length(p$categories) > 0) subcat <- p$categories[[1]]

      nom <- p$productName %||% NA_character_
      mar <- p$brand %||% NA_character_
      if (filtrar && !es_repelente(nom, mar)) return(NULL)

      desc <- p$description %||% ""
      txt_orig <- if (nchar(desc) > 0 && desc != nom) paste(nom, desc, sep = " | ") else nom

      tibble(
        comercio_id = comercio_id,
        producto_id_comercio = as.character(p$productId %||% NA),
        sku_comercio = sku,
        nombre_producto = nom,
        nombre_estandarizado = NA_character_,
        marca = mar,
        marca_estandarizada = NA_character_,
        subcategoria = subcat,
        presentacion = pres,
        contenido_valor = cont_val,
        contenido_unidad = cont_ud,
        precio = precio,
        precio_regular = precio_reg,
        es_promocion = as.integer(!is.na(precio) & !is.na(precio_reg) & precio < precio_reg),
        url = p$link %||% NA_character_,
        texto_original = txt_orig,
        fecha_hora_consulta = fecha,
        comparable = 0L,
        producto_comun_id = NA_character_,
        disponibilidad = disponible
      )
    }, error = function(e) NULL)
  })
  bind_rows(Filter(Negate(is.null), filas))
}

# clasifica si es uno de los 9 comparables
clasificar <- function(row) {
  n  <- tolower(row["nombre_producto"])
  m  <- tolower(row["marca"])
  cv <- as.numeric(row["contenido_valor"])
  cu <- tolower(row["contenido_unidad"])
  if (grepl("stay.?off", m)) {
    if (grepl("adulto", n) && grepl("spray|ato", n) &&
        !grepl("roll|amazonic|crema", n) &&
        !is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") return("P01")
    if (grepl("amazonic", n) && grepl("spray|ato", n) && !grepl("roll|crema", n) &&
        !is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") return("P02")
    if ((grepl("ni.o|nino|kids", n) || grepl("fspray", n)) &&
        grepl("spray|fspray|ato", n) && !grepl("amazonic|roll|bebe|baby", n)) {
      if ((!is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") ||
          (!is.na(cu) && cu == "unidad")) return("P08")
    }
  }
  if (grepl("autan", m)) {
    if (grepl("crema|cream", n) && !grepl("kids|ni.o", n) &&
        !is.na(cv) && cv >= 190 && cv <= 210 && cu %in% c("g", "ml")) return("P03")
    if (grepl("aerosol|spray|liquido|liquid|iquido", n) && !grepl("crema", n) &&
        !is.na(cv) && cv >= 170 && cv <= 180 && cu == "ml") return("P04")
  }
  if (grepl("nopikex", m) || grepl("nopikex|nopikes", n)) {
    if (!grepl("barra|solid", n) && !grepl("ni.o|nino|kids", n) &&
        !is.na(cv) && cv >= 110 && cv <= 130 && cu == "ml") return("P05")
    if (!is.na(cv) && cv >= 45 && cv <= 55 &&
        (grepl("barra|solid", n) || cu == "g")) return("P06")
  }
  if (grepl("bacterion", m)) {
    if (grepl("family|familiar", n) && !grepl("kids|baby|botanik|selvatic", n) &&
        !is.na(cv) && cv >= 140 && cv <= 160 && cu == "ml") return("P07")
    if (grepl("kids|ni.o|nino", n) && grepl("crema", n) &&
        !is.na(cv) && cv >= 110 && cv <= 160 && cu %in% c("g", "ml")) return("P09")
  }
  NA_character_
}

estandarizar_marca <- function(marca) {
  ml <- tolower(marca)
  case_when(
    str_detect(ml, "stay.?off") ~ "Stay Off",
    str_detect(ml, "^autan$")   ~ "Autan",
    str_detect(ml, "^nopikex$") ~ "Nopikex",
    str_detect(ml, "^bacterion$") ~ "Bacterion",
    str_detect(ml, "^raid$")    ~ "Raid",
    str_detect(ml, "^baygon$")  ~ "Baygon",
    str_detect(ml, "^off!?$")   ~ "Off!",
    str_detect(ml, "^repeblanc$") ~ "Repeblanc",
    TRUE ~ marca
  )
}

# asignar comparable y estandarizar marca/unidad
aplicar_clasificacion <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df$producto_comun_id <- apply(df, 1, clasificar)
  df$comparable <- as.integer(!is.na(df$producto_comun_id))
  df$nombre_estandarizado <- comparables_ref$nombre[match(df$producto_comun_id, comparables_ref$id)]
  df$marca_estandarizada <- estandarizar_marca(df$marca)
  # normalizar unidades
  cu <- tolower(df$contenido_unidad)
  cu[cu %in% c("grs", "gr", "gramo", "gramos")] <- "g"
  cu[cu %in% c("mililitro", "mililitros")] <- "ml"
  cu[cu == "cc"] <- "ml"
  df$contenido_unidad <- cu
  df
}

scrapear_tienda <- function(tienda, offline = FALSE, run_id = NULL) {
  message("\n=== ", tienda$nombre, " ===")
  if (is.null(run_id)) run_id <- gen_run_id()
  if (offline) {
    run_id <- ultimo_run(tienda$id)
    if (is.null(run_id)) { message("  No hay datos guardados"); return(NULL) }
  }

  fecha <- colombia_now()
  datos <- list()

  for (path in tienda$paths) {
    hay_que_filtrar <- grepl("ambientador", path)
    raw <- fetch_todos(tienda$base_url, path, tienda$id, run_id, offline)
    if (length(raw) == 0) next
    df <- parsear_productos(raw, tienda$id, tienda$nombre, fecha, filtrar = hay_que_filtrar)
    if (nrow(df) > 0) datos[[length(datos) + 1]] <- df
  }

  # busqueda por terminos si la tienda lo necesita
  if (!is.null(tienda$search_terms)) {
    for (term in tienda$search_terms) {
      sp <- sprintf("?ft=%s", URLencode(term, reserved = TRUE))
      raw <- fetch_todos(tienda$base_url, sp, tienda$id, run_id, offline)
      if (length(raw) == 0) next
      df <- parsear_productos(raw, tienda$id, tienda$nombre, fecha, filtrar = TRUE)
      if (nrow(df) > 0) datos[[length(datos) + 1]] <- df
    }
  }

  if (length(datos) == 0) return(NULL)
  resultado <- bind_rows(datos)
  resultado <- resultado[!duplicated(resultado$producto_id_comercio), ]
  resultado <- aplicar_clasificacion(resultado)
  #print(head(resultado[, c("nombre_producto", "precio", "comparable")]))
  message("  Unicos: ", nrow(resultado))
  list(data = resultado, run_id = run_id)
}

# convertir texto de precio a numero
precio_texto_a_numero <- function(txt) {
  if (is.na(txt) || is.null(txt)) return(NA_real_)
  txt <- str_remove_all(as.character(txt), "[^0-9.,]")
  txt <- str_remove_all(txt, "\\.")
  txt <- str_replace(txt, ",", ".")
  as.numeric(txt)
}

normalizar_unidad <- function(valor, unidad) {
  u <- tolower(unidad)
  if (is.na(u) || is.na(valor)) return(list(valor = valor, unidad = u))
  if (u == "kg") return(list(valor = valor * 1000, unidad = "g"))
  if (u == "l")  return(list(valor = valor * 1000, unidad = "ml"))
  if (u == "oz") return(list(valor = valor * 29.5735, unidad = "ml"))
  list(valor = valor, unidad = u)
}
