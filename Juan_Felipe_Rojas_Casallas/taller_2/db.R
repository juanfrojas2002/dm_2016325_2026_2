# db.R - acceso a base de datos
# Juan Felipe Rojas - 2016325
# toda la SQL va aca, la app no tiene queries directo

library(DBI)
library(RSQLite)

DB_PATH <- "comparador_precios.sqlite"

db_connect <- function(path = DB_PATH) {
  dbConnect(SQLite(), path)
}

db_disconnect <- function(con) {
  if (!is.null(con) && dbIsValid(con)) dbDisconnect(con)
}

db_setup <- function(con) {
  dbExecute(con, "CREATE TABLE IF NOT EXISTS comercios (
    comercio_id TEXT PRIMARY KEY,
    nombre TEXT NOT NULL,
    url_base TEXT NOT NULL
  )")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS ejecuciones (
    ejecucion_id TEXT PRIMARY KEY,
    fecha_hora_inicio TEXT NOT NULL,
    comercio_id TEXT NOT NULL,
    n_registros INTEGER DEFAULT 0,
    estado TEXT DEFAULT 'en_curso',
    FOREIGN KEY (comercio_id) REFERENCES comercios(comercio_id)
  )")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS observaciones (
    observacion_id INTEGER PRIMARY KEY AUTOINCREMENT,
    ejecucion_id TEXT NOT NULL,
    comercio_id TEXT NOT NULL,
    producto_id_comercio TEXT,
    sku_comercio TEXT,
    nombre_producto TEXT NOT NULL,
    nombre_estandarizado TEXT,
    marca TEXT,
    marca_estandarizada TEXT,
    subcategoria TEXT,
    presentacion TEXT,
    contenido_valor REAL,
    contenido_unidad TEXT,
    precio REAL,
    precio_regular REAL,
    es_promocion INTEGER DEFAULT 0,
    url TEXT,
    texto_original TEXT,
    fecha_hora_consulta TEXT NOT NULL,
    comparable INTEGER DEFAULT 0,
    producto_comun_id TEXT,
    disponibilidad TEXT DEFAULT 'no_observado',
    FOREIGN KEY (ejecucion_id) REFERENCES ejecuciones(ejecucion_id),
    FOREIGN KEY (comercio_id) REFERENCES comercios(comercio_id),
    UNIQUE(comercio_id, producto_id_comercio, ejecucion_id)
  )")

  dbExecute(con, "CREATE TABLE IF NOT EXISTS correcciones (
    correccion_id INTEGER PRIMARY KEY AUTOINCREMENT,
    fecha TEXT NOT NULL,
    tabla TEXT NOT NULL,
    registro_id TEXT NOT NULL,
    campo TEXT NOT NULL,
    valor_anterior TEXT,
    valor_nuevo TEXT,
    motivo TEXT NOT NULL
  )")

  # migracion: agregar columna si viene de taller 1
  cols <- dbGetQuery(con, "PRAGMA table_info(observaciones)")$name
  if (!"disponibilidad" %in% cols) {
    dbExecute(con, "ALTER TABLE observaciones ADD COLUMN disponibilidad TEXT DEFAULT 'no_observado'")
    dbExecute(con, "UPDATE observaciones SET disponibilidad = CASE
      WHEN precio > 0 THEN 'disponible'
      WHEN precio = 0 OR precio IS NULL THEN 'agotado'
      ELSE 'no_observado' END")
  }

  # indices
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_comercio ON observaciones(comercio_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_comparable ON observaciones(comparable)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_ejecucion ON observaciones(ejecucion_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_marca ON observaciones(marca_estandarizada)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_producto ON observaciones(producto_comun_id)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_precio ON observaciones(precio)")
  dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_disponibilidad ON observaciones(disponibilidad)")

  # vistas
  dbExecute(con, "DROP VIEW IF EXISTS v_ultimo_precio")
  dbExecute(con, "CREATE VIEW v_ultimo_precio AS
    SELECT o.*,
      c.nombre AS comercio_nombre,
      CASE WHEN contenido_valor > 0 THEN ROUND(precio / contenido_valor, 2) ELSE NULL END AS precio_unitario
    FROM observaciones o
    JOIN comercios c ON o.comercio_id = c.comercio_id
    JOIN (
      SELECT comercio_id, producto_id_comercio, MAX(fecha_hora_consulta) AS max_fecha
      FROM observaciones
      GROUP BY comercio_id, producto_id_comercio
    ) ult ON o.comercio_id = ult.comercio_id
      AND o.producto_id_comercio = ult.producto_id_comercio
      AND o.fecha_hora_consulta = ult.max_fecha")

  dbExecute(con, "DROP VIEW IF EXISTS v_cambios_precio")
  dbExecute(con, "CREATE VIEW v_cambios_precio AS
    SELECT
      a.comercio_id,
      c.nombre AS comercio_nombre,
      a.producto_id_comercio,
      a.nombre_producto,
      a.nombre_estandarizado,
      a.marca_estandarizada,
      a.ejecucion_id AS ejecucion_anterior,
      b.ejecucion_id AS ejecucion_nueva,
      a.precio AS precio_anterior,
      b.precio AS precio_nuevo,
      b.precio - a.precio AS diferencia_cop,
      CASE WHEN a.precio > 0 THEN ROUND(100.0 * (b.precio - a.precio) / a.precio, 2) ELSE NULL END AS diferencia_pct
    FROM observaciones a
    JOIN observaciones b ON a.comercio_id = b.comercio_id
      AND a.producto_id_comercio = b.producto_id_comercio
      AND a.ejecucion_id < b.ejecucion_id
    JOIN comercios c ON a.comercio_id = c.comercio_id
    WHERE a.precio IS NOT NULL AND b.precio IS NOT NULL")

  dbExecute(con, "DROP VIEW IF EXISTS v_comparables")
  dbExecute(con, "CREATE VIEW v_comparables AS
    SELECT o.*,
      c.nombre AS comercio_nombre,
      CASE WHEN contenido_valor > 0 THEN ROUND(precio / contenido_valor, 2) ELSE NULL END AS precio_unitario
    FROM observaciones o
    JOIN comercios c ON o.comercio_id = c.comercio_id
    WHERE o.comparable = 1 AND o.precio IS NOT NULL")

  invisible(NULL)
}

db_registrar_comercios <- function(con, tiendas_list) {
  for (t in tiendas_list) {
    dbExecute(con, "INSERT OR IGNORE INTO comercios VALUES (?, ?, ?)",
              params = list(t$id, t$nombre, t$base_url))
  }
}

# correcciones de datos: se aplican por lotes para no repetir
db_aplicar_correcciones <- function(con) {
  fecha <- format(Sys.time(), "%Y-%m-%d")

  # lote 1: unidades y contenido del taller 1
  ya_lote1 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM correcciones WHERE motivo LIKE '%gramos%'")$n
  if (ya_lote1 == 0) {
    correcciones <- list(
      list(campo = "contenido_unidad",
           filtro = "nombre_estandarizado = 'Autan Crema Family 200g' AND contenido_unidad = 'ml'",
           anterior = "ml", nuevo = "g",
           motivo = "La crema Autan se mide en gramos, no mililitros"),
      list(campo = "contenido_unidad",
           filtro = "nombre_estandarizado = 'Nopikex Barra 50g' AND contenido_unidad = 'ml'",
           anterior = "ml", nuevo = "g",
           motivo = "La barra Nopikex se mide en gramos, no mililitros"),
      list(campo = "contenido_valor",
           filtro = "nombre_estandarizado = 'Autan Crema Family 200g' AND contenido_valor = 196",
           anterior = "196", nuevo = "200",
           motivo = "La presentacion oficial es 200g segun empaque"),
      list(campo = "disponibilidad",
           filtro = "precio IS NULL AND disponibilidad != 'agotado'",
           anterior = "no_observado", nuevo = "agotado",
           motivo = "Productos sin precio estaban agotados")
    )
    for (corr in correcciones) {
      afectados <- dbGetQuery(con, sprintf(
        "SELECT observacion_id FROM observaciones WHERE %s", corr$filtro))
      if (nrow(afectados) > 0) {
        dbExecute(con, sprintf(
          "UPDATE observaciones SET %s = ? WHERE %s",
          corr$campo, corr$filtro),
          params = list(corr$nuevo))
        for (rid in afectados$observacion_id) {
          dbExecute(con, "INSERT INTO correcciones (fecha, tabla, registro_id, campo, valor_anterior, valor_nuevo, motivo)
                          VALUES (?, ?, ?, ?, ?, ?, ?)",
                    params = list(fecha, "observaciones", as.character(rid), corr$campo,
                                  corr$anterior, corr$nuevo, corr$motivo))
        }
      }
    }
  }

  # lote 2: precios en 0 pasan a NULL
  ya_precios <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM correcciones WHERE motivo LIKE '%precio en 0%'")$n
  if (ya_precios == 0) {
    afectados_p <- dbGetQuery(con, "SELECT observacion_id FROM observaciones WHERE precio = 0")
    if (nrow(afectados_p) > 0) {
      dbExecute(con, "UPDATE observaciones SET precio = NULL WHERE precio = 0")
      for (rid in afectados_p$observacion_id) {
        dbExecute(con, "INSERT INTO correcciones (fecha, tabla, registro_id, campo, valor_anterior, valor_nuevo, motivo)
                        VALUES (?, ?, ?, ?, ?, ?, ?)",
                  params = list(fecha, "observaciones", as.character(rid), "precio",
                                "0", "NULL", "Precio en 0 no es precio real, se pone NULL"))
      }
    }
    afectados_pr <- dbGetQuery(con, "SELECT observacion_id FROM observaciones WHERE precio_regular = 0")
    if (nrow(afectados_pr) > 0) {
      dbExecute(con, "UPDATE observaciones SET precio_regular = NULL WHERE precio_regular = 0")
      for (rid in afectados_pr$observacion_id) {
        dbExecute(con, "INSERT INTO correcciones (fecha, tabla, registro_id, campo, valor_anterior, valor_nuevo, motivo)
                        VALUES (?, ?, ?, ?, ?, ?, ?)",
                  params = list(fecha, "observaciones", as.character(rid), "precio_regular",
                                "0", "NULL", "Precio regular en 0 no es precio real, se pone NULL"))
      }
    }
  }

  invisible(NULL)
}

db_insertar_ejecucion <- function(con, ejec_id, comercio_id, fecha) {
  dbExecute(con, "INSERT OR IGNORE INTO ejecuciones VALUES (?, ?, ?, 0, 'en_curso')",
            params = list(ejec_id, fecha, comercio_id))
}

db_insertar_observaciones <- function(con, df, ejec_id) {
  if (is.null(df) || nrow(df) == 0) return(0)
  df$ejecucion_id <- ejec_id
  if (!"disponibilidad" %in% names(df)) df$disponibilidad <- "no_observado"
  cols <- c("ejecucion_id", "comercio_id", "producto_id_comercio", "sku_comercio",
            "nombre_producto", "nombre_estandarizado", "marca", "marca_estandarizada",
            "subcategoria", "presentacion", "contenido_valor", "contenido_unidad",
            "precio", "precio_regular", "es_promocion", "url", "texto_original",
            "fecha_hora_consulta", "comparable", "producto_comun_id", "disponibilidad")
  cols_present <- intersect(cols, names(df))
  n0 <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n
  tryCatch({
    dbWriteTable(con, "observaciones", df[, cols_present, drop = FALSE],
                 append = TRUE, row.names = FALSE)
    dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n - n0
  }, error = function(e) {
    message("Error insercion: ", e$message)
    0L
  })
}

db_finalizar_ejecucion <- function(con, ejec_id, n_registros, estado = "completa") {
  dbExecute(con, "UPDATE ejecuciones SET n_registros = ?, estado = ? WHERE ejecucion_id = ?",
            params = list(n_registros, estado, ejec_id))
}

# consultas para la app

db_get_comercios <- function(con) {
  dbGetQuery(con, "SELECT * FROM comercios ORDER BY nombre")
}

db_get_ejecuciones <- function(con) {
  dbGetQuery(con, "SELECT e.*, c.nombre AS comercio_nombre
    FROM ejecuciones e JOIN comercios c ON e.comercio_id = c.comercio_id
    ORDER BY e.fecha_hora_inicio DESC")
}

db_get_productos <- function(con, comercio_ids = NULL, subcategorias = NULL,
                              marcas = NULL, precio_min = NULL, precio_max = NULL,
                              ejecucion_ids = NULL, solo_promocion = FALSE,
                              solo_comparable = FALSE, busqueda = NULL,
                              solo_disponible = FALSE) {
  where <- c("1=1")
  params <- list()

  if (!is.null(comercio_ids) && length(comercio_ids) > 0) {
    ph <- paste(rep("?", length(comercio_ids)), collapse = ",")
    where <- c(where, sprintf("o.comercio_id IN (%s)", ph))
    params <- c(params, as.list(comercio_ids))
  }
  if (!is.null(subcategorias) && length(subcategorias) > 0) {
    ph <- paste(rep("?", length(subcategorias)), collapse = ",")
    where <- c(where, sprintf("o.subcategoria IN (%s)", ph))
    params <- c(params, as.list(subcategorias))
  }
  if (!is.null(marcas) && length(marcas) > 0) {
    ph <- paste(rep("?", length(marcas)), collapse = ",")
    where <- c(where, sprintf("o.marca_estandarizada IN (%s)", ph))
    params <- c(params, as.list(marcas))
  }
  if (!is.null(precio_min)) {
    where <- c(where, "o.precio >= ?")
    params <- c(params, list(precio_min))
  }
  if (!is.null(precio_max)) {
    where <- c(where, "o.precio <= ?")
    params <- c(params, list(precio_max))
  }
  if (!is.null(ejecucion_ids) && length(ejecucion_ids) > 0) {
    ph <- paste(rep("?", length(ejecucion_ids)), collapse = ",")
    where <- c(where, sprintf("o.ejecucion_id IN (%s)", ph))
    params <- c(params, as.list(ejecucion_ids))
  }
  if (solo_promocion) where <- c(where, "o.es_promocion = 1")
  if (solo_comparable) where <- c(where, "o.comparable = 1")
  if (solo_disponible) where <- c(where, "o.disponibilidad = 'disponible'")
  if (!is.null(busqueda) && nchar(trimws(busqueda)) > 0) {
    where <- c(where, "(o.nombre_producto LIKE ? OR o.marca LIKE ?)")
    params <- c(params, list(paste0("%", busqueda, "%"), paste0("%", busqueda, "%")))
  }

  sql <- sprintf("
    SELECT o.observacion_id, o.comercio_id, c.nombre AS comercio_nombre,
      o.nombre_producto, o.nombre_estandarizado, o.marca, o.marca_estandarizada,
      o.subcategoria, o.presentacion, o.contenido_valor, o.contenido_unidad,
      o.precio, o.precio_regular, o.es_promocion, o.disponibilidad,
      o.url, o.fecha_hora_consulta, o.comparable, o.producto_comun_id,
      o.ejecucion_id,
      CASE WHEN o.contenido_valor > 0 THEN ROUND(o.precio / o.contenido_valor, 2) ELSE NULL END AS precio_unitario
    FROM observaciones o
    JOIN comercios c ON o.comercio_id = c.comercio_id
    WHERE %s
    ORDER BY o.nombre_producto, c.nombre", paste(where, collapse = " AND "))

  if (length(params) > 0) dbGetQuery(con, sql, params = params) else dbGetQuery(con, sql)
}

# TODO: esta query es un monstruo, separar en varias mas chicas
db_get_indicadores <- function(con, comercio_ids = NULL) {
  where_com <- ""
  params <- list()
  if (!is.null(comercio_ids) && length(comercio_ids) > 0) {
    ph <- paste(rep("?", length(comercio_ids)), collapse = ",")
    where_com <- sprintf("WHERE o.comercio_id IN (%s)", ph)
    params <- as.list(comercio_ids)
  }

  sql <- sprintf("
    SELECT
      COUNT(DISTINCT o.producto_id_comercio || o.comercio_id) AS total_productos,
      COUNT(*) AS total_observaciones,
      COUNT(DISTINCT o.comercio_id) AS n_comercios,
      (SELECT MAX(fecha_hora_inicio) FROM ejecuciones) AS ultima_ejecucion,
      ROUND(100.0 * SUM(CASE WHEN o.es_promocion = 1 THEN 1 ELSE 0 END) / COUNT(*), 1) AS pct_promocion,
      CAST(ROUND((SELECT AVG(precio) FROM observaciones WHERE precio IS NOT NULL)) AS INTEGER) AS mediana_precio,
      (SELECT MAX(precio) - MIN(precio) FROM observaciones WHERE comparable = 1 AND precio IS NOT NULL
       GROUP BY nombre_estandarizado ORDER BY MAX(precio) - MIN(precio) DESC LIMIT 1) AS ahorro_maximo,
      (SELECT COUNT(*) FROM v_cambios_precio WHERE diferencia_cop <> 0) AS cambios_precio
    FROM observaciones o %s", where_com)

  if (length(params) > 0) dbGetQuery(con, sql, params = params) else dbGetQuery(con, sql)
}

db_get_marcas <- function(con) {
  dbGetQuery(con, "SELECT DISTINCT marca_estandarizada FROM observaciones
    WHERE marca_estandarizada IS NOT NULL AND marca_estandarizada != ''
    ORDER BY marca_estandarizada")$marca_estandarizada
}

db_get_subcategorias <- function(con) {
  dbGetQuery(con, "SELECT DISTINCT subcategoria FROM observaciones
    WHERE subcategoria IS NOT NULL AND subcategoria != ''
    ORDER BY subcategoria")$subcategoria
}

db_get_rango_precios <- function(con) {
  dbGetQuery(con, "SELECT MIN(precio) AS min_precio, MAX(precio) AS max_precio
    FROM observaciones WHERE precio IS NOT NULL")
}

db_get_comparables <- function(con) {
  dbGetQuery(con, "SELECT * FROM v_comparables ORDER BY producto_comun_id, comercio_id")
}

db_get_evolucion_precio <- function(con, producto_comun_id) {
  dbGetQuery(con, "
    SELECT o.producto_comun_id, o.nombre_estandarizado,
      c.nombre AS comercio_nombre, o.precio,
      e.fecha_hora_inicio AS fecha, o.ejecucion_id
    FROM observaciones o
    JOIN comercios c ON o.comercio_id = c.comercio_id
    JOIN ejecuciones e ON o.ejecucion_id = e.ejecucion_id
    WHERE o.comparable = 1 AND o.precio IS NOT NULL AND o.producto_comun_id = ?
    ORDER BY e.fecha_hora_inicio, c.nombre",
    params = list(producto_comun_id))
}

db_get_cambios_precio <- function(con, ejec_anterior, ejec_nueva, umbral_pct = 0) {
  dbGetQuery(con, "
    SELECT
      a.comercio_id, c.nombre AS comercio_nombre,
      a.nombre_producto, a.nombre_estandarizado, a.marca_estandarizada,
      a.precio AS precio_anterior, b.precio AS precio_nuevo,
      b.precio - a.precio AS diferencia_cop,
      CASE WHEN a.precio > 0 THEN ROUND(100.0 * (b.precio - a.precio) / a.precio, 2) ELSE NULL END AS diferencia_pct
    FROM observaciones a
    JOIN observaciones b ON a.comercio_id = b.comercio_id
      AND a.producto_id_comercio = b.producto_id_comercio
    JOIN comercios c ON a.comercio_id = c.comercio_id
    WHERE a.ejecucion_id = ? AND b.ejecucion_id = ?
      AND a.precio IS NOT NULL AND b.precio IS NOT NULL
      AND ABS(CASE WHEN a.precio > 0 THEN 100.0 * (b.precio - a.precio) / a.precio ELSE 0 END) >= ?
    ORDER BY diferencia_pct DESC",
    params = list(ejec_anterior, ejec_nueva, umbral_pct))
}

db_get_canasta <- function(con, producto_ids) {
  if (length(producto_ids) == 0) return(data.frame())
  ph <- paste(rep("?", length(producto_ids)), collapse = ",")
  dbGetQuery(con, sprintf("
    SELECT v.producto_comun_id, v.nombre_estandarizado, v.comercio_id,
      v.comercio_nombre, v.precio, v.precio_unitario
    FROM v_comparables v
    JOIN (
      SELECT producto_comun_id, comercio_id, MAX(fecha_hora_consulta) AS max_f
      FROM observaciones WHERE comparable = 1 AND precio IS NOT NULL
      GROUP BY producto_comun_id, comercio_id
    ) ult ON v.producto_comun_id = ult.producto_comun_id
      AND v.comercio_id = ult.comercio_id
      AND v.fecha_hora_consulta = ult.max_f
    WHERE v.producto_comun_id IN (%s)
    ORDER BY v.producto_comun_id, v.precio", ph),
    params = as.list(producto_ids))
}

db_get_precio_unitario_comparables <- function(con) {
  dbGetQuery(con, "
    SELECT v.producto_comun_id, v.nombre_estandarizado,
      v.comercio_nombre, v.precio, v.contenido_valor, v.contenido_unidad,
      v.precio_unitario
    FROM v_comparables v
    WHERE v.contenido_valor > 0 AND v.contenido_unidad IN ('ml', 'g')
    ORDER BY v.contenido_unidad, v.nombre_estandarizado, v.precio_unitario")
}

db_get_distribucion_precios <- function(con, comercio_ids = NULL) {
  where <- "precio IS NOT NULL"
  params <- list()
  if (!is.null(comercio_ids) && length(comercio_ids) > 0) {
    ph <- paste(rep("?", length(comercio_ids)), collapse = ",")
    where <- sprintf("%s AND o.comercio_id IN (%s)", where, ph)
    params <- as.list(comercio_ids)
  }
  sql <- sprintf("
    SELECT o.precio, c.nombre AS comercio_nombre, o.comercio_id
    FROM observaciones o
    JOIN comercios c ON o.comercio_id = c.comercio_id
    WHERE %s", where)
  if (length(params) > 0) dbGetQuery(con, sql, params = params) else dbGetQuery(con, sql)
}

db_productos_nuevos_desaparecidos <- function(con, ejec_anterior, ejec_nueva) {
  nuevos <- dbGetQuery(con, "
    SELECT b.comercio_id, c.nombre AS comercio_nombre, b.nombre_producto, b.precio
    FROM observaciones b
    JOIN comercios c ON b.comercio_id = c.comercio_id
    WHERE b.ejecucion_id = ?
      AND NOT EXISTS (
        SELECT 1 FROM observaciones a
        WHERE a.ejecucion_id = ? AND a.comercio_id = b.comercio_id
          AND a.producto_id_comercio = b.producto_id_comercio)",
    params = list(ejec_nueva, ejec_anterior))

  desaparecidos <- dbGetQuery(con, "
    SELECT a.comercio_id, c.nombre AS comercio_nombre, a.nombre_producto, a.precio
    FROM observaciones a
    JOIN comercios c ON a.comercio_id = c.comercio_id
    WHERE a.ejecucion_id = ?
      AND NOT EXISTS (
        SELECT 1 FROM observaciones b
        WHERE b.ejecucion_id = ? AND b.comercio_id = a.comercio_id
          AND b.producto_id_comercio = a.producto_id_comercio)",
    params = list(ejec_anterior, ejec_nueva))

  list(nuevos = nuevos, desaparecidos = desaparecidos)
}

db_ejecuciones_por_comercio <- function(con, comercio_id) {
  dbGetQuery(con, "SELECT ejecucion_id, fecha_hora_inicio, n_registros, estado
    FROM ejecuciones WHERE comercio_id = ? ORDER BY fecha_hora_inicio",
    params = list(comercio_id))
}
