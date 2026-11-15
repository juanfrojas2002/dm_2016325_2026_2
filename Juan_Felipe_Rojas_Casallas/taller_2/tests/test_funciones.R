# test_funciones.R - pruebas automatizadas
# Juan Felipe Rojas - 2016325

library(testthat)

source(file.path("..", "scraper.R"), local = TRUE)
source(file.path("..", "db.R"), local = TRUE)
source(file.path("..", "validacion.R"), local = TRUE)

## precios
test_that("precio_texto_a_numero convierte bien los formatos", {
  expect_equal(precio_texto_a_numero("$15.600"), 15600)
  expect_equal(precio_texto_a_numero("23,500"), 23.5)
  expect_equal(precio_texto_a_numero("12345"), 12345)
  expect_true(is.na(precio_texto_a_numero(NA)))
  expect_true(is.na(precio_texto_a_numero(NULL)))
})

test_that("normalizar_unidad convierte kg a g y l a ml", {
  res_kg <- normalizar_unidad(1.5, "kg")
  expect_equal(res_kg$valor, 1500)
  expect_equal(res_kg$unidad, "g")

  res_l <- normalizar_unidad(0.5, "l")
  expect_equal(res_l$valor, 500)
  expect_equal(res_l$unidad, "ml")

  res_ml <- normalizar_unidad(120, "ml")
  expect_equal(res_ml$valor, 120)
  expect_equal(res_ml$unidad, "ml")

  res_na <- normalizar_unidad(NA, NA)
  expect_true(is.na(res_na$valor))
})

test_that("extraer_contenido detecta ml y g en nombres", {
  r1 <- extraer_contenido("Stay Off Spray Adulto 120ml")
  expect_equal(r1$valor, 120)
  expect_equal(r1$unidad, "ml")

  r2 <- extraer_contenido("Autan Crema Family 200g")
  expect_equal(r2$valor, 200)
  expect_equal(r2$unidad, "g")

  r3 <- extraer_contenido("Producto sin unidad")
  expect_true(is.na(r3$valor))

  r4 <- extraer_contenido("")
  expect_true(is.na(r4$valor))
})

# insercion
test_that("no duplica si inserta la misma ejecucion dos veces", {
  tmp_db <- tempfile(fileext = ".sqlite")
  con <- db_connect(tmp_db)
  db_setup(con)
  db_registrar_comercios(con, tiendas)

  df_test <- tibble(
    comercio_id = "exito",
    producto_id_comercio = "TEST001",
    sku_comercio = "SKU1",
    nombre_producto = "Producto Test",
    nombre_estandarizado = NA_character_,
    marca = "TestMarca",
    marca_estandarizada = "TestMarca",
    subcategoria = "/test/",
    presentacion = "100ml",
    contenido_valor = 100,
    contenido_unidad = "ml",
    precio = 15000,
    precio_regular = 15000,
    es_promocion = 0L,
    url = "http://test.com",
    texto_original = "Producto Test",
    fecha_hora_consulta = "2026-01-01T00:00:00-05:00",
    comparable = 0L,
    producto_comun_id = NA_character_,
    disponibilidad = "disponible"
  )

  db_insertar_ejecucion(con, "test_ejec_1", "exito", "2026-01-01")
  n1 <- db_insertar_observaciones(con, df_test, "test_ejec_1")
  expect_equal(n1, 1)

  n2 <- db_insertar_observaciones(con, df_test, "test_ejec_1")
  expect_equal(n2, 0)

  total <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM observaciones")$n
  expect_equal(total, 1)

  db_disconnect(con)
  unlink(tmp_db)
})

test_that("db_get_cambios_precio pilla diferencias entre ejecuciones", {
  tmp_db <- tempfile(fileext = ".sqlite")
  con <- db_connect(tmp_db)
  db_setup(con)
  db_registrar_comercios(con, tiendas)

  db_insertar_ejecucion(con, "ejec_a", "exito", "2026-01-01")
  db_insertar_ejecucion(con, "ejec_b", "exito", "2026-01-02")

  df_a <- tibble(
    comercio_id = "exito", producto_id_comercio = "P100",
    sku_comercio = "S1", nombre_producto = "Repelente X",
    nombre_estandarizado = NA_character_, marca = "MarcaX",
    marca_estandarizada = "MarcaX", subcategoria = "/cat/",
    presentacion = "100ml", contenido_valor = 100, contenido_unidad = "ml",
    precio = 10000, precio_regular = 10000, es_promocion = 0L,
    url = NA_character_, texto_original = "Repelente X",
    fecha_hora_consulta = "2026-01-01T10:00:00-05:00",
    comparable = 0L, producto_comun_id = NA_character_,
    disponibilidad = "disponible")

  df_b <- df_a
  df_b$precio <- 12000
  df_b$fecha_hora_consulta <- "2026-01-02T10:00:00-05:00"

  db_insertar_observaciones(con, df_a, "ejec_a")
  db_insertar_observaciones(con, df_b, "ejec_b")

  cambios <- db_get_cambios_precio(con, "ejec_a", "ejec_b", umbral_pct = 0)
  expect_equal(nrow(cambios), 1)
  expect_equal(cambios$diferencia_cop[1], 2000)
  expect_equal(cambios$diferencia_pct[1], 20)

  # con umbral alto no sale nada
  cambios2 <- db_get_cambios_precio(con, "ejec_a", "ejec_b", umbral_pct = 25)
  expect_equal(nrow(cambios2), 0)

  db_disconnect(con)
  unlink(tmp_db)
})

test_that("db_get_canasta retorna solo tiendas con todos los productos", {
  tmp_db <- tempfile(fileext = ".sqlite")
  con <- db_connect(tmp_db)
  db_setup(con)
  db_registrar_comercios(con, tiendas)

  db_insertar_ejecucion(con, "ejec_canasta", "exito", "2026-01-01")

  df1 <- tibble(
    comercio_id = "exito", producto_id_comercio = "CP01",
    sku_comercio = "S1", nombre_producto = "Stay Off Spray Adulto 120ml",
    nombre_estandarizado = "Stay Off Spray Adulto 120ml",
    marca = "Stay Off", marca_estandarizada = "Stay Off",
    subcategoria = "/rep/", presentacion = "120ml",
    contenido_valor = 120, contenido_unidad = "ml",
    precio = 16000, precio_regular = 16000, es_promocion = 0L,
    url = NA_character_, texto_original = "Stay Off",
    fecha_hora_consulta = "2026-01-01T10:00:00-05:00",
    comparable = 1L, producto_comun_id = "P01",
    disponibilidad = "disponible")

  db_insertar_observaciones(con, df1, "ejec_canasta")

  datos <- db_get_canasta(con, c("P01", "P02"))
  prods_encontrados <- unique(datos$producto_comun_id)
  expect_false("P02" %in% prods_encontrados)

  datos1 <- db_get_canasta(con, c("P01"))
  expect_true(nrow(datos1) >= 1)

  db_disconnect(con)
  unlink(tmp_db)
})

# limpieza input
test_that("validar_texto_input quita caracteres raros", {
  expect_equal(validar_texto_input("repelente raid"), "repelente raid")
  expect_equal(validar_texto_input("test'; DROP TABLE--"), "test DROP TABLE--")
  expect_equal(validar_texto_input(NULL), "")
  expect_equal(validar_texto_input(NA), "")
  expect_equal(nchar(validar_texto_input(paste(rep("a", 300), collapse = ""))), 200)
})

test_that("es_repelente filtra bien ambientadores vs repelentes", {
  expect_true(es_repelente("Repelente de insectos Stay Off", "Stay Off"))
  expect_true(es_repelente("Insecticida Raid", "Raid"))
  expect_false(es_repelente("Ambientador Glade", "Glade"))
  expect_false(es_repelente("Desodorante ambiental Fresh", "Fresh"))
  expect_true(es_repelente("Aerosol mata mosquitos", "Baygon"))
})
