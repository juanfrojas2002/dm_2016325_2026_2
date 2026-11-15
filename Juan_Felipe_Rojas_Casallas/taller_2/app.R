# app.R - Comparador de precios
# Juan Felipe Rojas - 2016325
# Mineria de Datos 2026-2

library(shiny)
library(bslib)
library(highcharter)
library(DT)
library(dplyr)
library(stringr)
library(promises)
library(future)

plan(multisession)

source("scraper.R", local = TRUE)
source("db.R", local = TRUE)
source("validacion.R", local = TRUE)

col_tiendas <- c(
  "Exito" = "#FDDA24",
  "Jumbo Colombia" = "#E31937",
  "La Rebaja Virtual" = "#00A651"
)

ui <- page_navbar(
  title = "Comparador de Precios",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2E86AB",
    "navbar-bg" = "#2E86AB"
  ),
  sidebar = sidebar(
    width = 280,
    title = "Filtros",
    selectizeInput("fil_comercio", "Comercio", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Todos")),
    selectizeInput("fil_marca", "Marca", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Todas")),
    selectizeInput("fil_subcategoria", "Subcategoria", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Todas")),
    uiOutput("ui_precio_range"),
    selectizeInput("fil_ejecucion", "Ejecucion", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Todas")),
    checkboxInput("fil_promo", "Solo promociones", FALSE),
    checkboxInput("fil_comparable", "Solo comparables", FALSE),
    textInput("fil_busqueda", "Buscar producto", placeholder = "Nombre o marca..."),
    hr(),
    h6("Actualizar datos"),
    checkboxInput("modo_offline", "Modo sin conexion (raw/)", FALSE),
    input_task_button("btn_actualizar", "Actualizar precios", type = "default"),
    uiOutput("ui_update_status")
  ),

  nav_panel(
    "Resumen",
    icon = icon("chart-bar"),
    layout_columns(
      col_widths = c(4, 4, 4, 4, 4, 4),
      value_box(title = "Productos", value = textOutput("ind_productos"),
                theme = "primary"),
      value_box(title = "Observaciones", value = textOutput("ind_observaciones"),
                theme = "info"),
      value_box(title = "Ultima ejecucion", value = textOutput("ind_ultima"),
                theme = "secondary"),
      value_box(title = "En promocion", value = textOutput("ind_promocion"),
                theme = "warning"),
      value_box(title = "Precio mediano", value = textOutput("ind_mediana"),
                theme = "success"),
      value_box(title = "Ahorro maximo", value = textOutput("ind_ahorro"),
                theme = "danger")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Precios por producto comparable"),
        highchartOutput("chart_precios", height = "400px")
      ),
      card(
        card_header("Ahorro al comprar en el mas barato"),
        highchartOutput("chart_ahorro", height = "400px")
      )
    ),
    card(
      card_header("Productos"),
      DTOutput("tabla_productos"),
      downloadButton("btn_csv", "Descargar CSV", class = "btn-sm btn-outline-primary mt-2")
    )
  ),

  nav_panel(
    "Comparacion",
    icon = icon("balance-scale"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Evolucion de precio por producto"),
        selectInput("sel_prod_evol", "Producto comparable",
                    choices = setNames(comparables_ref$id, comparables_ref$nombre)),
        highchartOutput("chart_evolucion", height = "350px")
      ),
      card(
        card_header("Precio por unidad de contenido"),
        highchartOutput("chart_precio_unitario", height = "400px")
      )
    ),
    card(
      card_header("Distribucion de precios por comercio"),
      highchartOutput("chart_distribucion", height = "350px")
    )
  ),

  nav_panel(
    "Cambios de precio",
    icon = icon("exchange-alt"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Configuracion"),
        selectInput("ejec_anterior", "Ejecucion anterior", choices = NULL),
        selectInput("ejec_nueva", "Ejecucion nueva", choices = NULL),
        sliderInput("umbral_cambio", "Umbral minimo (%)", 0, 50, 0, step = 1),
        actionButton("btn_comparar", "Comparar", class = "btn-primary w-100")
      ),
      card(
        card_header("Cambios de precio"),
        DTOutput("tabla_cambios"),
        hr(),
        h6("Productos nuevos / desaparecidos"),
        uiOutput("ui_nuevos_desap")
      )
    )
  ),

  nav_panel(
    "Canasta de compra",
    icon = icon("shopping-cart"),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Armar canasta"),
        checkboxGroupInput("canasta_prods", "Productos",
                           choiceNames = comparables_ref$nombre,
                           choiceValues = comparables_ref$id),
        uiOutput("ui_cantidades"),
        actionButton("btn_calcular_canasta", "Calcular costo", class = "btn-success w-100")
      ),
      card(
        card_header("Resultado"),
        uiOutput("ui_canasta_resultado"),
        DTOutput("tabla_canasta")
      )
    )
  )
)

## server

server <- function(input, output, session) {

  con <- db_connect()
  db_setup(con)
  db_registrar_comercios(con, tiendas)
  db_aplicar_correcciones(con)
  onStop(function() db_disconnect(con))

  data_version <- reactiveVal(0)

  # poblar filtros
  observe({
    comercios <- db_get_comercios(con)
    updateSelectizeInput(session, "fil_comercio",
                         choices = setNames(comercios$comercio_id, comercios$nombre))
    updateSelectizeInput(session, "fil_marca", choices = db_get_marcas(con))
    updateSelectizeInput(session, "fil_subcategoria", choices = db_get_subcategorias(con))
    ejecs <- db_get_ejecuciones(con)
    choices_ej <- setNames(ejecs$ejecucion_id,
                           paste(ejecs$comercio_nombre, "-", substr(ejecs$fecha_hora_inicio, 1, 10)))
    updateSelectizeInput(session, "fil_ejecucion", choices = choices_ej)
    updateSelectInput(session, "ejec_anterior", choices = choices_ej)
    updateSelectInput(session, "ejec_nueva", choices = choices_ej)
  }) |> bindEvent(data_version(), ignoreNULL = FALSE)

  output$ui_precio_range <- renderUI({
    rng <- db_get_rango_precios(con)
    sliderInput("fil_precio", "Rango de precio (COP)",
                min = 0, max = ceiling(rng$max_precio / 1000) * 1000,
                value = c(0, ceiling(rng$max_precio / 1000) * 1000),
                step = 500, pre = "$", sep = ",")
  })

  # datos filtrados con cache
  datos_filtrados <- reactive({
    data_version()
    com <- if (length(input$fil_comercio) > 0) input$fil_comercio else NULL
    mar <- if (length(input$fil_marca) > 0) input$fil_marca else NULL
    subcat <- if (length(input$fil_subcategoria) > 0) input$fil_subcategoria else NULL
    ej  <- if (length(input$fil_ejecucion) > 0) input$fil_ejecucion else NULL
    pmin <- if (!is.null(input$fil_precio)) input$fil_precio[1] else NULL
    pmax <- if (!is.null(input$fil_precio)) input$fil_precio[2] else NULL
    bus  <- validar_texto_input(input$fil_busqueda)
    #cat("debug filtros:", com, mar, subcat, bus, "\n")
    db_get_productos(con,
      comercio_ids = com, subcategorias = subcat, marcas = mar,
      precio_min = pmin, precio_max = pmax,
      ejecucion_ids = ej,
      solo_promocion = isTRUE(input$fil_promo),
      solo_comparable = isTRUE(input$fil_comparable),
      busqueda = if (nchar(bus) > 0) bus else NULL,
      solo_disponible = FALSE)
  }) |> bindCache(
    data_version(),
    input$fil_comercio, input$fil_marca, input$fil_subcategoria,
    input$fil_ejecucion, input$fil_precio,
    input$fil_promo, input$fil_comparable, input$fil_busqueda
  )

  indicadores <- reactive({
    data_version()
    com <- if (length(input$fil_comercio) > 0) input$fil_comercio else NULL
    db_get_indicadores(con, comercio_ids = com)
  })

  output$ind_productos <- renderText({
    format(indicadores()$total_productos, big.mark = ",")
  })
  output$ind_observaciones <- renderText({
    format(indicadores()$total_observaciones, big.mark = ",")
  })
  output$ind_ultima <- renderText({
    ult <- indicadores()$ultima_ejecucion
    if (is.na(ult)) "Sin datos" else substr(ult, 1, 10)
  })
  output$ind_promocion <- renderText({
    paste0(indicadores()$pct_promocion, "%")
  })
  output$ind_mediana <- renderText({
    med <- indicadores()$mediana_precio
    paste0("$", format(med, big.mark = ","))
  })
  output$ind_ahorro <- renderText({
    ah <- indicadores()$ahorro_maximo
    if (is.na(ah)) "N/A" else paste0("$", format(ah, big.mark = ","))
  })

  # grafico comparables
  output$chart_precios <- renderHighchart({
    comp <- db_get_comparables(con)
    if (nrow(comp) == 0) return(highchart() |> hc_title(text = "Sin datos comparables"))

    comp_last <- comp |>
      group_by(producto_comun_id, nombre_estandarizado, comercio_nombre) |>
      filter(fecha_hora_consulta == max(fecha_hora_consulta)) |>
      ungroup()

    prods <- unique(comp_last$nombre_estandarizado)

    hc <- highchart() |>
      hc_chart(type = "bar") |>
      hc_title(text = NULL) |>
      hc_xAxis(categories = prods) |>
      hc_yAxis(title = list(text = "Precio (COP)"),
               labels = list(format = "${value:,.0f}")) |>
      hc_tooltip(pointFormat = "{series.name}: <b>${point.y:,.0f}</b>")

    for (tienda in unique(comp_last$comercio_nombre)) {
      datos_t <- comp_last |> filter(comercio_nombre == tienda)
      vals <- sapply(prods, function(p) {
        v <- datos_t$precio[datos_t$nombre_estandarizado == p]
        if (length(v) > 0) v[1] else NA
      })
      color <- unname(col_tiendas[tienda])
      hc <- hc |> hc_add_series(name = tienda, data = as.numeric(vals), color = color)
    }
    hc
  })

  output$chart_ahorro <- renderHighchart({
    comp <- db_get_comparables(con)
    if (nrow(comp) == 0) return(highchart() |> hc_title(text = "Sin datos"))

    ahorro <- comp |>
      group_by(nombre_estandarizado) |>
      summarise(
        ahorro_cop = max(precio) - min(precio),
        ahorro_pct = round(100 * (max(precio) - min(precio)) / max(precio), 1),
        .groups = "drop") |>
      arrange(desc(ahorro_pct))

    highchart() |>
      hc_chart(type = "bar") |>
      hc_title(text = NULL) |>
      hc_xAxis(categories = ahorro$nombre_estandarizado) |>
      hc_yAxis(title = list(text = "Ahorro (%)"), max = 100) |>
      hc_add_series(name = "Ahorro", data = ahorro$ahorro_pct, color = "#2E86AB",
                    dataLabels = list(enabled = TRUE, format = "{y}%")) |>
      hc_tooltip(pointFormat = "Ahorro: <b>{point.y}%</b>") |>
      hc_legend(enabled = FALSE)
  })

  output$chart_evolucion <- renderHighchart({
    req(input$sel_prod_evol)
    data_version()
    evol <- db_get_evolucion_precio(con, input$sel_prod_evol)
    if (nrow(evol) == 0) return(highchart() |> hc_title(text = "Sin datos de evolucion"))

    hc <- highchart() |>
      hc_chart(type = "line") |>
      hc_title(text = evol$nombre_estandarizado[1]) |>
      hc_xAxis(type = "category") |>
      hc_yAxis(title = list(text = "Precio (COP)"),
               labels = list(format = "${value:,.0f}")) |>
      hc_tooltip(pointFormat = "{series.name}: <b>${point.y:,.0f}</b>")

    for (tienda in unique(evol$comercio_nombre)) {
      datos_t <- evol |> filter(comercio_nombre == tienda) |> arrange(fecha)
      pts <- lapply(seq_len(nrow(datos_t)), function(i) {
        list(name = substr(datos_t$fecha[i], 1, 10), y = datos_t$precio[i])
      })
      color <- unname(col_tiendas[tienda])
      hc <- hc |> hc_add_series(name = tienda, data = pts, color = color)
    }
    hc
  })

  output$chart_precio_unitario <- renderHighchart({
    data_version()
    pu <- db_get_precio_unitario_comparables(con)
    if (nrow(pu) == 0) return(highchart() |> hc_title(text = "Sin datos"))

    pu_last <- pu |>
      group_by(producto_comun_id, nombre_estandarizado, comercio_nombre) |>
      filter(row_number() == 1) |>
      ungroup()

    hc <- highchart() |>
      hc_chart(type = "bar") |>
      hc_title(text = NULL) |>
      hc_xAxis(categories = unique(pu_last$nombre_estandarizado)) |>
      hc_yAxis(title = list(text = "Precio por unidad")) |>
      hc_tooltip(pointFormat = "{series.name}: <b>${point.y:,.1f}</b>")

    for (tienda in unique(pu_last$comercio_nombre)) {
      datos_t <- pu_last |> filter(comercio_nombre == tienda)
      all_prods <- unique(pu_last$nombre_estandarizado)
      vals <- sapply(all_prods, function(p) {
        v <- datos_t$precio_unitario[datos_t$nombre_estandarizado == p]
        if (length(v) > 0) v[1] else NA
      })
      color <- unname(col_tiendas[tienda])
      hc <- hc |> hc_add_series(name = tienda, data = as.numeric(vals), color = color)
    }
    hc
  })

  output$chart_distribucion <- renderHighchart({
    data_version()
    dist <- db_get_distribucion_precios(con)
    if (nrow(dist) == 0) return(highchart() |> hc_title(text = "Sin datos"))

    # armar boxplot por comercio
    highchart() |>
      hc_add_series_list(data_to_boxplot(dist$precio, dist$comercio_nombre,
                                         add_outliers = TRUE, name = "Precios")) |>
      hc_title(text = NULL) |>
      hc_yAxis(title = list(text = "Precio (COP)"))
  })

  # tabla principal
  output$tabla_productos <- renderDT({
    df <- datos_filtrados()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "Sin resultados")))

    show <- df |> select(
      Comercio = comercio_nombre,
      Producto = nombre_producto,
      Marca = marca_estandarizada,
      Presentacion = presentacion,
      Precio = precio,
      `Precio unitario` = precio_unitario,
      Promocion = es_promocion,
      Disponibilidad = disponibilidad,
      Fecha = fecha_hora_consulta,
      URL = url
    )
    show$Precio <- ifelse(is.na(show$Precio), "Sin precio",
                          paste0("$", format(show$Precio, big.mark = ",")))
    show$`Precio unitario` <- ifelse(is.na(show$`Precio unitario`), "-",
                                     paste0("$", format(show$`Precio unitario`, big.mark = ",")))
    show$Promocion <- ifelse(show$Promocion == 1, "Si", "No")
    show$Fecha <- substr(show$Fecha, 1, 10)
    show$URL <- ifelse(is.na(show$URL), "-",
                       paste0('<a href="', show$URL, '" target="_blank">Ver</a>'))

    datatable(show, escape = FALSE, rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE,
                             language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/es-ES.json")))
  })

  output$btn_csv <- downloadHandler(
    filename = function() paste0("productos_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- datos_filtrados()
      readr::write_csv(df, file)
    }
  )

  # cambios de precio
  cambios_data <- reactiveVal(NULL)
  nuevos_desap <- reactiveVal(NULL)

  observeEvent(input$btn_comparar, {
    req(input$ejec_anterior, input$ejec_nueva)
    if (input$ejec_anterior == input$ejec_nueva) {
      showNotification("Seleccione dos ejecuciones diferentes", type = "warning")
      return()
    }
    cambios <- db_get_cambios_precio(con, input$ejec_anterior, input$ejec_nueva,
                                     umbral_pct = input$umbral_cambio)
    cambios_data(cambios)
    nd <- db_productos_nuevos_desaparecidos(con, input$ejec_anterior, input$ejec_nueva)
    nuevos_desap(nd)
  })

  output$tabla_cambios <- renderDT({
    df <- cambios_data()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "Sin cambios o seleccione ejecuciones")))
    }
    show <- df |> select(
      Comercio = comercio_nombre,
      Producto = nombre_producto,
      `Precio anterior` = precio_anterior,
      `Precio nuevo` = precio_nuevo,
      `Diferencia COP` = diferencia_cop,
      `Diferencia %` = diferencia_pct
    )
    show$`Precio anterior` <- paste0("$", format(show$`Precio anterior`, big.mark = ","))
    show$`Precio nuevo` <- paste0("$", format(show$`Precio nuevo`, big.mark = ","))
    show$`Diferencia COP` <- paste0(ifelse(show$`Diferencia COP` >= 0, "+$", "-$"),
                                    format(abs(df$diferencia_cop), big.mark = ","))
    show$`Diferencia %` <- paste0(ifelse(df$diferencia_pct >= 0, "+", ""),
                                  round(df$diferencia_pct, 1), "%")
    datatable(show, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$ui_nuevos_desap <- renderUI({
    nd <- nuevos_desap()
    if (is.null(nd)) return(p("Seleccione ejecuciones y presione Comparar"))
    tags$div(
      if (nrow(nd$nuevos) > 0) {
        tags$div(
          tags$strong(paste0("Productos nuevos (", nrow(nd$nuevos), "):")),
          tags$ul(lapply(seq_len(min(nrow(nd$nuevos), 10)), function(i) {
            tags$li(paste(nd$nuevos$comercio_nombre[i], "-", nd$nuevos$nombre_producto[i]))
          }))
        )
      } else tags$p("No hay productos nuevos"),
      if (nrow(nd$desaparecidos) > 0) {
        tags$div(
          tags$strong(paste0("Productos desaparecidos (", nrow(nd$desaparecidos), "):")),
          tags$ul(lapply(seq_len(min(nrow(nd$desaparecidos), 10)), function(i) {
            tags$li(paste(nd$desaparecidos$comercio_nombre[i], "-",
                          nd$desaparecidos$nombre_producto[i]))
          }))
        )
      } else tags$p("No hay productos desaparecidos")
    )
  })

  # canasta
  output$ui_cantidades <- renderUI({
    req(input$canasta_prods)
    prods_sel <- comparables_ref[comparables_ref$id %in% input$canasta_prods, ]
    lapply(seq_len(nrow(prods_sel)), function(i) {
      numericInput(paste0("cant_", prods_sel$id[i]),
                   paste0(prods_sel$nombre[i], " (unidades)"),
                   value = 1, min = 1, max = 100, step = 1)
    })
  })

  canasta_resultado <- reactiveVal(NULL)

  observeEvent(input$btn_calcular_canasta, {
    req(input$canasta_prods)
    prods_sel <- input$canasta_prods
    cantidades <- sapply(prods_sel, function(pid) {
      val <- input[[paste0("cant_", pid)]]
      if (is.null(val) || is.na(val) || val < 1) 1 else val
    })
    names(cantidades) <- prods_sel

    datos <- db_get_canasta(con, prods_sel)
    if (nrow(datos) == 0) { canasta_resultado(NULL); return() }

    comercios_disponibles <- unique(datos$comercio_nombre)

    costo_tienda <- lapply(comercios_disponibles, function(t) {
      datos_t <- datos |> filter(comercio_nombre == t)
      prods_en_tienda <- unique(datos_t$producto_comun_id)
      tiene_todos <- all(prods_sel %in% prods_en_tienda)
      if (!tiene_todos) return(NULL)
      total <- sum(sapply(prods_sel, function(pid) {
        p <- datos_t$precio[datos_t$producto_comun_id == pid]
        if (length(p) > 0) p[1] * cantidades[pid] else 0
      }))
      data.frame(comercio = t, total = total, completa = TRUE)
    })
    costo_tienda <- dplyr::bind_rows(Filter(Negate(is.null), costo_tienda))

    mejor_mix <- datos |>
      group_by(producto_comun_id) |>
      filter(precio == min(precio)) |>
      slice(1) |>
      ungroup()
    costo_mix <- sum(sapply(prods_sel, function(pid) {
      p <- mejor_mix$precio[mejor_mix$producto_comun_id == pid]
      if (length(p) > 0) p[1] * cantidades[pid] else 0
    }))

    canasta_resultado(list(
      por_tienda = costo_tienda,
      mix = costo_mix,
      mejor_mix = mejor_mix,
      cantidades = cantidades
    ))
  })

  output$ui_canasta_resultado <- renderUI({
    res <- canasta_resultado()
    if (is.null(res)) return(p("Seleccione productos y calcule la canasta"))

    tags$div(
      if (nrow(res$por_tienda) > 0) {
        tags$div(
          h5("Costo por tienda (compra completa)"),
          tags$ul(lapply(seq_len(nrow(res$por_tienda)), function(i) {
            tags$li(tags$strong(res$por_tienda$comercio[i]), ": ",
                    paste0("$", format(res$por_tienda$total[i], big.mark = ",")))
          }))
        )
      } else tags$p(class = "text-warning",
                     "Ninguna tienda tiene todos los productos seleccionados"),
      tags$div(
        h5("Costo minimo (mejor precio por producto)"),
        tags$p(tags$strong(paste0("$", format(res$mix, big.mark = ","))),
               " comprando cada producto donde es mas barato")
      ),
      tags$p(class = "text-muted small",
             "Nota: no incluye costos de envio. ",
             "Comprar en varias tiendas puede salir mas caro por envios.")
    )
  })

  output$tabla_canasta <- renderDT({
    res <- canasta_resultado()
    if (is.null(res)) return(NULL)
    mm <- res$mejor_mix |>
      select(Producto = nombre_estandarizado,
             `Mejor tienda` = comercio_nombre,
             Precio = precio)
    mm$Cantidad <- res$cantidades[mm$Producto |> (\(x) res$mejor_mix$producto_comun_id)()]
    mm$Subtotal <- mm$Precio * mm$Cantidad
    mm$Precio <- paste0("$", format(mm$Precio, big.mark = ","))
    mm$Subtotal <- paste0("$", format(mm$Subtotal, big.mark = ","))
    datatable(mm, rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })

  # actualizacion en segundo plano
  update_task <- ExtendedTask$new(function(offline) {
    future_promise({
      source("scraper.R", local = TRUE)
      source("db.R", local = TRUE)
      source("validacion.R", local = TRUE)

      con_bg <- db_connect()
      db_setup(con_bg)
      on.exit(db_disconnect(con_bg))

      resumen <- list()
      for (key in names(tiendas)) {
        cfg <- tiendas[[key]]
        rid <- gen_run_id()
        eid <- paste0(key, "_", rid)
        fecha <- colombia_now()
        db_insertar_ejecucion(con_bg, eid, cfg$id, fecha)

        res <- tryCatch(
          scrapear_tienda(cfg, offline = offline, run_id = if (offline) NULL else rid),
          error = function(e) {
            db_finalizar_ejecucion(con_bg, eid, 0, "fallida")
            NULL
          }
        )

        if (!is.null(res) && !is.null(res$data) && nrow(res$data) > 0) {
          val <- validar_productos(res$data)
          dbBegin(con_bg)
          tryCatch({
            n <- db_insertar_observaciones(con_bg, val$datos, eid)
            db_finalizar_ejecucion(con_bg, eid, n, "completa")
            dbCommit(con_bg)

            resumen[[key]] <- list(
              ok = TRUE, comercio = cfg$nombre,
              recolectados = val$n_original, nuevos = n
            )
          }, error = function(e) {
            tryCatch(dbRollback(con_bg), error = function(e2) NULL)
            db_finalizar_ejecucion(con_bg, eid, 0, "fallida")
            resumen[[key]] <- list(ok = FALSE, comercio = cfg$nombre, error = e$message)
          })
        } else {
          db_finalizar_ejecucion(con_bg, eid, 0, "fallida")
          resumen[[key]] <- list(ok = FALSE, comercio = cfg$nombre,
                                 error = "Sin datos obtenidos")
        }
      }
      resumen
    })
  }) |> bind_task_button("btn_actualizar")

  observeEvent(input$btn_actualizar, {
    update_task$invoke(isTRUE(input$modo_offline))
  })

  observe({
    resumen <- update_task$result()
    data_version(data_version() + 1)

    lines <- vapply(resumen, function(r) {
      if (isTRUE(r$ok)) {
        sprintf("%s: %d recolectados, %d nuevos", r$comercio, r$recolectados, r$nuevos)
      } else {
        sprintf("%s: FALLO - %s", r$comercio, r$error %||% "desconocido")
      }
    }, character(1))

    showNotification(
      tags$div(h6("Actualizacion completa"), tags$pre(paste(lines, collapse = "\n"))),
      type = "message", duration = 15
    )
  })

  output$ui_update_status <- renderUI({
    status <- update_task$status()
    if (status == "running") {
      tags$div(class = "text-info small mt-2",
               tags$span(class = "spinner-border spinner-border-sm"),
               " Actualizando precios...")
    } else NULL
  })
}

shinyApp(ui, server)
