# Comparador de Precios - Repelentes e Insecticidas

App en Shiny para comparar precios de repelentes e insecticidas en 3 tiendas colombianas (Exito, Jumbo, La Rebaja Virtual).

**Link de la app**: https://juanfeliperojas.shinyapps.io/comparador-precios/

## Para correr la app

Primero instalar los paquetes:

```r
install.packages(c("shiny", "bslib", "highcharter", "DT", "dplyr",
                    "httr2", "jsonlite", "stringr", "purrr", "readr",
                    "DBI", "RSQLite", "pointblank",
                    "promises", "future", "testthat"))
```

Despues abrir R desde la carpeta `taller_2/` y ejecutar:

```r
shiny::runApp()
```

## Tests

Desde la carpeta `taller_2/`:

```r
testthat::test_dir("tests")
```

## Archivos

- `app.R` - la app Shiny
- `db.R` - consultas SQL (la app no tiene SQL directo)
- `scraper.R` - scraping de las tiendas (API VTEX)
- `validacion.R` - validacion con pointblank
- `comparador_precios.sqlite` - base de datos
- `diccionario_datos.md` - diccionario de datos
- `renv.lock` - versiones de paquetes
- `raw/` - jsons crudos de cada scraping
- `tests/test_funciones.R` - 8 pruebas

## Vistas SQL

- `v_ultimo_precio` - ultimo precio de cada producto por tienda, con precio unitario
- `v_cambios_precio` - diferencias de precio entre ejecuciones
- `v_comparables` - los 9 productos que estan en las 3 tiendas, con precio valido

## Que encontre

1. Exito casi siempre gana en precio. De los 9 productos comparables, en 5 o 6 tiene el menor precio. En el Stay Off Spray Adulto la diferencia llega al 37%.

2. La Rebaja Virtual es la mas cara, pero tiene sentido porque es drogueria, no supermercado. Estan entre 15-20% por encima. Aunque en Nopikex Liquido si es mas barata que las otras dos.

3. Una cosa interesante: si uno mira el precio por ml o por gramo en vez del precio a secas, la Autan Crema Family 200g sale como la mejor opcion por gramo. No es la mas barata de precio pero rinde mas.

## Uso de IA

Use Claude para ayudarme con la estructura general del proyecto y algunas consultas SQL que me costaban. Despues revise los JSON crudos a mano para que los datos cuadraran y probe la app completa.
