# Diccionario de datos

Base: `comparador_precios.sqlite`

## comercios

| Campo | Tipo | Que es |
|-------|------|--------|
| comercio_id | TEXT PK | exito, jumbo o la_rebaja |
| nombre | TEXT | Nombre bonito para la UI |
| url_base | TEXT | URL del sitio web |

## ejecuciones

| Campo | Tipo | Que es |
|-------|------|--------|
| ejecucion_id | TEXT PK | Se arma como {comercio}_{fecha_hora} |
| fecha_hora_inicio | TEXT | Timestamp, zona Colombia |
| comercio_id | TEXT FK | Tienda |
| n_registros | INTEGER | Cuantos productos quedaron |
| estado | TEXT | completa / fallida / parcial / en_curso |

## observaciones

| Campo | Tipo | Que es |
|-------|------|--------|
| observacion_id | INTEGER PK | Autoincremental |
| ejecucion_id | TEXT FK | A que ejecucion pertenece |
| comercio_id | TEXT FK | De cual tienda |
| producto_id_comercio | TEXT | ID del producto en la tienda |
| sku_comercio | TEXT | SKU del item |
| nombre_producto | TEXT | Nombre como lo trae la tienda |
| nombre_estandarizado | TEXT | Nombre limpio, solo los comparables lo tienen |
| marca | TEXT | Marca original |
| marca_estandarizada | TEXT | Marca limpia (Stay Off, Raid, etc) |
| subcategoria | TEXT | Ruta de categoria en la tienda |
| presentacion | TEXT | Texto de presentacion |
| contenido_valor | REAL | Cantidad numerica |
| contenido_unidad | TEXT | Unidad: ml, g, kg, unidad |
| precio | REAL | En pesos. NULL si esta agotado o no trae precio |
| precio_regular | REAL | Precio sin descuento, NULL si no tiene |
| es_promocion | INTEGER | 1 = en promo, 0 = no |
| url | TEXT | Link al producto |
| texto_original | TEXT | Nombre + descripcion |
| fecha_hora_consulta | TEXT | Cuando se consulto |
| comparable | INTEGER | 1 si esta en las 3 tiendas, 0 si no |
| producto_comun_id | TEXT | P01 a P09, los que se pueden comparar |
| disponibilidad | TEXT | disponible / agotado / no_observado |

**UNIQUE**: (comercio_id, producto_id_comercio, ejecucion_id) - no se duplican productos en la misma ejecucion

## correcciones

| Campo | Tipo | Que es |
|-------|------|--------|
| correccion_id | INTEGER PK | Autoincremental |
| fecha | TEXT | Cuando se hizo |
| tabla | TEXT | Tabla que se corrigio |
| registro_id | TEXT | ID del registro |
| campo | TEXT | Que campo cambio |
| valor_anterior | TEXT | Valor viejo |
| valor_nuevo | TEXT | Valor nuevo |
| motivo | TEXT | Por que se corrigio |

## Vistas

**v_ultimo_precio** - ultimo precio de cada producto por tienda, incluye precio unitario

**v_cambios_precio** - diferencias de precio entre ejecuciones del mismo producto (COP y %)

**v_comparables** - los 9 productos que estan en las 3 tiendas, con precio valido (no NULL)

## Productos comparables

| ID | Producto | Contenido |
|----|----------|-----------|
| P01 | Stay Off Spray Adulto | 120 ml |
| P02 | Stay Off Amazonic Spray | 120 ml |
| P03 | Autan Crema Family | 200 g |
| P04 | Autan Aerosol | 177 ml |
| P05 | Nopikex Liquido | 120 ml |
| P06 | Nopikex Barra | 50 g |
| P07 | Bacterion Family Spray | 150 ml |
| P08 | Stay Off Ninos Spray | 120 ml |
| P09 | Bacterion Kids Crema | 120 g |
