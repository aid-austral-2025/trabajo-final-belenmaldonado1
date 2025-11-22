# Trabajo Final

Repo con el enunciado y los materiales para la entrega final en **Análisis Inteligente de Datos**.

## Información sobre el trabajo final

Este trabajo final tiene como objetivo realizar un análisis y seguimiento de las operaciones
diarias en el mercado local de los principales cultivos agrícolas de Argentina (soja, maíz, trigo, 
girasol, cebada y sorgo). Los datos se obtienen a través de la plataforma **SIO Granos**, un 
*sistema unificado de información obligatoria* de las operaciones de compraventa de granos que 
conforman el mercado físico.

También busca poder monitorear de dónde proviene la producción de cada cultivo según la campaña. Los datos 
se descargan de la **Secretaría de Agricultura, Ganadería y Pesca**.


### Fuente de datos

* **Origen:** los datos mensuales de pricing de granos se obtienen de la página web 
de [SIO Granos](https://www.siogranos.com.ar/Consulta_publica/operaciones_informadas_exportar.aspx),
seleccionando en "Fecha Declaración en SioGranos" el primer y último día del mes a exportar. Por su parte,
los datos del origen de la producción, se obtienen de la página de
[SAGyP](https://datosestimaciones.magyp.gob.ar/reportes.php?reporte=Estimaciones).
* **Frecuencia de actualización:** los datos de pricing se actualizarán semanalmente, incorporando información
del mes en curso y actualizando los dos meses previos, ya que la información reciente puede sufrir ligeras 
modificaciones. Los datos de estimaciones se actualizarán cada un mes, para incorporar las nuevas estimaciones 
disponibles.

## Proceso de actualización

- **Paso 1**: cargar los datos provenientes de SIO Granos en la carpeta denominada "Datos crudos", en el mismo formato
de los demás meses.
- **Paso 2**: cargar los datos provenientes de la página de SAGyP en la carpeta "Mapa" con el mismo nombre.
- **Paso 3**: correr los scripts **en el orden que están enumerados**.


## Salidas

* Archivos generados: se generan archivos sobre el **pricing de granos diario y semanal**, los cuales
se exportan en formato **xlsx** en la carpeta "Datos limpios". Los mismos se separan en carpetas por
año, incluyendo archivos históricos para cada grano. Para calcular el pricing, se tienen en cuenta 
únicamente las operaciones finales.
* Tablero: se genera un tablero shiny para monitorear los datos de pricing y origen de la producción, donde
se pueden observar tanto datos númericos como gráficos de seguimiento. Se puede acceder al tablero a través
del siguiente [LINK](https://belenmaldonado.shinyapps.io/AID2025/).

***ADVERTENCIA***: la carga del mapa en shiny funciona lento por la gran cantidad de departamentos.