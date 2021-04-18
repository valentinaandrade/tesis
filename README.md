# Feminización del conflicto laboral

Repositorio que contiene tesis para optar al grado de Socióloga de la Universidad de Chile.

- Autora: Valentina Andrade de la Horra
- Profesor Guía: Dr. Omar Aguilar Novoa
- Profesor Co-tutor: Dr. Pablo Pérez Ahumada
- Comisión evaluadora:  Por definir

FONDECYT N°11190229 *"Determinantes institucionales y políticos del conflicto entre empresarios y trabajadores: los casos de Argentina y Chile en perspectiva comparada"*

## Abstract

La feminización de la sindicalización hoy es un fenómeno mundial. Distintas investigaciones han abordado el problema de la sindicalización femenina desde factores culturales que redundan en la pasividad de las mujeres y exclusión de estas en la acción colectiva. Ante la insuficiencia de un corpus que logre explicar por qué, en la actualidad,  hoy hay más mujeres en los sindicatos, la siguiente investigación pone énfasis en la importancia de las determinantes cíclicas, estructurales e institucionales de los cambios de la sindicalización. El aumento en la participación laboral femenina, la feminización de ciertas industrias, los cambios en las brechas salariales, aumento del empleo parcial y los distintos arreglos institucionales son algunos de las determinantes a considerar en un análisis de series temporales comparativo entre 45 países. Con la construcción de una [base de datos](../output/data) propia con cobertura de 1980-2018 cuyas principales fuentes de información son encuestas de hogares, datos administrativos, ICTWSS, ILO, Banco Mundial y OCDE, se estimaron modelos de corrección de error y usando efectos fijos se distinguió entre efectos a largo plazo y a corto plazo de las determinantes.

**Palabras claves**: sindicalización, feminización, mercado laboral, salarios, tiempos de trabajo

## Base de datos

Los datos usados en esta investigación provienen de la construcción de una base de datos longitudinal (**FDL**) sobre relaciones laborales a partir de la unión de fuentes como ICTWSS, OCDE, ILO y Banco Mundial, que proveen información temporal a nivel de los países. A esto se agrega la estimación y validación de los dos indicadores principales para este estudio, que son la densidad sindical femenina y masculina, las cuales  fueron construidas a partir de encuestas de hogares, encuestas a fuerza de trabajo y/o datos administrativos para los países en estudio (ver [Apéndice A](#apendicea)). FDL es una base de datos longitudinal que contiene 3360 observaciones y 340 variables laborales, económicas e instituciones a partir de información desde 1960 a 2018 de 45 países del mundo. Como se puede observar en la Tabla [3.1](#tab3-1), gran parte de estos países corresponden a países OCDE, y aquellos que no son países OCDE pertenecen al G20.

FDL (que proviene de las siglas Fábrica de Datos Laborales) es una base de datos de construcción reproducible. Por ello la construcción, validación y procesamiento de FDL responde a los criterios TIER (2020) y está libre en el repositorio de esta tesis. A su vez, la base de datos se actualiza cada vez que las fuentes principales de información actualizan la información disponible en sus sitios web. A su vez se puede encontrar en el siguiente link el libro de códigos donde se especifica la medición de cada una de las variables.

## Cómo reproducir la tesis

### Construcción de la base de Datos

- Para construir localmente la base de datos los pasos son los siguientes:
  1.  Fork o clone a este repositorio de GitHub en tu computadora local

```{r}
$ git clone https://github.com/valentinaandrade/tesis/input
```

  2. Ir a la carpeta de ruta `input/data`, abrir el archivo code_book.Rmd y compilar (`knit`)

  3. Con esto la base de datos se abrá creado localmente y quedará guardada en la ruta `output/data`. A su vez, en la misma carpeta del .Rmd podrás ver que se ha creado un libro de códigos automático con el nombre de las variables, fuente, cobertura y unidades de medida.

### Reproducción de análisis

- Para reproducir los modelos, análisis descriptivos, geoespaciales y la construcción de gráficas estáticas e interactivas se debe seguir las siguientes resutas

  1. Ir a la ruta `processing`. En esta ruta podrán encontrar diferentes sintaxis para cada capítulo.

    a. Sintaxis de procesamiento (sin capítulo)
    b. Sintaxis de validación (sin capítulo)
    c. Sintaxis de análisis descriptivo (capítulo 1 y 2)
    d. Sintaxis de test de supuestos (capítulo 3 y 4)
    c. Sintaxis de modelamiento (capítulo 4)
    d. Sintaxis de ajuste y test posthoc (capítulo 4 y 5)

  2. Estas sintaxis permiten simplificar los análisis y presentación en los RMarkdown que dividen a cada uno de los capítulos. Al igual que en las sintaxis, para hacer referencia al número del capítulo se parte con el número y luego nombre del capítulo (eg. `01-intro.Rmd`).

### Reproducción de la escritura y análisis

Para ello solo se debe ingresar al .Rproj thesis.Rproj en la ruta `/thesis`. Luego, en la consola indicar


```{r}
bookdown::serve_book
```

Con esto se compilará análisis y escritura y aparecerá en el visor de RStudio. De hecho, ese es el "único" paso necesario para reproducir la tesis, pero se han indicado los anteriores de modo de si alguien quiere reproducir la manipulación de datos de modo de tender hacia análisis y datos replicables.

## Nota

- Cualquier comentario de la base puede ser indicado en Issues

- Comentarios de contenidos, quejas o agradecimiento también son muy bien recibidos a valentinaandrade@uchile.cl

- Evite plagiar los análisis y escritura. La tesis está siendo evaluada para su publicación, y en consecuencia, cualquier uso indebido será penalizado para quién haga uso indebido de esta.
