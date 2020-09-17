## Results

### Descriptivos

Antes de exponer el modelo multivariado, se exponen las tendencias de $FUDi$ de los X países analizados. Los patrones muestran  tres tipos de tendencias: 
1. Crecimiento 
2. Estancamiento
3. Declive

**Figuras**

( --> Decir los n por cada uno, qué países destacan y cuándo <--)
( Esto asociarlo a algunas cosas de las indicadas en el capítulo 2 )

Aproximadamente 2 párrafos


En la **Tabla** se pueden ver os estadísticos descriptivos para todas las variables utilizadas para el analisis. 

Feminización de la densidad sindical --> media, minimo y máximo y para qué país. Luego cuándo fue el mayor lag de crecimiento y de declive.
Variación feminización densdad sindical 

Después cadauna de las variables independientes. 


## Modelo

Modelo Full

Chequeo de robustez

- Se ocupa Jackknife para ver casos influyentes*: esto son países que pueden estar influenciando mucho los resultados y a estos se les exluye del modelo

- Modelo con países LA y el resto. 

- Ir viendo hipótesis


### Interacciones

Modelo adicional donde se examina interacción entre modelo de relaciones laborales con variables economicas sobre la feminización de la densidad sindica. Se estomaron modelos separados paraconvsernar la estimación de cada interacción. El objetivo es ver como las estimaciones del modelo general son diferentes para países coordinados y liberlizados. 

Ver el error correction rate y R2 para cada modelo

Comparar como cambian con el resultado principal y luego la diferencia entre países

Ir incorporando datos y experiencia de organizaciones según variable 

# Paises feminizados y no segun coordinacion y segun part. empleo.

## Robustez

Especificación: T=2, N = 40-50 (los serios) --> Panel Data Econometrics by Badi Baltagi

**Robust Standard Error Estimators for PanelModels**

- Robust covariance matrix estimators a la Beck and Katz for panel models (a.k.a. Panel Corrected Standard Errors (PCSE))

Para el diagnóstico que relaja la hipótesis de homocedastecidad de los errores y la independencia de los errores, algo *escencial* para los modelamientos estadísticos donde se consideran unidades comp países en el tiempo (errores heterocedásticos). 

Esto es esperable en muestras panel o "cross sectional time series" donde las unidades (j) comparten las mismas caracteristicas, violando el principio de i.i.d. También las unidades podrían compartir caracteristicas similares basadas en la unidad de tiempo. 

Por eso se debe considerar la correlacion de los errores y la variación de los SE. Un método "robusto" descrito para esta literatura es Beck and Katz (1995) como el ECM.
(Beck and Katz, one way cluster BK)

- First-differencing, like fixed effects estimation, removes time-invariant effects


## Otros
verdistribucion de Coord para considerar como continuo

