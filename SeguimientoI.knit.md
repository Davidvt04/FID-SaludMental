---
title: "Seguimiento I: inspección de datos y planteamiento del análisis supervisado"
subtitle: "Global Mental Health & Lifestyle Survey"
author: "Grupo 1: Elena de los Santos Barrera, David Valencia Toscano y Beatriz Vela Moscoso"
date: "2026-11-19"
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    number_sections: true
---

# Información del grupo

- **Grupo de trabajo:** 1
- **Miembros:**
  1.  Elena de los Santos Barrera
  2.  David Valencia Toscano
  3.  Beatriz Vela Moscoso

# Conjunto de datos

- **Nombre:** Global Mental Health & Lifestyle Survey
- **Enlace:** <https://www.kaggle.com/code/lalit7881/global-mental-health-lifestyle-survey-92-15/input?scriptVersionId=294778087>
- **Fichero:** `data/mental_health.csv`

**Encuestados y variables.** El fichero contiene **10.000 encuestados**, uno por fila, y **51 variables**: 50 predictores y la variable objetivo. No hay valores ausentes ni registros duplicados. De las 51 columnas, 13 son categóricas y 38 numéricas, aunque buena parte de las numéricas son escalas discretas (de 1 a 10 o de 0 a 10) o indicadores binarios (0/1). Los predictores se organizan en siete bloques:

- **Sociodemográficos:** edad, género, país, nivel educativo, estado civil y nivel de ingresos.
- **Laborales:** situación laboral, horas de trabajo semanales, trabajo en remoto, satisfacción laboral, estrés laboral, conciliación, acoso en el trabajo y apoyo de la empresa a la salud mental.
- **Estilo de vida:** ejercicio, horas de sueño, consumo de cafeína, alcohol y tabaco, tiempo de pantalla y de redes sociales, tiempo de ocio, calidad de la dieta y estrés económico.
- **Síntomas:** doce síntomas emocionales puntuados de 0 a 10 (tristeza, pérdida de interés, problemas de sueño, fatiga, alteración del apetito, sentimiento de inutilidad, dificultad de concentración, ansiedad, cambios de humor, irritabilidad, pensamientos obsesivos y conductas compulsivas) y la frecuencia de ataques de pánico (de 0 a 4).
- **Riesgo clínico:** ideas de autolesión, ideas suicidas, antecedentes familiares de enfermedad mental y trauma previo.
- **Tratamiento:** diagnóstico previo, búsqueda de tratamiento, terapia actual y medicación actual.
- **Entorno social:** apoyo social, número de amigos íntimos, sentirse comprendido, soledad y facilidad para hablar de salud mental.

**Variable objetivo.** `Has_Mental_Health_Issue` es una variable binaria: vale 1 si la persona declara tener un problema de salud mental y 0 si no. Es una autodeclaración del encuestado, no un diagnóstico clínico. Su distribución e la siguiente: **9.216 personas (92,2 %)** declaran un problema y solo **784 (7,8 %)** no lo hacen. Este desbalanceo condiciona el análisis. Un modelo que respondiera siempre «sí» acertaría el 92,2 %

# Resumen de las tareas realizadas

> **Pendiente:** resumen de las tareas de este seguimiento (carga, inspección, visualización y planteamiento del análisis supervisado). Se redacta al final, cuando el resto del documento esté completo.

# Carga de los datos

En primer lugar, se cargan las librerías necesarias para realizar la inspección, el preprocesamiento, la visualización y el posterior análisis de los datos. Además, se establece una semilla para garantizar la reproducibilidad de los resultados.


``` r
library(tidyverse)
library(caret)
library(corrplot)
library(knitr)

# Semilla para garantizar la reproducibilidad
set.seed(2026)
```

A continuación, se importa el conjunto de datos `mental_health.csv`, almacenado en la carpeta `data` del proyecto. Se utiliza la función `read.csv()` y se comprueba que el fichero se ha cargado correctamente.


``` r
# Carga del conjunto de datos
datos_raw <- read.csv("data/mental_health.csv",
                      stringsAsFactors = FALSE)

# Comprobación de las dimensiones
dim(datos_raw)
```

```
## [1] 10000    51
```

``` r
# Número de observaciones y variables
cat("Número de observaciones:", nrow(datos_raw), "\n")
```

```
## Número de observaciones: 10000
```

``` r
cat("Número de variables:", ncol(datos_raw), "\n")
```

```
## Número de variables: 51
```

El conjunto de datos contiene **10.000 observaciones y 51 variables**, incluyendo la variable objetivo `Has_Mental_Health_Issue`. Los datos se almacenan en el objeto `datos_raw`, que se utilizará en los siguientes apartados para su inspección y análisis.

# Inspección de los datos

## Estructura y tipos de las variables

En primer lugar, vamos a comprobar las dimensiones del conjunto de datos, visualizar las primeras observaciones y conocer el tipo de cada variable.


``` r
# Dimensiones del dataset
dim(datos_raw)
```

```
## [1] 10000    51
```

``` r
# Primeras 6 filas
head(datos_raw)
```

```
##   Age Gender Country    Education Marital_Status Income_Level Employment_Status
## 1  40   Male     USA Some College         Single       Middle         Full-time
## 2  33   Male   India     Bachelor        Married       Middle        Unemployed
## 3  42   Male   Other  High School         Single          Low         Full-time
## 4  53   Male Germany     Bachelor         Single       Middle        Unemployed
## 5  32 Female   India     Bachelor         Single         High           Student
## 6  32   Male      UK Some College        Married       Middle         Part-time
##   Work_Hours_Per_Week Remote_Work Job_Satisfaction Work_Stress_Level
## 1                  27          No                6                 7
## 2                  47          No                6                 1
## 3                  53          No                1                 3
## 4                  42          No               10                 3
## 5                  13          No                4                 7
## 6                  53          No                3                 5
##   Work_Life_Balance Ever_Bullied_At_Work Company_Mental_Health_Support
## 1                 7                    0                            No
## 2                 5                    0                      Not sure
## 3                 8                    0                      Not sure
## 4                 5                    0                            No
## 5                 7                    1                      Not sure
## 6                 5                    1                           Yes
##   Exercise_Per_Week Sleep_Hours_Night Caffeine_Drinks_Day Alcohol_Frequency
## 1         1-2 times               6.2                   3             Never
## 2         1-2 times               9.6                   3            Weekly
## 3         3-4 times               7.7                   1             Never
## 4             Never               6.4                   1            Rarely
## 5         1-2 times               7.7                   0            Rarely
## 6         1-2 times               7.5                   0            Rarely
##   Smoking Screen_Time_Hours_Day Social_Media_Hours_Day Hobby_Time_Hours_Week
## 1  Former                   8.3                    4.8                     7
## 2 Current                   8.6                    0.0                     7
## 3  Former                  11.4                    3.0                     0
## 4  Former                   6.4                    2.9                     5
## 5 Current                   7.6                    4.0                     2
## 6   Never                   9.6                    5.3                     1
##   Diet_Quality Financial_Stress Feeling_Sad_Down Loss_Of_Interest Sleep_Trouble
## 1      Average                9                8                3            10
## 2      Average                9                1               10             4
## 3         Good                2                4                7             8
## 4      Average                1                3                8            10
## 5    Excellent                8                6                8             4
## 6         Good                1                9                3             0
##   Fatigue Poor_Appetite_Or_Overeating Feeling_Worthless
## 1       5                           9                 4
## 2       9                           4                10
## 3       8                           4                 9
## 4      10                           2                 2
## 5       5                           4                 5
## 6      10                           8                 3
##   Concentration_Difficulty Anxious_Nervous Panic_Attacks Mood_Swings
## 1                        4               1             0           9
## 2                        6               8             2           5
## 3                       10               1             0           9
## 4                        5               3             1           0
## 5                        7              10             1           0
## 6                        8              10             0           2
##   Irritability Obsessive_Thoughts Compulsive_Behavior Self_Harm_Thoughts
## 1            2                  3                   0                  0
## 2            9                  9                   3                  0
## 3            9                  7                   6                  0
## 4            2                  3                   3                  0
## 5           10                 10                   1                  0
## 6           10                  7                   3                  1
##   Suicidal_Thoughts Family_History_Mental_Illness Previously_Diagnosed
## 1                 0                             1                    1
## 2                 0                             0                    0
## 3                 0                             0                    0
## 4                 0                             1                    0
## 5                 0                             0                    1
## 6                 0                             0                    1
##   Ever_Sought_Treatment On_Therapy_Now On_Medication Trauma_History
## 1                     1              0             0              0
## 2                     0              0             0              0
## 3                     1              0             0              1
## 4                     0              1             0              0
## 5                     0              0             0              0
## 6                     0              0             0              0
##   Social_Support Close_Friends_Count Feel_Understood Loneliness
## 1              9                   3               4          6
## 2              3                   2               7          7
## 3              5                   3               6          9
## 4              1                   3              10          5
## 5              1                   5               1          6
## 6              3                   5               6          9
##   Discuss_Mental_Health Has_Mental_Health_Issue
## 1             Sometimes                       1
## 2             Sometimes                       1
## 3             Sometimes                       1
## 4                 Never                       1
## 5             Sometimes                       1
## 6                 Never                       1
```

``` r
# Estructura de los datos
str(datos_raw)
```

```
## 'data.frame':	10000 obs. of  51 variables:
##  $ Age                          : int  40 33 42 53 32 32 53 44 29 41 ...
##  $ Gender                       : chr  "Male" "Male" "Male" "Male" ...
##  $ Country                      : chr  "USA" "India" "Other" "Germany" ...
##  $ Education                    : chr  "Some College" "Bachelor" "High School" "Bachelor" ...
##  $ Marital_Status               : chr  "Single" "Married" "Single" "Single" ...
##  $ Income_Level                 : chr  "Middle" "Middle" "Low" "Middle" ...
##  $ Employment_Status            : chr  "Full-time" "Unemployed" "Full-time" "Unemployed" ...
##  $ Work_Hours_Per_Week          : int  27 47 53 42 13 53 41 13 22 29 ...
##  $ Remote_Work                  : chr  "No" "No" "No" "No" ...
##  $ Job_Satisfaction             : int  6 6 1 10 4 3 4 2 10 9 ...
##  $ Work_Stress_Level            : int  7 1 3 3 7 5 1 10 4 6 ...
##  $ Work_Life_Balance            : int  7 5 8 5 7 5 6 8 3 10 ...
##  $ Ever_Bullied_At_Work         : int  0 0 0 0 1 1 0 0 0 0 ...
##  $ Company_Mental_Health_Support: chr  "No" "Not sure" "Not sure" "No" ...
##  $ Exercise_Per_Week            : chr  "1-2 times" "1-2 times" "3-4 times" "Never" ...
##  $ Sleep_Hours_Night            : num  6.2 9.6 7.7 6.4 7.7 7.5 9.9 7.3 6.2 7.4 ...
##  $ Caffeine_Drinks_Day          : int  3 3 1 1 0 0 4 3 4 1 ...
##  $ Alcohol_Frequency            : chr  "Never" "Weekly" "Never" "Rarely" ...
##  $ Smoking                      : chr  "Former" "Current" "Former" "Former" ...
##  $ Screen_Time_Hours_Day        : num  8.3 8.6 11.4 6.4 7.6 9.6 2.6 7.4 9.3 10.7 ...
##  $ Social_Media_Hours_Day       : num  4.8 0 3 2.9 4 5.3 3.6 1 4.4 8.4 ...
##  $ Hobby_Time_Hours_Week        : int  7 7 0 5 2 1 2 0 4 5 ...
##  $ Diet_Quality                 : chr  "Average" "Average" "Good" "Average" ...
##  $ Financial_Stress             : int  9 9 2 1 8 1 4 1 9 3 ...
##  $ Feeling_Sad_Down             : int  8 1 4 3 6 9 5 5 6 0 ...
##  $ Loss_Of_Interest             : int  3 10 7 8 8 3 9 9 6 0 ...
##  $ Sleep_Trouble                : int  10 4 8 10 4 0 7 1 1 9 ...
##  $ Fatigue                      : int  5 9 8 10 5 10 1 8 3 0 ...
##  $ Poor_Appetite_Or_Overeating  : int  9 4 4 2 4 8 5 7 7 9 ...
##  $ Feeling_Worthless            : int  4 10 9 2 5 3 7 5 0 9 ...
##  $ Concentration_Difficulty     : int  4 6 10 5 7 8 4 0 6 7 ...
##  $ Anxious_Nervous              : int  1 8 1 3 10 10 10 3 8 2 ...
##  $ Panic_Attacks                : int  0 2 0 1 1 0 0 0 0 0 ...
##  $ Mood_Swings                  : int  9 5 9 0 0 2 10 1 10 5 ...
##  $ Irritability                 : int  2 9 9 2 10 10 4 1 1 4 ...
##  $ Obsessive_Thoughts           : int  3 9 7 3 10 7 9 2 5 6 ...
##  $ Compulsive_Behavior          : int  0 3 6 3 1 3 3 5 1 8 ...
##  $ Self_Harm_Thoughts           : int  0 0 0 0 0 1 0 0 0 0 ...
##  $ Suicidal_Thoughts            : int  0 0 0 0 0 0 1 0 0 0 ...
##  $ Family_History_Mental_Illness: int  1 0 0 1 0 0 0 0 0 0 ...
##  $ Previously_Diagnosed         : int  1 0 0 0 1 1 0 0 1 0 ...
##  $ Ever_Sought_Treatment        : int  1 0 1 0 0 0 0 1 0 1 ...
##  $ On_Therapy_Now               : int  0 0 0 1 0 0 1 0 1 0 ...
##  $ On_Medication                : int  0 0 0 0 0 0 0 0 1 0 ...
##  $ Trauma_History               : int  0 0 1 0 0 0 0 0 1 0 ...
##  $ Social_Support               : int  9 3 5 1 1 3 2 8 4 5 ...
##  $ Close_Friends_Count          : int  3 2 3 3 5 5 1 2 6 5 ...
##  $ Feel_Understood              : int  4 7 6 10 1 6 4 2 1 5 ...
##  $ Loneliness                   : int  6 7 9 5 6 9 2 8 6 2 ...
##  $ Discuss_Mental_Health        : chr  "Sometimes" "Sometimes" "Sometimes" "Never" ...
##  $ Has_Mental_Health_Issue      : int  1 1 1 1 1 1 1 1 1 0 ...
```

``` r
# Tipo de cada variable
tipos <- data.frame(
  Variable = names(datos_raw),
  Type = sapply(datos_raw, class)
)

kable(tipos, row.names = FALSE,
      caption = "Tipos de las variables")
```



Table: Tipos de las variables

|Variable                      |Type      |
|:-----------------------------|:---------|
|Age                           |integer   |
|Gender                        |character |
|Country                       |character |
|Education                     |character |
|Marital_Status                |character |
|Income_Level                  |character |
|Employment_Status             |character |
|Work_Hours_Per_Week           |integer   |
|Remote_Work                   |character |
|Job_Satisfaction              |integer   |
|Work_Stress_Level             |integer   |
|Work_Life_Balance             |integer   |
|Ever_Bullied_At_Work          |integer   |
|Company_Mental_Health_Support |character |
|Exercise_Per_Week             |character |
|Sleep_Hours_Night             |numeric   |
|Caffeine_Drinks_Day           |integer   |
|Alcohol_Frequency             |character |
|Smoking                       |character |
|Screen_Time_Hours_Day         |numeric   |
|Social_Media_Hours_Day        |numeric   |
|Hobby_Time_Hours_Week         |integer   |
|Diet_Quality                  |character |
|Financial_Stress              |integer   |
|Feeling_Sad_Down              |integer   |
|Loss_Of_Interest              |integer   |
|Sleep_Trouble                 |integer   |
|Fatigue                       |integer   |
|Poor_Appetite_Or_Overeating   |integer   |
|Feeling_Worthless             |integer   |
|Concentration_Difficulty      |integer   |
|Anxious_Nervous               |integer   |
|Panic_Attacks                 |integer   |
|Mood_Swings                   |integer   |
|Irritability                  |integer   |
|Obsessive_Thoughts            |integer   |
|Compulsive_Behavior           |integer   |
|Self_Harm_Thoughts            |integer   |
|Suicidal_Thoughts             |integer   |
|Family_History_Mental_Illness |integer   |
|Previously_Diagnosed          |integer   |
|Ever_Sought_Treatment         |integer   |
|On_Therapy_Now                |integer   |
|On_Medication                 |integer   |
|Trauma_History                |integer   |
|Social_Support                |integer   |
|Close_Friends_Count           |integer   |
|Feel_Understood               |integer   |
|Loneliness                    |integer   |
|Discuss_Mental_Health         |character |
|Has_Mental_Health_Issue       |integer   |

El conjunto de datos contiene **10.000 observaciones y 51 variables**, de las cuales 50 son predictoras y una es la variable objetivo.

Encontramos variables numéricas y categóricas. Aunque algunas aparecen almacenadas como numéricas, no todas representan cantidades continuas, ya que también hay indicadores binarios y escalas de valoración.

Esta diferencia será importante durante el preprocesamiento, puesto que no todas las variables deben tratarse de la misma manera.

## Descripción de las variables

Para comprender mejor el conjunto de datos, agrupamos las variables en ocho bloques temáticos según la información que representan.


``` r
bloques <- data.frame(
  Bloque = c(
    "Sociodemográficas",
    "Laborales",
    "Estilo de vida",
    "Síntomas",
    "Historial clínico",
    "Tratamiento",
    "Apoyo social",
    "Variable objetivo"
  ),

  Variables = c(
    "Age, Gender, Country, Education, Marital_Status, Income_Level",

    "Employment_Status, Work_Hours_Per_Week, Remote_Work, Job_Satisfaction, Work_Stress_Level, Work_Life_Balance, Ever_Bullied_At_Work, Company_Mental_Health_Support",

    "Exercise_Per_Week, Sleep_Hours_Night, Caffeine_Drinks_Day, Alcohol_Frequency, Smoking, Screen_Time_Hours_Day, Social_Media_Hours_Day, Hobby_Time_Hours_Week, Diet_Quality, Financial_Stress",

    "Feeling_Sad_Down, Loss_Of_Interest, Sleep_Trouble, Fatigue, Poor_Appetite_Or_Overeating, Feeling_Worthless, Concentration_Difficulty, Anxious_Nervous, Mood_Swings, Irritability, Obsessive_Thoughts, Compulsive_Behavior, Panic_Attacks",

    "Self_Harm_Thoughts, Suicidal_Thoughts, Family_History_Mental_Illness, Trauma_History",

    "Previously_Diagnosed, Ever_Sought_Treatment, On_Therapy_Now, On_Medication",

    "Social_Support, Close_Friends_Count, Feel_Understood, Loneliness, Discuss_Mental_Health",

    "Has_Mental_Health_Issue"
  ),

  Tipo = c(
    "Numérica, nominal y ordinal",
    "Numérica, categórica y binaria",
    "Numérica y categórica",
    "Numérica discreta",
    "Binaria (0/1)",
    "Binaria (0/1)",
    "Numérica y categórica",
    "Binaria (0/1)"
  )
)

kable(bloques, row.names = FALSE,
      caption = "Clasificación de las variables")
```



Table: Clasificación de las variables

|Bloque            |Variables                                                                                                                                                                                                                                |Tipo                           |
|:-----------------|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------------------------------|
|Sociodemográficas |Age, Gender, Country, Education, Marital_Status, Income_Level                                                                                                                                                                            |Numérica, nominal y ordinal    |
|Laborales         |Employment_Status, Work_Hours_Per_Week, Remote_Work, Job_Satisfaction, Work_Stress_Level, Work_Life_Balance, Ever_Bullied_At_Work, Company_Mental_Health_Support                                                                         |Numérica, categórica y binaria |
|Estilo de vida    |Exercise_Per_Week, Sleep_Hours_Night, Caffeine_Drinks_Day, Alcohol_Frequency, Smoking, Screen_Time_Hours_Day, Social_Media_Hours_Day, Hobby_Time_Hours_Week, Diet_Quality, Financial_Stress                                              |Numérica y categórica          |
|Síntomas          |Feeling_Sad_Down, Loss_Of_Interest, Sleep_Trouble, Fatigue, Poor_Appetite_Or_Overeating, Feeling_Worthless, Concentration_Difficulty, Anxious_Nervous, Mood_Swings, Irritability, Obsessive_Thoughts, Compulsive_Behavior, Panic_Attacks |Numérica discreta              |
|Historial clínico |Self_Harm_Thoughts, Suicidal_Thoughts, Family_History_Mental_Illness, Trauma_History                                                                                                                                                     |Binaria (0/1)                  |
|Tratamiento       |Previously_Diagnosed, Ever_Sought_Treatment, On_Therapy_Now, On_Medication                                                                                                                                                               |Binaria (0/1)                  |
|Apoyo social      |Social_Support, Close_Friends_Count, Feel_Understood, Loneliness, Discuss_Mental_Health                                                                                                                                                  |Numérica y categórica          |
|Variable objetivo |Has_Mental_Health_Issue                                                                                                                                                                                                                  |Binaria (0/1)                  |

Los bloques contienen la siguiente información:

- **Sociodemográficas:** edad, género, país, estudios, estado civil e ingresos. La edad es numérica, mientras que las restantes son categóricas nominales u ordinales.
- **Laborales:** situación laboral, horas trabajadas, trabajo en remoto, satisfacción laboral, estrés, conciliación y apoyo de la empresa. Incluyen cantidades, categorías y escalas de valoración.
- **Estilo de vida:** hábitos relacionados con el ejercicio, descanso, alimentación, consumo de sustancias, uso de pantallas y tiempo de ocio. Incluyen horas, frecuencias y categorías.
- **Síntomas:** doce síntomas emocionales y psicológicos valorados de 0 a 10, además de los ataques de pánico, cuya escala es de 0 a 4.
- **Historial clínico:** antecedentes familiares, experiencias traumáticas y pensamientos de autolesión o suicidio. Son indicadores binarios.
- **Tratamiento:** diagnóstico previo, búsqueda de ayuda, terapia y medicación. Son variables binarias.
- **Apoyo social:** número de amigos, apoyo recibido, sentimiento de comprensión y soledad, y facilidad para hablar de salud mental. Incluyen cantidades, escalas y categorías.
- **Variable objetivo:** `Has_Mental_Health_Issue`, que toma el valor 1 si el encuestado declara tener un problema de salud mental y 0 si no lo declara.

Esta clasificación nos ayudará a decidir cómo preparar las variables antes del entrenamiento de los modelos.

## Calidad de los datos

Antes de realizar el análisis, comprobamos si existen valores ausentes, filas duplicadas, valores fuera de rango, posibles inconsistencias y variables con varianza casi nula.

### Valores ausentes


``` r
# Número total de valores ausentes
sum(is.na(datos_raw))
```

```
## [1] 0
```

``` r
# Valores ausentes por variable
colSums(is.na(datos_raw))
```

```
##                           Age                        Gender 
##                             0                             0 
##                       Country                     Education 
##                             0                             0 
##                Marital_Status                  Income_Level 
##                             0                             0 
##             Employment_Status           Work_Hours_Per_Week 
##                             0                             0 
##                   Remote_Work              Job_Satisfaction 
##                             0                             0 
##             Work_Stress_Level             Work_Life_Balance 
##                             0                             0 
##          Ever_Bullied_At_Work Company_Mental_Health_Support 
##                             0                             0 
##             Exercise_Per_Week             Sleep_Hours_Night 
##                             0                             0 
##           Caffeine_Drinks_Day             Alcohol_Frequency 
##                             0                             0 
##                       Smoking         Screen_Time_Hours_Day 
##                             0                             0 
##        Social_Media_Hours_Day         Hobby_Time_Hours_Week 
##                             0                             0 
##                  Diet_Quality              Financial_Stress 
##                             0                             0 
##              Feeling_Sad_Down              Loss_Of_Interest 
##                             0                             0 
##                 Sleep_Trouble                       Fatigue 
##                             0                             0 
##   Poor_Appetite_Or_Overeating             Feeling_Worthless 
##                             0                             0 
##      Concentration_Difficulty               Anxious_Nervous 
##                             0                             0 
##                 Panic_Attacks                   Mood_Swings 
##                             0                             0 
##                  Irritability            Obsessive_Thoughts 
##                             0                             0 
##           Compulsive_Behavior            Self_Harm_Thoughts 
##                             0                             0 
##             Suicidal_Thoughts Family_History_Mental_Illness 
##                             0                             0 
##          Previously_Diagnosed         Ever_Sought_Treatment 
##                             0                             0 
##                On_Therapy_Now                 On_Medication 
##                             0                             0 
##                Trauma_History                Social_Support 
##                             0                             0 
##           Close_Friends_Count               Feel_Understood 
##                             0                             0 
##                    Loneliness         Discuss_Mental_Health 
##                             0                             0 
##       Has_Mental_Health_Issue 
##                             0
```

No se han encontrado valores ausentes en el conjunto de datos, por lo que no será necesario aplicar técnicas de imputación.

### Filas duplicadas


``` r
# Número de filas duplicadas
sum(duplicated(datos_raw))
```

```
## [1] 0
```

No se han encontrado registros duplicados, por lo que no será necesario eliminar observaciones por este motivo.

### Valores fuera de rango

Comprobamos los valores mínimos y máximos de algunas variables para detectar posibles errores en las observaciones.


``` r
# Edad
range(datos_raw$Age, na.rm = TRUE)
```

```
## [1] 18 75
```

``` r
# Horas de sueño
range(datos_raw$Sleep_Hours_Night, na.rm = TRUE)
```

```
## [1]  3 11
```

``` r
# Horas de trabajo semanales
range(datos_raw$Work_Hours_Per_Week, na.rm = TRUE)
```

```
## [1]  0 74
```

``` r
# Horas de pantalla al día
range(datos_raw$Screen_Time_Hours_Day, na.rm = TRUE)
```

```
## [1]  1 16
```

``` r
# Frecuencia de ataques de pánico
range(datos_raw$Panic_Attacks, na.rm = TRUE)
```

```
## [1] 0 4
```

Mediante estas comprobaciones podemos conocer los valores extremos de variables importantes como la edad, las horas de sueño y el tiempo dedicado al trabajo o a las pantallas.

También comprobamos que las variables de síntomas se encuentren dentro de sus escalas correspondientes.


``` r
# Variables de síntomas con escala de 0 a 10
sintomas <- c(
  "Feeling_Sad_Down",
  "Loss_Of_Interest",
  "Sleep_Trouble",
  "Fatigue",
  "Poor_Appetite_Or_Overeating",
  "Feeling_Worthless",
  "Concentration_Difficulty",
  "Anxious_Nervous",
  "Mood_Swings",
  "Irritability",
  "Obsessive_Thoughts",
  "Compulsive_Behavior"
)

# Síntomas fuera del rango 0-10
sum(datos_raw[sintomas] < 0 |
    datos_raw[sintomas] > 10, na.rm = TRUE)
```

```
## [1] 0
```

``` r
# Ataques de pánico fuera del rango 0-4
sum(datos_raw$Panic_Attacks < 0 |
    datos_raw$Panic_Attacks > 4, na.rm = TRUE)
```

```
## [1] 0
```

``` r
# Valores incorrectos en la variable objetivo
sum(!datos_raw$Has_Mental_Health_Issue %in% c(0, 1),
    na.rm = TRUE)
```

```
## [1] 0
```

Estas comprobaciones permiten detectar puntuaciones que no respeten las escalas definidas en el conjunto de datos.

Si se identifican valores fuera de rango, será necesario revisarlos antes de realizar el análisis supervisado.

### Posibles inconsistencias

Además de comprobar los rangos, revisamos algunas relaciones entre variables que podrían resultar contradictorias.


``` r
# Desempleados con horas de trabajo
sum(datos_raw$Employment_Status == "Unemployed" &
    datos_raw$Work_Hours_Per_Week > 0, na.rm = TRUE)
```

```
## [1] 1232
```

``` r
# Estudiantes con horas de trabajo
sum(datos_raw$Employment_Status == "Student" &
    datos_raw$Work_Hours_Per_Week > 0, na.rm = TRUE)
```

```
## [1] 2021
```

``` r
# Más horas de redes sociales que de pantalla
sum(datos_raw$Social_Media_Hours_Day >
    datos_raw$Screen_Time_Hours_Day, na.rm = TRUE)
```

```
## [1] 1479
```

Se han comprobado tres posibles inconsistencias: personas desempleadas que indican horas de trabajo, estudiantes que también trabajan y personas que declaran dedicar más tiempo a redes sociales que al uso total de pantallas.

Estas situaciones pueden indicar problemas en los datos, aunque no necesariamente son errores. Por ejemplo, una persona estudiante puede trabajar al mismo tiempo, y alguien desempleado puede haber realizado trabajos ocasionales.

Por este motivo, no eliminaremos estos registros automáticamente. Se tendrán en cuenta como posibles limitaciones del conjunto de datos.

### Variables con varianza casi nula

Utilizamos la función `nearZeroVar()` del paquete `caret`, que permite detectar variables cuyos valores presentan muy poca variación.


``` r
# Variables con varianza casi nula
nearZeroVar(datos_raw)
```

```
## integer(0)
```

``` r
# Información detallada
nzv <- nearZeroVar(datos_raw, saveMetrics = TRUE)

# Mostrar resultados
nzv
```

```
##                               freqRatio percentUnique zeroVar   nzv
## Age                            2.641243          0.57   FALSE FALSE
## Gender                         1.046193          0.04   FALSE FALSE
## Country                        1.239056          0.06   FALSE FALSE
## Education                      1.387763          0.05   FALSE FALSE
## Marital_Status                 1.135861          0.04   FALSE FALSE
## Income_Level                   1.714334          0.03   FALSE FALSE
## Employment_Status              2.213261          0.05   FALSE FALSE
## Work_Hours_Per_Week            1.017456          0.68   FALSE FALSE
## Remote_Work                    1.599871          0.03   FALSE FALSE
## Job_Satisfaction               1.013514          0.10   FALSE FALSE
## Work_Stress_Level              1.023099          0.10   FALSE FALSE
## Work_Life_Balance              1.005888          0.10   FALSE FALSE
## Ever_Bullied_At_Work           3.042037          0.02   FALSE FALSE
## Company_Mental_Health_Support  2.193728          0.03   FALSE FALSE
## Exercise_Per_Week              1.398319          0.04   FALSE FALSE
## Sleep_Hours_Night              1.035948          0.81   FALSE FALSE
## Caffeine_Drinks_Day            1.015666          0.09   FALSE FALSE
## Alcohol_Frequency              1.296031          0.04   FALSE FALSE
## Smoking                        2.339905          0.03   FALSE FALSE
## Screen_Time_Hours_Day          1.535948          1.51   FALSE FALSE
## Social_Media_Hours_Day         6.066327          1.14   FALSE FALSE
## Hobby_Time_Hours_Week          1.572854          0.20   FALSE FALSE
## Diet_Quality                   1.381892          0.04   FALSE FALSE
## Financial_Stress               1.001946          0.10   FALSE FALSE
## Feeling_Sad_Down               1.010695          0.11   FALSE FALSE
## Loss_Of_Interest               1.006349          0.11   FALSE FALSE
## Sleep_Trouble                  1.017058          0.11   FALSE FALSE
## Fatigue                        1.021277          0.11   FALSE FALSE
## Poor_Appetite_Or_Overeating    1.010753          0.11   FALSE FALSE
## Feeling_Worthless              1.012712          0.11   FALSE FALSE
## Concentration_Difficulty       1.012605          0.11   FALSE FALSE
## Anxious_Nervous                1.035032          0.11   FALSE FALSE
## Panic_Attacks                  4.644903          0.05   FALSE FALSE
## Mood_Swings                    1.005313          0.11   FALSE FALSE
## Irritability                   1.003195          0.11   FALSE FALSE
## Obsessive_Thoughts             1.006316          0.11   FALSE FALSE
## Compulsive_Behavior            1.004224          0.11   FALSE FALSE
## Self_Harm_Thoughts            11.804097          0.02   FALSE FALSE
## Suicidal_Thoughts              9.504202          0.02   FALSE FALSE
## Family_History_Mental_Illness  2.285151          0.02   FALSE FALSE
## Previously_Diagnosed           3.032258          0.02   FALSE FALSE
## Ever_Sought_Treatment          1.760906          0.02   FALSE FALSE
## On_Therapy_Now                 4.235602          0.02   FALSE FALSE
## On_Medication                  4.425936          0.02   FALSE FALSE
## Trauma_History                 2.990423          0.02   FALSE FALSE
## Social_Support                 1.004869          0.10   FALSE FALSE
## Close_Friends_Count            1.016771          0.14   FALSE FALSE
## Feel_Understood                1.004776          0.10   FALSE FALSE
## Loneliness                     1.034884          0.10   FALSE FALSE
## Discuss_Mental_Health          1.419944          0.04   FALSE FALSE
## Has_Mental_Health_Issue       11.755102          0.02   FALSE FALSE
```

Las variables con varianza casi nula pueden aportar poca información a los modelos predictivos, ya que sus valores son prácticamente iguales en la mayoría de las observaciones.

En caso de detectar alguna, se estudiará si conviene eliminarla durante el preprocesamiento.

## Estadísticos descriptivos

Calculamos los principales estadísticos para conocer mejor la distribución de las variables numéricas y categóricas.

### Variables numéricas

Utilizamos la función `summary()` para obtener los valores mínimos, máximos, media, mediana y cuartiles.


``` r
# Seleccionar variables numéricas
datos_num <- datos_raw %>%
  select(where(is.numeric))

# Resumen estadístico
summary(datos_num)
```

```
##       Age        Work_Hours_Per_Week Job_Satisfaction Work_Stress_Level
##  Min.   :18.00   Min.   : 0.00       Min.   : 1.000   Min.   : 1.000   
##  1st Qu.:26.00   1st Qu.:33.00       1st Qu.: 3.000   1st Qu.: 3.000   
##  Median :34.00   Median :40.00       Median : 6.000   Median : 6.000   
##  Mean   :34.95   Mean   :39.66       Mean   : 5.503   Mean   : 5.494   
##  3rd Qu.:43.00   3rd Qu.:46.00       3rd Qu.: 8.000   3rd Qu.: 8.000   
##  Max.   :75.00   Max.   :74.00       Max.   :10.000   Max.   :10.000   
##  Work_Life_Balance Ever_Bullied_At_Work Sleep_Hours_Night Caffeine_Drinks_Day
##  Min.   : 1.000    Min.   :0.0000       Min.   : 3.000    Min.   :0.000      
##  1st Qu.: 3.000    1st Qu.:0.0000       1st Qu.: 5.900    1st Qu.:1.000      
##  Median : 5.000    Median :0.0000       Median : 6.800    Median :2.000      
##  Mean   : 5.489    Mean   :0.2474       Mean   : 6.815    Mean   :2.023      
##  3rd Qu.: 8.000    3rd Qu.:0.0000       3rd Qu.: 7.700    3rd Qu.:3.000      
##  Max.   :10.000    Max.   :1.0000       Max.   :11.000    Max.   :8.000      
##  Screen_Time_Hours_Day Social_Media_Hours_Day Hobby_Time_Hours_Week
##  Min.   : 1.000        Min.   : 0.000         Min.   : 0.00        
##  1st Qu.: 5.000        1st Qu.: 1.300         1st Qu.: 2.00        
##  Median : 7.000        Median : 3.000         Median : 5.00        
##  Mean   : 7.065        Mean   : 3.142         Mean   : 4.78        
##  3rd Qu.: 9.100        3rd Qu.: 4.700         3rd Qu.: 7.00        
##  Max.   :16.000        Max.   :12.000         Max.   :19.00        
##  Financial_Stress Feeling_Sad_Down Loss_Of_Interest Sleep_Trouble   
##  Min.   : 1.000   Min.   : 0.00    Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 3.000   1st Qu.: 2.00    1st Qu.: 2.000   1st Qu.: 2.000  
##  Median : 5.000   Median : 5.00    Median : 5.000   Median : 5.000  
##  Mean   : 5.483   Mean   : 4.99    Mean   : 5.018   Mean   : 5.001  
##  3rd Qu.: 8.000   3rd Qu.: 8.00    3rd Qu.: 8.000   3rd Qu.: 8.000  
##  Max.   :10.000   Max.   :10.00    Max.   :10.000   Max.   :10.000  
##     Fatigue       Poor_Appetite_Or_Overeating Feeling_Worthless
##  Min.   : 0.000   Min.   : 0.000              Min.   : 0.000   
##  1st Qu.: 2.000   1st Qu.: 2.000              1st Qu.: 2.000   
##  Median : 5.000   Median : 5.000              Median : 5.000   
##  Mean   : 5.003   Mean   : 4.968              Mean   : 4.984   
##  3rd Qu.: 8.000   3rd Qu.: 8.000              3rd Qu.: 8.000   
##  Max.   :10.000   Max.   :10.000              Max.   :10.000   
##  Concentration_Difficulty Anxious_Nervous  Panic_Attacks     Mood_Swings    
##  Min.   : 0.000           Min.   : 0.000   Min.   :0.0000   Min.   : 0.000  
##  1st Qu.: 2.000           1st Qu.: 2.000   1st Qu.:0.0000   1st Qu.: 2.000  
##  Median : 5.000           Median : 5.000   Median :0.0000   Median : 5.000  
##  Mean   : 4.951           Mean   : 4.987   Mean   :0.5469   Mean   : 5.042  
##  3rd Qu.: 8.000           3rd Qu.: 8.000   3rd Qu.:1.0000   3rd Qu.: 8.000  
##  Max.   :10.000           Max.   :10.000   Max.   :4.0000   Max.   :10.000  
##   Irritability    Obsessive_Thoughts Compulsive_Behavior Self_Harm_Thoughts
##  Min.   : 0.000   Min.   : 0.000     Min.   : 0.00       Min.   :0.0000    
##  1st Qu.: 2.000   1st Qu.: 2.000     1st Qu.: 2.00       1st Qu.:0.0000    
##  Median : 5.000   Median : 5.000     Median : 5.00       Median :0.0000    
##  Mean   : 5.032   Mean   : 4.942     Mean   : 4.96       Mean   :0.0781    
##  3rd Qu.: 8.000   3rd Qu.: 8.000     3rd Qu.: 8.00       3rd Qu.:0.0000    
##  Max.   :10.000   Max.   :10.000     Max.   :10.00       Max.   :1.0000    
##  Suicidal_Thoughts Family_History_Mental_Illness Previously_Diagnosed
##  Min.   :0.0000    Min.   :0.0000                Min.   :0.000       
##  1st Qu.:0.0000    1st Qu.:0.0000                1st Qu.:0.000       
##  Median :0.0000    Median :0.0000                Median :0.000       
##  Mean   :0.0952    Mean   :0.3044                Mean   :0.248       
##  3rd Qu.:0.0000    3rd Qu.:1.0000                3rd Qu.:0.000       
##  Max.   :1.0000    Max.   :1.0000                Max.   :1.000       
##  Ever_Sought_Treatment On_Therapy_Now  On_Medication    Trauma_History  
##  Min.   :0.0000        Min.   :0.000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000        1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000        Median :0.000   Median :0.0000   Median :0.0000  
##  Mean   :0.3622        Mean   :0.191   Mean   :0.1843   Mean   :0.2506  
##  3rd Qu.:1.0000        3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:1.0000  
##  Max.   :1.0000        Max.   :1.000   Max.   :1.0000   Max.   :1.0000  
##  Social_Support  Close_Friends_Count Feel_Understood   Loneliness   
##  Min.   : 1.00   Min.   : 0.000      Min.   : 1.00   Min.   : 1.00  
##  1st Qu.: 3.00   1st Qu.: 3.000      1st Qu.: 3.00   1st Qu.: 3.00  
##  Median : 5.00   Median : 4.000      Median : 5.00   Median : 5.00  
##  Mean   : 5.46   Mean   : 4.019      Mean   : 5.47   Mean   : 5.45  
##  3rd Qu.: 8.00   3rd Qu.: 5.000      3rd Qu.: 8.00   3rd Qu.: 8.00  
##  Max.   :10.00   Max.   :13.000      Max.   :10.00   Max.   :10.00  
##  Has_Mental_Health_Issue
##  Min.   :0.0000         
##  1st Qu.:1.0000         
##  Median :1.0000         
##  Mean   :0.9216         
##  3rd Qu.:1.0000         
##  Max.   :1.0000
```

``` r
# Desviación típica
sapply(datos_num, sd, na.rm = TRUE)
```

```
##                           Age           Work_Hours_Per_Week 
##                    11.1563897                    10.0167111 
##              Job_Satisfaction             Work_Stress_Level 
##                     2.8843413                     2.8816303 
##             Work_Life_Balance          Ever_Bullied_At_Work 
##                     2.8755720                     0.4315227 
##             Sleep_Hours_Night           Caffeine_Drinks_Day 
##                     1.3779754                     1.4274316 
##         Screen_Time_Hours_Day        Social_Media_Hours_Day 
##                     2.9558740                     2.2542039 
##         Hobby_Time_Hours_Week              Financial_Stress 
##                     3.5635519                     2.8862312 
##              Feeling_Sad_Down              Loss_Of_Interest 
##                     3.1564677                     3.1483424 
##                 Sleep_Trouble                       Fatigue 
##                     3.1644590                     3.1331886 
##   Poor_Appetite_Or_Overeating             Feeling_Worthless 
##                     3.1509672                     3.1712073 
##      Concentration_Difficulty               Anxious_Nervous 
##                     3.1553755                     3.1561111 
##                 Panic_Attacks                   Mood_Swings 
##                     0.9809672                     3.1659643 
##                  Irritability            Obsessive_Thoughts 
##                     3.1470547                     3.1800425 
##           Compulsive_Behavior            Self_Harm_Thoughts 
##                     3.1545978                     0.2683423 
##             Suicidal_Thoughts Family_History_Mental_Illness 
##                     0.2935057                     0.4601759 
##          Previously_Diagnosed         Ever_Sought_Treatment 
##                     0.4318734                     0.4806602 
##                On_Therapy_Now                 On_Medication 
##                     0.3931087                     0.3877480 
##                Trauma_History                Social_Support 
##                     0.4333802                     2.8829858 
##           Close_Friends_Count               Feel_Understood 
##                     1.9984311                     2.8720214 
##                    Loneliness       Has_Mental_Health_Issue 
##                     2.8623223                     0.2688134
```

La media y la mediana permiten conocer los valores centrales, mientras que la desviación típica indica cuánto varían los datos respecto a la media.

Para interpretar mejor los resultados, nos centramos en algunas variables relacionadas con nuestro objetivo de estudio.


``` r
# Media de algunas variables relevantes
mean(datos_raw$Age, na.rm = TRUE)
```

```
## [1] 34.9479
```

``` r
mean(datos_raw$Sleep_Hours_Night, na.rm = TRUE)
```

```
## [1] 6.81545
```

``` r
mean(datos_raw$Work_Stress_Level, na.rm = TRUE)
```

```
## [1] 5.4939
```

``` r
mean(datos_raw$Social_Support, na.rm = TRUE)
```

```
## [1] 5.4597
```

Estas variables resultan interesantes porque representan características personales, hábitos de descanso, condiciones laborales y apoyo social.

La edad media permite conocer el perfil general de los encuestados, mientras que las horas de sueño aportan información sobre sus hábitos de descanso.

Por otro lado, el estrés laboral y el apoyo social son factores que podrían estar relacionados con el bienestar emocional.

Sin embargo, estos estadísticos describen al conjunto de encuestados y, por sí solos, no permiten establecer relaciones con la variable objetivo.

### Variables categóricas

Para las variables categóricas utilizamos tablas de frecuencias, que muestran cuántas observaciones pertenecen a cada categoría.


``` r
# Seleccionar variables categóricas
datos_cat <- datos_raw %>%
  select(where(is.character))

# Frecuencias de todas las variables categóricas
lapply(datos_cat, table)
```

```
## $Gender
## 
##            Female              Male        Non-binary Prefer not to say 
##              4892              4676               316               116 
## 
## $Country
## 
##  Brazil Germany   India   Other      UK     USA 
##    1077     968    2033    1945    1458    2519 
## 
## $Education
## 
##     Bachelor  High School       Master          PhD Some College 
##         3493         1991         1514          485         2517 
## 
## $Marital_Status
## 
## Divorced  Married   Single  Widowed 
##     1227     3982     4523      268 
## 
## $Income_Level
## 
##   High    Low Middle 
##   1990   2951   5059 
## 
## $Employment_Status
## 
##     Full-time     Part-time Self-employed       Student    Unemployed 
##          4473          1537           737          2021          1232 
## 
## $Remote_Work
## 
## Hybrid     No    Yes 
##   1943   4958   3099 
## 
## $Company_Mental_Health_Support
## 
##       No Not sure      Yes 
##     5526     1955     2519 
## 
## $Exercise_Per_Week
## 
## 1-2 times 3-4 times  5+ times     Never 
##      3493      2498      1511      2498 
## 
## $Alcohol_Frequency
## 
##  Daily  Never Rarely Weekly 
##    996   3074   3984   1946 
## 
## $Smoking
## 
## Current  Former   Never 
##    1530    2536    5934 
## 
## $Diet_Quality
## 
##   Average Excellent      Good      Poor 
##      4060      1050      2938      1952 
## 
## $Discuss_Mental_Health
## 
##      Never     Rarely  Sometimes Yes easily 
##       1482       2517       3574       2427
```

``` r
# Frecuencias de algunas variables relevantes
table(datos_raw$Gender)
```

```
## 
##            Female              Male        Non-binary Prefer not to say 
##              4892              4676               316               116
```

``` r
table(datos_raw$Education)
```

```
## 
##     Bachelor  High School       Master          PhD Some College 
##         3493         1991         1514          485         2517
```

``` r
table(datos_raw$Employment_Status)
```

```
## 
##     Full-time     Part-time Self-employed       Student    Unemployed 
##          4473          1537           737          2021          1232
```

``` r
table(datos_raw$Income_Level)
```

```
## 
##   High    Low Middle 
##   1990   2951   5059
```

``` r
table(datos_raw$Diet_Quality)
```

```
## 
##   Average Excellent      Good      Poor 
##      4060      1050      2938      1952
```

``` r
table(datos_raw$Smoking)
```

```
## 
## Current  Former   Never 
##    1530    2536    5934
```

Estas tablas permiten conocer las categorías más frecuentes y comprobar si existen diferencias importantes entre sus tamaños.

Nos interesan especialmente variables como el género, la situación laboral, el nivel de ingresos y la calidad de la alimentación, ya que forman parte de las características personales y los hábitos que queremos estudiar.

Las diferencias en la frecuencia de cada categoría se tendrán en cuenta posteriormente, especialmente si existen grupos con pocas observaciones.

## Variable objetivo

La variable que queremos predecir es `Has_Mental_Health_Issue`, que indica si una persona declara tener un problema de salud mental.

Se trata de una variable binaria:

- **0:** no declara un problema de salud mental.
- **1:** declara un problema de salud mental.

Es importante señalar que esta variable representa la declaración del encuestado y no necesariamente un diagnóstico clínico.

### Distribución de las clases


``` r
# Frecuencia de cada clase
table(datos_raw$Has_Mental_Health_Issue)
```

```
## 
##    0    1 
##  784 9216
```

``` r
# Proporción de cada clase
prop.table(
  table(datos_raw$Has_Mental_Health_Issue)
)
```

```
## 
##      0      1 
## 0.0784 0.9216
```

``` r
# Porcentaje de cada clase
round(
  prop.table(
    table(datos_raw$Has_Mental_Health_Issue)
  ) * 100,
  2
)
```

```
## 
##     0     1 
##  7.84 92.16
```

### Representación gráfica

Para observar más claramente la diferencia entre las dos clases, representamos su distribución mediante un diagrama de barras.


``` r
# Diagrama de barras de la variable objetivo
ggplot(datos_raw,
       aes(x = factor(Has_Mental_Health_Issue))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Distribución de la variable objetivo",
    x = "Problema de salud mental (0 = No, 1 = Sí)",
    y = "Número de personas"
  ) +
  theme_minimal()
```

<img src="C:/Users/david/AppData/Local/Temp/claude/2d7e0a91c531013b/6c6f195c-1ad3-40c2-adb3-b598d7ff1594/scratchpad/SeguimientoI_files/figure-html/grafico-objetivo-1.png" alt="" width="672" />

### Análisis del balance entre clases

El conjunto de datos presenta un importante desbalanceo entre las dos clases.

De las 10.000 personas encuestadas, **9.216 (92,16 %) declaran tener un problema de salud mental**, mientras que **784 (7,84 %) no lo declaran**.

Esto significa que la clase positiva es mucho más frecuente que la negativa.

Este desbalanceo supone una dificultad para la clasificación, ya que un modelo que predijera siempre la clase mayoritaria obtendría una exactitud del 92,16 % sin aprender a distinguir correctamente ambas clases.

Por ello, en el análisis supervisado será importante utilizar métricas como la sensibilidad, la especificidad, la exactitud balanceada y el AUC, además de la exactitud global.

También será necesario prestar especial atención al rendimiento sobre la clase minoritaria, para comprobar que el modelo es capaz de identificar ambas clases y no únicamente la más frecuente.

## Conclusiones de la inspección de los datos

Tras realizar la inspección inicial, hemos comprobado que el conjunto de datos contiene **10.000 observaciones y 51 variables** de diferentes tipos. Esto nos permitirá estudiar la salud mental desde distintos aspectos, como los hábitos de vida, las condiciones laborales, los síntomas y el apoyo social.

En cuanto a la calidad de los datos, no se han encontrado valores ausentes ni registros duplicados. También hemos realizado comprobaciones de los rangos y de posibles inconsistencias entre variables, que tendremos en cuenta durante el preprocesamiento.

Los estadísticos descriptivos nos han permitido conocer mejor las características de los encuestados y las distribuciones de las variables numéricas y categóricas.

Uno de los resultados más importantes es el **desbalanceo de la variable objetivo**, ya que el 92,16 % de los encuestados declara tener un problema de salud mental, frente al 7,84 % que no lo declara. Esto supone una dificultad para la clasificación, por lo que será necesario utilizar métricas como la sensibilidad, la especificidad, la exactitud balanceada y el AUC, en lugar de basarnos únicamente en la exactitud.

En general, esta primera inspección nos ha permitido conocer la estructura del conjunto de datos e identificar los principales aspectos que debemos tener en cuenta antes de realizar el análisis supervisado.

# Visualización de los datos

## Distribución de las variables numéricas


``` r
# Variables continuas o de recuento (las escalas y binarias se representan aparte)
continuas <- c("Age", "Work_Hours_Per_Week", "Sleep_Hours_Night", "Caffeine_Drinks_Day",
               "Screen_Time_Hours_Day", "Social_Media_Hours_Day", "Hobby_Time_Hours_Week",
               "Close_Friends_Count")
# Un histograma por variable (formato largo + facet_wrap)
datos_raw %>% select(all_of(continuas)) %>% pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", colour = "white") +
  facet_wrap(~ name, scales = "free", ncol = 4) +
  labs(title = "Distribución de las variables continuas y de recuento", x = "Valor", y = "Frecuencia")
```

<img src="C:/Users/david/AppData/Local/Temp/claude/2d7e0a91c531013b/6c6f195c-1ad3-40c2-adb3-b598d7ff1594/scratchpad/SeguimientoI_files/figure-html/histogramas-continuas-1.png" alt="" width="672" />


``` r
# Diagramas de caja: los puntos rojos son los atípicos según el criterio 1,5·RIC
datos_raw %>% select(all_of(continuas)) %>% pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_boxplot(fill = "#a6cee3", outlier.colour = "#e31a1c", outlier.alpha = .5, width = .5) +
  facet_wrap(~ name, scales = "free_x", ncol = 4) +
  labs(title = "Diagramas de caja de las variables continuas",
       subtitle = "En rojo, los valores atípicos según el criterio 1,5·RIC", x = "Valor", y = NULL) +
  theme(axis.text.y = element_blank())
```

<img src="C:/Users/david/AppData/Local/Temp/claude/2d7e0a91c531013b/6c6f195c-1ad3-40c2-adb3-b598d7ff1594/scratchpad/SeguimientoI_files/figure-html/boxplots-continuas-1.png" alt="" width="672" />

``` r
# Recuento de atípicos: valores a más de 1,5·RIC por debajo de Q1 o por encima de Q3
atipicos <- map_dfr(continuas, function(v) {
  x <- datos_raw[[v]]; q <- quantile(x, c(.25, .75)); r <- diff(q)
  tibble(Variable = v, `Atípicos (n)` = sum(x < q[1] - 1.5 * r | x > q[2] + 1.5 * r))
}) %>% mutate(`Atípicos (%)` = 100 * `Atípicos (n)` / nrow(datos_raw))
kable(atipicos, digits = 2, caption = "Valores atípicos (criterio 1,5·RIC).")
```



Table: Valores atípicos (criterio 1,5·RIC).

|Variable               | Atípicos (n)| Atípicos (%)|
|:----------------------|------------:|------------:|
|Age                    |           25|         0.25|
|Work_Hours_Per_Week    |           94|         0.94|
|Sleep_Hours_Night      |           84|         0.84|
|Caffeine_Drinks_Day    |           46|         0.46|
|Screen_Time_Hours_Day  |           36|         0.36|
|Social_Media_Hours_Day |           33|         0.33|
|Hobby_Time_Hours_Week  |           65|         0.65|
|Close_Friends_Count    |          217|         2.17|


``` r
# Histograma de cada síntoma (barras de anchura 1 porque son valores enteros)
datos_raw %>% select(all_of(sintomas), Panic_Attacks) %>% pivot_longer(everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, fill = "#2c7fb8", colour = "white") +
  facet_wrap(~ name, ncol = 5) +
  labs(title = "Distribución de los síntomas", x = "Intensidad del síntoma", y = "Frecuencia")
```

<img src="C:/Users/david/AppData/Local/Temp/claude/2d7e0a91c531013b/6c6f195c-1ad3-40c2-adb3-b598d7ff1594/scratchpad/SeguimientoI_files/figure-html/histogramas-sintomas-1.png" alt="" width="672" />

**Interpretación.** Las variables continuas tienen distribuciones aproximadamente **simétricas** (edad, horas de trabajo, sueño, pantalla) o **ligeramente asimétricas a la derecha** (cafeína, redes sociales, ocio, amigos). Los atípicos son pocos, como mucho el 2.2 % de una variable, y todos son **valores plausibles** (por ejemplo, 70 horas de trabajo o 11 horas de sueño), no errores de medida. **Decisión:** no los eliminamos; los modelos de árboles son insensibles a ellos y en los demás el escalado reduce su influencia. Los doce síntomas tienen una distribución **casi uniforme** entre 0 y 10, algo poco habitual en datos clínicos reales, donde esperaríamos muchas personas con pocos síntomas y pocas con muchos. Solo los ataques de pánico tienen esa forma asimétrica.\

## Distribución de las variables categóricas


``` r
# Tabla de frecuencias de cada variable categórica (Variable, Categoría, Frecuencia)
frec_cat <- do.call(rbind, lapply(names(datos_cat), function(v) {
  t <- table(datos_cat[[v]])
  data.frame(Variable = v, Categoría = names(t), Frecuencia = as.numeric(t))
}))

# Diagrama de barras horizontal de cada variable categórica, ordenado por frecuencia
frec_cat %>%
  ggplot(aes(x = reorder(Categoría, Frecuencia), y = Frecuencia)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Frecuencia de cada categoría en las variables categóricas", x = NULL, y = "Número de personas") +
  theme(axis.text.y = element_text(size = 8))
```

<img src="C:/Users/david/AppData/Local/Temp/claude/2d7e0a91c531013b/6c6f195c-1ad3-40c2-adb3-b598d7ff1594/scratchpad/SeguimientoI_files/figure-html/barras-categoricas-1.png" alt="" width="672" />

**Interpretación.** Las categorías están razonablemente repartidas en casi todas las variables. Los casos que destacan son las categorías minoritarias ya señaladas («Prefer not to say» en género, «Widowed» en estado civil, «PhD» en educación) y el peso de Estados Unidos e India entre los países. No hay erratas ni variantes de escritura de una misma categoría, así que **no hace falta unificar categorías**.

## Relación de las variables con la variable objetivo

> **Pendiente:** comparación de las variables según la clase (diagramas de caja, densidades y proporciones por categoría).

## Correlaciones entre variables

> **Pendiente:** matriz de correlaciones e identificación de variables redundantes.

## Conclusiones de la exploración

> **Pendiente:** principales hallazgos de la inspección y la visualización, y decisiones de preprocesado que se derivan de ellos.

# Planteamiento del análisis supervisado

## Problema a resolver

El objetivo del análisis supervisado es **predecir si una persona declara tener un problema de salud mental** a partir de sus características sociodemográficas, laborales, de estilo de vida, de sintomatología y de entorno social. La pregunta que se quiere responder es la siguiente: *¿es posible identificar, a partir de los hábitos y las circunstancias de una persona, si presenta un problema de salud mental, y qué factores influyen más en ello?*

Se trata de un problema de **clasificación binaria**, ya que la variable objetivo, `Has_Mental_Health_Issue`, es categórica y solo toma dos valores: **1** (la persona declara un problema de salud mental) y **0** (no lo declara). Para el modelado, esta variable se tratará como un factor con dos niveles.

Hay que tener en cuenta que **las clases están muy desbalanceadas** (92,2 % frente a 7,8 %). Por ello, la *accuracy* no es una métrica adecuada por sí sola: un modelo trivial que predijera siempre la clase mayoritaria (ZeroR) ya alcanzaría un 92,2 % de acierto sin aprender nada. Este valor se tomará como **línea base** que cualquier modelo debe superar, y la evaluación se apoyará en métricas que tengan en cuenta ambas clases.

Además de la capacidad predictiva, interesa la **interpretabilidad**: identificar qué variables tienen más peso en la predicción permitirá relacionar los resultados con los factores de riesgo y de protección de la salud mental.

## Variables predictoras y posible fuga de información

Se parte de las **50 variables** restantes como predictoras, agrupadas en siete bloques.


``` r
# Bloques temáticos de variables predictoras
bloques <- list(
  Sociodemograficos = c("Age", "Gender", "Country", "Education",
                        "Marital_Status", "Income_Level"),
  Laborales         = c("Employment_Status", "Work_Hours_Per_Week", "Remote_Work",
                        "Job_Satisfaction", "Work_Stress_Level", "Work_Life_Balance",
                        "Ever_Bullied_At_Work", "Company_Mental_Health_Support"),
  Estilo_de_vida    = c("Exercise_Per_Week", "Sleep_Hours_Night", "Caffeine_Drinks_Day",
                        "Alcohol_Frequency", "Smoking", "Screen_Time_Hours_Day",
                        "Social_Media_Hours_Day", "Hobby_Time_Hours_Week",
                        "Diet_Quality", "Financial_Stress"),
  Sintomas          = c("Feeling_Sad_Down", "Loss_Of_Interest", "Sleep_Trouble",
                        "Fatigue", "Poor_Appetite_Or_Overeating", "Feeling_Worthless",
                        "Concentration_Difficulty", "Anxious_Nervous", "Panic_Attacks",
                        "Mood_Swings", "Irritability", "Obsessive_Thoughts",
                        "Compulsive_Behavior"),
  Riesgo_clinico    = c("Self_Harm_Thoughts", "Suicidal_Thoughts",
                        "Family_History_Mental_Illness", "Trauma_History"),
  Tratamiento       = c("Previously_Diagnosed", "Ever_Sought_Treatment",
                        "On_Therapy_Now", "On_Medication"),
  Entorno_social    = c("Social_Support", "Close_Friends_Count", "Feel_Understood",
                        "Loneliness", "Discuss_Mental_Health")
)

# Número de variables por bloque y comprobación de que están todas las predictoras
lengths(bloques)
```

```
## Sociodemograficos         Laborales    Estilo_de_vida          Sintomas 
##                 6                 8                10                13 
##    Riesgo_clinico       Tratamiento    Entorno_social 
##                 4                 4                 5
```

``` r
setequal(unlist(bloques), setdiff(names(datos_raw), "Has_Mental_Health_Issue"))
```

```
## [1] TRUE
```

### Posible fuga de información

Las variables de **tratamiento** (diagnóstico previo, terapia, medicación…) podrían filtrar la respuesta, ya que son más una consecuencia del problema que una causa. Se comprueba si separan las clases:


``` r
# Proporción de personas que declaran un problema de salud mental
# según el valor de cada variable de tratamiento y de riesgo clínico (0/1)
variables_fuga <- c(bloques$Tratamiento, bloques$Riesgo_clinico)

# % que declara un problema según la variable valga 0 o 1
pct_problema <- sapply(variables_fuga, function(v)
  100 * tapply(datos_raw$Has_Mental_Health_Issue, datos_raw[[v]], mean))
rownames(pct_problema) <- c("Valor 0", "Valor 1")
round(t(pct_problema), 1)
```

```
##                               Valor 0 Valor 1
## Previously_Diagnosed             91.9    92.9
## Ever_Sought_Treatment            92.3    92.0
## On_Therapy_Now                   92.1    92.6
## On_Medication                    91.9    93.4
## Self_Harm_Thoughts               91.9    94.9
## Suicidal_Thoughts                91.8    95.4
## Family_History_Mental_Illness    91.2    94.4
## Trauma_History                   91.6    93.9
```

El porcentaje se mantiene **en torno al 92 %** con 0 y con 1, así que no hay fuga. Se calcula también la correlación de cada predictora numérica con la objetivo:


``` r
# Correlación de cada predictora numérica con la variable objetivo
predictoras_num <- datos_raw %>%
  select(where(is.numeric), -Has_Mental_Health_Issue) %>%
  names()

cor_objetivo <- data.frame(
  Variable    = predictoras_num,
  Correlacion = sapply(predictoras_num,
                       function(v) cor(datos_raw[[v]], datos_raw$Has_Mental_Health_Issue))
)

ggplot(cor_objetivo, aes(x = reorder(Variable, Correlacion), y = Correlacion,
                         fill = Correlacion > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Correlación de las predictoras numéricas con Has_Mental_Health_Issue",
       x = NULL, y = "Correlación de Pearson") +
  theme_minimal()
```

<img src="C:/Users/david/AppData/Local/Temp/claude/2d7e0a91c531013b/6c6f195c-1ad3-40c2-adb3-b598d7ff1594/scratchpad/SeguimientoI_files/figure-html/correlacion-objetivo-1.png" alt="" width="672" />

``` r
# Máxima correlación en valor absoluto
max(abs(cor_objetivo$Correlacion))
```

```
## [1] 0.05502823
```

Todas las correlaciones son **muy débiles** (inferiores a 0,06): ninguna variable explica el problema por sí sola, lo que anticipa una clasificación difícil. Aun así, se compararán los modelos **con y sin el bloque de tratamiento**.

## Preprocesado previsto

Se comprueba qué pasos de preprocesado son necesarios:


``` r
# Valores perdidos y filas duplicadas
sum(is.na(datos_raw))
```

```
## [1] 0
```

``` r
sum(duplicated(datos_raw))
```

```
## [1] 0
```

``` r
# Variables con varianza casi nula
nearZeroVar(datos_raw)
```

```
## integer(0)
```

``` r
# Predictoras numéricas muy correlacionadas entre sí (|r| > 0.9)
predictoras <- datos_raw[, predictoras_num]
findCorrelation(cor(predictoras), cutoff = 0.9)
```

```
## integer(0)
```

``` r
# Número de variables categóricas
sum(sapply(datos_raw, is.character))
```

```
## [1] 13
```

No hay valores perdidos, duplicados, variables con varianza casi nula ni variables redundantes, así que **no hace falta imputar ni eliminar variables**. El preprocesado previsto, aprendido solo con el conjunto de entrenamiento y aplicado igual al de test, es:

- **Variable objetivo** como factor con niveles `"Si"` y `"No"`, necesario para calcular la curva ROC con CARET.
- **One-hot encoding** de las 13 variables categóricas con `dummyVars()`.
- **Estandarización** (`preProcess(method = c("center", "scale"))`), ya que las variables tienen escalas distintas y algunos modelos, como KNN, se basan en distancias.

## Estrategia de evaluación y métricas

> **Pendiente:** división en entrenamiento y test, validación cruzada, tratamiento del desbalanceo si lo hay y métricas de evaluación.

## Modelos a comparar

> **Pendiente:** algoritmos de clasificación que se van a entrenar y ajustar, y cómo se compararán.

## Selección de variables

Como hay 50 predictoras con una relación débil con la objetivo, se aplicarán dos tipos de selección de variables:

-   **Filtro** (sin entrenar modelos): eliminar variables con varianza casi nula (`nearZeroVar`) o muy correlacionadas (`findCorrelation`), ya comprobado en el preprocesado, y ordenar las predictoras según su correlación con la objetivo.
-   **Wrapper** (entrenando modelos): eliminación recursiva de características con `rfe()`, que entrena el modelo repetidas veces y va descartando las variables menos importantes.

Después se compararán los modelos **con todas las variables** y **con las seleccionadas**. Como primera prueba, se aplican ambos métodos a las predictoras numéricas:


``` r
# Filtro: las 10 predictoras más correlacionadas (en valor absoluto) con la objetivo
cor_objetivo[order(-abs(cor_objetivo$Correlacion)), ][1:10, ]
```

```
##                                                    Variable Correlacion
## Work_Stress_Level                         Work_Stress_Level  0.05502823
## Family_History_Mental_Illness Family_History_Mental_Illness  0.05469327
## Feeling_Sad_Down                           Feeling_Sad_Down  0.05007524
## Social_Support                               Social_Support -0.04808252
## Financial_Stress                           Financial_Stress  0.04580659
## Anxious_Nervous                             Anxious_Nervous  0.04362994
## Poor_Appetite_Or_Overeating     Poor_Appetite_Or_Overeating  0.04052549
## Loneliness                                       Loneliness  0.04023699
## Suicidal_Thoughts                         Suicidal_Thoughts  0.03883465
## Trauma_History                               Trauma_History  0.03817635
```

``` r
# Wrapper: eliminación recursiva con regresión logística y validación cruzada de 5 folds
set.seed(2026)
objetivo <- factor(ifelse(datos_raw$Has_Mental_Health_Issue == 1, "Si", "No"))
rfe_ctrl <- rfeControl(functions = lrFuncs, method = "cv", number = 5)
rfe_res  <- rfe(x = predictoras, y = objetivo, sizes = c(5, 10, 20), rfeControl = rfe_ctrl)
rfe_res
```

```
## 
## Recursive feature selection
## 
## Outer resampling method: Cross-Validated (5 fold) 
## 
## Resampling performance over subset size:
## 
##  Variables Accuracy Kappa AccuracySD KappaSD Selected
##          5   0.9216     0  0.0002025       0        *
##         10   0.9216     0  0.0002025       0         
##         20   0.9216     0  0.0002025       0         
##         37   0.9216     0  0.0002025       0         
## 
## The top 5 variables (out of 5):
##    Work_Stress_Level, Family_History_Mental_Illness, Feeling_Sad_Down, Social_Support, Anxious_Nervous
```

``` r
predictors(rfe_res)
```

```
## [1] "Work_Stress_Level"             "Family_History_Mental_Illness"
## [3] "Feeling_Sad_Down"              "Social_Support"               
## [5] "Anxious_Nervous"
```

Ambos métodos coinciden en las variables más relevantes (estrés laboral, antecedentes familiares, tristeza, apoyo social y ansiedad). Sin embargo, `rfe` obtiene la misma *accuracy* (92,2 %, la del ZeroR) con cualquier número de variables: con este desbalanceo, la selección deberá guiarse por la **ROC** y no por la *accuracy*.
