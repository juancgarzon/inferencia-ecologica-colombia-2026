library(dplyr)
library(purrr)
library(tibble)
library(readxl)


# Datos

base_2022 = read_excel("base_modelo_1v2v22_2v26.xlsx")
primera_2026_larga = read_excel("primera_2026_larga.xlsx")

## Segunda Vuelta Presidencial----

# Función para evaluar combinaciones de modelos

probar_modelos <- function(data, y, x_vars, max_vars = length(x_vars)) {
  
  combinaciones <- unlist(
    lapply(
      1:max_vars,
      function(k) combn(x_vars, k, simplify = FALSE)
    ),
    recursive = FALSE
  )
  
  resultados <- map_dfr(combinaciones, function(vars) {
    
    formula_txt <- paste0(
      "`", y, "` ~ ",
      paste0("`", vars, "`", collapse = " + ")
    )
    
    modelo <- lm(as.formula(formula_txt), data = data)
    
    pred <- predict(modelo)
    real <- data[[y]]
    
    tibble(
      variable_dependiente = y,
      variables = paste(vars, collapse = " + "),
      n_variables = length(vars),
      r2 = summary(modelo)$r.squared,
      r2_ajustado = summary(modelo)$adj.r.squared,
      aic = AIC(modelo),
      bic = BIC(modelo),
      rmse = sqrt(mean((real - pred)^2, na.rm = TRUE)),
      mae = mean(abs(real - pred), na.rm = TRUE)
    )
  })
  
  resultados %>%
    arrange(desc(r2_ajustado), rmse, aic)
}


vars_petro <- c(
  "GUSTAVO PETRO.x",
  "SERGIO FAJARDO",
  "JOHN MILTON RODRIGUEZ",
  "LUIS PEREZ",
  "INGRID BETANCOURT",
  "VOTOS EN BLANCO ..x",
  "VOTOS NULOS ..x",
  "VOTOS NO MARCADOS ..x"
)

resultados_petro <- probar_modelos(
  data = base_2022,
  y = "GUSTAVO PETRO.y",
  x_vars = vars_petro,
  max_vars = 5
)

head(resultados_petro, 10)

View(resultados_petro)

vars_rodolfo <- c(
  "RODOLFO HERNANDEZ.x",
  "FEDERICO GUTIERREZ",
  "ENRIQUE GOMEZ MARTINEZ",
  "JOHN MILTON RODRIGUEZ",
  "INGRID BETANCOURT",
  "VOTOS EN BLANCO ..x",
  "VOTOS NULOS ..x",
  "VOTOS NO MARCADOS ..x"
)

resultados_rodolfo <- probar_modelos(
  data = base_2022,
  y = "RODOLFO HERNANDEZ.y",
  x_vars = vars_rodolfo,
  max_vars = 5
)

head(resultados_rodolfo, 10)

resultados_petro %>% slice(1)

resultados_rodolfo %>% slice(1)


mejor_petro <- resultados_petro %>% slice(1)

formula_petro <- as.formula(
  paste0(
    "`GUSTAVO PETRO.y` ~ ",
    paste0("`", unlist(strsplit(mejor_petro$variables, " \\+ ")), "`", collapse = " + ")
  )
)

modelo_petro_final <- lm(formula_petro, data = base_2022)

summary(modelo_petro_final)


resultados_petro %>%
  slice(1) %>%
  pull(variables)

resultados_rodolfo %>%
  slice(1) %>%
  pull(variables)


## Primera vuelta presidencia de 2026----

# Datos

base_modelo = read_excel("base_modelo.xlsx")

cols_2022 <- c(
  "GUSTAVO PETRO",
  "RODOLFO HERNANDEZ",
  "FEDERICO GUTIERREZ",
  "SERGIO FAJARDO",
  "JOHN MILTON RODRIGUEZ",
  "ENRIQUE GOMEZ MARTINEZ",
  "INGRID BETANCOURT",
  "LUIS PEREZ",
  "VOTOS EN BLANCO ._2022",
  "VOTOS NULOS ._2022",
  "VOTOS NO MARCADOS ._2022"
)

cols_2026 <- c(
  "IVÁN CEPEDA CASTRO",
  "ABELARDO DE LA ESPRIELLA",
  "PALOMA VALENCIA LASERNA",
  "SERGIO FAJARDO VALDERRAMA",
  "CLAUDIA LÓPEZ",
  "RAÚL SANTIAGO BOTERO JARAMILLO",
  "ÓSCAR MAURICIO LIZCANO ARANGO",
  "MIGUEL URIBE LONDOÑO",
  "VOTOS EN BLANCO ._2026",
  "VOTOS NULOS ._2026",
  "VOTOS NO MARCADOS ._2026"
)


resultados_cepeda <- probar_modelos(
  data = base_modelo,
  y = "IVÁN CEPEDA CASTRO",
  x_vars = cols_2022,
  max_vars = 5
)


resultados_abelardo <- probar_modelos(
  data = base_modelo,
  y = "ABELARDO DE LA ESPRIELLA",
  x_vars = cols_2022,
  max_vars = 5
)

resultados_paloma <- probar_modelos(
  data = base_modelo,
  y = "PALOMA VALENCIA LASERNA",
  x_vars = cols_2022,
  max_vars = 5
)


resultados_fajardo <- probar_modelos(
  data = base_modelo,
  y = "SERGIO FAJARDO VALDERRAMA",
  x_vars = cols_2022,
  max_vars = 5
)


resultados_cepeda %>%
  slice(1) %>%
  pull(variables)

resultados_abelardo %>%
  slice(1) %>%
  pull(variables)

resultados_paloma %>%
  slice(1) %>%
  pull(variables)

resultados_fajardo %>%
  slice(1) %>%
  pull(variables)

cat(
  "Cepeda:   ", resultados_cepeda$variables[1], "\n",
  "Abelardo: ", resultados_abelardo$variables[1], "\n",
  "Paloma:   ", resultados_paloma$variables[1], "\n",
  "Fajardo:  ", resultados_fajardo$variables[1], "\n"
)
