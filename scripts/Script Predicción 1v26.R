library(readxl)
library(dplyr)
library(tidyr)

base_2022 = read_excel("base_modelo_1v2v22_2v26.xlsx")
primera22_mun = read_excel("primera2022_mun.xlsx")
segunda22_mun = read_excel("segunda2022_mun.xlsx")
primera_2026_larga = read_excel("primera_2026_larga.xlsx")

## 1. Modelos para simular primera vuelta 2026 usando 2022----

modelo_cepeda_1v <- lm(
  `IVÁN CEPEDA CASTRO` ~
    `GUSTAVO PETRO` +
    `SERGIO FAJARDO` +
    `INGRID BETANCOURT` +
    `VOTOS NULOS ._2022` +
    `VOTOS NO MARCADOS ._2022`,
  data = base_modelo
)

modelo_abelardo_1v <- lm(
  `ABELARDO DE LA ESPRIELLA` ~
    `RODOLFO HERNANDEZ` +
    `FEDERICO GUTIERREZ` +
    `JOHN MILTON RODRIGUEZ` +
    `VOTOS NULOS ._2022` +
    `VOTOS NO MARCADOS ._2022`,
  data = base_modelo
)

modelo_paloma_1v <- lm(
  `PALOMA VALENCIA LASERNA` ~
    `GUSTAVO PETRO` +
    `RODOLFO HERNANDEZ` +
    `FEDERICO GUTIERREZ` +
    `JOHN MILTON RODRIGUEZ` +
    `VOTOS NULOS ._2022`,
  data = base_modelo
)

modelo_fajardo_1v <- lm(
  `SERGIO FAJARDO VALDERRAMA` ~
    `RODOLFO HERNANDEZ` +
    `SERGIO FAJARDO` +
    `ENRIQUE GOMEZ MARTINEZ` +
    `VOTOS EN BLANCO ._2022` +
    `VOTOS NO MARCADOS ._2022`,
  data = base_modelo
)

modelo_blanco_1v <- lm(
  `VOTOS EN BLANCO ._2026` ~
    `VOTOS EN BLANCO ._2022`,
  data = base_modelo
)


# 2. Pesos municipales 2022

pesos_2022 <- primera22_mun %>%
  group_by(Departamento, Municipio) %>%
  summarise(
    voters_2022 = sum(votos, na.rm = TRUE),
    .groups = "drop"
  )

base_modelo_freq <- base_modelo %>%
  left_join(
    pesos_2022,
    by = c("Departamento", "Municipio")
  )

# 3. Predicción municipal de primera vuelta 2026 usando solo variables explicativas de 2022 ----

base_pred_1v <- base_modelo_freq %>%
  mutate(
    pred_cepeda =
      predict(modelo_cepeda_1v, newdata = base_modelo_freq),
    
    pred_abelardo =
      predict(modelo_abelardo_1v, newdata = base_modelo_freq),
    
    pred_paloma =
      predict(modelo_paloma_1v, newdata = base_modelo_freq),
    
    pred_fajardo =
      predict(modelo_fajardo_1v, newdata = base_modelo_freq),
    
    pred_blanco =
      predict(modelo_blanco_1v, newdata = base_modelo_freq),
    
    pred_cepeda = pmax(pred_cepeda, 0),
    pred_abelardo = pmax(pred_abelardo, 0),
    pred_paloma = pmax(pred_paloma, 0),
    pred_fajardo = pmax(pred_fajardo, 0),
    pred_blanco = pmax(pred_blanco, 0),
    
    total_pred =
      pred_cepeda +
      pred_abelardo +
      pred_paloma +
      pred_fajardo +
      pred_blanco,
    
    cepeda_1v = pred_cepeda / total_pred,
    abelardo_1v = pred_abelardo / total_pred,
    paloma_1v = pred_paloma / total_pred,
    fajardo_1v = pred_fajardo / total_pred,
    blanco_1v = pred_blanco / total_pred
  )

# 4. Resultado nacional simulado----

resultado_1v_modelo <- base_pred_1v %>%
  summarise(
    votos_cepeda =
      sum(cepeda_1v * voters_2022, na.rm = TRUE),
    
    votos_abelardo =
      sum(abelardo_1v * voters_2022, na.rm = TRUE),
    
    votos_paloma =
      sum(paloma_1v * voters_2022, na.rm = TRUE),
    
    votos_fajardo =
      sum(fajardo_1v * voters_2022, na.rm = TRUE),
    
    votos_blanco =
      sum(blanco_1v * voters_2022, na.rm = TRUE)
  ) %>%
  mutate(
    total =
      votos_cepeda +
      votos_abelardo +
      votos_paloma +
      votos_fajardo +
      votos_blanco,
    
    pct_cepeda =
      100 * votos_cepeda / total,
    
    pct_abelardo =
      100 * votos_abelardo / total,
    
    pct_paloma =
      100 * votos_paloma / total,
    
    pct_fajardo =
      100 * votos_fajardo / total,
    
    pct_blanco =
      100 * votos_blanco / total
  )

resultado_1v_modelo

# 5. Tabla final----

tabla_1v_modelo <- resultado_1v_modelo %>%
  transmute(
    Cepeda = votos_cepeda,
    Abelardo = votos_abelardo,
    Paloma = votos_paloma,
    Fajardo = votos_fajardo,
    Blanco = votos_blanco
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Candidato",
    values_to = "Votos"
  ) %>%
  mutate(
    Porcentaje = 100 * Votos / sum(Votos),
    Votos = round(Votos),
    Porcentaje = round(Porcentaje, 2)
  ) %>%
  arrange(desc(Votos))

tabla_1v_modelo
