# Ejercicio de Transferencia de Votos entre las 1er y 2da vuelta presidencial
# de 2022 y la primera vuelta presidencial de 2026.


library(readxl)
library(dplyr)

## Bases de datos----

primera_2026_larga = read_excel("primera_2026_larga.xlsx")

primera22_mun = read_excel("primera2022_mun.xlsx")
primera22_wide =  read_excel("primera22_wide.xlsx")

segunda22_mun = read_excel("segunda2022_mun.xlsx")
segunda22_wide =  read_excel("segunda22_wide.xlsx")
base_2022 =  read_excel("base_modelo_1v2v22_2v26.xlsx")

## 1. Modelos históricos: primera vuelta -> segunda vuelta 2022----

modelo_petro2v <- lm(
  `GUSTAVO PETRO.y` ~
    `GUSTAVO PETRO.x` +
    `SERGIO FAJARDO` +
    `LUIS PEREZ` +
    `VOTOS EN BLANCO ..x` +
    `VOTOS NULOS ..x`,
  data = base_2022
)

summary(modelo_petro2v)

summary(modelo_petro2v)

modelo_rodolfo2v <- lm(
  `RODOLFO HERNANDEZ.y` ~
    `RODOLFO HERNANDEZ.x` +
    `FEDERICO GUTIERREZ` +
    `ENRIQUE GOMEZ MARTINEZ` +
    `JOHN MILTON RODRIGUEZ` +
    `VOTOS NULOS ..x`,
  data = base_2022
)

summary(modelo_rodolfo2v)

modelo_blanco2v <- lm(
  `VOTOS EN BLANCO ..y` ~#Primera vuelta 2022
    `VOTOS EN BLANCO ..x`,#Segunda vuelta 2022
  data = base_2022
)

modelo_nulo2v <- lm(
  `VOTOS NULOS ..y` ~#Primera vuelta 2022
    `VOTOS NULOS ..x`,#Segunda vuelta 2022
  data = base_2022
)

summary(modelo_nulo2v)

modelo_nomarcado2v <- lm(
  `VOTOS NO MARCADOS ..y` ~#Primera vuelta 2022
    `VOTOS NO MARCADOS ..x`,#Segunda vuelta 2022
  data = base_2022
)

summary(modelo_nomarcado2v)


## 2. Proyección 2026 usando estructura de primera vuelta 2026----


base_2026 <- primera_2026_larga %>%
  group_by(Departamento, Municipio, Candidato) %>%
  summarise(
    votos = sum(Votos, na.rm = TRUE),
    voters = max(voters, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Departamento, Municipio) %>%
  mutate(
    pct = votos / sum(votos, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(Departamento, Municipio, Candidato, pct, voters) %>%
  pivot_wider(
    names_from = Candidato,
    values_from = pct,
    values_fill = 0
  )

## 3. Simulación segunda vuelta 2026----


base_pred_2026 <- base_2026 %>%
  mutate(
    
    # CEPEDA
    
    pred_cepeda =
      coef(modelo_petro2v)["(Intercept)"] +
      coef(modelo_petro2v)["`GUSTAVO PETRO.x`"] * `IVÁN CEPEDA CASTRO` +
      coef(modelo_petro2v)["`SERGIO FAJARDO`"] * `SERGIO FAJARDO VALDERRAMA` +
      coef(modelo_petro2v)["`LUIS PEREZ`"] * 0 +
      coef(modelo_petro2v)["`VOTOS EN BLANCO ..x`"] * `VOTOS EN BLANCO .` +
      coef(modelo_petro2v)["`VOTOS NULOS ..x`"] * `VOTOS NULOS .`,
    
    # ABELARDO

    pred_abelardo =
      coef(modelo_rodolfo2v)["(Intercept)"] +
      coef(modelo_rodolfo2v)["`RODOLFO HERNANDEZ.x`"] * `ABELARDO DE LA ESPRIELLA` +
      coef(modelo_rodolfo2v)["`FEDERICO GUTIERREZ`"] * `PALOMA VALENCIA LASERNA` +
      coef(modelo_rodolfo2v)["`ENRIQUE GOMEZ MARTINEZ`"] * 0 +
      coef(modelo_rodolfo2v)["`JOHN MILTON RODRIGUEZ`"] * 0 +
      coef(modelo_rodolfo2v)["`VOTOS NULOS ..x`"] * `VOTOS NULOS .`,
    
    # VOTO EN BLANCO

    pred_blanco =
      coef(modelo_blanco2v)["(Intercept)"] +
      coef(modelo_blanco2v)["`VOTOS EN BLANCO ..x`"] * `VOTOS EN BLANCO .`,
    
    # VOTOS NULOS
    
    pred_nulo =
      coef(modelo_nulo2v)["(Intercept)"] +
      coef(modelo_nulo2v)["`VOTOS NULOS ..x`"] * `VOTOS NULOS .`,
    
    
    #VOTOS NO MARCADOS
    
    pred_nomarcado =
      coef(modelo_nomarcado2v)["(Intercept)"] +
      coef(modelo_nomarcado2v)["`VOTOS NO MARCADOS ..x`"] * `VOTOS NO MARCADOS .`,
    
    # LIMPIEZA
    
    pred_cepeda    = pmax(pred_cepeda, 0),
    pred_abelardo  = pmax(pred_abelardo, 0),
    pred_blanco    = pmax(pred_blanco, 0),
    pred_nulo      = pmax(pred_nulo, 0),
    pred_nomarcado = pmax(pred_nomarcado, 0),
    
    
    
    # NORMALIZACIÓN

    total_pred =
      pred_cepeda +
      pred_abelardo +
      pred_blanco +
      pred_nulo +
      pred_nomarcado,
    
    cepeda_2v =
      pred_cepeda / total_pred,
    
    abelardo_2v =
      pred_abelardo / total_pred,
    
    blanco_2v =
      pred_blanco / total_pred,
    
    nulo_2v =
      pred_nulo / total_pred,
    
    nomarcado =
      pred_nomarcado / total_pred
    
  )

resultado_nacional_2026 <- base_pred_2026 %>%
  summarise(
    votos_cepeda     = sum(cepeda_2v * voters, na.rm = TRUE),
    votos_abelardo   = sum(abelardo_2v * voters, na.rm = TRUE),
    votos_blanco     = sum(blanco_2v * voters, na.rm = TRUE),
    votos_nulos      = sum(nulo_2v * voters, na.rm = TRUE),
    votos_nomarcados = sum(nomarcado * voters, na.rm = TRUE)
  ) %>%
  mutate(
    total_votos =
      votos_cepeda +
      votos_abelardo +
      votos_blanco +
      votos_nulos +
      votos_nomarcados,
    
    pct_cepeda =
      100 * votos_cepeda / total_votos,
    
    pct_abelardo =
      100 * votos_abelardo / total_votos,
    
    pct_blanco =
      100 * votos_blanco / total_votos,
    
    pct_nulo =
      100 * votos_nulos / total_votos,
    
    pct_nomarcado =
      100 * votos_nomarcados / total_votos
  )

resultado_nacional_2026


resultado_nacional_2026 %>%
  select(
    votos_cepeda,
    votos_abelardo,
    votos_blanco,
    votos_nulos,
    votos_nomarcados
  ) %>%
  pivot_longer(
    everything(),
    names_to = "Candidato",
    values_to = "Votos"
  )
