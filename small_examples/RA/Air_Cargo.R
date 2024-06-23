devtools::load_all()

library(tidyverse)
library(kableExtra)
library(ggtext)

#Issue with this script -
#The values fetched for Aircargo are beyond the variable specification (all 3 frequencies)

dict %>%
  mutate(geo="DE") -> dict_de

dict_de %>%
  bind_rows(tibble(
    model_varname = "Aircargo",
    full_name = "Freight and mail air transport - Tonnes",
    database  = "eurostat",
    variable_code = "FRM_BRD",
    dataset_id = "avia_gonc",
    var_col = "tra_meas",
    freq = "q",
    geo = "DE",
    unit = "T")) -> dict_de

dict_de <- dict_de %>%
  bind_rows(tibble(
    model_varname = "Ind_Prod",
    full_name = "An index of Industrial Production",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt",
    freq = "q",
    geo = "DE",
    unit = "I15",
    s_adj = "NSA",
    nace_r2 = "B-D"))

dict_de <- dict_de %>%
  bind_rows(tibble(
    model_varname = "Trade_Vol",
    full_name = "Turnover and volume of sales in wholesale and retail trade",
    database  = "eurostat",
    variable_code = "TOVV",
    dataset_id = "sts_trtu_q",
    var_col = "indic_bt",
    freq = "q",
    geo = "DE",
    unit = "I15",
    s_adj = "NSA",
    nace_r2 = "G"))


specification <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "Aircargo",
    "EmiCO2Industry",
    "EmiCO2Combustion"
  ),
  independent = c(
    "Ind_Prod + Trade_Vol + GCapitalForm",
    "Export + Ind_Prod + Aircargo",
    "HICP_Liquid_Fuels + HICP_Gas + EmiCO2Industry + Ind_Prod"
  )
)


model <- run_model(specification = specification,
                   dictionary = dict_de,
                   inputdata_directory = "data-raw/air/",
                   primary_source = "local",
                   save_to_disk = "data-raw/air/data_ac.xlsx",
                   saturation.tpval = 0.001,
                   max.ar = 2,
                   max.dl = 2,
                   quiet = FALSE,
                   saturation = NULL,
                   constrain.to.minimum.sample = FALSE)

library(modelsummary)

model_list <- lapply(model$module_collection$model, gets::as.lm)
names(model_list) <- model$module_order$dependent

modelsummary::modelsummary(
  model_list,
  coef_omit = "iis|sis",
  gof_omit = "R",
  title = "Final models run for each sub-module for the illustrative example of Austria.",
  notes = "Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
  stars = TRUE
)

model %>%
  network()


f1 <- forecast_model(model,exog_fill_method = "auto")
plot(f1,first_date = "2018-01-01")


