#Error in value[[jvseq[[jjj]]]] : subscript out of bounds -> Model Forecasting

dict_de <- dict %>%
  mutate(geo = "DE")

dict_de <- dict_de %>%
  bind_rows(tibble(
  model_varname = "Metal_Prod",
  full_name = "Index of Manufacture of basic metals and fabricated metal products, except machinery and equipment",
  database  = "eurostat",
  variable_code = "PROD",
  dataset_id = "sts_inpr_q",
  var_col = "indic_bt",
  freq = "q",
  geo = "DE",
  unit = "I15",
  s_adj = "NSA",
  nace_r2 = "C24_C25"))

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

dict_de %>%
  bind_rows(tibble(
    model_varname = "PlaProd",
    full_name = "An index of Plastic Production",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt",
    freq = "q",
    geo = "DE",
    unit = "I15",
    s_adj = "NSA",
    nace_r2 = "C222")) -> dict_de

specification <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "Ind_Prod",
    "EmiCO2Industry",
    "EmiCO2Combustion"
  ),
  independent = c(
    "Metal_Prod + PlaProd + Import",
    "HICP_Energy + Metal_Prod + Export",
    "FinConsExpHH + HICP_Gas + EmiCO2Industry + Ind_Prod"
    )
)
specification

model <- run_model(specification = specification,
                   dictionary = dict_de,
                   inputdata_directory = "data-raw/metal/",
                   primary_source = "local",
                   save_to_disk = "data-raw/metal/data_metal.xlsx",
                   saturation.tpval = 0.001,
                   max.ar = 2,
                   max.dl = 2,
                   quiet = FALSE,
                   saturation = NULL,
                   constrain.to.minimum.sample = FALSE)


library(modelsummary)

class(model)
model_list <- lapply(model$module_collection$model, gets::as.lm)
names(model_list) <- model$module_order$dependent

modelsummary::modelsummary(
  model_list,
  coef_omit = "iis|sis",
  gof_omit = "R",
  notes = "Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
  stars = TRUE
)


diagnostics_model(model) %>%
  rename(Module = module,
         `Indicator Share` = indicator_share) %>%
  kable(booktabs = TRUE, digits = 3, label = "diagnostics", caption = "Diagnostic results for each sub-module.") %>%
  kable_styling()


model %>%
  network()

f2 <- forecast_model(model, exog_fill_method = "auto", plot.forecast = FALSE)

plot(f2)


