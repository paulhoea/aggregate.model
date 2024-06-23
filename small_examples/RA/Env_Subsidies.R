
#New Varibales needed. The sample is too small

dict %>%
  mutate(geo="NL") -> dict_nl

dict_nl %>%
  bind_rows(tibble(
    model_varname = "ENV_EXP_CORP",
    full_name = "Expenditure on environmental protection by Corporation - MIO_EUR",
    database  = "eurostat",
    variable_code = "S11_S12",
    dataset_id = "env_ac_epneis",
    var_col = "sector",
    freq = "a",
    geo = "NL",
    unit = "MIO_EUR")) -> dict_nl

dict_nl %>%
  bind_rows(tibble(
    model_varname = "ENV_TRF_GOV_CORP",
    full_name = "Environmental subsidies and similar transfers from general government to corporations - MIO_EUR",
    database  = "eurostat",
    variable_code = "TRF_CUR_D9",
    dataset_id = "env_esst_ggcp",
    var_col = "na_item",
    nace_r2 = "TOTAL",
    ceparema = "TOTAL",
    freq = "a",
    geo = "NL",
    unit = "MIO_EUR")) -> dict_nl

dict_nl <- dict_nl %>%
  bind_rows(tibble(
    model_varname = "Ind_Prod",
    full_name = "An index of Industrial Production",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_inpr_a",
    var_col = "indic_bt",
    freq = "a",
    geo = "NL",
    unit = "I15",
    s_adj = "NSA",
    nace_r2 = "B-D"))

specification <- dplyr::tibble(
  type = c(
    "n"
  ),
  dependent = c(
    "EmiCO2Industry"
  ),
  independent = c(
    "Export + ENV_EXP_CORP + ENV_TRF_GOV_CORP + Ind_Prod"
  )
)

model <- run_model(specification = specification,
                   dictionary = dict_nl,
                   inputdata_directory = "data-raw/your_folder/",
                   primary_source = "local",
                   save_to_disk = "data-raw/your_folder/data_nl.xlsx",
                   saturation.tpval = 0.001,
                   max.ar = 1,
                   max.dl = 1,
                   quiet = FALSE,
                   saturation = NULL,
                   constrain.to.minimum.sample = FALSE)
