library(conflicted)
devtools::load_all()
# library(aggregate.model)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)

# Julia small model environment ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Here I am adding the needed variables for the environmental model to the dictionary
dict %>%
  bind_rows(tibble(
    model_varname = "N2O_soils",
    full_name = "Direct N2O Emissions from managed soils",
    database  = "edgar",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_N2O_m_1970_2022b.zip",
    freq = "q",
    ipcc_sector = "3.C.4",
    geo = "AT" )) %>%
  bind_rows(tibble(
    model_varname = "Price_agrpro",
    full_name = "Price indices of agricultural products, output",
    database  = "eurostat",
    variable_code = "140000",
    dataset_id = "apri_pi20_outq",
    var_col = "product",
    freq = "q",
    geo = "AT",
    unit = "I20",
    s_adj = "NSA",
    nace_r2 = "B-D")) %>%
  bind_rows(tibble(
    model_varname = "Price_animpro",
    full_name = "Price indices of agricultural products - Agricultural goods output",
    database  = "eurostat",
    variable_code = "140000",
    dataset_id = "apri_pi15_outq",
    var_col = "product",
    freq = "q",
    geo = "AT",
    unit = "I15",
    p_adj = "NI")) %>%
  bind_rows(tibble(
    model_varname = "ind_N2O_mm",
    full_name = "Indirect N2O Emissions from manure management",
    database  = "edgar",
    ipcc_sector = "3.C.6",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_N2O_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%
  bind_rows(tibble(
    model_varname = "Flights",
    full_name = "National air passenger transport by reporting country",
    database  = "eurostat",
    variable_code = "CAF_PAS",
    dataset_id = "avia_panc",
    var_col = "tra_meas",
    freq = "m",
    geo = "AT",
    unit = "FLIGHT")) %>%
  bind_rows(tibble(
    model_varname = "Prod_Constr_Manu",
    full_name = "Production in Construction",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_copr_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "B-D_F")) %>%
  bind_rows(tibble(
    model_varname = "CO2_aviation",
    full_name = "CO2 (fossil) emissions from Civil Aviation",
    database  = "edgar",
    ipcc_sector = "1.A.3.a",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%
  bind_rows(tibble(
    model_varname = "CO2_roads",
    full_name = "CO2 (bio) emissions from Road",
    database  = "edgar",
    ipcc_sector = "1.A.3.b_noRES",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%
  bind_rows(tibble(
    model_varname = "CO2_Manufactoring",
    full_name = "CO2 (fossil) emissions from Manufacturing Industries and Construction",
    database  = "edgar",
    ipcc_sector = "1.A.2",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%
  bind_rows(tibble(
    model_varname = "CO2_Residential",
    full_name = "CO2 (fossil) emissions from Residential and other sectors",
    database  = "edgar",
    ipcc_sector = "1.A.4",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%
  bind_rows(tibble(
    model_varname = "Prod_Constr_Build",
    full_name = "Production in Construction",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_copr_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "F41")) %>%
  bind_rows(tibble(
    model_varname = "Lab_In_Constr",
    full_name = "Labour input in construction",
    database  = "eurostat",
    variable_code = "EMPL",
    dataset_id = "sts_colb_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "F")) %>%
  bind_rows(tibble(
    model_varname = "Lab_In_Indus",
    full_name = "Labour input in industry",
    database  = "eurostat",
    variable_code = "EMPL",
    dataset_id = "sts_inlb_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "B-E36")) %>%
  bind_rows(tibble(
    model_varname = "Elect_cons",
    full_name = "Consumption of electricity - GWh",
    database  = "eurostat",
    variable_code = "IS-CEL-GWH",
    dataset_id = "ei_isen_m",
    var_col = "indic",
    freq = "m",
    geo = "AT",
    unit = "NA",
    s_adj = "NSA",
    nace_r2 = "B-D")) %>%
  bind_rows(tibble(
    model_varname = "Animal_imp",
    full_name = "Total imports of live animals",
    database  = "eurostat",
    variable_code = "SL",
    dataset_id = "apro_mt_pheadm",
    var_col = "meatitem",
    freq = "m",
    geo = "AT",
    unit = "THS_T",
    s_adj = "NSA",
    nace_r2 = "B-D",
    meat = "B1000")) %>%
  bind_rows(tibble(
    model_varname = "GDP_new",
    full_name = "GDP and main aggregates - international data cooperation quarterly data",
    database  = "eurostat",
    variable_code = "B1GQ",
    dataset_id = "naidq_10_gdp",
    var_col = "na_item",
    freq = "q",
    geo = "AT",
    unit = "CP_MNAC",
    s_adj = "NSA",
    nace_r2 = "B-D")) -> environment_dict

# eurostat::get_eurostat("apri_pi15_outq", filters = list(geo = "AT")) -> test
#
# test %>%
#   filter(p_adj == "NI") %>%
# ggplot(aes(x = time, y = values, color = product)) + geom_line() + facet_wrap(~product, scale = "free_y")


# any estimation needs "n"
# any identity needs "d"
# this is an example environmental model with 4 equations
specification <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "N2O_soils",
    "CO2_aviation",
    "CO2_Manufactoring",
    "CO2_Residential"
  ),
  independent = c(
    "ind_N2O_mm + Price_animpro + Animal_imp",
    "GDP_new + Flights",
    "Prod_Constr_Manu + Prod_Constr_Build + Lab_In_Constr +  Lab_In_Indus",
    "Elect_cons + HDD + BuildingPermits"
  )
)
specification



# Estimation --------------------------------------------------------------
model <- run_model(specification = specification,
                   dictionary = environment_dict,
                   inputdata_directory = "small_examples/environmental_model/",
                   primary_source = "local",

                   # modify save_to_disk for each new country/specification
                   # this avoids having to download the same data multiple times
                   save_to_disk = "small_examples/environmental_model/Environmental_model_V2.R",

                   quiet = FALSE,
                   saturation.tpval = 0.001,
                   constrain.to.minimum.sample = FALSE)



forecast_model(model)
