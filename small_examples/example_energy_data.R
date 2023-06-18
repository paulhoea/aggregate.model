library(aggregate.model)
library(tidyverse)

specification <- dplyr::tibble(
  type = c(
    "d",
    "d",
    "n",
    "n"
  ),
  dependent = c(
    "StatDiscrep",
    "TOTS",
    "Import",
    "EmiCO2Combustion"
  ),
  independent = c(
    "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "HDD + CDD + HICP_Energy + GValueAdd"
  )
)

# fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
# fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
# filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa,
#                     "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

dictionary <- aggregate.model::dict

# for data download
specification <- aggregate.model:::check_config_table(specification)





test <- get_eurostat(id = "nrg_d_hhq")
test %>%
  filter(geo == "AT") %>%
  filter(nrg_bal == "FC_OTH_HH_E") %>%
  group_by(unit) %>%
  summarise(obs_per_unit = n()) # 156 measured in TJ
test %>%
  filter(geo == "AT") %>%
  filter(nrg_bal == "FC_OTH_HH_E") %>%
  distinct(nrg_bal, siec, geo, time) # and we do have 156 unique entries
# so TJ is the converted energy amount to compare across fuel types
sub <- test %>%
  filter(geo == "AT") %>%
  filter(nrg_bal == "FC_OTH_HH_E") %>%
  filter(siec == "TOTAL")

# are there more columns in dict than expected?
d_test <- dictionary %>%
  mutate(additional = "a")

NCOL(d_test) # here 8; let's say the first 7 are expected, all others are additional

for (k in 8:NCOL(d_test)) {
  filtername <- colnames(d_test)[k]
  test_filtered <- test %>%
  {if(dplyr::select(., dplyr::any_of(filtername)) %>% ncol == 1){dplyr::filter(.,.data[[filtername]] == d_test[j, filtername])} else {.}}
} # this works as intended

a <- c("P7", "P3", "P31_S14_S15", "P5G", "B1G", "P3_S13", "P6", "B1GQ")
b <- c("YA0")
c <- c("GHG")
d <- ""
e <- "PSQM"
f <- "LM-LCI-TOT"

dictionary <- dictionary %>%
  mutate(s_adj = case_when(eurostat_code %in% c(a, b, c, e, f) ~ "SCA",
                           TRUE ~ NA)) %>%
  mutate(unit = case_when(eurostat_code %in% a ~ "CLV05_MEUR",
                          eurostat_code %in% b ~ "CP_MEUR",
                          eurostat_code %in% c ~ "THS_T",
                          eurostat_code %in% e ~ "I15",
                          eurostat_code %in% f ~ "I16",
                          TRUE ~ NA)) %>%
  mutate(geo = case_when(eurostat_code %in% c(a, b, c, e, f) ~ "AT",
                           TRUE ~ NA)) %>%
  mutate(siec = NA) %>%
  select(model_varname, full_name, eurostat_code, dataset_id, var_col, geo, unit, s_adj, nace_r2, cpa2_1, siec)

# now add some examples from energy data
newdata <- tibble(model_varname = c("EnergyHHTotal", "EnergyHHGas", "EnergyManufBevElec", "EnergyManufBevGas"),
                  full_name = c("Energy consumption households, all energy types",
                                "Energy consumption households, natural gas",
                                "Energy consumption manufacturing of beverages, electricity",
                                "Energy consumption manufacturing of beverages, natural gas"),
                  eurostat_code = c("FC_OTH_HH_E", "FC_OTH_HH_E", NA, NA),
                  dataset_id = c("nrg_d_hhq", "nrg_d_hhq", "nrg_d_indq_n", "nrg_d_indq_n"),
                  var_col = c("nrg_bal", "nrg_bal", "nace_r2", "nace_r2"),
                  geo = c("AT", "AT", "AT", "AT"),
                  unit = c("TJ", "TJ", "TJ_NCV", "TJ_NCV"),
                  s_adj = c(NA, NA, NA, NA),
                  nace_r2 = c(NA, NA, "C11", "C11"),
                  cpa2_1 = c(NA, NA, NA, NA),
                  siec = c("TOTAL", "G3000", "E7000", "G3000"))

newdict <- bind_rows(dictionary, newdata)



# downloading EDGAR data:
tmp_download <- tempfile(fileext = "zip")
download.file(url = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_CH4_m_2000_2021.zip",
              destfile = tmp_download,
              mode = "wb")
tmp_extract <- tempdir()
unzip(zipfile = tmp_download, files = "CH4_2000_2021.xlsx", exdir = tmp_extract)

data <- readxl::read_excel(path = file.path(tmp_extract, "CH4_2000_2021.xlsx"),
                           sheet = "CH4_IPCC2006",
                           skip = 9)
unlink(tmp_download)
unlink(file.path(tmp_extract, "CH4_2000_2021.xlsx"))

library(countrycode)
countrycode(sourcevar = data$Country_code_A3, origin = "iso3c", destination = "eurostat")

data <- data %>%
  select(Country_code_A3, ipcc_code_2006_for_standard_report, Substance, 9:21) %>%
  rename(geo = Country_code_A3) %>%
  filter(geo == "AUT") %>%
  pivot_longer(cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
               names_to = "Month",
               values_to = "Emissions") %>%
  mutate(Month = case_when(Month == "Jan" ~ "01",
                           Month == "Feb" ~ "02",
                           Month == "Mar" ~ "03",
                           Month == "Apr" ~ "04",
                           Month == "May" ~ "05",
                           Month == "Jun" ~ "06",
                           Month == "Jul" ~ "07",
                           Month == "Aug" ~ "08",
                           Month == "Sep" ~ "09",
                           Month == "Oct" ~ "10",
                           Month == "Nov" ~ "11",
                           Month == "Dec" ~ "12",
                           TRUE ~ "error"))
stopifnot(!("error" %in% unique(data$Month)))

data %>%
  mutate(time_chr = paste0(Year, "-", Month, "-01")) %>%
  mutate(time = as.Date(time_chr))

# testing during development
library(aggregate.model)
library(tidyverse)

specification <- dplyr::tibble(
  type = c(
    "d",
    "d",
    "n",
    "n"
  ),
  dependent = c(
    "StatDiscrep",
    "TOTS",
    "Import",
    "EmiCO2Combustion"
  ),
  independent = c(
    "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "HDD + CDD + HICP_Energy + GValueAdd"
  )
)

# fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
# fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
# filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa,
#                     "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

dictionary <- aggregate.model::dict

# for data download
specification <- aggregate.model:::check_config_table(specification)
primary_source <- "download"
to_obtain <- aggregate.model:::determine_variables(specification = specification,
                                 dictionary = dictionary)







