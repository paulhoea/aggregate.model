library(conflicted)
library(tidyverse)

# FELIX: exchange your token here
# devtools::install_github("moritzpschwarz/aggregate.model", ref="devel_w_forecasting", auth_token = "YOUR_TOKEN")
#library(aggregate.model)

conflict_prefer_all("dplyr", quiet = TRUE)


cur_dict <- aggregate.model::dict %>%
  bind_rows(tibble(model_varname = "EmiGHGTotal",
                   full_name = "Total GHG Emissions from Edgar (not quite but combination from Industry, Combustion and a bit of non-CO2")) %>%
  #mutate(geo = "DE") %>%
  return()



# you can see, the first two
specification <- dplyr::tibble(
  type = c(
    "d",
    "n",
    "n",
    "n",
    "d",
    "n"
  ),
  dependent = c(
    "TOTS",
    "Import",
    "EmiCO2Combustion",
    "EmiCO2Industry",
    "EmiGHGTotal",
    "GValueAddIndus"
  ),
  independent = c(
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "HDD + HICP_Gas + HICP_Electricity + GValueAdd",
    "HICP_Gas + HICP_Electricity + GValueAddIndus",
    "EmiCO2Combustion + EmiCO2Industry + EmiCH4Livestock + EmiN2OTotal",
    "HICP_Gas + HICP_Electricity + Export + TOTS"
  )
)
print(specification)


model <- run_model(specification = specification,
                   dictionary = cur_dict,
                   inputdata_directory = "small_examples/EMCC Aug 23",
                   primary_source = "local",
                   save_to_disk = "small_examples/EMCC Aug 23/emcc_data.csv",
                   present = FALSE,
                   quiet = FALSE)

bb <- forecast_model(model)

plot(bb, interactive = TRUE, exclude.exogenous = FALSE, first_value = "2015-01-01")
plot(bb, first_value = "2015-01-01")

exog <- bb$exog_data_nowcast

sensitivity <- forecast_sensitivity(model)
sensitivity

exog %>%
  select(-starts_with("q_")) %>%
  pivot_longer(-time) %>%
  ggplot(aes(x = time, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name, scales ="free")+
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

# modifiying electricity price to be fixed at 300
bb_modified <- forecast_model(model, exog_predictions = bb$exog_data_nowcast %>%
                                mutate(HICP_Electricity = ifelse(time > as.Date("2023-07-01"), 300, HICP_Electricity)))

# now compare
plot(bb, first_value = "2015-01-01", exclude.exogenous = FALSE)
plot(bb_modified, first_value = "2015-01-01", exclude.exogenous = FALSE)

plot(bb, first_value = "2015-01-01")
plot(bb_modified, first_value = "2015-01-01")


process_forecasts <- function(x){
  x$forecast %>%
    dplyr::select("central.estimate") %>%
    tidyr::unnest("central.estimate") %>%
    tidyr::pivot_longer(-"time") %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = "time", names_from = "name", values_from = "value") -> central_forecasts

  central_forecasts %>%
    names %>%
    stringr::str_detect(., "^ln.") -> to_exponentiate

  central_forecasts %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(names(central_forecasts)[to_exponentiate]), exp)) %>%
    dplyr::rename_with(.fn = ~gsub("ln.","",.)) %>%

    tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
    dplyr::full_join(x$orig_model$module_order %>%
                       dplyr::select("dependent") %>%
                       dplyr::rename(na_item = "dependent"), by = "na_item") %>%

    dplyr::mutate(fit = "forecast") -> forecasts_processed

  return(forecasts_processed)
}

process_forecasts(bb) %>%
  mutate(type = "AR") %>%
  bind_rows(process_forecasts(bb_modified) %>%
              mutate(type = "Scenario Low Electricity Price")) %>%
  filter(na_item == "EmiCO2Combustion") %>%
  summarise(sums = sum(values), .by = type) -> results

results$sums[2] - results$sums[1]

# Result shows that Low Electricity Scenario leads to 24,4 Mio. Tonnes of CO2 over
# the 10 quarter cycle modelled






