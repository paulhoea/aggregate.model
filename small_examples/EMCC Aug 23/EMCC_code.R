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


# Model Run for AT --------------------------------------------------------


model <- run_model(specification = specification,
                   dictionary = cur_dict,
                   inputdata_directory = "small_examples/EMCC Aug 23",
                   primary_source = "local",
                   save_to_disk = "small_examples/EMCC Aug 23/emcc_data_AT.csv",
                   present = FALSE,
                   quiet = FALSE)

bb <- forecast_model(model)

plot(bb, interactive = TRUE, exclude.exogenous = FALSE, first_value = "2015-01-01")
plot(bb, first_value = "2015-01-01")
plot(bb, first_value = "2015-01-01", exclude.exogenous = FALSE)


# Insample Forecast -------------------------------------------------------


# This takes a long time (about 20 min, depending on the selected share)
bb_insample <- forecast_insample(model, sample_share = 0.75)

library(lubridate)

bb_insample$central %>%
  rowwise() %>%
  mutate(forecast_len = add.months(start, 24)) %>%
  ungroup() %>%
  filter(!(time > forecast_len)) -> central


bb_insample$uncertainty %>%
  rowwise() %>%
  mutate(forecast_len = add.months(start, 24)) %>%
  ungroup() %>%
  filter(!(time > forecast_len)) -> uncertainty



central %>%
  dplyr::filter(.data$start > min(bb_insample$central$start)) %>%
  ggplot2::ggplot() +

  ggplot2::facet_wrap(~dep_var, scales = "free") +
  ggplot2::geom_ribbon(data = uncertainty, ggplot2::aes(ymin = min, x = time, ymax = max, fill = as.factor(start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
  ggplot2::geom_ribbon(data = uncertainty, ggplot2::aes(ymin = p025, x = time, ymax = p975, fill = as.factor(start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
  ggplot2::geom_ribbon(data = uncertainty, ggplot2::aes(ymin = p25, x = time, ymax = p75, fill = as.factor(start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +

  ggplot2::geom_line(ggplot2::aes(y = value, x = time, color = as.factor(start)), inherit.aes = FALSE) +
  ggplot2::facet_wrap(~dep_var, scales = "free") +
  #ggplot2::scale_color_brewer(palette = "PRGn") +
  ggplot2::scale_colour_viridis_d() +
  ggplot2::coord_cartesian(expand = TRUE) +

  ggplot2::labs(x = NULL) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none",
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank()) +

  ggplot2::geom_line(data = model$full_data %>%
                       dplyr::rename(dep_var = "na_item") %>%
                       dplyr::filter(.data$dep_var %in% central$dep_var,
                                     .data$time > min(central$start)),
                     ggplot2::aes(x = time, y= values), color = "black", linewidth = 1) -> p

ggsave(plot = p, "data-raw/EMCC/Hindcast.png", width = 7, height = 5, dpi = 1000,bg = "white")





# Sensitivity -------------------------------------------------------------

# takes about 1 to 2 minutes
sensitivity <- forecast_sensitivity(model)
sensitivity$plot +
  scale_x_date(date_labels = "%Y", date_breaks = "year") -> p
ggsave(plot = p,filename = "data-raw/EMCC/Sensitivity.png", dpi = 1000, width = 9, height = 6, bg = "white")


# Scenarios ---------------------------------------------------------------

exog <- bb$exog_data_nowcast

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
(plot(bb, first_value = "2015-01-01", exclude.exogenous = FALSE,
      grepl_variables = "CO2|Electricity|GValueAddIndus") + geom_hline(aes(yintercept = 0)) -> p)
ggsave(p, filename = "small examples/EMCC Aug 23/Default_Forecast_focused.png", dpi = 1000, bg = "white",
       width = 7, height = 5)


(plot(bb_modified, first_value = "2015-01-01", exclude.exogenous = FALSE,
      grepl_variables = "CO2|Electricity|GValueAddIndus") + geom_hline(aes(yintercept = 0)) -> p)
ggsave(p, filename = "small examples/EMCC Aug 23/Scenario_Forecast_focused.png", dpi = 1000, bg = "white",
       width = 7, height = 5)


(plot(bb_modified, first_value = "2015-01-01", exclude.exogenous = FALSE,
      grepl_variables = "CO2|Electricity|GValueAddIndus") + geom_hline(aes(yintercept = 0)) -> p)
ggsave(p, filename = "small examples/EMCC Aug 23/Scenario_Forecast_focused.png", dpi = 1000, bg = "white",
       width = 7, height = 5)

plot(bb, first_value = "2015-01-01", exclude.exogenous = FALSE) %>%
  ggsave(.,filename = "small examples/EMCC Aug 23/Default_Forecast_Full.png", dpi = 1000, bg = "white",
         width = 7, height = 6)
plot(bb_modified, first_value = "2015-01-01", exclude.exogenous = FALSE) %>%
  ggsave(.,filename = "small examples/EMCC Aug 23/Scenario_Forecast_Full.png", dpi = 1000, bg = "white",
         width = 7, height = 6)


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







# Germany -----------------------------------------------------------------


model_ger <- run_model(specification = specification,
                       dictionary = cur_dict %>% mutate(geo = case_when(!is.na(geo)~"DE",TRUE~geo)),
                       inputdata_directory = "data-raw/EMCC/DE",
                       primary_source = "local",
                       save_to_disk = "data-raw/EMCC/DE/DE_emcc_data.csv",
                       present = FALSE,
                       quiet = FALSE)

bb_ger <- forecast_model(model_ger)

plot(bb_ger, first_value = "2015-01-01", exclude.exogenous = FALSE) %>%
  ggsave(.,filename = "data-raw/EMCC/DE/DE_Default_Forecast_Full.png", dpi = 1000, bg = "white",
         width = 7, height = 6)





# NL -----------------------------------------------------------------


model_nl <- run_model(specification = specification,
                      dictionary = cur_dict %>% mutate(geo = case_when(!is.na(geo)~"NL",TRUE~geo)),
                      inputdata_directory = "data-raw/EMCC/NL",
                      primary_source = "local",
                      save_to_disk = "data-raw/EMCC/NL/NL_emcc_data.csv",
                      present = FALSE,
                      quiet = FALSE)

bb_nl <- forecast_model(model_nl)

plot(bb_nl, first_value = "2015-01-01", exclude.exogenous = FALSE) %>%
  ggsave(.,filename = "data-raw/EMCC/NL/NL_Default_Forecast_Full.png", dpi = 1000, bg = "white",
         width = 7, height = 6)

