devtools::load_all()

library(tidyverse)
library(kableExtra)
library(ggtext)

new_dict <- dict %>%
  bind_rows(tibble(
    model_varname = "IndProd",
    full_name = "An index of Industrial Production",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt",
    freq = "q",
    geo = "AT",
    unit = "I15",
    s_adj = "NSA",
    nace_r2 = "B-D"))

specification <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "EmiCO2Industry",
    "EmiCO2Combustion",
    "FinConsExpHH"
  ),
  independent = c(
    "HICP_Energy + IndProd + Export",
    "FinConsExpHH + HICP_Energy",
    "FinConsExpGov + HICP_Energy"
  )
)
specification


# Estimation --------------------------------------------------------------

model2 <- run_model(specification = specification,
                   dictionary = new_dict,
                   inputdata_directory = "data-raw/your_folder/",
                   primary_source = "local",
                   save_to_disk = "data-raw/your_folder/data_new.xlsx",
                   max.ar = 2,
                   max.dl = 2,
                   quiet = FALSE,
                   saturation.tpval = 0.001,
                   constrain.to.minimum.sample = FALSE)

library(modelsummary)

model_list2 <- lapply(model2$module_collection$model, gets::as.lm)
names(model_list2) <- model2$module_order$dependent
modelsummary::modelsummary(
  model_list2,
  coef_omit = "iis|sis",
  gof_omit = "R",
  title = "Models run for each sub-module for this illustrative example of Austria.",
  notes = "Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
  stars = TRUE
)


diagnostics_model(model) %>%
  rename(Module = module,
         `Indicator Share` = indicator_share) %>%
  kable(booktabs = TRUE, digits = 3, label = "diagnostics", caption = "Diagnostic results for each sub-module.") %>%
  kable_styling()


model2 %>%
  network()

f1 <- forecast_model(model2, exog_fill_method = "AR", plot.forecast = FALSE)
View(f1$exog_data_nowcast)

help(forecast_model)

f1$exog_data_nowcast

exog_data_high<- f1$exog_data_nowcast %>%
  mutate(HICP_Energy = 0.85*HICP_Energy)

View(exog_data_high)

f1_high <- forecast_model(model2, exog_predictions = exog_data_high, plot.forecast = FALSE)

f1_data <- plot(f1, exclude.exogenous = FALSE, grepl_variables = "Combustion|Energy", return.data = TRUE)
f1_high_data <- plot(f1_high, exclude.exogenous = FALSE, grepl_variables = "Combustion|Energy", return.data = TRUE)

plotting_scenario_data <- f1_data %>%
  mutate(name = "AR Forecast") %>%
  bind_rows(f1_high_data %>%
              mutate(name = NA) %>%
              mutate(name = case_when(na_item == "EmiCO2Combustion" & type == "Endogenous Forecast" ~ "Scenario Values",
                                      na_item == "HICP_Energy" & type == "Exogenous Forecast" ~ "Scenario Values",
                                      TRUE ~ name)))
plotting_scenario_data %>%
  ggplot(aes(x = time, y = values, linetype = name, color = type)) +
  geom_line() +
  coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2026-07-01"))) +
  facet_wrap(~na_item, scales = "free") +
  geom_line(linewidth = 1, na.rm = TRUE) +
  facet_wrap(~.data$na_item, scales = "free") +
  labs(x = "Year", y = "Values") +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

f1_data %>%
  mutate(name = "AR Forecast") %>%
  bind_rows(f1_high_data %>%
              mutate(name = "Scenario Values")) %>%
  filter(na_item == "EmiCO2Combustion" & type == "Endogenous Forecast") %>%
  summarise(emissions = sum(values, na.rm = TRUE), .by = name) %>%
  mutate(total = sum(emissions),
         diff = c(NA,diff(emissions)),
         rel_diff = diff/total)

