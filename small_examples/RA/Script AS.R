library(tidyverse)
library(kableExtra)
library(ggtext)


# you can load the latest version of the code using these steps:
# make sure that you are in the project (top right says "aggregate.model")
# also make sure you have a "Git" tab on the top right panel
# also make sure the files contain the package code (and a folder named R)
# click "Pull" on the Git tab
# then execute:
devtools::load_all()
# then you should see "Loading aggregate.model"


# Setup -------------------------------------------------------------------
# before getting started make sure to add any further variables that you need
# here is one example but see: https://moritzschwarz.org/aggregate.model/articles/new_variable_to_dict.html

# adding a new variable from EUROSTAT to the dictionary
dict %>%
  bind_rows(tibble(
    model_varname = "IndProd", # this is free to choose but must be unique
    full_name = "An index of Industrial Production",
    database  = "eurostat",
    variable_code = "PROD", # in this case use the bt_indicator information here
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt", # here we specify what the column with the variables is called
    freq = "q", # for quarterly data, 'm' would be monthly
    geo = "AT",
    unit = "I15", # for index of 2015 = 100
    s_adj = "NSA", # not seasonally adjusted
    nace_r2 = "B-D")) -> new_dict


# now we specify our model
# any estimation needs "n"
# any identity needs "d"
# this is an example model with 4 equations
specification <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "EmiCO2Industry",
    "EmiCO2Combustion",
    "EmiCH4Livestock",
    "FinConsExpHH"
  ),
  independent = c(
    "HICP_Gas + HICP_Electricity + IndProd + Export",
    "FinConsExpHH + HICP_Electricity + EmiCO2Industry",
    "FinConsExpHH + Export + GValueAddAgri",
    "FinConsExpGov + HICP_Gas + HICP_Electricity"
  )
)
specification


# Estimation --------------------------------------------------------------
model <- run_model(specification = specification,
                   dictionary = new_dict,
                   inputdata_directory = "data-raw/your_folder/",
                   primary_source = "local",

                   # modify save_to_disk for each new country/specification
                   # this avoids having to download the same data multiple times
                   save_to_disk = "data-raw/your_folder/data_1.xlsx",

                   quiet = FALSE,
                   saturation.tpval = 0.001,
                   constrain.to.minimum.sample = FALSE)



# you can use these few lines to print a table of the model results
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


# -- Diagnostics ---------
# these few lines will enable you to look at the diagnostics of the model
diagnostics_model(model) %>%
  rename(Module = module,
         `Indicator Share` = indicator_share) %>%
  kable(booktabs = TRUE, digits = 3, label = "diagnostics", caption = "Diagnostic results for each sub-module.") %>%
  kable_styling()


# show the network graph
model %>%
  network()

# ggsave(p, width = 7, height = 5, file = "data-raw/test_for_now/EAERE_network.pdf")


# -- Forecasting ----------
# This is how we forecast our models
# below I have always chosen to use the "auto" method
# but the AR should also work
f1 <- forecast_model(model, exog_fill_method = "AR")
f2 <- forecast_model(model, exog_fill_method = "auto", plot.forecast = FALSE)

# -- Forecasting Plotting ---------------

plot(f2) +
  labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values.") +
  theme(text = element_text(family = "Myriad Pro"),
        plot.subtitle = element_markdown()) #-> p

# ggsave(p, width = 7, height = 5, file = "data-raw/test_for_now/EAERE_forecast_simple.pdf", device = cairo_pdf)



plot(f2, exclude.exogenous = FALSE) +
  labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.") +
  theme(text = element_text(family = "Myriad Pro"),
        plot.subtitle = element_markdown()) #-> p

# ggsave(p, width = 7, height = 5, file = "data-raw/test_for_now/EAERE_forecast_inclexog.pdf", device = cairo_pdf)


# Scenario/Policy Analysis ------------------------------------------------
# This is where your work will start

# first of all, we should check the modelsummary again so that we are aware of which effects we expect
# e.g. are two variables positively or negatively associated?
# e.g. are two variables retained in the model I specified?

modelsummary::modelsummary(
  model_list,
  coef_omit = "iis|sis",
  gof_omit = "R",
  title = "Final models run for each sub-module for the illustrative example of Austria.",
  notes = "Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
  stars = TRUE
)



# once we have an idea of what we want to model, we turn to our forecast
# we already have our f2 model from above (we can also use any other forecasting method)
f2

# from that, we will take the exogenous data and save it
# this data will be the one that we will play around with most
f2$exog_data_nowcast %>%

  # this data we will now modify to simulate the effect that we are interested in
  # in this particular example, we are assuming that the electricity price index soars to 400
  mutate(HICP_Electricity = 400) -> exog_data_high_ready

# using this dataset, we then run another forecast
# we pass the modified data to exog_predictions
f2_high <- forecast_model(model, exog_predictions = exog_data_high_ready, plot.forecast = FALSE)


# now we are done with our modelling, let's evaluate it
# we want to evaluate all this in a few plots
# because those plots are slightly more complicated, we first get the data for both forecasts
# this is simple using the plot function and using return.data = TRUE
f2_high_data <- plot(f2_high, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity", return.data = TRUE)
f2_data <- plot(f2, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity", return.data = TRUE)


f2_data %>%
  mutate(name = "AR Forecast") %>%
  bind_rows(f2_high_data %>%
              mutate(name = NA) %>%
              mutate(name = case_when(na_item == "EmiCO2Combustion" & type == "Endogenous Forecast" ~ "Scenario Price",
                                      na_item == "HICP_Electricity" ~ "Scenario Price",
                                      TRUE ~ name))) -> plotting_scenario_data

plotting_scenario_data %>%

  ggplot(aes(x = time, y = values, linetype = name, color = type)) +
  geom_line() +
  coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2025-01-01"))) +
  facet_wrap(~na_item, scales = "free") +
  geom_line(linewidth = 1, na.rm = TRUE) +

  facet_wrap(~.data$na_item, scales = "free") +

  labs(x = NULL, y = NULL) +

  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = manual_colours) +

  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title = "Illustrative Example for Austria") +
  theme(text = element_text(family = "Myriad Pro"))



# now we can quickly calculate the entire saving
f2_data %>%
  mutate(name = "AR Forecast") %>%
  bind_rows(f2_high_data %>%
              mutate(name = "Scenario Price")) %>%

  filter(na_item == "EmiCO2Combustion" & type == "Endogenous Forecast") %>%

  summarise(emissions = sum(values, na.rm = TRUE), .by = name) %>%
  mutate(total = sum(emissions),
         diff = c(NA,diff(emissions)),
         rel_diff = diff/total)

# So emissions are 1.45 % lower with higher electricity prices


