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


# Insample Forecasting ----------------------------------------------------
# Here we use hindcasts to check the validity of our model
# this takes quite a bit of time, so make sure to do this wisely (check everything before)
# then perhaps try to only use .95 for the first run
# then extend to e.g. .9 or .8 to check if the model really works

f2_insample <- forecast_insample(model, sample_share = .97, exog_fill_method = "auto")

# you can also change the exog_fill_method to e.g. "AR" or "last"

f2_insample$plot


extract_dep_vars <- f2_insample$central %>% distinct(dep_var) %>% pull

ggplot2::ggplot() +
  ggplot2::geom_line(data = model$full_data %>%
                       rename(dep_var = na_item) %>%
                       filter(dep_var %in% extract_dep_vars,
                              time > as.Date("2010-01-01")),
                     ggplot2::aes(x = .data$time, y = .data$values), linewidth = 1) +

  ggplot2::facet_wrap(~dep_var, scales = "free") +
  ggplot2::geom_ribbon(data = f2_insample$uncertainty, ggplot2::aes(ymin = .data$min, x = .data$time, ymax = .data$max, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
  ggplot2::geom_ribbon(data = f2_insample$uncertainty, ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
  ggplot2::geom_ribbon(data = f2_insample$uncertainty, ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +

  ggplot2::geom_line(data = f2_insample$central, ggplot2::aes(y = .data$value, x = .data$time, color = as.factor(.data$start)), inherit.aes = FALSE) +
  ggplot2::facet_wrap(~.data$dep_var, scales = "free") +
  #ggplot2::scale_color_brewer(palette = "PRGn") +
  ggplot2::scale_colour_viridis_d() +
  ggplot2::coord_cartesian(expand = TRUE) +

  ggplot2::labs(x = NULL, y = NULL, title = "Automatic Forecasting Hindcasts") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none",
                 text = element_text(family = "Myriad Pro"),
                 panel.grid.major.x = ggplot2::element_blank(),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_blank())






