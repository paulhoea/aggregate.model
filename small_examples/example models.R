########################

devtools::load_all()
#library(aggregate.model)

spec <- dplyr::tibble(
  type = c(
    "d",
    "d",
    "n",
    "n",
    "n",
    "n",
    "d",
    "n",
    "n",
    "d",
    "n",
    "n"
  ),
  dependent = c(
    "StatDiscrep",
    "TOTS",
    "Import",
    "FinConsExpHH",
    "GCapitalForm",
    "Emissions",
    "GDP",
    "GValueAddGov", # as in NAM, technical relationship
    "GValueAddManuf", # more complicated in NAM, see 2.3.3 and 6.3.1
    "DomDemand", # as in NAM
    "GValueAddConstr" ,
    "GValueAddWholesaletrade"
  ),
  independent = c(
    "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "",
    "FinConsExpGov",
    "GDP",
    "GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts",
    "FinConsExpGov", # as in NAM, technical relationship
    "DomDemand + Export + LabCostManuf", # NAM uses 'export market indicator' not exports - unclear what this is, NAM uses unit labour cost in NOR manufacturing relative to the foreign price level - here is just total labour cost
    "FinConsExpHH + FinConsExpGov + GCapitalForm",
    "DomDemand + LabCostConstr + BuildingPermits", # in NAM some form of YFP2J = 0.3JBOL + 0.2JF P N + 0.3JO + 0.3JOIL. Unclear what this is. Using Building Permits instead
    "DomDemand + Export + LabCostService"
  )
)



fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
fc <- list(geo = "AT", unit = "THS_T")
fd <- list(geo = "AT", s_adj = "SCA")
fe <- list(geo = "AT", s_adj = "SCA", unit = "I15")
ff <- list(geo = "AT", s_adj = "SCA", unit = "I16")

filter_list <- list(
  "P7" = fa,
  "YA0" = fb,
  "P31_S14_S15" = fa,
  "P5G" = fa,
  "B1G" = fa,
  "P3_S13" = fa,
  "P6" = fa,
  "GHG" = fc,
  "B1GQ" = fa,
  "PSQM" = fe,
  "LM-LCI-TOT" = ff
)
#
# model_result_1 <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   download = FALSE,
#   inputdata_directory = "data-raw/csv/",
#   trend = TRUE,
#   max.lag = 1
# )
#
# model_result_2 <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   download = FALSE,
#   inputdata_directory = "data-raw/csv/",
#   trend = TRUE,
#   max.lag = 2
# )

model_result_4 <- run_model(
  specification = spec,
  filter_list = filter_list,
  download = FALSE,
  inputdata_directory = "data-raw/csv/",
  trend = TRUE,
  max.lag = 4
)
#
# model_result_8 <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   download = FALSE,
#   inputdata_directory = "data-raw/csv/",
#   trend = TRUE,
#   max.lag = 8,
#   gets_selection = TRUE
# )
#
#
#
# model_result_notrend <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   download = FALSE,
#   inputdata_directory = "data-raw/csv/",
#   trend = FALSE
# )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# model_result_a <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   save_to_disk = NULL,
#   download = TRUE
# )
#
#
# model_result_xlsx <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   save_to_disk = "data-raw/xlsx/input.xlsx",
#   download = TRUE
# )
#
# model_result_xlsx <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   inputdata_directory = "data-raw/xlsx/",
#   download = FALSE
# )
#
# model_result_csv <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   save_to_disk = "data-raw/csv/input.csv",
#   download = TRUE
# )
#
# model_result_csv <- run_model(
#   specification = spec,
#   filter_list = filter_list,
#   inputdata_directory = "data-raw/csv/",
#   download = FALSE
# )
#
#
#
# model_result_mine <- model_result
# load(file = "data-raw/model_res.Rdata")
# model_result_fp <- model_result
#
# load(file = "data-raw/model_res2.Rdata")
# model_result_fp <- model_result
#
#
# model_result_fp$full_data$time <- as.character(model_result_fp$full_data$time)
#
# waldo::compare(model_result_mine$full_data, model_result_fp$full_data)
#
# waldo::compare(model_result_csv$full_data, model_result_xlsx$full_data)
#
# waldo::compare(model_result_csv$full_data, model_result_fp$full_data)
# waldo::compare(model_result_csv$full_data, model_result_xlsx$full_data)
# waldo::compare(model_result$full_data, model_result_csv$full_data)
# waldo::compare(model_result_xlsx$full_data, model_result$full_data)
#
#
# model_forecast <- forecast_model(model_result_csv, n.ahead = 10, exog_fill_method = "AR", plot.forecast = FALSE)
# model_forecast <- forecast_model(model_result_fp, n.ahead = 10, exog_fill_method = "last", plot.forecast = TRUE)
#
# plot(model_forecast)
# plot(model_forecast, exclude.exogenous = TRUE)
# plot(model_forecast, exclude.exogenous = TRUE, order.as.run = TRUE)
#
# present_model()
#
#
# model_forecast <- forecast_model(model_result_csv, n.ahead = 10, exog_fill_method = "AR", plot.forecast = FALSE)









# Experimenting with forecast ---------------------------------------------

x <- forecast_model(model_result_4)

plot(x)



### -- new model -----

spec <- dplyr::tibble(
  type = c(
    #"d",
    #"d",
    "n",
    "n",
    "n",
    "n",
    "d",
    "n",
    "n",
    #"d",
    "n",
    "n"
  ),
  dependent = c(
    #"StatDiscrep",
    #"TOTS",
    "Import",
    "FinConsExpHH",
    "GCapitalForm",
    "Emissions",
    "GDP",
    "GValueAddGov", # as in NAM, technical relationship
    "GValueAddManuf", # more complicated in NAM, see 2.3.3 and 6.3.1
    #"DomDemand", # as in NAM
    "GValueAddConstr" ,
    "GValueAddWholesaletrade"
  ),
  independent = c(
    #"TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    #"GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "",
    "FinConsExpGov + FinConsExpHH",
    "GDP + Export + GValueAddIndus",
    "GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts",
    "FinConsExpGov", # as in NAM, technical relationship
    "Export + LabCostManuf", # NAM uses 'export market indicator' not exports - unclear what this is, NAM uses unit labour cost in NOR manufacturing relative to the foreign price level - here is just total labour cost
    #"FinConsExpHH + FinConsExpGov + GCapitalForm",
    "LabCostConstr + BuildingPermits", # in NAM some form of YFP2J = 0.3JBOL + 0.2JF P N + 0.3JO + 0.3JOIL. Unclear what this is. Using Building Permits instead
    "Export + LabCostService"
  )
)



fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
fc <- list(geo = "AT", unit = "THS_T")
fd <- list(geo = "AT", s_adj = "SCA")
fe <- list(geo = "AT", s_adj = "SCA", unit = "I15")
ff <- list(geo = "AT", s_adj = "SCA", unit = "I16")

filter_list <- list(
  "P7" = fa,
  "YA0" = fb,
  "P31_S14_S15" = fa,
  "P5G" = fa,
  "B1G" = fa,
  "P3_S13" = fa,
  "P6" = fa,
  "GHG" = fc,
  "B1GQ" = fa,
  "PSQM" = fe,
  "LM-LCI-TOT" = ff
)


model_result_4_new <- run_model(
  specification = spec,
  filter_list = filter_list,
  download = FALSE,
  inputdata_directory = "data-raw/csv/",
  trend = TRUE,
  max.lag = 4
)


x1_new <- forecast_model(model_result_4_new, exog_fill_method = "AR")
plot(x1_new)

pp1 <- plot(x1_new, interactive = FALSE, exclude.exogenous = FALSE)
pp2 <- plot(x1_new, interactive = FALSE, exclude.exogenous = TRUE)

ggplot2::ggsave(filename = "Central.png", plot = pp2, bg = "white", width = 7, height = 5)
ggplot2::ggsave(filename = "Expanded.png", plot = pp1, bg = "white", width = 9, height = 7)


present_model(x1_new)

