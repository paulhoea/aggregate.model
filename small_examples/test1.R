# small example to follow the development and test functionalities of the package

# example specification
spec <- tibble(
  type = c(
    "d",
    "d",
    "n"
  ),
  dependent = c(
    "JL",
    "TOTS",
    "B"
  ),
  independent = c(
    "TOTS - CP - CO - J - A",
    "YF + B",
    "CP + J"
  )
)
# test specification
spec <- check_config_table(spec)
spec <- translate_variables(spec)

# check whether a different dictionary works

userdict <- tibble::tribble(
  ~`eurostat_code`, ~`model_varname`, ~`full_name`, ~`dataset_id`,
  "B1GQ", "GDP", "Gross domestic product at market prices", "namq_10_gdp",
  "B1G", "VA", "Value added, gross", "namq_10_gdp",
  "P6", "X", "Exports of goods and services", "namq_10_gdp",
  "P7", "I", "Imports of goods and services", "namq_10_gdp",
  "P5G", "GCF","Gross capital formation", "namq_10_gdp",
  "P3", "CP + CO", "Final consumption expenditure", "namq_10_gdp",
  "P3_S13", "CO", "Final consumption expenditure of general government", "namq_10_gdp",
  "P31_S14_S15", "CP", "Household and NPISH final consumption expenditure", "namq_10_gdp",
  "YA0", "JL", "Statistical discrepancy (expenditure approach)", "different_id"
)
userspec <- tibble(
  type = c(
    "d",
    "d",
    "n"
  ),
  dependent = c(
    "JL",
    "TOTS",
    "I"
  ),
  independent = c(
    "TOTS - CP - CO - GCF - X",
    "VA + I",
    "CP + GCF"
  )
)
userspec <- check_config_table(userspec)
userspec <- translate_variables(userspec, dictionary = userdict)

stopifnot(identical(spec$dependent_eu, userspec$dependent_eu))
stopifnot(identical(spec$independent_eu, userspec$independent_eu))

# check which dataset needs to be downloaded
dataset_ids <- determine_datacodes(specification = spec)
determine_datacodes(specification = userspec, dictionary = userdict)

# check which eurostat codes need to be found
code_ids <- determine_eurocodes(spec)

# Austria, seasonally & calendar adjusted, chain linked volumes (2015) in million euro for most vars, slightly different for YA0
fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

data <- load_or_download_variables(specification = spec, filter_list = filter_list,
                                   download = TRUE, dictionary = NULL, inputdata_directory = NULL, save_to_disk = NULL)
data2 <- load_or_download_variables(specification = spec, filter_list = filter_list,
                                   download = FALSE, dictionary = NULL, inputdata_directory = "./small_examples",
                                   save_to_disk = "./small_examples/full_localdata.csv")
stopifnot(identical(data, data2))
# seems to work as intended


# follow the names in run_model() so can run interactively
spec <- tibble(
  type = c(
    "d",
    "d",
    "n"
  ),
  dependent = c(
    "JL",
    "TOTS",
    "B"
  ),
  independent = c(
    "TOTS - CP - CO - J - A",
    "YF + B",
    "CP + J"
  )
)

fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

module_order <- check_config_table(spec)

module_order_eurostatvars <- translate_variables(module_order, dictionary = NULL)

loaded_data <- load_or_download_variables(specification = module_order_eurostatvars,
                                          filter_list = filter_list,
                                          download = TRUE)

full_data <- calculate_identities(specification = module_order_eurostatvars,
                                               data = loaded_data, dictionary = NULL)

classification <- classify_variables(specification = module_order_eurostatvars)

module_collection <- module_order_eurostatvars %>%
  mutate(predicted = list(NA_complex_),
         model = list(NA_complex_))

tmp_data <- full_data

i <- 1
cat(paste0("Estimating ", module_order_eurostatvars$dependent[i], " = ", module_order_eurostatvars$independent[i]))
module = module_order_eurostatvars[module_order_eurostatvars$order == i, ]
data <- tmp_data

# not run b/c not finished yet -> look inside run_module manually
# module_estimate <- run_module(
#   module = module_order_eurostatvars[module_order_eurostatvars$order == i, ],
#   data = tmp_data,
#   classification = classification
# )

raw_data <- identify_module_data(module, classification, data)
clean_df <- clean_data(raw_data)

# extract base variable names (and convert to lower case because janitor::clean_names() does so)
dep <- module$dependent_eu
dep <- tolower(dep)
indep <- strsplits(module$independent_eu, splits = c("\\+", "\\-"))
indep <- tolower(gsub(" ", "", indep))

estimated_module <- estimate_module(clean_data = clean_df,
                                    dep_var_basename = dep,
                                    x_vars_basename = indep)

moduledata <- add_to_original_data(clean_data = clean_df,
                                   isat_object = estimated_module$best_model,
                                   dep_var_basename = dep,
                                   ardl_or_ecm = estimated_module$args[[5]])

# would be the output from run_module()
module_estimate <- list(model = estimated_module$best_model, data = moduledata)

# check whether works as a whole
module_est <- run_module(
  module = module_order_eurostatvars[module_order_eurostatvars$order == i, ],
  data = tmp_data,
  classification = classification
)
waldo::compare(module_estimate, module_est) # almost identical, except for the date/time of course

module_collection[module_collection$order == i, "dataset"] <- tibble(dataset = list(module_estimate$data))
module_collection[module_collection$order == i, "model"] <- tibble(dataset = list(module_estimate$model))

tmp_data <- update_data(orig_data = tmp_data, new_data = module_estimate$data)


i <- 2
cat(paste0("Estimating ", module_order_eurostatvars$dependent[i], " = ", module_order_eurostatvars$independent[i]))
module = module_order_eurostatvars[module_order_eurostatvars$order == i, ]

# inside run_module()
raw_data <- identify_module_data(module, classification, tmp_data)


module_estimate <- run_module(
  module = module_order_eurostatvars[module_order_eurostatvars$order == i, ],
  data = tmp_data,
  classification = classification
)



# overall test

spec <- tibble(
  type = c(
    "d",
    "d",
    "n"
  ),
  dependent = c(
    "JL",
    "TOTS",
    "B"
  ),
  independent = c(
    "TOTS - CP - CO - J - A",
    "YF + B",
    "CP + J"
  )
)

fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

a <- run_model(specification = spec, dictionary = NULL, inputdata_directory = NULL, filter_list = filter_list, download = TRUE, save_to_disk = NULL, present = FALSE)

saveRDS(a, file = "./small_examples/modelrun1.Rds")





