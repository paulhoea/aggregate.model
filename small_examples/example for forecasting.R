
library(tidyverse)
library(extrafont)

# DGP
# y = 4 x1 + -0.8 x2
# x2 = 3 + 0.4 x3 + -0.1 x4

# Data gen
set.seed(124)

n <- 100
n.ahead <- 30
no.draws <- 50

x1 <- rnorm(mean = 5, n = n, sd = 1)
x3 <- rnorm(mean = -10, n = n, sd = 5)
x4 <- rnorm(mean = -20, n = n, sd = 2.5)

e1 <- rnorm(n, sd = 2)
e2 <- rnorm(n, sd = 3)


x2 <- 3 + 0.4 * x3 - 0.1 * x4 + e2
y <- 4 * x1 - 0.8 * x2 + e1


# Estimate Models

mod1 <- lm(x2 ~ x3 + x4)
mod2 <- lm(y ~ x1 + x2)

# Create exog values for predictions
set.seed(124)
exog_vals <- tibble(x1 = rnorm(n.ahead),
                    x3 = rnorm(n.ahead, mean = -20),
                    x4 = rnorm(n.ahead, mean = -10))

pred_x2 <- predict(mod1, newdata = exog_vals)

exog_vals_x2 <- bind_cols(exog_vals,tibble(x2 = pred_x2))

pred_y <- predict(mod2, newdata = exog_vals_x2)

tibble(index = 1:n,
       y_orig = y,
       y_fit = mod2$fitted.values,
       x2_orig = x2,
       x2_fit = mod1$fitted.values) %>%
  bind_rows(tibble(index = n:(n + n.ahead),
                   y_pred = c(mod2$fitted.values[n],pred_y),
                   x2_pred = c(mod1$fitted.values[n],pred_x2))) %>%
  pivot_longer(-index, names_sep = "_", names_to = c("var","type"))  -> inital_plot_df

inital_plot_df %>%
  ggplot(aes(y = value, x = index, color = type, group = paste0(type,var))) +
  geom_vline(aes(xintercept = n), size = 0.5, linetype = 2)+
  geom_line() +

  facet_wrap(~var, scales = "free") +
  theme_minimal(base_family = "Myriad Pro")

### Uncertainties -----

## Option 1: sample from the residuals --------------
res_mod1 <- sample(mod1$residuals, size = no.draws, replace = TRUE)

res_matrix <- matrix(rep(res_mod1, each = length(pred_x2)), nrow = length(pred_x2),
                     dimnames = list(NULL, paste0("x2_",1:no.draws)))

res_matrix_tib <- as_tibble(pred_x2 + res_matrix)

# now isolate the uncertainty from the model 2
res_mod2 <- sample(mod2$residuals, size = no.draws, replace = TRUE)

# now predict y
y_draws <- tibble()
for(i in 1:no.draws){
  # i = 1
  exog_vals_x2 %>%
    mutate(x2 = res_matrix_tib[,i, drop = TRUE]) -> exog_vas_x2_intermed

  tibble(id = i,
         value = predict(mod2, newdata = exog_vas_x2_intermed)) %>%

    # here adding the y uncertainty (residual draws from model2)
    mutate(value = value + res_mod2[i]) %>%
    bind_rows(y_draws, .) -> y_draws
}


y_draws %>%
  group_by(id) %>%
  mutate(index = (n+1) : (n + n.ahead)) %>%
  ungroup() %>%
  mutate(id = as.character(id),
         type = "draws",
         var = "y") -> y_draws_collected


res_matrix_tib %>%
  mutate(index = (n+1):(n + n.ahead)) %>%
  pivot_longer(-index, names_sep = "_", names_to = c("var","id")) %>%
  mutate(type = "draws") %>%
  bind_rows(inital_plot_df, .) %>%
  bind_rows(y_draws_collected, .) %>%

  ggplot(aes(y = value, x = index, color = type, group = paste0(type,var, id))) +
  geom_vline(aes(xintercept = n), size = 0.5, linetype = 2)+
  geom_line(size = 1) +

  facet_wrap(~var, scales = "free") +
  theme_minimal(base_family = "Myriad Pro")





## Option 2: sample from the residual variance --------

res_mod1 <- rnorm(sd = sd(mod1$residuals), n = no.draws)

res_matrix <- matrix(rep(res_mod1, each = length(pred_x2)), nrow = length(pred_x2),
                     dimnames = list(NULL, paste0("x2_",1:no.draws)))

res_matrix_tib <- as_tibble(pred_x2 + res_matrix)

# now isolate the uncertainty from the model 2
res_mod2 <- rnorm(sd = sd(mod2$residuals), n = no.draws)

# now predict y
y_draws <- tibble()
for(i in 1:no.draws){
  # i = 1
  exog_vals_x2 %>%
    mutate(x2 = res_matrix_tib[,i, drop = TRUE]) -> exog_vas_x2_intermed

  tibble(id = i,
         value = predict(mod2, newdata = exog_vas_x2_intermed)) %>%

    # here adding the y uncertainty (residual draws from model2)
    mutate(value = value + res_mod2[i]) %>%
    bind_rows(y_draws, .) -> y_draws
}


y_draws %>%
  group_by(id) %>%
  mutate(index = (n+1) : (n + n.ahead)) %>%
  ungroup() %>%
  mutate(id = as.character(id),
         type = "draws",
         var = "y") -> y_draws_collected


res_matrix_tib %>%
  mutate(index = (n+1):(n + n.ahead)) %>%
  pivot_longer(-index, names_sep = "_", names_to = c("var","id")) %>%
  mutate(type = "draws") %>%
  bind_rows(inital_plot_df, .) %>%
  bind_rows(y_draws_collected, .) %>%

  ggplot(aes(y = value, x = index, color = type, group = paste0(type,var, id))) +
  geom_vline(aes(xintercept = n), size = 0.5, linetype = 2)+
  geom_line(size = 1) +

  facet_wrap(~var, scales = "free") +
  theme_minimal(base_family = "Myriad Pro")


