#------------------------------------------------------------------------------#
#              COMPLETE ANALYSIS AND MODEL COMPARISON                          #
#       Theoretical Calibration (Optimization) of the Diffusion with Retention Model      #
#------------------------------------------------------------------------------#

# --- 1. Necessary Packages ---
# install.packages(c("ggplot2", "quantmod", "dplyr", "moments"))
library(ggplot2)
library(quantmod)
library(dplyr)
library(moments)

# --- 2. Definition of Simulation Functions ---

# Simulation function of the Normal (Advection-Diffusion) model
simulate_price_path <- function(S0, A, B, n_steps, dt) {
  S <- numeric(n_steps)
  x <- numeric(n_steps)
  S[1] <- S0
  x[1] <- log(S0)
  mean_dx <- B * dt
  sd_dx <- sqrt(A * 2 * dt)
  for (t in 2:n_steps) {
    dx <- rnorm(1, mean = mean_dx, sd = sd_dx)
    x[t] <- x[t - 1] + dx
    S[t] <- exp(x[t])
  }
  return(data.frame(time = 1:n_steps, price = S))
}

# Simulation function of the Retention model (t-Student approximation)
simulate_retention_path <- function(S0, drift, variance, df, n_steps, dt) {
  S <- numeric(n_steps)
  x <- numeric(n_steps)
  S[1] <- S0
  x[1] <- log(S0)
  # The variance of t-dist is df/(df-2). We need to scale the standard deviation.
  scale_factor <- sqrt(variance * (df - 2) / df)
  for (t in 2:n_steps) {
    dx <- drift * dt + rt(1, df = df) * scale_factor * sqrt(dt)
    x[t] <- x[t - 1] + dx
    S[t] <- exp(x[t])
  }
  return(data.frame(time = 1:n_steps, price = S))
}


# --- 3. Data ---
ticker <- "PETR4.SA"
start_date <- "2020-01-01"
end_date <- "2025-07-31" 

getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)
asset_prices <- Ad(get(ticker))
log_returns <- na.omit(dailyReturn(asset_prices, type = 'log'))

# Empirical moments (our targets for calibration)
mean_emp <- as.numeric(mean(log_returns))
var_emp <- as.numeric(var(log_returns))
kurt_emp_excess <- kurtosis(log_returns) - 3

cat("--- Empirical moments (target) ---\n")
cat("Mean:", mean_emp, "\n")
cat("Variance:", var_emp, "\n")
cat("Excess kurtosis:", kurt_emp_excess, "\n\n")


# --- 4. Calibration of the Advection-Diffusion Model (Normal) ---
D_estimate <- mean_emp
V_estimate <- var_emp / 2


# --- 5. Theoretical Calibration of the Diffusion model with retetion ---

# Objective Function Definition
objetive_function <- function(params, var_target, kurt_target) {
  k <- params[1]
  K2 <- params[2]
  K4 <- params[3]
  if (k <= 0 | k >= 1 | K2 <= 0 | K4 <= 0) {
    return(1e9)
  }
  dt <- 1
  var_theoretical <- 2 * (1 - k) * K2 * dt
  if (var_theoretical < 1e-9) return(1e9)
  kurt_theoretical <- (12 * k * (1 - k) * K4 * dt) / (var_theoretical^2)
  error_var <- ((var_theoretical - var_target) / var_target)^2
  error_kurt <- ((kurt_theoretical - kurt_target) / kurt_target)^2
  return(error_var + error_kurt)
}

#Fixed Optimization with Scale Control
cat("--- Calibrating Retention Model ---\n")
kickoff <- c(0.1, var_emp / 2, var_emp) 

result_otimization <- optim(
  par = kickoff,
  fn = objetive_function,
  var_target = var_emp, 
  kurt_target = kurt_emp_excess,
  method = "L-BFGS-B",
  lower = c(1e-6, 1e-9, 1e-9),
  upper = c(0.99, Inf, Inf),
  control = list(parscale = kickoff) 
)

params_calibrated <- result_otimization$par
k_calibrated <- params_calibrated[1]
K2_calibrated <- params_calibrated[2]
K4_calibrated <- params_calibrated[3]

cat("Calibrated params:\n")
cat("k (Inercia) =", k_calibrated, "\n")
cat("K2 (Diffusion) =", K2_calibrated, "\n")
cat("K4 (Retenção) =", K4_calibrated, "\n\n")
dt <- 1

# Calculate the theoretical variance using the new parameters
var_theoretical_final <- 2 * (1 - k_calibrated) * K2_calibrated * dt

# Calculate theoretical excess kurtosis
kurt_theoretical_final <- (12 * k_calibrated * (1 - k_calibrated) * K4_calibrated * dt) / (var_theoretical_final^2)

cat("--- Calibration Verification ---\n")
cat("Empirical Variance (Target):", var_emp, "\n")
cat("Theoretical Variance (Result):", var_theoretical_final, "\n\n")

cat("Empirical Kurtosis (Target):", kurt_emp_excess, "\n")
cat("Theoretical Kurtosis (Result):", kurt_theoretical_final, "\n")

# --- 6. Simulations ---
n_sims <- 5
n_steps_real <- nrow(asset_prices)

normal_sims_list <- list()
retention_sims_list <- list()

for (i in 1:n_sims) {
  current_seed <- 100 + i
  set.seed(current_seed)
  sim_path_normal <- simulate_price_path(S0 = 100, B = D_estimate, A = V_estimate, n_steps = n_steps_real, dt = 1)
  sim_path_normal$sim_id <- paste("Sim", i)
  normal_sims_list[[i]] <- sim_path_normal
  
  df_simulacao <- 6 / kurt_theoretical_final + 4
  
  set.seed(current_seed)
  sim_path_retention <- simulate_retention_path(S0 = 100, drift = mean_emp, variance = var_emp, df = df_simulacao, n_steps = n_steps_real, dt = 1)
  sim_path_retention$sim_id <- paste("Sim", i)
  retention_sims_list[[i]] <- sim_path_retention
}
normal_sims_df <- bind_rows(normal_sims_list)
retention_sims_df <- bind_rows(retention_sims_list)


# --- 7. Final Graphics ---

# Real data
real_data <- data.frame(time = 1:n_steps_real, price = as.numeric(coredata(asset_prices)))
real_data$price <- real_data$price * (100 / real_data$price[1])

# GRAPH 1: Real Price vs. Multiple Simulations of the Normal Model
print(
  ggplot() +
    geom_line(data = normal_sims_df, 
              aes(x = time, y = price, group = sim_id), 
              color = "deepskyblue3", alpha = 0.6) +
    geom_line(data = real_data, aes(x = time, y = price), color = "black", linewidth = 1) +
    labs(
      x = "Days of Negociation", 
      y = "Normalized Price (Start = 100)") +
    theme_minimal()
)

# GRAPH 2: Actual Price vs. Multiple Retention Model Simulations
print(
  ggplot() +
    geom_line(data = retention_sims_df, 
              aes(x = time, y = price, group = sim_id), 
              color = "firebrick", alpha = 0.6) +
    geom_line(data = real_data, aes(x = time, y = price), color = "black", linewidth = 1) +
    labs(
      x = "Days of Negociation", 
      y = "t-Student Price (Start = 100)") +
    theme_minimal()
)

# GRAPH 3: Comparative Histogram
returns_df <- data.frame(log_return = coredata(log_returns))
colnames(returns_df) <- "log_return"
sd_normal <- sqrt(var_emp)
df_plot <- 6 / kurt_theoretical_final + 4
scale_factor_t <- sqrt(var_emp * (df_plot - 2) / df_plot)
dscaled_t <- function(x, mean, scale, df) (1/scale * dt((x - mean) / scale, df))

print(
  ggplot(returns_df, aes(x = log_return)) +
    geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "gray80", color = "black") +
    stat_function(fun = dnorm, 
                  args = list(mean = mean_emp, sd = sd_normal), 
                  aes(color = "Normal Model"), linewidth = 0.6) +
    stat_function(fun = dscaled_t, 
                  args = list(mean = mean_emp, scale = scale_factor_t, df = df_plot), 
                  aes(color = "Retention Model (t-Student)"), linewidth = 0.6) +
    labs(
      x = "Daily Log-Retorn", 
      y = "Density", 
      color = "Theoretical Model") +
    scale_color_manual(values = c("Normal Model" = "deepskyblue3", "Retention Model (t-Student)" = "firebrick")) +
    coord_cartesian(xlim = c(-0.15, 0.15)) +
    theme_minimal() 
    #theme(legend.position = "top")
)

# --- QQ-plots ---

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1)) 

# GRAPH 1: Q-Q Plot for the Normal Model
returns_vec <- as.numeric(coredata(log_returns))

qqnorm(returns_vec,
       xlab = "Theoretical Quantiles (Normal)",
       ylab = "Sample Quantiles (PETR4.SA)",
       pch = 19, cex = 0.5, col = "deepskyblue4")
qqline(returns_vec, col = "red", lwd = 2)


# GRAPH 2: Q-Q Plot for the Retention Model

# Parameters of our theoretical Student's t-distribution
df_calibrated <- 6 / kurt_theoretical_final + 4
scale_factor_t <- sqrt(var_emp * (df_calibrated - 2) / df_calibrated)

# Generates a theoretical sample of our calibrated Student's t-distribution
n_samples <- length(returns_vec)
set.seed(100)
amostra_teorica_t <- mean_emp + rt(n_samples, df = df_calibrated) * scale_factor_t

# Creates the Q-Q plot base
qqplot(amostra_teorica_t, returns_vec,
       xlab = "Theoretical Quantiles (Calibrated t-Student)",
       ylab = "Sample Quantiles (PETR4.SA)",
       pch = 19, cex = 0.5, col = "firebrick")

# Calculates the robust line that passes through the 1st and 3rd quartiles of 
# both samples
q_x <- quantile(amostra_teorica_t, c(0.25, 0.75), na.rm = TRUE)
q_y <- quantile(returns_vec, c(0.25, 0.75), na.rm = TRUE)

# Calculates the slope and intercept of the line
slope <- (q_y[2] - q_y[1]) / (q_x[2] - q_x[1])
intercept <- q_y[1] - slope * q_x[1]

abline(a = intercept, b = slope, col = "blue", lwd = 2)

# Resets the graphical layout to default
par(mfrow = c(1, 1))
