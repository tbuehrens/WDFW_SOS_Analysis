library(tidyverse) # Use for data manipulation
library(posterior) # Use to manage simulated data

# Find all of the saved "SmoothResults.csv" files in the results directory
list_smoothresults <- function(res_dir) {
  list.files(res_dir) |>
    purrr::keep(\(fn) grepl("SmoothResults.csv$", fn))
}

# Read them in and bind them into a single data frame
read_smoothresults <- function(res_dir) {
  tibble(
    fn = list_smoothresults(res_dir),
    res = map(fn, \(fn) {
      read_csv(
        here::here(res_dir, fn),
        col_types = cols(
          COMMON_POPULATION_NAME = col_character(),
          BROOD_YEAR = col_double(),
          Q2.5. = col_double(),
          Q25. = col_double(),
          Q50. = col_double(),
          Q75. = col_double(),
          Q97.5. = col_double(),
          ESU = col_character(),
          NUMBER_OF_SPAWNERS = col_double()
        )
      )
    })
  ) |>
    unnest(res)
}

# Get initial values from the provided quantiles
init_lnorm_pars <- function(
  vals,
  quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975),
  log_sdlog = FALSE
) {
  # The log of the median (50% quantile) should be very close to the meanlog parameter
  if (0.5 %in% quantiles) {
    meanlog0 <- log(vals[quantiles == 0.5])
  } else {
    meanlog0 <- log(median(vals)) # Hopefully they're at least rougly symmetrical?
  }
  # The 95% interval of the log values should be roughly 4 standard deviations apart
  if (all(c(0.025, 0.975) %in% quantiles)) {
    sdlog0 <- diff(log(vals[quantiles %in% c(0.025, 0.975)])) / 4
  } else {
    sdlog0 <- 1
  }
  # Optionally take the log so that it can be optimized in unconstrained parameter space
  if (log_sdlog) {
    sdlog0 <- log(sdlog0)
  }
  c(meanlog0 = meanlog0, sdlog0 = sdlog0)
}

# Use a squared distance objective function. `data` is a data frame with at
# least columns `probs` and `vals` for the quantile probability and the
# associated values.
obj_lnorm_pars <- function(pars, data) {
  qhats <- qlnorm(data$probs, pars[1], exp(pars[2]))
  sum((qhats - data$vals)^2)
}

# Extract the quantile probabilities from the column names in the SmoothResults files
transform_qname <- function(qname) {
  q <- str_extract(qname, r"([0-9]{1,2}\.?[0-9]?)") |>
    as.numeric()
  q / 100
}

# Minimize the distance to the provided quantiles, then output a one-row data
# frame with the parameters and the convergence code.
optim_lnorm_pars <- function(data) {
  init_pars <- init_lnorm_pars(data$vals, data$vals, log_sdlog = TRUE)
  opt <- optim(init_pars, obj_lnorm_pars, data = data)
  tibble(
    meanlog = unname(opt$par[1]),
    sdlog = unname(exp(opt$par[2])),
    convergence = opt$convergence
  )
}

# Functions for calculating the geometric mean
geomean <- function(x) exp(mean(log(x)))
# This one applies the geomean function by draw across years
pop_geomean <- function(vals) {
  vals |>
    as_draws_array() |>
    apply(1, geomean) |> # Apply over variables
    rvar()
}

# Run the above functions to get the 2.5% and 97.5% quantiles of the geomean
res_dir <- here::here("results 2026-10-15")
res <- read_smoothresults(res_dir) |>
  filter(BROOD_YEAR %in% 2021:2025) |>
  pivot_longer(
    cols = c(`Q2.5.`:`Q97.5.`),
    names_to = "probs",
    names_transform = transform_qname,
    values_to = "vals"
  ) |>
  select(COMMON_POPULATION_NAME, BROOD_YEAR, ESU, probs, vals) |>
  nest(
    data = c(probs, vals),
    .by = c(COMMON_POPULATION_NAME, BROOD_YEAR, ESU)
  ) |>
  mutate(opt = map(data, optim_lnorm_pars)) |>
  unnest(opt)

# Check that all the optimizations have converged
if (any(res$convergence != 0)) {
  stop("Check convergence of optim runs")
}

res2 <- res |>
  mutate(
    # Generate approximate samples from the posterior based on the parameters
    # found above
    approx_post = map2_vec(meanlog, sdlog, \(ml, sdl) {
      rvar_rng(rlnorm, 1, ml, sdl)
    })
  ) |>
  summarize(
    # Take the geometric mean across years for each set of samples
    geomean = pop_geomean(approx_post),
    .by = c(ESU, COMMON_POPULATION_NAME)
  ) |>
  mutate(
    `Q2.5.` = drop(quantile(geomean, 0.025)),
    `Q97.5.` = drop(quantile(geomean, 0.975))
  ) |>
  # Drop the approximate posterior samples so the results can be saved as a CSV
  select(-geomean)

write_csv(
  res2,
  here::here(res_dir, "Approximate-Population-Geomean-Quantiles.csv")
)

# CAVEATS: This approximation uses two major assumptions:
# 1. Posterior estimates of yearly population are log-normally distributed.
#    This appears to be a good approximation, judging by how close it is
#    possible to get to the provided quaniles.
# 2. Estimates of yearly population are independent. If I remember the model
#    structure this is unlikely to be true. I would expect that high
#    correlation among years would increase uncertainty in the geometric means.
#    This could be check by fitting a few populations and comparing the
#    approximate intervals provided here with intervals calculated from the
#    full posterior.
