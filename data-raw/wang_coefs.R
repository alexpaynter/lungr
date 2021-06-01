## This is a data entry script for the Wang (1993) coefficients.  Rather
##   than typing these into a spreadsheet and loading that, I've opted to just
##   use the tribble() function for data entry.

library(tibble)
library(rlang)
library(dplyr)

# wm = white male.  Table 2 from the manuscript.
wang_coefs_wm <- tibble::tribble(
    ~alpha, ~beta,
    # fvc:
    -0.024, 2.470,
    -0.018, 2.489,
    0.005, 2.443,
    0.017, 2.426,
    0.030, 2.407,

    0.009, 2.468,
    -0.061, 2.649,
    -0.175, 2.924,
    -0.219, 3.060,
    -0.079, 2.859,

    0.104, 2.591,
    0.253, 2.374,
    0.296, 2.316,


    # fev:
    -0.109, 2.252,
    -0.104, 2.270,
    -0.089, 2.257,
    -0.063, 2.197,
    -0.057, 2.212,

    -0.093, 2.324,
    -0.161, 2.512,
    -0.292, 2.843,
    -0.329, 2.983,
    -0.141, 2.709,

    0.062, 2.409,
    0.262, 2.099,
    0.251, 2.129,


    # fev/fvc
    -0.078, -0.248,
    -0.086, -0.220,
    -0.091, -0.199,
    -0.086, -0.206,
    -0.081, -0.209,

    -0.101, -0.147,
    -0.101, -0.133,
    -0.116, -0.085,
    -0.106, -0.087,
    -0.060, -0.155,

    -0.045, -0.178,
    0.008,  -0.272,
    -0.054, -0.170,

    # fef2575 - omitted for 6 and 7 year olds in the paper.
    NA_real_, NA_real_,
    NA_real_, NA_real_,
    0.264, 1.505,
    0.308, 1.443,
    0.290, 1.557,

    0.242, 1.738,
    0.165, 1.982,
    0.007, 2.396,
    0.014, 2.483,
    0.241, 2.163,

    0.503, 1.764,
    0.762, 1.368,
    0.678, 1.528
)

wang_coefs_wm <- wang_coefs_wm %>%
    dplyr::mutate(
        race = "white",
        sex = "male",
        age_lb = rep(6:18, times = 4),
        measure = rep(c('fvc', 'fev', 'fev_fvc', 'fef2575'), each = 13)
    ) %>%
    dplyr::select(measure, race, sex, age_lb, alpha, beta)






# wf = white female.  Table 3 from the manuscript.
wang_coefs_wf <- tibble::tribble(
    ~alpha, ~beta,
    # fvc:
    -0.013, 2.007,
    -0.062, 2.385,
    -0.055, 2.381,
    -0.039, 2.351,
    -0.068, 2.458,

    -0.120, 2.617,
    -0.174, 2.776,
    -0.061, 2.576,
    0.139, 2.208,
    0.210, 2.099,

    0.226, 2.097,
    0.214, 2.146,
    0.195, 2.179,

    # fev:
    -0.109, 1.949,
    -0.144, 2.243,
    -0.137, 2.239,
    -0.123, 2.222,
    -0.161, 2.364,

    -0.223, 2.558,
    -0.264, 2.709,
    -0.153, 2.535,
    0.046, 2.178,
    0.148, 2.008,

    0.181, 1.972,
    0.176, 1.992,
    0.152, 2.031,

    # fev/fvc
    -0.097, -0.055,
    -0.084, -0.132,
    -0.079, -0.152,
    -0.084, -0.128,
    -0.092, -0.097,

    -0.102, -0.061,
    -0.090, -0.067,
    -0.093, -0.040,
    -0.096, -0.026,
    -0.062, -0.093,

    -0.048, -0.120,
    -0.038, -0.154,
    -0.069, -0.096,

    # fef2575 - omitted for 6 and 7 year olds in the paper.
    NA_real_, NA_real_,
    NA_real_, NA_real_,
    0.247, 1.668,
    0.254, 1.710,
    0.195, 1.933,

    0.161, 2.091,
    0.185, 2.120,
    0.294, 1.976,
    0.450, 1.711,
    0.581, 1.486,

    0.654, 1.366,
    0.688, 1.290,
    0.520, 1.622
)

wang_coefs_wf <- wang_coefs_wf %>%
    dplyr::mutate(
        race = "white",
        sex = "female",
        age_lb = rep(6:18, times = 4),
        measure = rep(c('fvc', 'fev', 'fev_fvc', 'fef2575'), each = 13)
    ) %>%
    dplyr::select(measure, race, sex, age_lb, alpha, beta)





# bm = black male.  Table 4 from the manuscript.
wang_coefs_bm <- tibble::tribble(
    ~alpha, ~beta,
    # fvc:
    -0.088, 1.961,
    -0.040, 2.040,
    -0.094, 2.323,
    -0.074, 2.308,
    -0.110, 2.417,

    -0.138, 2.453,
    -0.224, 2.710,
    -0.342, 2.975,
    -0.337, 3.035,
    -0.226, 2.889,

    0.058, 2.425,
    0.148, 2.310,
    0.152, 2.341,

    # fev:
    -0.166, 1.723,
    -0.122, 1.846,
    -0.225, 2.271,
    -0.142, 2.059,
    -0.157, 2.117,

    -0.176, 2.166,
    -0.307, 2.548,
    -0.486, 2.962,
    -0.472, 3.010,
    -0.318, 2.789,

    0.074, 2.140,
    0.053, 2.223,
    0.130, 2.121,

    # fev/fvc
    -0.091, -0.152,
    -0.091, -0.153,
    -0.118, -0.104,
    -0.079, -0.218,
    -0.047, -0.303,

    -0.048, -0.263,
    -0.084, -0.162,
    -0.141, -0.018,
    -0.123, -0.050,
    -0.070, -0.140,

    0.018, -0.289,
    -0.095, -0.087,
    -0.041, -0.190,

    # fef2575 - omitted for 6 and 7 year olds in the paper.
    NA_real_, NA_real_,
    NA_real_, NA_real_,
    0.097, 1.544,
    0.255, 1.248,
    0.230, 1.428,

    0.256, 1.438,
    0.085, 1.936,
    -0.121, 2.476,
    -0.115, 2.536,
    0.170, 2.120,

    0.663, 1.299,
    0.505, 1.618,
    0.859, 1.053
)

wang_coefs_bm <- wang_coefs_bm %>%
    dplyr::mutate(
        race = "black",
        sex = "male",
        age_lb = rep(6:18, times = 4),
        measure = rep(c('fvc', 'fev', 'fev_fvc', 'fef2575'), each = 13)
    ) %>%
    dplyr::select(measure, race, sex, age_lb, alpha, beta)




# bf = black female.  Table 5 from the manuscript.
wang_coefs_bf <- tibble::tribble(
    ~alpha, ~beta,
    # fvc:
    -0.172, 2.117,
    -0.135, 2.132,
    -0.176, 2.362,
    -0.200, 2.452,
    -0.230, 2.571,

    -0.204, 2.526,
    -0.107, 2.342,
    -0.042, 2.294,
    0.105, 2.021,
    0.253, 1.787,

    0.111, 2.098,
    0.205, 1.930,
    -0.042, 2.423,

    # fev:
    -0.288, 2.182,
    -0.250, 2.158,
    -0.276, 2.295,
    -0.294, 2.330,
    -0.344, 2.507,

    -0.308, 2.460,
    -0.219, 2.312,
    -0.117, 2.196,
    0.041, 1.920,
    0.203, 1.662,

    0.129, 1.824,
    0.273, 1.547,
    -0.084, 2.259,

    # fev/fvc
    -0.109, 0.059,
    -0.104, -0.030,
    -0.103, -0.066,
    -0.097, -0.104,
    -0.120, -0.043,

    -0.089, -0.105,
    -0.115, -0.021,
    -0.051, -0.148,
    -0.063, -0.103,
    -0.043, -0.139,

    -0.022, -0.188,
    0.048, -0.342,
    -0.197, 0.145,

    # fef2575 - omitted for 6 and 7 year olds in the paper.
    NA_real_, NA_real_,
    NA_real_, NA_real_,
    -0.283, 2.990,
    0.025, 2.062,
    0.051, 2.028,

    0.078, 2.006,
    0.225, 1.804,
    0.418, 1.504,
    0.574, 1.257,
    0.599, 1.281,

    0.653, 1.175,
    0.713, 1.067,
    -0.209, 2.896
)

wang_coefs_bf <- wang_coefs_bf %>%
    dplyr::mutate(
        race = "black",
        sex = "female",
        age_lb = rep(6:18, times = 4),
        measure = rep(c('fvc', 'fev', 'fev_fvc', 'fef2575'), each = 13)
    ) %>%
    dplyr::select(measure, race, sex, age_lb, alpha, beta)



wang_coefs <- dplyr::bind_rows(
    wang_coefs_wm,
    wang_coefs_wf,
    wang_coefs_bm,
    wang_coefs_bf
)



usethis::use_data(wang_coefs, overwrite = TRUE)





# A few diagnostics I used to check my data.  Uncomment these if desired, or
#   if some changes are needed to the data.
# plot discover big mistakes:
library(ggplot2)
library(tidyr)
wang_coefs %>%
    dplyr::mutate(sr = paste0(sex, race)) %>%
    tidyr::pivot_longer(cols = c('alpha', 'beta'),
                        names_to = 'coef',
                        values_to = 'value') %>%
    ggplot(data = ., aes(x = age_lb, y = value, color = coef)) +
    geom_line() +
    scale_color_viridis_d(option = "magma", begin = 0.3, end = 0.7) +
    scale_x_continuous(breaks = seq(6,18,by= 2)) +
    facet_grid(rows = vars(measure), cols = vars(sr))

# looking at the coefficients and double checking for little ones:
wang_coefs %>% dplyr::select(measure, age_lb, alpha, beta) %>%
    View(.)
