library(kableExtra)

weight_ut <- read.csv("C:/Users/jesus/Box/Energy/net_metering/data/clean data/weight_table_TEP_20_top_utilities14_Apr_2025.csv")

kable(weight_ut, "latex")

weight_cov <- read.csv("C:/Users/jesus/Box/Energy/net_metering/data/clean data/variable_table_synthetic_weights_TEP_20_valid_utilities_22_Apr_2025.csv")

kable(weight_cov, "latex")
