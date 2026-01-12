## code to prepare `DATASET` dataset goes here

# rsa_simulated <- read.csv("data-raw/rsa_simulated_subset.csv")
# fixed non-ascii strings in TANF desc variable
rsa_simulated <- read.csv("data-raw/rsa_simulated_subset_fixed.csv")


scores_simulated <- read.csv("data-raw/scores_simulated_subset.csv")


usethis::use_data(rsa_simulated, overwrite = TRUE)

usethis::use_data(scores_simulated, overwrite = TRUE)
