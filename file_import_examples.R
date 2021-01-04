# File import examples

# Load packages, installing if needed
if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readxl)

# Define functions for extracting metadata
extract_metadata_sep <- function(path, range, sep = ":") {
    read_xls(path = path, range = range, col_names = FALSE) %>%
        separate(col = 1, into = c('var', 'val'), sep = sep, extra = "merge")
}

extract_metadata <- function(path, range) {
  read_xls(path = path, range = range, col_names = FALSE) %>%
        rename("var" = "...1", "val" = "...2")
}

# Define data directory, creating if needed
data_dir <- "data"
if (!dir.exists(data_dir)) {
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
}

# Define file path
xls_file <- file.path(data_dir, "NaCL_25C_incubator test.xls")

# Get metadata
df_metadata <- rbind(extract_metadata_sep(xls_file, "A2"),
                     extract_metadata(xls_file, "A5:B6"), 
                     extract_metadata(xls_file, "D5:E6"),
                     extract_metadata(xls_file, "A8:B9"),
                     extract_metadata(xls_file, "A11:B14"), 
                     extract_metadata(xls_file, "D11:E14"),
                     extract_metadata(xls_file, "A21:B25"), 
                     extract_metadata(xls_file, "D21:E25")) %>% 
    pivot_wider(names_from = "var", values_from = "val")


# Get Temp and RH data
df <- read_xls(xls_file, skip = 26)

# Alternate way to read metadata from XLS file using readxl and data.table
pacman::p_load(readxl, data.table)
df_meta_alt <- read_xls(xls_file, n_max = 25)
df_meta_alt_1 <- data.frame(df_meta_alt[, 1:2])
df_meta_alt_1[1, ] <- strsplit(df_meta_alt_1[1, 1], " on:")[[1]]
setnames(df_meta_alt_1, names(df_meta_alt_1), c('var', 'val'))
df_meta_alt_2 <- data.frame(df_meta_alt[, 4:5])
setnames(df_meta_alt_2, names(df_meta_alt_2), c('var', 'val'))
df_meta_alt <- rbind(df_meta_alt_1, df_meta_alt_2)
df_meta_alt <- transpose(df_meta_alt[complete.cases(df_meta_alt),], 
                         make.names = TRUE)
