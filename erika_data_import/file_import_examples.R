# Import data example for DEOHS Coders Group 


# Load packages, installing if needed
if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readxl, ggplot2)

# set working directory (Erika to modify)
work_dir <- file.path(getwd(), "erika_data_import")
setwd(work_dir)

# Define data directory, creating if needed 
data_dir <- file.path(work_dir, "data")
##if (!dir.exists(data_dir)) {
 ## dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
## }

# name and create output directory
output_dir <- file.path(work_dir, "output")
dir.create(output_dir, showWarnings = TRUE, recursive = TRUE)


# get xls file names 
# (Erika: do you need any information in the file names? 
# Modify file names to label dataframes?)
file_names <- list.files(path = file.path(data_dir))
df_names <- tibble(file_names) %>% 
  separate(file_names, sep = "_|\\.", 
           into = c("letter", "time", "instrument", "date"), 
           extra = "drop") %>% 
  mutate(name = paste(letter, time, sep = "_"))

# Define functions for extracting metadata
extract_metadata_sep <- function(path, range, sep = ":") {
    read_xls(path = path, range = range, col_names = FALSE) %>%
        separate(col = 1, into = c('var', 'val'), sep = sep, extra = "merge")
}

extract_metadata <- function(path, range) {
  read_xls(path = path, range = range, col_names = FALSE) %>%
        rename("var" = "...1", "val" = "...2")
}

metadata_list <- lapply(file_names, function(i){

  # build file name
  fn <- file.path(data_dir, i)
  
  # Get metadata
  rbind(extract_metadata_sep(fn, "A2"),
        extract_metadata(fn, "A5:B6"), 
        extract_metadata(fn, "D5:E6"),
        extract_metadata(fn, "A8:B9"),
        extract_metadata(fn, "A11:B14"), 
        extract_metadata(fn, "D11:E14"),
        extract_metadata(fn, "A21:B25"), 
        extract_metadata(fn, "D21:E25")) %>% 
    pivot_wider(names_from = "var", values_from = "val")
})

# assign metadata names
names(metadata_list) <- df_names[["name"]]


# Get Temp and RH data
df_list <- lapply(file_names, function(i){ 
  
  # build file name
  fn <- file.path(data_dir, i) 
  
  # read files
  read_xls(fn, skip = 26) %>% 
    select(n = `No.`, time = Time, temp = `Temperature°C`, rh = `Humidity%RH`) %>% 
    mutate(time = as.POSIXct(time, tz = "GMT"), 
           timePST =format(time, tz= "US/Pacific", usetz= TRUE), 
           timePST= as.POSIXct(timePST, tz= "US/Pacific")) 
  })

# assign dataframe names (corresponding to metadata)
names(df_list) <- df_names[["name"]]


# create diagnostic plot
 
# prepare data 
df<- bind_rows(df_list, .id = "letter") %>% 
  pivot_longer(cols = c("temp", "rh"), names_to = "parameter") 
    
# create plot
fig <- ggplot(data = df, aes(x = time, y = value, color= letter)) + 
      geom_line() + 
      facet_wrap(~ parameter, ncol = 1, scales = "free_y") +
      theme_bw()
  

# save figures as ".png", ".jpg", or ".pdf" (and more) using lapply and ggsave
# units can be specified as "in" or "cm"
ggsave(filename = file.path(output_dir,"fig.png"), plot=fig, 
         width = 8, height = 5, units = "in", dpi = "print")
