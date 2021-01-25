# Import data example for DEOHS Coders Group 


# Load packages, installing if needed
if (!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, readxl, ggplot2, lubridate, rstatix, ggpubr)

# set working directory (Will be project file)
work_dir <- file.path(getwd(), "erika_data_import")
setwd(work_dir)

# Define data directory, creating if needed
data_dir <- file.path(work_dir, "data")
##(!dir.exists(data_dir)) {
 ## dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
## }

# name and create output directory
output_dir <- file.path(work_dir, "output")
dir.create(output_dir, showWarnings = TRUE, recursive = TRUE)


# get xls file names - Need information from file names 
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
           timePST = force_tzs(time, tzones = "GMT", tzone_out = "US/Pacific"))
    
})


# assign dataframe names (corresponding to metadata)
names(df_list) <- df_names[["name"]]


# create diagnostic plot
# prepare data 
df<- bind_rows(df_list, .id = "letter") %>% 
  pivot_longer(cols = c("temp", "rh"), names_to = "parameter")
 
# create plot
figure1 <- ggplot(data = df, aes(x = timePST, y = value, color= letter)) + 
  geom_line() + 
  facet_wrap(~ parameter, ncol = 1, scales = "free_y") +
  theme_bw()


#Summary stats for temp + RH for entire experiment
summary.stats <- df %>%
  group_by(parameter) %>%
  get_summary_stats()%>%
  select(parameter, n, mean, median, iqr)

#making a visual table of summary stats
sum_table <- ggtexttable(summary.stats, rows = NULL, theme = ttheme("blank")) %>%
        tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 2)

#Creating combined plot using ggarrange #looks bad but room for improvement!
figure2 <- ggarrange(fig, sum_table, heights = c(3, 3, 0.3), ncol = 1, nrow = 3)



# save 2 figures as ".png", ".jpg", or ".pdf" (and more) using lapply and ggsave
# units can be specified as "in" or "cm"
ggsave(filename = file.path(output_dir,"fig1.png"), plot=figure1, 
       width = 8, height = 5, units = "in", dpi = "print")


ggsave(filename = file.path(output_dir,"fig2.png"), plot=figure2, 
         width = 8, height = 5, units = "in", dpi = "print")

