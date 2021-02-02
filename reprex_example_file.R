
# reprex example

# Load pacman into memory, installing as needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

# Load the other packages, installing as needed.
pacman::p_load(palmerpenguins, dplyr, ggplot2)

# here's my data
df <- penguins
df

# average bill length - I don't get it, I know there is data there - why is it NA?!
mean(df$bill_length_mm)

# plot bills
ggplot(data = df, aes(x = bill_length_mm)) + geom_histogram(bins = 30)

