############################################################
# Load tidyverse (includes dplyr, tidyr, ggplot2, etc.)
############################################################
install.packages("tidyverse")
library(tidyverse)
install.packages("conflicted")

# mtcars is a built-in dataset in R
# It contains fuel consumption and car specifications

head(mtcars)


# In mtcars, car names are stored as row names
# Tidy data prefers everything to be in columns
# So we move row names into a proper column called "car"
#%>% is called the pipe operator and is part of magrittr package
#It states take the result from the left and pass it into the first argument of the function on the right.”
mtcars_clean <- mtcars %>%
  rownames_to_column(var = "car")

head(mtcars_clean)


# Use tidyr to reshape the data (Wide → Long)
# Right now, mpg, hp, and wt are separate columns
# We will pivot them into long format
# This makes it easier to analyze multiple variables together

mtcars_long <- mtcars_clean %>%
  pivot_longer(
    cols = c(mpg, hp, wt),   # Columns we want to reshape
    names_to = "variable",   # New column that stores column names
    values_to = "value"      # New column that stores values
  )

head(mtcars_long)


# Use dplyr to analyze the reshaped data

# Now we calculate the average value of each variable
# grouped by number of cylinders (cyl)

summary_table <- mtcars_long %>%
  group_by(cyl, variable) %>%     # Group by cylinders + variable
  summarise(
    average = mean(value),        # Calculate mean
    .groups = "drop"              # Remove grouping after summarise
  ) %>%
  arrange(cyl)                    # Sort by cylinder number

summary_table


# Filter and mutate 

# Let's create a new variable:horsepower per unit weight (hp/wt)
# Then filter cars with more than 6 cylinders

high_power_cars <- mtcars_clean %>%
  mutate(
    hp_per_wt = hp / wt   # Create new calculated column
  ) %>%
  filter(cyl > 6)         # Keep cars with more than 6 cylinders

high_power_cars
