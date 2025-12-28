library(tidyverse)

# Loading the data
happiness_data <- read.csv("World-happiness-report-2024.csv")


# Data inspection
head(happiness_data)
str(happiness_data) 
summary(happiness_data)


# Data cleaning
clean_data <- happiness_data %>%
  # 1. Renaming the columns to simpler names
  rename(
    Country = Country.name,
    Region = Regional.indicator,
    Score = Ladder.score,
    GDP = Log.GDP.per.capita,
    Social_Support = Social.support,
    Life_Expectancy = Healthy.life.expectancy,
    Freedom = Freedom.to.make.life.choices,
    Corruption = Perceptions.of.corruption
  ) %>%
  
  # 2. Selecting only the columns we will use in the analysis
  select(Country, Region, Score, GDP, Social_Support, Life_Expectancy, Freedom, Generosity, Corruption) %>%
  
  # 3. Droppping rows with missing values
  drop_na()



get_happiness_category <- function(score) {
  if (score >= 7) {
    return("High Happiness")
  } else if (score >= 5) {
    return("Medium Happiness")
  } else {
    return("Low Happiness")
  }
}


get_range <- function(x) {
  if(is.numeric(x)) {
    return(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Function 3: Normalize a column (Min-Max Scaling)
# This scales values between 0 and 1, useful for comparing variables with different units (like GDP vs Freedom)

normalize_column <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

# Applying the functions to the dataset for demonstration
clean_data$Score_Normalized <- normalize_column(clean_data$Score)
score_range <- get_range(clean_data$Score)

print(paste("The range of Happiness Scores is:", round(score_range, 2)))

# --- NEW FUNCTIONS END --

# preparing the data for ploting
clean_data$Category <- sapply(clean_data$Score, get_happiness_category)
clean_data$Region <- as.factor(clean_data$Region)


# PLOT 1: Average Happiness by Region

region_summary <- clean_data %>%
  group_by(Region) %>%
  summarise(Average_Score = mean(Score))

plot1 <- ggplot(region_summary, aes(x = reorder(Region, Average_Score), y = Average_Score)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() + # Flip coordinates to make labels readable
  labs(
    title = "Average Happiness Score by Region",
    x = "Region",
    y = "Average Happiness Score"
  ) +
  theme_minimal()
print(plot1)


# PLOT 2: GDP vs Happiness)

plot2 <- ggplot(clean_data, aes(x = GDP, y = Score)) +
  geom_point(aes(color = Region), alpha = 0.7) + 
  geom_smooth(method = "lm", color = "black", se = FALSE) + 
  labs(
    title = "Impact of Wealth on Happiness",
    subtitle = "Strong positive correlation between GDP and Happiness Score",
    x = "Log GDP per Capita",
    y = "Happiness Score"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot2)


plot3 <- ggplot(clean_data, aes(x = Corruption, y = Score)) +
  # 1. Same point style (colored by region, alpha 0.7)
  geom_point(aes(color = Region), alpha = 0.7) + 
  
  # 2. Same trend line style (Linear Model, black line, no grey confidence ribbon)
  geom_smooth(method = "lm", color = "black", se = FALSE) + 
  
  # 3. Labels
  labs(
    title = "Impact of Corruption on Happiness",
    subtitle = "Negative correlation: Higher corruption perceptions link to lower happiness",
    x = "Perception of Corruption (0=Low, 1=High)",
    y = "Happiness Score"
  ) +
  
  theme_minimal() +
  theme(legend.position = "bottom")

print(plot3)

# Formula to find the strongest varible on happiness

numeric_cols <- clean_data %>% 
  select(Score, GDP, Social_Support, Life_Expectancy, Freedom, Generosity, Corruption)

cor_matrix <- cor(numeric_cols)

score_correlations <- cor_matrix[, "Score"]

score_correlations <- score_correlations[names(score_correlations) != "Score"]

ranking_df <- data.frame(
  Variable = names(score_correlations),
  Correlation = score_correlations
)

# Printing the results sorted from Strongest to Weakest
print("--- CORRELATION RANKING ---")
print(ranking_df %>% arrange(desc(Correlation)))


# PLOT 4: Visualizing Variable Importance
plot4 <- ggplot(ranking_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  # Create the bars
  geom_bar(stat = "identity", fill = "steelblue") +
  
  
  coord_flip() +
  
  
  labs(
    title = "Strongest Factor on Happiness",
    subtitle = "Correlation coefficients of variables with Happiness Score",
    x = "Variable",
    y = "Correlation Strength"
  ) +
  

  theme_minimal()

# Display the plot
print(plot4)