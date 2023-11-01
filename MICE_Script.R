#Author: M. Dereviankin
#Date: 2-Nov-2023
#Title: General MICE Imputation Workflow

library(mice)
library(ggplot2)
library(gridExtra) 
library(tidyr)
library(dplyr)  
library(EnvStats)
library(umap)
library(plotly)

# MICE Imputation ---------------------------------------------------------

# Assume 'mydata' is your data frame and you've loaded it here
mydata <- read.csv('data.csv', stringsAsFactors = TRUE, na.strings = c("", NA))

# Convert character variables to factors (if they are not already)
mydata$SEQN <- as.factor(mydata$SEQN)
mydata$Gender <- as.factor(mydata$Gender)
mydata$Age <- as.factor(mydata$Age)
mydata$Ethnicity <- as.factor(mydata$Ethnicity)
mydata[, 41:59] <- lapply(mydata[, 41:59], as.factor) #making all demographical variables factors

# Perform MICE imputation
# Number of multiple imputations ('m') is set to 5 here, but this can be increased depending on the complexity of the data and the percentage of missingness.
set.seed(500)  # for reproducibility
imputed_data <- mice(mydata, method ="pmm", m = 5, maxit = 5)

# Review the imputed data (optional step for your understanding)
summary(imputed_data)

# Create the completed data by choosing one of the imputations
# Here, we are choosing the first set, but you might want to analyze all sets or use pooling methods.
completed_data <- complete(imputed_data, 1)  # or use 'action = "long"' for a long format of the dataset including all imputations

# Save the completed data to a CSV file
write.csv(completed_data, file = "my_completed_data.csv", row.names = FALSE)

# Note: The script assumes that you have already loaded your data into 'mydata'.
# Don't forget to replace placeholders like 'my_categorical_var1' and 'my_continuous_var1' with your actual column names.


# Visualization of new values ---------------------------------------------

# Specify the columns you want to check
columns_to_check <- 6:40

# Create a list to store individual plots for each variable
list_of_plots <- list()

# Go through each specified variable in the dataset
for (i in columns_to_check) {
  var_name <- names(mydata)[i]
  
  # We only want to plot variables that were imputed (i.e., had missing values)
  if (any(is.na(mydata[[var_name]]))) {
    
    # Extract the actual (non-missing) and imputed values for this variable
    actual_values <- mydata[[var_name]][!is.na(mydata[[var_name]])]
    imputed_values <- completed_data[[var_name]][is.na(mydata[[var_name]])]
    
    # Create a combined dataset for this variable, marking actual and imputed data
    combined_data <- data.frame(
      Value = c(actual_values, imputed_values),  # actual and imputed data
      Category = c(rep("Actual", length(actual_values)), rep("Imputed", length(imputed_values)))  # labels
    )
    
    # Create a plot for this variable
    p <- ggplot(combined_data, aes(x = Category, y = Value, color = Category)) +
      labs(title = paste("Comparison of Actual and Imputed Data for", var_name),
           y = "Values") +
      theme_minimal() +
      scale_y_continuous(trans = 'log10') +
      theme_classic() +
      scale_color_brewer(palette = "Dark2") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
      geom_jitter(
        shape = 21,
        alpha = 0.6,
        size = 0.1,
        width = 0.3) +
      stat_n_text(size = 1) +
      theme(legend.position = "none")  # since 'color' is self-explanatory here
    
    # Add this plot to the list
    list_of_plots[[var_name]] <- p
  }
}

# Now, you can view individual plots by calling them from the list. For example:
print(list_of_plots[["aldrin_Imputed_MICE"]])  # replace "variable_name_here" with the actual variable name

# If you want to view all plots, you can loop through the 'list_of_plots' and print each one.

# Now, arrange all the plots in a grid
number_of_columns <- 8  # or however many you'd like per row
grid_of_plots <- do.call("grid.arrange", c(list_of_plots, ncol = number_of_columns))

# Note: This script assumes that 'mydata' and 'completed_data' are already defined in your environment.
# 'completed_data' should be the dataset obtained after the MICE imputation.


# Validation of Testing MICE ----------------------------------------------
# 1. Randomly select 5% of the data for each column that has missing values
# 1. Randomly select 5 values from each column that has missing values
set.seed(123)  # for reproducibility
validation_indices <- lapply(completed_data[, columns_to_check], function(col) {
  non_missing_indices <- which(!is.na(col))
  sample(non_missing_indices, size = min(5, length(non_missing_indices)))
})

# Print the number of sampled indices for each column
cat("Number of sampled indices for each column:\n")
sapply(validation_indices, length)

# 2. Store the actual values and replace them with NA
actual_values_list <- lapply(seq_along(validation_indices), function(i) {
  col <- columns_to_check[i]
  indices <- validation_indices[[i]]
  actual_values <- completed_data[indices, col]
  completed_data[indices, col] <- NA
  return(actual_values)
})

# 3. Run MICE to impute these missing values
imputed_data_validation <- mice(completed_data, method = "pmm", m = 5, maxit = 5)
completed_data_validation <- complete(imputed_data_validation, 1)

# 4. Compare the imputed values with the actual values
comparison_list <- lapply(seq_along(validation_indices), function(i) {
  col <- columns_to_check[i]
  indices <- validation_indices[[i]]
  data.frame(
    Actual = actual_values_list[[i]],
    Imputed = completed_data_validation[indices, col]
  )
})

# Calculate percent error for each column
percent_errors <- list()

for (idx in seq_along(columns_to_check)) {
  col <- columns_to_check[idx]
  col_name <- names(completed_data)[col]
  
  # Extract the actual values for the selected rows
  actual_values <- completed_data[validation_indices[[idx]], col]
  
  # Extract the imputed values for the selected rows across all imputed datasets
  imputed_values_all <- sapply(1:5, function(m) complete(imputed_data_validation, m)[validation_indices[[idx]], col])
  
  # Average the imputed values
  imputed_values <- rowMeans(imputed_values_all, na.rm = TRUE)
  
  # Calculate the percent error
  percent_error <- abs(imputed_values - actual_values) / abs(actual_values) * 100
  
  # Store the results in a data frame
  df <- data.frame(Actual = actual_values, Imputed = imputed_values, Error = percent_error)
  comparison_list[[idx]] <- df
  percent_errors[[idx]] <- percent_error
}

# 5. Visualize and summarize the results
plots_list <- list()
for (i in seq_along(comparison_list)) {
  col_name <- names(completed_data)[columns_to_check[i]]
  df <- comparison_list[[i]]
  
  # Print the number of points in each ggplot graphic
  cat(paste("Number of points in the graphic for column", col_name, ":", nrow(df)), "\n")
  
  # Calculate R^2 value
  model <- lm(Imputed ~ Actual, data = df)
  r_squared <- summary(model)$r.squared
  
  p <- ggplot(df, aes(x = Actual, y = Imputed)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Line of best fit
    labs(title = paste("Actual vs. Imputed for", col_name, "\n", "R^2 =", sprintf("%.3f", r_squared)),
         x = "Actual Value",
         y = "Imputed Value") +
    theme_minimal()
  
  plots_list[[col_name]] <- p
}

# Display plots
number_of_columns <- 8  # or however many you'd like per row
grid_of_plots <- do.call("grid.arrange", c(plots_list, ncol = number_of_columns))

# Summary table
# Create a long-format summary table
summary_table <- do.call(rbind, lapply(seq_along(comparison_list), function(i) {
  df <- comparison_list[[i]]
  col_name <- names(completed_data)[columns_to_check[i]]
  df$Variable <- col_name
  return(df)
}))

# Rearrange columns for better readability
summary_table <- summary_table[, c("Variable", "Actual", "Imputed", "Error")]

# Print the summary table
print(summary_table)

# UMAP of Imputed Dataset -------------------------------------------------

# Assuming 'complete_data' is your dataframe after imputation

# Create a new dataframe excluding columns with any missing values
completed_umap_df <- completed_data[, colSums(is.na(completed_data)) == 0]

# First, we'll only select the columns from 8 to 33
selected_columns <- completed_umap_df[, 6:40]

# Now, we calculate the column sums for these selected columns
column_sums <- colSums(selected_columns, na.rm = TRUE)  # This also removes any NA values from the sum

# Next, we'll divide each element by the respective column sum and multiply by 100 to get percentage
normalized_columns <- sweep(selected_columns, 2, column_sums, FUN = "/") * 100

# Now, we need to replace the original columns with our new normalized columns
completed_umap_df[, 6:40] <- normalized_columns

# At this point, 'complete_umap_df' has the columns 8 through 33 normalized as percentage totals of their original sums.

# Now, 'complete_umap_df' is a dataframe with only the columns that have no missing values at all.

#Run to the UMAP

df <- as.matrix(completed_umap_df[, 6:40]) #remember to only set columns which will be used in the UMAP calculation

set.seed(123)
custom.config = umap.defaults  #manual settings
custom.config$n_components = 3
custom.config$n_neighbors = 13
custom.config$min_dist = 0.1

data_umap <- umap(df, n_components = custom.config$n_components,
                  n_neighbors = custom.config$n_neighbors,
                  min_dist = custom.config$min_dist)

data_raw  <- data_umap$layout %>% as.data.frame()

data_plot <- cbind(data_raw, completed_umap_df)


#running graphics script. Make sure to change color and size to appropriate labels
#This is the visual that will get manipulated playing around with the UMAP parameters

UMAP_plot <- ggplot(data_plot, aes(x = V1, y = V3)) +
  labs(title = '2D UMAP') +
  geom_point(alpha = 0.7, shape =19 ) + 
  theme_light() +
  theme(axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 17),
        legend.text = element_text(size=15)) +
  scale_radius(range=c(4, 10))

colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', 'darkolivegreen', 'blue')

UMAP_2D <- plot_ly(data_plot, x= ~V1, y= ~V2,  Type = "scatter", mode = "markers", text = ~paste('ID:', Sample_ID))

UMAP_3D <- plot_ly(data_plot, x= ~V1, y= ~V2, z= ~V3, Type="scatter3d", mode="markers", text = ~paste('ID:', Sample_ID))

UMAP_3D <- UMAP_3D %>% layout(title = '3D UMAP',
                              paper_bgcolor = 'grey',
                              plot_bgcolor = 'black')

print(UMAP_plot)
print(UMAP_2D)
print(UMAP_3D)

write.csv(data_plot, "data_plot_Revised.csv")