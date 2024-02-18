# Main script to run the entire analysis
# Step 1: Load and clean the data
source("clean_data.R")

# Step 2: Perform data visualization
source("data_visualization.R")

# Step 3: Conduct statistical analysis
source("statistical_modeling.R")

# Step 4: Execute advanced analysis techniques
source("bootstrap_analysis.R")

# Print a message upon successful completion of all scripts
cat("Completed successfully.\n")
