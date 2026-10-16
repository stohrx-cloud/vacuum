# Load necessary libraries
library(dplyr)
library(lubridate)
library(broom)
library(kableExtra)

# Set seed for reproducibility
set.seed(123)

# Parameters
num_customers <- 100
num_products <- 10
num_purchases <- 500

# Generate customer IDs
customer_ids <- paste0("C", 1:num_customers)

# Generate product names
product_names <- paste("Product", 1:num_products)

# Define customer segments
customer_segments <- c("Budget", "Premium", "Value")
segment_assignments <- sample(customer_segments, num_customers, replace = TRUE)

# Define additional customer heterogeneity variables
life_experiences <- sample(c("Travel", "Education", "Career", "Family"), num_customers, replace = TRUE)
functional_needs <- sample(c("Quality", "Price", "Convenience"), num_customers, replace = TRUE)
self_identity <- sample(c("Trendy", "Practical", "Luxury"), num_customers, replace = TRUE)
marketing_activities <- sample(c("Email", "Social Media", "In-Store", "Events"), num_customers, replace = TRUE)

# Define variables for "All Customers Change"
lifecycle_stages <- sample(c("Student", "Young Professional", "Parent", "Retiree"), num_customers, replace = TRUE)
purchase_frequencies <- sample(c("Occasional", "Regular", "Frequent"), num_customers, replace = TRUE)
brand_loyalties <- sample(c("Loyal", "Switching", "New"), num_customers, replace = TRUE)
recent_changes <- sample(c("Yes", "No"), num_customers, replace = TRUE)

# Define variables for "All Competitors React"
competitor_pricing <- sample(c("Lower", "Similar", "Higher"), num_products, replace = TRUE)
competitor_promotions <- sample(c("Discount", "Bundle", "Loyalty Program"), num_products, replace = TRUE)
competitive_response <- sample(c("Yes", "No"), num_purchases, replace = TRUE)
market_trends <- sample(c("Trending", "Stable", "Declining"), num_purchases, replace = TRUE)

# Define variables for "All Resources are Limited"
marketing_budget <- sample(c("Low", "Medium", "High"), length(customer_segments), replace = TRUE)
promotion_availability <- sample(c("Available", "Not Available"), num_products, replace = TRUE)
stock_availability <- sample(c("In Stock", "Out of Stock"), num_products, replace = TRUE)
resource_allocation <- sample(c("Evenly", "Weighted towards Premium", "Weighted towards Budget"), 1)

# Create a synthetic purchases dataset
purchases <- data.frame(
  CustomerID = sample(customer_ids, num_purchases, replace = TRUE),
  Product = sample(product_names, num_purchases, replace = TRUE),
  Quantity = sample(1:5, num_purchases, replace = TRUE),
  Price = round(runif(num_purchases, 5, 100), 2),
  PurchaseDate = sample(seq.Date(from = as.Date("2023-01-01"),
                                 to = as.Date("2023-12-31"),
                                 by = "day"), 
                        num_purchases, replace = TRUE)
)

# Assign customer segments and heterogeneity variables to the purchases dataset
purchases$CustomerSegment <- segment_assignments[match(purchases$CustomerID, customer_ids)]
purchases$LifeExperience <- life_experiences[match(purchases$CustomerID, customer_ids)]
purchases$FunctionalNeed <- functional_needs[match(purchases$CustomerID, customer_ids)]
purchases$SelfIdentity <- self_identity[match(purchases$CustomerID, customer_ids)]
purchases$MarketingActivity <- marketing_activities[match(purchases$CustomerID, customer_ids)]

# Assign variables related to "All Customers Change"
purchases$LifecycleStage <- lifecycle_stages[match(purchases$CustomerID, customer_ids)]
purchases$PurchaseFrequency <- purchase_frequencies[match(purchases$CustomerID, customer_ids)]
purchases$BrandLoyalty <- brand_loyalties[match(purchases$CustomerID, customer_ids)]
purchases$RecentChange <- recent_changes[match(purchases$CustomerID, customer_ids)]

# Assign variables related to "All Competitors React"
purchases$CompetitorPricing <- competitor_pricing[match(purchases$Product, product_names)]
purchases$CompetitorPromotions <- competitor_promotions[match(purchases$Product, product_names)]
purchases$CompetitiveResponse <- competitive_response
purchases$MarketTrends <- market_trends

# Assign variables related to "All Resources are Limited"
purchases$MarketingBudget <- marketing_budget[match(purchases$CustomerSegment, customer_segments)]
purchases$PromotionAvailability <- promotion_availability[match(purchases$Product, product_names)]
purchases$StockAvailability <- stock_availability[match(purchases$Product, product_names)]
purchases$ResourceAllocation <- resource_allocation

# Create the RepeatPurchase variable (1 = repeat purchase, 0 = no repeat purchase)
purchases$RepeatPurchase <- sample(c(0, 1), num_purchases, replace = TRUE)

# Perform logistic regression
logistic_model <- glm(RepeatPurchase ~ CustomerSegment + LifeExperience + FunctionalNeed + 
                        SelfIdentity + MarketingActivity + LifecycleStage + 
                        PurchaseFrequency + BrandLoyalty + RecentChange + 
                        CompetitorPricing + CompetitorPromotions + 
                        MarketingBudget + PromotionAvailability + StockAvailability,
                      data = purchases, family = binomial)

# Summary of the logistic regression
model_summary <- summary(logistic_model)

# Create a tidy data frame of the results
logistic_results <- tidy(logistic_model)

# Display the logistic regression results as a table
kable(logistic_results, digits = 3, caption = "Logistic Regression Results") %>%
  kable_styling(full_width = F)