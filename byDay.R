library(lubridate)
library(dplyr)

best_selling_time <- data %>%
  mutate(Hour = hour(InvoiceDate)) %>%
  group_by(Hour, Description) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE), .groups = "drop") %>%
  filter(Total_Quantity > 0) %>%
  arrange(desc(Total_Quantity))

head(best_selling_time, 50)
tail(best_selling_time, 50)
 
range(best_selling_time$Hour)

# Get the best-selling product for each hour
best_selling_per_hour <- best_selling_time %>%
  group_by(Hour) %>%
  slice_max(order_by = Total_Quantity, n = 1) # Selects the top product per hour

head(best_selling_per_hour, 50)
# Plot
b <- ggplot(best_selling_per_hour, aes(x = factor(Hour), y = Total_Quantity, fill = Description)) +
  geom_bar(stat = "identity") +
  labs(title = "Best Selling Product for Each Hour",
       x = "Hour of the Day",
       y = "Total Quantity Sold",
       fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(b)

# Get the top 5 best-selling products for each hour
top_5_per_hour <- best_selling_time %>%
  group_by(Hour) %>%
  slice_max(order_by = Total_Quantity, n = 5) %>%
  ungroup()

head(top_5_per_hour, 50)


# Plot
a <- ggplot(top_5_per_hour, aes(x = reorder(Description, Total_Quantity), 
                           y = Total_Quantity, fill = Description)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ Hour, scales = "free_y") +  # Creates a grid of plots by Hour
  coord_flip() +  # Flips bars for better readability
  labs(title = "Top 5 Best-Selling Products for Each Hour",
       x = "Product",
       y = "Total Quantity Sold") +
  theme_minimal()

ggplotly(a)
