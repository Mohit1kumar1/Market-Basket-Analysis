library(dplyr)

best_selling_products <- data %>%
  group_by(Description) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(Total_Quantity))

head(best_selling_products, 50)


ggplot(best_selling_products[1:10, ], aes(x = reorder(Description, Total_Quantity), y = Total_Quantity)) +
    #geom_point()+
    geom_bar(stat = "identity", fill = "#9c9797", colour ="#0cf00c") +
    coord_flip() +
    labs(title = "Top 20 Best Selling Products", x = "Product", y = "Total Quantity Sold")   


p <- ggplot(best_selling_products[1:10, ], aes(x = factor(Description), y = Total_Quantity, fill = Description)) +
    geom_bar(stat = "identity") +
    labs(title = "Top 10 Best Selling Products",
         x = "Product",
         y = "Total Quantity Sold",
         fill = "Product") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to Plotly
ggplotly(p)

