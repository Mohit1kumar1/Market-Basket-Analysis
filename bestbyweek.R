best_selling_day <- data %>%
  mutate(Weekday = weekdays(InvoiceDate)) %>%
  group_by(Weekday, Description) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE, .groups ="drop")) %>%
  arrange(desc(Total_Quantity))

head(best_selling_day, 10)

c <- ggplot(best_selling_day, aes(x = factor(Weekday), y = Total_Quantity, fill = Description)) +
  geom_bar(stat = "identity") +
  labs(title = "Best Selling Product for Each Weekday",
       x = "Weekday",
       y = "Total Quantity Sold",
       fill = "Product") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(c)
