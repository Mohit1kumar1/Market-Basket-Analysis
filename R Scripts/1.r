install.packages("jsonlite")
install.packages("htmlwidgets")
install.packages("openxlsx")
install.packages("arules")
install.packages(c("seriation", "vcd", "igraph", "scatterplot3d", "ggplot2", 
                   "ggraph", "tibble", "tidyr", "dplyr", "DT", "plotly", "visNetwork"))

install.packages("aruleViz")
install.packages("C:/Users/mh319/Downloads/arulesViz_1.5.3.tar.gz", repos = NULL, type = "source")

library(openxlsx, help, pos = 2, lib.loc = NULL)
library(arules, help, pos = 2, lib.loc = NULL)
library(arulesViz, help, pos = 2, lib.loc = NULL)
library(ggplot2, help, pos = 2, lib.loc = NULL)


retail_data <- read.xlsx("C:\\Users\\mh319\\Desktop\\VS Code\\Datasets\\online+retail\\Online Retail.xlsx")
head(retail_data)

# Remove missing CustomerID or Description
retail_data <- retail_data[!is.na(retail_data$Description) & retail_data$Quantity > 0, ]

# Select relevant columns
retail_data <- retail_data[, c("InvoiceNo", "Description")]
library(dplyr)
transactions <- retail_data %>%
  group_by(InvoiceNo) %>%
  summarise(items = paste(Description, collapse = ", ")) %>%
  ungroup()

  # Write to a temporary file for arules
write.csv(transactions$items, "C:\\Users\\mh319\\Desktop\\VS Code\\Datasets\\online+retail\\transactions.csv", row.names = FALSE, quote = FALSE)

library(arules)

# Read the transaction data
transaction_data <- read.transactions("C:\\Users\\mh319\\Desktop\\VS Code\\Datasets\\online+retail\\transactions.csv", format = "basket", sep = ",")

# Inspect the transaction data
summary(transaction_data)

 Apply the Apriori algorithm
rules <- apriori(transaction_data, 
                 parameter = list(supp = 0.01, conf = 0.8))

# View the rules
inspect(rules)

install.packages("plotly")  
install.packages("dplyr")  
library(plotly, help, pos = 2, lib.loc = NULL)
library(dplyr, help, pos = 2, lib.loc = NULL)

# Convert rules to a data frame
rules_df <- as(rules, "data.frame")

# Check the structure of the rules_df
head(rules_df)

# 3D scatter plot with Plotly
plot <- plot_ly(
  data = rules_df,
  x = ~support,
  y = ~confidence,
  z = ~lift,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 5, color = ~lift, colorscale = "Viridis", showscale = TRUE)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Support"),
      yaxis = list(title = "Confidence"),
      zaxis = list(title = "Lift")
    ),
    title = "3D Visualization of Association Rules"
  )

library(htmlwidgets, help, pos = 2, lib.loc = NULL)
saveWidget(plot, file = "C:\\Users\\mh319\\Desktop\\VS Code\\Datasets\\online+retail\\Association Rule.html")
