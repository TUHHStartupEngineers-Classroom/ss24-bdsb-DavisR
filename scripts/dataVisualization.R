library(tidyverse) # loads ggplot2
library(lubridate)


bike_orderlines_tbl <- read_rds("scripts/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# 1.0 Anatomy of a ggplot ----

# 1.1 How ggplot works ----

# Step 1: Format data ----

sales_by_year_tbl <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a year column
  select(order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Grouping by year, and summarizing sales
  group_by(year) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # € Format Text
  mutate(sales_text = scales::dollar(sales, 
                                     big.mark     = ".", 
                                     decimal.mark = ",", 
                                     prefix       = "", 
                                     suffix       = " €"))

sales_by_year_tbl


# Step 2: Plot ----
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales))

# Without piping 
ggplot(data = sales_by_year_tbl, 
       aes(x     = year, 
           y     = sales, 
           color = sales))
sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE)


# Data Manipulation
order_value_tbl <- bike_orderlines_tbl %>%
  
  select(order_id, order_line, total_price, quantity) %>%
  
  group_by(order_id) %>%
  summarize(
    total_quantity = sum(quantity),
    total_price    = sum(total_price)
  ) %>%
  ungroup()

# Scatter Plot
order_value_tbl %>%
  
  ggplot(aes(x = total_quantity, y = total_price)) +
  
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = FALSE)


# Data Manipulation
revenue_by_month_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(year_month = floor_date(order_date, "months") %>% ymd()) %>%
  
  group_by(year_month) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Line Plot
revenue_by_month_tbl %>%
  
  ggplot(aes(year_month, revenue)) +
  
  geom_line(size = 0.5, linetype = 1) +
  geom_smooth(method = "loess", span = 0.2)


# Data Manipulation
revenue_by_category_2_tbl <- bike_orderlines_tbl %>%
  
  select(category_2, total_price) %>%
  
  group_by(category_2) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup()

# Bar Plot
revenue_by_category_2_tbl %>%
  
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>%
  
  ggplot(aes(category_2, revenue)) +
  
  geom_col(fill = "#2c3e50") + 
  coord_flip()

# Histogram

bike_orderlines_tbl %>%
  
  distinct(model, price) %>%
  
  ggplot(aes(price)) +
  
  geom_histogram(bins = 25, fill = "blue", color = "white")

# Density
bike_orderlines_tbl %>%
  
  distinct(price, model, frame_material) %>%
  
  ggplot(aes(price, fill = frame_material)) +
  
  geom_density(alpha = 0.5) +
  # facet_wrap(~ frame_material, ncol = 1) +
  
  theme(legend.position = "bottom")


sales_by_year_tbl %>%
  
  # Canvas
  ggplot(aes(x = year, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE, color = "#d62dc6") +
  
  # Formatting
  expand_limits(y = 0) +
  # You can also type "red", "black" etc. for the colors
  scale_color_continuous(low    = "#95E1EA", high = "#2097A3", 
                         labels = scales::dollar_format(scale  = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_y_continuous(labels = scales::dollar_format(scale  = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M €")) +
  labs(
    title = "Revenue",
    subtitle = "Sales are trending up and to the right!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers showing year-over-year growth."
  )
  

library(ggthemes)
## DATA PREPARATION
sales_by_month_2015 <- bike_orderlines_tbl %>%
  
  # Selecting columns to focus on and adding a month column
  select(order_date, total_price) %>%
  mutate(year  = year(order_date)) %>% 
  mutate(month = month(order_date)) %>%
  
  filter(year == "2015") %>%
  
  # Grouping by month, and summarizing sales
  group_by(month) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # $ Format Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark    = ",",
                                     prefix          = "",  
                                     suffix          = " €"))

## PLOTTING
# Canvas
sales_by_month_2015 %>% 
  ggplot(aes(x = month, y = sales, color = sales)) +
  
  # Geometries 
  geom_line(size = 1) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = FALSE) +
  
  # Formatting
  expand_limits(y = 0) +
  scale_color_continuous(low = "red", high = "black",
                         labels = scales::dollar_format(scale = 1/1e6, 
                                                        prefix = "", 
                                                        suffix = "M €")) +
  scale_x_continuous(breaks = sales_by_month_2015$month, 
                     labels = month(sales_by_month_2015$month, label = T)) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M")) +
  labs(
    title = "Monthly sales (2015)",
    subtitle = "April is the strongest month!",
    x = "",
    y = "Sales (Millions)",
    color = "Rev (M €)",
    caption = "What's happening?\nSales numbers are dropping towards the end of the year."
  )  +  
  theme_economist() +
  theme(legend.position  = "right", 
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45))


# Data Manipulation

sales_by_year_category_1_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, total_price) %>%
  
  mutate(order_date = ymd(order_date)) %>%
  mutate(year = year(order_date)) %>%
  
  group_by(category_1, year) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Convert character vectors to factors
  # Arrange by year and revenue
  mutate(category_1 = fct_reorder2(category_1, year, revenue))

sales_by_year_category_1_tbl

# Uncover the factor levels (just for demonstration)
# sorted by years and the highest revenues
sales_by_year_category_1_tbl %>%
  mutate(category_1_num = as.numeric(category_1)) %>%
  arrange(category_1_num)


colors()

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = "slateblue")

# To RGB
col2rgb("slateblue")

col2rgb("#2C3E50")

# To HEX (this function should be provided to a geom)
rgb(44, 62, 80, maxColorValue = 255)

### Brewer. Comes with basic R.
#Primarly for discrete data.

# We can use those palletes by just calling their names (e.g. "Blues")
# Display the colors
RColorBrewer::display.brewer.all() 
# Get information
RColorBrewer::brewer.pal.info
# Get the HEX codes
RColorBrewer::brewer.pal(n = 8, name = "Blues")[1]

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = RColorBrewer::brewer.pal(n = 8, name = "Blues")[8])


### Viridis
viridisLite::viridis(n = 20)
# The last two characters indicate the transparency (e.g. FF makes it 100% transparent)

# Example
sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = viridisLite::viridis(n = 20)[10])


sales_by_year_category_1_tbl %>%
  
  # Put the aes color mapping here, to apply it to geom_line and geom_point
  ggplot(aes(year, revenue, color = category_1)) +
  
  # Or you could do it locally in each geom 
  # (aes mapping only necessary if you map it to a column)
  geom_line(size = 1) + # geom_line(aes(color = category_1))
  geom_point(color = "dodgerblue", size = 5)

sales_by_year_category_1_tbl %>%
  
  ggplot(aes(year, revenue)) +
  geom_col(aes(fill = category_1)) 






bike_orderlines_tbl <- read_rds("scripts/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

n <- 10
# Data Manipulation
top_customers_tbl <- bike_orderlines_tbl %>%
  
  # Select relevant columns
  select(bikeshop, total_price) %>%
  
  # Collapse the least frequent values into “other”
  mutate(bikeshop = as_factor(bikeshop) %>% fct_lump(n = n, w = total_price)) %>%
  
  # Group and summarize
  group_by(bikeshop) %>%
  summarize(revenue = sum(total_price)) %>%
  ungroup() %>%
  
  # Reorder the column customer_city by revenue
  mutate(bikeshop = bikeshop %>% fct_reorder(revenue)) %>%
  # Place "Other" at the beginning
  mutate(bikeshop = bikeshop %>% fct_relevel("Other", after = 0)) %>%
  # Sort by this column
  arrange(desc(bikeshop)) %>%
  
  # Add Revenue Text
  mutate(revenue_text = scales::dollar(revenue, 
                                       scale  = 1e-6, 
                                       prefix = "", 
                                       suffix = "M €")) %>%
  
  # Add Cumulative Percent
  mutate(cum_pct = cumsum(revenue) / sum(revenue)) %>%
  mutate(cum_pct_text = scales::percent(cum_pct)) %>%
  
  # Add Rank
  mutate(rank = row_number()) %>%
  mutate(rank = case_when(
    rank == max(rank) ~ NA_integer_,
    TRUE ~ rank
  )) %>%
  
  # Add Label text
  mutate(label_text = str_glue("Rank: {rank}\nRev: {revenue_text}\nCumPct: {cum_pct_text}"))



# Data Visualization
top_customers_tbl %>%
  
  # Canvas
  ggplot(aes(revenue, bikeshop)) +
  
  # Geometries
  geom_segment(aes(xend = 0, yend = bikeshop), 
               color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11],
               size  = 1) +
  
  geom_point(aes(size = revenue),
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  
  geom_label(aes(label = label_text), 
             hjust = "inward",
             size  = 3,
             color = RColorBrewer::brewer.pal(n = 11, name = "RdBu")[11]) +
  
  # Formatting
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M €")) +
  labs(
    title = str_glue("Top {n} Customers"),
    subtitle = str_glue(
      "Start: {year(min(bike_orderlines_tbl$order_date))}
               End:  {year(max(bike_orderlines_tbl$order_date))}"),
    x = "Revenue (M €)",
    y = "Customer",
    caption = str_glue("Top 6 customers contribute
                           52% of purchasing power.")
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )





# Select columns and filter categories
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
  
  select(bikeshop, category_1, category_2, quantity) %>%
  filter(category_1 %in% c("Mountain","Road")) %>% 
  
  # Group by category and summarize
  group_by(bikeshop, category_1, category_2) %>%
  summarise(total_qty = sum(quantity)) %>%
  ungroup() %>%
  
  # Add missing groups (not necessarily mandatory, but we'd get holes in the plot)
  # complete() creates NAs. We need to set those to 0.
  complete(bikeshop, nesting(category_1, category_2)) %>% 
  mutate(across(total_qty, ~replace_na(., 0))) %>%  
  
  # Group by bikeshop and calculate revenue ratio
  group_by(bikeshop) %>%
  mutate(pct = total_qty / sum(total_qty)) %>%
  ungroup() %>%
  
  # Reverse order of bikeshops
  mutate(bikeshop = as.factor(bikeshop) %>% fct_rev()) %>%
  # Just to verify
  mutate(bikeshop_num = as.numeric(bikeshop))

# Data Visualization
pct_sales_by_customer_tbl %>%
  
  ggplot(aes(category_2, bikeshop)) +
  
  # Geometries
  geom_tile(aes(fill = pct)) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1L)), 
            size = 3) +
  facet_wrap(~ category_1, scales = "free_x") +
  
  # Formatting
  scale_fill_gradient(low = "white", high = "#2C3E50") +
  labs(
    title = "Heatmap of Purchasing Habits",
    x = "Bike Type (Category 2)",
    y = "Customer",
    caption = str_glue(
      "Customers that prefer Road: 
        To be discussed ...
        
        Customers that prefer Mountain: 
        To be discussed ...")
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )
















# Challenge 1
library(tidyverse)
library(lubridate)
library(ggrepel) 

covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")


# Filter data for the specified countries and date range
selected_countries <- c("Germany", "United Kingdom", "United States", "France", "Spain")

filtered_data <- covid_data_tbl %>%
  filter(location %in% selected_countries,
         date >= "2020-01-01" & date <= "2022-04-20")

europe_data <- covid_data_tbl %>%
  filter(continent == "Europe",
         date >= "2020-01-01" & date <= "2022-04-20") %>%
  group_by(date) %>%
  summarise(total_cases = ifelse(sum(total_cases, na.rm = TRUE) == 0, NA, sum(total_cases, na.rm = TRUE)))


# Combine the Europe data with the original data
combined_data <- bind_rows(filtered_data, europe_data %>% mutate(location = "Europe"))

# Define custom colors for the lines
custom_colors <- c("red2", "skyblue2", "coral", "orange3", "yellow4", 'deeppink4')  # You can add more colors as needed

# Extract the last data points for Europe and United States
last_values <- combined_data %>%
  filter(location %in% c("Europe", "United States")) %>%
  group_by(location) %>%
  slice(n())  # Extract the last row for each location


# Plot cumulative Covid-19 cases over time
ggplot(combined_data, aes(x = date, y = total_cases, color = location)) +
  geom_line(size = 1.5, alpha = 0) +  # Increase size and reduce transparency
  geom_smooth(method = "loess", se = FALSE, size = 1, alpha = 0.2, span = 0.2) +  # Adjust span for reduced smoothing
  geom_label(data = last_values, aes(label = last_values$total_cases, color = location), 
             label.padding = unit(0.2, "lines"), 
             size = 3, 
             show.legend = FALSE,
             label=format(last_values$total_cases, big.mark = ".", scientific = FALSE)) + # Apply formatting to total_cases label
  labs(title = "Cumulative Covid-19 Cases Over Time",
       x = "Date",
       y = "Cumulative Cases",
       color = "Country") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # Set breaks and labels to every month
  scale_y_continuous(labels = function(x) ifelse(x >= 1e6, paste0(x/1e6, "M"), x),
                     limits = c(0, 200e6)) +
  theme_linedraw() +
  scale_color_manual(values = custom_colors) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), title = element_text(face = "bold", color = "#0b1562")) 




# Challenge 2
library(tidyverse)
library(lubridate)
library(ggrepel) 


# Load map data
world <- map_data("world")

# Read COVID-19 data
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Filter data for the specified date range
filtered_data <- covid_data_tbl %>%
  filter(date == "2024-04-28")

# Adjust country names
filtered_data <- filtered_data %>%
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  )) %>%
  distinct()


# Calculate mortality rate
filtered_data <- filtered_data %>%
  mutate(mortality_rate = ifelse(population == 0, NA, total_deaths / population))

# Merge COVID-19 data with map data
merged_data <- left_join(world, filtered_data, by = c("region" = "location"))


# Plot distribution of mortality rate
ggplot() +
  geom_map(data = merged_data, map = world, aes(map_id = region, fill = mortality_rate)) +
  expand_limits(x = world$long, y = world$lat) +
  scale_fill_gradient(name = "Mortality Rate", 
                      low = "#ff1d1d", 
                      high = "#460000", 
                      na.value = "grey", 
                      breaks = seq(0, 0.006, by = 0.002),  # Set breaks from 0 to 0.006 in steps of 0.001
                      labels = paste0(seq(0.00, 0.60, by = 0.20), "%")) +  # Set labels from 0% to 60%
  labs(title = "Distribution of Mortality Rate",
       fill = "Mortality Rate",
       map_id = "Country") +
  theme_void()