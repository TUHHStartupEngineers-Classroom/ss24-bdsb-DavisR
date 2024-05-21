library(RSQLite)
library(httr)
library(glue)
library(jsonlite)
library("tidyverse")
library(rvest)




# get the URL for the wikipedia page with all S&P 500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the S&P 500 table using rvest
library(rvest)
sp_500 <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # Get the nodes with the id
  html_nodes(css = "#constituents") %>%
  # html_nodes(xpath = "//*[@id='constituents']"") %>% 
  # Extract the table and turn the list into a tibble
  html_table() %>% 
  .[[1]] %>% 
  as_tibble()
sp_500


# Second example
url  <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- url %>% 
  read_html()

rank <- html |>  
  html_elements(css = ".cli-title .ipc-title__text") |>  
  html_text() |>  
  parse_number()


title <- html %>% 
  html_elements(css = ".cli-title .ipc-title__text") |> 
  html_text() |> 
  str_remove("^\\d*\\. ") # Remove numbers, dot and space at the beginning


rating <- html %>% 
  html_elements(css = ".ratingGroup--imdb-rating") |> 
  html_text() |> 
  str_remove("\\(.*\\)") |> 
  str_squish() |> 
  as.numeric()

# Merge
imdb_tbl <- tibble(rank, title, rating)

imdb_tbl





bike_data_lst <- fromJSON("bike_data.json")
# Open the data by clicking on it in the environment or by running View()
bike_data_lst %>%
  purrr::pluck("productDetail", "variationAttributes", "values", 1, "displayValue")



# WEBSCRAPING ----

# 1.0 LIBRARIES ----
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs


# 1.2 COLLECT PRODUCT CATEGORIES ----

url_home        <- "https://www.canyon.com/en-de"
xopen(url_home) # Open links directly from RStudio to inspect them

# Read HTML for the entire webpage
html_home       <- read_html(url_home)

# Extract the categories URLs (Roadbike-Endurance, Roadbike-Race, ...)
# All Models are listed on these levels.
bike_categories_chr <- html_home |>
  
  # Get the nodes for the categories. Take a look at the source code for the selector.
  # (Unfortunately not working with the Selector Gadget with the original page)
  html_elements(css = ".header__navBarPreloadItem--level2") |>
  
  # Extract the href attribute (the URLs)
  html_attr('href') |>
  
  # Remove the product families Sale, Outlet, Gear and Customer Service
  str_subset(pattern = "sale|outlet|gear|customer-service", negate = T) |>
  
  # Add the domain, because we will get only the subdirectories
  # Watch out for the new pipe placeholder `_` here.
  # It requires a named argument (`...` in this case)
  str_c("https://www.canyon.com", ... = _)

bike_categories_chr

# 2.0 COLLECT BIKE DATA ----

# Select first bike category url
bike_category_url <- bike_categories_chr[1]

# Get the URLs for the bikes of the first category
html_bike_category  <- read_html(bike_category_url)

bike_url_chr        <- html_bike_category |>
  
  # Get the 'a' nodes that containt the title and the link
  html_elements(".productTileDefault__productName") |>
  
  # html_elements(css = ".productTileDefault--bike > div > a") |>
  html_attr("href") |>
  
  # Remove the query parameters of the URL (everything after the '?')
  str_remove(pattern = "\\?.*")

bike_url_chr


# 2.2 Wrap it into a function ----
get_bike_urls <- function(url) {
  
  html_bike_category <- read_html(url)
  
  # Get the URLs
  bike_url_chr  <- html_bike_category |> 
    html_elements(css = ".productTileDefault__productName") |>
    html_attr("href") |>
    str_remove(pattern = "\\?.*")
  
  return(bike_url_chr)
  
}

# Run the function with the first url to check if it is working
get_bike_urls(url = bike_categories_chr[1])

# Map the function against all urls
bike_urls_chr <- map(bike_categories_chr, get_bike_urls) |>
  flatten_chr() |>
  unique()




# Split data to enable filtering
bike_urls_tbl <- bike_urls_chr |>
  
  tibble::as_tibble_col(column_name = "url") |>
  tidyr::separate_wider_regex(cols = url, patterns = c(".*en-de/", family   = "[^/]*", "/",
                                                       category = "[^/]*", "/",
                                                       model    = "[^/]*", "/",
                                                       material = "[^/]*", "/",
                                                       ".*"), cols_remove = F)

# Filter
bike_urls_endurace_tbl <- bike_urls_tbl |>
  filter(model == "endurace")

# Print
bike_urls_endurace_tbl |> slice(1:5)

# For 1 bike
html_bike_model <- read_html(bike_urls_endurace_tbl$url[1])

bike_price <- html_bike_model |>
  html_element(css = ".productDescription__priceSale") |>
  html_text() |>
  parse_number() |>
  str_remove("\\.")

bike_model <- html_bike_model |> 
  html_elements(css = ".xlt-pdpName") |> 
  html_text() |> 
  str_squish() # Clean

# Function to do it for multiple urls
get_model_data <- function(url) {
  
  html_bike_model <- read_html(url)
  
  bike_price <- html_bike_model |>
    html_element(css = ".productDescription__priceSale") |>
    html_text() |>
    parse_number() |>
    str_remove("\\.")
  
  bike_model <- html_bike_model |> 
    html_elements(css = ".xlt-pdpName") |> 
    html_text() |> 
    str_squish() # Clean
  
  bike_data <- tibble(url   = url,
                      model = bike_model,
                      price = bike_price)
  
  return(bike_data)
  
}



# For one model
bike_model_data_tbl <- get_model_data(url = bike_urls_endurace_tbl$url[1])

# For all models of the category
bike_model_data_tbl <- bike_urls_endurace_tbl$url |> map_dfr(get_model_data)

# Join data
bike_model_data_joined_tbl <- bike_urls_endurace_tbl |> 
  left_join(bike_model_data_tbl, by = join_by("url"))

# Print
bike_model_data_joined_tbl



















# Challenge 1

# Latitude and longitude of TUHH
latitude <- "53.46"
longitude <- "9.97"
  
resp <- GET(glue("https://api.open-meteo.com/v1/forecast?latitude={latitude}&longitude={longitude}&hourly=temperature_2m&timezone=Europe%2FBerlin"))

respTable <- resp %>% 
            .$content %>% 
            rawToChar() %>% 
            fromJSON()
respTable


# Extract relevant data from the response
forecast_data <- respTable$hourly

# Create a tibble
weather_tbl <- tibble(
  time = forecast_data$time,
  temperature_2m = forecast_data$temperature_2m
)

# Print the tibble
print(weather_tbl)



# Convert time to datetime format
weather_tbl$time <- as.POSIXct(weather_tbl$time, format = "%Y-%m-%dT%H:%M")

# Ensure data is sorted by time
weather_tbl <- weather_tbl %>% arrange(time)


# Extract day part of the date
weather_tbl$day <- factor(format(weather_tbl$time, "%d"), levels = unique(format(weather_tbl$time, "%d")))
weather_tbl <- na.omit(weather_tbl)


# Create a plot
p <- ggplot(weather_tbl, aes(x = time, y = temperature_2m)) +
  geom_point(position = "identity") +  # Set position to "identity" to remove space between points
  geom_line(color = "blue", show.legend = FALSE) +
  labs(title = "Hourly Temperature for 7 Days",
       subtitle = "Ordered by date and time",
       x = "Time",
       y = "Temperature (°C)") +
  theme_minimal() +
  facet_wrap(~ factor(format(weather_tbl$time, "%A"), levels = unique(format(weather_tbl$time, "%A"))), nrow = 1, scales = "free_x")

# Adjust x-axis labels to show only time with ":00"
p <- p + scale_x_datetime(date_breaks = "6 hours", date_labels = "%H:%M:00") # Adjust date_labels

# Remove space between plots
p <- p + theme(strip.text = element_text(size = 8, face = "bold"), 
               panel.spacing = unit(0, "lines"),
               axis.text.x = element_text(angle = 45, hjust = 1, size = 8), # Adjust angle and size
               legend.title = element_blank())

# Print the plot
print(p)





# Challenge 2

# Define the URL of the website
url_race <- "https://www.rosebikes.de/fahrr%C3%A4der/rennrad/race"
url_endurance <- "https://www.rosebikes.de/fahrr%C3%A4der/rennrad/endurance"
url_eBike <- "https://www.rosebikes.de/fahrr%C3%A4der/rennrad/e-rennrad"

# Read the HTML content of the webpage
webpage_race <- url_race %>% read_html()
webpage_endurance <- url_endurance %>% read_html()
webpage_eBike <- url_eBike %>% read_html()

# Extract the content within the h4 tag with class "basic-headline__title"
title_race        <- html_text(html_nodes(webpage_race, "h4.basic-headline__title"))
title_endurance   <- html_text(html_nodes(webpage_endurance, "h4.basic-headline__title"))
title_eBike       <- html_text(html_nodes(webpage_eBike, "h4.basic-headline__title"))

# Extract the content within the specified class "catalog-category-bikes__price-title"
price_title_race      <- html_text(html_nodes(webpage_race, ".catalog-category-bikes__price-title"))
price_title_endurance <- html_text(html_nodes(webpage_endurance, ".catalog-category-bikes__price-title"))
price_title_eBike<- html_text(html_nodes(webpage_eBike, ".catalog-category-bikes__price-title"))

# Remove "ab " and "\n" from the price
price_title_race <- gsub("ab |\\n", "", price_title_race)
price_title_endurance <- gsub("ab |\\n", "", price_title_endurance)
price_title_eBike<- gsub("ab |\\n", "", price_title_eBike)

# Remove trailing whitespaces
price_title_race <- trimws(price_title_race)
price_title_endurance <- trimws(price_title_endurance)
price_title_eBike<- trimws(price_title_eBike)


# Create a tibble with the extracted content
bike_data_race <- tibble(model = title_race, price = price_title_race)
bike_data_endurance <- tibble(model = title_endurance, price = price_title_endurance)
bike_data_eBike<- tibble(model = title_eBike, price = price_title_eBike)


# Print the tibble
print(bike_data_race)
print(bike_data_endurance)
print(bike_data_eBike)
