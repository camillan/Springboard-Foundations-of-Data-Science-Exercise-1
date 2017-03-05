# load the dataset into Rstudio
library(readxl)
library(tidyr)
refine <- read_excel("~/Downloads/refine.xlsx")


# view dataset and determine its class
View(refine)
class(refine)
tbl_df(refine)


# convert company names
# first lower
library(stringr)
refine$company <- tolower(refine$company) 

refine$company <- str_replace_all(refine$company, "^p.*ps", "philips")
refine$company <- str_replace_all(refine$company, "^f.*ps", "philips")
refine$company <- str_replace_all(refine$company, "^a.*", "akzo")
refine$company <- str_replace_all(refine$company, "^v.*", "van houten")
refine$company <- str_replace_all(refine$company, "^u.*", "unilever")


# create separate columns for product code and number
refine <- separate(refine, `Product code / number `, c('Product_code', 'Product_number'))


# creating a function that converts product code to product categories, and
# making a new variable for it
uncode_func <- function(code1) {
  if (code1 == "p") {
    return("Smartphone")
  } else if (code1 == "v") {
    return("TV")
  } else if (code1 == "x") {
    return("Laptop")
  } else if (code1 == "q") {
    return("Tablet")
  } else
    return("NA")
}

refine$product_categories <- sapply(refine$Product_code, uncode_func)


# concatenating address variables and forming new variable
refine %>% 
  mutate(full_address = paste(address, city, country, sep = ','))

# adding new columns containing dummy variables

refine$company_philips <- as.numeric(refine$company == 'philips')
refine$company_akzo <- as.numeric(refine$company == 'akzo')
refine$company_van_houten <- as.numeric(refine$company == 'van houten')
refine$company_unilever <- as.numeric(refine$company == 'unilever')

refine$product_smartphone <- as.numeric(refine$product_categories == 'Smartphone')
refine$product_tv <- as.numeric(refine$product_categories == 'TV')
refine$product_laptop <- as.numeric(refine$product_categories == 'Laptop')
refine$product_tablet <- as.numeric(refine$product_categories == 'Tablet')