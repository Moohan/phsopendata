pkgname <- "phsopendata"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('phsopendata')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("get_dataset")
### * get_dataset

flush(stderr()); flush(stdout())

### Name: get_dataset
### Title: Get Open Data resources from a dataset
### Aliases: get_dataset

### ** Examples

get_dataset("gp-practice-populations", max_resources = 2, rows = 10)



cleanEx()
nameEx("get_dataset_additional_info")
### * get_dataset_additional_info

flush(stderr()); flush(stdout())

### Name: get_dataset_additional_info
### Title: get a datasets additional info
### Aliases: get_dataset_additional_info

### ** Examples

get_dataset_additional_info("gp-practice-populations")



cleanEx()
nameEx("get_latest_resource")
### * get_latest_resource

flush(stderr()); flush(stdout())

### Name: get_latest_resource
### Title: Get the latest resource from a data set
### Aliases: get_latest_resource

### ** Examples

dataset_name <- "gp-practice-contact-details-and-list-sizes"

data <- get_latest_resource(dataset_name)

filters <- list("Postcode" = "DD11 1ES")
wanted_cols <- c("PracticeCode", "Postcode", "Dispensing")

filtered_data <- get_latest_resource(
  dataset_name = dataset_name,
  row_filters = filters,
  col_select = wanted_cols
)




cleanEx()
nameEx("get_resource")
### * get_resource

flush(stderr()); flush(stdout())

### Name: get_resource
### Title: Get Open Data resource
### Aliases: get_resource

### ** Examples

res_id <- "ca3f8e44-9a84-43d6-819c-a880b23bd278"

data <- get_resource(res_id)

filters <- list("HB" = "S08000030", "Month" = "202109")
wanted_cols <- c("HB", "Month", "TotalPatientsSeen")

filtered_data <- get_resource(
  res_id = res_id,
  row_filters = filters,
  col_select = wanted_cols
)



cleanEx()
nameEx("get_resource_sql")
### * get_resource_sql

flush(stderr()); flush(stdout())

### Name: get_resource_sql
### Title: Get PHS Open Data using SQL
### Aliases: get_resource_sql

### ** Examples

sql <- "
   SELECT
     \"TotalCancelled\",\"TotalOperations\",\"Hospital\",\"Month\"
   FROM
     \"bcc860a4-49f4-4232-a76b-f559cf6eb885\"
   WHERE
     \"Hospital\" = 'D102H'
"
df <- get_resource_sql(sql)

# This is equivalent to:
cols <- c("TotalCancelled", "TotalOperations", "Hospital", "Month")
row_filter <- c(Hospital = "D102H")

df2 <- get_resource(
  "bcc860a4-49f4-4232-a76b-f559cf6eb885",
  col_select = cols,
  row_filters = row_filter
)



cleanEx()
nameEx("list_datasets")
### * list_datasets

flush(stderr()); flush(stdout())

### Name: list_datasets
### Title: Lists all available datasets
### Aliases: list_datasets

### ** Examples

head(list_datasets())



cleanEx()
nameEx("list_resources")
### * list_resources

flush(stderr()); flush(stdout())

### Name: list_resources
### Title: Lists all available resources for a dataset
### Aliases: list_resources

### ** Examples

list_resources("weekly-accident-and-emergency-activity-and-waiting-times")



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
