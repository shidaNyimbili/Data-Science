# Data Preprocessing

# Importing the dataset
dataset <- read.csv('Data.csv')

view(dataset)

# Check total missing values per column
colSums(is.na(dataset))

# View rows with any missing values
missing_rows <- dataset[!complete.cases(dataset), ]
print(missing_rows)

missing_rows <- df[!complete.cases(df), ]

# Percentage of missing data per column
sapply(dataset, function(x) mean(is.na(x)) * 100)



# Taking care of missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)