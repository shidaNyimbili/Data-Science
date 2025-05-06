source("Scripts/Data Science/r prep2.r")

# rm(list =ls())
# graphics.off()

# library(terra)
# 
# install.packages("terra")
# 
# Sys.which("make")
# 
# install.packages("terra", type = "binary")


#inspect the data
help("mpg")

df <- mpg

df 

view(df)

#Checking data type and number of rowsColumns
str(df)

nrow(df); ncol(df)
names(df)
glimpse(df)

###*Manipulate the variables
#Select() - columns selection

##Extract: manufacturer, model year

df1 <- select(df, manufacturer, model, year)
df1

arrange(df, desc(year))

colnames(df)

#or

df3 <- df %>%
  select(1, 2, 4)

#or

df2 <- df %>%
  select(manufacturer, model, year)

df2

df.car.info <- select(df, manufacturer, model, year)

df.car.info


#Columns that begin with letter: "m"
select(df, starts_with(match = "m"))

##columns that contain with letter: "r"
select(df, contains("r"))
select(df, contains("a"))

##columns that end with letters
select(df, ends_with("y"))

mer <- select(df, ends_with("y"))
mer
colnames(df)
print(mer)

##select columns by column index or position
select(df, 1:5)
select(df,1)
select(df, c(1, 4, 6)) #select multiple columns in no chronological order

df

##Rename columns
df1 <- rename(df,
              shida = manufacturer,
              zevy = year)

select(df,
       shida = manufacturer,
       zevy = year,
       everything())

df

colnames(df1)
colnames(df)

##Mutate / Transmute
#Creating new variables
#Average calculations

df <- mutate(df, 
             'avg miles per gallon' = (cty + hwy/2))

View(df)
## car, cyl/ trans

df <- mutate(df, 
             car = paste(manufacturer, model, sep= " "))
view(df)

df <- mutate(df, 
             car = paste(manufacturer, model, sep= " "),
            'cyl / trans'= paste(cyl,"cylinders", trans, "transmission", sep=" "))
view(df)

#Transmute
#create the column and drops other columns off the initial table

dfme <- transmute(df, paste(manufacturer, model, sep= " "))

dfme

sh <- transmute(df, car = paste(manufacturer, model, sep=" "))
sh

ns <- transmute(df, manufacturer, model)

ns

ckn <- transmute(df, cars = paste(manufacturer, model, year, sep=""))

ckn

#Manulpulate cases - rows

#filter() - filter rows by condition

filter(df, manufacturer == "audi")

filter(df, manufacturer == "audi" & year == 1999) #&

filter(df, manufacturer == "audi" | manufacturer == "dodge") # using \ , &

filter(df, manufacturer == "audi" | manufacturer == "dodge" & year == 1999) # using \ , &

filter(df, hwy >= 30) # using \ , &, >=

filter(df, year != 1999) #using !=

#Extarct using slice, taking rows
slice(df, 1:5)

slice(df, 1:4)

nrow(df);ncol(df)
slice(df, (nrow(df)-9):nrow(df))

#Arrange - using for sorting rows
#sort rows by year (ascending order )
arrange(df, year)

arrange(df, desc(year))      
