##########Written By Liam Flaherty for ST558 HW 3##########
#####1. Load Data And Required Packages#####
install.packages("tidyverse")
install.packages("DBI")  
install.packages("RSQLite")
install.packages("Lahman")
install.packages("readr")
install.packages("readxl")
library(tidyverse)
library(DBI)
library(RSQLite)
library(Lahman)
library(readr)
library(readxl)





#####2. Practice Chaining#####
a=arrange(filter(select(as_tibble(iris), starts_with("Petal"), Species), Petal.Length<1.55), Species)

b=iris|>
  as_tibble() |>
  select(starts_with("Petal"), Species) |>
  filter(Petal.Length<1.55) |>
  arrange(Species)

all.equal(a,b)



###2a. Base R Solution###
df=iris[which(iris$Petal.Length<1.55), c("Petal.Length", "Petal.Width", "Species")]
df=df[order(df$Species, decreasing=FALSE),]
row.names(df)=1:nrow(df)

all.equal(as.data.frame(a), df)





#####3. Dealing With Delimited Data#####
###3a. Load in data###
glass.data=read_delim("https://www4.stat.ncsu.edu/~online/datasets/glass.data", 
            delim=",",
            col_names=c("Id","RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type_of_glass"))
glass.data

yeast.data=read_delim("https://www4.stat.ncsu.edu/~online/datasets/yeast.data",
                      delim="  ",
                      col_names=c("seq_name","mcg", "gvh", 
                                  "alm", "mit","erl","pox",
                                  "vac","nuc","class"))
yeast.data



###3b. Manipulate data###
rename=c("building_windows_float_processed",
         "building_windows_non_float_processed",
         "vehicle_windows_float_processed",
         "vehicle_windows_non_float_processed",
         "containers",
         "tableware",
         "headlamps")

glass=glass.data |>
  mutate(Type_of_glass=ifelse(Type_of_glass==1, rename[1],
                       ifelse(Type_of_glass==2, rename[2],
                       ifelse(Type_of_glass==3, rename[3],
                       ifelse(Type_of_glass==4, rename[4],
                       ifelse(Type_of_glass==5, rename[5],
                       ifelse(Type_of_glass==6, rename[6],
                       ifelse(Type_of_glass==7, rename[7],
                       "N/A")))))))) |>
  filter(Fe<0.2 & (Type_of_glass=="tableware" | Type_of_glass=="headlamps"))
glass

yeast=yeast.data |>
  select(-c(seq_name, nuc)) |>
  group_by(class) |>
  mutate(across(where(is.numeric), list(mean=mean, median=median), .names="{.col}_{.fn}"))
yeast



###3c. Base R Solution###
test=as.data.frame(glass.data)
test=test[which(test$Fe<0.2 & (test$Type_of_glass==6 | test$Type_of_glass==7)),]





#####4. Combining Excel And Delimited#####
excel_sheets("./white-wine.xlsx")

white_wine=read_excel("./white-wine.xlsx",
              sheet="white-wine")
white_wine

variables=as.data.frame(
              read_excel("./white-wine.xlsx",
              sheet="variables"))
variables

colnames(white_wine)=variables$Variables
white_wine=mutate(white_wine, wine_type=rep("white", nrow(white_wine)))

red_wine=read_csv2("./red-wine.csv",col_types="n")  
colnames(red_wine)=variables$Variables
red_wine= red_wine |>
  mutate_all(as.numeric)
red_wine= mutate(red_wine, wine_type=rep("red", nrow(red_wine)))

str(white_wine)
str(red_wine)

wine=bind_rows(white_wine, red_wine)  

wine=wine |>
  filter(quality>6.5 & alcohol<132) |>
  arrange(desc(quality)) |>
  select(contains("acid"), c("alcohol", "wine_type", "quality")) |>
  group_by(quality) |>
  mutate(across(alcohol, list(mean=mean, sd=sd), .names="{.col}_{.fn}"))
wine




#####4. Database Practice#####  
con=dbConnect(RSQLite::SQLite(), "lahman.db")
dbListTables(con)
tbl(con, "Teams")


hof=tbl(con, "HallofFame") |>
  filter(inducted=="Y") |>
  select(c("playerID", "yearID", "category"))

named_hof=right_join(tbl(con, "People") ,hof) |>
  select(c("playerID", "nameFirst", "nameLast", "yearID", "category"))
named_hof=as_tibble(named_hof)

managers=tbl(con, "Managers")

managers=managers |>
  select("playerID", "G", "W", "L") |>
  group_by(playerID) |>
  summarize(G_managed=sum(G, na.rm=TRUE),
            Total_W=sum(W, na.rm=TRUE),
            Total_L=sum(L, na.rm=TRUE)) |>
  collect()

  Total_WP=managers$Total_W/managers$G_managed
  managers=bind_cols(managers, Total_WP=Total_WP) |>
    arrange(desc(Total_WP))
  managers
  
manager_hof=left_join(managers, named_hof, by="playerID")
manager_hof
