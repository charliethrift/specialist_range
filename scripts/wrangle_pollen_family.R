# wrangle pollen data: family level
# 14 July 2025
# Charles Thrift

# 0. Libraries
library(tidyverse)

# 1. Read data
df <- read.csv("../../pollen_data/wood2023_supp2_data_forfamilyanalysis.csv")

# 2. Filter data
df1 <- df[,c(5:123)] 
df1[df1 <= 2] <- NA
df1[df1 > 2] <- 1
df1[is.na(df1)] <- 0
df2 <- as.data.frame(rowSums(df1))
data <- cbind(df,df2)
data <- data[,c(1,3,4,125)]
data <- rename(data, familydiet="rowSums(df1)")

# 3. Print data
write.csv(data, "../../pollen_data/pollen_data_familyDiet.csv")
