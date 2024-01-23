# we will be talking about data.frames

# Let's import some data
download.file(url = "https://ndownloader.figshare.com/files/2292169", 
              destfile = "data_raw/portal_data_joined.csv")

library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")

head(surveys)

str(surveys)

dim(surveys)
nrow(surveys)
ncol(surveys)

tail(surveys)

# equivalent to
colnames((surveys))
rownames(surveys)


summary(surveys)

# Indexing and subsetting
surveys[1, 6]

surveys[1, ]
surveys[ ,1]

surveys[c(1,2,3),c(4,5)]
surveys[c(1,2,3),]

surveys[1:3, 5:6]
surveys[,-1]

surveys[,"sex"]
surveys["sex"]
surveys$plot_id

# challenge
surveys_200 <- surveys[200, ]
surveys_200_again <- c(surveys[200, ])
surveys[nrow(surveys), ]

surveys[nrow(surveys)/2, ]

my_list <- list(names= c("Nora", "Lisanna", "Francesco"),
                money = c(1, 6, 7, 3, 5, 8))
