# package installer for the project

packages <- c(
  "proto",
  "gsubfn",
  "tm",
  "SnowballC",
  "wordcloud",  
  "RColorBrewer",
  "stringr",
  "chron",
  "aplpack",
  "ggpubr",
  "stringr",
  "ggplot2",
  "plotly",
  "ggthemes",
  "wordcloud2",
  "here",
  "sf",
  "reshape2",
  "tidyr",
  "lubridate",
  "readr",
  "forecast",
  "fpp2",
  "TTR",
  "dplyr",
  "RCurl",
  "timevis",
  "data.table",
  "twitteR",
  "tidytext",
  "wordcloud",
  "RColorBrewer",
  "SentimentAnalysis",
  "syuzhet",
  "wordcloud2",
  "scales",
  "timelineS"
)

verify.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = 
                       TRUE)
  sapply(pkg, library, character.only = TRUE)
}

verify.packages(packages)
