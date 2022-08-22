library(timevis)
timevis()

data_covid <- function(){ return(data.frame(
  id      = 1:25,
  content = c("China officially reports cases to WHO",
              "In France, the first evidence of the virus",
              "The virus has reached Germany",
              "The WHO declares a 'health emergency of international concern'",
              "France reports the first death in Europe",
              "Coronavirus infections have now been confirmed for the first time in Baden-Württemberg and North Rhine-Westphalia",
              "There are infections in about 60 countries. According to the WHO, there are around 3,000 deaths.",
              "The WHO declares a pandemic",
              "In most of the federal states schools and day care centres are already closed, others will follow. Also border controls and entry bans",
              "Italy is now the country with the most officially reported deaths worldwide",
              "Germany: Federal and state governments agree on strict exit and contact restrictions",
              "At over 140,000, more infections are now known in the USA than have been officially recorded in any other country in the world",
              "The nationwide contact restrictions are extended until 19 April.",
              "Start of the OVGU lecture period - digital",
              "Germany: The contact restrictions are extended until 3 May.",
              "Germany: first relaxation of corona protection measures",
              "Germany: In all federal states the mouth protection obligation applies",
              "Germany: further relaxation of corona restrictions",
              "Germany: end of controls at the German external borders (gradually)",
              "Brazil reports 15,000 new infections within 24 hours",
              "Germany: The number of new infections is below 1,000 (R) for the tenth consecutive day.",
              "The number of infections registered worldwide exceeds five million.",
              "Germany: economic stimulus package adopted",
              "China reports highest increase in new infections since April",
              "Germany: The government has launched the Corona Warning App"),
  start   = c("2019-12-31", "2020-01-24", "2020-01-27", "2020-01-30", "2020-02-15", "2020-02-24", "2020-03-02", 
              "2020-03-11", "2020-03-16", "2020-03-19", "2020-03-22", "2020-03-29", "2020-04-01", "2020-04-06",
              "2020-04-15", "2020-04-20", "2020-04-27", "2020-05-06", "2020-05-13", "2020-05-17", "2020-05-19",
              "2020-05-21", "2020-06-04", "2020-06-14", "2020-06-16"),
  end     = c(NA          ,           NA,           NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA)
))}

# timevis(data_covid)

