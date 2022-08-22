

library(timelineS)


age_dist_plot <- function(){
  #Distribution of patient's age
  # view(med)
  return(ggplotly(ggplot(med, aes(Age, fill=factor(Sex)))+geom_histogram(binwidth = 5,color = 'black')+ theme_bw() 
                  + scale_color_brewer(palette = "Dark2")
                  + scale_fill_brewer(palette = "Dark2"))
  )}

age_dist_bl20_plot <- function(){
  #Age distribution below 20
  child_Adol <- subset(med, Age<=20)
  return(ggplotly(ggplot(child_Adol,aes(Age, fill = factor(Sex))) + geom_histogram(binwidth = 1, color = 'black') + theme_bw() 
                  + scale_color_brewer(palette = "Dark2")
                  + scale_fill_brewer(palette = "Dark2"))
  )}
age_dist_20to60_plot <- function(){
  #Age distribution >20 and <=60
  adult <- subset(med, Age<=60&Age>20)
  return(ggplotly(ggplot(adult,aes(Age, fill = factor(Sex))) + geom_histogram(binwidth = 5, color = 'black') + theme_bw() 
                  + scale_color_brewer(palette = "Dark2")
                  + scale_fill_brewer(palette = "Dark2"))
         
  )}
age_dist_ov60_plot <- function(){
  #Age distribution >60
  senior_Adult <- subset(med, Age>60)
  return(ggplotly(ggplot(senior_Adult,aes(Age, fill = factor(Sex))) + geom_histogram(binwidth = 5, color = 'black') + theme_bw() 
                  + scale_color_brewer(palette = "Dark2")
                  + scale_fill_brewer(palette = "Dark2"))
  )}



days_to_recovery_plot <- function(){
  #mean time (in days) for recovery
  typeof(time.df[[1]])
  discharge.days <- durCalc(time.df, start="DateOfOnsetSymptoms", end="DateOfDischarge",timeunit = 'days')
  discharge.days <- discharge.days[,c("Age","days")]
  #mean(discharge.days$days, na.rm=T) # TODO : remove this line ?
  return(ggplotly(ggplot(discharge.days, aes(days, Age))+ geom_point(aes(color=Age),size = 2) +
                    scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))+
                    theme_bw() +
                    ggtitle("Days between date of onset symptoms and date of discharge"))
  )
}

days_to_death_plot <- function(){
  #mean time (in days) for death
  death.days <-durCalc(time.df, start="DateOfOnsetSymptoms", end="DateOfDeath",timeunit = 'days')
  death.days <- death.days[,c("Age","days")]
  return(ggplotly(ggplot(death.days, aes(days, Age))+ geom_point(aes(color=Age),size = 2) +
                    scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
                    theme_bw() +
                    ggtitle("Days between date of onset of symptoms and date of death"))
  )
}


patient_loc_map <- function(){
  LatLon <- colsplit(gsub("([A-Z])([0-9])", "\\1\\.\\2", med$GeoPosition), 
                     ",", c("Long", "Lat"))
  
  new <- data.frame(LatLon, med$Country)
  
  return(plot_geo(new) %>%
           add_markers(
             x = ~Lat, y = ~Long, hoverinfo = "text",
             text = ~paste(new$med.Country)) %>% layout (title = 'Patient Locations across the World')
  )}


symptom_wordcloud <- function(){
  com.symp <- subset(med, !is.na(med$Symptoms))
  com.symp <- com.symp[,"Symptoms"]
  write.table(com.symp, "./resources/symp.txt", append = FALSE, sep = ",", row.names = F,col.names=F)
  sent <- scan("./resources/symp.txt","character",sep="\n", quiet = TRUE)
  sent <- gsub("[^A-Za-z,]+"," ",sent)
  symp.words <- strsplit(sent,",")
  word.count <- table(unlist(symp.words))
  doc <- data.frame(cbind(names(word.count),as.integer(word.count)))
  doc <- doc[-c(1,34),]
  names(doc) <- c('symptoms','freq')
  doc$freq <- as.numeric(as.character(doc$freq))
  doc <- doc[order(doc$freq, decreasing = T),]
  return(wordcloud2(doc, size = 2 , color = 'random-light', backgroundColor
                    = 'black')
  )
}

symptom_piechart <- function(){
  df <- data.frame(med$Country,med$LivesInWuhan,med$Symptoms)
  df1 <- subset(df, (!is.na(med.Country) & !is.na(med.LivesInWuhan) & !is.na(med.Symptoms))) #remove NA values from these 3 columns
  chinaOnly <- df1[df1$med.Country == 'China' & df1$med.LivesInWuhan == 'True' ,] #Retreive only china and Only Wuhan since that is the 
  colnames(chinaOnly)[colnames(chinaOnly) == 'med.Symptoms'] <- 'Symptoms'
  library(plyr)
  
  dt <- count(chinaOnly, 'Symptoms') #Data frame with frequencies of each symtoms
  
  dt$rank <- rank(-dt$freq,ties.method="min") #Get the ranking
  df <- dt[order(dt$rank,decreasing = F),]
  
  topsym_inWuhan <- head(df,8) 
  dat <- topsym_inWuhan[,c('Symptoms', 'freq')]
  fig <- plot_ly(dat, labels = ~ Symptoms, values = ~freq, type = 'pie')
  fig <- fig %>% layout(title = 'Most Common Symptoms in Wuhan',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  return(fig)
}


likely_recover_boxplot <- function(){
  #Analysing the age groups that are most likely to recover
  
  alive <- subset(med, (!is.na(DischargedQ) | !is.na(DateOfDischarge)))
  alive <- alive[,c("Age","Sex","DischargedQ","DateOfDischarge")]
  return(ggplotly(ggplot(data=alive, aes(" ", Age)) + geom_boxplot(color='black', fill = '#10AB05') +
                    stat_summary(fun=mean, colour="darkred", geom="point", shape=18, size=3,show.legend  = FALSE) +
                    theme_bw()+
                    ggtitle("Analyzing age group who are likely to recover "))
  )
}


unlikely_recover_boxplot <- function(){
  #Analysing the age group that are unlikely to recover
  dead <- subset(med, !is.na(med$Age) & !is.na(med$Sex) & !is.na(med$DeathQ))
  dead <- dead[,c("Age","Sex","DeathQ")]
  return(ggplotly(ggplot(data=dead, aes(" ", Age)) + geom_boxplot(color='black', fill = '#3BA6E5') +
                    stat_summary(fun=mean, colour="darkred", geom="point", shape=18, size=3,show.legend  = FALSE) +
                    theme_bw() +
                    ggtitle("Analyzing age group who are unlikely to recover"))
  )
}


likely_recover_boxplot_gender <- function(){
  
  return(retuggplotly(ggplot(data=alive, aes(Sex, Age, fill= factor(Sex))) + geom_boxplot(color='black') +
                        stat_summary(fun=mean, colour="darkred", geom="point",shape=18, size=3,show.legend  = FALSE) + 
                        theme_bw() +
                        ggtitle("Analyzing age group that are likely to recover w.r.t Gender"))
  )
}

unlikely_recover_boxplot_gender <- function(){
  
  #Analysing the age groups that are unlikely to recover w.r.t Gender
  return(ggplotly(ggplot(data=dead, aes(Sex, Age, fill= factor(Sex))) + geom_boxplot(color='black') +
                    stat_summary(fun=mean, colour="darkred", geom="point", shape=18, size=3,show.legend  = FALSE)+  
                    theme_bw() + 
                    ggtitle("Analyzing age group that are unlikely to recover w.r.t Gender"))
  )
}

chroic_disease_gender_plot <- function(){
  true.chron <- subset(med, med$ChronicDiseaseQ=='True')
  true.chron <- true.chron[,c("Age","Sex","ChronicDiseaseQ")]
  pl <- ggplotly(ggplot(true.chron, aes(Age, fill = factor(Sex)))+ geom_histogram(binwidth = 10, color = 'black')+ facet_wrap(~Sex) + theme_bw() )
  return(pl)
}

chronic_disease_freq_plot <- function(){
  write.table(med$ChronicDiseases, "./resources/char.txt", append = FALSE, sep = ",", row.names = F,col.names=F)
  sentences <- scan("./resources/char.txt","character",sep="\n", quiet = TRUE)
  sentences <- gsub("[^A-Za-z,]+"," ",sentences)
  words <- strsplit(sentences,",")
  words.freq <- table(unlist(words))
  dm <- data.frame(cbind(names(words.freq),as.integer(words.freq)))
  names(dm) <- c('word','count')
  dm$count <- as.numeric(as.character(dm$count))
  dm <- dm[order(dm$count, decreasing = F),]
  return(ggplotly(ggplot(dm, aes(x = reorder(word, count), y = count)) +
                    geom_col(fill = '#7104FF', color = 'black' ) +
                    labs(title="Chronic Diseases ",
                         x = NULL,
                         y = "Frequency") +
                    coord_flip() + theme_bw())
  )
}