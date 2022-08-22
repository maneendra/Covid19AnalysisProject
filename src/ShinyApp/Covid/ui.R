
library(shiny)
library(shinythemes)
library(tidyr)
library(ggplot2)
library(reshape2)
library(lubridate)
library(readr)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(RCurl)
library(plotly)
library(lubridate)
library(threejs)
library(leaflet)
library(timevis)
library(wordcloud2)


confirmed_url = "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"
john_confirmed <- read.csv(textConnection(getURL(confirmed_url)))

# Define UI for application that draws a histogram
shinyUI(
  fluidPage( class = "main-body",
             theme = "styles.css",
             #theme = shinytheme("flatly"),
             tags$script(src = "particle.js"),
             
             tags$div( class="title",
                       tags$img(height="80px", width="80px", src="virus.png"),
                       tags$h1("COVID-19"),
                       tags$h3("Analysis and Predictions"),
             ),
             tags$div( class="title-images",
                       tags$a(href="https://www.inf.ovgu.de/",
                              tags$div(id = "fin-img",
                                       tags$img( src="fin_transp.png")
                              )
                       ),
                       tags$div(id = "dscr-img",
                                tags$img(src="DScR.png"),
                       )
             ),
             tabsetPanel(
               tabPanel("Overview",
                        fluidRow( class = "data-panel",
                                  fluidRow( class = "overview-top",
                                            column(7,
                                                   tags$div( id = "motivation-container",
                                                             tags$h2("Motivation"),
                                                             tags$div(class = "motivation-text",
                                                                      "The world is battling with an invisible and  fatal enemy, trying to understand how to live with the danger presented by a virus. It has been named COVID-19, belonging to a family of coronavirus to be the seventh known to affect humans.The novel coronavirus outbreak originated in Wuhan, China and has been declared as a 'pandemic' by the World Health Organization (WHO).
                                          Currently, being the only topic in everyone's heart, mind and soul, we would like to analyse and understand the global impact of the novel coronavirus (COVID-19).  Without any doubt, we can say that it has alarmed the wellbeing of people and has resulted in social, economic and political crisis. This inspires us to predict the number of cases in the future according to the current circumstance which will help the country to make appropriate decisions in order to control the spread of the virus. Also, understanding the cause and drift of the virus in different people is also one of the major concerns that everybody is curious to know about. Lastly, we would like to know the opinions and experience of people around the world during such a pandemic phase of life. "),
                                                   ),
                                            ),
                                            column(5, class = "video-column",
                                                   tags$div( id = "video-container",
                                                             tags$iframe(src="https://www.youtube.com/embed/DhgDT_I-Ogg", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA),
                                                   )
                                            )
                                            
                                  ),
                                  fluidRow( class = "overview-bot",
                                            tags$h2("Objectives"),
                                            
                                            tags$div( class = "research-question",
                                                      tags$div( class = "research-question-title",
                                                                "What will be the worldwide effect of the COVID-19?"
                                                      ),
                                                      tags$div(
                                                        class = "boxed",
                                                        tags$div("Countries have different strategies to minimize the spread of the virus and prevent a second wave. In order to predict the future cases ( confirmed, recovered, dead and active ) country wise purely based on previous values of the time series, we have considered the ARIMA model. ARIMA, short for ‘AutoRegressive Integrated Moving Average’, is a forecasting algorithm which works on the idea that the information in the past values of the time series can alone be used to predict the future values.Forecasting will give us an idea of the extent to which a country will be affected if no changes are made to improve the current situation. For example., government of a country can decide if there should be a lockdown or not, travel restrictions in different countries etc."),
                                                      ),
                                                      
                                                      
                                                      # class = "research-question",
                                                      tags$div( class = "research-question-title",
                                                                "Some people are killed whereas others are spared by COVID-19, why?"
                                                      ),
                                                      tags$div(
                                                        class = "boxed",
                                                        tags$div("We analysed the patient dataset in order to gain better insights of the symptoms and main risk factors such as age, history of chronic diseases and travel developing more serious COVID-19 outcomes. This understanding is important in order to help the early detection of the virus keeping in mind that this virus is asymptotic. Different countries' statutes vary, for example in Africa the advisors say that an early detection of the virus can help them develop a strategy which can minimize the hardship caused by the lockdowns. This is also the case in many other countries.
Analysing the different age groups since the risk of dying from the infection, and the likelihood of requiring intensive medical care significantly increases with age. Initially, we did the analysis of the patients across the globe, then we boiled it down to Wuhan, China where the mysterious pneumonia cases were first detected in December 2019. Transmission of the disease is primarily due to close contact with one and other, so we will look at the patients from various countries who visited Wuhan. Patients with a history of chronic diseases tend to be more vulnerable to this virus as they have a low immune system, so we wanted to analyse the severity or fatal rates of such patients.
."),
                                                      ),
                                                      
                                                      
                                                      # class = "research-question",
                                                      tags$div( class = "research-question-title",
                                                                "What is the role of social media such as Twitter during the pandemic?"
                                                      ),
                                                      tags$div(
                                                        class = "boxed",
                                                        tags$div("While this pandemic has kept on influencing the lives of millions, a number of nations have turned to total lockdown. During quarantine, people have taken social media to express their feelings by  support, creating awareness and entertainment. We have taken one of the social networking services and a microblogging platform Twitter in order to analyze the popular COVID-19 trends and engagement of people. Based on the tweets, we would like to see the different  behaviors and reactions exhibited worldwide. People are different and they behave differently during difficult situations. Therefore it is interesting to know how people have expressed their emotions and how they have dealt with this unfortunate situation. Furthermore, it is curious to know what are the common words that public has used to express their feelings. Therefore, here  we would like to perform sentiment analysis on the tweet texts retrieved from Twitter using Twitter API."),
                                                      )
                                                      
                                            )
                                  ),
                        )
               ),
               tabPanel("Prediction Tool",
                        fluidRow( class = "data-panel",
                                  
                                  column(4, id = "predictor-side-panel",
                                         fluidRow( class = "inputs",
                                                   selectInput("country", "Country:",
                                                               john_confirmed$Country.Region),
                                                   
                                                   selectInput("typeToPredict", "Type:",
                                                               c("confirmed" = "confirmed",
                                                                 "recovered" = "recovered",
                                                                 "dead" = "dead",
                                                                 "active" = "active")),
                                                   
                                                   sliderInput("daysToPredict", "Days to predict:", 1, 15, 7),
                                                   
                                                   
                                                   sliderInput("graphWindow", "Days to display on graph:", 16, ncol(john_confirmed), 20),
                                         ),
                                         globeOutput("globe")
                                  ),
                                  column(8,
                                         fluidRow( class = "right-top",
                                                   h2(textOutput("country")),
                                                   tags$div( class = "plot-wraper",
                                                             plotlyOutput("casesPlot")
                                                   )
                                         ),
                                         fluidRow( class = "right-bottom",
                                                   column(6,
                                                          id = "table-container",
                                                          h2(textOutput("casesType")),
                                                          tableOutput("casesTable")
                                                   ),
                                                   column(6,
                                                          id = "predictor-description",
                                                          "To get an idea about how the virus could spread across a country in the next couple of days, we created this predictor to forecast those values. The idea behind this was that, based on the current predictions, a country could take appropriate actions to cut down the spread of the virus.",
                                                          # like enforing a lockdown if the number of cases are increasing ih huge numbers or lift up the lockdown if things are getting better.
                                                          tags$br(),
                                                          "You can select a Country and the type of cases to predict (confirmed, recovered, dead, active) from the two dropdowns.",
                                                          tags$br(),
                                                          "The \"days to predict\" slider allows you to select the number of days you want to see the predictions for",
                                                          tags$br(),
                                                          "The \"Days to display on graph\" slider allows you to select the viewing window for the graph"
                                                   )
                                         )
                                  )
                                  
                        )
               ),
               
               tabPanel("Patient Data",
                        fluidRow( class = "data-panel",
                                  navlistPanel(
                                    tabPanel("Patient locations",
                                             
                                             tags$div( class = "plot-wraper patient-plot",
                                                       plotlyOutput("patient_loc_map")
                                             ),
                                             tags$div(class = "patient-caption",
                                                      tags$div(
                                                        "This shows us the locations of patients of our across the globe plotted on the world map.The outbreak spread from the Chinese city of Wuhan to more than 180 countries and territories affecting every continent except Antartica. Efforts to stamp out the pneumonia-like illness have driven all the countries to enforce lockdowns, widespread halts of international travel, mass layoffs and shattered financial markets."
                                                      ))
                                    ),
                                    tabPanel("Symptoms",
                                             tabsetPanel(
                                               tabPanel("Outline", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  wordcloud2Output("symptom_wordcloud")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "From our wordcloud we can say that cough and fever are most common symptoms among the patients. People generally develop signs and symptoms, including mild respiratory symptoms and fever, on an average of 5-6 days after infection (mean incubation period 5-6 days, range 1-14 days)."
                                                                 ))
                                               ),
                                               tabPanel("Distribution", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("symptom_piechart")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "We plotted a pie chart to display the common symptoms seen initially in Wuhan, China which is the origin.It describes the highest ranked symptoms seen together in the origin of COVID-19 that is Wuhan,China.Covid-19 symptoms are  non-specific and the disease presentation can range from no symptoms(asymptomatic) to severe pneumonia and death."
                                                                 ))
                                               )   
                                             )
                                             
                                    ),
                                    tabPanel("Age Group Analysis",
                                             tabsetPanel(
                                               tabPanel("Distribution", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("age_dist_plot")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "We compared the age distribution among the patients ( Child, Adult and Senior Adult) . The age group between 40 to 60 have been affected the most. From our analysis ans well as confirmed by International research the percentage of children among the confirmed COVID-19 patients is low, ranging from 1% in young children to 6% in older children.  Children with COVID-19 do have the same symptoms as adults. The most common symptoms in children are coughing, fever and sore throat. Worldwide, very few children with COVID-19 have died.In clusters of patients, adults are almost always the source patient. On basis of this, decisions such as re-opening of schools and childcare facilities were made [@Children]."
                                                                 ))
                                               ),
                                               tabPanel("Likely To Recover", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("likely_recover_boxplot")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "We create a boxplot to  analyse the age of people who have died and of those who did not. The rhombus in the boxplot shows the mean(y) value of those who survived as 43.9 , while the mean(x) for those who have died as 65.5. So, this indeed shows that older people have lower chances of survival."
                                                                 ))
                                               ),
                                               tabPanel("Unlikely To Recover", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("unlikely_recover_boxplot")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "We create a boxplot to  analyse the age of people who have died and of those who did not. The rhombus in the boxplot shows the mean(y) value of those who survived as 43.9 , while the mean(x) for those who have died as 65.5. So, this indeed shows that older people have lower chances of survival."
                                                                 ))
                                               )  
                                             )
                                             
                                    ),
                                    tabPanel("Post Onset Symptoms Analysis",
                                             tabsetPanel(
                                               tabPanel("Days Till Recovery", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("days_to_recovery_plot")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "Covid-19 is a confusing illness , wrapped with uncertainty. There has not been sufficient scientific studies to tell precisely how long it takes for a person to recover. While some possible ranges have been identified. These seems to vary from person to person. According to WHO, recovery times tend to be about 2 weeks for those with mild disease and about 3-2 weeks for those with severe/critical disease."
                                                                 ))
                                               ),
                                               tabPanel("Days Till Death", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("days_to_death_plot")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "Covid-19 is a confusing illness , wrapped with uncertainty. There has not been sufficient scientific studies to tell precisely how long it takes for a person to recover. While some possible ranges have been identified. These seems to vary from person to person. According to WHO, recovery times tend to be about 2 weeks for those with mild disease and about 3-2 weeks for those with severe/critical disease."
                                                                 ))
                                               )
                                             )
                                    ),
                                    tabPanel("Chronic Disease Analysis",
                                             tabsetPanel(
                                               tabPanel("Distribution", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("chroic_disease_gender_plot")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "Everyone exposed to virus are at risk. However, some people are more vulnerable than others to become severely ill and more in need for medical facilities. According to \"Centers for Disease Control and Prevention\" , people of any age with certain chronic diseases are at higher risk for severe illness from covid-19."
                                                                 ))
                                               ),
                                               tabPanel("Chronic Disease Frequency", 
                                                        tags$div( class = "plot-wraper patient-plot",
                                                                  plotlyOutput("chronic_disease_freq_plot")
                                                        ),
                                                        tags$div(class = "patient-caption",
                                                                 tags$div(
                                                                   "Most of the people infected with the virus have mild to moderate disease, which includes non-pneumonia and pneumonia cases. Asymptomatic infection reported are majority of the relatively rare cases which are asymptomatic on the date of identification/report went on to develop disease[@who]."
                                                                 ))
                                               )
                                             )
                                    )
                                  )         
                        )
               ),
               
               tabPanel("Timeline Analysis",
                        fluidRow( class = "data-panel",
                                  navlistPanel(
                                    tabPanel("Major Events Timeline",
                                             tags$div(id = "timeline-header",
                                                      "We have identified points in time which are important steps towards the containment of the virus in Germany as well as in other countries of the world.",
                                                      tags$br(),
                                                      "For a better overview, we have specialized in the following countries: India, Germany, Italy, Brazil, US and Spain.",
                                                      tags$br(),
                                                      "The following table shows the events and dates presented in the interactive timeline."
                                             ),
                                             tags$div( class = "timeline-wrapper",
                                                       timevisOutput("timeline")
                                             )
                                    ),
                                    tabPanel("Timeline Plots",
                                             tabsetPanel( id = "timeline-plots",
                                                          tabPanel("Confirmed Plot",
                                                                   tags$div( class = "plot-wraper timeline-plot",
                                                                             plotlyOutput("confirmed_timeline_plot")
                                                                   )
                                                          ),
                                                          tabPanel("Recovered Plot",
                                                                   tags$div( class = "plot-wraper timeline-plot",
                                                                             plotlyOutput("recovered_timeline_plot")
                                                                   )
                                                          ),
                                                          tabPanel("Deaths Plot",
                                                                   tags$div( class = "plot-wraper timeline-plot",
                                                                             plotlyOutput("deaths_timeline_plot")
                                                                   )
                                                          )
                                             )
                                    )
                                  )
                        )
                        
               ),
               
               tabPanel("Twitter Analysis",
                        fluidRow( class = "data-panel",
                                  sidebarLayout(
                                    sidebarPanel(
                                      h1(class="radioSelect", radioButtons( "twittercharts", "Please select the chart below.",
                                                                            c("Sentiment with Most Contributed Words" = "tweet_plot1",
                                                                              "Tweet Counts with Emotions" = "tweet_plot2",
                                                                              "English Word Cloud with Emotions" = "tweet_plot3",
                                                                              "Sentiment Change with Time(Hour)" = "tweet_plot4",
                                                                              "German Tweets Word Cloud" = "tweet_plot5")),
                                         
                                         br(),
                                      )
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                      
                                      tags$h4( class = "tweet-header",
                                               uiOutput("tweet_header"),
                                      ),
                                      tags$div( class = "plot-wraper twitter-plot",
                                                uiOutput("tweet_plot")
                                      ),
                                      tags$div( class = "tweet-summary",
                                                uiOutput("tweet_summary")
                                      ),
                                      
                                    )
                                    
                                  )
                        )
               ),
               tabPanel("Final Analysis",
                        fluidRow( class = "data-panel",
                                  navlistPanel(
                                    
                                    tabPanel("Patient Medical Data",
                                             fluidRow(class = "final-analysis-content",
                                                      tags$p("Our overall goal was to exploit the sample patient dataset. Each observation of the dataset is related to the details of the confirmed COVID-19 patient such as age, DatOfOnsetSymptoms , DateOfDischarge etc.
Among 15466 observations, 6006 (38.83%)are Females and 9460 (61.17%) are Males. 766(4.95%) are 18 and below, 12059(77.97%) are between 20 and 60 inclusive, 2641(17.07%) above 60. The low count of children suggests that there is a relatively low attack rate in this age group. The median age is 45  years (range 1 year-100 years old; IQR 38-53 years old) with the majority of cases aged between 16–75 years."
                                                      ),
                                                      tags$p("Individuals with higher risk for severe disease and death include those with some underlying medical conditions such as hypertension, diabetes, cardiovascular disease, chronic respiratory disease and cancer. Our sample consists of 720(4.6%) patients with chronic diseases out of which 302(41.94%) are Female and 418(58.06%) are Male. Additionally, extracting each chronic disease from 149 observations(excluding missing values) and plotting its frequency showed us that people with hypertension(34.7%)and diabetes(24.3%) are more vulnerable to COVID-19."
                                                      ),
                                                      tags$p("Since people with chronic disease are likely to face an increased risk of developing severe symptoms and eventually die, we try to find the chances of their recovery and also compare it with those who don’t. The recovery:death ratio for patients with chronic disease is 17:49 and for the others is 53:8. This indeed proves that people having chronic disease, when infected by COVID19 have very low chances of recovery."
                                                      ),
                                                      tags$p("
Based on 1644 confirmed cases (excluding observations with missing values for Symptoms)  collected until March 2020, typical signs and symptoms include: Fever (32.65%), Dry Cough(18.38%), sore throat,(3.76%) pneumonia (3.5%), fatigue(2.5%), malaise(2.5%), rhinorrhea(2.3%), headache(2.23%), myalgias(2.22%), shortness of breath(1.9%), sputum(1.5%) etc. Focusing on Wuhan City, we plot a pie chart to see the initially seen symptoms. "
                                                      ),
                                                      tags$p("Fever was seen in the majority of the cases (44.4%) on its own as well as with other symptoms like cough(28.1%), weakness(2.96%), sore throat(2.96%) and fatigue(2.96%).The outbreak soon spread from China to other parts of the world. We use the geographical locations of the patients  provided in our dataset to find the places that were affected or not affected from COVID-19. The map reveals that the virus was spread from Chinese city of Wuhan to more than 180 countries and territories affecting every continent except Antarctica. In addition to chronic disease, age also influences  the level of risk for disease and death.People aged more than 60 are at a higher risk than those below 60 can be concluded with the help of the statistical hypothesis testing such as t-test. Creating a separate data frame of those who recovered (372 observations) , we create a boxplot that shows the median age as 45 (IQR 30-53) and also the average age to be 43.29.  We then create another data frame of those who died and created a boxplot. The median in this plot is 67 (IQR 55-79)."
                                                      ),
                                                      tags$p("According to WHO, the recovery time tends to be about two weeks for those with mild symptoms and about 3-6 weeks for those with severe or critical disease. However, these seem to be only rough guidelines as studies have already shown a number of exceptions. We plot the timeline for recovery for some patients and see variations in the number of days taken for recovery. With this, we can conclude that a window of 2-4 weeks can be considered as recovery time. Similarly, we plot the timeline for death for some patients. In this case, most of the patients died within 3 weeks whereas the majority of the patients older than 70 took less than 2 weeks.These are analysis that we have made on the COVID-19 patient medical data."
                                                      )        
                                             )
                                    ),
                                    tabPanel("Time Series Prediction",
                                             fluidRow(class = "final-analysis-content",
                                                      tags$p("We need to know that no prediction is certain as long as once in a while the past repeats. There are different factors that come into play while doing the prediction such as psychological which emphasizes more on how people distinguish and react in a dangerous situation, availability of data and the variable used. Assuming that the information used is reliable which in future will follow the past trends of the disease, our forecasts say that there will be an increment within the confirmed COVID-19 cases ( deaths and recovered ) with a slight instability."
                                                      ),
                                                      tags$p("We can see that in Germany the restrictions have taken important steps towards the containment of the virus. This has led to fewer deaths and confirmed cases, as for example in the US.  
It is interesting to note that the strict Spanish restrictions on the virus have made only a small difference to the less stringent restrictions in Germany (confirmed cases)."
                                                      )
                                             )
                                    ),
                                    tabPanel("Twitter Analysis",
                                             fluidRow(class = "final-analysis-content",
                                                      tags$p( "The objective of this Twitter sentiment analysis was to identify the emotions and sentiment direction of the public during the corona virus outbreak. Based on the plots, it is revealed that people had more positive sentiment towards the situation rather than the negative feelings. Furthermore , it is identified that anticipation and trust are the most 
expressed emotions during the pandemic. When analyzed the frequent words used to express sentiments, it is found that \"ugh\", \"die\", \"miss\" , \"sue\", \"worse\" are the words used frequently to express negative sentiment while, \"good\", \"trump\", \"love\", \"hug\", \"wow\" are the words used for positive sentiment."
                                                      ),
                                                      tags$p("When analyzed the word cloud for German tweets, we could identify some of the words like \"schon\", \"deutschland\", \"pandamie\", \"lockdown\", \"youtube\" etc have been used frequently in the tweets."
                                                      )
                                             )
                                             
                                    )
                                  )
                        )
               ),
               tabPanel("Resources",
                        fluidRow( class = "data-panel",
                                  navlistPanel(
                                    
                                    tabPanel("R Markdown process notebook",
                                             fluidRow(class = "resources-content",
                                                      tags$a( href = "https://shivanihegde.github.io/Data-Science-with-R-2020/Process%20notebook/notebook_final.html", target="_blank",
                                                              tags$img( id = "r-logo", src="R-logo.png")
                                                      )
                                             )
                                    ),
                                    tabPanel("Source Code",
                                             fluidRow(class = "resources-content",
                                                      tags$a( href = "https://github.com/ShivaniHegde/Data-Science-with-R-2020", target="_blank",
                                                              tags$img( id = "git-logo", src="git-logo.png")
                                                      )
                                             )
                                    ),
                                    tabPanel("Datasets",
                                             fluidRow(class = "dataset-content",
                                                      tags$div( class= "data-link",
                                                                tags$a( href="https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases", target="_blank",
                                                                        "John Hopkins timeseries datasets"
                                                                )
                                                      ),
                                                      tags$div( class= "data-link",
                                                                tags$a( href="https://datarepository.wolframcloud.com/resources/Patient-Medical-Data-for-Novel-Coronavirus-COVID-19", target="_blank",
                                                                        "Patient Medical Dataset"       
                                                                )
                                                      )
                                             )
                                             
                                    ),
                                    tabPanel("References",
                                             fluidRow(class = "resources-content references-content",
                                                      tags$div(tags$ol(
                                                        tags$br(),
                                                        tags$li(tags$span("Dey, Samrat K., et al. \"Analyzing the epidemiological outbreak of COVID-19: A visual exploratory data analysis approach.\" Journal of medical virology 92.6 (2020): 632-638")),
                                                        tags$br(),
                                                        tags$li(tags$span("Gupta, Rajan, and Saibal Kumar Pal. \"Trend Analysis and Forecasting of COVID-19 outbreak in India.\" medRxiv (2020).")),
                                                        tags$br(),
                                                        tags$li(tags$span(" Singh, Sarbjit, et al. \"Development of new hybrid model of discrete wavelet decomposition and autoregressive integrated moving average (ARIMA) models in application to one month forecast the casualties cases of COVID-19.\" Chaos, Solitons & Fractals (2020): 109866")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://ncase.me/covid-19/ - 21.06.2020")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://www.zdf.de/nachrichten/heute/coronavirus-ausbreitung-infografiken-102.html - 21.06.2020")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://www.bundesgesundheitsministerium.de/coronavirus/chronik-coronavirus.html - 21.06.2020")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://www.mdr.de/nachrichten/politik/corona-chronik-chronologie-coronavirus-100.html - 21.06.2020")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://towardsdatascience.com/r-tutorial-analyzing-covid-19-data-12670cd664d6 ")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://www.rivm.nl/en/novel-coronavirus-covid-19/children-and-covid-19")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf?sfvrsn=fce87f4e_2")),
                                                        tags$br(),
                                                        tags$li(tags$span("https://edition.cnn.com/2020/03/24/health/coronavirus-gender-mortality-intl/index.html")),
                                                        tags$br(),
                                                        tags$li(tags$span("Manguri, Kamaran & Ramadhan, Rebaz & Mohammed Amin, Pshko. (2020). Twitter Sentiment Analysis on Worldwide COVID-19 Outbreaks. Kurdistan Journal of Applied Research. 54-65. 10.24017/covid.8.")),
                                                        tags$br(),
                                                        tags$li(tags$span("Dubey, Akash Dutt, Twitter Sentiment Analysis during COVID-19 Outbreak (April 9, 2020). Available at SSRN: https://ssrn.com/abstract=3572023 or http://dx.doi.org/10.2139/ssrn.3572023"))
                                                      )
                                                      )
                                             )
                                    )
                                  )
                        )
               )
               
               # tabPanel("References",
               #          fluidRow( class = "data-panel refrences-panel",
               #                    tags$div(tags$ol(
               #                      tags$br(),
               #                      tags$li(tags$span("Dey, Samrat K., et al. \"Analyzing the epidemiological outbreak of COVID-19: A visual exploratory data analysis approach.\" Journal of medical virology 92.6 (2020): 632-638")),
               #                      tags$br(),
               #                      tags$li(tags$span("Gupta, Rajan, and Saibal Kumar Pal. \"Trend Analysis and Forecasting of COVID-19 outbreak in India.\" medRxiv (2020).")),
               #                      tags$br(),
               #                      tags$li(tags$span(" Singh, Sarbjit, et al. \"Development of new hybrid model of discrete wavelet decomposition and autoregressive integrated moving average (ARIMA) models in application to one month forecast the casualties cases of COVID-19.\" Chaos, Solitons & Fractals (2020): 109866")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://ncase.me/covid-19/ - 21.06.2020")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://www.zdf.de/nachrichten/heute/coronavirus-ausbreitung-infografiken-102.html - 21.06.2020")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://www.bundesgesundheitsministerium.de/coronavirus/chronik-coronavirus.html - 21.06.2020")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://www.mdr.de/nachrichten/politik/corona-chronik-chronologie-coronavirus-100.html - 21.06.2020")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://towardsdatascience.com/r-tutorial-analyzing-covid-19-data-12670cd664d6 ")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://www.rivm.nl/en/novel-coronavirus-covid-19/children-and-covid-19")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://www.who.int/docs/default-source/coronaviruse/who-china-joint-mission-on-covid-19-final-report.pdf?sfvrsn=fce87f4e_2")),
               #                      tags$br(),
               #                      tags$li(tags$span("https://edition.cnn.com/2020/03/24/health/coronavirus-gender-mortality-intl/index.html")),
               #                      tags$br(),
               #                      tags$li(tags$span("Manguri, Kamaran & Ramadhan, Rebaz & Mohammed Amin, Pshko. (2020). Twitter Sentiment Analysis on Worldwide COVID-19 Outbreaks. Kurdistan Journal of Applied Research. 54-65. 10.24017/covid.8.")),
               #                      tags$br(),
               #                      tags$li(tags$span("Dubey, Akash Dutt, Twitter Sentiment Analysis during COVID-19 Outbreak (April 9, 2020). Available at SSRN: https://ssrn.com/abstract=3572023 or http://dx.doi.org/10.2139/ssrn.3572023"))
               #                    ))))
             )
  ))