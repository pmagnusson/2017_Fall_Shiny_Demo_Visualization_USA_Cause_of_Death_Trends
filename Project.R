
# Project Data Source Code


require(ggplot2)
require(ggthemes)
require(haven)
require(lubridate)
require(scales)
require(xlsx)
require(dplyr)
require(shiny)
require(shinythemes)


setwd("C:/Users/Patrick Magnusson/Google Drive/UTSA/Course 1 - DA 6233 Data Visualization & Communication/Final Project")

USvTX = read.xlsx(file = "Project Data - Compressed Mortality 1999-2015.xlsx", 
                  sheetIndex = 1, as.data.frame = TRUE, header = TRUE, 
                  stringsAsFactors = TRUE)


Top10YrSt = read.xlsx(file = "Project Data 2 - Top 10 by Year.xlsx", 
                  sheetIndex = 1, as.data.frame = TRUE, header = TRUE, 
                  stringsAsFactors = TRUE)

dataFinal = read.xlsx(file = "Project Data - Final US and Tx.xlsx", 
                      sheetIndex = 1, as.data.frame = TRUE, header = TRUE, 
                      stringsAsFactors = TRUE)

dataFinalHis = read.xlsx(file = "Project Data Final - Raw Data US and Tx Hispanic.xlsx", 
                      sheetIndex = 1, as.data.frame = TRUE, header = TRUE, 
                      stringsAsFactors = TRUE)

dataFinalHis$Disease.Short.Name = factor(dataFinalHis$Disease.Short.Name, levels=c("Diseases of the Heart","Cancers","Diseases of Respiratory System", "External Causes",
                                                                                   "Diseases of the Nervous System", "Diseases of the Endocrine System", "Mental and Behavioural Disorders",
                                                                                   "Diseases of the Digestive System", "Infectious and Parasitic Diseases",
                                                                                   "Diseases of the Genitourinary System"))

dataFinalHis = subset(dataFinalHis, Population != "Not Applicable")




 #Goals:
 #Top 10 causes of death in Time Series
 #Shiny App to allow user selection of which causes to display
 #Show aggregate, by race, and by gender (or both?)
 #Show by raw  or crude rate? Most likely crude to level set


USvTX$Crude.Rate = as.numeric(as.character(USvTX$Crude.Rate))

dataFinal$Disease.Short.Name = factor(dataFinal$Disease.Short.Name, levels=c("Diseases of the Heart","Cancers","Diseases of Respiratory System", "External Causes",
                                                                             "Diseases of the Nervous System", "Diseases of the Endocrine System", "Mental and Behavioural Disorders",
                                                                             "Diseases of the Digestive System", "Infectious and Parasitic Diseases",
                                                                             "Diseases of the Genitourinary System"))


USA = subset(dataFinal, dataFinal$State=="United States" & dataFinal$Year > 1)
USA.ALL = dataFinal %>% dplyr::filter(State == "United States") %>%
   dplyr::filter(Year > 1) %>%
   group_by(Disease.Short.Name, Year) %>%
   summarise(Sum_Deaths = sum(Deaths), Sum_Crude = sum(Crude.Rate))

TX = subset(dataFinal, dataFinal$State=="Texas" & dataFinal$Year > 1)
TX.ALL = dataFinal %>% dplyr::filter(State == "Texas") %>%
   dplyr::filter(Year > 1) %>%
   group_by(Disease.Short.Name, Year) %>%
   summarise(Sum_Deaths = sum(Deaths), Sum_Crude = sum(Crude.Rate))





usaTrend = ggplot(data=USA.ALL, 
                  aes(x=Year, y=Sum_Crude, 
                      color=Disease.Short.Name,
                      group=Disease.Short.Name)) +
     geom_point(size=2.5) +
     geom_line(size=1.25) +
     xlab("Year") +
     ylab("Deaths (Crude Rate)") +
     labs(color="Cause of Death") +
     theme_gdocs()
usaTrend


texasTrend = ggplot(data=TX.ALL, 
                    aes(x=Year, y=Sum_Crude, 
                        color=Disease.Short.Name,
                        group=Disease.Short.Name)) +
     geom_point(size=2.5) +
     geom_line(size = 1.25) +
     xlab("Year") +
     ylab("Deaths (Crude Rate)") +
     labs(color="Cause of Death") +
     theme_gdocs()
texasTrend


TSRace = reactive( {
   if(input$inputTSRace == "White") {return("White")}
   if(input$inputTSRace == "Black or African American") {return("Black or African American")}
   if(input$inputTSRace == "Asian or Pacific Islander") {return("Asian or Pacific Islander")}
   if(input$inputTSRace == "American Indian or Alaska Native") {return("American Indian or Alaska Native")}
   if(input$inputTSRace == "All") {return(c("White","Black or African American",
                                            "Asian or Pacific Islander",
                                            "American Indian or Alaska Native"))}      
}
)

TSGender = reactive( {
   if(input$inputTSGender == "Male") {return("Male")}
   if(input$inputTSGender == "Female") {return("Female")}
   if(input$inputTSGender == "All") {return(c("Male","Female"))}
}
)

TSregion = reactive( {
   if(input$inputTSRegion == "United States") {return("United States")}
   if(input$inputTSRegion == "Texas") {return("Texas")}
   if(input$inputTSRegion == "USA vs Texas") {return(c("United States", "Texas"))}
}
)

TSCause = reactive( {
   if(input$inputTSCause == "Diseases of the Heart") {return("Diseases of the Heart ")}
   if(input$inputTSCause == "Cancers") {return("Cancers")}
   if(input$inputTSCause == "Diseases of Respiratory System") {return("Diseases of Respiratory System")}
   if(input$inputTSCause == "External Causes") {return("External Causes")}
   if(input$inputTSCause == "Diseases of the Nervous System") {return("Diseases of the Nervous System")}
   if(input$inputTSCause == "Diseases of the Endocrine System") {return("Diseases of the Endocrine System")}
   if(input$inputTSCause == "Mental and Behavioural Disorders") {return("Mental and Behavioural Disorders")}
   if(input$inputTSCause == "Diseases of the Digestive System") {return("Diseases of the Digestive System")}
   if(input$inputTSCause == "Infectious and Parasitic Diseases") {return("Infectious and Parasitic Diseases")}
   if(input$inputTSCause == "Diseases of the Genitourinary System") {return("Diseases of the Genitourinary System")}
   if(input$inputTSCause == "All") {return(levels(dataFinal$Disease.Short.Name))}
})









