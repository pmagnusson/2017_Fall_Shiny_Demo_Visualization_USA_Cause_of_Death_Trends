

# Final Project Code
# Module 4 - Time Series Data
# Patrick Magnusson
# UTSA Data Visualization & Communication


require(ggplot2)
require(ggthemes)
require(haven)
require(lubridate)
require(scales)
require(xlsx)
require(dplyr)
require(shiny)
require(shinythemes)

#dataFinalHis = read.xlsx(file = "Project Data Final - Raw Data US and Tx Hispanic.xlsx", 
#                         sheetIndex = 1, as.data.frame = TRUE, header = TRUE, 
#                         stringsAsFactors = TRUE)

#dataFinalHis$Disease.Short.Name = factor(dataFinalHis$Disease.Short.Name, levels=c("Diseases of the Heart","Cancers","Diseases of Respiratory System", "External Causes",
#                                                                                   "Diseases of the Nervous System", "Diseases of the Endocrine System", "Mental and Behavioural Disorders",
#                                                                                   "Diseases of the Digestive System", "Infectious and Parasitic Diseases",
#                                                                                   "Diseases of the Genitourinary System"))

#dataFinalHis = subset(dataFinalHis, Population != "Not Applicable")

ui = navbarPage(title = "CDC Top 10 Causes of Death Statistics",
     theme = shinytheme("cosmo"),
     tabPanel(title = "Time Series: USA and Texas",
              fluidRow(column(3,
                    wellPanel(
                   #$head(
                   #tags$style(type="text/css", '#leftTSPanel { max-width:300px; margin:auto; padding:10px;}')),
                   #id="leftTSPanel",
                   selectInput(inputId = "inputTSRace", label = "Select an Ethnicity Set:",
                               choices = c("All",levels(dataFinalHis$Race)),
                               selected="All"),
                   conditionalPanel(
                      condition = "input.inputTSRace != 'All'",
                      selectInput(inputId = "inputTSHispanic", label = "Hispanic/Latino Designation?",
                                  choices = c("All","Hispanic or Latino","Not Hispanic or Latino"),
                                  selected = "All")
                   ),
                   selectInput(inputId = "inputTSGender", label="Select a Gender Set:",
                               choices = c("All", levels(dataFinalHis$Gender)),
                               selected="All"),
                   selectInput(inputId = "inputTSRegion", label = "Select a Region:",
                               choices = c("United States", "Texas"),
                               selected = "United States"),
                   selectInput(inputId = "inputTSCause", label = "Select a Condition:", 
                               choices = c("All",levels(dataFinalHis$Disease.Short.Name)),
                               selected = "All"),
                   conditionalPanel(
                        condition = "input.inputTSCause != 'All'",
                        selectInput(inputId = "inputTSCauseCompare", label = "Compare Cause",
                                    choices = levels(dataFinalHis$Disease.Short.Name),
                                    selected = NULL)
                   ),
                   radioButtons(inputId = "inputTSScale", label = "Scale", choices = c("Levels", "Log 10"))
                    )),
                   column(8,
                         wellPanel(
                              plotOutput("plotTS", height = 850)
                         )
                   ),
                   column(1, {})
          )
     )
)
 



server = function(input, output) {
      
      dataSub1 = reactive( {
         if(input$inputTSRace != "All") {subset(dataFinalHis, Race %in% input$inputTSRace)}
            else {dataFinalHis}
      }
      )
      
      dataSub2 = reactive({
         if(input$inputTSHispanic != "All") {subset(dataSub1(), Hispanic.Origin %in% input$inputTSHispanic)}
         else {dataSub1()}
      }
      )
      
      dataSub3 = reactive({
         if(input$inputTSGender != "All") {subset(dataSub2(), Gender %in% input$inputTSGender)}
         else {dataSub2()}
      }
      )
      dataSub4 = reactive({
         subset(dataSub3(), State %in% input$inputTSRegion)
      }
      )
      
      dataSub5 = reactive({
         if(input$inputTSCause != "All") {subset(dataSub4(), Disease.Short.Name %in% c(input$inputTSCause, input$inputTSCauseCompare))}
         else {dataSub4()}
      }
      )
     TSscaleUsed = reactive({
          if(input$inputTSScale == "Levels") {return("identity")}
          else if (input$inputTSScale=="Log 10") {return("log10")}
     })
     
     plotData = reactive(dataSub5() %>%
        dplyr::filter(Year > 1) %>%
        group_by(State, Disease.Short.Name, Year) %>%
        summarise(Sum_Deaths = sum(Deaths), Sum_Crude = sum(Crude.Rate)))
     
     output$plotTS = renderPlot( {
     
          usaTrend = ggplot(data=plotData(), 
                           aes(x=Year, y=Sum_Crude, 
                              color=Disease.Short.Name,
                              group=Disease.Short.Name)) +
               geom_point(size=2.5) +
               geom_line(size=1.25) +
               xlab("Year") +
               ylab("Deaths (Crude Rate)") +
               labs(color="Cause of Death         ") +
               theme_economist_white() +
               scale_y_continuous(trans=TSscaleUsed()) +
               theme(axis.title = element_text(size=16, face="bold")) +
               theme(legend.title = element_text(size = 14, face = "bold")) +
               theme(legend.position = 'right') +
               theme(legend.key = element_rect(size = 2)) +
               theme(legend.key.size = unit(2 , 'lines')) +
               theme(legend.text = element_text(size = 11)) 
          
          usaTrend
          
          
      }    
     )
}


shinyApp(ui = ui, server = server)





