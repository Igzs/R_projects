#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(ggmosaic)
#loading data

data("UCBAdmissions")
df_ucba <- tbl_df(UCBAdmissions)
# Define UI for application that draws a histogram
ui <- fluidPage(
      navbarPage("UCBAdmissions Shiny App",
                 tabPanel("Documentation",
                          h1("Student Admissions At UC Berkeley"),
                          p("Welcome to the UCBAdmission Shiny App"),
                          
                          h2("How to use this shiny application:"),
                          p("Navigate through the application using the navigation bar.",br(),"Interact with the different visualizations using the graphical interface."),
                          
                          h2("About the dataset:"),
                          h3("Details"),
                          p("This data set is frequently used for illustrating Simpson's paradox, see Bickel et al (1975).",br(),
                            " At issue is whether the data show evidence of sex bias in admission practices.",br(),
                            " There were 2691 male applicants, of whom 1198 (44.5%) were admitted, compared with 1835 female applicants of whom 557 (30.4%) were admitted.",br(),
                            " This gives a sample odds ratio of 1.83, indicating that males were almost twice as likely to be admitted.", br(),
                            ),
                          h3("Data format"),
                          p("4526 observations, 4 columns",br(),
                            "------------------",br(),
                            "Admit (Categorical) : Admissions status",br(),
                            "Gender (Categorical) : Gender of the candidate",br(),
                            "Dept (Categorical) : Department of application",br(),
                            "Freq (Numerical) : Number of occurences"),
                          h3("References"),
                          p("Bickel, P. J., Hammel, E. A., and O'Connell, J. W. (1975).",br(),
                            "Sex bias in gradinstuate admissions: Data from Berkeley. Science, 187, 398--403. http://www.jstor.org/stable/1739581.")
                         ),
                 
                 tabPanel("Application",
                          fluidRow(
                            titlePanel(h1("Percentage of admissions by Gender",align="center")),
                            
                            column(4,
                                   wellPanel(
                                     radioButtons("gender", "Gender:",
                                                  c("Male","Female")
                                     )
                                   )       
                            ),
                            
                            column(8,
                                   plotOutput("piechart",height = "100%")
                            )
                          ),
                          fluidRow(
                            column(4,
                                   h3(textOutput("text"))
                            )
                          )
                      
                 ),
                tabPanel("Details",
                         # Application title
                         titlePanel(h1("UC Berkeley Student Admissions by Gender and Department",align="center")),
                         
                         # Sidebar with a slider input for number of bins 
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("dept", "Department : ", 
                                         choices=c("All",unique(df_ucba["Dept"]))),
                             hr(),
                             helpText("Visual reprentation of admissions by gender in the selected department"),
                             width=2
                           ),
                           
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotOutput("mosaicplot",width = "100%")
                           )
                         )
                       
                       )
          
      )
)

# Define server logic required to draw plots
server <- function(input, output) {
    
    #create the mosaic plot
    #the formula for the mosaic plot is the following : Gender ~ Dept + Admit | n
    output$mosaicplot <- renderPlot({
      
      if(input$dept=="All"){
        ggplot(data = df_ucba) +
          geom_mosaic(aes(weight= n, x = product(Dept,Admit), fill=Admit)) + 
          facet_grid(Gender~.) + 
          scale_fill_manual(values=c("#56B4E9", "#D46A6A"))

      }else{
        #filter the dataframe according to the input
          df_plot <- df_ucba %>% filter(Dept==input$dept)
         ggplot(data = df_plot) +
            geom_mosaic(aes(weight= n, x = product(Dept,Admit), fill=Admit)) +
            facet_grid(Gender~.) + 
            scale_fill_manual(values=c("#56B4E9", "#D46A6A"))
      }
    })
  #create the piechart plot  
  output$piechart <- renderPlot({
    #filter the dataframe according to the input
    df_ucba <- df_ucba %>% filter(Gender== input$gender)
    #retrieve the total in a variable
    total <- as.integer(df_ucba %>% summarise(sum(n)))
    output$text  <- renderText({
      paste(total, input$gender, "applicants")
    })
    #modify the dataframe to be used in the plot
    #compute total percentage for aggregated data
    df_plot <- df_ucba %>% group_by(Admit,Gender)  %>% mutate(Percent = (n/total)*100) %>% summarise(Percent = sum(Percent))
    ggplot(df_plot, aes(x="", y=Percent, fill=Admit))+
          geom_bar(width = 1, stat = "identity") + 
          #transform the barplot into a pie chart
          coord_polar("y", start=0) + 
          #percentage labels inside the pie chart
          geom_text(aes(label=Percent),color='white',position = position_stack(vjust = 0.5)) + 
          #labels
          labs(title = paste("Percentage of",input$gender,"admissions"),
               y = "Admission percentage") +
          #colors
          scale_fill_manual(values=c("#56B4E9", "#D46A6A"))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
