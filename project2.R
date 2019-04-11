library(shiny)
library(MASS)
library(stringr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

ui <- fluidPage(
  
  titlePanel("Working in tech"),
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("radio", h3("Choose type of data to show"),
                   choices = c("Show dynamic employee-given scores" = 1, "Show feedback analysis" = 2),
                   selected = 1),
      conditionalPanel(condition = "input.radio == '1'", 
          checkboxGroupInput("job", 
                             h3("What people do"), 
                             choices = list("Management" = "Management", 
                                            "Office" = "Office", 
                                            "Engineer" = "Engineer",
                                            "Specialist"= "Specialist",
                                            "Other" = "Other"),
                             selected = c("Management","Office")),
          selectInput("company", h3("Where people work"), 
                                 choices = list("microsoft" = "microsoft", 
                                                "google" = "google", 
                                                "amazon" = "amazon",
                                                "facebook"=  "facebook",
                                                "apple" = "apple",
                                                "netflix" = "netflix"), selected = "microsoft"),
          
          sliderInput("slider", h3("When responses were taken"),
                      min = 2008, max = 2018, value = c(0, 1))  
        )
      ),
    # main panel: depending on the radio button either wordclouds or 
    # the bar charts are shown
    mainPanel(
      conditionalPanel(condition = "input.radio == '1'",
                       tabsetPanel(id="tab_numeric",
                         tabPanel("Culture",h2("Employees' scores of company's culture"), plotOutput("culture_plot")), 
                         tabPanel("Management",h2("Employees' scores of company's senior management"), plotOutput("management_plot")), 
                         tabPanel("Work-life balance",h2("Employees' scores of work-life balance in the company"), plotOutput("balance_plot"))
                       )),
      conditionalPanel(condition = "input.radio == '2'",
         tabsetPanel(id="tab_text",
                     tabPanel("Pros",h2("Bright side of employees' experience"), imageOutput("Pros")), 
                     tabPanel("Cons",h2("Dark side of employees' experience"), imageOutput("Cons")),
                     tabPanel("Summary",h2("General summary"), imageOutput("Summary"))
         ))
      
    )
  )
)
 

server <- function(input, output) {
  df = read.csv("./proj2_proprocessed.csv")
  
  # get values from the inputs
  reactTest = reactiveValues()
  reactTest$val = NULL
  
  observeEvent(input$job, {
    reactTest$val = input$job
    }, ignoreNULL = FALSE)
  
  observeEvent(input$slider, {
    reactTest$slider = input$slider
  }, ignoreNULL = FALSE)
  
  
  companyInput = reactive({switch(input$company,
                                  "microsoft", 
                                  "google" = "google" , 
                                  "amazon" = "amazon",
                                  "facebook" = "facebook",
                                  "apple" = "apple",
                                  "netflix" = "netflix")})
  
  # bar chart for the culture of the company
  output$culture_plot = renderPlot({
    comp = companyInput()
    jobs = reactTest$val
    times = reactTest$slider
    culture = df[df$Position %in% jobs & df$company == comp & (df$dates >= times[1]) & (df$dates <= times[2]),]$culture.values.stars
    clean_cul = as.numeric(culture[as.character(culture) != "none"])
    if (length(clean_cul) != 0){
      fr = table(clean_cul)
      barplot(fr,xlab="Score given by employees", ylab="Frequency")
    } else {
      dummy = c(1,2,3)
      barplot(dummy, col = NA, border= NA, axes = FALSE)
    }
  })
  
  #bar chart for the senior managements' scores
  output$management_plot = renderPlot({
    comp = companyInput()
    jobs = reactTest$val
    times = reactTest$slider
    if (length(jobs) != 0){
      management = df[df$Position %in% jobs & df$company == comp & (df$dates >= times[1]) & (df$dates <= times[2]),]$senior.mangemnet.stars
      clean_man = as.numeric(substr(management[as.character(management) != "none"],1,1))
      fr = table(clean_man)
      barplot(fr,xlab="Score given by employees", ylab="Frequency")
    } else {
      dummy = c(1,2,3)
      barplot(dummy, col = NA, border= NA, axes = FALSE)
    }
  })
  
  #bar chart for work-life balance scores
  output$balance_plot = renderPlot({
    comp = companyInput()
    jobs = reactTest$val
    times = reactTest$slider
    if (length(jobs) != 0){
      balance = df[df$Position %in% jobs & df$company == comp& (df$dates >= times[1]) & (df$dates <= times[2]),]$work.balance.stars
      clean_bal = as.numeric(substr(balance[as.character(balance) != "none"],1,1))
      fr = table(clean_bal)
      barplot(fr,xlab="Score given by employees", ylab="Frequency")
    } else {
      dummy = c(1,2,3)
      barplot(dummy, col = NA, border= NA, axes = FALSE)
    }
  })
  
  
  #show summary wordcloud
  output$Summary = renderImage({
    return(list(
      src = "./Wordcloud_summary.png",
      contentType = "image/png",
      alt = "Ku"
    ))
  },deleteFile = FALSE)
  
  #show cons wordcloud
  output$Cons = renderImage({
    return(list(
      src = "./Wordcloud_bad.png",
      contentType = "image/png",
      alt = "Ku"
    ))
  },deleteFile = FALSE)
  
  #show pros wordcloud
  output$Pros = renderImage({
    return(list(
      src = "./Wordcloud_good.png",
      contentType = "image/png",
      alt = "Ku"
    ))
  },deleteFile = FALSE)
  
  
  
}

shinyApp(ui = ui, server = server)