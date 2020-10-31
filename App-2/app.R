library(shiny)
library(tibble)
library(RCurl)
library(knitr)
library(plyr)
library(dplyr)
library(ggbiplot)
library(rCharts)
library(qcc)
library(threejs)
library(rgl)
library(pca3d)
library(gridExtra)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Case Study 3"),
  headerPanel("Exploring the Breast Cancer Dataset"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      h4("Choose the columns of your data to include in the PCA."),
               uiOutput("choose_columns_pca"),
               tags$hr(),
               "Select options for the PCA computation (we are using the prcomp function here)",
               radioButtons(inputId = 'center',  
                            label = 'Center',
                            choices = c('Shift variables to be zero centered'='Yes',
                                        'Do not shift variables'='No'), 
                            selected = 'Yes'),
               
               radioButtons('scale.', 'Scale',
                            choices = c('Scale variables to have unit variance'='Yes',
                                        'Do not scale variables'='No'), 
                            selected = 'Yes')
               
      ),
    # Main panel for displaying outputs ----
    mainPanel(
      h3("Variance Explained by PC"),
      showOutput("chart", "highcharts")
      )      
    )
)

server <- function(input, output) {
  # Read and clean the data
  bc <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer/breast-cancer.data'
  bc_dat <- read.csv(bc)
  bc_df <-data.frame(bc_dat)
  colnames(bc_df)<-c('class','age','menopause','tumorSize','invNodes','nodeCaps',
                     'degMalig','breast','breastQuad','irraDiat')
  bc_tibble <- tibble(bc_df)
  bc_tibble <- bc_tibble %>% mutate(class=case_when(bc_tibble$class =='no-recurrence-events' ~ 0,
                                                    bc_tibble$class=='recurrence-events' ~ 1)) %>% 
    mutate(age=case_when(bc_tibble$age=='10-19'~0,
                         bc_tibble$age=='20-29'~ 1,
                         bc_tibble$age=='30-39'~ 2,
                         bc_tibble$age=='40-49'~ 3,
                         bc_tibble$age=='50-59'~ 4,
                         bc_tibble$age=='60-69'~ 5,
                         bc_tibble$age=='70-79'~ 6,
                         bc_tibble$age=='80-89'~ 7,
                         bc_tibble$age=='90-99'~ 8)) %>%
    mutate(menopause=case_when(bc_tibble$menopause=='lt40' ~ 0,
                               bc_tibble$menopause=='ge40' ~ 1,
                               bc_tibble$menopause=='premeno' ~ 2)) %>%
    mutate(tumorSize=case_when(bc_tibble$tumorSize=='0-4' ~ 0,
                               bc_tibble$tumorSize=='5-9' ~ 1,
                               bc_tibble$tumorSize=='10-14' ~ 2,
                               bc_tibble$tumorSize=='15-19' ~ 3,
                               bc_tibble$tumorSize=='20-24' ~ 4,
                               bc_tibble$tumorSize=='25-29' ~ 5,
                               bc_tibble$tumorSize=='30-34' ~ 6,
                               bc_tibble$tumorSize=='35-39' ~ 7,
                               bc_tibble$tumorSize=='40-44' ~ 8,
                               bc_tibble$tumorSize=='45-49' ~ 9,
                               bc_tibble$tumorSize=='50-54' ~ 10,
                               bc_tibble$tumorSize=='55-59' ~ 11)) %>%
    mutate(invNodes=case_when(bc_tibble$invNodes=='0-2'~0,
                              bc_tibble$invNodes=='3-5'~1,
                              bc_tibble$invNodes=='6-8'~2,
                              bc_tibble$invNodes=='9-11'~3,
                              bc_tibble$invNodes=='12-14'~4,
                              bc_tibble$invNodes=='15-17'~5,
                              bc_tibble$invNodes=='18-20'~6,
                              bc_tibble$invNodes=='21-23'~7,
                              bc_tibble$invNodes=='24-26'~8,
                              bc_tibble$invNodes=='27-29'~9,
                              bc_tibble$invNodes=='30-32'~10,
                              bc_tibble$invNodes=='33-35'~11,
                              bc_tibble$invNodes=='36-39'~12)) %>%
    mutate(nodeCaps=if_else(bc_tibble$nodeCaps=='no',0,1)) %>%
    mutate(breast=if_else(bc_tibble$breast=='right',0,1)) %>%
    mutate(breastQuad=case_when(bc_tibble$breastQuad=='left_up'~0, 
                                bc_tibble$breastQuad=='left_low'~1,
                                bc_tibble$breastQuad=='right_up'~2, 
                                bc_tibble$breastQuad=='right_low'~3,
                                bc_tibble$breastQuad=='central'~4)) %>%
    mutate(irraDiat=if_else(bc_tibble$irraDiat=='no',0,1))
  bc_tibble<- bc_tibble %>%filter(breastQuad>=0)
  
  # Check boxes to choose columns
  output$choose_columns_pca <- renderUI({
    
    the_data <- bc_tibble
    
    # Get the data set with the appropriate name
    
    
    colnames <- names(bc_tibble)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
    
  })
  
  pca_objects <- reactive({
    # Keep the selected columns
    columns <-    input$columns
    the_data <- na.omit(sapply(bc_tibble,as.numeric))
    the_data_subset <- na.omit(the_data[, columns, drop = FALSE])
    
    # from http://rpubs.com/sinhrks/plot_pca
    pca_output <- prcomp(na.omit(the_data_subset), 
                         center = (input$center == 'Yes'), 
                         scale. = (input$scale. == 'Yes'))
    # data.frame of PCs
    pcs_df <- cbind(the_data, pca_output$x)
    
    return(list(the_data = the_data, 
                the_data_subset = the_data_subset,
                pca_output = pca_output, 
                pcs_df = pcs_df))
    
})

  output$chart <- renderChart2({
    pca_output <- pca_objects()$pca_output
    eig = (pca_output$sdev)^2
    variance <- round(eig*100/sum(eig),2)
    cov <- data.frame(c(1:length(input$columns)), variance )
    names(cov)[1] = 'PCs'
    names(cov)[2] = 'Variance'
    p1 <-hPlot(Variance~PCs, data=cov, type=c('line'), radius=7)
    return(p1)
  })
    }


shinyApp(ui=ui, server=server)
