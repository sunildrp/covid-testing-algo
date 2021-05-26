############################################################
#Program to develop two-test algorithm for Covid-19 testing#
############################################################
#The code uses shiny, gtools, data.table, tidyverse, ggplot2,dplyr, RColorBrewer, shinyBS, shinyFeedback, shinythemes packages and for loop functions#

library(shiny)
library(gtools)
library(data.table)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(shinyBS)
library(shinyFeedback)
library(shinythemes)

# Define UI for application
ui <- fluidPage(theme= shinytheme("united"),                tags$head(
                    tags$style(
                        HTML(".shiny-notification {
             margin: auto;
             width: 400px;
             text align: center;
             position:fixed;
             opacity: 1;
             
             bottom: calc(5%);
             left: calc(45%);
             }
             "
                        )
                    )
                ),
                titlePanel("Combining two different tests for COVID-19 diagnosis"),   # Application title
                sidebarLayout (position = "left",
                               sidebarPanel(width = 3,
                                            h3(strong("Input parameters")),
                                            "These are the starting point values that can be modified. 
                                            The test accuracy values provided below are the clinical estimates derived from", a("FIND's independent evaluation studies.", href = "https://www.finddx.org/sarscov2-eval-antigen/"),
                                            hr(),
                                            "Pelase select your parameters below:",
                                            hr(),
                                            numericInput (inputId = "P", 
                                                          label= "Population undergoing testing", 
                                                          min=0,
                                                          max=1000000000000,
                                                          value=1000),
                                            hr(),
                                            sliderInput(inputId = "pv",
                                                        label = "Infection prevalence",
                                                        min = 0,
                                                        max = 20,
                                                        value = 2),
                                            hr(),
                                            selectInput("sce", "Select test scenarios",
                                                        width = "100%", selected = NULL,
                                                        c("", "Ag test and RT-PCR", "Panbio Ag and NowCheck Ag", "Panbio Ag and SD Biosensor Ag", "NowCheck Ag and SD Biosensor Ag", "My tests")),
                                            hr(),
                                            textInput("name_test_1", "Test #1", width = "100%"),
                                            conditionalPanel(condition = "input.name_test_1 != ''",
                                                             fluidRow(
                                                                 
                                                                 column(width = 6, numericInput("sn1", "Sensitivity (%)", value = '', min = 0, max = 100)),
                                                                 column(width = 6, numericInput("sp1", "Specificity (%)", value = '', min = 0, max = 100)),
                                                                 column(width = 9, numericInput("t1", "Time to result (Hours)", value = '', min = 0, max = 96))
                                                             ),
                                                             bsPopover("t1", "",
                                                                       "Average time from sample collection to result", "bottom", trigger = "hover", options = NULL),   ##add explanation balloon
                                                             hr()
                                            ),
                                            textInput("name_test_2", "Test #2", width = "100%"),
                                            conditionalPanel(condition = "input.name_test_2 != ''",
                                                             fluidRow(
                                                                 
                                                                 column(width = 6, numericInput("sn2", "Sensitivity (%)", value = '', min = 0, max = 100)),
                                                                 column(width = 6, numericInput("sp2", "Specificity (%)", value = '', min = 0, max = 100)),
                                                                 column(width = 9, numericInput("t2", "Time to result (Hours)", value = '', min = 0, max = 96))
                                                             ),
                                                             bsPopover("t2", "",
                                                                       "Average time from sample collection to result", "bottom", trigger = "hover", options = NULL),   ##add explanation balloon
                                                             hr(),
                                                             fluidRow(
                                                                 column(width = 9, selectInput("ord", "Change the order of tests", c("Order 1", "Order 2")))
                                                             ),
                                                             bsPopover("ord", "",
                                                                       "This allows to switch the order of tests", "top", trigger = "hover", options = NULL),   ##add explanation balloon
                                                             hr(),
                                                             fluidRow(
                                                                 column(width = 6, numericInput("k1", "Kappa coeffecient (infected)", value = 0, min = -1, max = 1)),
                                                                 column(width = 6, numericInput("k0", "Kappa coeffecient (non-infected)", value = 0, min = -1, max = 1))
                                                             ),
                                                             bsPopover("k1", "",
                                                                       "Measure of agreement between two test results among infected", "bottom", trigger = "hover", options = NULL),   ##add explanation balloon
                                                             bsPopover("k0", "",
                                                                       "Measure of agreement between two test results among non-infected", "bottom", trigger = "hover", options = NULL)   ##add explanation balloon
                                            )
                               ),
                               mainPanel(
                                   tabsetPanel(id = "panels",
                                               tabPanel(h4(strong("About")), value = "about", 
                                                        hr(),
                                                        fluidRow(
                                                            splitLayout(cellWidths = c("3%", "65%", "30%"),
                                                                        "",
                                                                        HTML('<a href = "https://www.finddx.org"><img src= "FIND.png" width = "25%"/></a>'),
                                                                        #HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),
                                                                        #HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),
                                                                        #HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),
                                                                        #HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),
                                                                        # HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),
                                                                        # HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),  HTML('&emsp;'),
                                                                        # HTML('<a href = "https://www.ndm.ox.ac.uk"><img src= "NDM.png" width = "40%"/></a>'),
                                                                        #HTML('&emsp;'),
                                                                        HTML('<img src="OXFORD.png" width = "20%"/></a>'))),
                                                        br(),
                                                        h4(strong("Comparison scenarios:")),
                                                        br(),
                                                        p("Given the population under testing and prevalence of disease under consideration, this application predicts the sensitivities and specificities of combination of tests, predictive values, number of second tests required, and average time to result in the following scenarios: "),
                                                        p(strong("1: Baseline scenario:"), "Either one of the two tests are applied alone as the only diagnostic tool."),
                                                        p(strong("2: Sequential testing:"), "Two tests are applied one after another to confirm the first test's result:"),
                                                        p(strong("2.a: Confirmatory testing for positives:"), "The second test is applied only if the first test result is positive. This approach optimizes the specificity of combination testing, meaning you are aiming to minimize false positives."),
                                                        p(strong("2.b: Confirmatory testing for negatives:"), "The second test is applied only if the first test is negative. This approach optimizes the sensitivity of combination testing, meaning you are aiming to minimize false negatives."),
                                                        hr(),
                                                        h4(strong("Definitions:")),
                                                        br(),
                                                        p(strong("1.	Average time to result:"), "This is the average turnaround time for a testing strategy from the time of sample collection to the time of reporting of results to the subjects whom samples were collected from."),
                                                        p(strong("2.	Positive predictive value (PPV):"), "PPV is the probability that following a positive test result, the individual will truly have the disease."),
                                                        p(strong("3.	Negative predictive value (NPV):"), "PPV is the probability that following a negative test result, the individual will truly not have the disease."),
                                                        p("   PPV and NPV are directly related to prevalence and allow you to determine how likely it is a patient has the specific disease."),
                                                        p(strong("4.	Sensitivity:"), "Sensitivity is the proportion of population who test positive among all those who actually have the disease. "),
                                                        p(strong("5.	Specificity:"), "Specificity is the proportion of population who test negative among all those who actually do not have the disease. "),
                                                        p("   A highly sensitive test helps to rule out a disease when the test is negative and a highly specific test helps to rule in a disease when the test in positive. "),
                                                        hr(),
                                                        h4(strong("Limitations:")),
                                                        br(),
                                                        p("Due to a lack of diagnostic accuracies data, the current version of the application does not account for the potential differences in test performances depending on the test population (asymptomatic vs symptomatic; acute vs screening). The input data on the test accuracy estimates were selected from the independent clinical studies on symptomatic infections. "),
                                                        p("The average time to result is the function of inputs for the time to result for each test. It is suggested that the input parameter for this turnaround time reflect the total time for the test to inform result to a subject form the time of sample collection and incorporate the delays occurring due to shipping, batching of samples and reporting delays. The average output time to result should be interpreted based on the assumptions you have made while selecting input parameters for the time to result. "),
                                                        p("The user should consider that the default parameters of performance characteristics (sensitivity and specificity) of each Ag-based tests in the model are relative to that of RT-PCR and might not reflect thier true performance in the population. While combining Ag-test and RT-PCR, we empirically selected the input parameters for the test performance of Ag-test and RT-PCR taking into consideration the available knowledge to reflect the true performance of these tests in the population."),
                                                        hr(),
                                                        p(strong("Authors")),
                                                        p(a("Sunil Pokharel", href = "mailto:sunil.pokharel@ndm.ox.ac.uk"),
                                                          ", ",
                                                          a("Lisa J. White"),
                                                          ", ",
                                                          a("Jilian Sacks"),
                                                          ", ",
                                                          a("Camille Escadafal"),
                                                          ", ",
                                                          a("Amy Toporowski"),
                                                          ", ",
                                                          a("Sabine Dittrich")
                                                        ),
                                                        hr(),
                                                        br()
                                               ),
                                               
                                               tabPanel(h4(strong("Summary output")), value = "Summary Output",
                                                        hr(),
                                                        h4(strong("Summary of outputs:")),
                                                        hr(),
                                                        fluidRow(column (tableOutput("table_comparison_summary"), width = 10)
                                                        ),
                                                        hr(),
                                                        downloadButton("downloaddata1", "Download summary outputs"),
                                                        hr(),
                                                        br(),
                                                        h4(strong("Depending on your use case and situations, please select the parameters below to compare:")),
                                                        fluidRow(
                                                            splitLayout(cellWidths = c("30%", "50%"),
                                                                        div(class = "center-div",
                                                                            radioButtons("max_display", label = "", 
                                                                                         choices = c("Sensitivity (%)", "Specificity (%)", "Number of 2nd tests", "Time to result (Hours)", "True positives", "False positives", "True negatives", "False negatives", "Positive predictive value (%)", "Negative predictive value (%)"), inline = FALSE, width = '100px'),
                                                                        ),
                                                                        plotOutput("plot_comparison_summary"))),
                                                        hr(),
                                               ),
                                               tabPanel(h4(strong("Detailed output")), value = "Detailed Output",
                                                        h4(strong("Outputs details:")), 
                                                        fluidRow(column (tableOutput("table_comparison_details"), width = 10)
                                                        ),
                                                        hr(),
                                                        downloadButton("downloaddata2", "Download detailed outputs"),
                                                        hr(),
                                                        h4(strong("Comparison of outputs accross different testing scenarios:")), 
                                                        hr(),
                                                        fluidRow(
                                                            splitLayout(cellWidths = c("50%", "50%"),
                                                                        plotOutput("plot_comparison_sensitivity"), plotOutput("plot_comparison_specificity"))
                                                        ),
                                                        hr(),
                                                        fluidRow(
                                                            splitLayout(cellWidths = c("50%", "50%"),
                                                                        plotOutput("plot_comparison_Number_of_second_tests"), plotOutput("plot_comparison_Time_to_result"))
                                                        ),
                                                        hr(),
                                                        fluidRow(
                                                            splitLayout(cellWidths = c("50%", "50%"),
                                                                        plotOutput("plot_comparison_TP"), plotOutput("plot_comparison_TN"))
                                                        ),
                                                        hr(),
                                                        fluidRow(
                                                            splitLayout(cellWidths = c("50%", "50%"),
                                                                        plotOutput("plot_comparison_FP"), plotOutput("plot_comparison_FN"))
                                                        ),
                                                        hr(),
                                                        fluidRow(
                                                            splitLayout(cellWidths = c("50%", "50%"),
                                                                        plotOutput("plot_comparison_PPV"), plotOutput("plot_comparison_NPV"))
                                                        )
                                               ))
                               ))
)

# Define server logic required to create output tables
server<-(function(input, output, session) {
    observe({
        showNotification(id = "disclaimer_message", 
                         p(h3(strong ("Disclaimer")),
                           br(),
                           "Whilst every effort has been taken during the development of this tool/model for it to be as accurate and reliable as possible, it is important that the user understands that the outputs are a prediction based on the assumptions chosen through the input parameter values.", 
                           br(),
                           br(),
                           "The appropriate use of this tool/model and its output can contribute to effective policymaking, but misuse or misinterpretation of the output can mislead decision-making. Any decisions taken whist using these tools are the responsibility of the user and no liability whatsoever will be taken by the developers/authors of the tool."
                           , br(),
                           br(),
                         ),
                         duration = NULL, closeButton = FALSE, type = "warning", action = actionButton("understand_disclaimer", "I understand and wish to continue.")
        )
    })
    observeEvent(input$understand_disclaimer, {
        removeNotification("disclaimer_message")
    })
    
    # select test values based on predefined scenarios    
    observeEvent(input$sce, {
        if(input$sce == 'Ag test and RT-PCR'){
            updateTextInput(session, "name_test_1", value = "Ag test")
            updateTextInput(session, "name_test_2", value = "RT PCR")
            updateNumericInput(session, "sn1", value = 70)
            updateNumericInput(session, "sp1", value = 95)
            updateNumericInput(session, "sn2", value = 90)
            updateNumericInput(session, "sp2", value = 98)
            updateNumericInput(session, "t1", value = 0.5)
            updateNumericInput(session, "t2", value = 24)
        }
        
        if(input$sce == 'Panbio Ag and NowCheck Ag'){
            updateTextInput(session, "name_test_1", value = "Panbio Ag")
            updateTextInput(session, "name_test_2", value = "NowCheck Ag")
            updateNumericInput(session, "sn1", value = 86)
            updateNumericInput(session, "sp1", value = 99)
            updateNumericInput(session, "sn2", value = 89)
            updateNumericInput(session, "sp2", value = 97)
            updateNumericInput(session, "t1", value = 0.5)
            updateNumericInput(session, "t2", value = 0.5)
        }
        
        if(input$sce == 'Panbio Ag and SD Biosensor Ag'){
            updateTextInput(session, "name_test_1", value = "Panbio Ag")
            updateTextInput(session, "name_test_2", value = "SD Biosensor Ag")
            updateNumericInput(session, "sn1", value = 86)
            updateNumericInput(session, "sp1", value = 99)
            updateNumericInput(session, "sn2", value = 80)
            updateNumericInput(session, "sp2", value = 99)
            updateNumericInput(session, "t1", value = 0.5)
            updateNumericInput(session, "t2", value = 0.5)
        }
        
        if(input$sce == 'NowCheck Ag and SD Biosensor Ag'){
            updateTextInput(session, "name_test_1", value = "NowCheck Ag")
            updateTextInput(session, "name_test_2", value = "SD Biosensor Ag")
            updateNumericInput(session, "sn1", value = 89)
            updateNumericInput(session, "sp1", value = 97)
            updateNumericInput(session, "sn2", value = 80)
            updateNumericInput(session, "sp2", value = 99)
            updateNumericInput(session, "t1", value = 0.5)
            updateNumericInput(session, "t2", value = 0.5)
        }
        
        if(input$sce == 'My tests'){
            updateTextInput(session, "name_test_1", value = "")
            updateTextInput(session, "name_test_2", value = "")
            updateNumericInput(session, "sn1", value = "")
            updateNumericInput(session, "sp1", value = "")
            updateNumericInput(session, "sn2", value = "")
            updateNumericInput(session, "sp2", value = "")
            updateNumericInput(session, "t1", value = "")
            updateNumericInput(session, "t2", value = "")
        }
    })
    ## change test order
    observeEvent(input$'ord', {
        if(input$ord == "Order 1"){
            updateTextInput(session, "name_test_1", value = input$name_test_2)
            updateTextInput(session, "name_test_2", value = input$name_test_1)
            updateNumericInput(session, "sn1", value = input$sn2)
            updateNumericInput(session, "sp1", value = input$sp2)
            updateNumericInput(session, "sn2", value = input$sn1)
            updateNumericInput(session, "sp2", value = input$sp1)
            updateNumericInput(session, "t1", value = input$t2)
            updateNumericInput(session, "t2", value = input$t1)
        }
        
        if(input$ord == 'Order 2'){
            updateTextInput(session, "name_test_1", value = input$name_test_2)
            updateTextInput(session, "name_test_2", value = input$name_test_1)
            updateNumericInput(session, "sn1", value = input$sn2)
            updateNumericInput(session, "sp1", value = input$sp2)
            updateNumericInput(session, "sn2", value = input$sn1)
            updateNumericInput(session, "sp2", value = input$sp1)
            updateNumericInput(session, "t1", value = input$t2)
            updateNumericInput(session, "t2", value = input$t1)
        }
    })
    
df12 <- reactiveValues(data = NULL)  #create dataframe for singular testing 
df3 <- reactiveValues(data = NULL)   #create dataframe for confirmatory testing until positive
df4 <- reactiveValues(data = NULL)   #create dataframe for confirmatory testing until negative

observeEvent(input$P & input$pv &
                 input$sn1 & input$sp1 & input$sn2 & input$sp2 & input$t1 & input$t2, {
                     po<- input$P  # total population
                     p <- input$pv/100  # disase prevalance
                     pp<-po*p        #population with disease
                     
                     snx1<- input$sn1/100  #sensitivity of test 1
                     spx1<- input$sp1/100  #specificity of test 1
                     snx2<- input$sn2/100  #sensitivity of test 2
                     spx2<- input$sp2/100  #specificity of test 2
                     
                     X1<-c(1, 2)       #test 1
                     X2<-c(2, 1)       #test 2
                     
                     #outputs for singular testing
                     TP0_1<-po*p*snx1
                     FN0_1<- po*p*(1-snx1)
                     FP0_1<- po*(1-p)*(1-spx1)
                     TN0_1<- po*(1-p)*spx1
                     TP0_2<-po*p*snx2
                     FN0_2<- po*p*(1-snx2)
                     FP0_2<- po*(1-p)*(1-spx2)
                     TN0_2<- po*(1-p)*spx2
                     T0_1<- c(po, po)
                     T0_2<- c(0, 0)
                     sn0_1<-snx1*100
                     sp0_1<-spx1*100
                     
                     PPV1<- round(TP0_1/(TP0_1 + FP0_1)*100, digits =2)
                     NPV1<- round(TN0_1/(TN0_1 + FN0_1)*100, digits =2)
                     PPV2<- round(TP0_2/(TP0_2 + FP0_2)*100, digits =2)
                     NPV2<- round(TN0_2/(TN0_2 + FN0_2)*100, digits =2)
                     
                     TP0_1<-round(TP0_1, digits = 0)
                     FN0_1<-round(FN0_1, digits = 0)
                     FP0_1<-round(FP0_1, digits = 0)
                     TN0_1<-round(TN0_1, digits = 0)
                     
                     sn0_2<-snx2*100
                     sp0_2<-spx2*100
                     TP0_2<-round(TP0_2, digits = 0)
                     FN0_2<-round(FN0_2, digits = 0)
                     FP0_2<-round(FP0_2, digits = 0)
                     TN0_2<-round(TN0_2, digits = 0)
                     
                     t_result_0<- c(input$t1, input$t2)          #average time to result
                     
                     TP0 <- c(TP0_1, TP0_2)
                     FP0 <- c(FP0_1, FP0_2)
                     TN0 <- c(TN0_1, TN0_2)
                     FN0 <- c(FN0_1, FN0_2)
                     sn0 <- c(sn0_1, sn0_2)
                     sp0 <- c(sp0_1, sp0_2)
                     PPV0<- c(PPV1, PPV2)
                     NPV0<- c(NPV1, NPV2)
                     
                     df12$data<- data.frame(X1, X2, pp, TP0, FP0, TN0, FN0, sn0, sp0, PPV0, NPV0, T0_1, T0_2, t_result_0)
                     setnames(df12$data, old = c('X1','X2','pp', 'TP0', 'FP0', 'TN0', 'FN0', 'sn0', 'sp0', 'PPV0', 'NPV0', 'T0_1', 'T0_2', 't_result_0'),
                              new = c('Test 1','Test 2','Infected population','True Positives','False Positives','True Negatives','False Negatives',
                                      'Combined Sensitivity', 'Combined Specificity', 'PPV', 'NPV', 'Num of 1st tests', "Num of 2nd tests", "Average time to result (Hours)"))
                     df12$data$'Test 1'[df12$data$'Test 1'==1]<-input$name_test_1
                     df12$data$'Test 1'[df12$data$'Test 1'==2]<-input$name_test_2
                     df12$data$'Test 2'[df12$data$'Test 2'==1]<-'-'
                     df12$data$'Test 2'[df12$data$'Test 2'==2]<-'-'
                     df12$data
                 })
   
observeEvent(input$P & input$pv &
                     input$sn1 & input$sp1 & input$sn2 & input$sp2 & input$t1 & input$t2 & input$k1 &input$k0, {
                         
                         po<- input$P  # total population
                         p <- input$pv/100  # disase prevalance 
                         snx<- c(input$sn1/100, input$sn2/100)  # sensitivities
                         spx<- c(input$sp1/100, input$sp2/100)  # corresponding specificities
                         pp<-po*p        #population with disease
                         t_resultx <-c(input$t1, input$t2)   #time to result for test 1 and test 2
                         k1<- input$k1    #kappa coffecient for infected
                         k0<- input$k0    #kappa coffecient for non-infected
                         
                         n<- 2     #number of tests
                         r<- 2     #number of test positions 
                         perm<-permutations(n, r, 1:n)   # possible combinations 
                         l<- length(perm[,1])      #length of permutations 
                         
                         #initialize outputs
                         sn<-matrix(NA, l, r)      #sensitivities depending on position of test while applying in combination
                         sp<-matrix(NA, l, r)      #specificities depending on position of test while applying in combination
                         t_result <-matrix(NA, l, r)
                         
                         for (i in 1:l)   #repeats the run for each possible algorithm
                         {
                             sni<-snx[perm[i,]]
                             spi<-spx[perm[i,]]
                             t_resulti<- t_resultx[perm[i,]]
                             
                             sn[i,]<-sni
                             sp[i,]<-spi
                             t_result[i,]<-t_resulti
                         }
                         
                         #calculation of test covariance from kappa
                         gsn<- k1 * (sn[,1]*(1-sn[,2]) + sn[,2]*(1-sn[,1])) / 2
                         gsp<- k0 * (sp[,1]*(1-sp[,2]) + sp[,2]*(1-sp[,1])) / 2
                         
                         #outputs for confirmatory testing until positives
                         TP3<-po*p*(1-(1-sn[,1])*(1-sn[,2])-gsn)
                         FN3<-po*p*((1-sn[,1])*(1-sn[,2])+gsn)
                         FP3<-po*(1-p)*(1-sp[,1]*sp[,2]-gsp)
                         TN3<-po*(1-p)*((sp[,1]*sp[,2])+gsp)
                         sn3<-(1-((1-sn[,1])*(1-sn[,2])) - gsn)*100
                         sp3<-(sp[,1]*sp[,2] + gsp)*100
                         PPV3<- round(TP3/(TP3 + FP3)*100, digits = 2)
                         NPV3<- round(TN3/(TN3 + FN3)*100, digits = 2)
                         T1_3x<-po
                         T2_3x<-po*(1-(p*sn[,1] + (1-p)*(1-sp[,1])))
                         TP3<-round(TP3, digits = 0)
                         FN3<-round(FN3, digits = 0)
                         FP3<-round(FP3, digits = 0)
                         TN3<-round(TN3, digits = 0)
                         T1_3<- round(T1_3x, digits = 0)
                         T2_3<- round(T2_3x, digits = 0)
                         t_result_3<- (t_result[,1]*T1_3x + t_result[,2]*T2_3x)/input$P          #average time to result
                         df3$data<- data.frame(perm, pp, TP3, FP3, TN3, FN3, sn3, sp3, PPV3, NPV3, T1_3, T2_3, t_result_3)
                         setnames(df3$data, old = c('X1','X2','pp', 'TP3', 'FP3', 'TN3', 'FN3', 'sn3', 'sp3', 'PPV3', 'NPV3', 'T1_3', 'T2_3', 't_result_3'),
                                  new = c('1st test in algorithm','2nd test in algorithm','Infected population','True Positives','False Positives','True Negatives','False Negatives',
                                          'Combined Sensitivity', 'Combined Specificity', 'PPV', 'NPV', 'Num of 1st tests', "Num of 2nd tests", "Average time to result (Hours)"))
                         df3$data$'1st test in algorithm'[df3$data$'1st test in algorithm'==1]<-input$name_test_1
                         df3$data$'1st test in algorithm'[df3$data$'1st test in algorithm'==2]<-input$name_test_2
                         df3$data$'2nd test in algorithm'[df3$data$'2nd test in algorithm'==1]<-input$name_test_1
                         df3$data$'2nd test in algorithm'[df3$data$'2nd test in algorithm'==2]<-input$name_test_2
                         df3$data
                         
                         #outputs for confirmatory testing until negatives
                         TP4<- po*p*(sn[,1]*sn[,2] + gsn)
                         FN4<- po*p* (1-sn[,1]*sn[,2]-gsn)
                         FP4<- po*(1-p)*((1-sp[,1])*(1-sp[,2])+gsp)
                         TN4<- po*(1-p)*(1-(1-sp[,1])*(1-sp[,2])-gsp)
                         sn4<-(sn[,1]*sn[,2]+gsn)*100
                         sp4<-(1-(1-sp[,1])*(1-sp[,2]) - gsp)*100
                         PPV4<- round(TP4/(TP4 + FP4)*100, digit = 2)
                         NPV4<- round(TN4/(TN4 + FN4)*100, digit = 2)
                         T1_4x<-po
                         T2_4x<-po*(1-(p*(1-sn[,1]) + (1-p)*sp[,1]))
                         TP4<-round(TP4, digits = 0)
                         FN4<-round(FN4, digits = 0)
                         FP4<-round(FP4, digits = 0)
                         TN4<-round(TN4, digits = 0)
                         T1_4<- round(T1_4x, digits = 0)
                         T2_4<- round(T2_4x, digits = 0)
                         t_result_4<- (t_result[,1]*T1_4x + t_result[,2]*T2_4x)/input$P          #average time to result
                         df4$data<- data.frame(perm, pp, TP4, FP4, TN4, FN4, sn4, sp4, PPV4, NPV4, T1_4, T2_4, t_result_4)
                         setnames(df4$data, old = c('X1','X2','pp', 'TP4', 'FP4', 'TN4', 'FN4', 'sn4', 'sp4', 'PPV4', 'NPV4', 'T1_4', 'T2_4', 't_result_4'),
                                  new = c('1st test in algorithm','2nd test in algorithm','Infected population','True Positives','False Positives','True Negatives','False Negatives',
                                          'Combined Sensitivity', 'Combined Specificity', 'PPV', 'NPV','Num of 1st tests', "Num of 2nd tests", "Average time to result (Hours)"))
                         df4$data$'1st test in algorithm'[df4$data$'1st test in algorithm'==1]<-input$name_test_1
                         df4$data$'1st test in algorithm'[df4$data$'1st test in algorithm'==2]<-input$name_test_2
                         df4$data$'2nd test in algorithm'[df4$data$'2nd test in algorithm'==1]<-input$name_test_1
                         df4$data$'2nd test in algorithm'[df4$data$'2nd test in algorithm'==2]<-input$name_test_2
                         df4$data
                     })

    output$onetest <- renderTable ({df12$data})   #tabulate output of singular testings
    output$seq1 <- renderTable({df3$data})    #tabulate output for confirmatory testing until positives
    output$seq2 <- renderTable({df4$data})    #tabulate output for confirmatory testing until negatives

    #Create summary outputs
    summary<- reactive ({
        X1<- c(df12$data$'Test 1'[1], df12$data$'Test 1'[2], df3$data$'1st test in algorithm'[1], 
               df3$data$'1st test in algorithm'[1]) 
        X2<- c(df12$data$'Test 2'[1], df12$data$'Test 2'[2], df4$data$'1st test in algorithm'[2], 
               df4$data$'1st test in algorithm'[2]) 
        Sn <- c(df12$data$'Combined Sensitivity'[1], df12$data$'Combined Sensitivity'[2], df3$data$'Combined Sensitivity'[1],  
                df4$data$'Combined Sensitivity'[1])
        Sp <- c(df12$data$'Combined Specificity'[1], df12$data$'Combined Specificity'[2], df3$data$'Combined Specificity'[1],  
                df4$data$'Combined Specificity'[1])
        cat<-c('Test 1 only','Test 2 only','Confirmatory testing for negatives', 'Confirmatory testing for positives')
        data<- data.frame (cat, X1, X2, Sn, Sp)
        setnames (data, old = c('cat', 'X1', 'X2', 'Sn', 'Sp'), 
                  new = c ('Testing scenarios        ','1st test','2nd test','Combined Sensitivity (%)', 'Combined Specificity (%)'))
    })
    
    #create detailed outputs
    details<- reactive ({
        X1<- c(df12$data$'Test 1'[1], df12$data$'Test 1'[2], df3$data$'1st test in algorithm'[1], 
               df3$data$'1st test in algorithm'[1])   
        X2<- c(df12$data$'Test 2'[1], df12$data$'Test 2'[2], df4$data$'1st test in algorithm'[2], 
               df4$data$'1st test in algorithm'[2])  
        pp<- c(df12$data$'Infected population'[1], df12$data$'Infected population'[2], df3$data$'Infected population'[1], 
               df4$data$'Infected population'[1])
        TP <- c(df0$data$'True Positives'[1], df0$data$'True Positives'[2], df3$data$'True Positives'[1], 
                df4$data$'True Positives'[1])
        FP <- c(df12$data$'False Positives'[1], df12$data$'False Positives'[2], df3$data$'False Positives'[1], 
                df4$data$'False Positives'[1])
        TN <- c(df12$data$'True Negatives'[1], df12$data$'True Negatives'[2], df3$data$'True Negatives'[1],  
                df4$data$'True Negatives'[1])
        FN <- c(df12$data$'False Negatives'[1], df12$data$'False Negatives'[2], df3$data$'False Negatives'[1],  
                df4$data$'False Negatives'[1])
        Sn <- c(df12$data$'Combined Sensitivity'[1], df12$data$'Combined Sensitivity'[2], df3$data$'Combined Sensitivity'[1],  
                df4$data$'Combined Sensitivity'[1])
        Sp <- c(df12$data$'Combined Specificity'[1], df12$data$'Combined Specificity'[2], df3$data$'Combined Specificity'[1],  
                df4$data$'Combined Specificity'[1])
        PPV<- c(df12$data$'PPV'[1], df12$data$'PPV'[2], df3$data$'PPV'[1],  
                df4$data$'PPV'[1])
        NPV<- c(df12$data$'NPV'[1], df12$data$'NPV'[2], df3$data$'NPV'[1],  
                df4$data$'NPV'[1])
        Nt2 <- c(df12$data$'Num of 2nd tests'[1], df12$data$'Num of 2nd tests'[2], df3$data$'Num of 2nd tests'[1],  
                 df4$data$'Num of 2nd tests'[1])
        Tr <- c(df12$data$'Average time to result (Hours)'[1], df12$data$'Average time to result (Hours)'[2], df3$data$'Average time to result (Hours)'[1], 
                df4$data$'Average time to result (Hours)'[1])
        cat<-c('Test 1 only','Test 2 only','Confirmatory testing for negatives', 'Confirmatory testing for positives')
        data<- data.frame (cat, X1, X2, Sn, Sp, Nt2, Tr, pp, TP, FP, TN, FN, PPV, NPV)
        setnames (data, old = c('cat', 'X1', 'X2', 'Sn', 'Sp', 'Nt2', 'Tr', 'pp', 'TP', 'FP', 'TN', 'FN', 'PPV', 'NPV'), 
                  new = c ('Testing scenarios        ','1st test','2nd test','Combined Sensitivity (%)', 'Combined Specificity (%)', "Num of 2nd tests", "Average time to result (Hours)", 
                           'Infected population','True Positives','False Positives','True Negatives','False Negatives', 'Positive predictive value (%)', 'Negative predictive value(%)'))
    })
    
    output$table_comparison_summary <- renderTable({summary()})   #generate table for summary outputs
    output$table_comparison_details <- renderTable({details()})   #generate table for detailed outputs
    
    ## plot the comparison between different scenarios for summary output   
    output$plot_comparison_summary <- renderPlot({
        TP <- c(df12$data$'True Positives'[1], df12$data$'True Positives'[2], df3$data$'True Positives'[1], 
                df4$data$'True Positives'[1])
        FP <- c(df12$data$'False Positives'[1], df12$data$'False Positives'[2], df3$data$'False Positives'[1], 
                df4$data$'False Positives'[1])
        TN <- c(df12$data$'True Negatives'[1], df12$data$'True Negatives'[2], df3$data$'True Negatives'[1],  
                df4$data$'True Negatives'[1])
        FN <- c(df12$data$'False Negatives'[1], df12$data$'False Negatives'[2], df3$data$'False Negatives'[1],  
                df4$data$'False Negatives'[1])
        Sn <- c(df12$data$'Combined Sensitivity'[1], df12$data$'Combined Sensitivity'[2], df3$data$'Combined Sensitivity'[1],  
                df4$data$'Combined Sensitivity'[1])
        Sp <- c(df12$data$'Combined Specificity'[1], df12$data$'Combined Specificity'[2], df3$data$'Combined Specificity'[1],  
                df4$data$'Combined Specificity'[1])
        PPV<- c(df12$data$'PPV'[1], df12$data$'PPV'[2], df3$data$'PPV'[1],  
                df4$data$'PPV'[1])
        NPV<- c(df12$data$'NPV'[1], df12$data$'NPV'[2], df3$data$'NPV'[1],  
                df4$data$'NPV'[1])
        Nt <- c(df12$data$'Num of 2nd tests'[1], df12$data$'Num of 2nd tests'[2], df3$data$'Num of 2nd tests'[1],  
                df4$data$'Num of 2nd tests'[1])
        Tr <- c(df12$data$'Average time to result (Hours)'[1], df12$data$'Average time to result (Hours)'[2], df3$data$'Average time to result (Hours)'[1], 
                df4$data$'Average time to result (Hours)'[1])
        
        if (input$max_display == "True positives")
        {df <- TP}
        if (input$max_display == "False positives")
        {df <- FP}
        if (input$max_display == "True negatives")
        {df <- TN}
        if (input$max_display == "False negatives")
        {df <- FN}
        if (input$max_display == "Sensitivity (%)")
        {df <- Sn}
        if (input$max_display == "Specificity (%)")
        {df <- Sp}
        if (input$max_display == "Positive predictive value (%)")
        {df <- PPV}
        if (input$max_display == "Negative predictive value (%)")
        {df <- NPV}
        if (input$max_display == "Number of 2nd tests")
        {df <- Nt}
        if (input$max_display == "Time to result (Hours)")
        {df <- Tr}
        
     #plot bar diagrams for summary output   
        cat<- c('Test 1 only','Test 2 only','Confirmatory testing for negatives', 'Confirmatory testing for positives')
        data<-data.frame(cat, df)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only','Test 2 only','Confirmatory testing for negatives', 'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=df)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=df), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 14) +
            xlab("") +
            ylab("")+
            labs(title="")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12))
    })
    
    ## detailed output plots
    #Plot sensitivity
    output$plot_comparison_sensitivity <- renderPlot({
        Sn <- c(df12$data$'Combined Sensitivity'[1], df12$data$'Combined Sensitivity'[2], df3$data$'Combined Sensitivity'[1],  
                df4$data$'Combined Sensitivity'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, Sn)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=Sn)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=Sn), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="Sensitivity (%)")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Plot specificity
    output$plot_comparison_specificity <- renderPlot({
        Sp <- c(df12$data$'Combined Specificity'[1], df12$data$'Combined Specificity'[2], df3$data$'Combined Specificity'[1],  
                df4$data$'Combined Specificity'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, Sp)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=Sp)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=Sp), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="Specificity (%)")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12))  
    })
    
    #Plot number of tests
    output$plot_comparison_Number_of_second_tests<- renderPlot({
        Nt <- c(df12$data$'Num of 2nd tests'[1], df12$data$'Num of 2nd tests'[2], df3$data$'Num of 2nd tests'[1],  
                df4$data$'Num of 2nd tests'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, Nt)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=Nt)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=Nt), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="Number of second tests")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Plot time to result
    output$plot_comparison_Time_to_result<- renderPlot({
        Tr <- c(df12$data$'Average time to result (Hours)'[1], df12$data$'Average time to result (Hours)'[2], df3$data$'Average time to result (Hours)'[1], 
                df4$data$'Average time to result (Hours)'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, Tr)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=Tr)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=Tr), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="Average time to result")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Plot time to result
    output$plot_comparison_TP<- renderPlot({
        TP <- c(df12$data$'True Positives'[1], df12$data$'True Positives'[2], df3$data$'True Positives'[1], 
                df4$data$'True Positives'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, TP)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=TP)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=TP), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="True positives")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Plot true negatives
    output$plot_comparison_TN<- renderPlot({
        TN <- c(df12$data$'True Negatives'[1], df12$data$'True Negatives'[2], df3$data$'True Negatives'[1],  
                df4$data$'True Negatives'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, TN)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=TN)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=TN), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="True negatives")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Plot false positives
    output$plot_comparison_FP<- renderPlot({
        FP <- c(df12$data$'False Positives'[1], df12$data$'False Positives'[2], df3$data$'False Positives'[1], 
                df4$data$'False Positives'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, FP)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=FP)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=FP), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="False positives")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Plot fase negatives
    output$plot_comparison_FN<- renderPlot({
        FN <- c(df12$data$'False Negatives'[1], df12$data$'False Negatives'[2], df3$data$'False Negatives'[1],  
                df4$data$'False Negatives'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, FN)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=FN)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=FN), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="False negatives")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12))  
    })
    
    #Plot positive predictive value
    output$plot_comparison_PPV<- renderPlot({
        PPV<- c(df12$data$'PPV'[1], df12$data$'PPV'[2], df3$data$'PPV'[1],  
                df4$data$'PPV'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, PPV)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=PPV)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=PPV), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="Positive predictive values (%)")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Plot negative predictive value
    output$plot_comparison_NPV<- renderPlot({
        NPV<- c(df12$data$'NPV'[1], df12$data$'NPV'[2], df3$data$'NPV'[1],  
                df4$data$'NPV'[1])
        cat<- c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                'Confirmatory testing for positives')
        data<-data.frame(cat, NPV)
        data %>%
            arrange(cat) %>%
            mutate(cat = factor(cat, levels=c('Test 1 only', 'Test 2 only', 'Confirmatory testing for negatives',  
                                              'Confirmatory testing for positives'))) %>%
            ggplot(aes(x=cat, y=NPV)) +
            geom_bar(stat="identity", width = 0.5, 
                     fill = c(rep("steelblue",1), rep("peru",1), ("sienna"), rep("olivedrab",1)), position=position_dodge())+
            geom_text(aes(label=NPV), vjust=1.6, color="white", size=5.5)+
            theme_bw(base_size = 16) +
            xlab("") +
            ylab("")+
            labs(title="Negative predictive values (%)")+
            theme(axis.text.x = element_text(angle = 15, hjust=1, size = 15))+ 
            theme(axis.text.y = element_text(size =12)) 
    })
    
    #Download summary output as csv
    output$downloaddata1 <- downloadHandler(
        filename = function(){"summaryoutput.csv"}, 
        content = function(fname){
            write.csv(summary(), fname)
        })
    
    #Download detailed output as csv
    output$downloaddata2 <- downloadHandler(
        filename = function(){"detailoutput.csv"}, 
        content = function(fname){
            write.csv(details(), fname)
        })
})

# Run the application 
shinyApp(ui = ui, server = server)


