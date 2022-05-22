# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, https://shiny.rstudio.com/gallery/shiny-theme-selector.html
# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)

# Define UI
ui <- fluidPage(tags$style(type='text/css', '#txtout {white-space: pre-wrap;}'),
                theme = shinytheme("united"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Hematology Note Writer",
                  tabPanel("Note Writer",
                           sidebarPanel(
                             tags$h3("Input:"),
                             textInput("txt1", "Given Name:", ""),
                             textInput("txt2", "Surname:", ""),
                             actionButton("disclaimer", "Virtual Disclaimer"),
                             
                             
                             actionButton("hemtit", "Hematology Consult Note"),
                             headerPanel(""),
                             
                             actionButton("newline", "New Line"),
                             actionButton("id", "ID"),
                             actionButton("cc", "CC"),
                             actionButton("rfr", "Reason for Referral"),
                             actionButton("pmh", "Past Medical History"),
                             actionButton("rx", "Medication"),
                             actionButton("sohx", "Social History"),
                             
                             actionButton("hpi", "History of Presenting Illness"),
                             actionButton("phyex", "Physical Exam"),
                             actionButton("ix", "Investigations:"),
                             actionButton("ip", "Impression"),
                             
                             headerPanel(""),
                             
                             actionButton("sig", "Signature"),
                             
                             headerPanel(""),
                             
                             actionButton("date", "Date"),
                             
                             headerPanel(""),
                             
                             
                             actionButton("iviron", "IV Iron Counseling"),
                             
                             
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("NOTE WRITER"),
                             
                             h4("Output"),
                             verbatimTextOutput ("txtout"),

                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  
                  tabPanel(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "Hematology Calculators",
                    tabPanel("Heme Calculators",
                             sidebarPanel(
                               
                               tags$h3("Corrected Count Increment:"),
                               numericInput("preplt", "Pre-transfusion count (/L):", 50, min = 0, max = 3000),
                               numericInput("postplt", "Post-transfusion count (/L):", 80, min = 0, max = 3000),
                               numericInput("height", "Height (cm):",  170, min = 0, max = 300),
                               numericInput("weight", "Weight (kg):",  60, min = 0, max = 2000),
                               numericInput("plttx", "Plts transfused (10e9):",  275, min = 0, max = 500),
                               
                               actionButton("calc", "Calculate"),
                               
                               h4("Corrected count increment:"),
                               verbatimTextOutput ("txtout2"),
                               

                             ), # sidebarPanel
                             mainPanel(
                               h1("Calculator Output"),

                               
                             ) # mainPanel
                             
                    ),
                  tabPanel("Placeholder", "This panel is intentionally left blank"),
                  
                  )
                  
                )
                
)

                  
          
                  
   # navbarPage
 # fluidPage

# Define server function  
server <- function(input, output) {
  
  
  # for nav panel 2 
  
  ot2 <- reactiveValues(cci=1, b=1)
  
  
  observeEvent(input$calc, {
    
    bsa <- (((input$weight* input$height)/3600)^(1/2))
    
    ot2$cci <- (((input$postplt-input$preplt )*1000)*bsa)/(input$plttx/100)
    

  })
  
  output$txtout2 <- renderText({
    
    bsa <- (((input$weight* input$height)/3600)^(1/2))
    
    ot2$cci <- (((input$postplt-input$preplt )*1000)*bsa)/(input$plttx/100)
    
    
    paste (round(ot2$cci, 0))    

  })
  
  
  
  #for nav panel 1 
  
  ot <- reactiveValues(a="", b=1)
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  
  })

  observeEvent(input$date, {
    
    currentDate <- paste(Sys.time())
    
    ot$a <-  paste(ot$a, currentDate, sep="\n")
    
  })
  
  observeEvent(input$newline, {
    
    ot$a <-  paste(ot$a,"", sep="\n")
    
  })
  
  observeEvent(input$hemtit, {
    
    ot$a <-  paste(ot$a, "HEMATOLOGY CONSULT NOTE", "", sep="\n")
    
  })
  
  observeEvent(input$disclaimer, {
    
    ot$a <-  paste(ot$a, "This was a virtual visit and the patient was expalined the privacy risks of online communicaiton. They understood and agreed to proceed with this interview", "", sep="\n")

  })
  
  observeEvent(input$id, {
    ot$a <-  paste(ot$a, "IDENTIFICATION", "", sep="\n")
  })

  observeEvent(input$cc, {
    ot$a <-  paste(ot$a, "CHIEF COMPLAINT", "", sep="\n")
  })  
  
  observeEvent(input$rfr, {
    ot$a <-  paste(ot$a, "REASON FOR REFERRAL", "", sep="\n")
  })  
  
  observeEvent(input$pmh, {
    ot$a <-  paste(ot$a, "PAST MEDICAL HISTORY:", "", sep="\n")
    
    })  
  
  observeEvent(input$rx, {
               ot$a <-  paste(ot$a, "MEDICATIONS:", "", sep="\n")
               
               })  
  
  observeEvent(input$sohx, {
    ot$a <-  paste(ot$a, "SOCIAL HISTORY:", "", sep="\n")
  })  
  
  observeEvent(input$hpi, {
    ot$a <-  paste(ot$a, "HISTORY OF PRESENTINGILLNESS:", "", sep="\n")
  })  
  
  observeEvent(input$phyex, {
               ot$a <-  paste(ot$a, "PHYSICAL EXAMINATION:", "", sep="\n")
               })  
  
  observeEvent(input$ix, {
               ot$a <-  paste(ot$a, "INVESTIGATIONS:", "", sep="\n")
               })  
  
  observeEvent(input$iviron, {
    ot$a <-  paste(ot$a, "The patient was counseled about the risks of IV iron therapy, including the risk of mild and anaphylactic infusion reactions.:", "", sep="\n")
  })  
  
  observeEvent(input$ip, {
    ot$a <-  paste(ot$a, "IMPRESSION AND PLAN:", "", sep="\n")
  }) 
  
  observeEvent(input$sig, {
    
    name = paste("Dr.", input$txt1, " ", input$txt2)
    ot$a <-  paste(ot$a, name,  " in service of Dr. ___________, Staff Hematologist", "", sep="\n")
  })  
  
  observeEvent(input$action, {
    

    
    ot$a <-  paste(ot$a, ot$b, input$txt1, input$txt2)
    
    ot$b <- ot$b+1

  })
  
  output$txtout <- renderText({
    paste(ot$a)
    
    
  })
  
  

  
  
  
  
  
} # server




# Create Shiny object
shinyApp(ui = ui, server = server)