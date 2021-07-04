
library(shiny)

fluidPage(
    navbarPage(
        "TMC Calculator",
        
        
          tabPanel(
            "TMC curves",
            fluidPage(
              sidebarLayout(
                
                sidebarPanel(
                  selectInput("MP_type","Select the type of microplastic",
                              choices = unique(Microplastics_type$Microplastic)),
                  
                  selectInput("MP_shape","Select the shape of microplastic",
                              choices=c("Long Cylinder","Short Cylinder","Sphere",
                                        "Oblate Spheroid e=0.2","Oblate Spheroid e=0.9")),
                  
                  sliderInput("MP_size", "Select the size of microplastic: ",
                              min = 1, 
                              max = 5000, 
                              value = 100, 
                              step = 1),
                  
                  selectInput("Contaminant", "Select the contaminant of concern",
                              choices = unique(Contaminant$Contaminant), 
                              "Antimony"),
                  
                  numericInput("AC", "Enter the contaminant adsorption capacity (mg/g)",
                               value = 1)
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plot", plotOutput("TMC_curve", height = 650, width = 950)),
                    tabPanel("Summary", verbatimTextOutput("summary"),
                             verbatimTextOutput("min_size")),
                    tabPanel("Table", tableOutput("table"))
                  )
                )
            )
        )),
      
        tabPanel("Calculator",
            fluidPage(
              
              sidebarLayout(
                
                sidebarPanel(style = "position:fixed;width:inherit;",
                             "Inputs",
                             width = 2,
              
                  selectInput("MP_type1","Select the type of microplastic",
                              choices = c("HDPE","PC","PE","PET","PP","PS","PVC")),
                  
                  selectInput("MP_shape1","Select the shape of microplastic",
                              choices=c("Long Cylinder","Short Cylinder","Sphere",
                                        "Oblate Spheroid e=0.2","Oblate Spheroid e=0.9")),
                  
                  selectInput("Contaminant1", "Select the contaminant of concern",
                              choices = unique(Contaminant$Contaminant), 
                              "Antimony"),
                  selectInput("Plot_type", "Choose the plot type",
                              choices = c("Full TMC plot","Based on contaminant","Based on input selection")),
                ),
                  
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plot", plotOutput("TMC_curve1", height = 750, width = 1200)),
                    tabPanel("Summary",textOutput("summary1"),
                             width = 6.5
                             
                    )
                )
                )
            )
              
            )
        )
)
    
)

