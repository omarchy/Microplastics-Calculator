library(shiny)
library("tidyverse")
library(ggplot2)
library(readxl)


#Densities of microplastics
Microplastics_type <- data.frame(Microplastic = c("Polyethylene","Polypropylene","Polystyrene",
                                                  "Polyethylene Terephthalate","Polycarbonate",
                                                  "Polyvinylchloride","High Density Polyethylene"),
                                 Unit_volume = c(1.16, 1.18, 0.93, 0.73, 0.81, 0.72, 1.03)*10^(-6))

#Contaminants and their corresponding minimum health endpoint parameters and adsorption capacities
Contaminant <- data.frame("Contaminant"= c("Aluminum","Antimony","Arsenic","BPA","Bromine",
                                           "Cadmium","Chromium","Manganese","Mercury","Propanolol",
                                           "Sulfamethoxazole"),
                          "AC_la" = c(0.375,27.8,1.92,0.19,13,0.00014,0.000454,0.13,
                                                           0.00125,0.133,0.087),
                          "G" = c(2.9,0.004,0.003,0.00006,0.01,0.005,0.03,0.12,0.002,0.0005,0.02))

#Defining functions for surface area and volume of shapes (sphere, long & short cylinders and oblate spheroids (e = 0.2 and 0.9))
SPa <- function(d){4*pi*(d/2)^2}
SPv <- function(d){(4/3)*pi*(d/2)^3}
LCa <- function(d){0.1*(2.1/2)*pi*d^2}
LCv <- function(d){pi*(0.1*d/2)^2*(d)}
SCa <- function(d){0.6*pi*d^2}
SCv <- function(d){pi*(d/2)^2*(0.1*d)}
OSa <- function(d,e){2*pi*(d/2)^2 + (pi/e)*log((1+e)/(1-e))*((d/2)*(1-e^2)^0.5)^2}
OSv <- function(d,e){(pi/6)*d^3*sqrt(1-e^2)}


############################
function(input, output, session){
    
    d <- c(1, 10, 20, 50, 100, 150, 300, 500, 750)
   
    a <- reactive({
        
        
        if (as.character(input$MP_shape) == "Sphere"){
            10^(-12) * SPa(d)
            
        } else if (as.character(input$MP_shape) == "Long Cylinder"){
            10^(-12) * LCa(d)
            
        } else if (as.character(input$MP_shape) == "Short Cylinder"){
            10^(-12) * SCa(d)
            
        } else if (as.character(input$MP_shape) == "Oblate Spheroid e=0.2"){
            10^(-12) * OSa(d, 0.2)
            
        } else if (as.character(input$MP_shape) == "Oblate Spheroid e=0.9"){
            10^(-12) * OSa(d, 0.9)
            
        }
    })
    
    v <- reactive ({
       
        
        if (as.character(input$MP_shape) == "Sphere"){
        
            10^(-18) * SPv(d)
        } else if (as.character(input$MP_shape) == "Long Cylinder"){
    
            10^(-18) * LCv(d)
        } else if (as.character(input$MP_shape) == "Short Cylinder"){
        
            10^(-18) * SCv(d)
        } else if (as.character(input$MP_shape) == "Oblate Spheroid e=0.2"){
        
           10^(-18) * OSv(d, 0.2)
        } else if (as.character(input$MP_shape) == "Oblate Spheroid e=0.9"){
    
           10^(-18) * OSv(d, 0.9)
        }  
        
    })
 
    #For TSA
       
    A <- reactive ({
        
        if (as.character(input$MP_shape) == "Sphere"){
            10^(-12) * SPa(input$MP_size)
            
        } else if (as.character(input$MP_shape) == "Long Cylinder"){
            10^(-12) * LCa(input$MP_size)
            
        } else if (as.character(input$MP_shape) == "Short Cylinder"){
            10^(-12) * SCa(input$MP_size)
            
        } else if (as.character(input$MP_shape) == "Oblate Spheroid e=0.2"){
            10^(-12) * OSa(input$MP_size, 0.2)
            
        } else if (as.character(input$MP_shape) == "Oblate spheroid e=0.9"){
            10^(-12) * OSa(input$MP_size, 0.9)
            
        }
        
    })
    
    V <- reactive ({
        
        
        if (as.character(input$MP_shape) == "Sphere"){
            
            10^(-18) * SPv(input$MP_size)
        } else if (as.character(input$MP_shape) == "Long Cylinder"){
            
            10^(-18) * LCv(input$MP_size)
        } else if (as.character(input$MP_shape) == "Short Cylinder"){
            
            10^(-18) * SCv(input$MP_size)
        } else if (as.character(input$MP_shape) == "Oblate Spheroid e=0.2"){
            
            10^(-18) * OSv(input$MP_size, 0.2)
        } else if (as.character(input$MP_shape) == "Oblate Spheroid e=0.9"){
            
            10^(-18) * OSv(input$MP_size, 0.9)
        }  
        
    })
    
    
    TMC_curve_df <- reactive({
        print(a())
        print(v())
        print(A())
        print(V())
        
        
        Microplastics_type_now <- Microplastics_type %>%
            filter (Microplastic == input$MP_type ) 
        
        TSA <- Microplastics_type_now$Unit_volume * (1/V()) * A()
        
        
        Contaminant_now <- Contaminant %>%
            filter (Contaminant == as.character(input$Contaminant) ) 
        
        TMC <- Contaminant_now$G/(a()*((as.numeric(input$AC/TSA))))
        
        print(TMC)
        
        TMC_curve <- data.frame("TMC" = TMC, "Size" = d)
    })
    
    
    output$TMC_curve <- renderPlot({
        
        minor_breaks <- rep(1:9, 42)*(10^rep(-20:20, each=9))
        ggplot(TMC_curve_df(), aes(x = Size, y = TMC)) +
            geom_point(size = 4.5, color = "Red") +
            geom_line(size = 1.5)+
            theme_bw() +
            scale_y_log10(breaks=c(0.01,1,100,10000,1000000,100000000,
                                   10000000000,1000000000000,
                                   100000000000000,10000000000000000), 
                          minor_breaks= minor_breaks, limits = c(0.01,100000000000000)) +
            scale_x_continuous(breaks=c(1,100,200,300,400,500,600,700,800))+
            
            xlab('Size in micrometers')+
            ylab('TMC (#/L)')+ #ggtitle('Threshold microplastics concentration curve')
            theme(axis.text=element_text(size = 16),
                  axis.title=element_text(size = 18),
                  legend.title = element_text(size = 18),
                  legend.text = element_text(size = 16))
    })
    
    output$summary <- renderText({
       
        
        min_TMC <- min(TMC_curve_df()$TMC)
        min_TMC <- round(min_TMC,digits=2)
        print(paste0("The lowest (most concerning) TMC is ", min_TMC, " (particles/L) of source water"))
                    
    })
    
    output$min_size <- renderText({
        min_size_df <- TMC_curve_df() %>%
            filter(TMC == min(TMC))
        
        
        min <- min_size_df$Size
        print(paste0("This is calculated for microplastics of size ", min, " micrometers"))
    })
      
    #Plot based on 3 options
     output$TMC_curve1 <- renderPlot({
         TMC_matrix <- read_excel("O:/Me/R/TMC/TMC/TMC_matrix_new.xlsx")
         TMC_matrix_new <- TMC_matrix %>%
             rename(Size = size_assigned, TMC = TMC_rsd_0)
         TMC_matrix_new$Size <- as.factor(as.numeric(TMC_matrix_new$Size))
         minor_breaks <- rep(1:9, 42)*(10^rep(-20:20, each=9))
         
         
         
         if (input$Plot_type == "Based on input selection"){
           data <- TMC_matrix_new %>%
             filter(Contaminant == input$Contaminant1) %>%
             filter(Microplastic == input$MP_type1) %>%
             filter(Shape == input$MP_shape1)

           data$Size <- as.numeric(as.character(data$Size))
           
           #minor_breaks <- rep(1:9, 42)*(10^rep(-20:20, each=9))
           ggplot(data, aes(x = Size, y = TMC)) +
             geom_point(width= 0.5, height = 0.5, size = 4,alpha= 0.70)+
             theme_bw() +
             scale_color_brewer()+
             scale_y_log10(breaks=c(0.01,1,100,10000,1000000,100000000,
                                    10000000000,1000000000000,
                                    100000000000000,10000000000000000), 
                           minor_breaks= minor_breaks, limits = c(0.01,100000000000000)) +
             scale_x_continuous(breaks=c(1,100,200,300,400,500,600,700,800))+
             
             xlab('Size in micrometers')+
             ylab('TMC (#/L)')+ #ggtitle('Threshold microplastics concentration curve')
             theme(axis.text=element_text(size = 16),
                   axis.title=element_text(size = 18),
                   legend.title = element_text(size = 18),
                   legend.text = element_text(size = 16))  
         }
         
         else if (input$Plot_type == "Full TMC plot"){
          
           
           #minor_breaks <- rep(1:9, 42)*(10^rep(-20:20, each=9))
           ggplot(TMC_matrix_new, aes(x = Size, y = TMC)) +
             geom_jitter(width= 0.5, height = 0.5, size = 4,alpha= 0.70, 
                         aes(color = Contaminant, shape = Shape))+
             theme_bw() +
             scale_color_brewer(palette="Paired")+
             scale_y_log10(breaks=c(0.01,1,100,10000,1000000,100000000,
                                    10000000000,1000000000000,
                                    100000000000000,10000000000000000), 
                           minor_breaks= minor_breaks, limits = c(1,100000000000000)) +
             
             
             xlab('Size in micrometers')+
             ylab('TMC (#/L)')+ #ggtitle('Threshold microplastics concentration curve')
             theme(axis.text=element_text(size = 16),
                   axis.title=element_text(size = 18),
                   legend.title = element_text(size = 18),
                   legend.text = element_text(size = 16)) 
           
         }
         
         
         
        
     })          
     
     #Full TMC summary
     output$summary1 <- renderText({
       
       TMC_matrix <- read_excel("O:/Me/R/TMC/TMC/TMC_matrix_new.xlsx")
       TMC_matrix_new <- TMC_matrix %>%
         rename(Size = size_assigned, TMC = TMC_rsd_0)
       TMC_matrix_new$Size <- as.factor(as.numeric(TMC_matrix_new$Size))
       TMC_matrix_new <- select(TMC_matrix_new, c("Size","TMC","Contaminant","Shape"))
       
       summary(TMC_matrix_new)
     })
    
    
}
