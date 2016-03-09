
lake <- read.csv("lake.csv")
area <- read.csv("area.csv")
pop <- read.csv("pop.csv")
coHi <- read.csv("coHi.csv")
coLk <- read.csv("coLk.csv")
coLo <- read.csv("coLo.csv")

# areal.water.loading
Q <- (lake$Ad*lake$r)+(lake$Ao*lake$Pr)
qs <- Q/lake$Ao

# export coefficients}
NCap <- (pop$IndP*(pop$DaysP/365)*pop$UnitsP)+(pop$IndS*(pop$DaysS/365)*pop$UnitsS)

# p.mass.loadings
MHi <- (area$For*coHi$EcFrHi)+(area$Agr*coHi$EcAgHi)+(area$Urb*coHi$EcUrHi)+(lake$Ao*coHi$EcPrHi)+(NCap*coHi$EcStHi*(1-coHi$SRHi))+coHi$PSIHi
MLo <- (area$For*coLo$EcFrLo)+(area$Agr*coLo$EcAgLo)+(area$Urb*coLo$EcUrLo)+(lake$Ao*coLo$EcPrLo)+(NCap*coLo$EcStLo*(1-coLo$SRLo))+coLo$PSILo

shinyServer(function(input, output) {
    
    output$range <- renderText({
        MLk <- (area$For*input$coefF)+(area$Agr*input$coefA)+(area$Urb*input$coefU)+(lake$Ao*input$coefPr)+(NCap*input$coefSt*(1-input$coefSr))+0
        
        # p.areal.loading
        LHi <- (MHi/lake$Ao)*10^-3
        LLk <- (MLk/lake$Ao)*10^-3
        LLo <- (MLo/lake$Ao)*10^-3
        
        # p.lake
        PHi<- LHi/(11.6+(1.2*qs))
        PLk<- LLk/(11.6+(1.2*qs))
        PLo<- LLo/(11.6+(1.2*qs))
        
        # log.p
        logPLk <- log10(PLk)
        
        # error.uncertainty.confidence
        #Positive and negative model errors
        smpos <- (10^(logPLk+0.128))-PLk
        smneg <- (10^(logPLk-0.128))-PLk
        
        #Positive and negative loading errors
        sLpos <- (PHi-PLk)/2
        sLneg <- (PLk-PLo)/2
        
        #Positive and negative uncertainty
        sTpos <- sqrt((smpos)^2+(sLpos)^2)
        sTneg <- sqrt((smneg)^2+(sLneg)^2)
        
        # Confidence limits
        # multiple of prediction error, h=1=55% bounds, h=2=90% bounds
        hvar = sqrt(1/(2.25*(1-input$civar)))
        limitlo <- PLk-(hvar*sTneg)
        limitup <- PLk+(hvar*sTpos)
        
        #trophic.state
        #determine where lower bound falls
        if (limitlo<0.010){
            trophlo <- "oligotrophic"
        } else if(0.01<limitlo & limitlo<0.020){
            trophlo <- "mesotrophic"
        } else if (0.020<limitlo & limitlo<0.050){
            trophlo <- "eutrophic"
        } else {
            trophlo <- "hypereutrophic"
        }
        #determine where upper bound falls
        if (limitup<0.010){
            trophup <- "oligotrophic"
        } else if(0.01<limitup & limitup<0.020){
            trophup <- "mesotrophic"
        } else if (0.020<limitup & limitup<0.050){
            trophup <- "eutrophic"
        } else {
            trophup <- "hypereutrophic"
        }
        
        paste0("For the lake Higgins there is a ", input$hvar, " probability that the true phosphorus concentration value falls between ", 
               round(limitlo,5), " mg/L (", trophlo, ") and ", round(limitup,5), " mg/L (", trophup, ").")
    })
    
    output$laketype <- renderPlot({
        library(ggplot2)
        
        MLk <- (area$For*input$coefF)+(area$Agr*input$coefA)+(area$Urb*input$coefU)+(lake$Ao*input$coefPr)+(NCap*input$coefSt*(1-input$coefSr))+0
        
        # p.areal.loading
        LHi <- (MHi/lake$Ao)*10^-3
        LLk <- (MLk/lake$Ao)*10^-3
        LLo <- (MLo/lake$Ao)*10^-3
        
        # p.lake
        PHi<- LHi/(11.6+(1.2*qs))
        PLk<- LLk/(11.6+(1.2*qs))
        PLo<- LLo/(11.6+(1.2*qs))
        
        # log.p
        logPLk <- log10(PLk)
        
        # error.uncertainty.confidence
        #Positive and negative model errors
        smpos <- (10^(logPLk+0.128))-PLk
        smneg <- (10^(logPLk-0.128))-PLk
        
        #Positive and negative loading errors
        sLpos <- (PHi-PLk)/2
        sLneg <- (PLk-PLo)/2
        
        #Positive and negative uncertainty
        sTpos <- sqrt((smpos)^2+(sLpos)^2)
        sTneg <- sqrt((smneg)^2+(sLneg)^2)
        
        # Confidence limits
        # multiple of prediction error, h=1=55% bounds, h=2=90% bounds
        hvar = sqrt(1/(2.25*(1-input$civar)))
        limitlo2 <- PLk-(hvar*sTneg)
        limitup2 <- PLk+(hvar*sTpos)
        
        rangex <- seq(0, 0.1, 0.005)
        rangeline <- data.frame(x = c(limitlo2, limitup2), y = c(1,1))
        trophicbins <- data.frame(xstart = c(0,0.01,0.02,0.05), xend = c(0.01,0.02,0.05,0.06), 
                                  Category = c("1.Oligotrophic", "2.Mesotrophic", "3.Eutrophic", "4.Hypertrophic"))
        
        ggplot() +
            theme_bw() +
            geom_rect(data=trophicbins, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Category), alpha = 0.4) +
            geom_line(data=rangeline, aes(x,y), color="blue", size=3) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y=element_blank()) +
            ggtitle("Higgins Lake Estimated Phosphorus Concentration") +
            xlab("mg/L")

    })
    
})

