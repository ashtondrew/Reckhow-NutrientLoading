#setwd("E:/P_Consulting/Proposals-Dev/Reckhow/LakeShiny")
lake <- read.csv("lake.csv")
coHi <- read.csv("coHi.csv")
coLo <- read.csv("coLo.csv")

# areal.water.loading
Q <- (lake$Ad*lake$r)+(lake$Ao*lake$Pr)
qs <- Q/lake$Ao

shinyServer(function(input, output) {
    
    output$range <- renderText({
        
        # export coefficients
        NCap <- (input$indP*(365/365)*input$unitP)+(input$indS*(input$daysS/365)*input$unitS)
        
        forest <- (input$areaF/100)*87.41
        agri <- (input$areaA/100)*87.41
        urban <- (input$areaU/100)*87.41
        
        # p.mass.loadings
        MHi <- (forest*coHi$EcFrHi)+(agri*coHi$EcAgHi)+(urban*coHi$EcUrHi)+(lake$Ao*coHi$EcPrHi)+(NCap*coHi$EcStHi*(1-coHi$SRHi))+coHi$PSIHi
        MLo <- (forest*coLo$EcFrLo)+(agri*coLo$EcAgLo)+(urban*coLo$EcUrLo)+(lake$Ao*coLo$EcPrLo)+(NCap*coLo$EcStLo*(1-coLo$SRLo))+coLo$PSILo
        MLk <- (forest*input$coefF)+(agri*input$coefA)+(urban*input$coefU)+(lake$Ao*input$coefPr)+(NCap*input$coefSt*(1-input$coefSr))+0
   
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
        
        # export coefficients
        NCap2 <- (input$indP*(365/365)*input$unitP)+(input$indS*(input$daysS/365)*input$unitS)
        
        forest <- (input$areaF/100)*87.41
        agri <- (input$areaA/100)*87.41
        urban <- (input$areaU/100)*87.41
        
        # p.mass.loadings
        MHi2 <- (forest*coHi$EcFrHi)+(agri*coHi$EcAgHi)+(urban*coHi$EcUrHi)+(lake$Ao*coHi$EcPrHi)+(NCap2*coHi$EcStHi*(1-coHi$SRHi))+coHi$PSIHi
        MLo2 <- (forest*coLo$EcFrLo)+(agri*coLo$EcAgLo)+(urban*coLo$EcUrLo)+(lake$Ao*coLo$EcPrLo)+(NCap2*coLo$EcStLo*(1-coLo$SRLo))+coLo$PSILo
        MLk2 <- (forest*input$coefF)+(agri*input$coefA)+(urban*input$coefU)+(lake$Ao*input$coefPr)+(NCap2*input$coefSt*(1-input$coefSr))+0
        
        # p.areal.loading
        LHi2 <- (MHi2/lake$Ao)*10^-3
        LLk2 <- (MLk2/lake$Ao)*10^-3
        LLo2 <- (MLo2/lake$Ao)*10^-3
        
        # p.lake
        PHi2<- LHi2/(11.6+(1.2*qs))
        PLk2<- LLk2/(11.6+(1.2*qs))
        PLo2<- LLo2/(11.6+(1.2*qs))
        
        # log.p
        logPLk2 <- log10(PLk2)
        
        # error.uncertainty.confidence
        #Positive and negative model errors
        smpos2 <- (10^(logPLk2+0.128))-PLk2
        smneg2 <- (10^(logPLk2-0.128))-PLk2
        
        #Positive and negative loading errors
        sLpos2 <- (PHi2-PLk2)/2
        sLneg2 <- (PLk2-PLo2)/2
        
        #Positive and negative uncertainty
        sTpos2 <- sqrt((smpos2)^2+(sLpos2)^2)
        sTneg2 <- sqrt((smneg2)^2+(sLneg2)^2)
        
        # Confidence limits
        # multiple of prediction error, h=1=55% bounds, h=2=90% bounds
        hvar2 = sqrt(1/(2.25*(1-input$civar)))
        limitlo2 <- PLk2-(hvar2*sTneg2)
        limitup2 <- PLk2+(hvar2*sTpos2)
        
        rangex2 <- seq(0, 0.1, 0.005)
        rangeline2 <- data.frame(x = c(limitlo2, limitup2), y = c(1,1))
        trophicbins2 <- data.frame(xstart = c(0,0.01,0.02,0.05), xend = c(0.01,0.02,0.05,0.06), 
                                  Category = c("1.Oligotrophic", "2.Mesotrophic", "3.Eutrophic", "4.Hypertrophic"))
        
        ggplot() +
            theme_bw() +
            geom_rect(data=trophicbins2, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Category), alpha = 0.4) +
            geom_line(data=rangeline2, aes(x,y), color="blue", size=3) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y=element_blank()) +
            ggtitle("Higgins Lake Estimated Phosphorus Concentration") +
            xlab("mg/L")

    })
    
    output$range2 <- renderText({
        
        # export coefficients
        NCap <- (input$indP*(365/365)*input$unitP)+(input$indS*(input$daysS/365)*input$unitS)
        
        forest <- (input$areaF/100)*87.41
        agri <- (input$areaA/100)*87.41
        urban <- (input$areaU/100)*87.41
        
        # p.mass.loadings
        MHi <- (forest*coHi$EcFrHi)+(agri*coHi$EcAgHi)+(urban*coHi$EcUrHi)+(lake$Ao*coHi$EcPrHi)+(NCap*coHi$EcStHi*(1-coHi$SRHi))+coHi$PSIHi
        MLo <- (forest*coLo$EcFrLo)+(agri*coLo$EcAgLo)+(urban*coLo$EcUrLo)+(lake$Ao*coLo$EcPrLo)+(NCap*coLo$EcStLo*(1-coLo$SRLo))+coLo$PSILo
        MLk <- (forest*input$coefF)+(agri*input$coefA)+(urban*input$coefU)+(lake$Ao*input$coefPr)+(NCap*input$coefSt*(1-input$coefSr))+0
        
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
    
    output$laketype2 <- renderPlot({
        library(ggplot2)
        
        # export coefficients
        NCap2 <- (input$indP*(365/365)*input$unitP)+(input$indS*(input$daysS/365)*input$unitS)
        
        forest <- (input$areaF/100)*87.41
        agri <- (input$areaA/100)*87.41
        urban <- (input$areaU/100)*87.41
        
        # p.mass.loadings
        MHi2 <- (forest*coHi$EcFrHi)+(agri*coHi$EcAgHi)+(urban*coHi$EcUrHi)+(lake$Ao*coHi$EcPrHi)+(NCap2*coHi$EcStHi*(1-coHi$SRHi))+coHi$PSIHi
        MLo2 <- (forest*coLo$EcFrLo)+(agri*coLo$EcAgLo)+(urban*coLo$EcUrLo)+(lake$Ao*coLo$EcPrLo)+(NCap2*coLo$EcStLo*(1-coLo$SRLo))+coLo$PSILo
        MLk2 <- (forest*input$coefF)+(agri*input$coefA)+(urban*input$coefU)+(lake$Ao*input$coefPr)+(NCap2*input$coefSt*(1-input$coefSr))+0
        
        # p.areal.loading
        LHi2 <- (MHi2/lake$Ao)*10^-3
        LLk2 <- (MLk2/lake$Ao)*10^-3
        LLo2 <- (MLo2/lake$Ao)*10^-3
        
        # p.lake
        PHi2<- LHi2/(11.6+(1.2*qs))
        PLk2<- LLk2/(11.6+(1.2*qs))
        PLo2<- LLo2/(11.6+(1.2*qs))
        
        # log.p
        logPLk2 <- log10(PLk2)
        
        # error.uncertainty.confidence
        #Positive and negative model errors
        smpos2 <- (10^(logPLk2+0.128))-PLk2
        smneg2 <- (10^(logPLk2-0.128))-PLk2
        
        #Positive and negative loading errors
        sLpos2 <- (PHi2-PLk2)/2
        sLneg2 <- (PLk2-PLo2)/2
        
        #Positive and negative uncertainty
        sTpos2 <- sqrt((smpos2)^2+(sLpos2)^2)
        sTneg2 <- sqrt((smneg2)^2+(sLneg2)^2)
        
        # Confidence limits
        # multiple of prediction error, h=1=55% bounds, h=2=90% bounds
        hvar2 = sqrt(1/(2.25*(1-input$civar2)))
        limitlo2 <- PLk2-(hvar2*sTneg2)
        limitup2 <- PLk2+(hvar2*sTpos2)
        
        rangex2 <- seq(0, 0.1, 0.005)
        rangeline2 <- data.frame(x = c(limitlo2, limitup2), y = c(1,1))
        trophicbins2 <- data.frame(xstart = c(0,0.01,0.02,0.05), xend = c(0.01,0.02,0.05,0.06), 
                                   Category = c("1.Oligotrophic", "2.Mesotrophic", "3.Eutrophic", "4.Hypertrophic"))
        
        ggplot() +
            theme_bw() +
            geom_rect(data=trophicbins2, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = Category), alpha = 0.4) +
            geom_line(data=rangeline2, aes(x,y), color="blue", size=3) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y=element_blank()) +
            ggtitle("Higgins Lake Estimated Phosphorus Concentration") +
            xlab("mg/L")
        
    })
    
    output$totperc <- renderText({
        percentage = input$areaF + input$areaA + input$areaU
        paste0("Total land use percentage should not exceed 100%.  Current selected values sum to: ", percentage, "%")
    })
    
})

