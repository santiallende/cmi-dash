library(tidyverse)
library(vroom)

kansasExpoDat <- vroom("kansasExpoDat.csv")
balloonDat <- vroom("balloonDat.csv")
everDxDat <- vroom("everDxDat.csv")

expoAndDxUI <- function(id) {
        ns <- NS(id)
        tabItem(tabName = "ExpoAndDx",
                fluidRow(
                        infoBoxModuleExpoAndDxUI(ns("infoBoxExpoDxModule")),
                        box(
                                title = HTML("<b>ICD-10 Classifications Chart</b><br><br>
                                <large>Total number of times a condition within each class was endorsed. <br> <br>
                                Participants may have comorbidities within each class (i.e., repeat counts across conditions within class).</large>"),
                                width = 12,
                                fluidRow(
                                  column(6,
                                         radioButtons(ns("conflictButton"),
                                                      "Select Conflict",
                                                      choices = c("ODSS", "OIF/OEF/OND"),
                                                      selected = "ODSS",
                                                      inline = TRUE)
                                         ),
                                  column(6,
                                         radioButtons(ns("dataButton"),
                                                      "Select Data",
                                                      choices = c("All data, including participants with missing CMI score", 
                                                                  "Only participants with CMI score (will match data in first tab)"),
                                                      selected = "All data, including participants with missing CMI score",
                                                      inline = TRUE)
                                         )
                                ),
                                plotlyOutput(ns("stackedDxBarChart"), height = "600px")
                        )
                ),
                fluidRow(
                        tabBox(
                                width = 12,
                                tabPanel(
                                        title = "Toxic Exposures by Conflict",
                                        radioButtons(ns("dataButton1"),
                                                     "Select Data",
                                                     choices = c("All data, including participants with missing CMI score", 
                                                                 "Only participants with CMI score (will match data in first tab)"),
                                                     selected = "All data, including participants with missing CMI score",
                                                     inline = TRUE
                                        ),
                                        radioButtons(ns("splitDataButton"),
                                                     "Select 1st, 2nd, or last 3rd of exposures",
                                                     choices = c("First Third", "Second Third", "Last Third"),
                                                     selected = "First Third",
                                                     inline = TRUE
                                        ),
                                        plotlyOutput(ns("balloonBarChart1"), height = "600px")
                                ),
                                tabPanel(
                                        title = "Diagnoses by Conflict",
                                        radioButtons(ns("conflictButton1"),
                                                     "Select 1st, 2nd, or last 3rd of exposures",
                                                     choices = c("First Third", "Second Third", "Last Third"),
                                                     selected = "First Third",
                                                     inline = TRUE
                                        ),
                                        plotlyOutput(ns("balloonBarChart2"), height = "600px")
                                )
                        )
                ),
                fluidRow(
                        box(
                                title = HTML("<b>Contingency Chart: Diagnoses & Exposures</b><br><br>
                                <large>Note: consider base rates, vaccination coverage, etc & interpret with caution.</large>"),
                                width = 12,
                                radioButtons(ns("conflictButton2"),
                                             "Select Conflict",
                                             choices = c("ODSS", "OIF/OEF/OND"),
                                             selected = "ODSS",
                                             inline = TRUE
                                ),
                                radioButtons(ns("splitDataButton2"),
                                             "Select 1st, 2nd, or last 3rd of exposures",
                                             choices = c("First Third", "Second Third", "Last Third"),
                                             selected = "First Third",
                                             inline = TRUE
                                ),
                                plotlyOutput(ns("contingencyChart"), height = "650px")
                        )
                )
        )
}

## Static Data ##
expoBalloonDatWithCmi <- kansasExpoDat %>%
        dplyr::add_count(milConflictCode, exposureName, exposureValue, name = "exposureFreqByWar") %>%
        mutate(percent = round((exposureFreqByWar / 52) * 100, 2)) %>%
        select(dummyExpoSplit, milConflictCode, exposureName, exposureValue, percent) %>%
        distinct() %>%
        arrange(milConflictCode, exposureName)

expoBalloonDatNoCmi <- balloonDat

## End Static Data ##

expoAndDxServer <- function(id) {
        moduleServer(id, function(input, output, session) {
                ns <- session$ns
                
                # Cache filteredKansasCmiData based on input$dataButton
                filteredKansasCmiData <- reactive({
                        req(input$dataButton)
                        if (input$dataButton == "Only participants with CMI score (will match data in first tab)") {
                                everDxDat %>%
                                        filter(!is.na(sumKansasYes))
                        } else {
                                everDxDat
                        }
                }) %>% bindCache(input$dataButton)
                
                infoBoxModuleExpoAndDxServer("infoBoxExpoDxModule")
                
                ### Stacked Dx Bar Chart ###
                output$stackedDxBarChart <- renderPlotly({
                        req(input$conflictButton, input$dataButton)
                        
                        plot <- filteredKansasCmiData() %>%
                                filter(milConflictCode == input$conflictButton) %>%
                                select(id, condition, conditionClass, conditionYesNo) %>%
                                distinct() %>%
                                group_by(condition, conditionClass) %>%
                                summarise(sumConditionYesNo = sum(conditionYesNo), .groups = 'drop') %>%
                                group_by(conditionClass) %>%
                                mutate(totalConditionYesNo = sum(sumConditionYesNo)) %>%
                                ungroup() %>%
                                mutate(percentConditionYesNo = (sumConditionYesNo / totalConditionYesNo) * 100,
                                       conditionClass = fct_reorder(conditionClass, totalConditionYesNo, .desc = FALSE),
                                       condition = fct_reorder(condition, sumConditionYesNo, .desc = TRUE)) %>%
                                ggplot(aes(x = conditionClass, y = sumConditionYesNo, fill = condition, 
                                           text = paste0("<b>Condition:</b> ", condition, 
                                                         "<br><b>Total:</b> ", sumConditionYesNo, 
                                                         "<br><b>Percent:</b> ", round(percentConditionYesNo, 1), "%"))) +
                                geom_bar(stat = "identity", position = "stack") +
                                theme_economist() +
                                theme(legend.position = "none",
                                      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                                      plot.caption = element_text(hjust = 0)) +
                                coord_flip() +
                                labs(title = "", x = "", y = "Frequency")
                        
                        ggplotly(plot, tooltip = "text") %>%
                                layout(
                                        hoverlabel = list(align = "left"),
                                        margin = list(l = 100, r = 100, t = 50, b = 50)
                                ) %>%
                                config(displayModeBar = FALSE)
                })
                
                ### Balloon Plot ###
                filteredBallonData <- reactive({
                        req(input$dataButton1, input$splitDataButton)
                        data <- if (input$dataButton1 == "All data, including participants with missing CMI score") {
                                expoBalloonDatNoCmi
                        } else {
                                expoBalloonDatWithCmi
                        }
                        
                        splitData <- case_when(
                                input$splitDataButton == "First Third" ~ 0,
                                input$splitDataButton == "Second Third" ~ 1,
                                input$splitDataButton == "Last Third" ~ 2
                        )
                        
                        data %>%
                                filter(dummyExpoSplit == splitData)
                }) %>% bindCache(input$dataButton1, input$splitDataButton)
                
                output$balloonBarChart1 <- renderPlotly({
                        
                        data <- filteredBallonData()
                        
                        data$tooltipText <- paste0(
                                "<b>Exposure:</b> ", data$exposureName,
                                "<br><b>Conflict:</b> ", data$milConflictCode,
                                "<br><b>Percent:</b> ", data$percent, "%"
                        )
                        
                        plot <- ggballoonplot(data,
                                              x = "exposureName",
                                              y = "milConflictCode",
                                              size = "percent",
                                              fill = "exposureValue") +
                                facet_wrap(~ exposureValue) +
                                geom_point(aes(size = percent, text = tooltipText), color = NA) +
                                theme(legend.position = "none")
                        
                        ggplotly(plot, tooltip = "text") %>%
                                layout(margin = list(l = 100, r = 100, t = 50, b = 50)) %>%
                                config(displayModeBar = FALSE)
                })
                
                ### Contingency Chart ###
                filteredContingencyChartData <- reactive({
                        req(input$conflictButton2, input$splitDataButton2)
                        contingencyChartDat %>%
                                filter(milConflictCode == input$conflictButton2, splitGroup == input$splitDataButton2) %>%
                                mutate(exposureValue = factor(exposureValue, levels = c(0, 1), labels = c("No", "Yes")),
                                       hasConditionClass = factor(hasConditionClass, levels = c(0, 1), labels = c("No", "Yes"))) %>%
                                filter(exposureValue == "Yes" & hasConditionClass == "Yes")
                }) %>% bindCache(input$conflictButton2, input$splitDataButton2)
                
                output$contingencyChart <- renderPlotly({
                        data <- filteredContingencyChartData()
                        
                        # Create the custom tooltip text
                        data$tooltipText <- paste0(
                                "<b>Exposure:</b> ", data$exposureName,
                                "<br><b>Condition:</b> ", data$conditionClass,
                                "<br><b>Percent:</b> ", round(data$Percent, 1), "%"
                        )
                        
                        # Create the balloon plot with custom tooltips
                        plot <- ggballoonplot(
                                data,
                                x = "exposureName",
                                y = "conditionClass",
                                size = "Percent",
                                fill = "conditionClass"
                        ) +
                                theme_economist() +
                                geom_point(aes(size = Percent, text = tooltipText), color = NA) + # Ensure only one size aesthetic
                                scale_size_continuous(range = c(1, 8)) +
                                theme(legend.position = "none") +
                                labs(title = "", x = "Exposure", y = "Condition") +
                                theme(axis.text.x = element_text(angle = 45, hjust = 1))
                        
                        # Convert ggplot to plotly with custom tooltips
                        ggplotly(plot, tooltip = "text") %>%
                                layout(margin = list(l = 100, r = 100, t = 50, b = 50)) %>%
                                config(displayModeBar = FALSE)
                })
        })
}