exposuresCmiDxUI <- function(id) {
        ns <- NS(id)
        tabItem(tabName = "exposuresCmiDx",
                fluidRow(
                        infoBoxModuleExpoCmiDxUI(ns("infoBoxExpoCmiModule")),
                        # Density Plot Tab Boxes
                        tabBox(
                                title = "Density Plots",
                                width = 6,
                                tabPanel(
                                        title = "Exposure & CMI",
                                        selectInput(ns("exposureNameDensityPlot"), 
                                                    "Select Exposure",
                                                    choices = uniqueExposures, 
                                                    selected = uniqueExposures[1]),
                                        plotOutput(ns("exposureDensityPlot"))
                                ),
                                tabPanel(
                                        title = "Diagnoses & CMI",
                                        selectInput(ns("indDx"),
                                                    "Select Diagnosis",
                                                    choices = uniqueDx,
                                                    selected = uniqueDx[1]),
                                        plotOutput(ns("dxIndDensityPlot"))
                                ),
                                tabPanel(
                                        title = "Diagnostic Classes & CMI",
                                        selectInput(ns("dxClass"),
                                                    "Select Diagnostic Class",
                                                    choices = dxClass,
                                                    selected = dxClass[1]),
                                        plotOutput(ns("dxClassDensityPlot"))
                                )
                        ),
                        # Correlation Plot Tab Boxes
                        tabBox(
                                title = "Correlation Plots",
                                width = 6,
                                tabPanel(
                                        title = "Exposure & CMI",
                                        fluidRow(
                                                column(6,
                                                       radioButtons(ns("conflictButtonExpCmi"),
                                                                    "Select Conflict",
                                                                    choices = c("ODSS", "OIF/OEF/OND"),
                                                                    selected = "ODSS",
                                                                    inline = T)
                                                       ),
                                                column(6,
                                                       selectInput(ns("exposureNameCorrPlot"),
                                                                   "Select Exposure",
                                                                   choices = uniqueExposures,
                                                                   selected = uniqueExposures[1])
                                                       )
                                        ),
                                        plotOutput(ns("exposureCorrPlot"))
                                ),
                                tabPanel(
                                        title = "Diagnoses & CMI",
                                        fluidRow(
                                                column(6,
                                                       radioButtons(ns("conflictButtonDxCmi"),
                                                                    "Select Conflict",
                                                                    choices = c("ODSS", "OIF/OEF/OND"),
                                                                    selected = "ODSS",
                                                                    inline = T)
                                                       ),
                                                column(6,
                                                       selectInput(ns("dxCorrPlot"),
                                                                   "Select Diagnosis",
                                                                   choices = uniqueDx,
                                                                   selected = uniqueDx[1])
                                                       )
                                        ),
                                        plotOutput(ns("dxCorrPlot"))
                                ),
                                tabPanel(
                                        title = "Diagnostic Class & CMI",
                                        fluidRow(
                                                column(6,
                                                       radioButtons(ns("conflictButtonDxClassCmi"),
                                                                    "Select Conflict",
                                                                    choices = c("ODSS", "OIF/OEF/OND"),
                                                                    selected = "ODSS",
                                                                    inline = T)
                                                        ),
                                                column(6,
                                                       selectInput(ns("dxClassCorrPlot"),
                                                                   "Select Diagnostic Class",
                                                                   choices = dxClass,
                                                                   selected = dxClass[1])
                                                       )
                                        ),
                                        plotOutput(ns("dxClassCorrPlot"))
                                )
                        )
                ),
                fluidRow(
                        # Correlation Results Tab Boxes
                        tabBox(
                                title = "ODSS CMI Correlations Table",
                                width = 6,
                                tabPanel(
                                        title = "Exposures",
                                        DT::DTOutput(ns("expoCorrResultsOdss"))
                                ),
                                tabPanel(
                                        title = "Diagnoses",
                                        DT::DTOutput(ns("dxCorrResultsOdss"))
                                ),
                                tabPanel(
                                        title = "Diagnostic Classes",
                                        DT::DTOutput(ns("dxCorrClassResultsOdss"))
                                )
                        ),
                        tabBox(
                                title = "OIF/OEF/OND CMI Correlations Table",
                                width = 6,
                                tabPanel(
                                        title = "Exposures",
                                        DT::DTOutput(ns("expoCorrResultsOif"))
                                ),
                                tabPanel(
                                        title = "Diagnoses",
                                        DT::DTOutput(ns("dxCorrResultsOif"))
                                ),
                                tabPanel(
                                        title = "Diagnostic Classes",
                                        DT::DTOutput(ns("dxCorrClassResultsOif"))
                                )
                        )
                )
        )
}


## Static Data ##

# Exposure Corr Table Dat
expoCorTableDat <- kansasExpoDat %>%
        filter(exposureValue != "Don't Know") %>%
        mutate(exposureValue = ifelse(exposureValue == "Yes", 1, 0)) %>%
        group_by(milConflictCode, exposureName) %>%
        nest() %>%
        mutate(corTest = map(data, ~cor.test(.x$exposureValue, .x$sumKansasYes))) %>%
        mutate(corTest = map(corTest, broom::tidy)) %>% 
        unnest(corTest) %>%
        select(Conflict = milConflictCode, Exposure = exposureName, 
               "r" = estimate, t = statistic, 
               "p" = p.value, DF = parameter, "95% Confidence Interval: Low" = conf.low,
               "95% Confidence Interval: High" = conf.high) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 3)) %>%
        arrange(p)

# Ind Dx Corr Table Dat
dxCorTableDat <- everDxDat %>%
        group_by(milConflictCode, condition) %>%
        nest() %>%
        mutate(corTest = map(data, ~cor.test(.x$conditionYesNo, .x$sumKansasYes))) %>%
        mutate(corTest = map(corTest, broom::tidy)) %>%
        unnest(corTest) %>%
        select(Conflict = milConflictCode, Diagnosis = condition,
               "r" = estimate, t = statistic,
               "p" = p.value, DF = parameter, "95% Confidence Interval: Low" = conf.low,
               "95% Confidence Interval: High" = conf.high) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 3)) %>%
        arrange(p)

# Class Dx Corr Table Dat
dxClassCorTableDat <- everDxDat %>%
        select(id, milConflictCode, conditionClass, hasConditionClass, sumKansasYes) %>%
        distinct() %>%
        mutate(hasConditionClass = ifelse(hasConditionClass == "No", 0, 1)) %>%
        group_by(milConflictCode, conditionClass) %>%
        nest() %>%
        mutate(corTest = map(data, ~cor.test(.x$hasConditionClass, .x$sumKansasYes))) %>%
        mutate(corTest = map(corTest, broom::tidy)) %>%
        unnest(corTest) %>%
        select(Conflict = milConflictCode, "Diagnostic Class" = conditionClass,
               "r" = estimate, t = statistic,
               "p" = p.value, DF = parameter, "95% Confidence Interval: Low" = conf.low,
               "95% Confidence Interval: High" = conf.high) %>%
        ungroup() %>%
        mutate(across(where(is.numeric), round, 3)) %>%
        arrange(p) ###############################################see if i can save rds for faster load

## End Static Data

exposuresCmiDxServer <- function(id) {
        moduleServer(id, function(input, output, session) {
                ns <- session$ns
                
                # Call info box module
                infoBoxModuleExpoCmiDxServer("infoBoxExpoCmiModule")
                
                ### Exposures and CMI Panels ###
                
                # Density Plots: Exposures & CMI
                output$exposureDensityPlot <- renderPlot({
                        req(input$exposureNameDensityPlot)
                        kansasExpoDat %>%
                                filter(exposureName == input$exposureNameDensityPlot) %>%
                                ggplot(aes(x = sumKansasYes, y = exposureValue, fill = exposureValue)) +
                                geom_density_ridges(alpha = 0.6, 
                                                    quantile_lines = TRUE, 
                                                    quantiles = 2,
                                                    jittered_points = T) +
                                theme_ridges() +
                                theme(legend.position = "none") +
                                facet_grid(~ milConflictCode)
                })
                
                # Correlation Plot
                filteredDat <- reactive({
                        req(input$conflictButtonExpCmi, input$exposureNameCorrPlot)
                        kansasExpoDat %>%
                                filter(milConflictCode == input$conflictButtonExpCmi) %>%
                                filter(exposureName == input$exposureNameCorrPlot) %>%
                                filter(exposureValue != "Don't Know") %>%
                                mutate(exposureValue = ifelse(exposureValue == "No", 0, 1))
                }) %>% bindCache(input$conflictButtonExpCmi, input$exposureNameCorrPlot)
                
                output$exposureCorrPlot <- renderPlot({
                        req(input$conflictButtonExpCmi, input$exposureNameCorrPlot)
                        ggscatterstats(
                                data = filteredDat(),
                                x = exposureValue,
                                y = sumKansasYes,
                                title = "Correlation Plot",
                                subtitle = paste0("Conflict: ", input$conflictButton, ", Exposure: ", input$exposureNameCorrPlot),
                                xlab = "Exposure Value",
                                ylab = "Sum of Kansas Yes",
                                ggtheme = theme_minimal(base_size = 14),
                                ggstatsplot.layer = TRUE) +
                                ggplot2::geom_rug(sides = "b") +
                                scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
                                theme(
                                        plot.title = element_text(size = 16, face = "bold"),
                                        plot.subtitle = element_text(size = 14),
                                        axis.title = element_text(size = 14),
                                        axis.text = element_text(size = 12)
                                )
                })
                
                # Correlation Results Table: ODSS
                output$expoCorrResultsOdss <- DT::renderDT({
                        expoCorTableDat %>%
                                filter(Conflict == "ODSS") %>%
                                select(-Conflict)
                })
                
                # Correlation Results Table: OIF/OEF/OND
                output$expoCorrResultsOif <- DT::renderDT({
                        expoCorTableDat %>%
                                filter(Conflict == "OIF/OEF/OND") %>%
                                select(-Conflict)
                })
                
                ### Diagnoses and CMI Panels ###
                
                # Density Plots: Diagnoses & CMI
                output$dxIndDensityPlot <- renderPlot({
                        req(input$indDx)
                        everDxDat %>%
                                mutate(conditionYesNo = ifelse(conditionYesNo == 0, "No", "Yes")) %>%
                                filter(condition == input$indDx) %>%
                                ggplot(aes(x = sumKansasYes, y = conditionYesNo, fill = conditionYesNo)) +
                                geom_density_ridges(alpha = 0.6, 
                                                    quantile_lines = TRUE, 
                                                    quantiles = 2,
                                                    jittered_points = T) +
                                theme_ridges() +
                                theme(legend.position = "none") +
                                facet_grid(~ milConflictCode)
                })
                
                output$dxClassDensityPlot <- renderPlot({
                        req(input$dxClass)
                        everDxDat %>%
                                select(id, milConflictCode, conditionClass, hasConditionClass, sumKansasYes) %>%
                                distinct() %>%
                                filter(conditionClass == input$dxClass) %>%
                                ggplot(aes(x = sumKansasYes, y = hasConditionClass, fill = hasConditionClass)) +
                                geom_density_ridges(alpha = 0.6, 
                                                    quantile_lines = TRUE, 
                                                    quantiles = 2,
                                                    jittered_points = T) +
                                theme_ridges() +
                                theme(legend.position = "none") +
                                facet_grid(~ milConflictCode)
                })
                
                # Correlation Plot: Individual Dx
                filteredDxDat <- reactive({
                        req(input$conflictButtonDxCmi, input$dxCorrPlot)
                        everDxDat %>%
                                filter(milConflictCode == input$conflictButtonDxCmi) %>%
                                filter(condition == input$dxCorrPlot)
                }) %>% bindCache(input$conflictButtonDxCmi, input$dxCorrPlot)
                
                output$dxCorrPlot <- renderPlot({
                        ggscatterstats(
                                data = filteredDxDat(),
                                x = conditionYesNo,
                                y = sumKansasYes,
                                title = "Correlation Plot",
                                subtitle = paste0("Conflict: ", input$conflictButton, ", Diagnosis: ", input$dxCorrPlot),
                                xlab = "Diagnosis",
                                ylab = "Sum of Kansas Yes",
                                ggtheme = theme_minimal(base_size = 14),
                                ggstatsplot.layer = TRUE) +
                                ggplot2::geom_rug(sides = "b") +
                                scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
                                theme(
                                        plot.title = element_text(size = 16, face = "bold"),
                                        plot.subtitle = element_text(size = 14),
                                        axis.title = element_text(size = 14),
                                        axis.text = element_text(size = 12)
                                )
                })
                
                # Correlation Plot: Dx Class
                filteredDxClassDat <- reactive({
                        req(input$conflictButtonDxClassCmi, input$dxClassCorrPlot)
                        everDxDat %>%
                                select(id, milConflictCode, conditionClass, hasConditionClass, sumKansasYes) %>%
                                distinct() %>%
                                mutate(hasConditionClass = ifelse(hasConditionClass == "No", 0, 1)) %>%
                                filter(milConflictCode == input$conflictButtonDxClassCmi) %>%
                                filter(conditionClass == input$dxClassCorrPlot)
                }) %>% bindCache(input$conflictButtonDxClassCmi, input$dxClassCorrPlot)
                
                output$dxClassCorrPlot <- renderPlot({
                        ggscatterstats(
                                data = filteredDxClassDat(),
                                x = hasConditionClass,
                                y = sumKansasYes,
                                title = "Correlation Plot",
                                subtitle = paste0("Conflict: ", input$conflictButton, ", Diagnosis: ", input$dxClassCorrPlot),
                                xlab = "Diagnostic Class",
                                ylab = "Sum of Kansas Yes",
                                ggtheme = theme_minimal(base_size = 14),
                                ggstatsplot.layer = TRUE) +
                                ggplot2::geom_rug(sides = "b") +
                                scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
                                theme(
                                        plot.title = element_text(size = 16, face = "bold"),
                                        plot.subtitle = element_text(size = 14),
                                        axis.title = element_text(size = 14),
                                        axis.text = element_text(size = 12)
                                )
                })
                
                # Correlation Results Table: ODSS Ind Dx
                output$dxCorrResultsOdss <- DT::renderDT({
                        dxCorTableDat %>%
                                filter(Conflict == "ODSS") %>%
                                select(-Conflict)
                })
                
                # Correlation Results Table: OIF/OEF/OND Ind Dx
                output$dxCorrResultsOif <- DT::renderDT({
                        dxCorTableDat %>%
                                filter(Conflict == "OIF/OEF/OND") %>%
                                select(-Conflict)
                })
                
                # Correlation Results Table: ODSS Dx Class
                output$dxCorrClassResultsOdss <- DT::renderDT({
                        dxClassCorTableDat %>%
                                filter(Conflict == "ODSS") %>%
                                select(-Conflict)
                })
                
                # Correlation Results Table: OIF/OEF/OND Dx Class
                output$dxCorrClassResultsOif <- DT::renderDT({
                        dxClassCorTableDat %>%
                                filter(Conflict == "OIF/OEF/OND") %>%
                                select(-Conflict)
                })
                
        })
}

