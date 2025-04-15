library(vroom)
# Load Data
ageGenderDat <- vroom("ageGenderDat.csv") %>% filter(age < 105 & age > 15)

# Define UI
infoBoxModuleExpoCmiDxUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, align = "center",
             selectInput(ns("selectData"),
                         "Select Data Source (i.e., plot below) for Info Boxes (note: some Vets were in both conflicts)",
                         choices = c("Exposures and CMI Density Plot (top left 1st tab)",
                                     "Diagnoses & CMI Density Plot (top left 2nd tab)",
                                     "Diagnostic Classes & CMI Density Plot (top left 3rd tab)",
                                     "Exposures & CMI Correlation Plot (top right 1st tab)",
                                     "Diagnoses & CMI Correlation Plot (top right 2nd tab)",
                                      "Diagnostic Class & CMI Correlation Plot (top right 3rd tab)"),
                         selected = "ICD-10 Classifications Chart w/ CMI",
                         width = '100%'
             )
      )
    ),
    fluidRow(
      infoBoxOutput(ns("totalStats"), width = 4),
      infoBoxOutput(ns("odssStats"), width = 4),
      infoBoxOutput(ns("oifStats"), width = 4)
    )
  )
}

## Static Data ##

#expoNameDensityPlot Dat
expoNameDensPlotDat <- kansasExpoDat %>% 
  select(id, milConflictCode) %>% 
  distinct() %>% 
  left_join(., ageGenderDat, "id")

#indDx: Diagnosis & CMI
dxIndDensityPlotDat <- everDxDat %>%
  select(id, sumKansasYes, milConflictCode) %>%
  filter(!is.na(sumKansasYes)) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")

#dxClassDensityPlotDat Dx Class & CMI
dxClassDensityPlotDat <- everDxDat %>%
  select(id, milConflictCode, conditionClass, 
         hasConditionClass, sumKansasYes) %>%
  filter(!is.na(sumKansasYes)) %>%
  select(id, milConflictCode) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")

#exposureCorrPlot Exposures & CMI Corr Plot
exposureCorrPlotDat <- kansasExpoDat %>%
  filter(exposureValue != "Don't Know") %>%
  mutate(exposureValue = ifelse(exposureValue == "No", 0, 1)) %>%
  select(id, milConflictCode) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")

#dxCorrPlot Diagnoses & CMI Corr Plot
dxCorrPlotDat <- everDxDat %>%
  filter(!is.na(sumKansasYes)) %>%
  select(id, milConflictCode) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")

#dxClassCorrPlot Diagnostic Class & CMI Corr Plot
dxClassCorrPlotDat <- everDxDat %>%
  select(id, milConflictCode, conditionClass, hasConditionClass, sumKansasYes) %>%
  distinct() %>%
  mutate(hasConditionClass = ifelse(hasConditionClass == "No", 0, 1)) %>%
  select(id, milConflictCode) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")


## End Static Data ##

# Define Server
infoBoxModuleExpoCmiDxServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    calcStats <- function(data) {
      if (!is.data.frame(data)) {
        stop("Data must be a data frame")
      }
      
      meanAge <- mean(as.numeric(data$age), na.rm = TRUE)
      medianAge <- median(as.numeric(data$age), na.rm = TRUE)
      nPatients <- n_distinct(data$id, na.rm = TRUE)
      percentMale <- data %>%
        filter(gender == "Male") %>%
        summarise(percent = n() / nrow(data) * 100) %>%
        pull(percent)
      
      list(
        meanAge = meanAge,
        medianAge = medianAge,
        nPatients = nPatients,
        percentMale = percentMale
      )
    }
    
    reactiveData <- reactive({
      switch(input$selectData,
             "Exposures and CMI Density Plot (top left 1st tab)" = expoNameDensPlotDat,
             "Diagnoses & CMI Density Plot (top left 2nd tab)" = dxIndDensityPlotDat,
             "Diagnostic Classes & CMI Density Plot (top left 3rd tab)" = dxClassDensityPlotDat,
             "Exposures & CMI Correlation Plot (top right 1st tab)" = icd10ChartKansasCmiDatNoCmi,
             "Diagnoses & CMI Correlation Plot (top right 2nd tab)" = dxCorrPlotDat,
             "Diagnostic Class & CMI Correlation Plot (top right 3rd tab)" = dxClassCorrPlotDat
      )
    }) %>% bindCache(input$selectData)
    
    selectedStats <- reactive({
      data <- reactiveData()
      list(
        totalStats = calcStats(data %>% filter(milConflictCode %in% c("ODSS", "OIF/OEF/OND"))),
        odssStats = calcStats(data %>% filter(milConflictCode == "ODSS")),
        oifStats = calcStats(data %>% filter(milConflictCode == "OIF/OEF/OND"))
      )
    }) %>% bindCache(input$selectData, reactiveData)
    
    output$totalStats <- renderInfoBox({
      stats <- selectedStats()$totalStats
      infoBox(
        title = "Total Statistics",
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:12px; font-weight:normal;'>",
            "<strong>Mean Age:</strong> ", round(stats$meanAge, 2), "<br>",
            "<strong>Median Age:</strong> ", round(stats$medianAge, 2), "<br>",
            "<strong>Number of Patients:</strong> ", stats$nPatients, "<br>",
            "<strong>Percent Male:</strong> ", round(stats$percentMale, 2), "%",
            "</div>"
          )
        ),
        icon = icon("user"),
        color = "red"
      )
    })
    
    output$odssStats <- renderInfoBox({
      stats <- selectedStats()$odssStats
      infoBox(
        title = "ODSS Statistics",
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:12px; font-weight:normal;'>",
            "<strong>Mean Age:</strong> ", round(stats$meanAge, 2), "<br>",
            "<strong>Median Age:</strong> ", round(stats$medianAge, 2), "<br>",
            "<strong>Number of Patients:</strong> ", stats$nPatients, "<br>",
            "<strong>Percent Male:</strong> ", round(stats$percentMale, 2), "%",
            "</div>"
          )
        ),
        icon = icon("user"),
        color = "green"
      )
    })
    
    output$oifStats <- renderInfoBox({
      stats <- selectedStats()$oifStats
      infoBox(
        title = "OIF/OEF/OND Statistics",
        value = HTML(
          paste0(
            "<div style='text-align:left; font-size:12px; font-weight:normal;'>",
            "<strong>Mean Age:</strong> ", round(stats$meanAge, 2), "<br>",
            "<strong>Median Age:</strong> ", round(stats$medianAge, 2), "<br>",
            "<strong>Number of Patients:</strong> ", stats$nPatients, "<br>",
            "<strong>Percent Male:</strong> ", round(stats$percentMale, 2), "%",
            "</div>"
          )
        ),
        icon = icon("user"),
        color = "blue"
      )
    })
  })
}