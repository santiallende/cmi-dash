library(vroom)
# Load Data
ageGenderDat <- vroom("ageGenderDat.csv") %>% filter(age < 105 & age > 15)
contingencyChartDemoInfoBoxDat <- vroom("contingencyChartDemoInfoBoxDat.csv")

# Define UI
infoBoxModuleExpoAndDxUI <- function(id) {
        ns <- NS(id)
        tagList(
                fluidRow(
                        column(width = 12, align = "center",
                               selectInput(ns("selectData"),
                                           "Select Data Source (chart) for Info Boxes (note: some Vets were in both conflicts)",
                                           choices = c("ICD-10 Classifications Chart w/ CMI",
                                                       "ICD-10 Classifications Chart w/ Out CMI",
                                                       "Toxic Exposures By Conflict Chart w/ CMI",
                                                       "Toxic Exposures By Conflict Chart w/ Out CMI",
                                                       "Contingency Chart: Diagnoses & Exposures"),
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

#ICD-10 Chart in expoAndDx.R (contains num of deployments, if i'd like to add)
icd10ChartKansasCmiDatWithCmi <- everDxDat %>% 
  filter(!is.na(sumKansasYes)) %>%
  select(id, milConflictCode) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")

icd10ChartKansasCmiDatNoCmi <- everDxDat %>% 
  select(id, milConflictCode) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")

#expoBalloonDat in expoAndDx.R
expoBalloonDatWithCmiDemoDat <- kansasExpoDat %>%
  add_count(milConflictCode, exposureName, exposureValue, name = "exposureFreqByWar") %>%
  mutate(percent = round((exposureFreqByWar / 52) * 100, 2)) %>%
  select(id, dummyExpoSplit, milConflictCode, exposureName, exposureValue, percent) %>%
  distinct() %>%
  arrange(milConflictCode, exposureName) %>% 
  select(id, milConflictCode) %>% 
  distinct() %>%
  left_join(., ageGenderDat, "id")

expoBalloonDatNoCmiDemoDat <- balloonDat %>%
  select(id, milConflictCode) %>%
  distinct() %>%
  left_join(., ageGenderDat, "id")

# Contingency Chart in expoAndDx.R
contingencyChartDemoDat <- contingencyChartDemoInfoBoxDat %>%
  left_join(., ageGenderDat, "id")

## End Static Data ##

# Define Server
infoBoxModuleExpoAndDxServer <- function(id) {
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
             "Toxic Exposures By Conflict Chart w/ CMI" = expoBalloonDatWithCmiDemoDat,
             "Toxic Exposures By Conflict Chart w/ Out CMI" = expoBalloonDatNoCmiDemoDat,
             "ICD-10 Classifications Chart w/ CMI" = icd10ChartKansasCmiDatWithCmi,
             "ICD-10 Classifications Chart w/ Out CMI" = icd10ChartKansasCmiDatNoCmi,
             "Contingency Chart: Diagnoses & Exposures" = contingencyChartDemoDat
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