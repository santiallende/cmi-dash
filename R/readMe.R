# readMe.R

readMeUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "readMe",
    fluidRow(
      column(
        width = 12,
        box(
          width = NULL,
          title = "Read Me: Understanding WRIISC CMI-Dash and Key Variables",
          status = "primary",
          solidHeader = TRUE,
          h3("Welcome to the WRIISC CMI-Dash"),
          p("The goal of this dashboard is to help you visualize and better understand WRIISC CA intake packet data. This dashboard 
            includes only deployed veterans. To get started, select a tab from the sidebar menu. If you encounter any issues or have questions, please contact santiago.allende@va.gov.
            This dashboard is still under development. Here are a few examples of things that need some work:"),
          tags$ul(
            tags$li("Tab/Page for data filtering, joining, and exporting to csv etc."),
            tags$li("Aesthetics... use consistent color palette, naming conventions, axis labels etc."),
            tags$li("Optimize code for better runtime performance.")
          ),
          
          h4("The sumKansasYes Variable and Chronic Multisymptom Illness (CMI)"),
          p("The sumKansasYes variable quantifies the total number of 'Yes' responses across multiple intake packet 
            items for each participant. This method was chosen to maximize data completeness and consistency, addressing 
            challenges of missing data and variations in scaling across different measures."),
          p("The 2021 VA/DoD Guidelines adopt an inclusive approach, using both the Kansas and Fukuda/CDC criteria 
            for CMI. This reflects a shift towards creating a unified definition, as noted in a 2017 GAO report:"),
          tags$blockquote("In a 2017 report on VA claims and Gulf War Illness (GWI), a historical, collective term for 
                          certain medical conditions among Veterans who have served in Southwest Asia since 1990, 
                          the U.S. Government Accountability Office (GAO) recommended the VA develop a plan to 
                          create a singular case definition of GWI/CMI. The GAO also noted that a 2014 IOM report 
                          recommended that the Kansas and CDC definitions be used in the interim."),
          p("The guidelines emphasize that CMI is not exclusive to military personnel or specific combat eras:"),
          tags$blockquote("While symptom-based illnesses may be particularly prevalent among deployed Veterans, CMI is not 
                          unique to those who have served in the military, those who served during any specific combat era, 
                          or those who were deployed to either combat or non-combat environments. Studies suggest that 
                          approximately 30% of primary care patients have a symptom-based illness and 40 – 49% have 
                          at least one medically unexplained symptom."),
          p("This represents a shift from the 2014 Guidelines, which limited the applicability of case definitions to the 1990-1991 Gulf War Veteran population."),
          p("The guidelines also acknowledge the complexity of CMI diagnosis:"),
          tags$blockquote("While symptoms of CMI should not be better accounted for by another behavioral health or 
                          physical health condition, patients with CMI often have multiple comorbidities. The presence of
                          other behavioral or physical health conditions that contribute to relevant symptoms does not 
                          preclude a diagnosis of CMI. Furthermore, CMI can overlap with other symptom-based conditions, 
                          such as fibromyalgia (FMS), irritable bowel syndrome (IBS), and myalgic encephalomyelitis/chronic 
                          fatigue syndrome (ME/CFS); therefore, a whole person approach is very important to management 
                          decisions for CMI patients."),
          h4("Here's a detailed breakdown of how the sumKansasYes variable was created:"),
          tags$ol(
            tags$li("Individual symptom scores were calculated from the intake data for various symptoms, including:"),
            tags$ul(
              tags$li("Fatigue and exercise intolerance"),
              tags$li("Sleep problems"),
              tags$li("Pain (muscle, joint, and widespread)"),
              tags$li("Cognitive issues (memory, concentration, word-finding)"),
              tags$li("Mood disturbances (depression, irritability)"),
              tags$li("Gastrointestinal symptoms"),
              tags$li("Respiratory symptoms"),
              tags$li("Neurological symptoms (dizziness, numbness)"),
              tags$li("Sensitivities (to light, chemicals, temperature)"),
              tags$li("Other physical symptoms (headaches, night sweats, rashes)")
            ),
            tags$li("For each symptom, a binary 'Yes' or 'No' score was calculated. For example:"),
            tags$ul(
              tags$li("Fatigue: Scored as 'Yes' if PHQ-15 fatigue item score was 1 or 2"),
              tags$li("Pain: Scored as 'Yes' if average pain score across neck, limb, and back was 3 or higher"),
              tags$li("Cognitive issues: Scored as 'Yes' if memory or concentration problems were rated 3 or higher")
            ),
            tags$li("These individual symptom scores were combined into a single dataset."),
            tags$li("The symptoms were grouped into six domains: Fatigue/Sleep, Pain, Neurocognitive/Mood, Gastrointestinal, Respiratory, and Skin (rashes)."),
            tags$li("For each domain, if any symptom within that domain was 'Yes', the entire domain was marked as 'Yes' (coded as 1) or 'No' (coded as 0)."),
            tags$li("The total number of 'Yes' domains was calculated for each participant (totalDomainYesNo)."),
            tags$li("Finally, the sumKansasYes was calculated by summing all individual 'Yes' responses across all symptoms for each participant.")
          ),
          p("This approach allows for an assessment of symptom burden while accounting for data limitations and
            measurement inconsistencies across different intake packet health indicators. The resulting sumKansasYes variable provides a 
            numerical representation of the total number of Kansas questionnaire items that each participant responded 'Yes' to, 
            offering a measure of overall symptom burden. Please note that this is only one method for deriving a 
            measure of CMI from the intake packet.")
        )
      )
    )
  )
}

readMeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server-side logic needed for this module
  })
}