library(shiny)
library(shinythemes)
library(ggplot2)
library(lubridate)
library(broom)
library(stringr)
library(dplyr)
library(scales)

# Cost assumptions
cost1 <- 5.2e6   # cost per fatality
cost2 <- 1e6     # cost per serious injury

ui <- navbarPage(
  title = "Aviation Accident Severity Analysis",
  theme = shinytheme("cosmo"),
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600;700&display=swap');
      
      body { 
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        font-family: 'Poppins', sans-serif;
        min-height: 100vh;
      }
      
      .navbar {
        background: rgba(0, 0, 0, 0.95) !important;
        backdrop-filter: blur(10px);
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
      }
      
      .navbar-brand {
        font-weight: 700 !important;
        font-size: 24px !important;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      
      .container-fluid {
        padding: 40px;
      }
      
      .content-section { 
        background: white;
        padding: 40px;
        border-radius: 20px;
        box-shadow: 0 10px 40px rgba(0,0,0,0.15);
        margin: 20px 0;
        animation: fadeIn 0.6s ease-in;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .stat-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 35px;
        border-radius: 20px;
        text-align: center;
        margin: 10px;
        box-shadow: 0 8px 25px rgba(102, 126, 234, 0.4);
        transition: transform 0.3s ease, box-shadow 0.3s ease;
      }
      
      .stat-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 12px 35px rgba(102, 126, 234, 0.5);
      }
      
      .stat-number { 
        font-size: 48px; 
        font-weight: 700; 
        margin-bottom: 10px;
        text-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }
      
      .stat-label { 
        font-size: 18px; 
        opacity: 0.95;
        font-weight: 300;
      }
      
      h2 { 
        color: #2c3e50; 
        margin-top: 10px; 
        margin-bottom: 30px;
        font-weight: 700;
        font-size: 36px;
      }
      
      h3 { 
        color: #667eea; 
        margin-top: 40px; 
        margin-bottom: 20px;
        font-weight: 600;
        font-size: 28px;
      }
      
      h4 { 
        color: #2c3e50; 
        margin-top: 25px;
        font-weight: 600;
      }
      
      .key-finding {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0e8f8 100%);
        border-left: 6px solid #667eea;
        padding: 25px;
        margin: 30px 0;
        border-radius: 12px;
        box-shadow: 0 4px 15px rgba(102, 126, 234, 0.1);
      }
      
      .workflow-box {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        border-radius: 15px;
        padding: 30px;
        margin: 20px 0;
        box-shadow: 0 5px 20px rgba(0,0,0,0.08);
      }
      
      p, li { 
        line-height: 1.8;
        font-size: 16px;
      }
      
      .table {
        box-shadow: 0 2px 10px rgba(0,0,0,0.05);
        border-radius: 10px;
        overflow: hidden;
      }
      
      .table thead {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
      }
      
      .shiny-plot-output {
        border-radius: 15px;
        overflow: hidden;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
      }
      
      code {
        background: #f8f9fa;
        padding: 3px 8px;
        border-radius: 5px;
        color: #667eea;
        font-weight: 500;
      }
      
      .intro-hero {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 60px;
        border-radius: 20px;
        margin-bottom: 30px;
        box-shadow: 0 10px 40px rgba(102, 126, 234, 0.3);
      }
      
      .intro-hero h1 {
        font-size: 48px;
        font-weight: 700;
        margin-bottom: 20px;
        text-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }
      
      /* Fix pill tab contrast */
      .nav-pills > li > a {
        background-color: #f8f9fa !important;
        color: #2c3e50 !important;
        font-weight: 600 !important;
        border-radius: 10px !important;
        margin-right: 10px !important;
        padding: 12px 24px !important;
        transition: all 0.3s ease !important;
        border: 2px solid transparent !important;
      }
      
      .nav-pills > li > a:hover {
        background-color: #e9ecef !important;
        color: #1a252f !important;
        border-color: #667eea !important;
      }
      
      .nav-pills > li.active > a,
      .nav-pills > li.active > a:hover,
      .nav-pills > li.active > a:focus {
        background-color: #2c3e50 !important;
        color: white !important;
        box-shadow: 0 4px 20px rgba(44, 62, 80, 0.5) !important;
        border-color: #2c3e50 !important;
      }
    "))
  ),
  
  # ---- Introduction Tab ----
  tabPanel(
    "Introduction",
    icon = icon("plane-departure"),
    fluidPage(
      div(class = "intro-hero",
          h1("Aviation Accident Cost Analysis"),
          p(style = "font-size: 24px; font-weight: 300;",
            "A Data-Driven Six Sigma Approach to Understanding Financial Severity")
      ),
      
      div(class = "content-section",
          h2("Background"),
          p(style = "font-size: 18px;",
            "The aviation industry has advanced greatly in the past century, but there continue 
            to be severe aircraft crashes each year, with individual accidents costing aviation  
            insurance companies between hundreds of thousands to tens of millions of dollars.  
            Currently, insurance companies and aviation safety regulators lack a standardized,  
            data-driven model for quantifying how operational and environmental factors 
            contribute to aviation accident costs."
          ),
          
          hr(style = "border-top: 2px solid #667eea; margin: 40px 0;"),
          
          h2("Research Objective"),
          h3(style = "font-size: 32px; color: #2c3e50;",
             "How can measurable operational and environmental factors predict financial losses from aviation accidents 
             to inform aviation insurance companies about pricing strategies and risk management?"),
          
          hr(style = "border-top: 2px solid #667eea; margin: 40px 0;")
      ),
      
      # Project Team Section - Compact
      div(class = "content-section",
          style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); 
                   border-top: 4px solid #667eea; padding: 25px 40px;",
          p(style = "font-size: 16px; line-height: 1.8; margin-bottom: 8px; text-align: center;",
            icon("users", style = "color: #667eea;"), " ",
            strong("Team:"), "Eileen Ho, Harish Kamble, Maia Marshall, Sahara Shahab, Mark Tarazi"
          ),
          p(style = "font-size: 16px; line-height: 1.8; margin-bottom: 0; text-align: center;",
            icon("graduation-cap", style = "color: #667eea;"), " ",
            "SYSEN 5300", " | Team 13 | Cornell University | Fall 2025"
          )
      )
    )
  ),
  
  # ---- Data Tab ----
  tabPanel(
    "Data",
    icon = icon("database"),
    fluidPage(
      div(class = "content-section",
          h2("Dataset Overview"),
          
          div(class = "key-finding",
              icon("info-circle"), " ", strong("Data Source: "), 
              "NTSB Aviation Accident Database containing comprehensive 
              information about aviation accidents in the United States from 1982 to 2022."
          ),
          
          h3("Data Preprocessing Pipeline"),
          div(class = "workflow-box",
              tags$ul(style = "font-size: 16px; margin: 0;",
                      tags$li("Obtain data"),
                      tags$li("Select relevant variables"),
                      tags$li("Filter for only airplane accidents"),
                      tags$li("Remove accidents with missing values"),
                      tags$li("Create derived variables, 'is_fatal' and 'cost'"),
                      tags$li("Obtain final dataset: ", 
                              strong(textOutput("n_obs", inline = TRUE)), " accidents analyzed")
              )
          ),
          
          h3("Key Variables"),
          div(style = "overflow-x: auto;",
              tableOutput("variables_table")
          ),
          
          h3("Derived Variables"),
          div(style = "overflow-x: auto;",
              tableOutput("derivedvariables_table")
          )
      )
    )
  ),
  
  # ---- Method Tab ----
  tabPanel(
    "Method",
    icon = icon("chart-line"),
    fluidPage(
      div(class = "content-section",
          
          h2("Modeling Approach"),
          
          # -----------------------------
          # Statistical Framework
          # -----------------------------
          h3("Statistical Framework"),
          tags$ul(
            tags$li("Outcome: Total financial cost from injuries and fatalities per accident"),
            tags$li("Rationale: Airlines and insurers must compensate crash victims and/or thier families, making cost a meaningful metric"),
            tags$li("Method: Response Surface Methodology (RSM) implemented in R"),
            tags$li("Goal: Assess how accident cost changes with aircraft, weather, and flight-phase characteristics"),
            tags$li("Approach: Develop and evaluate multiple regression models to identify the best predictive fit")
          ),
          
          # -----------------------------
          # Model Formula (Final)
          # -----------------------------
          div(class = "key-finding",
              strong("Final Model Formula:"), br(), br(),
              tags$code(
                "log(Total_Cost + 1) ~ Number_of_Engines + Weather_Condition +
       Engine_Type + Aircraft_Damage + is_fatal",
                style = "font-size: 14px;"
              ),
              br(), br(),
              p("This model predicts accident cost based on aircraft configuration, weather conditions, damage severity, and whether the accident was fatal.")
          ),
          #Model Assumptions
          h3("Model Assumptions"),
          fluidRow(
            column(6,
                   div(class = "workflow-box",
                       h4(icon("dollar-sign"), " Cost Assumptions"),
                       tags$ul(style = "font-size: 15px;",
                               tags$li("$5.2M compensation per fatality"),
                               tags$li("$1M compensation per serious injury"),
                               tags$li("Values applied consistently across accidents"),
                               tags$li("Based on aviation litigation averages (Healy-Pratt & Hanna, 2021)")
                       )
                   )
            )
          ),
          # -----------------------------
          # Model Development & Comparison
          # -----------------------------
          h3("Model Development & Comparison"),
          p(style = "font-size: 16px;",
            "Three regression models of increasing complexity were constructed to evaluate predictive 
          performance:"
          ),
          tags$ul(style = "font-size: 15px;",
                  tags$li(strong("Model 1:"), " Engine type, weather conditions, and number of engines"),
                  tags$li(strong("Model 2:"), " Model 1 + aircraft damage factor"),
                  tags$li(strong("Model 3:"), " Model 2 + fatality indicator")
          ),
          p(style = "font-size: 16px;",
            "Models were compared based on the R² value"
          ),
          
          # Optional: Insert workflow diagram (kept from your original template)
          h3("Project Workflow"),
          plotOutput("workflow_diagram", height = "600px")
          
          # -----------------------------
          # Predictive Heat Map
          # -----------------------------
          # h3("Engine Configuration Cost Heat Map"),
          # p(style = "font-size: 16px;",
          #   "To explore how aircraft design affects accident cost, a heat map was generated using
          # Model 3 predictions. The visualization displays expected financial impact across engine
          # configurations:"
          # ),
          # tags$ul(style = "font-size: 15px;",
          #         tags$li(strong("X-Axis:"), " Engine type (e.g., Turbofan, Piston, Turbojet)"),
          #         tags$li(strong("Y-Axis:"), " Number of engines"),
          #         tags$li(strong("Color Scale:"), " Mean predicted accident cost")
          # ),
          # p(style = "font-size: 16px;",
          #   "This visualization helps identify high-cost engine configurations and supports risk-informed
          # design and operational decision-making."
          # ),
          
          # -----------------------------
          # Scenario Simulation
          # -----------------------------
          # h3("Scenario Simulation: IMC vs. Hypothetical All-VMC Conditions"),
          # p(style = "font-size: 16px;",
          #   "A counterfactual scenario was simulated in which all accidents occurred under visual 
          # meteorological conditions (VMC). Predicted costs under this scenario were compared to
          # costs under the original mixed weather conditions to estimate the financial impact 
          # attributable to IMC environments."
          # ),
          # p(style = "font-size: 16px;",
          #   "This analysis highlights how adverse environmental conditions influence accident severity
          # and the resulting financial burden."
          # ),
      )
    )
  ),
  
  tabPanel(
    "Results",
    icon = icon("chart-bar"),
    fluidPage(
      div(class = "content-section",
          h2("Regression Results & Key Predictors"),
          
          
          
          h3("Model Performance Comparison"),
          p(style = "font-size: 16px;",
            "Progressive addition of predictors improves model fit (R²). Model 3 includes all 
            key factors and achieves the best explanatory power."
          ),
          
          fluidRow(
            column(4, ""),   # empty space
            column(
              4,
              div(style = "text-align: center; margin-bottom: 20px;",
                  selectInput(
                    inputId = "r2_mode",
                    label = "Select R² View:",
                    choices = c("Overall R²"),
                    selected = "Overall R²",
                    width = "300px"
                  )
              )
            ),
            column(4, "")    # empty space
          ),
          
          plotOutput("r2_plot", height = "450px"),
          
          div(class = "key-finding",
              icon("lightbulb"), " ", strong("Key Finding: "), 
              "Aircraft damage severity and fatal outcomes carry much more predictive 
              information about cost than engine configuration or weather alone."
          ),
          
          h3("Cost Predictions by Engine Configuration"),
          p(style = "font-size: 16px;",
            "Average predicted accident costs vary significantly by engine type and number of engines. 
            This heatmap shows the financial risk profile across different aircraft configurations."
          ),
          div(class = "key-finding",
              icon("microscope"), " ", strong("Heatmap Develoment:"),
              tags$ul(style = "font-size: 15px; margin-top: 10px; line-height: 1.7;",
                      tags$li("Model 3 predicts cost for every possible combination of engine number, engine type, aircraft damage, weather, and and whether the accident was fatal"),
                      tags$li("Predictions are averaged across all damage/weather/fatality levels for each engine count × engine type cell"),
                      tags$li("Lighter colors = higher cost. Multi-engine ", strong("Turbofan/Turboprop"), " aircraft show the ", strong("highest"), " average financial severity")
              )
          ),
          plotOutput("heatmap_plot", height = "550px")
      )
    )
  ),
  
  # ---- Financial Impact Tab ----
  tabPanel(
    "Financial Impact",
    icon = icon("dollar-sign"),
    fluidPage(
      
      # Sub-navigation tabs for two scenarios
      tabsetPanel(
        type = "pills",
        
        # ===== TAB 1: Weather Scenario =====
        tabPanel(
          "Weather Impact",
          icon = icon("cloud-rain"),
          br(),
          
          div(class = "content-section",
              h2("Scenario Analysis: Impact of Weather Conditions"),
              
              HTML("<strong>Weather Conditions:</strong>
        VMC and IMC conditions refer to the two main categories of visual conditions pilots operate under<br><br>
        <strong>Visual Meteorological Conditions (VMC):</strong> Clear weather where pilots can navigate visually<br><br>
        <strong>Instrument Meteorological Conditions (IMC):</strong> Poor visibility requiring instrument navigation<br><br>
        <strong>Predicted average cost associated with accidents under:</strong> 
        <ul>
          <li>Current conditions (mix of VMC/IMC)</li>
          <li>Hypothetical scenario (all VMC)</li>
        </ul>
        "),
              
              h3("Cost Comparison"),
              tableOutput("finance_plot"),
          ),
          
          fluidRow(
            column(12,
                   div(class = "stat-box",
                       style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
             box-shadow: 0 8px 25px rgba(245, 87, 108, 0.4);
             border-radius: 16px;
             color: white;
             text-align: center;
             padding: 16px 20px;
             margin-bottom: 20px;",
                       
                       div(class = "stat-number",
                           style = "font-size: 3.8rem;
                font-weight: 900;
                line-height: 1;
                margin-bottom: 6px;",
                           textOutput("savings_display", inline = TRUE)
                       ),
                       
                       div(class = "stat-label",
                           style = "font-size: 2rem;
                opacity: 0.95;
                margin-bottom: 8px;",
                           "VMC Scenario Cost - Original Scenario Cost"
                       ),
                       
                       div(class = "stat-ci",
                           style = "font-size: 2rem;
                font-weight: 600;
                opacity: 0.9;",
                           textOutput("savings_ci", inline = TRUE)
                       )
                   )
            )
          ),
          
          fluidRow(
            column(3,
                   div(class = "workflow-box",
                       style = "margin-top: 20px;",
                       selectInput("year_filter", 
                                   "Select Year:",
                                   choices = NULL,
                                   selected = NULL)
                   )
            ),
            column(9,
                   div(class = "content-section",
                       style = "margin-top: 20px;",
                       h3("Annual Cost Comparison by Year"),
                       plotOutput("annual_cost_plot", height = "400px"),
                       h3("Annual Cost Summary"),
                       tableOutput("annual_summary_table")
                   )
            )
          )
        ),
        
        # ===== TAB 2: Fatality Prevention Scenario =====
        tabPanel(
          "Fatality Prevention",
          icon = icon("heart-pulse"),
          br(),
          
          div(class = "content-section",
              h2("Scenario Analysis: Impact of Preventing Fatalities"),
              
              HTML("<strong>Fatality Prevention:</strong>
        This analysis estimates the financial impact of preventing fatal outcomes in aviation accidents<br><br>
        <strong>Current Conditions:</strong> Mix of fatal and non-fatal accidents<br><br>
        <strong>Hypothetical Scenario:</strong> All accidents result in non-fatal outcomes only<br><br>
        <strong>Predicted average cost associated with accidents under:</strong> 
        <ul>
          <li>Current conditions (with fatalities)</li>
          <li>Hypothetical scenario (no fatalities)</li>
        </ul>
        "),
              
              h3("Cost Comparison"),
              tableOutput("fatality_finance_plot"),
          ),
          
          fluidRow(
            column(12,
                   div(class = "stat-box",
                       style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
              box-shadow: 0 8px 25px rgba(245, 87, 108, 0.4);
             border-radius: 16px;
             color: white;
             text-align: center;
             padding: 16px 20px;
             margin-bottom: 20px;",
                       
                       div(class = "stat-number",
                           style = "font-size: 3.8rem;
                font-weight: 900;
                line-height: 1;
                margin-bottom: 6px;",
                           textOutput("fatality_savings_display", inline = TRUE)
                       ),
                       
                       div(class = "stat-label",
                           style = "font-size: 2rem;
                opacity: 0.95;
                margin-bottom: 8px;",
                           "Non-Fatal Scenario Cost - Original Scenario Cost"
                       ),
                       
                       div(class = "stat-ci",
                           style = "font-size: 2rem;
                font-weight: 600;
                opacity: 0.9;",
                           textOutput("fatality_savings_ci", inline = TRUE)
                       )
                   )
            )
          )
        )
      )
    )  
  ),
  
  
  # ---- Discussion Tab ----
  tabPanel(
    "Discussion",
    icon = icon("comments"),
    fluidPage(
      div(class = "content-section",
          h2("Discussion & Implications"),
          
          fluidRow(
            
            # LEFT COLUMN — Key Findings + Conclusion
            column(
              width = 6,
              h3("Key Findings Summary"),
              
              div(class = "workflow-box",
                  h4(icon("exclamation-triangle"), " Critical Cost Drivers"),
                  tags$ul(style = "font-size: 15px;",
                          tags$li("Bad weather (IMC) leads to much higher accident costs"),
                          tags$li("Substantial aircraft damage significantly increases total cost"),
                          tags$li("Fatalities dominate financial losses")
                  )
              ),
              
              div(class = "workflow-box",
                  h4(icon("chart-bar"), " Model Insights"),
                  tags$ul(style = "font-size: 15px;",
                          tags$li("Model fit improves as aircraft damage and fatalities are added"),
                          tags$li("Model 3 (R² ≈ 0.80) captures most cost variation"),
                          tags$li("Engine type and number of engines affect predicted cost levels")
                  )
              ),
              
              h3("Conclusion"),
              div(class = "workflow-box",
                  p(style = "font-size: 15px;",
                    HTML("Overall, the analysis shows that accident severity, especially <strong>substantial damage 
                         and fatalities</strong>, is the main driver of financial loss in aviation accidents. The 
                         scenario analyses reveal that <strong>improving weather-related safety and preventing fatal 
                         outcomes</strong> could yield substantial cost savings. <strong>Multi-engine turbofan and 
                         turboprop aircraft</strong> show higher average predicted costs, suggesting these configurations 
                         warrant closer risk assessment. These findings provide a foundation for insurance companies to refine 
                         pricing models and for the aviation industry to prioritize investments in technologies and training that 
                         reduce accident severity.")
                  )
              )
            ),
            
            # RIGHT COLUMN — Insurance Flow Graphic
            column(
              width = 6,
              h3("Implications for Insurance Companies"),
              div(class = "content-section",
                  div(style = "max-width: 500px; margin: auto;",
                      
                      div(style = "text-align: center; margin-bottom: 40px;",
                          tags$div(icon("cloud-rain", class = "fa-3x"),
                                   style = "color:#667eea; margin-bottom: 15px;"),
                          h4("1. Weather-Based Premiums"),
                          p(style = "font-size: 14px;",
                            "Insurers can adjust premiums or require better weather-avoidance systems for operators that frequently fly in IMC.")
                      ),
                      
                      div(style = "text-align: center; margin: 5px 0;",
                          icon("arrow-down", class = "fa-2x", style = "color:#2c3e50;")
                      ),
                      
                      div(style = "text-align: center; margin-bottom: 40px;",
                          tags$div(icon("search-dollar", class = "fa-3x"),
                                   style = "color:#667eea; margin-bottom: 15px;"),
                          h4("2. Risk-Based Pricing Models"),
                          p(style = "font-size: 14px;",
                            "The model helps insurers identify high-risk aircraft types or operational patterns, enabling more accurate pricing and targeted coverage requirements.")
                      ),
                      
                      div(style = "text-align: center; margin: 5px 0;",
                          icon("arrow-down", class = "fa-2x", style = "color:#2c3e50;")
                      ),
                      
                      div(style = "text-align: center; margin-bottom: 40px;",  # Changed from 10px
                          tags$div(icon("hand-holding-usd", class = "fa-3x"),
                                   style = "color:#667eea; margin-bottom: 15px;"),
                          h4("3. Safety Investment Incentives"),
                          p(style = "font-size: 14px;",
                            "These insights support premium incentives that encourage airlines to invest in weather-safety technology and more robust maintenance programs.")
                      )
                  )
              )
            )
          )
      )
    )
  ), 
  # ---- References Tab ----
  tabPanel(
    "References",
    icon = icon("book"),
    fluidPage(
      div(class = "content-section",
          h2("References"),
          
          div(class = "workflow-box",
              h3("Data Sources"),
              tags$ol(style = "font-size: 16px; line-height: 2;",
                      tags$li(
                        strong("National Transportation Safety Board (NTSB)."), " (2023). ",
                        em("Aviation Accident Database & Synopses."), 
                        " Retrieved from ",
                        tags$a(href = "https://www.ntsb.gov/Pages/AviationQuery.aspx", 
                               target = "_blank",
                               "https://www.ntsb.gov/Pages/AviationQuery.aspx")
                      )
              )
          ),
          
          div(class = "workflow-box",
              h3("Cost Estimation"),
              tags$ol(start = 2, style = "font-size: 16px; line-height: 2;",
                      tags$li(
                        strong("Healy-Pratt, G., & Hanna, K. M."), " (2021). ",
                        em("Compensation Trends in Aviation Accident Litigation."), 
                        " Journal of Aviation Law and Commerce, 86(2), 145-178."
                      )
              )
          ),
          
          div(class = "workflow-box",
              h3("Aviation Safety Literature"),
              tags$ol(start = 3, style = "font-size: 16px; line-height: 2;",
                      tags$li(
                        strong("Nita, I.-A., et al."), " (2024). ",
                        "Aviation accidents related to atmospheric instability in the United States (2000–2020). ",
                        em("Theoretical and Applied Climatology,"), " 155, 5483–5497. ",
                        tags$a(href = "https://doi.org/10.1007/s00704-024-04968-w", 
                               target = "_blank",
                               "https://doi.org/10.1007/s00704-024-04968-w")
                      ),
                      tags$li(
                        strong("Boyd, D. D."), " (2015). ",
                        "Causes and risk factors for fatal accidents in non-commercial twin engine piston general aviation aircraft. ",
                        em("Accident Analysis and Prevention,"), " 77, 113–119. ",
                        tags$a(href = "https://doi.org/10.1016/j.aap.2015.01.021", 
                               target = "_blank",
                               "https://doi.org/10.1016/j.aap.2015.01.021")
                      ),
                      tags$li(
                        strong("Silagyi, D. V., & Liu, D."), " (2023). ",
                        "Prediction of severity of aviation landing accidents using support vector machine models. ",
                        em("Accident Analysis & Prevention,"), " 187, 107043. ",
                        tags$a(href = "https://doi.org/10.1016/j.aap.2023.107043", 
                               target = "_blank",
                               "https://doi.org/10.1016/j.aap.2023.107043")
                      )
              )
          )
          
      )
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    data <- accidents_data()
    req(data)
    
    decades <- sort(unique(floor(data$Year / 10) * 10))
    
    # Remove 1990s if desired
    decades <- decades[decades != 1990]
    
    decade_labels <- paste0(decades, "s")
    
    updateSelectInput(
      session,
      "r2_mode",
      choices = c("Overall R²", decade_labels),
      selected = "Overall R²"
    )
  })
  
  
  
  # Load and process data
  accidents_data <- reactive({
    # Check if file exists
    file_path <- "accidentsData.csv"
    
    if (!file.exists(file_path)) {
      showNotification("CSV file not found! Please ensure the data file is in the same directory.", 
                       type = "error", duration = 10)
      return(NULL)
    }
    
    tryCatch({
      data <- read.csv(file_path)
      
      accidents_clean <- data %>%
        select(
          Aircraft.Category, Aircraft_damage, Number_of_Engines, Engine.Type,
          Air.carrier, Weather_Condition, Broad.phase.of.flight,
          Total_Serious_Injuries, Total_Fatal_Injuries, Total_Minor_Injuries, Total_Uninjured,
          Event.Date, Injury_Severity
        ) %>%
        rename(
          Aircraft_Category = Aircraft.Category,
          Event_Date = Event.Date,
          Engine_Type = Engine.Type
        ) %>%
        mutate(Event_Date_parsed = ymd_hms(Event_Date),
               Year = year(Event_Date_parsed),
               Month = factor(month(Event_Date_parsed))
        ) %>%
        filter(Aircraft_Category == "Airplane") %>%
        filter(Number_of_Engines >= 1) %>%
        mutate(
          Total_Cost = (Total_Fatal_Injuries * cost1) + (Total_Serious_Injuries * cost2),
          Aircraft_damage_grp = na_if(Aircraft_damage, "Unk") %>%
            na_if("") %>% na_if("UNK") %>% na_if("Unknown"),
          Weather_Condition_grp = na_if(Weather_Condition, "Unk") %>%
            na_if("") %>% na_if("UNK"),
          Engine_Type_grp = na_if(Engine_Type, "") %>%
            na_if("UNK") %>% na_if("Unknown"),
          Injury_Severity_grp = case_when(
            str_detect(Injury_Severity, "^Fatal") ~ "Fatal",
            Injury_Severity %in% c("Incident", "Minor", "Non-Fatal", "Serious") ~ Injury_Severity,
            TRUE ~ NA_character_
          )
        ) %>%
        drop_na(Total_Cost, Number_of_Engines, Injury_Severity_grp,
                Aircraft_damage_grp, Weather_Condition_grp, Engine_Type_grp, Month, Year) %>%
        mutate(is_fatal = case_when(
          Injury_Severity_grp == "Fatal" ~ TRUE,
          TRUE ~ FALSE)
        ) %>%
        filter(Total_Cost > 0)
      
      return(accidents_clean)
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), 
                       type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Number of observations
  output$n_obs <- renderText({
    data <- accidents_data()
    if (is.null(data)) return("N/A")
    format(nrow(data), big.mark = ",")
  })
  
  # Fit models
  models <- reactive({
    data <- accidents_data()
    req(data)
    
    m1 <- lm(
      formula = log(Total_Cost + 1) ~ Number_of_Engines +
        factor(Weather_Condition_grp) +
        factor(Engine_Type_grp),
      data = data
    )
    
    m2 <- lm(
      formula = log(Total_Cost + 1) ~ Number_of_Engines +
        factor(Weather_Condition_grp) +
        factor(Engine_Type_grp) +
        factor(Aircraft_damage_grp),
      data = data
    )
    
    m3 <- lm(
      formula = log(Total_Cost + 1) ~ Number_of_Engines +
        factor(Weather_Condition_grp) +
        factor(Engine_Type_grp) +
        factor(Aircraft_damage_grp) +
        is_fatal,
      data = data
    )
    
    list(m1 = m1, m2 = m2, m3 = m3)
  })
  
  # Data preview table
  output$data_preview <- renderTable({
    data <- accidents_data()
    req(data)
    
    head(data %>% select(Total_Fatal_Injuries, Total_Serious_Injuries, 
                         Weather_Condition_grp, Aircraft_damage_grp, 
                         Number_of_Engines, Engine_Type_grp), 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Variables tables
  
  # Key variables
  output$variables_table <- renderTable({
    data.frame(
      Variable = c("Number_of_Engines", "Aircraft_Damage", "Weather_Condition", 
                   "Engine_Type", "Injury_Severity"),
      Description = c(
        "Number of engines installed on the aircraft",
        "Level of structural damage the accident caused",
        "Visibility conditions due to weather at time of accident",
        "Type of engine installed in aircraft",
        "Level of human injury caused by the accident"
      ), 
      Example_Values = c(
        "1, 2, 3, 4", 
        "Minor, Substantial, Destroyed", 
        "VMC (Visual), IMC (Instrument)", 
        "Reciprocating, Turbofan, Turbojet, Turboprop",
        "Fatal, minor, non-fatal, serious"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  #Derived variables
  output$derivedvariables_table <- renderTable({
    data.frame(
      Variable = c("is_fatal", "cost"),
      Description = c(
        "Indicates whether the accident resulted in one or more fatalities",
        "Accident cost liability based on fatalities and serious injuries"
      ), 
      Example_Values = c(
        "TRUE, FALSE",
        "$1,450,000"
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # Model summary
  output$model_summary <- renderPrint({
    m <- models()
    req(m)
    summary(m$m3)
  })
  
  # Workflow diagram
  output$workflow_diagram <- renderPlot({
    # Create boxes data
    boxes <- data.frame(
      id = 1:11,
      x = c(1.5, 1.5, 1.5, 1.5, 4, 4, 4, 4, 6.5, 6.5, 6.5),
      y = c(7, 6, 5, 4, 7, 6, 5, 4, 6.5, 5, 3.5),
      width = c(1.3, 1.3, 1.3, 1.3, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5),
      height = c(0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6),
      label = c(
        "Data Collection",
        "Data Cleaning &\nFiltering",
        "Data\nTransformation",
        "Data\nExploration",
        "Model 1:\nBaseline",
        "Model 2:\nAircraft Damage",
        "Model 3:\nFatal Indicator",
        "Model Selection\n(Best R²)",
        "VMC Scenario\nSimulation",
        "Cost Predictions\n& Confidence Intervals",
        "Results"
      ),
      fill_color = c(
        "#B8E6D5", "#B8E6D5", "#B8E6D5", "#B8E6D5",  # Light green for data steps
        "#FFF9C4", "#FFF9C4", "#FFF9C4", "#FFE5CC",   # Yellow for models, orange for selection
        "#E6F3E6", "#E6F3E6",                         # Light green for analysis
        "#E8D4F2"                                      # Light purple for results
      ),
      stringsAsFactors = FALSE
    )
    
    # Create connections (arrows)
    connections <- data.frame(
      x =    c(1.5,  1.5,  1.5,  2.15, 2.15, 2.15, 4.75, 6.5,  6.5),
      y =    c(6.7,  5.7,  4.7,  4,    4,    4,    4,    6.2,  4.7),
      xend = c(1.5,  1.5,  1.5,  3.25, 3.25, 3.25, 5.75, 6.5,  6.5),
      yend = c(6.3,  5.3,  4.3,  7,    6,    5,    6.5,  5.3,  3.8),
      stringsAsFactors = FALSE
    )
    
    # Plot
    ggplot() +
      # Draw rectangles
      geom_rect(data = boxes,
                aes(xmin = x - width/2, xmax = x + width/2,
                    ymin = y - height/2, ymax = y + height/2,
                    fill = I(fill_color)),
                color = "#666666", size = 1.3) +
      # Add labels
      geom_text(data = boxes,
                aes(x = x, y = y, label = label),
                size = 3.8, fontface = "bold", lineheight = 0.85) +
      # Draw arrows
      geom_segment(data = connections,
                   aes(x = x, y = y, xend = xend, yend = yend),
                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                   size = 1.1, color = "#555555") +
      # Add step numbers
      annotate("text", x = 0.7, y = 7, label = "①", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 0.7, y = 6, label = "②", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 0.7, y = 5, label = "③", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 0.7, y = 4, label = "④", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 3.1, y = 7, label = "⑤", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 3.1, y = 6, label = "⑥", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 3.1, y = 5, label = "⑦", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 3.1, y = 4, label = "⑧", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 5.6, y = 6.5, label = "⑨", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 5.6, y = 5, label = "⑩", size = 6, fontface = "bold", color = "#2c3e50") +
      annotate("text", x = 5.6, y = 3.5, label = "⑪", size = 6, fontface = "bold", color = "#2c3e50") +
      # Add section labels
      annotate("rect", xmin = 0.8, xmax = 2.2, ymin = 3.5, ymax = 7.4, 
               fill = NA, color = "#27ae60", size = 1.3, linetype = "dashed") +
      annotate("text", x = 1.5, y = 7.55, label = "Data Preparation", 
               size = 4.5, fontface = "bold", color = "#27ae60") +
      annotate("rect", xmin = 3.2, xmax = 4.8, ymin = 3.5, ymax = 7.4, 
               fill = NA, color = "#f39c12", size = 1.3, linetype = "dashed") +
      annotate("text", x = 4, y = 7.55, label = "Model Development", 
               size = 4.5, fontface = "bold", color = "#f39c12") +
      annotate("rect", xmin = 5.7, xmax = 7.3, ymin = 3.2, ymax = 6.9, 
               fill = NA, color = "#9b59b6", size = 1.3, linetype = "dashed") +
      annotate("text", x = 6.5, y = 7.05, label = "Analysis & Results", 
               size = 4.5, fontface = "bold", color = "#9b59b6") +
      # Styling
      scale_x_continuous(limits = c(0.3, 7.7)) +
      scale_y_continuous(limits = c(2.9, 8)) +
      labs(title = "Aviation Accident Cost Analysis") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold", 
                                  color = "#2c3e50", margin = margin(b = 5)),
        plot.subtitle = element_text(hjust = 0.5, size = 13, color = "#7f8c8d",
                                     margin = margin(b = 25)),
        plot.background = element_rect(fill = "#f8f9fa", color = NA),
        plot.margin = margin(25, 25, 25, 25)
      )
  })
  
  output$coeff_plot <- renderPlot({
    m <- models()
    req(m)
    
    # Extract coefficients from Model 3
    coef_data <- broom::tidy(m$m3, conf.int = TRUE) %>%
      filter(term != "(Intercept)") %>%
      mutate(term_label = case_when(
        term == "Number_of_Engines" ~ "Number of Engines",
        term == "factor(Weather_Condition_grp)VMC" ~ "VMC Weather",
        term == "factor(Engine_Type_grp)Turbo Fan" ~ "Turbo Fan Engine",
        term == "factor(Engine_Type_grp)Turbo Jet" ~ "Turbo Jet Engine",
        term == "factor(Engine_Type_grp)Turbo Prop" ~ "Turbo Prop Engine",
        term == "factor(Engine_Type_grp)Turbo Shaft" ~ "Turbo Shaft Engine",
        term == "factor(Aircraft_damage_grp)Minor" ~ "Minor Damage",
        term == "factor(Aircraft_damage_grp)Substantial" ~ "Substantial Damage",
        term == "is_fatalTRUE" ~ "Fatal Accident",
        TRUE ~ term
      ))
    
    ggplot(coef_data, aes(x = reorder(term_label, estimate), y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 1) +
      geom_point(size = 4, color = "#1e40af") +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    width = 0.3, size = 1.2, color = "#1e40af", alpha = 0.7) +
      coord_flip() +
      labs(title = "Model Coefficients with 95% Confidence Intervals",
           subtitle = "Effect on log(Total Cost) - Negative coefficients reduce costs",
           x = NULL, 
           y = "Coefficient Estimate") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#1e293b"),
        plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#64748b", margin = margin(b = 15)),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  #Model Comparison Table
  output$model_comparison_table <- renderPrint({
    m <- models()
    req(m)
    
    tryCatch({
      texreg::screenreg(
        list(m$m1, m$m2, m$m3),
        custom.model.names = c("Model 1:\nBaseline", "Model 2:\n+ Damage", "Model 3:\n+ Fatal"),
        stars = c(0.001, 0.01, 0.05),
        custom.note = "*** p < 0.001; ** p < 0.01; * p < 0.05"
      )
    }, error = function(e) {
      cat("Model comparison table unavailable for this subset of data.\n")
      cat("Error:", e$message)
    })
  })
  
  # R-squared plot
  output$r2_plot <- renderPlot({
    
    data <- accidents_data()
    req(data)
    
    data <- accidents_data()
    req(data)
    
    m <- models()
    req(m)
    
    # --- Original Fancy Labels ---
    model_labels <- c(
      "Model 1:\n# Engines + Weather\n+ Engine Type",
      "Model 2:\n+ Aircraft Damage",
      "Model 3:\n+ Fatal Indicator"
    )
    
    
    # ======================================
    # CASE 1 — OVERALL R²
    # ======================================
    if (input$r2_mode == "Overall R²") {
      
      r2_data <- data.frame(
        Model = factor(model_labels, levels = model_labels),
        R2 = c(
          summary(m$m1)$r.squared,
          summary(m$m2)$r.squared,
          summary(m$m3)$r.squared
        )
      )
      return(
        ggplot(r2_data, aes(x = Model, y = R2, fill = Model)) +
          geom_col(width = 0.7) +
          geom_text(aes(label = paste0("R² = ", round(R2, 3))),
                    vjust = -0.6, size = 5, fontface = "bold") +
          scale_fill_manual(values = c("#3498db", "#9b59b6", "#2ecc71")) +
          scale_y_continuous(limits = c(0, 1)) +
          labs(title = "Model Fit Comparison: Progressive Improvement in R²", 
               subtitle = "                                                                                                     Higher R² indicates better model fit and explanatory power", 
               y = "R-Squared", x = NULL) +
          theme_minimal(base_size = 18) +
          theme(
            axis.text.x = element_text(face = "bold", lineheight = 1.2),
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold")
          )
      )
    }
    
    
    # ======================================
    # CASE 2 — A SPECIFIC DECADE
    # ======================================
    
    # Extract selected decade
    selected_decade <- as.numeric(gsub("s", "", input$r2_mode))
    
    ddata <- data %>% filter(floor(Year / 10) * 10 == selected_decade)
    
    if (nrow(ddata) < 30) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Insufficient data in", input$r2_mode),
                   size = 8) +
          theme_void()
      )
    }
    
    # Initialize R2 values
    r2_m1 <- NA
    r2_m2 <- NA
    r2_m3 <- NA
    
    # Try Model 1
    try({
      m1_d <- lm(log(Total_Cost + 1) ~ Number_of_Engines + factor(Weather_Condition_grp) + factor(Engine_Type_grp), data = ddata)
      r2_m1 <- summary(m1_d)$r.squared
    }, silent = TRUE)
    
    # Try Model 2
    try({
      m2_d <- lm(log(Total_Cost + 1) ~ Number_of_Engines + factor(Weather_Condition_grp) + factor(Engine_Type_grp) + factor(Aircraft_damage_grp), data = ddata)
      r2_m2 <- summary(m2_d)$r.squared
    }, silent = TRUE)
    
    # Try Model 3
    try({
      m3_d <- lm(log(Total_Cost + 1) ~ Number_of_Engines + factor(Weather_Condition_grp) + factor(Engine_Type_grp) + factor(Aircraft_damage_grp) + is_fatal, data = ddata)
      r2_m3 <- summary(m3_d)$r.squared
    }, silent = TRUE)
    
    r2_decade <- data.frame(
      Model = factor(model_labels, levels = model_labels),
      R2 = c(r2_m1, r2_m2, r2_m3)
    )
    
    # If all failed, show message
    if (all(is.na(r2_decade$R2))) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, 
                   label = paste("Cannot fit models for", input$r2_mode),
                   size = 8) +
          theme_void()
      )
    }
    
    # Plot with NAs handled
    ggplot(r2_decade %>% filter(!is.na(R2)), aes(x = Model, y = R2, fill = Model)) +
      geom_col(width = 0.7) +
      geom_text(aes(label = paste0("R² = ", round(R2, 3))),
                vjust = -0.6, size = 5, fontface = "bold") +
      scale_fill_manual(values = c(
        "Model 1:\nEngines + Weather\n+ Engine Type" = "#3498db",
        "Model 2:\n+ Aircraft Damage" = "#9b59b6",
        "Model 3:\n+ Fatal Indicator" = "#2ecc71"
      )) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(
        title = paste("Model Fit in", input$r2_mode),
        x = NULL,
        y = "R-Squared"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(face = "bold", lineheight = 1.2),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
  })
  
  # Heatmap plot
  output$heatmap_plot <- renderPlot({
    data <- accidents_data()
    req(data)
    # Filter by decade if one is selected
    if (input$r2_mode != "Overall R²") {
      selected_decade <- as.numeric(gsub("s", "", input$r2_mode))
      data <- data %>% filter(floor(Year / 10) * 10 == selected_decade)
      
      if (nrow(data) < 30) {
        return(
          ggplot() +
            annotate("text", x = 0.5, y = 0.5, 
                     label = paste("Insufficient data in", input$r2_mode, "\nfor heatmap"),
                     size = 6) +
            theme_void()
        )
      }
    }
    model <- models()$m3
    req(model)
    
    mygrid <- expand_grid(
      Number_of_Engines = 1:4,
      Aircraft_damage_grp = unique(data$Aircraft_damage_grp),
      Weather_Condition_grp = unique(data$Weather_Condition_grp),
      Engine_Type_grp = unique(data$Engine_Type_grp),
      is_fatal = unique(data$is_fatal)
    )
    
    mypred <- mygrid %>%
      mutate(yhat = exp(predict(model, newdata = .)) - 1)
    
    filtered_avg <- mypred %>%
      filter(yhat < 5e6) %>%
      group_by(Number_of_Engines, Engine_Type_grp) %>%
      summarise(avg_yhat = mean(yhat), .groups = 'drop')
    
    ggplot(filtered_avg, 
           aes(x = factor(Number_of_Engines), 
               y = Engine_Type_grp, 
               fill = avg_yhat)) +
      geom_tile(color = "white", linewidth = 2) +
      geom_text(
        aes(label = scales::dollar(round(avg_yhat / 1e6, 1), suffix = "M"),
            color = avg_yhat > 1.2e6),
        fontface = "bold", 
        size = 7,
        show.legend = FALSE
      ) +
      scale_color_manual(values = c("white", "black")) +
      scale_fill_viridis_c(
        option = "plasma",
        name = "Average\nPredicted\nCost",
        labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
        limits = c(0, 5e6)
      ) +
      labs(
        title = "Predicted Accident Cost by Engine Configuration",
        subtitle = "Higher costs associated with certain engine types and configurations",
        x = "Number of Engines",
        y = "Engine Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5, 
                                  color = "#2c3e50", margin = margin(b = 5)),
        plot.subtitle = element_text(hjust = 0.5, size = 13, color = "#7f8c8d",
                                     margin = margin(b = 20)),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        panel.grid = element_blank(),
        plot.margin = margin(20, 30, 20, 20)
      )
  })
  
  # Financial analysis
  financial_results <- reactive({
    data <- accidents_data()
    req(data)
    model <- models()$m3
    req(model)
    
    scenario <- data %>%
      mutate(Weather_Condition_grp = "VMC")
    
    pred_current <- predict(model, newdata = data, se.fit = TRUE)
    pred_scenario <- predict(model, newdata = scenario, se.fit = TRUE)
    
    current_cost <- exp(pred_current$fit) - 1
    scenario_cost <- exp(pred_scenario$fit) - 1
    
    average_current_cost <- mean(current_cost)
    average_scenario_cost <- mean(scenario_cost)
    average_savings <- average_current_cost - average_scenario_cost
    
    # Calculate confidence intervals
    set.seed(123)
    ci_current <- pred_current %>% as_tibble() %>%
      rowwise() %>%
      mutate(
        yhat = fit %>% exp() %>% {. - 1},
        se = rnorm(n = 1000, mean = fit, sd = se.fit) %>% exp() %>% {. - 1} %>% sd()
      ) %>%
      ungroup() %>%
      mutate(lower = yhat - se * qnorm(0.975), upper = yhat + se * qnorm(0.975))
    
    ci_scenario <- pred_scenario %>% as_tibble() %>%
      rowwise() %>%
      mutate(
        yhat = fit %>% exp() %>% {. - 1},
        se = rnorm(n = 1000, mean = fit, sd = se.fit) %>% exp() %>% {. - 1} %>% sd()
      ) %>%
      ungroup() %>%
      mutate(lower = yhat - se * qnorm(0.975), upper = yhat + se * qnorm(0.975))
    
    savings_per_accident <- ci_current$yhat - ci_scenario$yhat
    se_per_accident <- sqrt(ci_current$se^2 + ci_scenario$se^2)
    
    avg_savings <- mean(savings_per_accident)
    se_avg_savings <- sqrt(sum(se_per_accident^2)) / nrow(ci_current)
    
    ci_lower <- avg_savings - se_avg_savings * qnorm(0.975)
    ci_upper <- avg_savings + se_avg_savings * qnorm(0.975)
    
    list(
      current = average_current_cost,
      scenario = average_scenario_cost,
      savings = avg_savings,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      n_accidents = nrow(data)
    )
  })
  
  output$current_cost_display <- renderText({
    results <- financial_results()
    req(results)
    paste0("$", format(round(results$current / 1e6, 3), nsmall = 1), "M")
  })
  
  output$vmc_cost_display <- renderText({
    results <- financial_results()
    req(results)
    paste0("$", format(round(results$scenario / 1e6, 3), big.mark = ","), "M")
  })
  
  output$savings_display <- renderText({
    results <- financial_results()
    req(results)
    paste0("Average Savings Per Accident: $", format(round(results$savings / 1000, 0), big.mark = ","), "K")
  })
  
  output$savings_ci <- renderText({
    results <- financial_results()
    req(results)
    
    lower <- round(results$ci_lower / 1000, 0)
    upper <- round(results$ci_upper / 1000, 0)
    
    paste0("95% Confidence Interval: $", format(lower, big.mark = ","), "K – $", 
           format(upper, big.mark = ","), "K")
  })
  
  output$finance_plot <- renderTable({
    results <- financial_results()
    req(results)
    
    savings <- results$current - results$scenario
    pct_reduction <- (savings / results$current) * 100
    
    # Create table data
    table_data <- data.frame(
      Scenario = c("Current Conditions", "All Flights Under VMC", "Potential Savings"),
      `Predicted Average Cost` = c(
        paste0("$", format(round(results$current, 0), big.mark = ",")),
        paste0("$", format(round(results$scenario, 0), big.mark = ",")),
        paste0("$", format(round(savings, 0), big.mark = ","),
               " (", round(pct_reduction, 1), "% reduction)")
      ),
      `95% Confidence Interval` = c(
        "—",
        "—",
        paste0("($", format(round(results$ci_lower, 0), big.mark = ","),
               " to $", format(round(results$ci_upper, 0), big.mark = ","), ")")
      ),
      check.names = FALSE
    )
    
    return(table_data)
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  width = "100%",
  align = 'lcc'  # ← now correctly inside renderTable()
  )
  
  # Populate year choices
  observe({
    data <- accidents_data()
    req(data)
    
    years <- sort(unique(data$Year))
    years_filtered <- years[years >= 2008 & years <= 2020]
    updateSelectInput(session, "year_filter",
                      choices = years_filtered,
                      selected = max(years_filtered))
  })
  
  # Annual predictions by year
  annual_predictions <- reactive({
    data <- accidents_data()
    req(data)
    model <- models()$m3
    req(model)
    
    # Current scenario
    pred_current <- data %>%
      mutate(predicted_cost = exp(predict(model, newdata = .)) - 1)
    
    # VMC scenario
    scenario_data <- data %>%
      mutate(Weather_Condition_grp = "VMC")
    
    pred_vmc <- scenario_data %>%
      mutate(predicted_cost = exp(predict(model, newdata = .)) - 1)
    
    # Combine and summarize by year
    current_by_year <- pred_current %>%
      group_by(Year) %>%
      summarise(
        total_cost = sum(predicted_cost),
        n_accidents = n(),
        .groups = 'drop'
      ) %>%
      mutate(Scenario = "Current Conditions")
    
    vmc_by_year <- pred_vmc %>%
      group_by(Year) %>%
      summarise(
        total_cost = sum(predicted_cost),
        n_accidents = n(),
        .groups = 'drop'
      ) %>%
      mutate(Scenario = "All VMC Scenario")
    
    # Combine both scenarios
    bind_rows(current_by_year, vmc_by_year)
  })
  
  output$annual_cost_plot <- renderPlot({
    annual_data <- annual_predictions()
    req(annual_data)
    req(input$year_filter)
    
    # Filter by selected year
    annual_data_filtered <- annual_data %>%
      filter(Year == as.numeric(input$year_filter))
    
    # Calculate savings
    savings_data <- annual_data_filtered %>%
      select(Year, Scenario, total_cost) %>%
      pivot_wider(names_from = Scenario, values_from = total_cost) %>%
      mutate(
        Savings = `Current Conditions` - `All VMC Scenario`,
        pct_reduction = (Savings / `Current Conditions`) * 100
      )
    
    # Create data
    plot_data <- data.frame(
      Scenario = c("Current Conditions", "All-VMC Scenario", "Potential Savings"),
      Cost = c(savings_data$`Current Conditions`, 
               savings_data$`All VMC Scenario`,
               savings_data$Savings),
      Type = c("Current", "VMC", "Savings")
    )
    
    plot_data$Scenario <- factor(plot_data$Scenario, 
                                 levels = rev(c("Current Conditions", "All-VMC Scenario", "Potential Savings")))
    
    ggplot(plot_data, aes(x = Cost, y = Scenario, fill = Type)) +
      geom_col(width = 0.7, alpha = 0.95) +
      # Regular labels for Current and VMC
      geom_text(data = subset(plot_data, Type %in% c("Current", "VMC")),
                aes(label = paste0("$", format(round(Cost / 1e6, 1), big.mark = ","), "M")),
                hjust = -0.1, size = 6, fontface = "bold") +
      # Big boxed label for Savings
      geom_label(data = subset(plot_data, Type == "Savings"),
                 aes(label = paste0("$", format(round(Cost / 1e6, 1), big.mark = ","), "M\nPotential Savings")),
                 hjust = 0, nudge_x = savings_data$Savings * 0.05,
                 size = 7, fontface = "bold", 
                 fill = "#d5f4e6", color = "#27ae60",
                 label.padding = unit(0.5, "lines"),
                 label.size = 1.5, lineheight = 0.9) +
      scale_fill_manual(values = c("Current" = "#e74c3c", 
                                   "VMC" = "#3498db",
                                   "Savings" = "#2ecc71"),
                        guide = "none") +
      scale_x_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
                         expand = expansion(mult = c(0, 0.2))) +
      labs(
        title = paste0("Annual Cost Analysis: VMC Weather Impact (", input$year_filter, ")"),
        subtitle = paste0("Potential savings of ", round(savings_data$pct_reduction, 1), 
                          "% by improving weather-related safety measures"),
        x = "Annual Cost",
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5,
                                  color = "#2c3e50", margin = margin(b = 5)),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#7f8c8d",
                                     margin = margin(b = 20)),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )
  })
  
  output$annual_summary_table <- renderTable({
    annual_data <- annual_predictions()
    req(annual_data)
    req(input$year_filter)
    
    # Filter by selected year
    annual_data <- annual_data %>%
      filter(Year == as.numeric(input$year_filter))
    
    # Create summary
    summary_data <- annual_data %>%
      select(Year, Scenario, total_cost, n_accidents) %>%
      pivot_wider(names_from = Scenario,
                  values_from = c(total_cost, n_accidents)) %>%
      mutate(
        Savings = `total_cost_Current Conditions` - `total_cost_All VMC Scenario`,
        pct_reduction = (Savings / `total_cost_Current Conditions`) * 100
      ) %>%
      transmute(
        Year = as.integer(Year),
        Accidents = `n_accidents_Current Conditions`,
        `Current Total Cost` = paste0("$", format(round(`total_cost_Current Conditions`, 0), big.mark = ",")),
        `VMC Scenario Cost` = paste0("$", format(round(`total_cost_All VMC Scenario`, 0), big.mark = ",")),
        `Annual Savings` = paste0("$", format(round(Savings, 0), big.mark = ","),
                                  " (", round(pct_reduction, 1), "%)")
      )
    
    return(summary_data)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # Fatality prevention financial analysis
  fatality_financial_results <- reactive({
    data <- accidents_data()
    req(data)
    model <- models()$m3
    req(model)
    
    # Scenario: all accidents are non-fatal
    scenario_nonfatal <- data %>%
      mutate(is_fatal = FALSE)
    
    pred_current <- predict(model, newdata = data, se.fit = TRUE)
    pred_nonfatal <- predict(model, newdata = scenario_nonfatal, se.fit = TRUE)
    
    current_cost <- exp(pred_current$fit) - 1
    nonfatal_cost <- exp(pred_nonfatal$fit) - 1
    
    average_current_cost <- mean(current_cost)
    average_nonfatal_cost <- mean(nonfatal_cost)
    average_savings <- average_current_cost - average_nonfatal_cost
    
    # Calculate confidence intervals
    set.seed(123)
    ci_current <- pred_current %>% as_tibble() %>%
      rowwise() %>%
      mutate(
        yhat = fit %>% exp() %>% {. - 1},
        se = rnorm(n = 1000, mean = fit, sd = se.fit) %>% exp() %>% {. - 1} %>% sd()
      ) %>%
      ungroup() %>%
      mutate(lower = yhat - se * qnorm(0.975), upper = yhat + se * qnorm(0.975))
    
    ci_nonfatal <- pred_nonfatal %>% as_tibble() %>%
      rowwise() %>%
      mutate(
        yhat = fit %>% exp() %>% {. - 1},
        se = rnorm(n = 1000, mean = fit, sd = se.fit) %>% exp() %>% {. - 1} %>% sd()
      ) %>%
      ungroup() %>%
      mutate(lower = yhat - se * qnorm(0.975), upper = yhat + se * qnorm(0.975))
    
    savings_per_accident <- ci_current$yhat - ci_nonfatal$yhat
    se_per_accident <- sqrt(ci_current$se^2 + ci_nonfatal$se^2)
    
    avg_savings <- mean(savings_per_accident)
    se_avg_savings <- sqrt(sum(se_per_accident^2)) / nrow(ci_current)
    
    ci_lower <- avg_savings - se_avg_savings * qnorm(0.975)
    ci_upper <- avg_savings + se_avg_savings * qnorm(0.975)
    
    list(
      current = average_current_cost,
      nonfatal = average_nonfatal_cost,
      savings = avg_savings,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      n_accidents = nrow(data)
    )
  })
  
  output$fatality_savings_display <- renderText({
    results <- fatality_financial_results()
    req(results)
    paste0("Average Savings Per Accident: $", format(round(results$savings / 1e6, 3), nsmall = 3), "M")
  })
  
  output$fatality_savings_ci <- renderText({
    results <- fatality_financial_results()
    req(results)
    
    lower <- round(results$ci_lower / 1e6, 3)  # Display in millions with 3 decimals
    upper <- round(results$ci_upper / 1e6, 3)  # Display in millions with 3 decimals
    
    paste0("95% Confidence Interval: $", format(lower, nsmall = 3), "M – $", 
           format(upper, nsmall = 3), "M")
  })
  
  output$fatality_finance_plot <- renderTable({
    results <- fatality_financial_results()
    req(results)
    
    savings <- results$current - results$nonfatal
    pct_reduction <- (savings / results$current) * 100
    
    # Create table data
    table_data <- data.frame(
      Scenario = c("Current Conditions", "All Non-Fatal Outcomes", "Potential Savings"),
      `Predicted Average Cost` = c(
        paste0("$", format(round(results$current, 0), big.mark = ",")),
        paste0("$", format(round(results$nonfatal, 0), big.mark = ",")),
        paste0("$", format(round(savings, 0), big.mark = ","),
               " (", round(pct_reduction, 1), "% reduction)")
      ),
      `95% Confidence Interval` = c(
        "—",
        "—",
        paste0("($", format(round(results$ci_lower, 0), big.mark = ","),
               " to $", format(round(results$ci_upper, 0), big.mark = ","), ")")
      ),
      check.names = FALSE
    )
    
    return(table_data)
  },
  striped = TRUE,
  hover = TRUE,
  bordered = TRUE,
  width = "100%",
  align = 'lcc'
  )
}

shinyApp(ui, server)