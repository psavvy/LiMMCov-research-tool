################################################################################
# Title: LiMMCov
# Author: Perseverence Savieri
# Date: Sys.Date()
# Version: 001
################################################################################

options(shiny.maxRequestSize = 10000 * 1024^2)

#############################
# Load libraries and sources
#############################
source("utils.R")
source("global.R")
library(shiny)
library(lme4)
library(nlme)
library(ggplot2)
library(plotly)
library(DT)
library(formatR)
library(knitr)
library(haven)
library(readxl)
library(readr)
library(openxlsx)
library(dplyr)
library(tidyr)
library(tidyverse)
library(car)
library(MASS)
library(MuMIn)
library(astsa)
library(lindia)
library(summarytools)
library(shinymeta)
library(shinyBS)
library(shinythemes)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(kableExtra)
library(shinyFeedback)
library(shinycssloaders)
library(shinyAce)
library(bslib)
library(tinytex)
library(webshot2)
library(pbkrtest)

#####################################
# Define UI
#####################################
ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  tags$head(
    # Google Analytics tracking
    HTML(
      "<!-- Google tag (gtag.js) -->
        <script async src='https://www.googletagmanager.com/gtag/js?id=G-FEB7W5B2B0'></script>
        <script>
        window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      
      gtag('config', 'G-FEB7W5B2B0');
      </script>"
    )
  ),

  # Open navbarPage for main headings
  navbarPage(
    id = "navbar",
    # Application title
    title = "LiMMCov",
    # tags$p(strong("LiMMCov"), style = "color:#003399"),
    theme = shinytheme("flatly"),

    # Main tabs for the web application
    ###################################
    # Home Tab
    ###################################
    tabPanel(
      tags$p(strong("Home"), style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          # Welcome page sidebar content
          width = 4,
          img(src = "logo_bisi_rgb.jpg", width = 420, height = 100),
          br(),
        ),
        mainPanel(
          # Welcome page main content
          width = 8,
          tags$h4("Welcome to the Linear Mixed Model research tool", style = "color:#003399"),
          tags$p("This tool guides you through the process of selecting residual covariance structures in linear mixed models. It uses time series insights for complex covariance structure. The LiMMCov tool offers visualisations to assess the residuals of fixed effects models and offer suggestions for the correct structure based on the data. The application is based on the Shiny package and the user-friendly interface enables the user to navigate the workflow. A report can be downloaded with the results."),
          tags$p("This app was developed by the", tags$a(href = "https://square.research.vub.be/", "Support for Quantitative and Qualitative Research (SQUARE)"), "to provide the research community with complimentary support in statistics. The developers are part of the", tags$a(href = "https://bisi.research.vub.be/", "Biostatistics and Medical Informatics research group (BISI)"), "at the Vrije Universiteit Brussel (VUB)."),
          br(),
          tags$h4("Terms of use", style = "color:#003399"),
          tags$p("LiMMCov is not designed to be exhaustive and is thus appropriate for the use cases described in the app."),
          tags$p("Uploaded data and outputs from analyses won't be kept on our servers. Therefore, you must refrain from uploading any sensitive information because the research tool is supplied WITHOUT ANY WARRANTY. Instead, you can download the reports and any modified data. If you submit any data to this application, you are solely responsible for its confidentiality, availability, security, loss, abuse, and misappropriation."),
          br(),
          tags$h4("Feedback", style = "color:#003399"),
          tags$p("We would love to hear your thoughts, suggestions, concerns or problems you encountered while using LiMMCov so that we can improve. To do this, kindly evaluate the web-application via this", tags$a(href = "https://vub.fra1.qualtrics.com/jfe/form/SV_8hPgQVMvJ4FF1MW", "link."), "For other questions and comments please email ", tags$a(href = "mailto:perseverence.savieri@vub.be", "Perseverence.Savieri@vub.be")),
          br(),
          tags$h4("Contribute your dataset", style = "color:#003399"),
          tags$p(
            "Have a real-world dataset you'd like to share with the LiMMCov community? ",
            "We welcome user-submitted datasets that are publicly available (e.g., openly licensed), ", "as they help us improve LiMMCov by covering a wider range of study designs and boundary cases. ", "If you are interested in contributing your dataset, please contact the development team via email so we can discuss adding it to our online repository."
          ),
          br(),
          tags$h4("Citation", style = "color:#003399"),
          tags$p(
            "If you use LiMMCov for your research, teaching, or presentations, please cite the following publication:"
          ),
          tags$blockquote(
            style = "background:#f6f8fa; border-left:4px solid #FF6600; padding:12px 20px; margin:10px 0 20px 0; font-size:1.05em;",
            HTML(
              "Savieri, P., Stas, L., &amp; Barbé, K. (2025). <i>LiMMCov: An interactive research tool for efficiently selecting covariance structures in linear mixed models using insights from time series analysis</i>. <i>PLoS One, 20</i>(6), e0325834.<br>
              <a href='https://doi.org/10.1371/journal.pone.0325834' target='_blank'>https://doi.org/10.1371/journal.pone.0325834</a>"
            )
          ),
          tags$details(
            tags$summary("Show BibTeX citation"),
            tags$pre(
              "@article{savieri2025limmcov,
  title={LiMMCov: An interactive research tool for efficiently selecting covariance structures in linear mixed models using insights from time series analysis},
  author={Savieri, Perseverence and Stas, Lara and Barb{\\'e}, Kurt},
  journal={PLoS One},
  volume={20},
  number={6},
  pages={e0325834},
  year={2025},
  doi={https://doi.org/10.1371/journal.pone.0325834},
  publisher={Public Library of Science San Francisco, CA USA}
}"
            )
          ),
          br(), br(),
          tags$h4(strong("Click on the Dataset tab to get started."), style = "color:#FF6600"),
          br(), br(),
          br(), br(),

        )
      )
    ),

    ###################################
    # Analysis tab
    ###################################
    navbarMenu(
      title = tags$p(strong("Analysis"), style = "color:#FF6600; display: inline-block;"),

      ###################################
      # Data Tab
      ###################################
      tabPanel(
        tags$p("Dataset", style = "color:#FF6600"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # Example Data
            tags$p(strong("In this tab, you can upload your dataset or use example datasets provided in the app. You can also view and manage your data before proceeding to analysis in the GLM tab.
          Note: LiMMCov assumes data are provided in long format, where repeated measures are stacked by subject ID and time variable.", style = "color:#003399")),
            tags$hr(),
            selectizeInput(
              inputId = "exdata", label = strong("Choose example data", style = "color:#003399"), selected = "",
              choices = c("", "AR(1) Example", "AR(2) Example", "Comp.Symmetry Example", "COVID data"),
              options = list(placeholder = "Choose example data")
            ),
            tags$hr(),
            # Upload own data
            tags$h5(em("OR load your own data file", style = "color:#FF6600")),
            tags$br(),
            # Missing Data Warning
            tags$p(
              strong("Important:", style = "color:#CC0000"), " This application does not support missing data.",
              "Please ensure that your dataset is complete before uploading. If your data contains missing values, consider handling them appropriately (for example through multiple imputation) before importing.",
              "Some R packages that can be used for multiple imputation include ",
              tags$code("mice"), ", ", tags$code("Amelia"), ", and ", tags$code("missForest"), "."
            ),
            tags$br(),
            radioButtons("ext",
              label = strong("Select file extension", style = "color:#003399"),
              choices = list(
                "Text file (.txt)" = "txt",
                "CSV file (.csv)" = "csv",
                "Excel file (.xlsx)" = "xlsx",
                "SPSS file (.sav)" = "sav",
                "Stata Dataset (.dta)" = "dta"
              ),
              selected = ""
            ),
            conditionalPanel(
              condition = "input.ext == 'txt' | input.ext == 'csv' | input.ext == 'xlsx' | input.ext == 'sav' | input.ext == 'dta'",
              tryCatch(
                fileInput("file1", strong("Select file"),
                  accept = c(
                    ".txt", ".csv", ".xlsx", ".sav", ".dta",
                    ".CSV", ".TXT", ".XLSX", ".SAV", ".DTA"
                  )
                )
              )
            ),
            tags$hr(),
            # Data management
            uiOutput("edit_vars"),
            uiOutput("data_management"),
            uiOutput("manage_data_options"),
            uiOutput("var_name_extension"),
            uiOutput("changes_button"),
            tags$hr(),
            tags$p(
              "Use the ", strong("View Data"), " tab to preview your dataset and verify the data has been loaded correctly. 
  Navigate to the ", strong("Data Summary"), " tab for descriptive statistics and variable distributions."
            )
          ),
          mainPanel(
            width = 9,
            # Data loading/uploading main content
            tabsetPanel(
              id = "datatabs",
              # Data preview
              tabPanel(
                tags$p("View Data", style = "color:#003399"),
                value = "datatab1",
                br(), br(),
                dataTableOutput("data_preview"),
                br(), br(),
              ),
              # Data summary
              tabPanel(
                tags$p("Data Summary", style = "color:#003399"),
                value = "datatab2",
                # br(), br(),
                # h5(strong("Data summary")),
                htmlOutput("data_summ"),
                br(), br(),
              )
            )
          )
        )
      ),
      ###################################
      # Fixed Effects Model Tab
      ###################################
      tabPanel(
        tags$p("General Linear Model (GLM)", style = "color:#FF6600"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # style = "position: fixed; ",
            #    # Help Text for Users
            tags$p(
              strong("Instructions:", style = "color:#CC0000"), " Use this tab to fit a General Linear Model (GLM) by selecting an outcome variable, fixed effects, and interaction terms if needed. Click 'Run GLM' to estimate the model and view the summary output."
            ),
            tags$hr(),
            # Fixed effects model sidebar content
            selectizeInput(
              inputId = "subject_id_glm", label = strong("Select subject or ID variable", style = "color:#003399"),
              choices = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "outcome_glm", label = strong("Select outcome variable", style = "color:#003399"),
              choices = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "timevar_glm", label = strong("Select time variable", style = "color:#003399"),
              choices = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "predictor_glm", label = strong("Select fixed effects", style = "color:#003399"),
              choices = NULL,
              multiple = TRUE
            ),
            tags$hr(),
            selectizeInput(
              inputId = "interaction_glm",
              label = strong("Select interaction term(s)", style = "color:#003399"),
              choices = NULL,
              multiple = TRUE
            ),
            actionButton("actionBtnAdd_glm", "Create and add term(s)"),
            uiOutput("uiAdded_glm"),
            tags$hr(),
            actionButton("run_glm_model", "Run GLM")
          ),
          mainPanel(
            width = 9,
            tags$h4("What is a General Linear Model?", style = "color:#003399"),
            tags$p("A General Linear Model (GLM) explains the relationship between a continuous dependent variable and one or more independent variables through a linear equation. GLMs assume a linear relationship between the dependent variable and predictors. If interactions are relevant, you can include them to assess whether the effect of one predictor depends on another."),
            br(),
            tags$h4("Why use GLMs for covariance analysis?", style = "color:#003399"),
            tags$p("Fitting a GLM helps model residuals to uncover the covariance structure in longitudinal data. By specifying the outcome variable and mean structure (fixed effects), you can explore relationships among variables."),
            br(),
            uiOutput("glm_summaryUI"),
            br(),
            br()
          )
        )
      ),
      ###################################
      # Covariance Analysis Tab
      ###################################
      tabPanel(
        tags$p("Covariance Analysis", style = "color:#FF6600"),
        fluidPage(
          fluidRow(
            column(
              width = 10, offset = 1,
              # Covariance analysis main content
              tags$h4("Understanding covariance structures", style = "color:#003399"),
              tags$p("In longitudinal data analysis, covariance structures are used to account for correlations between repeated measurements. The choice of covariance structure directly affects the interpretation of residuals, providing insights into the dependency between observations across time points or spatial dimensions. Residual plots, alongside the partial autocorrelation function (PACF), allow users to visualise the underlying covariance structure that adequately captures these dependencies."),
              tags$p("In this tab, we explore how different covariance structures influence the pattern of residuals and their autocorrelations. For instance, a compound symmetry (CS) structure suggests equal correlation across all observations, leading to a flat residual pattern. Autoregressive structures like AR1 or AR2, on the other hand, reflect time-dependent correlations, with residuals decaying or oscillating based on the lag between observations. The PACF helps further clarify these patterns, highlighting how residuals behave at various time lags, which can guide the selection of a suitable covariance structure for your data."),
              br(),
              checkboxInput("show_resid_info", "Show ideal plots of correlation structures", value = FALSE),
              conditionalPanel(
                condition = "input.show_resid_info == true",
                tags$h4("Ideal residual plots of common correlation structures", style = "color:#003399"),
                fluidRow(
                  column(1, ),
                  column(
                    5, img(src = "unstr2.png", height = 420, align = "center"),
                    br(),
                    tags$p("No specific pattern. The correlation values vary widely across lags, and there is no clear trend. CAUTION: not of practical use because it is hard to estimate.")
                  ),
                  column(
                    5, img(src = "compsymm.png", height = 420, align = "center"),
                    br(),
                    tags$p("A flat line at the constant correlation (rho). In a compound symmetry structure, the correlation is the same for all pairs of variables, regardless of the lag.")
                  )
                ),
                br(),
                fluidRow(
                  column(1, ),
                  column(
                    5, img(src = "ar1.png", height = 420, align = "center"),
                    br(),
                    tags$p("A declining line. As the lag increases, the correlation decreases exponentially. The correlation drops more rapidly for larger lags.")
                  ),
                  column(
                    5, img(src = "ar2.png", height = 420, align = "center"),
                    br(),
                    tags$p("Oscillations in the average autocorrelation reflect the interplay between lag terms, with values alternating around zero, indicating no systematic bias.")
                  )
                ),
                br(),
                br(),
                br(),
                tags$h4("Ideal PACF plots of common correlation structures", style = "color:#003399"),
                fluidRow(
                  column(1, ),
                  column(
                    5, img(src = "unstrpacf.png", height = 420, align = "center"),
                    br(),
                    tags$p("No clear pattern or systematic decay in the partial autocorrelations. Significant spikes may appear at various lags, but without a consistent or interpretable pattern. The partial autocorrelations may fluctuate randomly around zero, with some lags potentially exceeding the significance bounds.")
                  ),
                  column(
                    5, img(src = "cspacf.png", height = 420, align = "center"),
                    br(),
                    tags$p("A single significant spike at lag 1, indicating a constant correlation across all time points followed by a rapid decay towards 0 for the subsequent lags.")
                  )
                ),
                br(),
                fluidRow(
                  column(1, ),
                  column(
                    5, img(src = "ar1pacf.png", height = 420, align = "center"),
                    br(),
                    tags$p("A significant spike at lag 1, with subsequent lags showing values close to zero, reflecting the single-level decay of correlation over time.")
                  ),
                  column(
                    5, img(src = "ar2pacf.png", height = 420, align = "center"),
                    br(),
                    tags$p("For an AR(p) process, the PACF will have significant spikes up to lag p, after which the values drop to zero, highlighting the true order of the autoregressive model. Here, there are 2 signicficant spikes indicating an AR(2) structure.")
                  )
                )
              ),
              br(),
              uiOutput("residualsUI"),
              br()
            )
          ),
        )
      ),
      ###################################
      # Linear Mixed Model Tab
      ###################################
      tabPanel(
        tags$p("Linear Mixed Model (LMM)", style = "color:#FF6600"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            # Linear mixed model sidebar content
            tags$p(
              strong("Instructions:", style = "color:#CC0000"), "Use this tab to fit a Linear Mixed Model (LMM) by selecting an outcome variable, fixed effects, the chosen residual covariance structure (mandatory), and optional interaction terms. Click 'Run LMM' to estimate the model and view the summary outputs. If a required input (such as the covariance structure) is missing, the app will now prompt the user with an error message before proceeding."
            ),
            tags$hr(),
            # variableSelectorUI("lmm_selector"),
            selectizeInput(
              inputId = "subject_id_lmm", label = strong("Select subject or ID variable", style = "color:#003399"),
              choices = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "outcome_lmm", label = strong("Select outcome variable", style = "color:#003399"),
              choices = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "timevar_lmm", label = strong("Select time variable", style = "color:#003399"),
              choices = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              inputId = "predictor_lmm", label = strong("Select fixed effects", style = "color:#003399"),
              choices = NULL,
              multiple = TRUE
            ),
            tags$hr(),
            selectizeInput(
              inputId = "interaction_lmm", label = strong("Select interaction term(s)", style = "color:#003399"),
              choices = NULL,
              multiple = TRUE
            ),
            actionButton("actionBtnAdd", "Create and add term(s)"),
            uiOutput("uiAdded"),
            tags$hr(),
            selectInput("cov_structure",
              label = strong("Select covariance structure", style = "color:#003399"),
              choices = c(" ", "AR(p)", "CompSymm", "Exp", "Gaus", "Lin", "Ratio", "Spher", "Symm")
            ),
            tags$hr(),
            conditionalPanel(
              "input.cov_structure === 'AR(p)'",
              selectInput(
                inputId = "ar_order", label = strong("Select autoregressive order", style = "color:#003399"),
                choices = 1:5
              )
            ),
            tags$hr(),
            checkboxInput(
              inputId = "heteroscedastic_check",
              label = "Include heteroscedastic variances (varIdent)",
              value = FALSE
            ),
            # A warning message placeholder
            uiOutput("hetero_warning"),
            actionButton("run_lmm_model", "Run LMM"),
          ),

          ########################################################################
          mainPanel(
            width = 9,
            # Linear mixed model main content
            tags$h4("What are Linear Mixed Models?", style = "color:#003399"),
            tags$p("Linear Mixed Models (LMMs) are an extension of linear regression that can model longitudinal data by accounting for the correlation in repeated measurements."),
            withMathJax(
              tags$p("The model is often represented as: \\(y_i = X_i β + Z_i b_i + ε_i\\), where \\(b_i \\sim N(0, D)\\), \\(ε_i \\sim N(0, Σ_i)\\)"),
              tags$p("Here, \\(X_i\\) and \\(Z_i\\) are the fixed and random design matrices, respectively. The vector \\(β\\) represents the fixed effects, which describe the mean response in the population, while \\(b_i\\) represents the random effects, capturing individual-specific deviations. The term \\(ε_i\\) refers to the random error, representing unexplained variability in the data. \\(D\\) is the covariance matrix for the random effects, while \\(Σ_i\\) specifies the covariance structure for the residuals.")
            ),
            tags$p("The LMM accommodates both fixed and random effects to model the within-subject and between-subject variability. The model can be formulated hierachically or marginally to capture the correlation. In the", em(" hierachical formulation,", style = "color:#FF6600"), "given the random effects, the measurements of each subject are independent (conditional independence assumption), meaning the more random effects we include the more flexibly we capture the correlations. In the", em(" marginal formulation,", style = "color:#FF6600"), "the measurements of each subject are correlated and this correlation is estimated by the marginal covariance matrix."),
            tags$p(strong("This app focuses on the marginal formulation.")),
            br(),
            tags$h4("Variance Components", style = "color:#003399"),
            withMathJax(
              tags$p("In the marginal formulation, the covariance matrix of the outcome variable \\(y_i\\) is given by: \\(V_i = Z_i D Z_i' + Σ_i\\)")
            ),
            tags$p("We need an appropriate choice for \\(V_i\\) in order to appropriately
describe the correlations between the repeated measurements."),
            br(),
            tags$h4("Application", style = "color:#003399"),
            tags$p("In practice, valid and efficient inferences for the fixed effects require an appropriately specified marginal covariance structure. This ensures that the correlation between repeated measurements is accounted for, reducing bias in the estimation of fixed effects."),
            tags$p("This app allows users to focus on modeling the marginal covariance structure, ensuring robust model fitting. The choice of covariance structure influences both the variance component estimates and the accuracy of hypothesis testing for fixed effects."),
            br(),
            uiOutput("lmm_summaryUI")
          )
        )
      ),
      ###################################
      # Reports Tab
      ###################################
      tabPanel(
        tags$p("Reports", style = "color:#FF6600"),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            tags$p(
              strong("Generate a report summarizing your analyses. Choose a format (HTML, PDF, or Word) and click the button to download.", style = "color:#003399")
            ),
            tags$hr(),
            # Report generation sidebar content
            selectInput("report_format", label = strong("Select Report Format", style = "color:#003399"), choices = c("HTML (Recommended)" = "HTML", "PDF" = "PDF", "Word" = "Word")),
            downloadButton("download_report", "Download Report")
          ),
          mainPanel(
            width = 9,
            tags$p("This dynamic report includes the following sections:"),
            tags$ol(
              tags$li("GLM Analysis", style = "color:#003399"),
              tags$p("Summary and plots from the General Linear Model (GLM) analysis."),
              tags$li("Covariance Analysis", style = "color:#003399"),
              tags$p("Details of the covariance structures used and their impact on the model."),
              tags$li("LMM Analysis", style = "color:#003399"),
              tags$p("Results and plots from the Linear Mixed Model (LMM) analysis.")
            )
          )
        )
      )
    ),

    ###################################
    # Manual Tab
    ###################################
    tabPanel(
      tags$p(strong("Manual"), style = "color:#FF6600"),
      fluidPage(
        img(src = "logoVUB.png", height = 50, align = "right"),
        titlePanel(h3("Documentation", style = "color:#003399")),
        tags$hr(),
        tags$h3("Steps on how to use the LiMMCov app"),
        br(),
        tags$h4("Step 1: Load data"),
        tags$p(
          "Begin by selecting an example dataset or uploading your data file. You can do this in the ",
          strong("Data Input"), " sidebar tab. Make sure to choose the correct file extension to ensure proper data loading. Make sure your dataset is in long format (i.e., one row per subject per time point). Wide-format data must be reshaped prior to use."
        ),
        tags$p(
          "If you need to change the data type of a variable, select the variable in question and then choose the ",
          em("New data type."), "Apply changes by clicking the ", em("Change data type"), " button."
        ),
        tags$p(
          "To verify that the data has been loaded correctly, check the ", strong("View Data"), " tab, which displays a preview (10 rows by default). You can adjust this setting through the ",
          em("Show entries"), " dropdown. A summary of your dataset, including descriptive statistics and variable distributions, can be found in the ",
          strong("Data Summary"), " tab."
        ),
        br(),
        # tags$p(
        #   strong("Note on Missing Data: "),
        #   "LiMMCov requires complete datasets, so we recommend handling missing values before uploading. While multiple imputation is one possible strategy, other approaches (e.g., full information maximum likelihood, inverse probability weighting) may be more appropriate depending on factors like sample size or missing not at random (MNAR) data. Because LiMMCov focuses on choosing the covariance structure, data preprocessing and imputation lie outside the app’s scope. We encourage users to consult the relevant literature or references for best practices in addressing missingness, ensuring the validity of subsequent analyses."
        # ),
        tags$p(
          strong("Note on Missing Data: "),
          "LiMMCov requires complete datasets, so we recommend handling missing values before uploading. While multiple imputation is one possible strategy, other approaches (e.g., full information maximum likelihood, inverse probability weighting) may be more appropriate depending on factors like sample size or missing not at random (MNAR) data. Because LiMMCov focuses on choosing the covariance structure, data preprocessing and imputation lie outside the app’s scope. We encourage users to consult the references below for best practices in addressing missingness, ensuring the validity of subsequent analyses:"
        ),
        tags$ul(
          tags$li("Molenberghs G, Fitzmaurice G, Kenward MG, Tsiatis A, Verbeke G. Handbook of missing data methodology. CRC Press; 2014."),
          tags$li("Nooraee N, Molenberghs G, Ormel J, Van den Heuvel ER. Strategies for handling missing data in longitudinal studies with questionnaires. J Stat Comput Simul. 2018;88(17):3415–36. https://doi.org/10.1080/00949655.2018.1520854"),
          tags$li("Ji L, Chow SM, Schermerhorn AC, Jacobson NC, Cummings EM. Handling Missing Data in the Modeling of Intensive Longitudinal Data. Struct Equ Model. 2018;25(5):715–36. https://doi.org/10.1080/10705511.2017.1417046"),
          tags$li("Heymans MW, Twisk JWR. Handling missing data in clinical research. J Clin Epidemiol. 2022;151:185–8. https://doi.org/10.1016/j.jclinepi.2022.08.01")
        ),
        br(),
        tags$h4("Step 2: Fit linear model (GLM)"),
        tags$p(
          "Next, specify the variables for the model. Start by selecting the subject/id, outcome variable, and the desired mean structure in the", strong("General Linear Model (GLM)"), "tab."
        ),
        tags$p(
          "After specifying the model, click the ", em("Run GLM"), " button to fit the model. You can view the model results under the ", em("Model Summary"), " section."
        ),
        br(),
        tags$h4("Step 3: Analyse the covariance"),
        tags$p("Use the ", strong("Covariance Analysis"), " tab to explore the underlying covariance structure of the residuals. It provides insights into the correlation structure of residuals from the GLM. Residual plots help identify specific patterns related to different correlation structures, while PACF plots provide further insight."),
        br(),
        tags$p(em("Interpreting Residual Plots and PACF:")),
        tags$p("Use this quick reference guide to determine the appropriate covariance structure based on your residual plots and PACF patterns:"),
        tags$ul(
          tags$li(tags$b("Step 1: "), "Examine the residual plot. If there is no correlation (flat line across all lags), use Compound Symmetry (CS)."),
          tags$li(tags$b("Step 2: "), "If residuals oscillate, use AR(2)."),
          tags$li(tags$b("Step 3: "), "If there is a gradual decay in correlation, check the PACF."),
          tags$ul(
            tags$li(tags$b("PACF: Single spike at lag 1 → "), "Use AR(1)."),
            tags$li(tags$b("PACF: Spikes at lags 1 and 2 → "), "Use AR(2)."),
            tags$li(tags$b("PACF: Multiple spikes up to lag p → "), "Use AR(p).")
          )
        ),
        tags$br(),
        # tags$p("Refer to the decision tree below for a visual representation:"),
        img(src = "decision_tree.png", height = "400px", width = "600px", align = "center"),
        tags$br(),
        tags$br(),
        tags$p("The following covariance structures are available in the nlme package, each with different applications:"),
        tags$ol(
          tags$li(tags$b("corAR1 (Autoregressive order 1):")),
          tags$ul(
            tags$li("Models a correlation where observations closer in time/space are more correlated."),
            tags$li("The correlation decays exponentially as time/distance increases."),
            tags$li("In residual plots, covariances decay linearly with increasing lags."),
            tags$li("Use for equally spaced measurements.")
          ),
          br(),
          tags$li(tags$b("corAR(p) - Autoregressive order p:")),
          tags$ul(
            tags$li("Allows for complex autoregressive patterns beyond AR1."),
            tags$li("Graphical patterns depend on specific AR orders."),
            tags$li("Suitable for equally spaced time points.")
          ),
          br(),
          tags$li(tags$b("corCAR1 (Continuous AR1):")),
          tags$ul(
            tags$li("Similar to AR1, but for continuous-time covariates."),
            tags$li("Correlation decays exponentially with increasing time/distance."),
            tags$li("Useful for unequally spaced time points."),
            tags$li("Ideal for continuous-time and unbalanced data.")
          ),
          br(),
          tags$li(tags$b("corCompSymm (Compound Symmetry):")),
          tags$ul(
            tags$li("Assumes constant correlation across all observation pairs."),
            tags$li("Equal variance parameters (homogeneous)."),
            tags$li("Covariances at all lags in residual plots will be similar."),
            tags$li("For equally spaced time points and exchangeable structures.")
          ),
          br(),
          tags$li(tags$b("Spatial Correlation (corExp, corGaus, etc.):")),
          tags$ul(
            tags$li("Models spatial correlation based on distance between observations."),
            tags$li("Useful for irregularly spaced observations in 2D/3D space."),
            tags$li("Non-linear covariance patterns based on spatial locations."),
            tags$li("Ideal for continuous-time and unbalanced data.")
          ),
          br(),
          tags$li(tags$b("corSymm (General Correlation):")),
          tags$ul(
            tags$li("Unstructured correlation matrix."),
            tags$li("No pattern assumptions."),
            tags$li("No specific graphical pattern."),
            tags$li("Useful for complex, irregular correlation structures but requires more parameters.")
          )
        ),
        br(),
        tags$h4("Step 4: Fit linear mixed model (LMM)"),
        tags$p(
          "To fit a linear mixed model, select the subject/id, outcome, time variables, and fixed effects. If your model includes interaction terms, define them using the ",
          em("Select interaction term(s)"), " field."
        ),
        tags$p(
          "You can select the observed covariance structure from the dropdown. For higher AR terms, use the AR(p) option and specify the appropriate order."
        ),
        tags$p(
          "Fit the model by clicking the ", em("Run LMM"), " button. The results will appear under the ", em("Model Summary"), " section."
        ),
        tags$p(
          "To compare different models, refer to the AIC table. The model with the lowest AIC is generally considered the best-fitting model. If convergence issues occur, they will be flagged in the AIC column."
        ),
        br(),
        tags$h4("Step 5: Generate a report"),
        tags$p(
          "After your analysis, you can generate a report by selecting the desired format (HTML, PDF, or Word). This report includes a summary of the linear model, covariance analysis, and the best-fitting linear mixed model, along with plots."
        ),
        br(),
        tags$h4("Note"),
        tags$p(
          "At any time, you can refresh the session and start over by clicking the ", strong("Power"), " button."
        ),
        br(),
        br(),
        br(),
        br(),
        br()
      )
    ),


    ###################################
    # Contact Us Tab
    ###################################
    tabPanel(
      tags$p(strong("Contact Us"), style = "color:#FF6600"),
      fluidPage(
        img(src = "logo_bisi_rgb.jpg", height = 60, align = "right"),
        titlePanel(h3("Developers", style = "color:#003399")),
        hr(),
        # h3("Perseverence Savieri"),
        fluidRow(
          column(2,
            img(src = "HR_BIBI_Percy2.jpg", height = 200),
            br(), br(),
            style = "text-align: center;"
          ),
          column(
            8,
            br(), br(),
            tags$p("Perseverence Savieri is a doctoral researcher in the", tags$a(href = "https://bisi.research.vub.be/", "Biostatistics and Medical Informatics research group (BISI)"), "at the Vrije Universiteit Brussel medical campus Jette. He is also a principal statistical consultant for the humanities and social sciences at campus Etterbeek through the", tags$a(href = "https://square.research.vub.be/", "Support for Quantitative and Qualitative Research (SQUARE)"), "core facility. Here, he offers statistical and methodological quantitative support in the form of consultations, statistical coaching, data analyses and workshops.", style = "text-align: center;"),
            h5(tags$a(href = "mailto:perseverence.savieri@vub.be", "Perseverence.Savieri@vub.be"), style = "text-align: center;"),
            br(),
            h4("Supervisors", style = "text-align: center;"),
            h5("dr. Lara Stas & Prof. dr. Kurt Barbe", style = "text-align: center;")
            # Kurt Barb\xe9
          )
        )
      ),
      # Footer
      # tags$hr(),
      absolutePanel(
        bottom = 10,
        left = 0,
        right = 0,
        height = "auto",
        fixed = TRUE,
        tags$div(
          style = "text-align: left; width: 100%; padding: 10px;",
          tags$h6(
            em("Copyright 2025. Support for Quantitative and Qualitative Research. Version 28.03.25")
          )
        )
      )
    ),
    navbarMenu(
      title = tagList(icon("power-off", style = "color:#FF6600")),
      tabPanel("Stop", value = "stop"),
      tabPanel("Refresh", value = "refresh"),
      tabPanel("New session", value = "new_session")
    ),
    position = c("fixed-top"),
    tags$style(type = "text/css", "body{padding-top: 90px;}")
  )
)

#####################################
# Define server logic
#####################################
server <- function(input, output, session) {
  # INPUTS --------------------------------------------------------------------
  observe({
    if (input$navbar == "stop") {
      stopApp() # This will stop the Shiny app
    } else if (input$navbar == "refresh") {
      session$reload() # This will refresh the session
    } else if (input$navbar == "new_session") {
      session$reload() # Assuming starting a new session is equivalent to reloading
    }
  })

  # Load/upload data
  data_input <- reactive({
    inFile <- input$file1
    exdata <- input$exdata

    # Example dataset ----------------------------------------------------------
    if (is.null(inFile)) {
      if (exdata == "") {
        return(NULL)
      } else if (exdata == "AR(1) Example") {
        return(as.data.frame(simdata_ar1))
      } else if (exdata == "AR(2) Example") {
        return(as.data.frame(simdata_ar2))
      } else if (exdata == "Comp.Symmetry Example") {
        return(as.data.frame(simdata_cs))
      } else if (exdata == "COVID data") {
        return(as.data.frame(covid))
      }
    }

    # Load own dataset ---------------------------------------------------------
    if (!is.null(inFile)) {
      if (input$ext == "txt") {
        return(read.csv(inFile$datapath, sep = "", header = T))
      } else if (input$ext == "csv") {
        return(read.csv(inFile$datapath, header = T))
      } else if (input$ext == "xlsx") {
        return(as.data.frame(read_excel(inFile$datapath, sheet = 1)))
      } else if (input$ext == "sav") {
        # return(read.spss(inFile$datapath, to.data.frame = T, use.value.labels = F))
        return(as.data.frame(read_sav(inFile$datapath)))
      } else if (input$ext == "dta") {
        # return(read.dta(inFile$datapath))
        return(as.data.frame(read_dta(inFile$datapath)))
      }
    }
  })

  dataset <- reactiveVal(NULL)

  # Read data
  observeEvent(data_input(), {
    dataset(data_input())
  })

  # Generate preprocessing options based on user selection
  output$edit_vars <- renderUI({
    req(dataset())

    # Generate variable selection
    var_names <- colnames(dataset())
    tagList(
      tags$h5(em("Data manipulation and preprocessing", style = "color:#FF6600")),
      selectizeInput("selected_var", label = strong("Select variable(s)", style = "color:#003399"), choices = c(" ", var_names), selected = NULL, multiple = FALSE)
    )
  })

  # Show data management options when variables are selected
  output$data_management <- renderUI({
    req(input$selected_var)

    selectizeInput("data_management", label = strong("Select procedure", style = "color:#003399"), choices = c("", "Change data type", "Transform"))
  })

  # Conditional panels for data management options
  output$manage_data_options <- renderUI({
    req(input$selected_var, input$data_management)

    if (input$data_management == "Change data type") {
      selectizeInput("new_data_type", label = strong("New data type", style = "color:#003399"), choices = c("", "Factor", "Numeric", "Integer", "Character", "Date (ddmmyy)"))
    } else if (input$data_management == "Transform") {
      selectizeInput("transform_type", label = strong("Transformation type", style = "color:#003399"), choices = c("", "Ln (natural log)", "Ln (X+1)", "Exp", "Square", "Cube", "Square root", "Standardize", "Center", "Inverse"))
    }
  })

  # Conditional panel for variable name extension
  output$var_name_extension <- renderUI({
    req(input$selected_var, input$transform_type)

    if (!is.null(input$transform_type)) {
      textInput("var_name_extension", label = strong("Variable name extension", style = "color:#003399"), placeholder = "Optional")
    }
  })

  output$changes_button <- renderUI({
    req(dataset())
    actionButton("apply_changes", "Apply Changes")
  })

  # Apply changes to dataset
  observeEvent(input$apply_changes, {
    # req(input$selected_var)
    new_data <- dataset()

    if (input$data_management == "Change data type") {
      new_data <- new_data %>%
        mutate(!!input$selected_var := switch(input$new_data_type,
          "Factor" = as.factor(!!sym(input$selected_var)),
          "Numeric" = as.numeric(!!sym(input$selected_var)),
          "Integer" = as.integer(!!sym(input$selected_var)),
          "Character" = as.character(!!sym(input$selected_var)),
          "Date (dmy)" = as.Date(!!sym(input$selected_var), format = "%d/%m/%Y")
        ))
    } else if (input$data_management == "Transform") {
      transform_func <- switch(input$transform_type,
        "Ln (natural log)" = log,
        "Ln (X+1)" = function(x) log(x + 1),
        "Exp" = exp,
        "Square" = function(x) x^2,
        "Cube" = function(x) x^3,
        "Square root" = sqrt,
        "Standardize" = scale,
        "Center" = function(x) x - mean(x),
        "Inverse" = function(x) 1 / x
      )

      new_var <- paste(input$selected_var, input$var_name_extension, sep = "_")

      new_data <- new_data %>%
        mutate(!!new_var := transform_func(!!sym(input$selected_var)))
    }

    dataset(new_data)
  })

  # OUTPUTS --------------------------------------------------------------------

  ## OVERVIEW OF DATA
  # Data table from DT
  output$data_preview <- renderDataTable({
    validate(need(dataset(), "Please, upload own dataset or select one from the examples to continue."))
    n <- dataset()
    dprint <- format(n, digits = 3)
    dprint
  })

  # Data summary using dfSummary() from summarytools
  output$data_summ <- renderUI({
    req(dataset())
    print(
      dfSummary(dataset(),
        graph = TRUE, valid.col = FALSE, graph.magnif = 0.75,
        style = "grid"
      ),
      max.tbl.height = 800, method = "render",
      headings = FALSE, bootstrap.css = FALSE
    )
  })

  ## Generalised Linear Model
  # Select variables
  observe({
    if (!is.null(dataset())) {
      value <- colnames(dataset())
      updateSelectizeInput(session, "subject_id_glm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "outcome_glm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "timevar_glm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "predictor_glm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "interaction_glm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "subject_id_lmm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "outcome_lmm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "timevar_lmm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "predictor_lmm", choices = c("", value), server = TRUE)
      updateSelectizeInput(session, "interaction_lmm", choices = c("", value), server = TRUE)
    }
  })

  #------------------------------------------
  # Create list of interaction terms for GLM
  listN_glm <- reactiveValues()
  makeReactiveBinding("listN_glm")

  # Rendering the list of added interactions to the UI
  output$uiAdded_glm <- renderUI({
    checkboxGroupInput("added_glm", "",
      choices = names(listN_glm)
    )
  })

  # Trigger Add actions for GLM interactions
  observe({
    input$actionBtnAdd_glm
    isolate({
      new_selections <- c(input$interaction_glm)
      new_selections_name <- new_selections %>% paste(collapse = "*")
      if (new_selections_name != "") {
        listN_glm[[new_selections_name]] <- new_selections
      }
    })
  })

  #--------------------------------------------
  # Create list of interaction terms for LMM
  listN <- reactiveValues()
  makeReactiveBinding("listN")

  # Rendering the list to the ui
  output$uiAdded <- renderUI({
    checkboxGroupInput("added", "",
      choices = names(listN)
    )
  })

  # Trigger Add actions for LMM interactions
  observe({
    input$actionBtnAdd
    isolate({
      new_selections <- c(input$interaction_lmm)
      new_selections_name <- new_selections %>% paste(collapse = "*")

      if (new_selections_name != "") {
        listN[[new_selections_name]] <- new_selections
      }
    })
  })

  # Run the generalised linear model
  # glm_model <- reactive({
  #   req(input$run_glm_model)
  #   predictors <- paste(input$predictor_glm, collapse = "+")
  #
  #   # Construct the formula with interactions if any
  #   if (!is.null(input$added_glm)) {
  #     interaction_terms <- paste(input$added_glm, collapse = " + ")
  #     formula_str <- paste(input$outcome_glm, "~", input$timevar_glm)
  #
  #     if (!is.null(input$predictor_glm)) {
  #       formula_str <- paste(formula_str, "+", predictors)
  #     }
  #
  #     formula_str <- paste(formula_str, "+", interaction_terms)
  #
  #     gls(as.formula(formula_str), data = dataset())
  #   } else {
  #     # Original logic without interactions
  #     if (!is.null(input$predictor_glm)) {
  #       gls(as.formula(paste(input$outcome_glm, "~", input$timevar_glm, "+", predictors)), data = dataset())
  #     } else {
  #       gls(as.formula(paste(input$outcome_glm, "~", input$timevar_glm)), data = dataset())
  #     }
  #   }
  # })
  ###############################################

  # glm_model <- reactive({
  #   req(input$run_glm_model)
  # 
  #   # Build up the formula string
  #   predictors <- paste(input$predictor_glm, collapse = "+")
  #   formula_str <- paste(input$outcome_glm, "~", input$timevar_glm)
  # 
  #   # If there are main-effect predictors
  #   if (!is.null(input$predictor_glm)) {
  #     formula_str <- paste(formula_str, "+", predictors)
  #   }
  # 
  #   # If there are interaction terms
  #   if (!is.null(input$added_glm)) {
  #     interaction_terms <- paste(input$added_glm, collapse = " + ")
  #     formula_str <- paste(formula_str, "+", interaction_terms)
  #   }
  # 
  #   # Convert to a proper formula object
  #   form_obj <- as.formula(formula_str)
  # 
  #   # Fit the model with the explicit formula in the call
  #   mod <- gls(model = form_obj, data = dataset())
  # 
  #   # Update the call attribute to show the proper formula
  #   mod$call$model <- form_obj
  # 
  #   return(mod)
  # })
  # 
  # # Generate a summary of the regression model
  # output$glm_summaryUI <- renderUI({
  #   req(glm_model())
  #   tagList(
  #     tags$h4("Model summary", style = "color:#003399"),
  #     br(),
  #     uiOutput("glm_model_summary"),
  #     br(), br(), br(), br(),
  #     tags$h4(strong("Click on the Covariance Analysis tab to continue."), style = "color:#FF6600")
  #   )
  # })
  # 
  # output$glm_model_summary <- renderUI({
  #   tab <- tab_model(glm_model())
  #   HTML(tab$knitr)
  # })
  #################################################
  
  glm_model <- reactive({
    req(input$run_glm_model)
    
    dat <- dataset()
    
    # Build up the formula string
    predictors <- paste(input$predictor_glm, collapse = "+")
    formula_str <- paste(input$outcome_glm, "~", input$timevar_glm)
    
    if (!is.null(input$predictor_glm)) {
      formula_str <- paste(formula_str, "+", predictors)
    }
    if (!is.null(input$added_glm)) {
      interaction_terms <- paste(input$added_glm, collapse = " + ")
      formula_str <- paste(formula_str, "+", interaction_terms)
    }
    form_obj <- as.formula(formula_str)
    
    # ---- Check for missing values in variables used ----
    vars_used <- all.vars(form_obj)
    dat_used <- dat[, vars_used, drop = FALSE]
    if (anyNA(dat_used)) {
      showNotification(
        "Missing values detected! This app requires complete data. Please impute or remove missing values before analysis.",
        type = "error",
        duration = NULL
      )
      return(NULL)
    }
    
    # ---- Fit model safely ----
    mod <- tryCatch({
      gls(model = form_obj, data = dat)
    }, error = function(e) {
      showNotification(
        "Error fitting the model (possible missing values). Please check your data.",
        type = "error",
        duration = NULL
      )
      return(NULL)
    })
    
    if (is.null(mod)) return(NULL)
    
    # Update the call attribute to show the proper formula
    mod$call$model <- form_obj
    mod
  })
  
  # Generate a summary of the regression model
  output$glm_summaryUI <- renderUI({
    req(glm_model())
    tagList(
      tags$h4("Model summary", style = "color:#003399"),
      br(),
      uiOutput("glm_model_summary"),
      br(), br(), br(), br(),
      tags$h4(strong("Click on the Covariance Analysis tab to continue."), style = "color:#FF6600")
    )
  })
  
  output$glm_model_summary <- renderUI({
    mod <- glm_model()
    if (is.null(mod)) {
      tags$span(style="color:red;", "No model fitted due to missing data.")
    } else {
      tab <- tab_model(mod)
      HTML(tab$knitr)
    }
  })
  
  # Residuals from fixed effects model
  glm_residuals <- reactive({
    # Extract residuals
    dtresidL <- subset(dataset(), select = c(input$subject_id_glm, input$timevar_glm, input$outcome_glm))
    dtresidL$residuals <- glm_model()$residuals
    dtresidW <- pivot_wider(dtresidL,
      id_cols = input$subject_id_glm, names_from = input$timevar_glm, values_from = residuals,
      names_prefix = "resid_"
    )

    # Create correlation matrices
    resCor <- cor(dtresidW[, -1])

    # Create a list of vectors from the matrix
    listCor <- map(-(ncol(resCor) - 1):(ncol(resCor) - 1), ~ mean(resCor[outer(1:ncol(resCor), 1:ncol(resCor), "-") == .x]))

    # Create a data frame from listCor
    tp <- ncol(resCor)
    lag <- 1:(tp - 1)
    means <- unlist(listCor[1:(tp - 1)])
    dat <- data.frame(lag, means = rev(means))

    # Plot line graph using the mean for each lag in the list
    # This is the plot we will add under "Residual Plots from the Fixed Effects Model"
    p <- ggplot(dat, aes(x = lag, y = means)) +
      geom_line() +
      scale_x_continuous(breaks = lag) +
      ylim(c(-1, 1)) +
      xlab("Lag") +
      ylab("Mean") +
      ggtitle("Correlation structure") +
      theme_bw()

    ggplotly(p, width = 425, height = 425)
  })

  output$plot_glm_residuals <- renderPlotly({
    glm_residuals()
  })

  # Plot the PACF.gls from the nlme package
  auto_plots <- reactive({
    plot_acf_pacf(glm_model())
  })

  output$plot_glm_acf <- renderPlotly({
    auto_plots()$pacf_plot
  })

  # Dynamic UI for the residual plots
  output$residualsUI <- renderUI({
    req(glm_model())
    tagList(
      tags$h4("Residual plots from your fixed effects model (GLM)", style = "color:#003399"),
      br(),
      fluidRow(
        column(1, ),
        column(5, plotlyOutput("plot_glm_residuals")),
        column(5, plotlyOutput("plot_glm_acf"))
      ),
      br(),
      br(),
      br(),
      tags$h4(strong("Proceed to the Linear Mixed Model (LMM) tab with the chosen covariance structure."), style = "color:#FF6600"),
      br(),
      br()
    )
  })

  # LMM Models
  # Reactive formula for GLS model
  gls_formula <- reactive({
    req(input$outcome_lmm, input$timevar_lmm)

    # Collect all terms: timevar, predictors, and interactions
    all_terms <- c(input$timevar_lmm, input$predictor_lmm, input$added)

    # Remove any empty strings (if present)
    all_terms <- all_terms[nzchar(all_terms)]

    # Construct the formula string
    formula_str <- paste(
      input$outcome_lmm,
      "~",
      paste(all_terms, collapse = "+")
    )

    as.formula(formula_str)
  })

  # Function to generate the correlation structure string
  cor_structure <- function(structure) {
    req(input$subject_id_lmm)
    if (structure == "AR(p)") {
      req(input$ar_order)
      order <- as.numeric(input$ar_order)
      return(paste0("corARMA(form = ~ 1 | ", input$subject_id_lmm, ", p = ", order, ")"))
    }
    switch(structure,
      "CompSymm" = paste0("corCompSymm(form = ~ 1 | ", input$subject_id_lmm, ")"),
      "Exp" = paste0("corExp(form = ~ 1 | ", input$subject_id_lmm, ")"),
      "Gaus" = paste0("corGaus(form = ~ 1 | ", input$subject_id_lmm, ")"),
      "Lin" = paste0("corLin(form = ~ 1 | ", input$subject_id_lmm, ")"),
      "Ratio" = paste0("corRatio(form = ~ 1 | ", input$subject_id_lmm, ")"),
      "Spher" = paste0("corSpher(form = ~ 1 | ", input$subject_id_lmm, ")"),
      "Symm" = paste0("corSymm(form = ~ 1 | ", input$subject_id_lmm, ")")
    )
  }

  # # Helper function to fit models
  # Update fit_model() to accept weights
  fit_model <- function(form, correlation, weights = NULL) {
    req(form, correlation)

    # Convert formula to a string
    form_str <- paste(deparse(form, width.cutoff = 500), collapse = " ")

    # Build the gls() call
    final_call <- paste0(
      "gls(", form_str,
      ", data = dataset(), 
       correlation = ", correlation
    )

    # Add weights if specified
    if (!is.null(weights)) {
      final_call <- paste0(final_call, ", weights = ", weights)
    }
    final_call <- paste0(final_call, ")")

    # Evaluate safely
    tryCatch(
      eval(parse(text = final_call)),
      error = function(e) {
        print(e)
        NULL
      }
    )
  }

  # Reactive block for the initial model
  initial_model <- reactive({
    req(input$run_lmm_model, input$cov_structure)
    # Instead of passing deparse(gls_formula()) directly,
    # pass the formula object, letting fit_model handle deparsing
    fit_model(
      form = gls_formula(),
      correlation = cor_structure(input$cov_structure)
    )
  })
  
  observeEvent(input$run_lmm_model, {
    if (is.null(input$cov_structure) || input$cov_structure == " ") {
      showModal(modalDialog(
        title = "Missing Input",
        "You must select a covariance structure before fitting the model.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  

  # Reactive for the heteroscedastic model
  heteroscedastic_model <- reactive({
    req(input$heteroscedastic_check, input$cov_structure, input$subject_id_lmm)

    # Define weights structure (varIdent per subject)
    weights_str <- paste0("varIdent(form = ~1 | ", input$subject_id_lmm, ")")

    # Fit model with selected correlation + heteroscedastic variance
    fit_model(
      form = gls_formula(),
      correlation = cor_structure(input$cov_structure),
      weights = weights_str # Pass weights argument
    )
  })

  # Show warning when heteroscedastic model is selected
  output$hetero_warning <- renderUI({
    if (input$heteroscedastic_check) {
      div(
        class = "alert alert-warning",
        HTML("&#9888; Including heteroscedastic variances (varIdent) may increase computation time.")
      )
    }
  })

  # Reactive block for alternative models
  fit_alternative_models <- reactive({
    req(input$run_lmm_model)

    # The full set of correlation structures
    all_structures <- c(
      "AR(p)", "CompSymm", "Exp", "Gaus",
      "Lin", "Ratio", "Spher", "Symm"
    )
    remaining_structures <- setdiff(all_structures, input$cov_structure)

    # Fit each remaining structure
    models <- lapply(remaining_structures, function(structure) {
      fit_model(
        form = gls_formula(),
        correlation = cor_structure(structure)
      )
    })

    # Name the list of models after their structures
    names(models) <- remaining_structures

    models
  })

  # Helper function to safely get AIC or BIC
  safe_stat <- function(model, stat = "AIC") {
    if (is.null(model)) "Model did not converge" else round(get(stat)(model), 2)
  }

  # Combine AICs and BICs into a table
  comparison_table <- reactive({
    req(input$run_lmm_model)

    # Initial model (homoscedastic)
    first_model_aic <- safe_stat(initial_model(), "AIC")
    first_model_bic <- safe_stat(initial_model(), "BIC")
    first_model_aicc <- safe_stat(initial_model(), "AICc")

    # Heteroscedastic model (if selected)
    if (input$heteroscedastic_check) {
      hetero_model <- heteroscedastic_model()
      hetero_aic <- safe_stat(hetero_model, "AIC")
      hetero_bic <- safe_stat(hetero_model, "BIC")
      hetero_aicc <- safe_stat(hetero_model, "AICc")
    } else {
      hetero_aic <- hetero_bic <- hetero_aicc <- NULL
    }

    # Alternative models (other correlation structures)
    models <- fit_alternative_models()
    alternative_aics <- sapply(models, safe_stat, stat = "AIC")
    alternative_bics <- sapply(models, safe_stat, stat = "BIC")
    alternative_aiccs <- sapply(models, safe_stat, stat = "AICc")

    # Build table rows
    selected_label <- ifelse(
      input$cov_structure == "AR(p)",
      paste0("AR(", input$ar_order, ") (Selected structure)"),
      paste(input$cov_structure, "(Selected structure)")
    )

    df_rows <- list(
      c(selected_label, first_model_aic, first_model_bic, first_model_aicc)
    )

    if (input$heteroscedastic_check) {
      df_rows <- append(
        df_rows,
        list(c("Selected structure + Heteroscedastic variances", hetero_aic, hetero_bic, hetero_aicc))
      )
    }

    df_rows <- append(df_rows, lapply(seq_along(alternative_aics), function(i) {
      c(names(alternative_aics)[i], alternative_aics[i], alternative_bics[i], alternative_aiccs[i])
    }))

    # Create data.frame
    data.frame(
      do.call(rbind, df_rows),
      stringsAsFactors = FALSE
    ) %>% setNames(c("Correlation Structure", "AIC", "BIC", "AICc"))
  })

  # selected_ar <- paste0("AR(", input$ar_order, ") (Selected)")

  # Create table with all models
  #   data.frame(
  #     "Correlation Structure" = c(
  #       ifelse(input$cov_structure == "AR(p)", selected_ar, paste(input$cov_structure, "(Selected)")),
  #       "Selected + Heteroscedastic variances",
  #       names(alternative_aics)
  #     ),
  #
  #     "AIC" = c(first_model_aic, hetero_aic, alternative_aics),
  #     "BIC" = c(first_model_bic, hetero_bic, alternative_bics),
  #     "AICc" = c(first_model_aicc, hetero_aicc, alternative_aiccs),
  #     stringsAsFactors = FALSE
  #   )
  # })

  # Render AIC and BIC table
  observeEvent(input$run_lmm_model, {
    output$lmm_aic_table <- renderDT({
      req(input$run_lmm_model)
      tryCatch(
        datatable(comparison_table(), options = list(pageLength = 10), rownames = FALSE),
        error = function(e) {
          showNotification("Error generating table: Check model inputs", type = "error")
          NULL
        }
      )
    })
  })


  # Render AIC and BIC table
  # observeEvent(input$run_lmm_model, {
  #   output$lmm_aic_table <- renderDT({
  #     datatable(comparison_table(), options = list(pageLength = 10), rownames = FALSE)
  #   })
  # })

  # Print the correlation structure for debugging
  # observeEvent(input$run_lmm_model, {
  #   req(cor_structure())
  #   print(cor_structure())
  # })

  #-------------------------------------------------------------------------------  
  # Dynamic UI for the LMM summary
  output$lmm_summaryUI <- renderUI({
    req(initial_model(), input$run_lmm_model)
    tagList(
      tags$h4("Model Summary", style = "color:#003399"),
      # Spinner and conditional message for model summary
      withSpinner(
        tagList(
          verbatimTextOutput("lmm_summary"),
          conditionalPanel(
            condition = "!output.lmm_summary || output.lmm_summary === ''",
            tags$p("The model is being fitted. This process may take a few moments, so please be patient.",
              style = "color:#003399; font-style: italic;"
            )
          )
        ),
        type = 6,
        color = "#003399"
      ),
      br(),
      fluidRow(
        column(
          12,
          tags$h4("Model Fit Comparison", style = "color:#003399"),
          # Spinner and conditional message for AIC table
          withSpinner(
            tagList(
              DTOutput("lmm_aic_table"),
              conditionalPanel(
                condition = "!output.lmm_aic_table || output.lmm_aic_table === ''",
                tags$p("Calculating model fit metrics. Please wait while the table is generated.",
                  style = "color:#003399; font-style: italic;"
                )
              )
            ),
            type = 6,
            color = "#003399"
          ),
          br(),
          br(),
          br(),
          br(),
          br()
        )
      )
    )
  })

  # Display the summary of the initial model
  observeEvent(input$run_lmm_model, {
    output$lmm_summary <- renderPrint({
      req(input$run_lmm_model)
      validate(need(input$cov_structure, "Please select covariance structure"))
      summary(initial_model())
    })
  })

  #------------------------------------------------------------------------------
  output$download_report <- downloadHandler(
    filename = function() {
      paste("report", switch(input$report_format,
        HTML = ".html",
        PDF = ".pdf",
        Word = ".docx"
      ), sep = "")
    },
    content = function(file) {
      rmarkdown::render(
        input = "report_template.Rmd",
        output_file = file,
        params = list(
          glm_summary = summary(glm_model()),
          glm_residuals = glm_residuals(),
          glm_acf = auto_plots()$pacf_plot,
          # aic_plot = aic_plot(),
          lmm_summary = summary(initial_model()),
          aic_table = comparison_table()
        ),
        envir = new.env(parent = globalenv()),
        output_format = switch(input$report_format,
          HTML = "html_document",
          PDF = "pdf_document",
          Word = "word_document"
        )
      )
    }
  )
}

#################################
# Run the Shiny app
shinyApp(ui, server)
