# CAIS Cost-Effectiveness Model - Shiny App
# Launch with: huncMarkovABD::launch_app()
library(shiny)
library(huncMarkovABD)
has_ggplot <- requireNamespace("ggplot2", quietly=TRUE)
has_DT     <- requireNamespace("DT",      quietly=TRUE)
if (has_ggplot) library(ggplot2)
default_reg <- get_parameter_registry()
get_default_se <- function(nm) {
  idx <- which(default_reg$name == nm)
  if (length(idx)==1 && !is.na(default_reg$se[idx])) default_reg$se[idx] else NA_real_
}

ui <- fluidPage(
  titlePanel("ABD Cost-Effectiveness Model"),
  sidebarLayout(
    sidebarPanel(width=3,
      h4("Model Configuration"),
      numericInput("time_horizon","Time Horizon (years)",value=50,min=10,max=50,step=1),
      numericInput("starting_age","Starting Age",value=48,min=18,max=80,step=1),
      sliderInput("discount_rate","Discount Rate (%)",min=0,max=10,value=3,step=0.5),
      numericInput("wtp","WTP Threshold (CAD $)",value=100000,min=0,step=10000),
      hr(),
      h4("Analysis Type"),
      radioButtons("analysis_type",NULL,
                   choices=c("Deterministic"="det","Probabilistic (PSA)"="psa"),selected="det"),
      conditionalPanel(condition="input.analysis_type == 'psa'",
        numericInput("n_sims","PSA Iterations:",value=1000,min=100,max=5000,step=100),
        checkboxInput("use_seed","Use Random Seed",value=FALSE),
        conditionalPanel(condition="input.use_seed == true",
          numericInput("seed","Seed Value:",value=12345,min=1))
      ),
      hr(),
      actionButton("run","Run Model",class="btn-primary btn-lg"),
      actionButton("reset_params","Reset to Defaults",class="btn-secondary"),
      br(),br(),
      helpText("All costs in CAD. 2-week cycles, Canadian life table (55% female).")
    ),
    mainPanel(width=9,
      tabsetPanel(
        tabPanel("About",br(),
          h3("ABD Cost-Effectiveness Model"),
          p("Interactive Markov cohort model: Lunavar+SoC (biologic add-on) vs SoC alone for Acute Bronchial Disorder (ABD)."),
          h4("Model Structure"),
          tags$ul(
            tags$li("5 health states: Stable, FlareUp_Mild, FlareUp_Moderate, FlareUp_Severe, Dead"),
            tags$li("3 tunnel states: flare-up states last exactly 1 cycle (2 weeks), then resolve to Stable"),
            tags$li("2-week cycle length (2/52 years)"),
            tags$li("50-year (lifetime) time horizon, 1,300 cycles"),
            tags$li("Age-dependent background mortality (Canadian life table, 55% female)"),
            tags$li("Flare-up rates: Poisson conversion from annual event rates, per-severity RR treatment effects"),
            tags$li("Severe flare-ups carry additional mortality risk"),
            tags$li("Mid-cycle discounting convention")
          ),
          h4("Treatments"),
          tags$ul(
            tags$li("SoC - comparator (standard of care: inhaled corticosteroids + LABA)"),
            tags$li("Lunavar+SoC - intervention (biologic add-on therapy)")
          ),
          h4("Source"),
          p("Fictional illustrative model developed for HEPackageR skill validation. Not for clinical or policy use."),
          hr(),
          h4("Development"),
          p("Ian Cromwell (healthyuncertainty@gmail.com)"),
          p("Developed using the ",tags$a(href="https://github.com/HealthyUncertainty/hepackager","HEPackageR")," skill for Claude AI.")
        ),
        tabPanel("Parameters",br(),
          h4("Adjust Model Parameters"),
          p("Modify means and SEs. Fixed-distribution parameters cannot be varied in PSA."),
          tabsetPanel(
            tabPanel("Event Rates & Treatment Effects",br(),
              fluidRow(column(6,h5("Mean")),column(6,h5("SE (PSA)"))),
              h5("Annual Flare-Up Event Rate"),
              fluidRow(
                column(6,numericInput("aaer_soc","Annual Flare-Up Rate (SoC)",1.65,min=0,step=0.01)),
                column(6,numericInput("se_aaer_soc","SE",get_default_se("aaer_soc"),min=0,step=0.01))
              ),
              h5("Treatment Effect RRs (Lunavar vs SoC)"),
              fluidRow(
                column(6,numericInput("rr_mild","RR: Mild Flare-Up",0.45,min=0.01,max=2,step=0.01)),
                column(6,numericInput("se_rr_mild","SE (log scale)",get_default_se("rr_mild"),min=0,step=0.01))
              ),
              fluidRow(
                column(6,numericInput("rr_moderate","RR: Moderate Flare-Up",0.25,min=0.01,max=2,step=0.01)),
                column(6,numericInput("se_rr_moderate","SE (log scale)",get_default_se("rr_moderate"),min=0,step=0.01))
              ),
              fluidRow(
                column(6,numericInput("rr_severe","RR: Severe Flare-Up",0.18,min=0.01,max=2,step=0.01)),
                column(6,numericInput("se_rr_severe","SE (log scale)",get_default_se("rr_severe"),min=0,step=0.01))
              ),
              h5("Flare-Up Type Proportions"),
              p(em("Note: sampled independently in PSA (original model behaviour).")),
              fluidRow(
                column(6,sliderInput("prop_mild","Proportion: Mild",0,1,0.72,0.01)),
                column(6,numericInput("se_prop_mild","SE",get_default_se("prop_mild"),min=0,step=0.001))
              ),
              fluidRow(
                column(6,numericInput("prop_moderate","Proportion: Moderate",0.12,min=0,max=1,step=0.01)),
                column(6,numericInput("se_prop_moderate","SE",get_default_se("prop_moderate"),min=0,step=0.001))
              ),
              fluidRow(
                column(6,numericInput("prop_severe","Proportion: Severe",0.16,min=0,max=1,step=0.01)),
                column(6,numericInput("se_prop_severe","SE",get_default_se("prop_severe"),min=0,step=0.001))
              )
            ),
            tabPanel("Mortality",br(),
              fluidRow(column(6,h5("Mean")),column(6,h5("SE (PSA)"))),
              fluidRow(
                column(6,numericInput("prob_death_severe_flare","Prob: Death during severe flare-up",0.0055,min=0,max=1,step=0.0001)),
                column(6,numericInput("se_prob_death_severe_flare","SE",get_default_se("prob_death_severe_flare"),min=0,step=0.0001))
              )
            ),
            tabPanel("Utilities",br(),
              fluidRow(column(6,h5("Mean")),column(6,h5("SE (PSA)"))),
              fluidRow(
                column(6,sliderInput("utility_stable_lunavar","Utility: Stable (Lunavar+SoC)",0,1,0.805,0.005)),
                column(6,numericInput("se_utility_stable_lunavar","SE",get_default_se("utility_stable_lunavar"),min=0,step=0.001))
              ),
              fluidRow(
                column(6,sliderInput("utility_stable_soc","Utility: Stable (SoC)",0,1,0.760,0.005)),
                column(6,numericInput("se_utility_stable_soc","SE",get_default_se("utility_stable_soc"),min=0,step=0.001))
              ),
              h5("Disutilities (Fixed in PSA)"),
              fluidRow(column(6,numericInput("disutility_mild","Disutility: Mild flare-up",0.08,min=0,step=0.005)),column(6,helpText("Fixed"))),
              fluidRow(column(6,numericInput("disutility_moderate","Disutility: Moderate flare-up",0.14,min=0,step=0.005)),column(6,helpText("Fixed"))),
              fluidRow(column(6,numericInput("disutility_severe","Disutility: Severe flare-up",0.22,min=0,step=0.01)),column(6,helpText("Fixed")))
            ),
            tabPanel("Costs",br(),
              h5("Drug Costs (Annual, CAD)"),
              fluidRow(column(6,h6("Mean")),column(6,h6("SE (PSA)"))),
              fluidRow(
                column(6,numericInput("cost_drug_lunavar_annual","Lunavar (annual)",4800,min=0,step=100)),
                column(6,numericInput("se_cost_drug_lunavar_annual","SE",get_default_se("cost_drug_lunavar_annual"),min=0,step=100))
              ),
              fluidRow(
                column(6,numericInput("cost_drug_soc_annual","SoC (annual)",5200,min=0,step=100)),
                column(6,helpText("Fixed in PSA"))
              ),
              h5("Flare-Up Event Costs (CAD)"),
              fluidRow(
                column(6,numericInput("cost_mild_event","Mild flare-up event",1350,min=0,step=50)),
                column(6,numericInput("se_cost_mild_event","SE",get_default_se("cost_mild_event"),min=0,step=10))
              ),
              fluidRow(
                column(6,numericInput("cost_moderate_event","Moderate flare-up event",1850,min=0,step=50)),
                column(6,numericInput("se_cost_moderate_event","SE",get_default_se("cost_moderate_event"),min=0,step=10))
              ),
              fluidRow(
                column(6,numericInput("cost_severe_event","Severe flare-up event",8200,min=0,step=100)),
                column(6,numericInput("se_cost_severe_event","SE",get_default_se("cost_severe_event"),min=0,step=50))
              )
            )
          )
        ),
        tabPanel("Results",br(),
          h4("Incremental Cost-Effectiveness Analysis"),
          conditionalPanel(condition="output.has_results == false",
            div(class="alert alert-info",style="font-size:16px;",
                "ℹ Click Run Model to generate results.")),
          if(has_DT) DT::dataTableOutput("icer_table") else tableOutput("icer_table"),
          br(),downloadButton("download_icer","Download ICER Table (CSV)")
        ),
        tabPanel("Probabilistic",br(),
          conditionalPanel(condition="input.analysis_type == 'psa'",
            h4("Cost-Effectiveness Plane"),plotOutput("ce_plane",height="500px"),br(),
            h4("Cost-Effectiveness Acceptability Curve"),
            plotOutput("ceac_plot",height="500px"),br(),
            downloadButton("download_psa","Download PSA Plots (PNG)")
          ),
          conditionalPanel(condition="input.analysis_type == 'det'",br(),
            div(class="alert alert-info",style="font-size:16px;",
                "ℹ Switch to Probabilistic (PSA) to view CE plane and CEAC."))
        ),
        tabPanel("Trace",br(),
          h4("Markov Trace"),
          p("State occupancy over time. Tunnel states (flare-up states) appear as brief spikes."),
          selectInput("trace_arm","Treatment arm:",
                      choices=c("Lunavar+SoC"="Lunavar","SoC"="SoC","Both"="both"),
                      selected="both"),
          plotOutput("trace_plot",height="550px"),br(),
          downloadButton("download_trace","Download Trace Plot (PNG)")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  model_results <- reactiveVal(NULL)
  output$has_results <- reactive({ !is.null(model_results()) })
  outputOptions(output, "has_results", suspendWhenHidden=FALSE)

  collect_se_overrides <- function() {
    reg <- get_parameter_registry(); overrides <- list()
    for (nm in reg$name) {
      val <- input[[paste0("se_",nm)]]
      if (!is.null(val) && !is.na(val) && val > 0) overrides[[nm]] <- val
    }
    overrides
  }

  observeEvent(input$reset_params, {
    updateNumericInput(session,"aaer_soc",value=1.65)
    updateNumericInput(session,"rr_mild",value=0.45)
    updateNumericInput(session,"rr_moderate",value=0.25)
    updateNumericInput(session,"rr_severe",value=0.18)
    updateSliderInput(session,"prop_mild",value=0.72)
    updateNumericInput(session,"prop_moderate",value=0.12)
    updateNumericInput(session,"prop_severe",value=0.16)
    updateNumericInput(session,"prob_death_severe_flare",value=0.0055)
    updateSliderInput(session,"utility_stable_lunavar",value=0.805)
    updateSliderInput(session,"utility_stable_soc",value=0.760)
    updateNumericInput(session,"disutility_mild",value=0.08)
    updateNumericInput(session,"disutility_moderate",value=0.14)
    updateNumericInput(session,"disutility_severe",value=0.22)
    updateNumericInput(session,"cost_drug_lunavar_annual",value=4800)
    updateNumericInput(session,"cost_drug_soc_annual",value=5200)
    updateNumericInput(session,"cost_mild_event",value=1350)
    updateNumericInput(session,"cost_moderate_event",value=1850)
    updateNumericInput(session,"cost_severe_event",value=8200)
    updateNumericInput(session,"time_horizon",value=50)
    updateNumericInput(session,"starting_age",value=48)
    updateSliderInput(session,"discount_rate",value=3)
    updateNumericInput(session,"wtp",value=100000)
    showNotification("Parameters reset to defaults",type="message")
  })

  observeEvent(input$run, {
    withProgress(message="Running ABD model...",value=0,{
      incProgress(0.15,detail="Building parameters...")
      params <- get_default_parameters()
      params$time_horizon_years  <- input$time_horizon
      params$starting_age        <- input$starting_age
      params$discount_rate       <- input$discount_rate / 100
      params$wtp_threshold       <- input$wtp
      params$aaer_soc            <- input$aaer_soc
      params$rr_mild             <- input$rr_mild
      params$rr_moderate         <- input$rr_moderate
      params$rr_severe           <- input$rr_severe
      params$prop_mild           <- input$prop_mild
      params$prop_moderate       <- input$prop_moderate
      params$prop_severe         <- input$prop_severe
      params$prob_death_severe_flare <- input$prob_death_severe_flare
      params$utility_stable_lunavar  <- input$utility_stable_lunavar
      params$utility_stable_soc      <- input$utility_stable_soc
      params$disutility_mild         <- input$disutility_mild
      params$disutility_moderate     <- input$disutility_moderate
      params$disutility_severe       <- input$disutility_severe
      params$cost_drug_lunavar_annual <- input$cost_drug_lunavar_annual
      params$cost_drug_soc_annual     <- input$cost_drug_soc_annual
      params$cost_mild_event          <- input$cost_mild_event
      params$cost_moderate_event      <- input$cost_moderate_event
      params$cost_severe_event        <- input$cost_severe_event
      n_sim    <- if(input$analysis_type=="psa") input$n_sims else 1
      seed_val <- if(input$analysis_type=="psa" && input$use_seed) input$seed else NULL
      se_overrides <- collect_se_overrides()
      incProgress(0.3,detail="Running model...")
      results <- run_model(parameters=params, n_sim=n_sim, return_trace=TRUE,
                           seed=seed_val,
                           se_overrides=if(length(se_overrides)>0) se_overrides else NULL)
      model_results(results)
      incProgress(1.0,detail="Complete")
    })
    showNotification("Analysis complete!",type="message",duration=3)
  })

  output$icer_table <- if(has_DT) {
    DT::renderDataTable({
      req(model_results())
      tbl <- create_icer_table(model_results()$psa_results)
      cost_cols <- grep("Cost|cost",names(tbl),value=TRUE)
      eff_cols  <- grep("Effect|effect",names(tbl),value=TRUE)
      icer_cols <- grep("ICER|icer",names(tbl),value=TRUE)
      dt <- DT::datatable(tbl,options=list(pageLength=-1,dom="t",paging=FALSE,
                          ordering=TRUE,autoWidth=FALSE),rownames=FALSE)
      if(length(cost_cols)>0)  dt <- DT::formatCurrency(dt,columns=cost_cols,digits=0)
      if(length(eff_cols)>0)   dt <- DT::formatRound(dt,columns=eff_cols,digits=2)
      if(length(icer_cols)>0)  dt <- DT::formatCurrency(dt,columns=icer_cols,digits=0)
      dt
    })
  } else { renderTable({ req(model_results()); create_icer_table(model_results()$psa_results) }) }

  output$ce_plane <- renderPlot({
    req(model_results()); req(input$analysis_type=="psa")
    if(!has_ggplot){plot.new();text(0.5,0.5,"ggplot2 required",cex=1.5);return()}
    plot_ce_plane(model_results()$psa_results, wtp=input$wtp)
  })
  output$ceac_plot <- renderPlot({
    req(model_results()); req(input$analysis_type=="psa")
    if(!has_ggplot){plot.new();text(0.5,0.5,"ggplot2 required",cex=1.5);return()}
    plot_ceac(model_results()$psa_results)
  })
  output$trace_plot <- renderPlot({
    req(model_results())
    if(!has_ggplot){plot.new();text(0.5,0.5,"ggplot2 required",cex=1.5);return()}
    plot_trace(model_results()$trace_data, treatment=input$trace_arm)
  })
  output$download_icer <- downloadHandler(
    filename=function() paste0("ABD_icer_",Sys.Date(),".csv"),
    content=function(file){req(model_results());write.csv(create_icer_table(model_results()$psa_results),file,row.names=FALSE)}
  )
  output$download_psa <- downloadHandler(
    filename=function() paste0("ABD_psa_",Sys.Date(),".png"),
    content=function(file){
      req(model_results())
      png(file,width=12,height=10,units="in",res=300)
      p1 <- plot_ce_plane(model_results()$psa_results,wtp=input$wtp)
      p2 <- plot_ceac(model_results()$psa_results)
      print(p1); print(p2); dev.off()
    }
  )
  output$download_trace <- downloadHandler(
    filename=function() paste0("ABD_trace_",Sys.Date(),".png"),
    content=function(file){
      req(model_results())
      png(file,width=10,height=7,units="in",res=300)
      print(plot_trace(model_results()$trace_data,treatment=input$trace_arm)); dev.off()
    }
  )
}

shinyApp(ui=ui, server=server)
