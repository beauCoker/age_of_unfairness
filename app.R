library(shiny)
library(tidyverse)
library(lubridate)

load("Table_construction.Rdata")  ## You can comment out if data already loaded. The app will load faster.

date_data_pulled = ymd("2016-03-30") ### HARDCODED. ADUST IF NEW DATASET
  
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Recidivism Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
     # span(textOutput("message"), style="color:red"),
      
      actionButton("go", label = "Update"),
      
      numericInput("person_id", label = h3("Input Person ID"), value = 1),
      
      selectInput("screening_date", label = h3("Input Screening Date"), 
                  choices = NULL),
      
      h3("Summary"),
      
      tableOutput("info")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(
        id = 'dataset',
        tabPanel("Charge",
                 h4("Before Current Offense Date"),
                 DT::dataTableOutput("charge_before"),
                 h4("On Current Offense Date"),
                 DT::dataTableOutput("charge_on"),
                 h4("After Current Offense Date"),
                 DT::dataTableOutput("charge_after")
        ),
        
        tabPanel("Arrest",
                 h4("Before Current Offense Date"),
                 DT::dataTableOutput("arrest_before"),
                 h4("On Current Offense Date"),
                 DT::dataTableOutput("arrest_on"),
                 h4("After Current Offense Date"),
                 DT::dataTableOutput("arrest_after")
        ),
        
        tabPanel("Jail",
                 h4("Before Current Offense Date"),
                 DT::dataTableOutput("jail_before"),
                 h4("On Current Offense Date"),
                 DT::dataTableOutput("jail_on"),
                 h4("After Current Offense Date"),
                 DT::dataTableOutput("jail_after")
        ),
        
        tabPanel("Prison",
                 h4("Before Current Offense Date"),
                 DT::dataTableOutput("prison_before"),
                 h4("On Current Offense Date"),
                 DT::dataTableOutput("prison_on"),
                 h4("After Current Offense Date"),
                 DT::dataTableOutput("prison_after")
        ),
        
        tabPanel("Probation",
                 h4("Before Current Offense Date"),
                 DT::dataTableOutput("prob_before"),
                 h4("On Current Offense Date"),
                 DT::dataTableOutput("prob_on"),
                 h4("After Current Offense Date"),
                 DT::dataTableOutput("prob_after")
        ),
        
        tabPanel("Profile",
                 h4("Profile"),
                 tableOutput("profile")
        ),
        
        
        tabPanel("Features",tableOutput("features")),
        tabPanel("COMPAS",tableOutput("compas"))
      )
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  
  ### Update the screening_date dropdown
  observeEvent(input$person_id,{
    
    if(!is.na(input$person_id)){
      updateSelectInput(session, "screening_date", 
                        choices =  features$screening_date[features$person_id == input$person_id])
    }
    
    ## Set all tables to NULL
    
    # Info
    output$info = renderTable(NULL)
    
    # Charge
    output$charge_before <- DT::renderDataTable(NULL)
    output$charge_on = DT::renderDataTable(NULL)
    output$charge_after = DT::renderDataTable(NULL)
    
    # Arrest
    output$arrest_before <- DT::renderDataTable(NULL)
    output$arrest_on = DT::renderDataTable(NULL)
    output$arrest_after = DT::renderDataTable(NULL)
    
    # Jail
    output$jail_before <- DT::renderDataTable(NULL)
    output$jail_on = DT::renderDataTable(NULL)
    output$jail_after = DT::renderDataTable(NULL)
    
    # Prison
    output$prison_before <- DT::renderDataTable(NULL)
    output$prison_on = DT::renderDataTable(NULL)
    output$prison_after = DT::renderDataTable(NULL)
    
    # Probation
    output$prob_before <- DT::renderDataTable(NULL)
    output$prob_on = DT::renderDataTable(NULL)
    output$prob_after = DT::renderDataTable(NULL)
    
    # Features
    output$features = renderTable(NULL)
    
    # Profile
    output$profile = renderTable(NULL)
    
    # COMPAS
    output$compas = renderTable(NULL)
    
    output$message = renderText("Hit Update")
  })
  
  
  observeEvent(input$go,{

    output$message = renderText("-")
    
    isolate({
      
      ## Info
      output$info <- renderTable({
        person = data_before %>%
          filter(person_id == input$person_id, screening_date == as_date(input$screening_date))
        
        bind_cols(
          person %>%
            select(people) %>%
            unnest() %>%
            select(name,sex,race),
          
          person %>%
            select(first_offense_date, current_offense_date),
          
          compas_df_wide %>% 
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(`General Decile Score` = `Risk of Recidivism_decile_score`,
                   `Violence Decile Score` = `Risk of Violence_decile_score`),
          
          outcomes %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(recid, recid_violent)
          
        ) %>%
          t()
      },colnames = FALSE, rownames = TRUE)

      
      ## Charge tab
      output$charge_before <- DT::renderDataTable({
        
        DT::datatable({
          
          data_before %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(charge) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))}
      )
      
      
      output$charge_on <- DT::renderDataTable(
        DT::datatable({
          data_on %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(charge) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      
      output$charge_after <- DT::renderDataTable(
        DT::datatable({
          data_after %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(charge) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      ## Arrest tab
      output$arrest_before <- DT::renderDataTable({
        DT::datatable({
          data_before %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(arrest) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))}
      )
      
      
      output$arrest_on <- DT::renderDataTable(
        DT::datatable({
          data_on %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(arrest) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      
      output$arrest_after <- DT::renderDataTable(
        DT::datatable({
          data_after %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(arrest) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      ## Jail tab
      output$jail_before <- DT::renderDataTable({
        DT::datatable({
          data_before %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(jail) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))}
      )
      
      
      output$jail_on <- DT::renderDataTable(
        DT::datatable({
          data_on %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(jail) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      
      output$jail_after <- DT::renderDataTable(
        DT::datatable({
          data_after %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(jail) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      ## Prison tab
      output$prison_before <- DT::renderDataTable({
        DT::datatable({
          data_before %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(prison) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))}
      )
      
      
      output$prison_on <- DT::renderDataTable(
        DT::datatable({
          data_on %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(prison) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      
      output$prison_after <- DT::renderDataTable(
        DT::datatable({
          data_after %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(prison) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      ## Probation tab
      output$prob_before <- DT::renderDataTable({
        DT::datatable({
          data_before %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(prob) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))}
      )
      
      
      output$prob_on <- DT::renderDataTable(
        DT::datatable({
          data_on %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(prob) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      
      output$prob_after <- DT::renderDataTable(
        DT::datatable({
          data_after %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(prob) %>%
            .[[1,1]]}, 
          options = list(paging = FALSE))
      )
      
      ## Features
      output$features <- renderTable({
        features %>%
          filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
          select(-person_id, -screening_date, -first_offense_date, -current_offense_date,
                 -`Risk of Failure to Appear_decile_score`,-`Risk of Failure to Appear_raw_score`,
                 -`Risk of Recidivism_decile_score`,-`Risk of Recidivism_raw_score`,
                 -`Risk of Violence_decile_score`,-`Risk of Violence_raw_score`) %>%
          mutate_all(as.character) %>%
          t()
      },
      colnames = FALSE, rownames = TRUE)
      
      ## COMPAS
      output$compas <- renderTable({
        compas_df_wide %>%
          filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
          select(-person_id, -screening_date) %>%
          mutate_all(as.character) %>%
          t()
      },
      colnames = FALSE, rownames = TRUE)
      
      ## Profile
      output$profile <- renderTable({
        
        features_person = features %>%
          filter(person_id == input$person_id, screening_date == as_date(input$screening_date))
        
        person_before = data_before %>%
          filter(person_id == input$person_id, screening_date == as_date(input$screening_date))
        
        
        charge_onbefore = bind_rows(
          person_before %>%
            select(charge) %>%
            .[[1,1]],
          data_on %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(charge) %>%
            .[[1,1]]
        )
        
        if(nrow(charge_onbefore)>0){
          charge_onbefore_sum =  charge_onbefore %>%
            select(charge, charge_degree) %>%
            mutate(charge_degree_letters = str_extract(charge_degree,"[:alpha:]+")) %>%
            group_by(charge, charge_degree_letters) %>%
            summarize(count = n()) %>%
            ungroup() %>%
            arrange(desc(count)) %>%
            summarize(charge_all = paste(pmap_chr(list(charge, charge_degree_letters, count), 
                                                  function(charge, charge_degree_letters, count) 
                                                  {paste0(charge," (",charge_degree_letters,",",count,")")} ), collapse=', '))
        } else {
          charge_onbefore_sum = data.frame(priors = NA)
        }
        
        
        charge_after = data_after %>%
          filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
          select(charge) %>%
          .[[1,1]]
        
        if(!is.null(charge_after)){
          charge_after_sum =  charge_after %>%
            mutate(new = paste0("(",str_extract(charge_degree,"[:alpha:]+"),",",str_extract(charge_degree,"[:digit:]+"),")")) %>%
            summarize(charges_after_screening = paste(map2_chr(charge, new, paste), collapse=', '))
        } else {
          charge_after_sum = data.frame(charges_after_screening = NA)
        }
        
        if(nrow(charge_onbefore>0) & !is.null(charge_after)){ 
          charges_both = data.frame(charges_both=paste(base::intersect(charge_onbefore$charge, charge_after$charge), collapse=", "))
        } else {
          charges_both = data.frame(charges_both=NA)
        }
        
        
        bind_cols(
          person_before %>%
            select(people) %>%
            unnest() %>%
            select(name,sex,race),
          
          person_before %>%
            select(first_offense_date, current_offense_date),
          
          compas_df_wide %>% 
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(`General Decile Score` = `Risk of Recidivism_decile_score`,
                   `Violence Decile Score` = `Risk of Violence_decile_score`),
          
          outcomes %>%
            filter(person_id == input$person_id, screening_date == as_date(input$screening_date)) %>%
            select(recid, recid_violent),
          
          days_recid_info = as.numeric(as.period(interval(person_before$screening_date,date_data_pulled)), "days"),
          
          data.frame(p_charge = features_person$p_charge), 
          
          charge_onbefore_sum,
          
          charge_after_sum,
          
          charges_both
          
        ) %>%
          t()
      },colnames = FALSE, rownames = TRUE)
      
      
      
    })
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)
