# make more variation between months in spending
# make black line show in legend for net balance

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(cowplot)
library(bslib)
library(thematic)
library(markdown)
library(shinycssloaders)
library(RColorBrewer)


# apply thematic over ggplot in server code
thematic_shiny(
  bg = "auto",
  fg = "auto",
  accent = "auto",
  font = "auto",
  sequential = sequential_gradient(),
  qualitative = okabe_ito(),
  inherit = FALSE,
  session = shiny::getDefaultReactiveDomain()
)
# UI ----
ui <- navbarPage(
  
  fluid = TRUE,
  
  theme = bs_theme(version = 4, bootswatch = "pulse"),
  
  titlePanel("Personal Budget Sheet"),
  fluidRow(
  plotOutput("distPlot")
  ),
  
  hr(),
  
  fluidRow(
    column(3,
           fileInput(
             inputId = "csv", 
             label = "Upload excel/csv file", 
             accept = ".csv" 
           ),
           selectInput(
             inputId = "assignby",
             label = "Assign by",
             choices = c(Choose = ' ', "Keyword & Amount" = 1, "ID" = 2),
             selected = NULL
           ),
           conditionalPanel(
             condition = "input.assignby == 1",
             textInput(
               inputId = "keyword1",
               label = "Keyword"
             ),
             sliderInput(
               inputId = "amountrange",
               label = "Amount range",
               min = -10000,
               max = 10000,
               value = c(-10000, 10000)
             ),
             textInput(
               inputId = "category1",
               label = "Category"
             ),
             actionButton(
               inputId = "assign1",
               label = "Assign category"
             ),
             actionButton(
               inputId = "unassign1",
               label = "Unassign category"
             )
           ),
           conditionalPanel(
             condition = "input.assignby == 2",
             textInput(
               inputId = "id",
               label = "ID"
             ),
             textInput(
               inputId = "category2",
               label = "Category"
             ),
             actionButton(
               inputId = "assign2",
               label = "Assign category"
             ),
             actionButton(
               inputId = "unassign2",
               label = "Unassign category"
             )
           ),
           p(),
           selectInput(
             inputId = "filterby",
             label = "Filter by",
             choices = c(Choose = ' ', "Date" = 1, "Transaction type" = 2, "Keyword" = 3, "Amount" = 4, "Category" = 5)
           ),
           conditionalPanel(
             condition = "input.filterby == 1",
             dateRangeInput(
               inputId = "daterange",
               label = "Daterange",
               start = "2020/8/1",
               end = "2020/12/31"
             )
           ),
           conditionalPanel(
             condition = "input.filterby == 2",
             selectInput(
               inputId = "transaction",
               label = "Transaction",
               choices = c("Deposit", "Withdrawal"),
               multiple = T,
               selected = c("Deposit", "Withdrawal")
             )
           ),
           conditionalPanel(
             condition = "input.filterby == 3",
             textInput(
               inputId = "keyword2",
               label = "Keyword"
             )
           ),
           conditionalPanel(
             condition = "input.filterby == 4",
             numericInput(
               inputId = "min",
               label = "Min",
               value = -10000,
               step = 50
             ),
             numericInput(
               inputId = "max",
               label = "Max",
               value = 10000,
               step = 50
             ),
           ),
           conditionalPanel(
             condition = "input.filterby == 5",
             selectInput(
               inputId = "category3",
               label = "Category",
               choices = "",
               multiple = T,
               selected = ""
             )
           ),
           numericInput(
             inputId = "limit",
             label = "Enter monthly budget",
             value = 1500
           )
    ),
    column(3, div(DT::dataTableOutput("distTable"), style = "height:400px; width: 800px; overflow-y: scroll; font-size = 75%"), offset = 0.9)
  )
)


# Server ----
server <- function(input, output, session) {
  
  
  budget <- tibble(
    date = as.Date(character()),
    transaction = character(),
    name = character(),
    amount = numeric(),
    category = character()
  )

  values <- reactiveValues(budget = NULL)
  
  if (file.exists("budget.rds")) 
  {values$budget <- read_rds("budget.rds")} 
  
  observeEvent(input$csv,{
    file <- input$csv
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    values$budget <-   read_csv(file$datapath) %>% 
      janitor::clean_names() %>% 
      select(-memo) %>% 
      mutate(category = {if("category" %in% names(.)) category else "Unassigned"})
  })
  
  
  observeEvent(input$assign1, {
    temp <- values$budget %>% 
      mutate(
        category = case_when(
          str_detect(name, str_replace(input$keyword1, ",", "|")) & between(amount, input$amountrange[1], input$amountrange[2]) ~ input$category1,
          TRUE ~ category,
          # record string & amount range 
        )
      )
    values$budget <- temp
  })
  
  observeEvent(input$unassign1, {
    temp <- values$budget %>% 
      mutate(
        category = case_when(
          str_detect(name, str_replace(input$keyword1, ",", "|")) & between(amount, input$amountrange[1], input$amountrange[2])  ~ "Unassigned",
          TRUE ~ category
        )
      )
    values$budget <- temp
  })
  
  dat <- reactive({
    values$budget %>%
      mutate(
        category = if_else(is.na(category), "Unassigned", category)
      ) %>%
      arrange(category != "Unassigned", category)
  })
  
  observe({
    updateSelectInput(session, "category3",
                      choices = dat()$category,
                      selected = dat()$category
    )})
  
  
  output$distTable <- DT::renderDT({
    dat() %>% 
      mutate(transaction = case_when(
        amount > 0 ~ "Deposit",
        amount < 0 ~ "Withdrawal"
      )) %>%
      DT::datatable(options = list(paging = FALSE))  
      # DT::formatStyle(names(dat()), lineHeight='80%') 
  })  
  output$distPlot <- renderPlot({
    
      Withdrawal_dat <- dat() %>% 
        mutate(
          transaction = case_when(
            amount > 0 ~ "Deposit",
            amount < 0 ~ "Withdrawal"
          ),
        ) %>%
        filter(transaction == "Withdrawal") %>% 
        filter(date > input$daterange[1], date < input$daterange[2]) %>%
        filter(transaction %in% input$transaction) %>%
        # filter(keyword == str_detect(name, input$keyword2)) %>%
        filter(between(amount, input$min, input$max)) %>%
        filter(category %in% input$category3) 
      
      Deposit_dat <- dat() %>% 
        mutate(
          transaction = case_when(
            amount > 0 ~ "Deposit",
            amount < 0 ~ "Withdrawal"
          ),
        ) %>%
        filter(transaction == "Deposit") %>% 
        filter(date > input$daterange[1], date < input$daterange[2]) %>%
        filter(transaction %in% input$transaction) %>%
        # filter(keyword == str_detect(name, input$keyword2)) %>%
        filter(between(amount, input$min, input$max)) %>%
        filter(category %in% input$category3) 
      
      
      color_pallete_function <- colorRampPalette(
        colors = palette(),
        space = "Lab")
      
      num_colors <- nlevels(as.factor(dat()$category))
      colors_pal <- color_pallete_function(num_colors)
      colors_pal <-setNames(colors_pal, levels(as.factor(dat()$category)))

      
      
      main <- dat() %>% 
        mutate(
          transaction = case_when(
            amount > 0 ~ "Deposit",
            amount < 0 ~ "Withdrawal"
          ),
        ) %>%
        filter(date > input$daterange[1], date < input$daterange[2]) %>%
        filter(transaction %in% input$transaction) %>%
        # filter(keyword == str_detect(name, input$keyword2)) %>%
        filter(between(amount, input$min, input$max)) %>%
        filter(category %in% input$category3) %>% 
        mutate(limit = input$limit) %>% 
        ggplot(aes(ym(str_sub(date, 1, 7)) + 6, abs(amount), fill = category)) +
        geom_col(
          data = Withdrawal_dat,
          position = "stack",
          width = 10,
          alpha = 0.55,
          color = "orange",
          size = 0.2
        ) +
        geom_col(
          data = Deposit_dat,
          aes(ym(str_sub(date, 1, 7)) - 6),
          position = "stack",
          width = 10,
          alpha = 0.55,
          color = "blue",
          size = 0.2
        ) +
        geom_line(
          aes(ym(str_sub(date, 1, 7)) + 11, as.numeric(limit)),
          color = "red",
          size = 0.33
        ) +
        geom_line(
          aes(ym(str_sub(date, 1, 7)) - 11, as.numeric(limit)),
          color = "red",
          size = 0.33
          ) +
        annotate(
          label = "monthly budget",
          geom = "text",
          color = "red",
          fontface = 'italic',
          y = input$limit,
          vjust = -1,
          x = input$daterange[1]
        ) +
        scale_fill_manual(values = colors_pal, drop = FALSE) +
        scale_y_continuous(
          expand = expansion(c(0, 0.1))
        ) +
        scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
        coord_cartesian(xlim = c(ym(str_sub(input$daterange[1], 1, 7)) - 6, ym(str_sub(input$daterange[2], 1, 7)) + 6)) +
        labs(
          fill = "Category",
          title = "Monthly comparison of deposits and withdrawals"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_blank(),
          legend.direction = "vertical"
          )
      
      
      
      main2 <- dat() %>% 
        mutate(
          transaction = case_when(
            amount > 0 ~ "Deposit",
            amount < 0 ~ "Withdrawal"
          ),
        ) %>%
        filter(date > input$daterange[1], date < input$daterange[2]) %>%
        filter(transaction %in% input$transaction) %>%
        # filter(keyword == str_detect(name, input$keyword2)) %>%
        filter(between(amount, input$min, input$max)) %>%
        filter(category %in% input$category3) %>% 
        mutate(limit = input$limit) %>% 
        arrange(date) %>% 
        group_by(transaction) %>%
        mutate(amount_transaction = cumsum(amount)) %>% 
        ungroup() %>%
        ggplot(aes(ymd(date), abs(amount_sum))) +
        geom_line(aes(y = abs(amount_transaction), color = transaction)) +
        scale_color_manual(values = c("blue", "orange")) +
        scale_y_continuous(
          expand = expansion(c(0, 0.1))
        ) +
        scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
        coord_cartesian(xlim = c(ym(str_sub(input$daterange[1], 1, 7)) - 6, ym(str_sub(input$daterange[2], 1, 7)) + 6)) +
        labs(
          title = "Cumulative deposits and withdrawals over time",
          color = "Transaction"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold"),
          axis.title = element_blank(),
          legend.direction = "vertical"
        )
     
    Withdrawal <- Withdrawal_dat %>% 
      mutate(dummy = "dummy") %>% 
      ggplot(aes(dummy, amount, fill = category)) +
      geom_col(
        position = "stack",
        alpha = 0.55,
        color = "orange",
        size = 0.1
        ) + 
      coord_polar("y", start=0) +
      labs(
        # title = "Withdrawal",
        x = NULL,
        y = NULL,
        fill = "Category"
      ) +
      scale_fill_manual(values = colors_pal, drop = FALSE) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid=element_blank(),
        legend.position = "none"
      )
    
    Deposit <- Deposit_dat %>% 
      mutate(dummy = "dummy") %>% 
      ggplot(aes(dummy, amount, fill = category)) +
      geom_col(
        position = "stack",
        alpha = 0.55,
        color = "blue",
        size = 0.1
        ) + 
      coord_polar("y", start=0) +
      labs(
        title = "Proportions of spending by category",
        x = NULL,
        y = NULL,
        fill = "Category"
      ) +
      scale_fill_manual(values = colors_pal, drop = FALSE) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid=element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
    # lemon::grid_arrange_shared_legend(main, gridExtra::arrangeGrob(Withdrawal, Deposit, ncol = 1),
    #                                   position = "right")
    
    title_text <- paste(format(ymd(input$daterange[1]), "%b %y"), format(ymd(input$daterange[2]), "%b %y"), sep = " - ")
    
     main + (Deposit + Withdrawal) / main2 + guide_area() +
      plot_layout(guides = 'collect') +
      plot_annotation(
        title = title_text,
        theme = theme(plot.title = element_text(size = 16, face = "bold"))
      )
    })
  
  save_function <- session$onSessionEnded(function() {
    isolate(write_rds(values$budget, "budget.rds"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
