library(DBI)
library(RPostgres)
library(DT)
library(shiny)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "d3no3h5gu618lk",
                 host = "ec2-52-31-70-136.eu-west-1.compute.amazonaws.com",
                 user = "xukakqtyxoklsj",
                 password = "0335ee352ff54fa6bc2e80fcd7ae3cb7648546b113fb5923f7ae51fbd52fb5e9",
                 port = "5432"
)

maxmin_pos <- c(dbGetQuery(con,
                           "SELECT MAX(last_year), MIN(last_year) from postgraduates;"))
maxmin_dep <- c(dbGetQuery(con,
                           "SELECT MAX(department_number), MIN(department_number) from sotrudniki;"))
maxmin_hindex <- c(dbGetQuery(con,
                              "SELECT MAX(h_index), MIN(h_index) from dissertations;"))

ui <- navbarPage(
  
  theme = bslib::bs_theme(bootswatch = "minty"),
  "База данных 'Аспиранты'",
  tabPanel("Аспиранты",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_selector","Последний год обучения",       
                           min = maxmin_pos$min,
                           max = maxmin_pos$max,
                           value = c(maxmin_pos$min, maxmin_pos$max),
                           step = 1
               ),
               checkboxGroupInput("checkGroup_pos", label = "Код специальности",
                                  choices = list("01.01.00", "01.02.00", "01.03.00"), 
                                  selected = "01.01.00")
             ),
             mainPanel(
               DT::dataTableOutput("table1")
             )
           )
  ),
  tabPanel("Руководители",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("checkGroup", label = "Кафедры",
                                  choices = list(801, 802, 804, 805, 806), 
                                  selected = 801),
               checkboxGroupInput("checkGroup_sot", label = "Учёная степень",
                                  choices = list("Доктор", "Кандидат"), 
                                  selected = "Доктор")
             ),
             mainPanel(
               DT::dataTableOutput("table2")
             )
           )
  ),
  tabPanel("Предметы",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("checkGroup_sub", label = "Предметы",
                                  choices = list("Экзамен", "Рейтинг", "Зачет"), 
                                  selected = "Экзамен")
             ),
             mainPanel(
               DT::dataTableOutput("table3")
             )
           )
  ),
  tabPanel("Диссертации",
           sidebarLayout(
             sidebarPanel(
               sliderInput("dis_selector","Индекс Хирша",       
                           min = maxmin_hindex$min,
                           max = maxmin_hindex$max,
                           value = c(maxmin_hindex$min, maxmin_hindex$max),
                           step = 1
               )
             ),
             mainPanel(
               DT::dataTableOutput("table4")
             )
           )
  )
)




server <- function(input, output, session) {
  
  output$table1 <- DT::renderDataTable({
    sql_1 <- "SELECT * from postgraduates 
           WHERE last_year 
           BETWEEN ?year_min and ?year_max
           AND id_spec IN (?spec);"
    
    query <- sqlInterpolate(ANSI(), sql_1,
                            year_min = input$year_selector[1],
                            year_max = input$year_selector[2],
                            spec = input$"checkGroup_pos"
    )
    outp <- dbGetQuery(con, query)
    ret <- DT::datatable(outp)
    return(ret)
  })
  output$table2 <- DT::renderDataTable({
    sql_2 <- "SELECT * from sotrudniki
            WHERE department_number IN (?numb)
            AND degree IN (?deg);"
    
    query <- sqlInterpolate(ANSI(), sql_2, 
                            numb = input$"checkGroup",
                            deg = input$"checkGroup_sot")
    
    outp <- dbGetQuery(con, query)
    ret <- DT::datatable(outp)
    return(ret)
  })
  output$table3 <- DT::renderDataTable({
    sql_3 <- "SELECT * from subjects
            WHERE grade_tipe IN (?subj);"
    
    query <- sqlInterpolate(ANSI(), sql_3, 
                            subj = input$"checkGroup_sub")
    
    outp <- dbGetQuery(con, query)
    ret <- DT::datatable(outp)
    return(ret)
  })
  output$table4 <- DT::renderDataTable({
    sql_4 <- "SELECT * from dissertations 
             WHERE h_index 
             BETWEEN ?hindex_min and ?hindex_max;"
    
    query <- sqlInterpolate(ANSI(), sql_4,
                            hindex_min = input$dis_selector[1],
                            hindex_max = input$dis_selector[2])
    outp <- dbGetQuery(con, query)
    ret <- DT::datatable(outp)
    return(ret)
  })
}

shinyApp(ui = ui, server = server)

# library(shiny)
# 
# ui <- navbarPage(
#   "Page title",
#   tabPanel("panel 1", "one"),
#   tabPanel("panel 2", "two"),
#   tabPanel("panel 3", "three"),
#   navbarMenu("subpanels",
#              tabPanel("panel 4a", "four-a"),
#              tabPanel("panel 4b", "four-b"),
#              tabPanel("panel 4c", "four-c")
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
#fhfhyfhufhuf
