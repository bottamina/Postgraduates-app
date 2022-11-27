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
               )
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
                                  selected = 801)
             ),
             mainPanel(
               DT::dataTableOutput("table2")
             )
           )
  )
)




server <- function(input, output, session) {
  
  output$table1 <- DT::renderDataTable({
    query <- sqlInterpolate(ANSI(),
                            "SELECT * from postgraduates 
                              WHERE last_year 
                              BETWEEN ?year_min and ?year_max;",
                            year_min = input$year_selector[1],
                            year_max = input$year_selector[2])
    outp <- dbGetQuery(con, query)
    ret <- DT::datatable(outp)
    return(ret)
  })
  output$table2 <- DT::renderDataTable({
    query <- sqlInterpolate(ANSI(),
                            "SELECT * from sotrudniki
                             WHERE department_number IN (?numb);", 
                            numb = input$"checkGroup")
    
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
