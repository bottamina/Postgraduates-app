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
