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
maxmin_ids <- c(dbGetQuery(con,
                           "SELECT MAX(id_sotr), MIN(id_sotr) from sotrudniki;"))
maxmin_idp <- c(dbGetQuery(con,
                           "SELECT MAX(id_postgr), MIN(id_postgr) from postgraduates;"))

ui <- navbarPage(
  
  theme = bslib::bs_theme(bootswatch = "minty"),
  "База данных 'Аспиранты'",
  tabPanel("Аспиранты",
           sidebarLayout(
             sidebarPanel(
               sliderInput("year_selector", "Последний год обучения",       
                           min = maxmin_pos$min,
                           max = maxmin_pos$max,
                           value = c(maxmin_pos$min, maxmin_pos$max),
                           step = 1
               ),
               
               selectInput("checkGroup_pos", label = "Код специальности",
                           choices = list("Все специальности" = 1, "01.01.00", "01.02.00", "01.03.00"), 
                           selected = 1),
               sliderInput("depsl", "Кафедра научного руководителя",
                           min = maxmin_dep$min,
                           max = maxmin_dep$max,
                           value = c(maxmin_dep$min, maxmin_dep$max),
                           step = 1),
               sliderInput("hind", label = "h_index работ аспирантов", 
                           min = maxmin_hindex$min,
                           max = maxmin_hindex$max,
                           value = c(maxmin_hindex$min, maxmin_hindex$max),
                           step = 5),
               sliderInput("sotrid", label = "id сотрудников",
                           min = maxmin_ids$min,
                           max = maxmin_ids$max,
                           value = c(maxmin_ids$min, maxmin_ids$max),
                           step = 2),
               sliderInput("postid", label = "id аспирантов",
                           min = maxmin_idp$min,
                           max = maxmin_idp$max,
                           value = c(maxmin_idp$min, maxmin_idp$max),
                           step = 2)
             ),
             mainPanel(
               DT::dataTableOutput("table1")
             )
           )
  ),
  tabPanel("Руководители",
           sidebarLayout(
             sidebarPanel(
               sliderInput("checkGroup", label = "Номера кафедр",
                           min = maxmin_dep$min,
                           max = maxmin_dep$max,
                           value = c(maxmin_dep$min, maxmin_dep$max),
                           step = 1),
               selectInput("checkGroup_sot", label = "Названия кафедр",
                           choices = list("Все кафедры" = 1, "Физика",
                                          "Компьютерные науки и информатика",
                                          "Математическая кибернетика",
                                          "Теория вероятностей и компьютерное моделирование",
                                          "Мехатроника и теоретическая механика"), 
                           selected = 1),
             ),
             mainPanel(
               DT::dataTableOutput("table2")
             )
           )
  ),
  tabPanel("Предметы",
           sidebarLayout(
             sidebarPanel(
               selectInput("checkGroup_sub", label = "Предметы",
                                  choices = list("Все типы" = 1, "Экзамен", "Рейтинг", "Зачет"), 
                                  selected = 1)
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
    
    sql_1 <- "SELECT p.id_postgr, p.name, p.last_year, p.id_spec, sw.work_name, sw.h_index, s.id_sotr, s.name, s.email, s.department_number
    from postgraduates AS p
            JOIN sotrudniki AS s
           ON s.id_sotr = p.id_nauch
           JOIN dissertations AS sw
           ON sw.id_author = p.id_postgr 
           WHERE last_year 
           BETWEEN ?year_min and ?year_max
           AND id_spec IN (?spec1)
           AND (s.department_number >= ?dep_min AND s.department_number <= ?dep_max)
           AND (sw.h_index >= ?hmin AND sw.h_index <= ?hmax)
           AND (p.id_postgr >= ?ipmin AND p.id_postgr <= ?ipmax)
           AND (s.id_sotr >= ?ismin AND s.id_sotr <= ?ismax)
           ORDER BY p.name;"
    
    sql_11 <- "SELECT p.id_postgr, p.name, p.last_year, p.id_spec, sw.work_name, sw.h_index, s.id_sotr, s.name, s.email, s.department_number
    from postgraduates AS p
            JOIN sotrudniki AS s
           ON s.id_sotr = p.id_nauch
           JOIN dissertations AS sw
           ON sw.id_author = p.id_postgr 
           WHERE last_year 
           BETWEEN ?year_min and ?year_max
           AND (s.department_number >= ?dep_min AND s.department_number <= ?dep_max)
           AND (sw.h_index >= ?hmin AND sw.h_index <= ?hmax)
           AND (p.id_postgr >= ?ipmin AND p.id_postgr <= ?ipmax)
           AND (s.id_sotr >= ?ismin AND s.id_sotr <= ?ismax)
           ORDER BY p.name;"
    if (input$checkGroup_pos == 1) 
      query <- sqlInterpolate(ANSI(), sql_11,
                              year_min = input$year_selector[1],
                              year_max = input$year_selector[2],
                              dep_min = input$depsl[1],
                              dep_max = input$depsl[2],
                              hmin = input$hind[1],
                              hmax = input$hind[2],
                              ipmin = input$postid[1],
                              ipmax = input$postid[2],
                              ismin = input$sotrid[1],
                              ismax = input$sotrid[2])
    
    else {query <- sqlInterpolate(ANSI(), sql_1,
                                  year_min = input$year_selector[1],
                                  year_max = input$year_selector[2],
                                  spec1 = input$checkGroup_pos,
                                  dep_min = input$depsl[1],
                                  dep_max = input$depsl[2],
                                  hmin = input$hind[1],
                                  hmax = input$hind[2],
                                  ipmin = input$postid[1],
                                  ipmax = input$postid[2],
                                  ismin = input$sotrid[1],
                                  ismax = input$sotrid[2])
    }
    outp <- dbGetQuery(con, query)
    ret <- DT::datatable(outp)
    return(ret)
  })
  output$table2 <- DT::renderDataTable({
    sql_2 <- "SELECT * from sotrudniki
            WHERE department_number
            BETWEEN ?numb_min and ?numb_max
            AND department_name IN (?kaf);"
    
    if (input$checkGroup_sot == 1)
      query <- sqlInterpolate(ANSI(), "SELECT * from sotrudniki;")
    
    else {query <- sqlInterpolate(ANSI(), sql_2, 
                                  numb_min = input$checkGroup[1],
                                  numb_max = input$checkGroup[2],
                                  kaf = input$checkGroup_sot)
    }
    
    outp <- dbGetQuery(con, query)
    ret <- DT::datatable(outp)
    return(ret)
  })
  output$table3 <- DT::renderDataTable({
    sql_3 <- "SELECT * from subjects
            WHERE grade_tipe IN (?subj);"
    
    if (input$checkGroup_sub == 1)
      query <- sqlInterpolate(ANSI(), "SELECT * from subjects;")
    
    else {query <- sqlInterpolate(ANSI(), sql_3, 
                                  subj = input$checkGroup_sub)
      }
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
