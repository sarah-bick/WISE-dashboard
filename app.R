library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(googlesheets4)
library(dashboardthemes)

library(googledrive)
library(googlesheets4)

# Google sheets authentification -----------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "sbick95@gmail.com")
gs4_auth(token = drive_token())

# gs4_auth(email = "sbick95@gmail.com", use_oob = TRUE, token = "4/1AX4XfWj7ei0lh_Fu3VO7g5Kr9X5Du3wvW5OlGL-uDGYcY8VORFthixz1dQs")

subs_volume <- function(submissions, by = "day") {
  submissions %>%
    mutate(by_day = lubridate::floor_date(survey_time, by)) %>%
    group_by(by_day, type, .add = TRUE) %>%
    add_count() %>%
    mutate(id=row_number()) %>%
    # drop by_day group
    # {
    #   if (!is.null(groups(submissions))) {
    #     group_by(., !!!groups(submissions))
    #   } else ungroup(.)
    # } %>%
    ungroup() %>%
    filter(id==1) %>%
    # fill missing time units
    complete(by_day = seq(ymd(min(by_day)), ymd(today()), by), fill = list(n = 0)) %>%
    spread(type, n, fill = 0) %>%
    arrange(by_day) %>%
    mutate(cum_sch = cumsum(school_enr)
           # , cum_stu = cumsum(student_enr), cum_fol = cumsum(follow_up)
           )
}


COLOURS <- list(
  "light-blue" = "#6699CC",
  "green"      = "#99C794",
  "red"        = "#EC5f67",
  "purple"     = "#C594C5",
  "aqua"       = "#a3c1e0",
  "yellow"     = "#FAC863",
  "navy"       = "#343D46",
  "olive"      = "#588b8b",
  "blue"       = "#4080bf",
  "orange"     = "#F99157",
  "teal"       = "#5FB3B3",
  "fuchsia"    = "#aa62aa",
  "lime"       = "#b0d4b0",
  "maroon"     = "#AB7967",
  "black"      = "#1B2B34",
  "gray-lte"   = "#D8DEE9",
  "primary"    = "#6699CC",
  "success"    = "#99C794",
  "danger"     = "#EC5f67",
  "info"       = "#a3c1e0",
  "warning"    = "#FAC863"
)

sch_enrolment <- as_tibble(read_sheet("https://docs.google.com/spreadsheets/d/14j-Gki56s9oTJZUcE-hPpTdw3MVf_KMIFjMOHxYoLD0/edit?usp=sharing"))
stu_enrolment <- as_tibble(read_sheet("https://docs.google.com/spreadsheets/d/1p_A0QBNKPrdYj-OPF-kxjtfP2DwfLYeK2aJD-zo8ydA/edit?usp=sharing"))
stu_followup <- as_tibble(read_sheet("https://docs.google.com/spreadsheets/d/1XNjQ5PndGMmBBJuTarxR5Ps_54lr0qQjkw3VGCT9cQw/edit?usp=sharing"))
# sch_enrolment <- as_tibble(read.csv("test_school_enrolment.csv"))
# stu_enrolment <- as_tibble(read.csv("test_student_enrolment.csv"))
# stu_followup <- as_tibble(read.csv("test_follow_up.csv"))

sch_enrolment %>%
  select(SubmissionDate:endtime, siteID, subcity, woreda) %>%
  mutate(survey_time = as_datetime(endtime), type = "school_enr") -> sch_enrolment_subs

stu_enrolment %>%
  select(SubmissionDate:endtime, siteID, classID, studentID, assent) %>%
  filter(assent == 1) %>%
  mutate(survey_time = as_datetime(endtime), type = "student_enr") %>%
  distinct(siteID, .keep_all = TRUE) %>%
  mutate_if(is.logical, as.character) -> stu_enrolment_subs

stu_enrolment %>%
  select(SubmissionDate:endtime, siteID, classID, studentID, assent) %>%
  filter(assent == 1) %>%
  mutate(survey_time = as_datetime(endtime), type = "student_enr") %>%
  mutate_if(is.logical, as.character) -> stu_enrolment_all

stu_followup %>%
  select(SubmissionDate:endtime, siteID, classID, follow_up_num, follow_upID) %>%
  mutate(survey_time = as_datetime(endtime), type = "follow_up") %>%
  distinct(siteID, follow_up_num, .keep_all = TRUE)  %>%
  mutate_if(is.logical, as.character) -> stu_followup_subs

bind_rows(sch_enrolment_subs, stu_enrolment_subs, stu_followup_subs) -> submissions

# submissions %>%
#   #       tweets_just(created_at, is_topic) %>%
#   #       group_by(is_topic) %>%
#   subs_volume() %>%
#   #       mutate(topic = if_else(is_topic, "topic", "all")) %>%
#   ungroup() %>%
#   rename(Date = by_day) %>%
#   #       select(-is_topic) %>%
#   spread(type, n, fill = 0) %>%
#   add_row(Date = as.POSIXct("2021-05-25"), school_enr = 1, student_enr = 0, follow_up = 0)
#   arrange(Date)
# data$survey_date <- as.Date(data$endtime)
# data$survey_datetime <- as_datetime(data$endtime)



# subs_volume(submissions) -> new

  # ggplot(data, aes(x=survey_date))+
  #   geom_density(color="darkblue", fill="lightblue")

ui <- dashboardPage(
  title = "WISE evaluation dashboard",
  skin = 'black',
  # sidebar_mini = TRUE,
  dashboardHeader(title = HTML(glue::glue(
    '<span class="logo-mini">{"<em>WISE&nbsp;</em><strong>dash</strong>"}</span>
      <span class="logo-lg">{"<em>WISE&nbsp</em><strong>dashboard</strong>"}</span>'
  ))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Older Children", tabName = "tab_dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "ocean-next/AdminLTE.css")
    ),
    tabItems(
      tabItem(
        "tab_dashboard",
        tags$head(
          #       # Metadata <head> ---------------------------------------------------------
          HTML(glue::glue(
            '<meta property="og:title" content="{"WISE evaluation dashboard"}">
            <meta property="og:description" content="{"A Shiny Dashboard for monitoring the WISE evaluation"}">
            <meta property="og:url" content="{"https://WISE-evaluation.shinyapps.io/dashboard/"}">
            <meta property="og:image" content="{"https://garrickadenbuie.com/images/2019/rstudioconf-2019-icon.png"}">
            <meta name="msapplication-TileColor" content="#6699CC">
            <meta name="msapplication-TileImage" content="ms-icon-144x144.png">
            <meta name="theme-color" content="#6699CC">
            '
          ))
          # Metadata <head> end -----------------------------------------------------
        ),
        fluidRow(
          valueBoxOutput(
            outputId = "total_sch_enrol",
            # "—", "School Enrolment — schools",
            # color = "purple",
            # icon = icon("comment-dots"),
            width = 3),
          valueBoxOutput(
            outputId = "total_stu_enrol",
            # "—", "School Enrolment — schools",
            # color = "purple",
            # icon = icon("comment-dots"),
            width = 3),
          valueBoxOutput(
              outputId = "total_follow",
              # "—", "School Enrolment — schools",
              # color = "purple",
              # icon = icon("comment-dots"),
              width = 3),
          valueBoxOutput(
            outputId = "total_final",
            # "—", "School Enrolment — schools",
            # color = "purple",
            # icon = icon("comment-dots"),
            width = 3)
        ),
        fluidRow(
          tabBox(
            width=12,
            tabPanel(
              status = "primary",
              title = "Schools (daily)",
              withSpinner(plotlyOutput("plot_schools", height = "400px"))
            ),
            tabPanel(
              status = "primary",
              title = "Schools (cumulative)",
              withSpinner(plotlyOutput("plot_schools_cum", height = "400px"))
            ),
            tabPanel(
              status = "primary",
              title = "Students",
              withSpinner(plotlyOutput("plot_students", height = "400px"))
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {

  output$total_sch_enrol <- renderValueBox({
    valueBox(
      paste0((sch_enrolment_subs %>%
          count() %>%
          pull(n) %>%
          format(big.mark = ",", digits = 0)
      ), "/60"),
      " schools completed school enrolment",
      icon = icon("school"),
      color = "purple"
    )
  })

  output$total_stu_enrol <- renderValueBox({
    valueBox(
      paste0((stu_enrolment_subs %>%
                count() %>%
                pull(n) %>%
                format(big.mark = ",", digits = 0)
      ), "/60"),
      " schools completed student enrolment",
      icon = icon("child"),
      color = "teal"
    )
  })

  output$total_follow <- renderValueBox({
    valueBox(
      paste0((stu_followup_subs %>%
                distinct(siteID, .keep_all = TRUE) %>%
                count() %>%
                pull(n) %>%
                format(big.mark = ",", digits = 0)
      ), "/60"),
      " schools completed 1+ follow-up",
      icon = icon("calendar-alt"),
      color = "orange"
    )
  })

  output$total_final <- renderValueBox({
    valueBox(
      paste0("0", "/60"),
      " schools completed final follow-up",
      icon = icon("calendar-check"),
      color = "red"
    )
  })

  output$plot_schools <- renderPlotly({
      submissions %>%
  #       tweets_just(created_at, is_topic) %>%
  #       group_by(is_topic) %>%
        subs_volume() %>%
  #       mutate(topic = if_else(is_topic, "topic", "all")) %>%
        ungroup() %>%
        rename(Date = by_day) %>%
  #       select(-is_topic) %>%
        # spread(type, n, fill = 0) %>%
        add_row(Date = as.POSIXct("2021-11-01"), school_enr = 0) %>%
        plot_ly(x = ~ Date) %>%
        add_lines(y = ~ school_enr, name = "School enrolment", color = I(COLOURS$purple)) %>%
        # add_lines(., y = ~ student_enr, name = "Student enrolment", color = I(COLOURS$teal)) %>%
        # add_lines(., y = ~ follow_up, name = "Follow-up", color = I(COLOURS$orange)) %>%
        # {
        #   if (!is.null(student_enr)) {
        #     add_lines(., y = ~ student_enr, name = "Student enrolment", color = I(COLOURS$teal))
        #   } else .
        # }%>%
         config(displayModeBar = FALSE) %>%
         layout(
           xaxis = list(
             range = c(now() - days(14), now()),
             rangeselector = list(
               buttons = list(
                 # list(
                 #   count = 1,
                 #   label = "Today",
                 #   step = "day",
                 #   stepmode = "todate"),
                 # list(
                 #   count = 1,
                 #   label = "Yesterday",
                 #   step = "day",
                 #   stepmode = "backward"),
                 list(
                   count = 7,
                   label = "1 Week",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 14,
                   label = "2 Weeks",
                   step = "day",
                   stepmode = "backward"),
                 list(
                   count = 1,
                   label = "Month",
                   step = "month",
                   stepmode = "backward"),
                 list(step = "all", label = "All"))),
             rangeslider = list(type = "date")),
           yaxis = list(title = "Schools"),
           legend = list(orientation = 'h', x = 0.05, y = 0.9),
           hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
         ) %>%
         config(cloud = FALSE, mathjax = NULL)
   })

  output$plot_schools_cum <- renderPlotly({
    submissions %>%
      #       tweets_just(created_at, is_topic) %>%
      #       group_by(is_topic) %>%
      subs_volume() %>%
      #       mutate(topic = if_else(is_topic, "topic", "all")) %>%
      # ungroup() %>%
      rename(Date = by_day) %>%
      #       select(-is_topic) %>%
      # spread(type, cum_n, fill = 0) %>%
      add_row(Date = as.POSIXct("2021-11-01"), cum_sch = 0) %>%
      plot_ly(x = ~ Date) %>%
      add_lines(y = ~ cum_sch, name = "School enrolment", color = I(COLOURS$purple)) %>%
      # add_lines(., y = ~ student_enr, name = "Student enrolment", color = I(COLOURS$teal)) %>%
      # add_lines(., y = ~ follow_up, name = "Follow-up", color = I(COLOURS$orange)) %>%
      # {
      #   if (!is.null(student_enr)) {
      #     add_lines(., y = ~ student_enr, name = "Student enrolment", color = I(COLOURS$teal))
      #   } else .
      # }%>%
      config(displayModeBar = FALSE) %>%
      layout(
        xaxis = list(
          range = c(now() - days(14), now()),
          rangeselector = list(
            buttons = list(
              # list(
              #   count = 1,
              #   label = "Today",
              #   step = "day",
              #   stepmode = "todate"),
              # list(
              #   count = 1,
              #   label = "Yesterday",
              #   step = "day",
              #   stepmode = "backward"),
              list(
                count = 7,
                label = "1 Week",
                step = "day",
                stepmode = "backward"),
              list(
                count = 14,
                label = "2 Weeks",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "Month",
                step = "month",
                stepmode = "backward"),
              list(step = "all", label = "All"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Schools"),
        legend = list(orientation = 'h', x = 0.05, y = 0.9),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      ) %>%
      config(cloud = FALSE, mathjax = NULL)
  })

  output$plot_students <- renderPlotly({
    stu_enrolment_all %>%
      #       tweets_just(created_at, is_topic) %>%
      #       group_by(is_topic) %>%
      subs_volume() %>%
      #       mutate(topic = if_else(is_topic, "topic", "all")) %>%
      ungroup() %>%
      rename(Date = by_day) %>%
      #       select(-is_topic) %>%
      spread(type, n, fill = 0) %>%
      add_row(Date = as.POSIXct("2021-06-06"), student_enr = 0) %>%
      plot_ly(x = ~ Date) %>%
      add_lines(y = ~ student_enr, name = "Student enrolment", color = I(COLOURS$teal)) %>%
      #       {
      #         if (!is.null(TOPIC$full_community)) {
      #           add_lines(., y = ~all, name = TOPIC$full_community, color = I(ADMINLTE_COLORS$purple))
      #         } else .
      #       }%>%
      config(displayModeBar = FALSE) %>%
      layout(
        xaxis = list(
          range = c(now() - days(7), now()),
          rangeselector = list(
            buttons = list(
              # list(
              #   count = 1,
              #   label = "Today",
              #   step = "day",
              #   stepmode = "todate"),
              # list(
              #   count = 1,
              #   label = "Yesterday",
              #   step = "day",
              #   stepmode = "backward"),
              list(
                count = 7,
                label = "Week",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "Month",
                step = "month",
                stepmode = "backward"),
              list(step = "all", label = "All"))),
          rangeslider = list(type = "date")),
        yaxis = list(title = "Schools"),
        legend = list(orientation = 'h', x = 0.05, y = 0.9),
        hovermode = "compare" # thanks: https://stackoverflow.com/a/46733461/2022615
      ) %>%
      config(cloud = FALSE, mathjax = NULL)
  })
}

shinyApp(ui, server)
