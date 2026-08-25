#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2026-08-03
#'@email : amael.dupaix@ird.fr
#'#***************************************************************************
#'@description: ui of the Shiny app
#'
#'#***************************************************************************

# small CSS snippet for the progress bar style
progress_css <- "
.progress-wrap { display: inline-block; vertical-align: middle; margin-left: 12px; width: 200px; }
.progress-bg { background: #e6e6e6; border-radius: 4px; width: 100%; height: 14px; overflow: hidden; }
.progress-fill { height: 100%; width: 100%; transition: background 300ms ease-in-out; }
.progress-label { display: inline-block; vertical-align: middle; margin-right: 10px; font-weight: 600; }
"

ui <- dashboardPage(
  title = 'PAMS coding results',
  
  header = dashboardHeader(
    title = tags$span(
      tags$img(src = "Logo-PPR.png",
               height = "50px",
               style = "margin-right:10px;margin-left:0px"),
      "PAMS coding",
      style = "white-space: nowrap; overflow: visible; font-size:20px"
    ),
    # put the progress UI into the header bar (right side)
    tags$li(class = "dropdown",
            style = "padding: 8px; margin-right: 20px;",
            uiOutput("top_progress_ui")
    )
  ),
  sidebar = dashboardSidebar(
    
    sidebarMenu(
      # Tab where we can force data loading from sysrev
      # menuItem("Load data", tabName = "load", icon = icon('database')),
      # Meta-data, results at the scale of the articles
      menuItem("Article-level variables", tabName = "tab_articles", icon = icon("chart-pie")),
      menuItem("Location and ecosystems", tabName = "tab_ecosystem", icon = icon("map")),
      menuItem("Case-study-level variables", tabName = "tab_case_study", icon = icon("chart-bar")),
      menuItem("Participants-level variables", tabName = "tab_participants", icon = icon("users")),
      id = "tabs"
    ),
    
    hr(),
    actionButton(
      inputId = "reload_data",
      label = "Reload data"
    ),
    hr(),
    
    
    p("Number of coded articles: ",
      textOutput("n_coded", inline = T)),
    p("Number of included articles: ",
      textOutput("n_included", inline = T)),
    p("Number of articles to code: ",
      textOutput("n_to_code", inline = T)),
    hr(),
    
    width = 300
    
  ),
  
  body = dashboardBody(
    
    # CSS for the progress bar
    tags$head(tags$style(HTML(progress_css))),
    tabItems(
      
      # ---- TAB 1: Articles level ----
      tabItem(
        tabName = "tab_articles",
        ## First row ----
        fluidRow(
          column(
            width = 7,
            div(
              box(title = "Reasons for excluding articles at full-text",
                  plotOutput("piechart_excluded"),
                  div(p(
                    'There should not be any NA. Be carefull to fill the',
                    code('exclude_note'),
                    'variable when excluding an article.'
                  )),
                  width = 12)
            )

          ),
          
          column(
            width = 5,
            div(
              box(title = "Disciplines of the included articles",
                  plotOutput("venn_disciplines"),
                  width = 12)
            )
          )
          
        ),
        ## Second row ----
        fluidRow(
          column(
            width = 5,
            div(
              box(title = "Role of the study authors in the participatory process",
                  plotOutput("venn_role"),
                  width = 12)
            )
          ),
          
          column(
            width = 7,
            div(
              box(title = "Impact / levers / challenges",
                  plotOutput("bar_impacts"),
                  width = 12)
            )
            
            
          )
        ),
        # Third row ----
        fluidRow(
          box(title = "Engagement objectives",
              
              fluidRow(
                column(width = 3,
                       tags$div(
                         p(strong("Definitions:"),
                           tags$ul(
                             tags$li(strong("Implementation:"),
                                     "Increase of acceptance and legitimacy of the process outcomes."),
                             tags$li(strong("None:"),
                                     "The authors explicitly state that there was initially no objectives for involving stakeholders."),
                             tags$li(strong("Normative:"),
                                     "Democratic principle that those affected should have a say."),
                             tags$li(strong("Social learning:"),
                                     "Stimulating new behaviors, attitudes, and/or emotional reactions."),
                             tags$li(strong("Substantive:"),
                                     "Improvement of the quality of research."),
                             tags$li(strong("Unclear:"),
                                     "Objectives are not stated or are not clearly stated.")
                           ),
                           p('The total number of articles on the following graphs can be above the number of included articles since one article can have multiple associated engagement objectives.')
                           ))
                ),
                column(width = 9,
                       div(style = 'margin-bottom = 20px;',
                         plotOutput("bar_engage_large")),
                       div(
                         plotOutput("bar_engage_detail")
                       )
                )
              ),
              
              width = 12)
        )
      ),
      # ---- TAB 2: Ecosystems and location ----
      tabItem(
        tabName = 'tab_ecosystem',
        ## First row ----
        fluidRow(
          column(
            width = 5,
            div(
              box(title = "Ecosystems in which the studies were performed",
                  plotOutput("venn_ecosystem"),
                  width = 12)
            )
          ),
          column(
            width = 7,
            div(
              box(title = "Studied habitats in the corpus",
                  plotOutput("bar_habitat"),
                  width = 12)
            )
            
          )
        ),
        ## Second row ----
        fluidRow(
          
          column(
            width = 12,
            div(
              box(title = "Map of study site location in the Hexagone",
                  plotOutput("map_hexagone"),
                  width = 12)
            )
          )
        )
      ),
      # ---- TAB 3: Case study level variables ----
      tabItem(
        tabName = 'tab_case_study',
        ## First row ----
        fluidRow(
          column(
            width = 6,
            div(
              box(title = "Drivers of change",
                  plotOutput('bar_drivers'),
                  width = 12)
            )
          ),
          column(
            width = 6,
            div(
              box(title = "Activities and uses",
                  plotOutput('bar_activities'),
                  width = 12)
            )
          )
        )
      ),
      # ---- TAB 4: Participant type level variables ----
      tabItem(
        tabName = 'tab_participants',
        ## First row ----
        fluidRow(
          column(
            width = 6,
            div(
              box(title = 'Participant type',
                  plotOutput('bar_part_type'),
                  width = 12
              )
            )
          ),
          column(
            width = 6,
            div(
              box(title = 'Involvement iteration per participant type',
                  plotOutput('bar_iter_part_type'),
                  width = 12
              )
            )
          )
        ),
        ## Second row ----
        fluidRow(
          column(
            width = 8,
            div(
              box(title = 'Research step and involvement level',
                  plotOutput('bar_step_involvement'),
                  width = 12
              )
            )
          ),
          column(
            width = 4,
            div(
              box(title = 'Participant inclusivity',
                  p("Do the authors provide gender, ethnic, age or
                    nationality information on participants or any
                    information of inclusivity of participants?"),
                  plotOutput('radar_inclusivity'),
                  width = 12
              )
            )
          )
        )
      )
    )
  ),
  controlbar = dashboardControlbar(skinSelector())
)
