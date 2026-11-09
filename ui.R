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
      # Number of articles coded per user
      menuItem("Users activity", tabName = "tab_users", icon = icon("user")),
      # Meta-data, results at the scale of the articles
      menuItem("Article-level variables", tabName = "tab_articles", icon = icon("chart-pie")),
      menuItem("Engagement objectives", tabName = "tab_objectives", icon = icon("bullseye")),
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

      # ---- TAB 0: Users activity ----
      tabItem(
        tabName = "tab_users",
        fluidRow(
          box(
            title = "Number of articles coded per user",
            plotly::plotlyOutput("user_activity", height = "700px"),
            width = 12
          )
        )
      ),

      # ---- TAB 1: Articles level ----
      tabItem(
        tabName = "tab_articles",
        ## First row ----
        fluidRow(
          box(
            title = "Number of papers per publication year",
            fluidRow(
              column(
                width = 3,
                pickerInput(
                  inputId = "picker_area_var",
                  label = "Colour by",
                  choices = area_choices,
                  selected = area_choices[1],
                  multiple = FALSE
                )
              ),
              column(
                width = 9,
                plotly::plotlyOutput("bar_papers_year")
              )
            ),
            width = 12
          )
        ),
        ## Second row ----
        fluidRow(
          column(
            width = 7,
            div(
              box(title = "Reasons for excluding articles at full-text",
                  plotly::plotlyOutput("bar_excluded"),
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
        ## Third row ----
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
                  plotly::plotlyOutput("bar_impacts"),
                  width = 12)
            )
            

          )
        )
      ),
      # ---- TAB 2: Engagement objectives ----
      tabItem(
        tabName = 'tab_objectives',
        ## First row ----
        fluidRow(
          box(title = "Definitions",
              tags$ul(
                tags$li(strong("Implementation:"),
                        "Increase of acceptance and legitimacy of the process outcomes."),
                tags$li(strong("None:"),
                        "The authors explicitly state that there was initially no objectives for involving stakeholders."),
                tags$li(strong("Normative:"),
                        "Democratic principle that those affected should have a say."),
                tags$li(strong("Requirement:"),
                        "Imposed on the researchers."),
                tags$li(strong("Social learning:"),
                        "Stimulating new behaviors, attitudes, and/or emotional reactions."),
                tags$li(strong("Substantive:"),
                        "Improvement of the quality of research."),
                tags$li(strong("Unclear:"),
                        "Objectives are not stated or are not clearly stated.")
              ),
              p('Percentages on the bar plots can sum to more than 100 % since one article can have several engagement objectives.'),
              width = 3),
          box(title = "Detailed categories",
              plotly::plotlyOutput("bar_engage_detail"),
              width = 9)
        ),
        ## Second row ----
        fluidRow(
          box(title = "Broad categories",
              plotly::plotlyOutput("bar_engage_large", height = "500px"),
              width = 6),
          box(title = "Co-occurence of the engagement objectives",
              # static plot: ggplotly() renders the size/linewidth legends
              # of the network poorly
              plotOutput("network_engage", height = "500px"),
              width = 6)
        )
      ),
      # ---- TAB 3: Ecosystems and location ----
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
                  plotly::plotlyOutput("bar_habitat"),
                  width = 12)
            )
            
          )
        ),
        ## Second row ----
        fluidRow(
          box(title = "Map of study site location in the Hexagone",
              plotOutput("map_hexagone"),
              width = 6),
          box(title = "Map of study site location in the Caribbean",
              plotOutput("map_caribbean"),
              width = 6)  
        ),
        ## Third row ----
        fluidRow(
          box(title = "Map of Pacific and America",
              plotOutput("map_epo"),
              width = 6),
          box(title = "Map of study site location in Polynesia",
              plotOutput("map_polynesie"),
              width = 6)
        ),
        ## Fourth row ----
        fluidRow(
          box(title = "Map of Indian Ocean and Oceania",
              plotOutput("map_io"),
              width = 6),
          box(title = "Map of study site location in the South-West Indian Ocean",
              plotOutput("map_swio"),
              width = 6)
        )
      ),
      # ---- TAB 4: Case study level variables ----
      tabItem(
        tabName = 'tab_case_study',
        ## First row ----
        fluidRow(
          column(
            width = 6,
            div(
              box(title = "Drivers of change",
                  plotly::plotlyOutput('bar_drivers'),
                  width = 12)
            )
          ),
          column(
            width = 6,
            div(
              box(title = "Activities and uses",
                  plotly::plotlyOutput('bar_activities'),
                  width = 12)
            )
          )
        )
      ),
      # ---- TAB 5: Participant type level variables ----
      tabItem(
        tabName = 'tab_participants',
        ## First row ----
        fluidRow(
          column(
            width = 8,
            div(
              box(title = 'Participant type',
                  plotly::plotlyOutput('bar_part_type'),
                  width = 12
              )
            )
          ),
          column(
            width = 4,
            div(
              box(title = 'Involvement iteration per participant type',
                  plotly::plotlyOutput('bar_iter_part_type'),
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
                  p("A case study counts once per research step and
                    involvement level, whatever the number of participant
                    types selected. A participant group with several steps
                    and several levels contributes every step × level pair."),
                  fluidRow(
                    column(width = 3,
                           pickerInput(
                             inputId = "picker_part_types", 
                             label = "Participant type(s)", 
                             choices = participant_choices,
                             selected = participant_choices,
                             options = pickerOptions(
                               actionsBox = TRUE, 
                               size = 10,
                               selectedTextFormat = "count > 3"
                             ), 
                             multiple = TRUE
                           )
                    ),
                    column(width = 9,
                           plotly::plotlyOutput('bar_step_involvement')
                    )
                  ),
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
