#'#***************************************************************************
#'@author : Amael DUPAIX
#'@update : 2025-11-23
#'@email : amael.dupaix@ird.fr
#'#***************************************************************************
#'@description: server of the Shiny app
#'
#'#***************************************************************************

server <- function(input, output) {
  
  # Observe the load_file button
  # when the user clicks it, loads the sysrev data and the reviewers data
  observeEvent(input$reload_data, {

    reload_data <<- T
    source('scripts/load_sysrev_data.R')
    reload_data <<- F

  })
  
  # Output for number of coded articles
  output$n_coded <- renderText({
    input$reload_data
    as.character(n_coding)
  })
  output$n_to_code <- renderText({
    input$reload_data
    as.character(n_to_code)
  })
  output$n_included <- renderText({
    input$reload_data
    as.character(n_included)
  })
  
  # Progress bar on coding
  # Render progress UI
  output$top_progress_ui <- renderUI({
    input$reload_data
    
    # compute percent as integer for display (only fully reviewed)
    pct_label <- paste0(round(100 * n_coding / n_to_code), " %")
    
    # textual fraction: fully_reviewed / total_articles
    fraction_label <- paste0(n_coding, " / ", n_to_code)
    
    # Calculate segment widths as percentages
    # pct_fully_display <- round(100 * .20)
    # pct_partly_display <- round(100 * .20)
    # pct_not_display <- round(100 * .60)
    pct_fully_display <- round(100 * n_included / n_to_code)
    pct_partly_display <- round(100 * (n_coding - n_included) / n_to_code)
    pct_not_display <- round(100 * (n_to_code - n_coding) / n_to_code)
    
    # Build CSS gradient for the 3-segment progress bar
    # Format: green (fully) | yellow (partly) | gray (not reviewed)
    gradient_stops <- c()
    current_pct <- 0
    
    # Green segment (included)
    if (pct_fully_display > 0) {
      gradient_stops <- c(gradient_stops, 
                          paste0("#8BDBA1 0%"),
                          paste0("#8BDBA1 ", pct_fully_display, "%"))
      current_pct <- pct_fully_display
    }
    
    # Orange segment (coded, included or not)
    if (pct_partly_display > 0) {
      gradient_stops <- c(gradient_stops,
                          paste0("#F9B58B ", current_pct, "%"),
                          paste0("#F9B58B ", current_pct + pct_partly_display, "%"))
      current_pct <- current_pct + pct_partly_display
    }
    
    # Gray segment (not coded)
    if (pct_not_display > 0) {
      gradient_stops <- c(gradient_stops,
                          paste0("#e6e6e6 ", current_pct, "%"),
                          paste0("#e6e6e6 100%"))
    }
    
    # Create gradient string
    gradient_str <- paste(gradient_stops, collapse = ", ")
    bar_style <- paste0("background: linear-gradient(90deg, ", gradient_str, ");")
    
    # Build the HTML with gradient progress bar
    div(
      style = "display:inline-block; margin-left:20px; vertical-align: middle;",
      span(class = "progress-label", pct_label),
      span(style = "font-size:14px; margin-right:6px;", fraction_label),
      # progress bar wrapper
      div(class = "progress-wrap",
          div(class = "progress-bg",
              div(class = "progress-fill", style = bar_style)
          )
      )
    )
  })
  
  
  # Number of articles coded per user (all reviews, from the raw SysRev
  # exports re-downloaded when reload_data is triggered)
  output$user_activity <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_user_activity(data_dir), tooltip = 'text') |>
      # ggplotly ignores legend.position: put the legend on top explicitly
      plotly::layout(legend = list(orientation = 'h', x = 0.5,
                                   xanchor = 'center', y = 1.05))
  })

  # Reasons for excluding the articles
  # (same plot as 01.exclusion_reasons.png in qmd/plot_workflow.qmd)
  output$bar_excluded <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_exclusion_reasons(excluded_papers))
  })
  
  # Disciplines
  output$venn_disciplines <- renderPlot({
    input$reload_data

    plot_disciplines_venn(included_papers)
  })
  
  # Authors role
  output$venn_role <- renderPlot({
    input$reload_data

    plot_authors_role_venn(included_papers)
  })
  
  # Impacts / levers / challenges variables
  output$bar_impacts <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_impacts_levers_challenges(included_papers,
                                                    as_percent = TRUE))
  })
  
  # Stacked bar plot: number of papers per publication year, coloured by
  # chosen variable
  output$bar_papers_year <- plotly::renderPlotly({
    input$reload_data

    req(input$picker_area_var)

    plotly::ggplotly(plot_papers_per_year(
      included_papers, fill_var = input$picker_area_var
    ))
  })
  
  ## Engagement objectives plots
  # a. plot with coarse categories
  output$bar_engage_large <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_engagement_objectives_broad(included_papers))
  })
  
  #b. plot with detailed categories
  output$bar_engage_detail <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_engagement_objectives_detailed(included_papers))
  })
  
  # Venn diagram of ecosystems
  output$venn_ecosystem <- renderPlot({
    input$reload_data

    plot_ecosystems_venn(case_study_data)
  })
  
  # Bar plot of habitats
  output$bar_habitat <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_habitats(case_study_data))
  })
  
  # Map of study sites in the Hexagone
  output$map_hexagone <- renderPlot({
    input$reload_data
    
    SLRtools::build_map_hexagone(eez_path, lands_path, regions_path,
                                 count_data)+
      ggplot2::ggtitle('')
  })

  # Map of the Caribbean
  output$map_caribbean <- renderPlot({
    input$reload_data

    SLRtools::build_map_caraibe(eez_path, lands_path, regions_path,
                                   count_data)+
      ggplot2::ggtitle('')
  })

  # Map of Pacific and America
  output$map_epo <- renderPlot({
    input$reload_data

    SLRtools::build_map_epo(eez_path, lands_path, regions_path,
                            count_data)+
      ggplot2::ggtitle('')
  })

  # Map of French Polynesia
  output$map_polynesie <- renderPlot({
    input$reload_data

    SLRtools::build_map_polynesie(eez_path, lands_path, count_data)+
      ggplot2::ggtitle('')
  })

  # Map of Indian Ocean and Oceania
  output$map_io <- renderPlot({
    input$reload_data

    SLRtools::build_map_io(eez_path, lands_path, regions_path,
                           count_data)+
      ggplot2::ggtitle('')
  })

  # Map of South-West Indian Ocean
  output$map_swio <- renderPlot({
    input$reload_data

    SLRtools::build_map_swio(eez_path, lands_path, regions_path,
                            count_data)+
      ggplot2::ggtitle('')
  })
  
  # Bar plot of drivers of change
  output$bar_drivers <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_drivers_of_change(case_study_data))
  })
  
  # Bar plot of activities and uses
  output$bar_activities <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_activities_and_uses(case_study_data))
  })
  
  # Radar participant inclusivity
  output$radar_inclusivity <- renderPlot({
    input$reload_data

    plot_participant_inclusivity(participant_data)
  })
  
  # Barplot participant type
  output$bar_part_type <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_participant_types(participant_data))
  })
  
  # Barplot research step and involvement level
  output$bar_step_involvement <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_research_step_involvement(
      participant_data, part_types = input$picker_part_types
    ))
  })
  
  # Barplot involvement iteration per participant type
  output$bar_iter_part_type <- plotly::renderPlotly({
    input$reload_data

    plotly::ggplotly(plot_participant_iteration(participant_data))
  })
  
  
}

