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
    source('R/load_sysrev_data.R')
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
                          paste0("#4caf50 0%"),
                          paste0("#4caf50 ", pct_fully_display, "%"))
      current_pct <- pct_fully_display
    }
    
    # Yellow segment (coded, included or not)
    if (pct_partly_display > 0) {
      gradient_stops <- c(gradient_stops,
                          paste0("#FBC02D ", current_pct, "%"),
                          paste0("#FBC02D ", current_pct + pct_partly_display, "%"))
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
  
  
  # Reasons for excluding the articles
  output$piechart_excluded <- renderPlot({
    input$reload_data
    
    ggplot2::ggplot(exclusion_reasons,
                    ggplot2::aes(x = '',
                                 y = n,
                                 fill = exclude_note))+
      ggplot2::geom_col()+
      ggplot2::coord_polar(theta = "y")+
      ggplot2::geom_text(ggplot2::aes(label = n),
                         position = ggplot2::position_stack(vjust = 0.5)) +
      ggplot2::scale_fill_viridis_d()+
      ggplot2::theme_void()
    
  })
  
  # Disciplines
  output$venn_disciplines <- renderPlot({
    input$reload_data
    
    disciplines_list <- list(
      A = grep('Life science & Environment',
               included_papers$article_discipline),
      B = grep('Sciences & Technology',
               included_papers$article_discipline),
      C = grep('Social Sciences & Humanities',
               included_papers$article_discipline)
    )
    
    ggVennDiagram::ggVennDiagram(disciplines_list, lwd = 0)+
      ggplot2::ggtitle(paste0('A: Life science & Environment\n',
                              'B: Sciences & Technology\n',
                              'C: Social Sciences & Humanities\n'
      ))+
      ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
      ggplot2::theme(legend.position = 'none')
  })
  
  # Authors role
  output$venn_role <- renderPlot({
    input$reload_data
    
    venn_leader <- list(
      Leader = grep('Leader',
                    included_papers$leader_authors),
      Participant = grep('Participant',
                         included_papers$leader_authors),
      Observer = grep('Observer',
                      included_papers$leader_authors)
    )
    
    ggVennDiagram::ggVennDiagram(venn_leader, lwd = 0)+
      ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
      ggplot2::theme(legend.position = 'none')
    
  })
  
  # Impacts / levers / challenges variables
  output$bar_impacts <- renderPlot({
    input$reload_data
    
    impact_lever_chal <- dplyr::bind_rows(
      included_papers |>
        dplyr::count(impact_evaluation) |>
        dplyr::rename(value = impact_evaluation) |>
        dplyr::mutate(var = 'impact_evaluation'),
      included_papers |>
        dplyr::count(levers) |>
        dplyr::rename(value = levers) |>
        dplyr::mutate(var = 'levers'),
      included_papers |>
        dplyr::count(challenges) |>
        dplyr::rename(value = challenges) |>
        dplyr::mutate(var = 'challenges')
    ) |>
      dplyr::mutate(value_plot = dplyr::case_when(
        value == 'Detailed' ~ 'Detailed/Evaluated',
        value == 'Evaluated' ~ 'Detailed/Evaluated',
        value == 'Mentioned' ~ 'Mentioned/Discussed',
        value == 'Discussed' ~ 'Mentioned/Discussed',
        value == 'No' ~ 'No'
      ))
    
    impact_lever_chal |>
      dplyr::select(var, value, n)
    
    ggplot2::ggplot(impact_lever_chal,
                    ggplot2::aes(x = var, y = n,
                                 fill = value_plot))+
      ggplot2::geom_bar(stat = 'identity')+
      ggplot2::scale_fill_discrete('Coded\nvalue',
                                   palette = 'Blues')+
      ggplot2::coord_flip()+
      ggplot2::ylab('Number of articles')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        panel.grid.major = ggplot2::element_line(color = 'grey'),
        axis.title.y = ggplot2::element_blank())
  })
  
  ## Engagement objectives plots
  # a. plot with coarse categories
  output$bar_engage_large <- renderPlot({
    input$reload_data

    objective_list <- lapply(strsplit(included_papers$engagement_objectives, ';'),
                             format_and_correct_objectives)
    main_objective_categories <- sort(unique(unlist(lapply(lapply(
      strsplit(unique(unlist(objective_list)),'-'),
      stringr::str_trim),
      function(x) x[1]))))
    main_objectives_df <- data.frame(objective = main_objective_categories) |>
      dplyr::mutate(nb_articles = sapply(main_objective_categories,
                                         function(x) length(grep(x,objective_list))))


    ggplot2::ggplot(main_objectives_df,
                    ggplot2::aes(x = objective,
                                 y = nb_articles,
                                 fill = objective))+
      ggplot2::geom_bar(stat='identity')+
      ggplot2::scale_fill_brewer(palette = 'Set1')+
      ggplot2::coord_flip()+
      ggplot2::scale_x_discrete(limits=rev)+
      ggplot2::xlab('Engagement objective')+
      ggplot2::ylab('Number of articles')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        legend.position = 'none',
        panel.grid.major = ggplot2::element_line(color = 'grey'))
  })
  
  #b. plot with detailed categories
  output$bar_engage_detail <- renderPlot({
    input$reload_data
    
    objective_list <- lapply(strsplit(included_papers$engagement_objectives, ';'),
                             format_and_correct_objectives)
    
    objective_categories <- sort(unique(unlist(objective_list)))
    objectives_df <- data.frame(objective = objective_categories,
                                main_objectives = sapply(strsplit(objective_categories,'-'),
                                                         function(x) x[1])) |>
      dplyr::mutate(nb_articles = sapply(objective_categories,
                                         function(x) length(grep(x,objective_list))))
    
    ggplot2::ggplot(objectives_df,
                    ggplot2::aes(x = objective,
                                 y = nb_articles,
                                 fill = main_objectives))+
      ggplot2::geom_bar(stat='identity')+
      ggplot2::scale_fill_brewer(palette = 'Set1')+
      ggplot2::coord_flip()+
      ggplot2::scale_x_discrete(limits=rev)+
      ggplot2::xlab('Engagement objective')+
      ggplot2::ylab('Number of articles')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        legend.position = 'none',
        panel.grid.major = ggplot2::element_line(color = 'grey'))
    
  })
  
  # Venn diagram of ecosystems
  output$venn_ecosystem <- renderPlot({
    input$reload_data
    
    venn_ecosystem <- list(
      Land = grep('Land \\(coastal\\)',
                  case_study_data$ecosystem_main),
      Coastal = grep('Coastal ocean',
                     case_study_data$ecosystem_main),
      `Open Ocean` = grep('Open-ocean',
                          case_study_data$ecosystem_main)
    )
    
    ggVennDiagram::ggVennDiagram(venn_ecosystem, lwd = 0)+
      ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
      ggplot2::theme(legend.position = 'none')
  })
  
  # Bar plot of habitats
  output$bar_habitat <- renderPlot({
    input$reload_data
    
    habitat_list <- lapply(strsplit(case_study_data$vulnerable_habitat, ';'),
                           stringr::str_trim)
    habitat_categories <- data.frame(category =
                                       c(
                                         'Coastal saltmarshes', 'Deltas', 'Mangroves',
                                         'Coastal lagoons', 'Other littoral ecosystems',
                                         
                                         "Kelp forests", "Tropical coral reef",
                                         'Cold & temperate coral & sponge ecosystems',
                                         'Rhodolith/Maërl beds',
                                         'Seagrass meadows', 'Shellfish beds',
                                         'Other continental shelf ecosystems',
                                         
                                         'Submarine canyons', 'Seamounts',
                                         'Other deep-sea ecosystems', 'None'),
                                     color = c(rep('Land', 5),
                                               rep('Coastal', 7),
                                               rep('Open-ocean', 3),
                                               'None'))
    habitat_df <- data.frame(habitat = habitat_categories$category,
                             color = habitat_categories$color) |>
      dplyr::mutate(nb_case_studies = sapply(habitat_categories$category,
                                             function(x) length(grep(x,habitat_list))))
    
    ggplot2::ggplot(habitat_df,
                    ggplot2::aes(x = habitat,
                                 y = nb_case_studies,
                                 fill = color))+
      ggplot2::geom_bar(stat = 'identity')+
      ggplot2::coord_flip()+
      ggplot2::scale_x_discrete(limits=habitat_categories$category[
        length(habitat_categories$category):1
      ])+
      ggplot2::scale_fill_manual('', values = c(Coastal = "#3182BD",
                                            Land = "#6BAED6",
                                            `Open-ocean` = "#08519C",
                                            None = 'grey'))+
      ggplot2::xlab('Habitats')+
      ggplot2::ylab('Number of case studies')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        panel.grid.major = ggplot2::element_line(color = 'grey'))
    
  })
  
  # Map of the Hexagone
  output$map_hexagone <- renderPlot({
    input$reload_data
    
    SLRtools::build_map_hexagone(eez_path, lands_path, regions_path,
                                 count_data)+
      ggplot2::ggtitle('')
  })
  
  # Bar plot of drivers of change
  output$bar_drivers <- renderPlot({
    input$reload_data
    
    drivers_list <- lapply(strsplit(case_study_data$drivers_of_change, ';'),
                           stringr::str_trim)
    drivers_categories <- data.frame(category = c(
      'Climate change', 'Exploitation \\(living resources\\)',
      'Pollution', 'Land/Sea use changes', 'Invasive alien species and disease',
      
      'Governance issues', 'Demographic', 'Economics', 'Technological', 'Crises',
      
      'Others', 'Unspecified', 'Unrelevant'),
      color = c(rep('Primary', 5),
                rep('Secondary', 5),
                rep('None', 3)))
    
    drivers_df <- data.frame(driver = drivers_categories$category,
                             color = drivers_categories$color) |>
      dplyr::mutate(nb_case_studies = sapply(drivers_categories$category,
                                             function(x) length(grep(x,drivers_list))))
    
    ggplot2::ggplot(drivers_df,
                    ggplot2::aes(x = driver,
                                 y = nb_case_studies,
                                 fill = color))+
      ggplot2::geom_bar(stat = 'identity')+
      ggplot2::coord_flip()+
      ggplot2::scale_x_discrete(limits=drivers_categories$category[
        length(drivers_categories$category):1
      ])+
      ggplot2::scale_fill_manual('', values = c(Primary = "#6BAED6",
                                                Secondary = "#08519C",
                                                None = 'grey'))+
      ggplot2::xlab('Drivers of change')+
      ggplot2::ylab('Number of case studies')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        panel.grid.major = ggplot2::element_line(color = 'grey'))
  })
  
  # Bar plot of activities and uses
  output$bar_activities <- renderPlot({
    input$reload_data
    
    activities_list <- lapply(strsplit(case_study_data$activities_and_uses, ';'),
                              stringr::str_trim)
    activities_categories <- data.frame(category = sort(c(
      "Aquaculture", "Commercial and subsistence fishing",
      "Marine Renewable Energy (MRE)", "Extractive activities (non-living)",
      "Ports and shipping", "Recreational activities",
      "Conservation activities", "Coastal land uses and infrastructures",
      "Ocean literacy", "Public policy and regulation",
      "Cultural and territorial identity contributions",
      "Marine geoengineering", "Other activities and uses",
      "Unclear", "None"
    ))) |>
      dplyr::mutate(color = dplyr::case_when(category %in% c('None', 'Unclear',
                                                             'Other activities and uses') ~
                                               'Other',
                                             T ~ 'Known'))
    
    activities_df <- data.frame(activity = activities_categories$category,
                                color = activities_categories$color) |>
      dplyr::mutate(nb_case_studies = sapply(activities_categories$category,
                                             function(x) length(grep(x,activities_list,
                                                                     fixed = T))))
    
    ggplot2::ggplot(activities_df,
                    ggplot2::aes(x = activity,
                                 y = nb_case_studies,
                                 fill = color))+
      ggplot2::geom_bar(stat = 'identity')+
      ggplot2::coord_flip()+
      ggplot2::scale_x_discrete(limits=activities_categories$category[
        length(activities_categories$category):1
      ])+
      ggplot2::scale_fill_manual('', values = c(Known = "#08519C",
                                                Other = 'grey'))+
      ggplot2::xlab('Drivers of change')+
      ggplot2::ylab('Number of case studies')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        panel.grid.major = ggplot2::element_line(color = 'grey'),
        legend.position = 'none')
  })
  
  # Radar participant inclusivity
  output$radar_inclusivity <- renderPlot({
    input$reload_data
    
    inclusivity_categories <- c('Gender', 'Ethnic', 'Age', 'Nationality', 'None')
    radar_data <- data.frame(group = 1)
    for (i in 1:length(inclusivity_categories)){
      radar_data <- radar_data |>
        dplyr::mutate(!!rlang::sym(inclusivity_categories[i]) :=
                        100 * length(grep(inclusivity_categories[i],
                                          participant_data$participant_inclusivity)) / nrow(participant_data))
    }
    names(radar_data) <- c("group",
                           paste0(names(radar_data[2:ncol(radar_data)]),
                                  '\n',
                                  round(radar_data[1,2:ncol(radar_data)]), '%'))
    
    
    ggradar::ggradar(radar_data,
                     group.point.size = 1,
                     group.line.width = 1,
                     axis.label.size = 3,
                     background.circle.colour = 'white',
                     gridline.mid.colour = "grey")
  })
  
  # Barplot participant type
  output$bar_part_type <- renderPlot({
    input$reload_data
    
    part_type_list <- lapply(strsplit(participant_data$participant_type, ';'),
                             stringr::str_trim)
    part_type_categories <- data.frame(category = sort(c(
      "General public", "Art & culture sector", "Education sector", "Fisheries sector", "Aquaculture sector", "Energy sector", "Other industries", "Public authorities, decision makers and managers", "NGOs", "Science & research", "Others", "Unclear"
    ))) |>
      dplyr::mutate(color = dplyr::case_when(category %in% c('Others', 'Unclear') ~
                                               'Other',
                                             T ~ 'Known'))
    
    part_type_df <- data.frame(part_type = part_type_categories$category,
                               color = part_type_categories$color) |>
      dplyr::mutate(nb_part_type = sapply(part_type_categories$category,
                                          function(x) length(grep(x,part_type_list,
                                                                  fixed = T))))
    
    ggplot2::ggplot(part_type_df,
                    ggplot2::aes(x = part_type,
                                 y = nb_part_type,
                                 fill = color))+
      ggplot2::geom_bar(stat = 'identity')+
      ggplot2::coord_flip()+
      ggplot2::scale_x_discrete(limits=part_type_categories$category[
        length(part_type_categories$category):1
      ])+
      ggplot2::scale_fill_manual('', values = c(Known = "#08519C",
                                                Other = 'grey'))+
      ggplot2::xlab('Participant type')+
      ggplot2::ylab('Number of coding units')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        panel.grid.major = ggplot2::element_line(color = 'grey'),
        legend.position = 'none')
  })
  
  # Barplot research step and involvement level
  output$bar_step_involvement <- renderPlot({
    input$reload_data
    
    developed_data <- participant_data |>
      dplyr::filter(involvement_level != 'No implication') |>
      dplyr::select(participant_type, research_step, involvement_level) |>
      split_duplicate_rows(split_col = 'participant_type', sep = ';') |>
      split_duplicate_rows(split_col = 'research_step', sep = ';') |>
      split_duplicate_rows(split_col = 'involvement_level', sep = ';') |>
      dplyr::mutate(involvement_level = factor(involvement_level,
                                               levels = c(
                                                 'Fully-lead',
                                                 'Involve/Collaborate',
                                                 'Consult', 'Inform',
                                                 'Unclear'
                                               )),
                    research_step = factor(research_step,
                                           levels = c(
                                             'Design', 'Collect',
                                             'Analyze', 'Evaluate',
                                             'Disseminate', 'Unclear'
                                           )))
    
    ggplot2::ggplot(developed_data, ggplot2::aes(x = research_step,
                                                 fill = involvement_level))+
      ggplot2::geom_bar()+
      ggplot2::scale_fill_viridis_d('Involvement level', direction = -1)+
      ggplot2::xlab('Research step')+
      ggplot2::ylab('Number of coding units')+
      ggplot2::theme(panel.background = ggplot2::element_rect(
        fill = 'white', color = 'black'),
        panel.grid.major = ggplot2::element_line(color = 'grey'))
  })
  
}

