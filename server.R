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

  output$n_included <- renderText({
    input$reload_data
    as.character(n_included)
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
    
  }
  )
  
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
    venn_ecosystem <- list(
      Land = grep('Land \\(coastal\\)',
                  included_answers$ecosystem_main),
      Coastal = grep('Coastal ocean',
                     included_answers$ecosystem_main),
      `Open Ocean` = grep('Open-ocean',
                          included_answers$ecosystem_main)
    )
    
    ggVennDiagram::ggVennDiagram(venn_ecosystem, lwd = 0)+
      ggplot2::scale_fill_gradient(low = "#F4FAFE", high = "#4981BF") +
      ggplot2::theme(legend.position = 'none')
  })
  
  # Bar plot of habitats
  output$bar_habitat <- renderPlot({
    
    habitat_list <- lapply(strsplit(included_answers$vulnerable_habitat, ';'),
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
    SLRtools::build_map_hexagone(eez_path, lands_path, regions_path,
                                 count_data)+
      ggplot2::ggtitle('')
  })
  
  # Bar plot of drivers of change
  output$bar_drivers <- renderPlot({
    drivers_list <- lapply(strsplit(included_answers$drivers_of_change, ';'),
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
  
}

