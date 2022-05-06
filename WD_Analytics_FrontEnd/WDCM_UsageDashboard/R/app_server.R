### ---------------------------------------------------------------------------
### --- WDCM Usage
### --- Version 1.0.0
### --- 2020.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and Wikimedia Deutschland (WMDE).
### --- Contact: goran.milovanovic_ext@wikimedia.de
### --- Contact: goran.milovanovic@datakolektiv.com
### ---------------------------------------------------------------------------

#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  ### --- functions
  # - get_WDCM_table()
  get_WDCM_table <- function(url_dir, filename, row_names = F) {
    if (row_names == T) {
      read.csv(URLencode(paste0(url_dir, filename)), 
               header = T, 
               stringsAsFactors = F,
               check.names = F, 
               row.names = 1)
    } else {
      read.csv(URLencode(paste0(url_dir, filename)), 
               header = T, 
               stringsAsFactors = F,
               check.names = F)
    }
  }
  # - projectType() to determine project type
  projectType <- function(projectName) {
    unname(sapply(projectName, function(x) {
      if (grepl("commons", x, fixed = T)) {"Commons"
      } else if (grepl("mediawiki|meta|species|wikidata", x)) {"Other"
      } else if (grepl("wiki$", x)) {"Wikipedia"
      } else if (grepl("quote$", x)) {"Wikiquote"
      } else if (grepl("voyage$", x)) {"Wikivoyage"
      } else if (grepl("news$", x)) {"Wikinews"
      } else if (grepl("source$", x)) {"Wikisource"
      } else if (grepl("wiktionary$", x)) {"Wiktionary"
      } else if (grepl("versity$", x)) {"Wikiversity"
      } else if (grepl("books$", x)) {"Wikibooks"
      } else {"Other"}
    }))
  }
  
  ### --- dirTree
  etl_dir <- 'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/etl/'
  updatePath <-'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/WDCM_MainReport.csv'
  
  ### --- DATA
  withProgress(message = 'Downloading data', 
               detail = "Please be patient.", 
               value = 0, {
    
    ### --- fetch wdcm2_project
    wdcmProject <- get_WDCM_table(etl_dir, 'wdcm_project.csv')
    wdcmProject$type <- projectType(wdcmProject$eu_project)
    colnames(wdcmProject) <- c('Project', 'Usage', 'Project Type')
    incProgress(1/6, detail = "Please be patient.")
    
    ### --- fetch wdcm2_project_category
    wdcmProjectCategory <- get_WDCM_table(etl_dir, 
                                          'wdcm_project_category.csv', 
                                          row_names = T)
    wdcmProjectCategory <- 
      wdcmProjectCategory[, c('category', 'eu_project', 'eu_count')]
    wdcmProjectCategory$type <- 
      projectType(wdcmProjectCategory$eu_project)
    colnames(wdcmProjectCategory) <- 
      c('Category', 'Project', 'Usage', 'Project Type')
    # - fix `Wikimedia_Internal` to `Wikimedia`
    wdcmProjectCategory$Category[wdcmProjectCategory$Category == 'Wikimedia_Internal'] <- 
      'Wikimedia'
    incProgress(2/6, detail = "Please be patient.")
    
    ### --- fetch wdcm2_project_item100
    wdcmProjectItem100 <- 
      get_WDCM_table(etl_dir, 
                     'wdcm_project_item100_labels.csv', 
                     row_names = T)
    wdcmProjectItem100$type <- projectType(wdcmProjectItem100$eu_project)
    colnames(wdcmProjectItem100) <- 
      c('Project', 'EntityID', 'Usage','Label', 'Project Type')
    incProgress(3/6, detail = "Please be patient.")
    
    ### --- fetch wdcm2_project_category_item100
    wdcmProjectCategoryItem100 <- 
      get_WDCM_table(etl_dir, 
                     'wdcm_project_category_item100.csv', 
                     row_names = T)
    wdcmProjectCategoryItem100$projectType <- 
      projectType(wdcmProjectCategoryItem100$eu_project)
    colnames(wdcmProjectCategoryItem100) <- 
      c('Project', 'Category', 'EntityID', 'Usage', 'Label', 'Project Type')
    wdcmProjectCategoryItem100 <- dplyr::arrange(wdcmProjectCategoryItem100, 
                                                 Project, Category, desc(Usage))
    # - fix `Wikimedia_Internal` to `Wikimedia`
    wdcmProjectCategoryItem100$Category[wdcmProjectCategoryItem100$Category == 'Wikimedia_Internal'] <- 
      'Wikimedia'
    incProgress(4/6, detail = "Please be patient.")
    
    ### --- fetch wdcm2_category
    wdcmCategory <- get_WDCM_table(etl_dir, 
                                   'wdcm_category.csv', 
                                   row_names = T)
    colnames(wdcmCategory) <- c('Category', 'Usage')
    incProgress(5/6, detail = "Please be patient.")
    
    ### --- fetch wdcm2_category_item100
    wdcmCategoryItem100 <- get_WDCM_table(etl_dir, 
                                          'wdcm_category_item.csv', 
                                          row_names = T)
    wdcmCategoryItem100 <- 
      wdcmCategoryItem100[, c('eu_entity_id', 'eu_count', 'eu_label', 'Category')]
    colnames(wdcmCategoryItem100) <- c('EntityID', 'Usage', 'Label', 'Category')
    # - fix `Wikimedia_Internal` to `Wikimedia`
    wdcmCategoryItem100$Category[wdcmCategoryItem100$Category == 'Wikimedia Internal'] <- 
      'Wikimedia'
    incProgress(6/6, detail = "Please be patient.")
    
  })
  
  ### --- Compute per `Project Type` tables
  # - wdcmProjectType
  wdcmProjectType <- wdcmProject %>% 
    dplyr::group_by(`Project Type`) %>% 
    dplyr::summarise(Usage = sum(Usage)) %>% 
    dplyr::arrange(desc(Usage))
  # - wdcmProjectTypeCategory
  wdcmProjectTypeCategory <- wdcmProjectCategory %>% 
    dplyr::group_by(`Project Type`, Category) %>% 
    dplyr::summarise(Usage = sum(Usage)) %>% 
    dplyr::arrange(desc(Usage))
  # - wdcmProjectTypeItem100
  wdcmProjectTypeItem100 <- wdcmProjectItem100 %>% 
    dplyr::select(`Project Type`, EntityID, Label, Usage) %>% 
    dplyr::group_by(`Project Type`, EntityID, Label) %>% 
    dplyr::summarise(Usage = sum(Usage)) %>% 
    dplyr::arrange(`Project Type`, desc(Usage))
  
  ### --- Compute project similarity structure
  projectSimilarity <- wdcmProjectCategory %>% 
    dplyr::select(Project, Category, Usage) %>% 
    tidyr::spread(key = Category,
                  value = Usage,
                  fill = 0)
  projectNames <- projectSimilarity$Project
  projectSimilarity$Project <- NULL
  # - normalize:
  projectSimilarity <- t(apply(projectSimilarity, 1, function(x) {x/sum(x)}))
  # projectSimilarity[projectSimilarity > 0] <- 1
  projectSimilarity <- as.matrix(
    parallelDist::parDist(as.matrix(projectSimilarity),
                          method = "kullback")
    )
  rownames(projectSimilarity) <- projectNames
  colnames(projectSimilarity) <- projectNames
  
  ### - Determine Constants
  # - determine Projects
  projects <- wdcmProject$Project
  # - determine present Project Types
  projectTypes <- unique(wdcmProject$`Project Type`)
  # - and assign Brewer colors
  lengthProjectColor <- length(unique(wdcmProject$`Project Type`))
  projectTypeColor <- RColorBrewer::brewer.pal(lengthProjectColor, "Set1")
  names(projectTypeColor) <- unique(wdcmProject$`Project Type`)
  # - determine Categories
  categories <- wdcmCategory$Category
  # - fix `Wikimedia_Internal` to `Wikimedia`
  categories[categories == 'Wikimedia_Internal'] <- 'Wikimedia'
  # - totalUsage
  totalUsage <- sum(wdcmProject$Usage)
  totalProjects <- length(wdcmProject$Project)
  totalCategories <- length(wdcmCategory$Category)
  totalProjectTypes <- length(wdcmProjectType$`Project Type`)
  
  ### --- prepare search constants for Tabs/Crosstabs
  search_projectTypes <- paste("_", projectTypes, sep = "")
  unzip_projectTypes <- lapply(projectTypes, function(x) {
    wdcmProject$Project[which(wdcmProject$`Project Type` %in% x)]
  })
  names(unzip_projectTypes) <- search_projectTypes
  
  ### --- Fetch update info
  update <- read.csv(updatePath, 
                     header = T,
                     check.names = F,
                     stringsAsFactors = F,
                     row.names = 1)
  
  
  ### --- OUTPUTS
  
  ### --- output: updateString
  output$updateString <- renderText({
    date <- update[max(which(grepl("Orchestra END", update$Step))), ]$Time
    date <- paste0(date, " UTC")
    return(paste('<p style="font-size:80%;"align="right"><b>Last update: </b><i>', date, '</i></p>', sep = ""))
  })
  
  ### ----------------------------------
  ### --- BASIC FACTS
  ### ----------------------------------
  
  ### --- valueBox: totalUsage
  # output$totalUsageBox
  output$totalUsageBox <- renderValueBox({
    valueBox(
      value = as.character(totalUsage),
      subtitle = "Total Wikidata Item Usage",
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$totalUsageBox
  
  ### --- valueBox: totalProjects
  # output$totalProjectsBox
  output$totalProjectsBox <- renderValueBox({
    valueBox(
      value = as.character(totalProjects),
      subtitle = "Projects Tracked",
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$totalProjectsBox
  
  ### --- valueBox: totalCategories
  # output$totalCategoriesBox
  output$totalCategoriesBox <- renderValueBox({
    valueBox(
      value = as.character(totalCategories),
      subtitle = "Semantic Categories",
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$totalCategoriesBox
  
  ### --- valueBox: totalProjectTypes
  # output$totalProjectTypesBox
  output$totalProjectTypesBox <- renderValueBox({
    valueBox(
      value = as.character(totalProjectTypes),
      subtitle = "Project Types",
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$totalProjectTypesBox
  
  ### ----------------------------------
  ### --- CATEGORIES OVERVIEW
  ### ----------------------------------
  
  ### --- SELECT: update select 'categories'
  updateSelectizeInput(session,
                       'categories',
                       choices = categories,
                       selected = 
                         categories[round(runif(1, 1, length(categories)))],
                       server = TRUE)
  
  ### --- htmlOutput: categoryProjects_overview_Title
  output$categoryProjects_overview_Title <- renderText({
    paste("<b>", input$categories, " top 30 projects:</b>")
  })
  
  ### --- lineplot: categoryProjects_overview
  output$categoryProjects_overview <- renderPlot({
    if (!(input$categories == "")) {
      
      plotFrame <- wdcmProjectCategory %>% 
        dplyr::select(Project, Category, Usage) %>% 
        dplyr::filter(Category %in% input$categories) %>% 
        dplyr::arrange(desc(Usage))
      
      otherSum <- sum(plotFrame$Usage[31:dim(plotFrame)[1]]) 
      
      other <- data.frame(Project = 'Other', 
                          Category = input$categories,
                          Usage = otherSum,
                          stringsAsFactors = F)
      
      plotFrame <- rbind(plotFrame[1:30, ], other)
      plotFrame$Percent <- paste(round(plotFrame$Usage/sum(plotFrame$Usage, na.rm = T)*100, 2),
                                 "%", sep = "")
      plotFrame$Project <- factor(plotFrame$Project, 
                                  levels = plotFrame$Project[order(plotFrame$Usage)])
      ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Project)) +
        ggplot2::geom_line(size = .35, color = "firebrick", group = 1) +
        ggplot2::geom_point(size = 2, color = "firebrick") +
        ggplot2::geom_point(size = 1.5, color = "white") + 
        ggrepel::geom_text_repel(ggplot2::aes(label = plotFrame$Percent), 
                        size = 3) +
        ggplot2::xlab("Item Usage") + ggplot2::ylab("Project") +
        ggplot2::scale_x_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### --- dynamic lineplot: categoryProjects_overview_dynamic
  output$categoryProjects_overview_dynamic <- plotly::renderPlotly({
    if (!(input$categories == "")) {
      
      plotFrame <- wdcmProjectCategory %>% 
        dplyr::select(Project, Category, Usage) %>% 
        dplyr::filter(Category %in% input$categories) %>% 
        dplyr::arrange(desc(Usage))
      
      otherSum <- sum(plotFrame$Usage[31:dim(plotFrame)[1]]) 
      
      other <- data.frame(Project = 'Other', 
                          Category = input$categories,
                          Usage = otherSum,
                          stringsAsFactors = F)
      
      plotFrame <- rbind(plotFrame[1:30, ], other)
      plotFrame$Percent <- paste(round(plotFrame$Usage/sum(plotFrame$Usage, na.rm = T)*100, 2),
                                 "%", sep = "")
      plotFrame$Project <- factor(plotFrame$Project, 
                                  levels = plotFrame$Project[order(plotFrame$Usage)])
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Project)) +
        ggplot2::geom_line(size = .35, color = "firebrick", group = 1) +
        ggplot2::geom_point(size = 2, color = "firebrick") +
        ggplot2::geom_point(size = 1.5, color = "white") + 
        ggplot2::xlab("Item Usage") + ggplot2::ylab("Project") +
        ggplot2::geom_text(ggplot2::aes(label = `Percent`),
                           size = 3) +
        ggplot2::scale_x_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
      plotly::ggplotly(g) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### --- htmlOutput: categoryItems_overview_Title
  output$categoryItems_overview_Title <- renderText({
    paste("<b>", input$categories, " top 30 Wikidata items:</b>")
  })
  
  ### --- lineplot: categoryItems_overview
  output$categoryItems_overview <- renderPlot({
    if (!(input$categories == "")) {
      
      plotFrame <- wdcmCategoryItem100 %>% 
        dplyr::filter(Category %in% input$categories) %>% 
        dplyr::arrange(desc(Usage))
      plotFrame <- plotFrame[1:30, ]
      plotFrame$Label[plotFrame$Label == ''] <- plotFrame$EntityID[plotFrame$Label == '']
      lCheck <- table(plotFrame$Label)
      dLabs <- which(lCheck > 1)
      wdLabs <- which(plotFrame$Label == names(dLabs))
      plotFrame$Label[wdLabs[2]] <- paste0(plotFrame$Label[wdLabs[2]], " ")
      plotFrame$Label <- factor(plotFrame$Label, 
                                levels = plotFrame$Label[order(plotFrame$Usage)])
      ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Label)) +
        ggplot2::geom_line(size = .35, color = "firebrick", group = 1) +
        ggplot2::geom_point(size = 2, color = "firebrick") +
        ggplot2::geom_point(size = 1.5, color = "white") + 
        ggrepel::geom_text_repel(ggplot2::aes(label = plotFrame$EntityID), 
                        size = 3) +
        ggplot2::xlab("Item Usage") + ggplot2::ylab("Item") +
        ggplot2::scale_x_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### --- lineplot: categoryItems_overview_interactive
  output$categoryItems_overview_interactive <- plotly::renderPlotly({
    if (!(input$categories == "")) {
      
      plotFrame <- wdcmCategoryItem100 %>% 
        dplyr::filter(Category %in% input$categories) %>% 
        dplyr::arrange(desc(Usage))
      plotFrame <- plotFrame[1:30, ]
      plotFrame$Label[plotFrame$Label == ''] <- plotFrame$EntityID[plotFrame$Label == '']
      lCheck <- table(plotFrame$Label)
      dLabs <- which(lCheck > 1)
      wdLabs <- which(plotFrame$Label == names(dLabs))
      plotFrame$Label[wdLabs[2]] <- paste0(plotFrame$Label[wdLabs[2]], " ")
      plotFrame$Label <- factor(plotFrame$Label, 
                                levels = plotFrame$Label[order(plotFrame$Usage)])
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Label)) +
        ggplot2::geom_line(size = .35, color = "firebrick", group = 1) +
        ggplot2::geom_point(size = 2, color = "firebrick") +
        ggplot2::geom_point(size = 1.5, color = "white") + 
        ggplot2::geom_text(ggplot2::aes(label = plotFrame$EntityID),
                           size = 3) +
        ggplot2::xlab("Item Usage") + ggplot2::ylab("Item") +
        ggplot2::scale_x_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
      plotly::ggplotly(g) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### --- lineplot: basicFacts_CategoryLine
  output$basicFacts_CategoryLine <- renderPlot({
    plotFrame <- wdcmCategory
    plotFrame$Percent <- paste(round(plotFrame$Usage/sum(plotFrame$Usage)*100, 2),
                               "%", sep = "")
    plotFrame$Category <- factor(plotFrame$Category, 
                                 levels = plotFrame$Category[order(plotFrame$Usage)])
    ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Category)) +
      ggplot2::geom_line(size = .35, color = "firebrick", group = 1) +
      ggplot2::geom_point(size = 2, color = "firebrick") +
      ggplot2::geom_point(size = 1.5, color = "white") + 
      ggrepel::geom_text_repel(ggplot2::aes(label = plotFrame$Percent), 
                      size = 3) +
      ggplot2::xlab("Item Usage") + ggplot2::ylab("Category") +
      ggplot2::scale_x_continuous(labels = scales::comma) + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- lineplot: basicFacts_CategoryLine_interactive
  output$basicFacts_CategoryLine_interactive <- plotly::renderPlotly({
    plotFrame <- wdcmCategory
    plotFrame$Percent <- paste(round(plotFrame$Usage/sum(plotFrame$Usage)*100, 2),
                               "%", sep = "")
    plotFrame$Category <- factor(plotFrame$Category, 
                                 levels = plotFrame$Category[order(plotFrame$Usage)])
    g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Category)) +
      ggplot2::geom_line(size = .35, color = "firebrick", group = 1) +
      ggplot2::geom_point(size = 2, color = "firebrick") +
      ggplot2::geom_point(size = 1.5, color = "white") + 
      ggplot2::geom_text(ggplot2::aes(label = plotFrame$Percent),
                         size = 3) +
      ggplot2::xlab("Item Usage") + ggplot2::ylab("Category") +
      ggplot2::scale_x_continuous(labels = scales::comma) + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
    plotly::ggplotly(g) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- barplot: basicFacts_ProjectTypeCategory
  output$basicFacts_ProjectTypeCategory <- renderPlot({
    ggplot2::ggplot(wdcmProjectTypeCategory, ggplot2::aes(y = log(Usage), x = Category, color = Category, fill = Category)) +
      ggplot2::geom_bar(stat = "identity", width = .15) + 
      ggplot2::facet_wrap(~wdcmProjectTypeCategory$`Project Type`, ncol = 3) +
      ggplot2::xlab("Category") + ggplot2::ylab("log(Item Usage)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
      ggplot2::theme(legend.position = "top") +
      ggplot2::theme(strip.background = ggplot2::element_blank()) + 
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold")) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- barplot: basicFacts_ProjectTypeCategory_interactive
  output$basicFacts_ProjectTypeCategory_interactive <- plotly::renderPlotly({
    g <- ggplot2::ggplot(wdcmProjectTypeCategory, ggplot2::aes(y = log(Usage), 
                                                          x = Category, 
                                                          color = Category, 
                                                          fill = Category)) +
      ggplot2::geom_bar(stat = "identity", width = .15) + 
      ggplot2::facet_wrap(~wdcmProjectTypeCategory$`Project Type`, 
                          ncol = 3) +
      ggplot2::ylab("log(Item Usage)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) + 
      ggplot2::theme(legend.position='none') +
      ggplot2::theme(strip.background = ggplot2::element_blank()) + 
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
    plotly::ggplotly(g, 
                     tooltip = c("x","y"),
                     originalData = T) %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### ----------------------------------
  ### --- PROJECT OVERVIEW
  ### ----------------------------------
  
  ### --- SELECT: update select 'projects'
  updateSelectizeInput(session,
                       'projects',
                       choices = projects,
                       selected = 'enwiki',
                       server = TRUE)
  
  ### --- barplot: projectOverview_Category
  output$projectOverview_Category <- renderPlot({
    plotFrame <- wdcmProjectCategory %>% 
      dplyr::filter(Project %in% input$projects)
    ggplot2::ggplot(plotFrame, ggplot2::aes(y = Usage, x = Category, color = Category, fill = Category)) +
      ggplot2::geom_bar(stat = "identity", width = .15) + 
      ggplot2::xlab("Category") + ggplot2::ylab("Item Usage") +
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
      ggplot2::theme(legend.position = "top") + 
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- barplot: projectOverview_Category_interactive
  output$projectOverview_Category_interactive <- plotly::renderPlotly({
    plotFrame <- wdcmProjectCategory %>% 
      dplyr::filter(Project %in% input$projects)
    g <- ggplot2::ggplot(plotFrame, ggplot2::aes(y = Usage,
                                                 x = Category, 
                                                 color = Category, 
                                                 fill = Category)) +
      ggplot2::geom_bar(stat = "identity", width = .15) + 
      ggplot2::xlab("Category") + ggplot2::ylab("Item Usage") +
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) + 
      ggplot2::theme(legend.position = "top") + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
    plotly::ggplotly(g, tooltip = c("x","y"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- htmlOutput: projectOverview_Report
  output$projectOverview_Report <- renderText({
    # - project:
    project <- input$projects
    # - total project rank:
    totalRank <- which(wdcmProject$Project %in% input$projects)
    # - total projects:
    totalProjects <- length(projects)
    # - usage volume:
    volume <- wdcmProject$Usage[totalRank]
    # - percentage of total Wikidata usage:
    percVolume <- paste(
      round(volume/sum(wdcmProject$Usage)*100, 2), "%", 
      sep = "")
    # - rank in its `Project Type`
    projectType <- wdcmProject$`Project Type`[totalRank]
    rankType <- wdcmProject %>% 
      dplyr::filter(`Project Type` %in% projectType) %>% 
      dplyr::arrange(desc(Usage))
    rankProjectType <- which(rankType$Project %in% project)
    # - total projects of this type
    totalProjectType <- dim(rankType)[1]
    paste("<p style=\"font-size:80%;\"align=\"left\">Wikidata usage on <b>", 
          project, "</b>:<br><br>", "<font size = 2><b>", 
          project, "</b> ", " has a total Wikidata usage volume of <b>", 
          volume, "</b> items (<b>", 
          percVolume, "</b> of total Wikidata usage across the 
          client projects).<br>In terms of Wikidata usage, it is ranked <b>", 
          totalRank, "/", totalProjects, 
          "</b> among all client projects, and <b>", rankProjectType, "/", 
          totalProjectType, ".</b> in 
          its Project Type (<b><i>", projectType, 
          "</i></b>).</font></p>", sep = "") %>% 
      withProgress(message = 'Generating report',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- htmlOutput: projectOverview_relativeRank_Title
  output$projectOverview_relativeRank_Title <- renderText({
    paste("<b>", input$projects, " Wikidata usage rank:</b>")
  })
  
  ### --- barplot: projectOverview_relativeRank
  output$projectOverview_relativeRank <- renderPlot({
    if (!(input$projects == "")) {
      ix <- which(wdcmProject$Project %in% input$projects)
      ixRange <- seq(ix - 10, ix + 10, by = 1)
      ixRange <- ixRange[which(ixRange > 0 & ixRange <= length(wdcmProject$Project))]
      plotFrame <- wdcmProject[ixRange, ]
      plotFrame$Rank <- ixRange
      plotFrame$Color <- rep('cadetblue3', dim(plotFrame)[1])
      plotFrame$Fill <- rep('white', dim(plotFrame)[1])
      plotFrame$Fill[which(plotFrame$Project %in% input$projects)] <- 'cadetblue3'
      plotFrame$Project <- factor(plotFrame$Project, 
                                  levels = plotFrame$Project[order(-plotFrame$Usage)])
      ggplot2::ggplot(plotFrame, ggplot2::aes(y = Usage, x = Project, color = Color, fill = Fill, label = Rank)) +
        ggplot2::geom_bar(stat = "identity", width = .1, color = plotFrame$Color, fill = plotFrame$Fill) + 
        ggplot2::xlab("Project") + ggplot2::ylab("Item Usage") +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(fill = "cadetblue3",
                   colour = "white", 
                   fontface = "bold", 
                   position = ggplot2::position_dodge(width = 1),
                   size = 4) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### --- barplot: projectOverview_relativeRank_interactive
  output$projectOverview_relativeRank_interactive <- plotly::renderPlotly({
    if (!(input$projects == "")) {
      ix <- which(wdcmProject$Project %in% input$projects)
      ixRange <- seq(ix - 10, ix + 10, by = 1)
      ixRange <- ixRange[which(ixRange > 0 & ixRange <= length(wdcmProject$Project))]
      plotFrame <- wdcmProject[ixRange, ]
      plotFrame$Rank <- ixRange
      plotFrame$Color <- rep('cadetblue3', dim(plotFrame)[1])
      plotFrame$Fill <- rep('white', dim(plotFrame)[1])
      plotFrame$Fill[which(plotFrame$Project %in% input$projects)] <- 'cadetblue3'
      plotFrame$Project <- factor(plotFrame$Project, 
                                  levels = plotFrame$Project[order(-plotFrame$Usage)])
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(y = Usage, x = Project, color = Color, fill = Fill, label = Rank)) +
        ggplot2::geom_bar(stat = "identity", width = .1, color = plotFrame$Color, fill = plotFrame$Fill) + 
        ggplot2::xlab("Project") + ggplot2::ylab("Item Usage") +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(fill = "cadetblue3",
                            colour = "white", 
                            fontface = "bold", 
                            position = ggplot2::position_dodge(width = 1),
                            size = 4) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### --- htmlOutput: projectOverview_topItems_Title
  output$projectOverview_topItems_Title <- renderText({
    paste("<b>", input$projects, " top 30 Wikidata items:</b>")
  })
  
  ### --- lineplot: projectOverview_topItems
  output$projectOverview_topItems <- renderPlot({
    if (!(input$projects == "")) {
      
      plotFrame <- wdcmProjectItem100 %>% 
        dplyr::filter(Project %in% input$projects) %>% 
        dplyr::arrange(desc(Usage))
      
      w <- which(!duplicated(plotFrame$Label))
      plotFrame <- plotFrame[w, ]
      plotFrame <- plotFrame[1:30, ]
      plotFrame$Label <- factor(plotFrame$Label, 
                                levels = plotFrame$Label[order(plotFrame$Usage)])
      ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Label)) +
        ggplot2::geom_line(size = .35, color = "darkblue", group = 1) +
        ggplot2::geom_point(size = 2, color = "darkblue") +
        ggplot2::geom_point(size = 1.5, color = "white") + 
        ggrepel::geom_text_repel(ggplot2::aes(label = plotFrame$EntityID), 
                        size = 3) +
        ggplot2::xlab("Item Usage") + ggplot2::ylab("Item") +
        ggplot2::scale_x_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### --- lineplot: projectOverview_topItems_interactive
  output$projectOverview_topItems_interactive <- plotly::renderPlotly({
    if (!(input$projects == "")) {
      
      plotFrame <- wdcmProjectItem100 %>% 
        dplyr::filter(Project %in% input$projects) %>% 
        dplyr::arrange(desc(Usage))
      
      w <- which(!duplicated(plotFrame$Label))
      plotFrame <- plotFrame[w, ]
      plotFrame <- plotFrame[1:30, ]
      plotFrame$Label <- factor(plotFrame$Label, 
                                levels = plotFrame$Label[order(plotFrame$Usage)])
      g <- ggplot2::ggplot(plotFrame, ggplot2::aes(x = Usage, y = Label)) +
        ggplot2::geom_line(size = .35, color = "darkblue", group = 1) +
        ggplot2::geom_point(size = 2, color = "darkblue") +
        ggplot2::geom_point(size = 1.5, color = "white") + 
        ggplot2::geom_text(ggplot2::aes(label = plotFrame$EntityID),
                           size = 3) +
        ggplot2::xlab("Item Usage") + ggplot2::ylab("Item") +
        ggplot2::scale_x_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, size = 9, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T) %>% 
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    } else {
      return(NULL)
    }
  })
  
  ### ----------------------------------
  ### --- TABS AND CROSSTABS
  ### ----------------------------------
  
  ### --- SELECT: update select 'selectProject'
  updateSelectizeInput(session,
                       'selectProject',
                       choices = c(projects, paste("_", projectTypes, sep = "")),
                       selected = c("_Wikipedia", "_Wikinews", "_Wiktionary"),
                       server = TRUE)
  
  ### --- SELECT: update select 'selectCategories'
  updateSelectizeInput(session,
                       'selectCategories',
                       choices = categories,
                       selected = categories[round(runif(6, 1, length(categories)))],
                       server = TRUE)
  
  tabsDataset <- reactive({
    ### --- selected projects:
    selectedProjects <- character()
    wUnzip <- which(names(unzip_projectTypes) %in% input$selectProject)
    if (length(wUnzip > 0)) {
      selectedProjects <- unname(do.call(c, unzip_projectTypes[wUnzip]))
    }
    wSel <- which(projects %in% input$selectProject)
    if (length(wSel > 0)) {
      selectedProjects <- c(selectedProjects, projects[wSel])
    }
    selectedProjects <- unique(selectedProjects)
    output$testSelectedProjects <- renderText({
      paste(selectedProjects, collapse = ", ", sep = "")
    })
    ### --- selected categories:
    selectedCategories <- input$selectCategories
    ### --- output
    out <- wdcmProjectCategory %>% 
      dplyr::filter(Project %in% selectedProjects & Category %in% selectedCategories)
    out
  })
  
  ### --- OBSERVE: input$applySelection
  observeEvent(input$applySelection, {
    
    #### ---  Chart: tabulations_projectsChart
    output$tabulations_projectsChart <- renderPlot({
      # - Chart Frame for output$tabulations_projectsChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(Project) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      # - top 25 projects:
      if (dim(plotFrame)[1] >= 25) {
        plotFrame <- plotFrame[1:25, ]
      }
      plotFrame$Project <- factor(plotFrame$Project, 
                                  levels = plotFrame$Project[order(-plotFrame$Usage)])
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      ggplot2::ggplot(plotFrame,
             ggplot2::aes(x = Project, y = Usage, label = Label)) +
        ggplot2::geom_bar(stat = "identity", width = .6, fill = "#4c8cff") +
        ggplot2::xlab('Projects') + ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .1*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(size = 3, vjust = -.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15)) %>%
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    #### ---  Chart: tabulations_projectsChart_interactive
    output$tabulations_projectsChart_interactive <- plotly::renderPlotly({
      # - Chart Frame for output$tabulations_projectsChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(Project) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      # - top 25 projects:
      if (dim(plotFrame)[1] >= 25) {
        plotFrame <- plotFrame[1:25, ]
      }
      plotFrame$Project <- factor(plotFrame$Project, 
                                  levels = plotFrame$Project[order(-plotFrame$Usage)])
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      g <- ggplot2::ggplot(plotFrame,
                      ggplot2::aes(x = Project, 
                                   y = Usage, 
                                   label = Label)) +
        ggplot2::geom_bar(stat = "identity", width = .6, fill = "#4c8cff") +
        ggplot2::xlab('Projects') + ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .1*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(size = 3, vjust = -.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T) %>%         
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    # - Download Frame: tabulations_projectsChart
    tabulations_projectsDownload_Frame <- reactive({
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(Project) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      plotFrame
    })
    # - Download: tabulations_projectsChart
    output$tabulations_projectsDownload_Frame <- downloadHandler(
      filename = function() {
        'WDCM_Data.csv'},
      content = function(file) {
        write.csv(tabulations_projectsDownload_Frame(),
                  file,
                  quote = FALSE,
                  row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    #### ---  Chart: tabulations_categoriesChart
    output$tabulations_categoriesChart <- renderPlot({
      # - Chart Frame for output$tabulations_categoriesChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(Category) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      # - top 25 categories:
      if (dim(plotFrame)[1] > 25) {
        plotFrame <- plotFrame[1:25, ]
      }
      plotFrame$Category <- 
        factor(plotFrame$Category,
               levels = plotFrame$Category[order(-plotFrame$Usage)])
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      ggplot2::ggplot(plotFrame,
             ggplot2::aes(x = Category, y = Usage, label = Label)) +
        ggplot2::geom_bar(stat = "identity", width = .6, fill = "#4c8cff") +
        ggplot2::xlab('Category') + ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .1*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(size = 3, vjust = -.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                           size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15)) %>%
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    #### ---  Chart: tabulations_categoriesChart_interactive
    output$tabulations_categoriesChart_interactive <- plotly::renderPlotly({
      # - Chart Frame for output$tabulations_categoriesChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(Category) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      # - top 25 categories:
      if (dim(plotFrame)[1] > 25) {
        plotFrame <- plotFrame[1:25, ]
      }
      plotFrame$Category <- 
        factor(plotFrame$Category,
               levels = plotFrame$Category[order(-plotFrame$Usage)])
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      g <- ggplot2::ggplot(plotFrame,
                      ggplot2::aes(x = Category, y = Usage, label = Label)) +
        ggplot2::geom_bar(stat = "identity", width = .6, fill = "#4c8cff") +
        ggplot2::xlab('Category') + ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .1*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(size = 3, vjust = -.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
                                                           size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T) %>%       
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    # - Download Frame: tabulations_categoriesChart
    tabulations_categoriesDownload_Frame <- reactive({
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(Category) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      plotFrame
    })
    # - Download: tabulations_categoriesChart
    output$tabulations_categoriesDownload_Frame <- downloadHandler(
      filename = function() {
        'WDCM_Data.csv'},
      content = function(file) {
        write.csv(tabulations_categoriesDownload_Frame(),
                  file,
                  quote = FALSE,
                  row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    #### ---  Chart: tabulations_projectTypesChart
    output$tabulations_projectTypesChart <- renderPlot({
      # - Chart Frame for output$tabulations_projectTypesChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(`Project Type`) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      # - top 25 categories:
      if (dim(plotFrame)[1] > 25) {
        plotFrame <- plotFrame[1:25, ]
      }
      plotFrame$`Project Type` <- 
        factor(plotFrame$`Project Type`,
               levels = plotFrame$`Project Type`[order(-plotFrame$Usage)])
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      ggplot2::ggplot(plotFrame,
             ggplot2::aes(x = `Project Type`, y = Usage, label = Label)) +
        ggplot2::geom_bar(stat = "identity", width = .6, fill = "#4c8cff") +
        ggplot2::xlab('Project Type') + ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .1*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(size = 3, vjust = -.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15)) %>%
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    #### ---  Chart: tabulations_projectTypesChart_interactive
    output$tabulations_projectTypesChart_interactive <- plotly::renderPlotly({
      # - Chart Frame for output$tabulations_projectTypesChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(`Project Type`) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      # - top 25 categories:
      if (dim(plotFrame)[1] > 25) {
        plotFrame <- plotFrame[1:25, ]
      }
      plotFrame$`Project Type` <- 
        factor(plotFrame$`Project Type`,
               levels = plotFrame$`Project Type`[order(-plotFrame$Usage)])
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      g <- ggplot2::ggplot(plotFrame,
                      ggplot2::aes(x = `Project Type`, y = Usage, label = Label)) +
        ggplot2::geom_bar(stat = "identity", width = .6, fill = "#4c8cff") +
        ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .1*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::geom_label(size = 3, vjust = -.1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T) %>%      
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    # - Download Frame: tabulations_projectTypesChart
    tabulations_projectTypesChartDownload_Frame <- reactive({
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(`Project Type`) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      plotFrame
    })
    # - Download: tabulations_projectTypesChart
    output$tabulations_projectTypesChart_Frame <- downloadHandler(
      filename = function() {
        'WDCM_Data.csv'},
      content = function(file) {
        write.csv(tabulations_projectTypesChartDownload_Frame(),
                  file,
                  quote = FALSE,
                  row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    #### ---  Chart: crosstabulations_projectsCategoriesChart
    output$crosstabulations_projectsCategoriesChart <- renderPlot({
      # - Chart Frame for output$crosstabulations_projectsCategoriessChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::arrange(desc(Usage))
      projectOrder <- plotFrame %>%
        dplyr::group_by(Project) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      selProj <- projectOrder$Project[1:25]
      plotFrame <- plotFrame %>% 
        dplyr::filter(Project %in% selProj)
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      plotFrame$Project <- factor(plotFrame$Project,
                                  levels = selProj)
      # - Plot
      ggplot2::ggplot(plotFrame,
             ggplot2::aes(x = Project, y = Usage, label = Label)) +
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") + 
        ggplot2::geom_point(size = 1, color = "white") + 
        ggrepel::geom_text_repel(data = plotFrame, 
                        ggplot2::aes(x = Project, y = Usage, label = Label), 
                        size = 3) +
        ggplot2::facet_wrap(~ Category, ncol = 3, scales = "free_y") +
        ggplot2::xlab('Project') + ggplot2::ylab('Entity Usage') +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15)) %>%
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    #### ---  Chart: crosstabulations_projectsCategoriesChart_interactive
    output$crosstabulations_projectsCategoriesChart_interactive <- plotly::renderPlotly({
      # - Chart Frame for output$crosstabulations_projectsCategoriessChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::arrange(desc(Usage))
      projectOrder <- plotFrame %>%
        dplyr::group_by(Project) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>%
        dplyr::arrange(desc(Usage))
      selProj <- projectOrder$Project[1:25]
      plotFrame <- plotFrame %>% 
        dplyr::filter(Project %in% selProj)
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      plotFrame$Project <- factor(plotFrame$Project,
                                  levels = selProj)
      # - Plot
      g <- ggplot2::ggplot(plotFrame,
                      ggplot2::aes(x = Project, y = Usage, label = Label)) +
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") + 
        ggplot2::geom_point(size = 1, color = "white") + 
        ggplot2::geom_text(data = plotFrame,
                           ggplot2::aes(x = Project, y = Usage, label = Label),
                           size = 3) +
        ggplot2::facet_wrap(~ Category, ncol = 3, scales = "free_y") +
        ggplot2::xlab('Project') + ggplot2::ylab('Entity Usage') +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T) %>%   
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    # - Download Frame: crosstabulations_projectsCategoriesChart
    crosstabulations_projectsCategoriesChartDownload_Frame <- reactive({
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::arrange(desc(Usage))
      plotFrame
    })
    # - Download: crosstabulations_projectsCategoriesFrame
    output$crosstabulations_projectsCategoriesFrame <- downloadHandler(
      filename = function() {
        'WDCM_Data.csv'},
      content = function(file) {
        write.csv(crosstabulations_projectsCategoriesChartDownload_Frame(),
                  file,
                  quote = FALSE,
                  row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
    #### ---  Chart: crosstabulations_projectTypesCategoriesChart
    output$crosstabulations_projectTypesCategoriesChart <- renderPlot({
      # - Chart Frame for output$crosstabulations_projectTypesCategoriesChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(`Project Type`, Category) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>% 
        dplyr::arrange(desc(Usage))
      projectTypeOrder <- plotFrame %>% 
        dplyr::group_by(`Project Type`) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>% 
        dplyr::arrange(desc(Usage))
      plotFrame$`Project Type` <- factor(plotFrame$`Project Type`, 
                                         levels = projectTypeOrder$`Project Type`)
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      ggplot2::ggplot(plotFrame,
             ggplot2::aes(x = `Project Type`, y = Usage, label = Label)) +
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") + 
        ggplot2::geom_point(size = 1, color = "white") + 
        ggrepel::geom_text_repel(data = plotFrame, 
                        ggplot2::aes(x = `Project Type`, y = Usage, label = Label), 
                        size = 3) +
        ggplot2::facet_wrap(~ Category, ncol = 3, scales = "free_y") +
        ggplot2::xlab('Project Type') + ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .5*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15)) %>%
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    #### ---  Chart: crosstabulations_projectTypesCategoriesChart_interactive
    output$crosstabulations_projectTypesCategoriesChart_interactive <- plotly::renderPlotly({
      # - Chart Frame for output$crosstabulations_projectTypesCategoriesChart
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(`Project Type`, Category) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>% 
        dplyr::arrange(desc(Usage))
      projectTypeOrder <- plotFrame %>% 
        dplyr::group_by(`Project Type`) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>% 
        dplyr::arrange(desc(Usage))
      plotFrame$`Project Type` <- factor(plotFrame$`Project Type`, 
                                         levels = projectTypeOrder$`Project Type`)
      # - express labels as K, M:
      plotFrame$Label <- sapply(plotFrame$Usage, function(x) {
        if (x >= 1e+03 & x < 1e+06) {
          out <- paste(round(x/1e+03, 1), "K", sep = "")
        } else if (x > 1e+06) {
          out <- paste(round(x/1e+06, 1), "M", sep = "")
        } else {
          out <- as.character(x)
        }
        return(out)
      })
      # - Plot
      g <- ggplot2::ggplot(plotFrame,
                      ggplot2::aes(x = `Project Type`, y = Usage, label = Label)) +
        ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
        ggplot2::geom_point(size = 1.5, color = "#4c8cff") + 
        ggplot2::geom_point(size = 1, color = "white") + 
        ggplot2::geom_text(data = plotFrame,
                           ggplot2::aes(x = `Project Type`, y = Usage, label = Label),
                           size = 3) +
        ggplot2::facet_wrap(~ Category, ncol = 3, scales = "free_y") +
        ggplot2::xlab('Project Type') + ggplot2::ylab('Entity Usage') +
        ggplot2::ylim(0, max(plotFrame$Usage) + .5*max(plotFrame$Usage)) +
        ggplot2::scale_y_continuous(labels = scales::comma) + 
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) +
        ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12)) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 15))
      plotly::ggplotly(g, 
                       tooltip = c("x","y"),
                       originalData = T) %>%   
        withProgress(message = 'Generating plot',
                     min = 0,
                     max = 1,
                     value = 1, {incProgress(amount = 0)})
    })
    
    # - Download Frame: crosstabulations_projectTypeCategoriesChart
    crosstabulations_projectTypeCategoriesChartDownload_Frame <- reactive({
      plotFrame <- isolate(tabsDataset()) %>%
        dplyr::group_by(`Project Type`, Category) %>% 
        dplyr::summarise(Usage = sum(Usage)) %>% 
        dplyr::arrange(desc(Usage))
      plotFrame
    })
    # - Download: crosstabulations_projectTypeCategoriesChartFrame
    output$crosstabulations_projectTypeCategoriesChartFrame <- downloadHandler(
      filename = function() {
        'WDCM_Data.csv'},
      content = function(file) {
        write.csv(crosstabulations_projectTypeCategoriesChartDownload_Frame(),
                  file,
                  quote = FALSE,
                  row.names = FALSE)
      },
      contentType = "text/csv"
    )
    
  }, ignoreNULL = FALSE)
  
  
  
  ### ----------------------------------
  ### --- TABLES
  ### ----------------------------------
  
  ### --- output$projectTable
  output$projectTable <- DT::renderDT({
    DT::datatable(wdcmProject,
              options = list(
                pageLength = 20,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$CategoryTable
  output$CategoryTable <- DT::renderDT({
    DT::datatable(wdcmCategory,
              options = list(
                pageLength = 20,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectCategoryDT
  output$projectCategoryDataTable <- DT::renderDT({
    DT::datatable(wdcmProjectCategory,
              options = list(
                pageLength = 20,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectType
  output$projectType <- DT::renderDT({
    DT::datatable(wdcmProjectType,
              options = list(
                pageLength = 20,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$projectTypeCategory
  output$projectTypeCategory <- DT::renderDT({
    DT::datatable(wdcmProjectTypeCategory,
              options = list(
                pageLength = 20,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
}
