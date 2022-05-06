### ---------------------------------------------------------------------------
### --- WDCM Biases
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
  get_WDCM_table <- function(url_dir, filename, row_names) {
    read.csv(paste0(url_dir, filename), 
             header = T, 
             stringsAsFactors = F,
             check.names = F)
  }
  
  # - remote dir
  remoteDir <- 
    'https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/'
  
  # - download ML and ETL files:
  withProgress(message = 'Downloading data', 
               detail = "Please be patient.", 
               value = 0, {
    
    # - list files:
    # - genderProjectData
    incProgress(1/5, detail = "Access data repository.")
    genderProjectData <- get_WDCM_table(remoteDir, 
                                        'genderProjectDataSet.csv')
    genderProjectData[, 1] <- NULL
    genderProjectData <- dplyr::select(genderProjectData,
                                       -propM, -propF)
    genderProjectData$CI5 <- round(genderProjectData$CI5, 2)
    genderProjectData$CI95 <- round(genderProjectData$CI95, 2)
    genderProjectData$pMF <- round(genderProjectData$pMF, 2)
    genderProjectData$mfRation <- 
      round(genderProjectData$usageM/genderProjectData$usageF, 2)
    colnames(genderProjectData) <- c('Project', 'M Usage', 'F Usage', 
                                     'M%', 'F%', 'Project Type', 
                                     'Probability (M>F)', 'CI 5%', 'CI 95%', 'M/F')
    genderProjectData <- dplyr::arrange(genderProjectData, desc(`M Usage`))
    genderProjectData$`M%` <- round(genderProjectData$`M%`, 2)
    genderProjectData$`F%` <- round(genderProjectData$`F%`, 2)
    mfData <- genderProjectData %>% 
      dplyr::select('Project', 'Project Type', 'M/F', 'M Usage', 'F Usage')
    mfData$Usage <- mfData$`M Usage` + mfData$`F Usage`
    mfData <- mfData %>% 
      dplyr::arrange(desc(Usage)) %>% 
      dplyr::select('Project', 'Project Type', 'M/F', 'Usage')
    mfData <- mfData[1:50, ]
    mfData$Project <- 
      factor(mfData$Project, levels = mfData$Project[order(-mfData$Usage)])
    
    incProgress(2/5, detail = "Access data repository.")
    # - globalIndicators
    globalIndicators <- get_WDCM_table(remoteDir, 
                                       'globalIndicators.csv')
    globalIndicators[, 1] <- NULL
    
    incProgress(3/5, detail = "Access data repository.")
    # - mfPropProject
    mfPropProject <- get_WDCM_table(remoteDir, 
                                    'mfPropProject.csv')
    mfPropProject[, 1] <- NULL
    
    # - occUsage
    incProgress(4/5, detail = "Access data repository.")
    occUsage <- get_WDCM_table(remoteDir, 
                               'occUsage.csv')
    occUsage[, 1] <- NULL
    occUsage$MF <- round(occUsage$usageM/occUsage$usageF, 2)
    occUsage$CI5 <- round(occUsage$usageM/occUsage$CI5, 2)
    occUsage$CI95 <- round(occUsage$usageM/occUsage$CI95, 2)
    
    incProgress(5/5, detail = "Update timestamp.")
    # - update timestamp
    # - get update stamp:
    h <- curl::new_handle()
    curl::handle_setopt(h,
                        copypostfields = "WD_Biases");
    curl::handle_setheaders(h,
                            "Cache-Control" = "no-cache"
                            )
    timestamp <- curl::curl_fetch_memory('https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/wdcm/biases/')
    timestamp <- rawToChar(timestamp$content)
    timestamp <- stringr::str_extract_all(timestamp,
                                          "[[:digit:]]+-[[:digit:]]+-[[:digit:]]+\\s[[:digit:]]+:[[:digit:]]+")[[1]][1]
    timestamp <- trimws(timestamp, which = "both")
  })
  # - Where F items are used more then M items per occupation:
  fOcc <- occUsage %>% 
    dplyr::arrange(desc(usageF)) %>% 
    dplyr::filter(pMF == 0)
  fOcc <- data.frame(Occupation = fOcc$occupation, 
                     Label = fOcc$label,
                     'Usage (F)' = fOcc$usageF,
                     'Usage (M)' = fOcc$usageM,
                     '% Female' = round(fOcc$usageF/(fOcc$usageM + fOcc$usageF)*100, 2),
                     stringsAsFactors = F,
                     check.names = F)
  
  ### --- output: updateInfo
  output$updateInfo <- renderText({
    return(paste("<p align=right>Last update: <i>", timestamp, "</i></p>", sep = ""))
  })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Gender
  ### ------------------------------------------
  
  ### --- valueBox: stat1_M
  # output$stat1_Ms
  output$stat1_M <- renderValueBox({
    valueBox(
      value = globalIndicators$nMaleItems,
      subtitle = paste0("Male"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$stat1_M
  
  ### --- valueBox: stat2_F
  # output$stat2_F
  output$stat2_F <- renderValueBox({
    valueBox(
      value = globalIndicators$nFemaleItems,
      subtitle = paste0("Female"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$stat2_F
  
  ### --- valueBox: stat3_Intersex
  # output$stat3_Intersex
  output$stat3_Intersex <- renderValueBox({
    valueBox(
      value = globalIndicators$nIntersexItems,
      subtitle = paste0("Intersex"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$stat3_Intersex
  
  ### --- valueBox: stat4_transM
  # output$stat4_transM
  output$stat4_transM <- renderValueBox({
    valueBox(
      value = globalIndicators$nTransMItems,
      subtitle = paste0("Transgender Male"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$stat4_transM
  
  ### --- valueBox: stat4_transF
  # output$stat5_transF
  output$stat5_transF <- renderValueBox({
    valueBox(
      value = globalIndicators$nTransFItems,
      subtitle = paste0("Transgender Female"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$stat5_transF
  
  # - No. of M/F items pie-chart
  output$numGender <- renderPlot({
    pFrame <- data.frame(gender = c('Male', 'Female'),
                         count = c(globalIndicators$nMaleItems, 
                                   globalIndicators$nFemaleItems))
    ggplot2::ggplot(pFrame, ggplot2::aes(x = "", y = count, fill = gender)) +
      ggplot2::geom_bar(width = 1, stat = "identity") +
      ggplot2::coord_polar("y", start = 0) + 
      ggplot2::scale_fill_manual(values = c('lightgreen', 'aliceblue')) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "right") +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::theme(strip.background = ggplot2::element_blank()) + 
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  })
  
  # - Usage of M/F items pie-chart
  output$useGender <- renderPlot({
    pFrame <- data.frame(gender = c('Male', 'Female'),
                         count = c(globalIndicators$totalUsage_M, 
                                   globalIndicators$totalUsage_F))
    ggplot2::ggplot(pFrame, ggplot2::aes(x = "", y = count, fill = gender)) +
      ggplot2::geom_bar(width = 1, stat = "identity") +
      ggplot2::coord_polar("y", start = 0) + 
      ggplot2::scale_fill_manual(values = c('lightgreen', 'aliceblue')) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "right") +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::theme(strip.background = ggplot2::element_blank()) + 
      ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
  })
  
  ### --- valueBox: nMaleItems
  # output$nMaleItems
  output$nMaleItems <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$nMaleItems, 
                     " (", 
                     round(
                       globalIndicators$nMaleItems/(globalIndicators$nMaleItems + 
                                                      globalIndicators$nFemaleItems)*100, 2), 
                     "%)"),
      subtitle = paste0("Wikidata items: Male"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$nMaleItems
  
  ### --- valueBox: nFemaleItems
  # output$nFemaleItems
  output$nFemaleItems <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$nFemaleItems, 
                     " (", 
                     round(
                       globalIndicators$nFemaleItems/(globalIndicators$nMaleItems + 
                                                        globalIndicators$nFemaleItems)*100, 2), 
                     "%)"),      
      subtitle = paste0("Wikidata items: Female"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$nFemaleItems
  
  ### --- valueBox: totalUsage_M
  # output$totalUsage_M
  output$totalUsage_M <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$totalUsage_M, 
                     " (", 
                     round(
                       globalIndicators$totalUsage_M/(globalIndicators$totalUsage_M + 
                                                        globalIndicators$totalUsage_F)*100, 2), 
                     "%)"),
      subtitle = paste0("Wikidata items: Male"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$nMaleItems
  
  ### --- valueBox: totalUsage_F
  # output$totalUsage_F
  output$totalUsage_F <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$totalUsage_F, 
                     " (", 
                     round(
                       globalIndicators$totalUsage_F/(globalIndicators$totalUsage_M + 
                                                        globalIndicators$totalUsage_F)*100, 2), 
                     "%)"),      subtitle = paste0("Wikidata items: Female"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$nFemaleItems
  
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Gender Bias per Project
  ### ------------------------------------------
  
  ### --- output$projectTable
  output$projectTable <- DT::renderDataTable({
    
    DT::datatable(genderProjectData,
                  options = list(
                    pageLength = 50,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                  rownames = FALSE
                  )
    })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Gender and Occupation
  ### ------------------------------------------
  
  ### --- output$genderOccDT
  output$genderOccDT <- DT::renderDataTable({
    
    genOccData <- occUsage
    colnames(genOccData) <- c('Occupation Item', 
                              'Occupation Label', 
                              'Usage (M)', 
                              'Usage (F)', 
                              'Usage (Total)', 
                              'Probability Usage(M) > Usage(F)', 
                              'CI5%', 
                              'CI95%', 
                              'Usage(M)/Usage(F)')
    
    DT::datatable(genOccData,
                  options = list(
                    pageLength = 25,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                  rownames = FALSE
                  )
    })
  
  ### --- output$femaleOccDT
  output$femaleOccDT <- DT::renderDataTable({
    
    DT::datatable(fOcc,
                  options = list(
                    pageLength = 25,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                  rownames = FALSE
                  )
    })
  
  # - Gender and Occupation Plot
  # - output$genderOccPlot
  output$genderOccPlot <- renderPlot({
    # - select top 100 used from occUsage
    pFrame <- occUsage[1:100, ]
    pFrame <- data.frame(Occupation = rep(pFrame$label, 2),
                         Usage = c(pFrame$usageM, pFrame$usageF),
                         Gender = c(rep('M', length(pFrame$usageM)), rep('F', length(pFrame$usageF))),
                         stringsAsFactors = F)
    pFrame$Occupation <- factor(pFrame$Occupation, 
                                levels = unique(pFrame$Occupation))
    # - plot M/F occupations usage: top 100 most used occupations
    # - {ggplot2}
    ggplot2::ggplot(pFrame, ggplot2::aes(x = Occupation,
                       y = Usage,
                       group = Gender, 
                       color = Gender)) +
      ggplot2::geom_line(size = .25) + 
      ggplot2::geom_point(size = 1.5) + 
      ggplot2::geom_point(size = 1, color = "white") + 
      ggplot2::scale_color_manual(values = c('green', 'cadetblue3')) +
      ggplot2::xlab("Occupation") + ggplot2::ylab("Wikidata Usage") + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10, hjust = 1)) + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
  })
  
  # - Gender and Occupation Plot - Interactive
  # - output$genderOccPlot_interactive
  output$genderOccPlot_interactive <- plotly::renderPlotly({
    # - select top 100 used from occUsage
    pFrame <- occUsage[1:100, ]
    pFrame <- data.frame(Occupation = rep(pFrame$label, 2),
                         Usage = c(pFrame$usageM, pFrame$usageF),
                         Gender = c(rep('M', length(pFrame$usageM)), rep('F', length(pFrame$usageF))),
                         stringsAsFactors = F)
    pFrame$Occupation <- factor(pFrame$Occupation, 
                                levels = unique(pFrame$Occupation))
    # - plot M/F occupations usage: top 100 most used occupations
    # - {ggplot2}
    g <- ggplot2::ggplot(pFrame, ggplot2::aes(x = Occupation,
                                         y = Usage,
                                         group = Gender, 
                                         color = Gender)) +
      ggplot2::geom_line(size = .25) + 
      ggplot2::geom_point(size = 1.5) + 
      ggplot2::geom_point(size = 1, color = "white") + 
      ggplot2::scale_color_manual(values = c('green', 'cadetblue3')) +
      ggplot2::xlab("Occupation") + ggplot2::ylab("Wikidata Usage") + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10, hjust = 1)) + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
    plotly::ggplotly(g, 
                     tooltip = c("x","y", "group"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### ------------------------------------------
  ### --- TAB: tabPanel Gender Geography
  ### ------------------------------------------
  
  ### --- valueBox: globalUsageM
  # output$globalUsageM
  output$globalUsageM <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$globalGenderProportion_M, 
                     " (", 
                     round(
                       globalIndicators$globalGenderProportion_M/(globalIndicators$globalGenderProportion_M + 
                                                                    globalIndicators$globalGenderProportion_F)*100, 2), 
                     "%)"),      
      subtitle = paste0("Male"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageM
  
  ### --- valueBox: globalUsageF
  # output$globalUsageF
  output$globalUsageF <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$globalGenderProportion_F, 
                     " (", 
                     round(globalIndicators$globalGenderProportion_F/(globalIndicators$globalGenderProportion_M + 
                                                                        globalIndicators$globalGenderProportion_F)*100, 2), 
                     "%)"),      
      subtitle = paste0("Female"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageF
  
  ### --- valueBox: globalUsageN
  # output$globalUsageN
  output$globalUsageN <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$globalGenderProportion_N, 
                     " (", 
                     round(globalIndicators$globalGenderProportion_N/(globalIndicators$globalGenderProportion_N + 
                                                                        globalIndicators$globalGenderProportion_S)*100, 2), 
                     "%)"),      
      subtitle = paste0("North"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageM
  
  ### --- valueBox: globalUsageS
  # output$globalUsageS
  output$globalUsageS <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$globalGenderProportion_S, 
                     " (", 
                     round(globalIndicators$globalGenderProportion_S/(globalIndicators$globalGenderProportion_N + 
                                                                        globalIndicators$globalGenderProportion_S)*100, 2), 
                     "%)"),      
      subtitle = paste0("South"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageF
  
  ### --- valueBox: globalUsageMN
  # output$globalUsageMN
  output$globalUsageMN <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$genderPropotion_M_N, 
                     " (", 
                     round(globalIndicators$genderPropotion_M_N/sum(globalIndicators$genderPropotion_M_N,
                                                                    globalIndicators$genderPropotion_M_S
                     )*100, 2), 
                     "%)"),      
      subtitle = paste0("Male, North"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageMN
  
  ### --- valueBox: globalUsageMS
  # output$globalUsageMS
  output$globalUsageMS <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$genderPropotion_M_S, 
                     " (", 
                     round(globalIndicators$genderPropotion_M_S/sum(globalIndicators$genderPropotion_M_N,
                                                                    globalIndicators$genderPropotion_M_S
                     )*100, 2), 
                     "%)"),      
      subtitle = paste0("Male, South"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageMS
  
  ### --- valueBox: globalUsageFN
  # output$globalUsageFN
  output$globalUsageFN <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$genderPropotion_F_N, 
                     " (", 
                     round(globalIndicators$genderPropotion_F_N/sum(globalIndicators$genderPropotion_F_N,
                                                                    globalIndicators$genderPropotion_F_S
                     )*100, 2), 
                     "%)"),      
      subtitle = paste0("Female, North"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageFN
  
  ### --- valueBox: globalUsageFS
  # output$globalUsageFS
  output$globalUsageFS <- renderValueBox({
    valueBox(
      value = paste0(globalIndicators$genderPropotion_F_S, 
                     " (", 
                     round(globalIndicators$genderPropotion_F_S/sum(globalIndicators$genderPropotion_F_N,
                                                                    globalIndicators$genderPropotion_F_S
                     )*100, 2), 
                     "%)"),      
      subtitle = paste0("Female, South"),
      icon = icon("bars", lib = "font-awesome"),
      color = "blue"
    )
  }) # END output$globalUsageFS
  
  
  ### ------------------------------------------
  ### --- TAB: M/F Ratio
  ### ------------------------------------------
  
  output$mfRatioPlot <- renderPlot({
    mfData$Project <- factor(mfData$Project, 
                             levels = mfData$Project[order(-mfData$`M/F`)])
    ggplot2::ggplot(mfData, ggplot2::aes(x = Project,
                       y = `M/F`,
                       label = `M/F`)) +
      ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) + 
      ggplot2::geom_point(size = 1.5, color = "#4c8cff") + 
      ggplot2::geom_point(size = 1, color = "white") + 
      ggrepel::geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
      ggplot2::ylab("M/F Usage Ratio") + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
  })
  
  output$mfRatioPlot_interactive <- plotly::renderPlotly({
    mfData$Project <- factor(mfData$Project, 
                             levels = mfData$Project[order(-mfData$`M/F`)])
    g <- ggplot2::ggplot(mfData, ggplot2::aes(x = Project,
                                         y = `M/F`)) +
      ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) + 
      ggplot2::geom_point(size = 1.5, color = "#4c8cff") + 
      ggplot2::ylab("M/F Usage Ratio") + 
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 12, hjust = 1)) + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12, hjust = 1)) +
      ggplot2::theme(axis.title.x = ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = ggplot2::element_text(size = 12))
    plotly::ggplotly(g, 
                     tooltip = c("x","y"),
                     originalData = T) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  
  ### ------------------------------------------
  ### --- GENERAL: Externally hosted images
  ### ------------------------------------------
  
  # - genderUsage_Distribution.png
  output$genderUsage_Distribution_img <- 
    renderText({
      src = paste0(remoteDir, "genderUsage_Distribution.png")
      c('<img src="',src,'">')
    })
  
  # - genderUsage_Distribution_jitterp.png
  output$genderUsage_Distribution_jitterp_img <- 
    renderText({
      src = paste0(remoteDir, "genderUsage_Distribution_jitterp.png")
      c('<img src="',src,'">')
    })
  
  # - Gender_LorenzCurves.png
  output$Gender_LorenzCurves_img <- 
    renderText({
      src = paste0(remoteDir, "Gender_LorenzCurves.png")
      c('<img src="',src,'">')
    })
  
  # - M_Items_Distribution.png
  output$M_Items_Distribution_img <- 
    renderText({
      src = paste0(remoteDir, "M_Items_Distribution.png")
      c('<img src="',src,'">')
    })
  
  # - F_Items_Distribution.png
  output$F_Items_Distribution_img <- 
    renderText({
      src = paste0(remoteDir, "F_Items_Distribution.png")
      c('<img src="',src,'">')
    })
  
}
