#load packages
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(randomForest)
library(shinyBS)
library(plotly)
library(gt)



# Load data ---------------------------------------------------------------
# // I would reformat soilsData.txt into standard csv format

#load data
soils <- read.table("data/soilsData.txt", header = TRUE, sep = "|")
cropSystems <- read_csv("data/TableMenu.csv")
names <- read_csv("data/nameConversions.csv")
counties <- unique(soils$County)
crops <- unique(cropSystems$crop)

# load erosion models
ccnc_e <- readRDS("models/ContCorn_NoCoverErosion.rds")
cccc_e <- readRDS("models/ContCorn_WithCoverErosion.rds")
ptrt_e <- readRDS("models/RotationalPastureErosion.rds")
ptcn_e <- readRDS("models/ContinouosPastureErosion.rds")
dr_e <- readRDS("models/dairyRotationErosion.rds")

# load PI models
ccnc_pi <- readRDS("models/ContCorn_NoCoverPI.rds")
cccc_pi <- readRDS("models/ContCorn_withCoverPI.rds")
ptrt_pi <- readRDS("models/RotationalPasturePI.rds")
ptcn_pi <- readRDS("models/ContinuousPasturePI.rds")
dr_pi <- readRDS("models/dairyRotationPI.rds")

# UI ----------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Page header
  fixedRow(
    column(1,
           h2(img(src = "grasslandColorCenter.png", height = 60))),
    
    column(10, align = "center",
           h2("Erosion and Phosphorus Loss Predictions for Soil Types and Cropping Management")),
    
    column(1,
           h2(img(src = "uw-crest.png", height = 60)))
  ),
  
  hr(),
  
  fixedRow(
    column(12, 
           h3("Select Your Soil and Soil Properties"),
           helpText(
                      a(
                        "Click here to find your soil (NRCS Web Soil Survey)",
                        href = "https://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx",
                        target = "_blank"
                      ))
    )
  ),
  # soil selection begins
  fluidRow(
    #bsCollapse(id = "soilselect", open = "Select your soil properties (click to close)",
               #bsCollapsePanel("Select your soil properties (click to close)",
                               # column(12,
                               #        helpText(
                               #          a(
                               #            "Click here to find your soil (NRCS Web Soil Survey)",
                               #            href = "https://websoilsurvey.sc.egov.usda.gov/App/WebSoilSurvey.aspx",
                               #            target = "_blank"
                               #          ))
                               # ),
                               # row of selectInputs for selecting soils
                               fixedRow(
                                 # Input 1: dropdown for county selections
                                 column(4,
                                        selectInput(
                                          inputId = "county",
                                          label = h4("County"),
                                          choices = counties
                                        )),
                                 #Input 2: dropdown for component name selections. list is created in server.
                                 column(4, uiOutput("compname")),
                                 # Input 3: dropdown for soil symbol. list is created in server.
                                 column(4, uiOutput("musym"))
                               ),
                               
                               # row of sliders for defining slope and soil P
                               fixedRow(
                                 
                                 #slider for slope values
                                 column(6, align = "center", uiOutput("slope")),
                                 
                                 #slider for phosphorus values
                                 column(6, align = "center", uiOutput("phos"))
                               )
    #           )
   # )
  ),
  
  hr(),
  
  # crop management definition begins
  fixedRow(
    column(12,
           h3(strong(em("Define your cropping System"))))
  ),
  
  # row of input including crop rotation, cover crop and tillage (density for dry lot and pasture)
  fixedRow(
    
    ## COL 1 ##
    # crop rotation selection
    column(4,
           radioButtons(
             inputId = "crop",
             label = h4("Crop Rotation"),
             choices = crops
           )
    ),
    
    ## COL 2 ##
    # cover crop, tillage or density, depending on crop rotation
    column(4,
           uiOutput("cover"),
           uiOutput("tillage")),
    
    ## COL 3 ##
    # tillage selection or density (if rotational pasture)
    column(4,
           uiOutput("contour"),
           uiOutput("rotation"),
           uiOutput("density"))
  ),
  
  conditionalPanel(
    condition = "input.crop != 'Dry lot'",
    h4("Fertilizer inputs:")
  ),
  
  # fertilizer (turned off for dry lot)
  fixedRow(
    column(6, align = "center",
           uiOutput("fert")),
    
    column(6,
           uiOutput("manure"))
  ),
  
  br(),
  
  fluidRow(
    column(12, align = "center",
           actionBttn(
             inputId = "runModels",
             label = "Run models",
             style = "simple",
             color = "success"
           )
    )
  ),
  
  br(),
  hr(),
  
  fluidRow(
    column(12, align = "center",
           gt_output("scenario"))
  ),
  br(),
  br(),
  
  fluidRow(
    column(6,
           plotlyOutput("erosionPred")
    ),
    column(6,
           plotlyOutput("PIPred")
    )
  )
  
)




# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # update soil comp list when county selected
  observeEvent(input$county, {
    soils_by_county <- soils %>% filter(County == input$county)
    comps <- unique(soils_by_county$SoilSeries)
    
    output$compname <- renderUI({
      selectInput(
        inputId = "compname",
        label = h4("Soil Name"),
        choices = comps
      )
    })
  })
  
  
  # update musym list when soil comp selected
  observeEvent(input$compname, {
    soils_by_comp <-
      soils %>% filter(County == input$county, SoilSeries == input$compname)
    musyms <- unique(soils_by_comp$SoilSymbol)
    
    output$musym <- renderUI({
      selectInput(
        inputId = "musym",
        label = h4("Soil Symbol"),
        choices = musyms
      )
    })
  })
  
  observeEvent(input$musym, {
    soil <- soils %>%
      filter(County == input$county,
             SoilSeries == input$compname,
             SoilSymbol == input$musym)
    minSlope <- soil$minSlope
    maxSlope <- soil$maxSlope
    repSlope <- soil$slopeRep
    
    output$slope <- renderUI({
      sliderInput(
        inputId = "slope",
        label = h4("Slope (%)"),
        min = minSlope,
        max = maxSlope,
        value = repSlope
      )
    })
  })
  
  output$phos <- renderUI({
    sliderInput(
      "initialP",
      label = h4("Soil Phosphorous"),
      min = 0,
      max = 175,
      value = 35
    )
  })
  
  
  # cover crop / contour / rotational
  # dependent only on cropping system
  observeEvent(input$crop, {
    
    options <- filter(cropSystems, crop == input$crop)
    
    covers <- unique(options$cover)
    tillages <- unique(options$tillage)
    contours <- unique(options$contour)
    rotations <- unique(options$rotation)
    densities <- unique(options$density)
    
    
    # cover crop
    output$cover <- renderUI({
      selectInput(
        inputId = "cover",
        label = h4("Cover Crop System"),
        choices = covers
      )
    })
    
    # rotation and density
    output$rotation <- renderUI({
      radioButtons(
        inputId = "rotation",
        label = h4("Rotational Management"),
        choices = rotations
      )
    })
    
    output$contour <- renderUI({
      checkboxInput("contour", label = "On Contour", value = TRUE)
    })
    
    
    # hide elements when appropriate
    if (anyNA(covers)) output$cover <- NULL
    if (anyNA(tillages)) output$tillage <-  NULL
    if (anyNA(contours)) output$contour <- NULL
    if (anyNA(rotations)) output$rotation <- NULL
    if (anyNA(densities)) output$density <- NULL
    
    
    # fertilizer (off for dry lot)
    if (input$crop != "Dry lot") {
      output$fert <- renderUI({
        sliderInput(
          inputId = "fertilizerP",
          label = h4("Percent of crop P needs with synthetic fertilizer"),
          min = 0, max = 100, value = 50
        )
      })
      
      output$manure <- renderUI({
        sliderInput(
          inputId = "manureP",
          label = h4("Percent of crop P needs with manure"),
          min = 0, max = 200, value = 100
        )
      })
    } else {
      output$fert <- NULL
      output$manure <- NULL
    }
  })
  
  
  # tillage
  # depends on crop system and cover crop
  observeEvent(list(input$crop, input$cover), {
    
    options <- cropSystems %>%
      filter(crop == input$crop)
    
    if(!anyNA(options$cover)) {
      req(input$cover)
      options <- filter(options, cover == input$cover)
    }
    
    tillages <- unique(options$tillage)
    
    if (anyNA(tillages)) {
      output$tillage <- NULL
    } else {
      output$tillage <- renderUI({
        selectInput(
          inputId = "tillage",
          label = h4("Tillage"),
          choices = tillages
        )
      })
    }
  })
  
  
  # stocking density
  # depends on rotation
  observeEvent(list(input$crop, input$rotation), {
    
    options <- filter(cropSystems, crop == input$crop)
    
    if (!anyNA(options$rotation)) {
      req(input$rotation)
      options <- filter(options, rotation == input$rotation)
    }
    
    densities <- unique(options$density)
    
    if (anyNA(densities)) {
      output$density <- NULL
    } else {
      output$density <- renderUI({
        radioButtons(
          inputId = "density",
          label = h4("Herd density"),
          choices = densities
        )
      })
    }
  })
  
  
  
  observeEvent(input$runModels, {
    
    # soil properties
    soil <- soils %>%
      filter(County == input$county,
             SoilSeries == input$compname,
             SoilSymbol == input$musym)
    
    slope <- input$slope
    slopelenusle.r <- soil$slopelenusle.r
    sand <- soil$sand
    silt <- soil$silt
    clay <- soil$clay
    K <- soil$k
    OM <- soil$OM 
    initialP <- input$initialP
    total.depth <- soil$total.depth
    factor <- ifelse(between(slope, 3.01, 4), 0.4, 
                     ifelse(between(slope, 1, 3), 0.3,
                            ifelse(slope < 1, 0.2, 0.5)))
    LSsurgo = round((((slope/((10000+(slope^2))^0.5))*4.56)+
                       (slope/(10000+(slope^2))^0.5)^2*(65.41)+0.065)*((slopelenusle.r*3.3)/72.6)^(factor),2)
    
    soildf <- data.frame(slope = slope, slopelenusle.r = slopelenusle.r, sand = sand, silt = silt, clay = clay, k = K,
                         OM = OM, initialP = initialP, total.depth = total.depth, LSsurgo = LSsurgo)
    
    # process user inputs
    crop <- input$crop
    options <- filter(cropSystems, crop == input$crop)
    
    if (anyNA(options$cover)) {
      cover <- NA
    } else {
      cover <- input$cover
      options <- filter(options, cover == input$cover)
    }
    
    if(anyNA(options$tillage)) {
      tillage <- NA
    } else {
      tillage <- input$tillage
      options <- filter(options, tillage == input$tillage)
    }
    
    if(anyNA(options$contour)) {
      contour <- NA
    } else {
      contour <- input$contour
    }
    
    if(anyNA(options$rotation)) {
      rotation <- NA
    } else if (input$rotation == "Rotational") {
      rotation <- input$rotation
      options <- filter(options, rotation == input$rotation)
    } else {
      rotation <- input$rotation
      options <- filter(options, rotation == input$rotation, density == input$density)
    }
    
    if(anyNA(options$density)) {
      density <- NA
    } else {
      density <- input$density
    }
    
    if(crop != "Dry lot") {
      fert <- input$fertilizerP
      manure <- input$manureP
    } else {
      fert <- NA
      manure <- NA
      options <- filter(cropSystems, crop == input$crop, density == input$density)
    } 
    
    # fertilizer values
    p_needs <- unique(options$Pneeds)
    # total_DM_lbs = appliedManureDM_lbs + grazedManurDM_lbs
    grazedManureDM_lbs <- unique(options$grazed_DM_lbs)
    appliedDM_lbs <-  ((p_needs * (manure/100))/6) * 1000 * 8.4 * (6/100)
    total_DM_lbs <- sum(grazedManureDM_lbs, appliedDM_lbs, na.rm = TRUE)
    # totalP2O5 = grazedP2O5 + P2O5_applied_lbs
    grazedP2O5 <- unique(options$grazed_P2O5_lbs)
    P2O5_applied_lbs = (fert + manure)*(p_needs/100)
    totalP2O5_lbs = grazedP2O5 + P2O5_applied_lbs
    
    croppingdf <- tibble(
      Variable = c("crop"),
      Value = crop) %>%
      rbind(c("cover", cover)) %>%
      rbind(c("tillage", tillage)) %>%
      rbind(c("rotational", rotation)) %>%
      rbind(c("density", density)) %>%
      rbind(c("Contour", contour))
    
    scenario <- croppingdf %>%
      drop_na() %>%
      pivot_wider(names_from = "Variable", values_from = "Value") %>%
      bind_cols(Manure = paste0(manure, "%"), Fertilizer = paste0(fert, "%"))
    
    output$scenario <- render_gt({
      scenario %>%
        gt() %>%
        cols_label("crop" = "Crop",
                   "cover" = "Cover",
                   "tillage" = "Tillage")
    })
    
    # connect the correct names for model runs and transpose
    cropsForPred <- left_join(croppingdf, names) %>%
      drop_na() %>%
      select(-Value) %>%
      pivot_longer('Variable', 'Value') %>%
      pivot_wider(names_from = "value", values_from = "modelName")
    
    # bind cropping and soil dfs  and transform characters to factors
    full_df <- cbind(cropsForPred, soildf, total_DM_lbs = total_DM_lbs, totalP2O5_lbs = totalP2O5_lbs) %>%
      mutate_if(is.character, as.factor)
    
    # complete data frame creation and predict for any scenario
    if (full_df$crop == "cc") # crop = cc
    {if (full_df$cover == "nc") { # cover = nc
      tillage <- factor(ccnc_e$forest$xlevels$tillage)
      Contour <- factor(ccnc_e$forest$xlevels$Contour)
      
      level_df <- expand_grid(tillage, Contour)
      
      df <- full_df %>%
        select(c(slope:totalP2O5_lbs)) %>% 
        slice(rep(1:n(), each=nrow(level_df)))
      
      df <- cbind(level_df, df)
      
      pred_df <- df %>%
        filter(tillage == levels(full_df$tillage), Contour == levels(full_df$Contour))
      
      erosion_pred_df <- pred_df %>%
        select(c(tillage:k, total_DM_lbs))
      
      erosion <- round(predict(ccnc_e, erosion_pred_df),2)
      
      pi_pred_df <- pred_df %>%
        select(c(tillage:slopelenusle.r, silt, k:totalP2O5_lbs)) %>%
        mutate(Erosion = erosion)
      
      pi <- round(predict(ccnc_pi, pi_pred_df),2)
      
    } else { # other covers
      tillage <- factor(cccc_e$forest$xlevels$tillage)
      Contour <- factor(cccc_e$forest$xlevels$Contour)
      cover <- factor(cccc_e$forest$xlevels$cover)
      
      level_df <- expand_grid(cover, tillage, Contour)
      
      df <- full_df %>%
        select(c(slope:totalP2O5_lbs)) %>% 
        slice(rep(1:n(), each=nrow(level_df)))
      
      df <- cbind(level_df, df)
      
      pred_df <- df %>%
        filter(cover == levels(full_df$cover), tillage == levels(full_df$tillage), Contour == levels(full_df$Contour))
      
      erosion_pred_df <- pred_df %>%
        select(c(cover:k, total_DM_lbs))
      
      erosion <- round(predict(cccc_e, erosion_pred_df),2)
      
      pi_pred_df <- pred_df %>%
        select(c(cover:slopelenusle.r, silt, k:totalP2O5_lbs)) %>%
        mutate(Erosion = erosion)
      
      pi <- round(predict(cccc_pi, pi_pred_df),2) }
      
    } else if (full_df$crop == "dr") {
      
      cover <- factor(dr_e$forest$xlevels$cover)
      tillage <- factor(dr_e$forest$xlevels$tillage)
      Contour <- factor(dr_e$forest$xlevels$Contour)
      
      level_df <- expand_grid(cover, tillage, Contour)
      
      df <- full_df %>%
        select(c(slope:totalP2O5_lbs)) %>% 
        slice(rep(1:n(), each=nrow(level_df)))
      
      df <- cbind(level_df, df)
      
      pred_df <- df %>%
        filter(cover == levels(full_df$cover), tillage == levels(full_df$tillage), Contour == levels(full_df$Contour))
      
      erosion <- round(predict(dr_e, pred_df),2)
      
      pi_pred_df <- pred_df %>%
        mutate(Erosion = erosion)
      
      pi <- round(predict(dr_pi, pi_pred_df),2)
      
    } else if (full_df$crop == "pt") {
      if(full_df$rotational == "rt") {# rotational pasture
        
        erosion_pred_df <- full_df %>%
          select(c(slope:totalP2O5_lbs))
        
        erosion <- round(predict(ptrt_e, erosion_pred_df), 4)
        
        pi_pred_df <- full_df %>%
          select(c(slope, slopelenusle.r, silt, k:totalP2O5_lbs)) %>%
          mutate(Erosion = erosion)
        
        pi <- round(predict(ptrt_pi, pi_pred_df), 4)
        
      } else { # continuous pasture
        
        density <- factor(ptcn_e$forest$xlevels$density)
        
        df <- full_df %>%
          select(c(slope:totalP2O5_lbs)) %>% 
          slice(rep(1:n(), each=length(density)))
        
        df <- cbind(density, df)
        
        pred_df <- df %>%
          filter(density == levels(full_df$density))
        
        erosion <- round(predict(ptcn_e, pred_df),3)
        
        pi_pred_df <- pred_df %>%
          mutate(Erosion = erosion)
        
        pi <- round(predict(ptcn_pi, pi_pred_df),3)
        
      } } else {
        erosion <- NA
        pi <- NA
      }
    
    erosion_table <- data.frame(system =  paste((na.omit(croppingdf[,2]))), Erosion = erosion)
    
    output$erosionPred <- renderPlotly({
      
      x <- list(
        title = "Erosion + 1 (tons/acre)",
        range = c(0, 50)
      )
      y <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      plot_ly(erosion_table, 
              x = ~Erosion + 1, y = ~system,
              orientation = "h",
              marker = list(color = 'rgba(50, 171, 96, 0.7)'),
              type = "bar",
              hoverinfo = "text",
              text = ~paste("Erosion:", Erosion)) %>% 
        layout(title = "Predicted Erosion", 
               xaxis = x, yaxis = y, barmode = 'group') 
      
    })
    
    PI_table <- data.frame(system =  paste((na.omit(croppingdf[,2]))), PI = pi)
    
    output$PIPred <- renderPlotly({
      
      x <- list(
        title = "P Loss (lbs P/acre)",
        range = c(0, 50)
      )
      y <- list(
        title = "",
        zeroline = FALSE,
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE
      )
      
      plot_ly(PI_table, 
              x = ~PI, y = ~system,
              orientation = "h",
              marker = list(color = 'rgba(55, 128, 191, 0.7)'),
              type = "bar",
              hoverinfo = "text",
              text = ~paste("P Loss:", PI)) %>% 
        layout(title = "Predicted Phosphorus loss", xaxis = x, yaxis = y, barmode = 'group') 
      
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)