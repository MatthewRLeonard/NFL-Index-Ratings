# 1. Load Packages ----

library(shiny)
library(shinyWidgets)
library(scales)
library(reactable)
library(reactablefmtr)
library(htmltools)
library(gfonts)

# 2. Load Data ----

team_stats <- read.csv(url("https://drive.google.com/uc?export=download&id=1aMXgswjKKrok2nvH_9WRRdtFBbjMeaTE"), header = TRUE)

# 3. Functions ----

## Sidebar Panel ----
sidebarPanelFunction <- function(sidebarPanelx) {
  sidebarPanelx <- sidebarPanel(
    pickerInput(
      inputId = "season",
      label = "Season",
      choices = sort(unique(team_stats$season), method = "radix"),
      selected = 2021,
      multiple = FALSE,
      options = list(`multiple-separator` = ", ")
    ),
    pickerInput(
      inputId = "conf",
      label = "Conference",
      choices = sort(unique(team_stats$conf), method = "radix"),
      selected = unique(team_stats$conf),
      multiple = TRUE,
      options = list(`multiple-separator` = "    "),
      choicesOpt = list(
        content = sprintf("<img src='https://a.espncdn.com/i/teamlogos/nfl/500/%s.png', width = 25, height = 25, alt = %s>",
                          sort(unique(team_stats$conf), method = "radix"),
                          sort(unique(team_stats$conf), method = "radix")))
    ),
    pickerInput(
      inputId = "div",
      label = "Division",
      choices = sort(unique(team_stats$div), method = "radix"),
      selected = unique(team_stats$div),
      multiple = TRUE,
      options = list(`multiple-separator` = " / ")
    ),
    sliderInput(
      inputId = "net_rating",
      label = "Overall Index Rating",
      min = 0,
      max = 100,
      value = c(0, 100),
      step = 2,
      ticks = FALSE
    ),
    sliderInput(
      inputId = "off_rating",
      label = "Offensive Index Rating",
      min = 0,
      max = 100,
      value = c(0, 100),
      step = 2,
      ticks = FALSE
    ),
    sliderInput(
      inputId = "def_rating",
      label = "Defensive Index Rating",
      min = 0,
      max = 100,
      value = c(0, 100),
      step = 2,
      ticks = FALSE
    ),
    width = 2
  )
}

## Filter Function ----
filter <- function(x, val) {
  if ((is.numeric(x)) && (x == team_stats$season)) {
    x %in% val
  } else if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.character(x)) {
    x %in% val
  } else if (is.factor(x)) {
    x %in% val
  } else {
    TRUE
  }
}

# 4. Shiny UI ----

ui <- shiny::fluidPage(

  gfonts::use_font("roboto", "www/css/roboto.css"),
  
  ## Link to CSS file ----    
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css")
  ),
  ## Page Layout ----
  shiny::titlePanel("NFL Index Ratings"),
  shiny::sidebarLayout(
    sidebarPanelFunction()
    ,
    shiny::mainPanel(
      shiny::fluidRow(align = "center",
               ### Tabset Panel ----
               shiny::tabsetPanel(
                 shiny::tabPanel("Index Ratings & Net Factors",
                          htmltools::p(),
                          reactable::reactableOutput("ratings"),
                          htmltools::p()
                 ),
                 tabPanel("Offensive Factors",
                          p(),
                          reactableOutput("offense"),
                          p()
                 ),
                 tabPanel("Defensive Factors",
                          p(),
                          reactableOutput("defense"),
                          p()
                 ),
                 tabPanel("Passing Offense",
                          p(),
                          reactableOutput("passing_off"),
                          p()
                 ),
                 tabPanel("Rushing Offense",
                          p(),
                          reactableOutput("rushing_off"),
                          p()
                 ),
                 tabPanel("Passing Defense",
                          p(),
                          reactableOutput("passing_def"),
                          p()
                 ),
                 tabPanel("Rushing Defense",
                          p(),
                          reactableOutput("rushing_def"),
                          p()
                 )
               ),
               width = 10
      ))
  )
)

# 5. Shiny Server ----

server <- function(input, output, session) {

  ## Reactive to Filtered ----
  filtered <- shiny::reactive({
    
    filteredTeamStats <- team_stats[filter(team_stats$season, input$season) &
                                      filter(team_stats$conf, input$conf) &
                                      filter(team_stats$div, input$div) &
                                      filter(team_stats$net_rating, input$net_rating / 100) &
                                      filter(team_stats$off_rating, input$off_rating / 100) &
                                      filter(team_stats$def_rating, input$def_rating / 100)
    ,]
  })

  ## Theme for ReacTables ----
  theme <- reactable::reactableTheme(color = "hsl(0, 0%, 87%)", backgroundColor = "hsl(220, 13%, 18%)",
                            borderColor = "hsl(0, 0%, 22%)", stripedColor = "rgba(255, 255, 255, 0.04)",
                            highlightColor = "rgba(255, 255, 255, 0.12)", inputStyle = list(backgroundColor = "hsl(0, 0%, 24%)"),
                            selectStyle = list(backgroundColor = "hsl(0, 0%, 24%)"),
                            pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 24%)"),
                            pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 28%)"),
                            headerStyle = list(color = "hsl(0, 0%, 70%)", "&:hover, &:focus" = list(color = "hsl(0, 0%, 95%)")))

  ReactablefmtrCell = function(value, type, colname) {
    reactable::colDef(
      name = colname,
      align = "center",
      class = "number",
      minWidth = 122,
      defaultSortOrder = if (colname == "TFLs" && type == "off") {
          "asc"
        } else if (colname == "TFLs" && type == "def") {
          "desc"
        } else if (type == "def" | type == "defoth") {
          "asc"
        } else {
          "desc"
        },
      cell = reactablefmtr::color_tiles(
        team_stats,
        colors = if (colname == "TFLs" && type == "off") {
          scales::brewer_pal(palette = "RdBu", direction = -1)(11)
        } else if (colname == "TFLs" && type == "def") {
          scales::brewer_pal(palette = "RdBu")(11)
        } else if (type == "def" | type == "defoth") {
          scales::brewer_pal(palette = "RdBu", direction = -1)(11)
        } else {
          scales::brewer_pal(palette = "RdBu")(11)
        },
        text_color = "#23262b",
        brighten_text_color = "hsl(0, 0%, 80%)",
        opacity = 0.9,
        number_fmt = if (colname == "EPA / Play" | colname == "Yards / Play" | type == "offoth" | type == "defoth") {
            scales::label_number(accuracy = 0.01)
          } else if (type == "rating") {
            scales::label_number(accuracy = 0.1, scale = 100)
          } else {
            scales::label_percent(accuracy = 0.1)
          }
      ),
      style = if (colname == "EPA / Play" | colname == "Yards / Att" | colname == "Yards / Rush") {
          list(borderLeft = "1px solid rgba(255, 255, 255, 0.12)")
        } else {
          NULL
        }
    )
  }
  
  ## Index Rtgs & Net Factors ----  

  output$ratings <- reactable::renderReactable({    
        
    reactable::reactable(filtered()[1:16], resizable = TRUE, pagination = FALSE, highlight = TRUE, fullWidth = FALSE, showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = c("net_rating"),
              columns = list(
                season = colDef(
                  show = FALSE
                ),
                team = colDef(
                  show = FALSE
                ),
                team_name = colDef(
                  name = "Team",
                  defaultSortOrder = "asc",
                  minWidth = 220,
                  cell = function(value, index) {
                    teamIndex <- filtered()[2:3]$team[index]
                    logo <- filtered()[7]$team_logo_espn[index]
                    htmltools::tagList(
                      htmltools::div(htmltools::img(class = "logo", alt = paste(teamIndex, "logo"), src = logo), value)
                    )
                  }
                ),
                conf = colDef(
                  show = FALSE
                ),
                division = colDef(
                  name = "Division",
                  defaultSortOrder = "asc"
                ),
                div = colDef(
                  show = FALSE
                ),
                team_logo_espn = colDef(
                  show = FALSE
                ),
                net_rating = ReactablefmtrCell(value, "rating", "Overall"),
                off_rating = ReactablefmtrCell(value, "rating", "Offense"),
                def_rating = ReactablefmtrCell(value, "rating", "Defense"),
                net_epa_pp = ReactablefmtrCell(value, "net", "EPA / Play"),
                net_success_rate = ReactablefmtrCell(value, "net", "Success Rate"),
                net_yards_per_play = ReactablefmtrCell(value, "net", "Yards / Play"),
                net_available_yards_perc = ReactablefmtrCell(value, "net", "Available Yds"),
                net_explosiveness = ReactablefmtrCell(value, "net", "Explosiveness"),
                net_negatives = ReactablefmtrCell(value, "net", "TFLs")
              ),
              columnGroups = list(
                reactable::colGroup(name = "Index Ratings", columns = c("net_rating", "off_rating", "def_rating")),
                reactable::colGroup(name = "Net Factors", columns = c("net_epa_pp", "net_success_rate", "net_yards_per_play", "net_available_yards_perc", "net_explosiveness", "net_negatives"))
              ),
              language = reactable::reactableLang(
                noData = "No teams found, please tweak filters.",
              ),
              theme = theme
    )
  })
  
  ## Offensive Factors Table ---- 
  
  output$offense <- reactable::renderReactable({
    
    reactable::reactable(filtered()[c(1:9,18:19,32:33,20:21)], resizable = TRUE, pagination = FALSE, highlight = TRUE, fullWidth = FALSE, showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = c("off_rating"),
              columns = list(
                season = colDef(
                  show = FALSE
                ),
                team = colDef(
                  show = FALSE
                ),
                team_name = colDef(
                  name = "Team",
                  defaultSortOrder = "asc",
                  minWidth = 220,
                  cell = function(value, index) {
                    teamIndex <- filtered()[2:3]$team[index]
                    logo <- filtered()[7]$team_logo_espn[index]
                    htmltools::tagList(
                      div(img(class = "logo", alt = paste(teamIndex, "logo"), src = logo), value)
                    )
                  }
                ),
                conf = colDef(
                  show = FALSE
                ),
                division = colDef(
                  name = "Division",
                  defaultSortOrder = "asc"
                ),
                div = colDef(
                  show = FALSE
                ),
                team_logo_espn = colDef(
                  show = FALSE
                ),
                net_rating = ReactablefmtrCell(value, "rating", "Overall"),
                off_rating = ReactablefmtrCell(value, "rating", "Offense"),
                off_epa_pp = ReactablefmtrCell(value, "off", "EPA / Play"),
                off_success_rate = ReactablefmtrCell(value, "off", "Success Rate"),
                off_season_yards_per_play = ReactablefmtrCell(value, "off", "Yards / Play"),
                off_season_available_yards_perc = ReactablefmtrCell(value, "off", "Available Yds"),
                off_explosiveness = ReactablefmtrCell(value, "off", "Explosiveness"),
                off_negatives = ReactablefmtrCell(value, "off", "TFLs")
              ),
              columnGroups = list(
                colGroup(name = "Index Ratings", columns = c("net_rating", "off_rating")),
                colGroup(name = "Offensive Factors", columns = c("off_epa_pp", "off_success_rate", "off_season_yards_per_play", "off_season_available_yards_perc", "off_explosiveness", "off_negatives"))
              ),
              language = reactableLang(
                noData = "No teams found, please tweak filters.",
              ),
              theme = theme
    )
  })

  ## Defensive Factors Table ---- 
  
  output$defense <- renderReactable({
    
    reactable(filtered()[c(1:8,10,23:24,39:40,25:26)], resizable = TRUE, pagination = FALSE, highlight = TRUE, fullWidth = FALSE, showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = c("def_rating"),
              columns = list(
                season = colDef(
                  show = FALSE
                ),
                team = colDef(
                  show = FALSE
                ),
                team_name = colDef(
                  name = "Team",
                  defaultSortOrder = "asc",
                  minWidth = 220,
                  cell = function(value, index) {
                    teamIndex <- filtered()[2:3]$team[index]
                    logo <- filtered()[7]$team_logo_espn[index]
                    tagList(
                      div(img(class = "logo", alt = paste(teamIndex, "logo"), src = logo), value)
                    )
                  }
                ),
                conf = colDef(
                  show = FALSE
                ),
                division = colDef(
                  name = "Division",
                  defaultSortOrder = "asc"
                ),
                div = colDef(
                  show = FALSE
                ),
                team_logo_espn = colDef(
                  show = FALSE
                ),
                net_rating = ReactablefmtrCell(value, "rating", "Overall"),
                def_rating = ReactablefmtrCell(value, "rating", "Defense"),
                def_epa_pp = ReactablefmtrCell(value, "def", "EPA / Play"),
                def_success_rate = ReactablefmtrCell(value, "def", "Success Rate"),
                def_season_yards_per_play = ReactablefmtrCell(value, "def", "Yards / Play"),
                def_season_available_yards_perc = ReactablefmtrCell(value, "def", "Available Yds"),
                def_explosiveness = ReactablefmtrCell(value, "def", "Explosiveness"),
                def_negatives = ReactablefmtrCell(value, "def", "TFLs")
              ),
              columnGroups = list(
                colGroup(name = "Index Ratings", columns = c("net_rating", "def_rating")),
                colGroup(name = "Defensive Factors", columns = c("def_epa_pp", "def_success_rate", "def_season_yards_per_play", "def_season_available_yards_perc", "def_explosiveness", "def_negatives"))
              ),
              language = reactableLang(
                noData = "No teams found, please tweak filters.",
              ),
              theme = theme
    )
  })
  
  ## Passing Offense Table ---- 
  
  output$passing_off <- renderReactable({
    
    reactable(filtered()[c(1:9,42:45,53,49:50)], resizable = TRUE, pagination = FALSE, highlight = TRUE, fullWidth = FALSE, showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = c("off_rating"),
              columns = list(
                season = colDef(
                  show = FALSE
                ),
                team = colDef(
                  show = FALSE
                ),
                team_name = colDef(
                  name = "Team",
                  defaultSortOrder = "asc",
                  minWidth = 220,
                  cell = function(value, index) {
                    teamIndex <- filtered()[2:3]$team[index]
                    logo <- filtered()[7]$team_logo_espn[index]
                    tagList(
                      div(img(class = "logo", alt = paste(teamIndex, "logo"), src = logo), value)
                    )
                  }
                ),
                conf = colDef(
                  show = FALSE
                ),
                division = colDef(
                  name = "Division",
                  defaultSortOrder = "asc"
                ),
                div = colDef(
                  show = FALSE
                ),
                team_logo_espn = colDef(
                  show = FALSE
                ),
                net_rating = ReactablefmtrCell(value, "rating", "Overall"),
                off_rating = ReactablefmtrCell(value, "rating", "Offense"),
                off_pass_epa_pp = ReactablefmtrCell(value, "off", "EPA / Play"),
                off_pass_success_rate = ReactablefmtrCell(value, "off", "Success Rate"),
                off_pass_explosiveness = ReactablefmtrCell(value, "off", "Explosiveness"),
                off_pass_negatives = ReactablefmtrCell(value, "off", "TFLs"),
                off_pass_ypa = ReactablefmtrCell(value, "offoth", "Yards / Att"),
                off_pass_air_yards_pc = ReactablefmtrCell(value, "offoth", "Air Yds / Comp"),
                off_pass_yac_pc = ReactablefmtrCell(value, "offoth", "YAC / Comp")
              ),
              columnGroups = list(
                colGroup(name = "Index Ratings", columns = c("net_rating", "off_rating")),
                colGroup(name = "Passing Factors", columns = c("off_pass_epa_pp", "off_pass_success_rate", "off_pass_explosiveness", "off_pass_negatives")),
                colGroup(name = "Other Stats", columns = c("off_pass_ypa", "off_pass_air_yards_pc", "off_pass_yac_pc"))
                ),
              language = reactableLang(
                noData = "No teams found, please tweak filters.",
              ),
              theme = theme
    )
  })
  
  ## Rushing Offense Table ---- 
  
  output$rushing_off <- renderReactable({
    
    reactable(filtered()[c(1:9,55:58,60)], resizable = TRUE, pagination = FALSE, highlight = TRUE, fullWidth = FALSE, showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = c("off_rating"),
              columns = list(
                season = colDef(
                  show = FALSE
                ),
                team = colDef(
                  show = FALSE
                ),
                team_name = colDef(
                  name = "Team",
                  defaultSortOrder = "asc",
                  minWidth = 220,
                  cell = function(value, index) {
                    teamIndex <- filtered()[2:3]$team[index]
                    logo <- filtered()[7]$team_logo_espn[index]
                    tagList(
                      div(img(class = "logo", alt = paste(teamIndex, "logo"), src = logo), value)
                    )
                  }
                ),
                conf = colDef(
                  show = FALSE
                ),
                division = colDef(
                  name = "Division",
                  defaultSortOrder = "asc"
                ),
                div = colDef(
                  show = FALSE
                ),
                team_logo_espn = colDef(
                  show = FALSE
                ),
                net_rating = ReactablefmtrCell(value, "rating", "Overall"),
                off_rating = ReactablefmtrCell(value, "rating", "Offense"),
                off_rush_epa_pp = ReactablefmtrCell(value, "off", "EPA / Play"),
                off_rush_success_rate = ReactablefmtrCell(value, "off", "Success Rate"),
                off_rush_explosiveness = ReactablefmtrCell(value, "off", "Explosiveness"),
                off_rush_negatives = ReactablefmtrCell(value, "off", "TFLs"),
                off_rush_ypc = ReactablefmtrCell(value, "offoth", "Yards / Att")
              ),
              columnGroups = list(
                colGroup(name = "Index Ratings", columns = c("net_rating", "off_rating")),
                colGroup(name = "Rushing Factors", columns = c("off_rush_epa_pp", "off_rush_success_rate", "off_rush_explosiveness", "off_rush_negatives")),
                colGroup(name = "Other Stats", columns = c("off_rush_ypc"))
              ),
              language = reactableLang(
                noData = "No teams found, please tweak filters.",
              ),
              theme = theme
    )
  })
  
  ## Passing Defense Table ---- 
  
  output$passing_def <- renderReactable({
    
    reactable(filtered()[c(1:8,10,62:65,73,69:70)], resizable = TRUE, pagination = FALSE, highlight = TRUE, fullWidth = FALSE, showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = c("def_rating"),
              columns = list(
                season = colDef(
                  show = FALSE
                ),
                team = colDef(
                  show = FALSE
                ),
                team_name = colDef(
                  name = "Team",
                  defaultSortOrder = "asc",
                  minWidth = 220,
                  cell = function(value, index) {
                    teamIndex <- filtered()[2:3]$team[index]
                    logo <- filtered()[7]$team_logo_espn[index]
                    tagList(
                      div(img(class = "logo", alt = paste(teamIndex, "logo"), src = logo), value)
                    )
                  }
                ),
                conf = colDef(
                  show = FALSE
                ),
                division = colDef(
                  name = "Division",
                  defaultSortOrder = "asc"
                ),
                div = colDef(
                  show = FALSE
                ),
                team_logo_espn = colDef(
                  show = FALSE
                ),
                net_rating = ReactablefmtrCell(value, "rating", "Overall"),
                def_rating = ReactablefmtrCell(value, "rating", "Defense"),
                def_pass_epa_pp = ReactablefmtrCell(value, "def", "EPA / Play"),
                def_pass_success_rate = ReactablefmtrCell(value, "def", "Success Rate"),
                def_pass_explosiveness = ReactablefmtrCell(value, "def", "Explosiveness"),
                def_pass_negatives = ReactablefmtrCell(value, "def", "TFLs"),
                def_pass_ypa = ReactablefmtrCell(value, "defoth", "Yards / Att"),
                def_pass_air_yards_pc = ReactablefmtrCell(value, "defoth", "Air Yds / Comp"),
                def_pass_yac_pc = ReactablefmtrCell(value, "defoth", "YAC / Comp")
              ),
              columnGroups = list(
                colGroup(name = "Index Ratings", columns = c("net_rating", "def_rating")),
                colGroup(name = "Passing Factors", columns = c("def_pass_epa_pp", "def_pass_success_rate", "def_pass_explosiveness", "def_pass_negatives")),
                colGroup(name = "Other Stats", columns = c("def_pass_ypa", "def_pass_air_yards_pc", "def_pass_yac_pc"))
              ),
              language = reactableLang(
                noData = "No teams found, please tweak filters.",
              ),
              theme = theme
    )
  })
  
  ## Rushing Defense Table ---- 
  
  output$rushing_def <- renderReactable({
    
    reactable(filtered()[c(1:8,10,75:78,80)], resizable = TRUE, pagination = FALSE, highlight = TRUE, fullWidth = FALSE, showSortable = TRUE,
              defaultSortOrder = "desc",
              defaultSorted = c("def_rating"),
              columns = list(
                season = colDef(
                  show = FALSE
                ),
                team = colDef(
                  show = FALSE
                ),
                team_name = colDef(
                  name = "Team",
                  defaultSortOrder = "asc",
                  minWidth = 220,
                  cell = function(value, index) {
                    teamIndex <- filtered()[2:3]$team[index]
                    logo <- filtered()[7]$team_logo_espn[index]
                    tagList(
                      div(img(class = "logo", alt = paste(teamIndex, "logo"), src = logo), value)
                    )
                  }
                ),
                conf = colDef(
                  show = FALSE
                ),
                division = colDef(
                  name = "Division",
                  defaultSortOrder = "asc"
                ),
                div = colDef(
                  show = FALSE
                ),
                team_logo_espn = colDef(
                  show = FALSE
                ),
                net_rating = ReactablefmtrCell(value, "rating", "Overall"),
                def_rating = ReactablefmtrCell(value, "rating", "Defense"),
                def_rush_epa_pp = ReactablefmtrCell(value, "def", "EPA / Play"),
                def_rush_success_rate = ReactablefmtrCell(value, "def", "Success Rate"),
                def_rush_explosiveness = ReactablefmtrCell(value, "def", "Explosiveness"),
                def_rush_negatives = ReactablefmtrCell(value, "def", "TFLs"),
                def_rush_ypc = ReactablefmtrCell(value, "defoth", "Yards / Att")
              ),
              columnGroups = list(
                colGroup(name = "Index Ratings", columns = c("net_rating", "def_rating")),
                colGroup(name = "Rushing Factors", columns = c("def_rush_epa_pp", "def_rush_success_rate", "def_rush_explosiveness", "def_rush_negatives")),
                colGroup(name = "Other Stats", columns = c("def_rush_ypc"))
              ),
              language = reactableLang(
                noData = "No teams found, please tweak filters.",
              ),
              theme = theme
    )
  })
  
}

# 6. Create Shiny App ----

shiny::shinyApp(ui, server)