# NFL Anticipation + Path Tracking Deploy

library(shiny)
library(shinydashboard)
library(dplyr)
library(gt)
library(gtExtras)
library(base64enc)

# Load pre-computed data with error handling
tryCatch({
  load("app_data.RData")
  message("Data loaded successfully!")
  message(paste("Selected plays:", nrow(selected_plays)))
  message(paste("GIF paths:", length(gif_paths)))
}, error = function(e) {
  message(paste("Error loading data:", e$message))
})

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Defender Anticipation Tracker"),
  
  dashboardSidebar(
    width = 300,
    selectInput("play_select", "Select Play:", choices = NULL, width = "100%"),
    hr(),
    h4("Play Info", style = "color: white; padding-left: 15px;"),
    div(style = "padding: 10px; color: white; font-size: 12px;", textOutput("play_epa")),
    hr(),
    h4("Legend", style = "color: white; padding-left: 15px;"),
    div(style = "padding: 10px; color: white; font-size: 11px;",
        HTML("<span style='color: #00FF00;'>&#9679;</span> Broke on ball<br>"),
        HTML("<span style='color: #FFD700;'>&#9679;</span> Defender (not broke)<br>"),
        HTML("<span style='color: #FF0000;'>&#9679;</span> Never broke<br>"),
        HTML("<span style='color: white;'>&#9679;</span> Offense<br>"),
        HTML("<span style='color: #D2691E;'>&#9679;</span> Ball landing spot")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #2c3e50; }
      .box { background-color: #34495e; border-top: none; }
      .box-header { color: white; }
      .box-title { color: white; }
    "))),
    fluidRow(
      box(title = "Play Animation", status = "success", solidHeader = TRUE, width = 8,
          uiOutput("gif_display")),
      box(title = "Season Percentiles", status = "warning", solidHeader = TRUE, width = 4,
          gt_output("percentile_table"))
    ),
    fluidRow(
      box(title = "All Defenders - Probability Changes", status = "info", solidHeader = TRUE, width = 12,
          gt_output("summary_table"))
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  observe({
    choices_list <- setNames(
      paste(selected_plays$game_id, selected_plays$play_id, sep = "_"),
      paste0(selected_plays$epa_type, " | EPA: ", round(selected_plays$expected_points_added, 2))
    )
    updateSelectInput(session, "play_select", choices = choices_list)
  })
  
  current_play <- reactive({
    req(input$play_select)
    parts <- strsplit(input$play_select, "_")[[1]]
    list(
      game_id = parts[1],
      play_id = as.numeric(parts[2]),
      gif_path = gif_paths[[input$play_select]],
      summary = summary_stats_all %>% filter(game_id == parts[1], play_id == as.numeric(parts[2])),
      epa = selected_plays %>% filter(game_id == parts[1], play_id == as.numeric(parts[2])) %>% pull(expected_points_added)
    )
  })
  
  output$gif_display <- renderUI({
    req(current_play())
    gif_file <- current_play()$gif_path
    if (!is.null(gif_file) && file.exists(gif_file)) {
      tags$img(src = dataURI(file = gif_file, mime = "image/gif"),
               style = "width: 100%; max-width: 700px;")
    } else {
      tags$p(paste("GIF not found:", gif_file), style = "color: white;")
    }
  })
  
  output$play_epa <- renderText({
    req(current_play())
    paste("EPA:", round(current_play()$epa[1], 2))
  })
  
  output$summary_table <- render_gt({
    req(current_play())
    
    summary_stats <- current_play()$summary
    if (nrow(summary_stats) == 0) return(data.frame(Message = "No data") %>% gt())
    
    breakers <- summary_stats %>% filter(broke_on_ball == 1) %>% arrange(desc(pd_end))
    non_breakers <- summary_stats %>% filter(broke_on_ball == 0) %>% arrange(break_oe)
    combined <- bind_rows(breakers, non_breakers)
    
    max_change <- max(abs(c(combined$pd_change, combined$tkl_change)), na.rm = TRUE)
    max_change <- max(max_change, 10, na.rm = TRUE)
    
    combined %>%
      mutate(broke_label = ifelse(broke_on_ball == 1, "Y", "N")) %>%
      select(headshot, player_name, player_position, broke_label,
             break_oe_fmt, break_oe, frames_oe,
             pd_start, pd_mid, pd_end, pd_change,
             tkl_start, tkl_mid, tkl_end, tkl_change) %>%
      gt() %>%
      gt_img_rows(columns = headshot, height = 35, img_source = "web") %>%
      cols_hide(columns = c(break_oe, pd_change, tkl_change)) %>%
      cols_label(headshot = "", player_name = "Player", player_position = "Pos",
                 broke_label = "Broke?",
                 break_oe_fmt = "O/E", frames_oe = "O/E",
                 pd_start = "Start", pd_mid = "Mid", pd_end = "End",
                 tkl_start = "Start", tkl_mid = "Mid", tkl_end = "End") %>%
      tab_spanner(label = "Break", columns = c(broke_label, break_oe_fmt)) %>%
      tab_spanner(label = "Frames", columns = frames_oe) %>%
      tab_spanner(label = "Pass Def Prob (%)", columns = c(pd_start, pd_mid, pd_end)) %>%
      tab_spanner(label = "Tackle Prob (%)", columns = c(tkl_start, tkl_mid, tkl_end)) %>%
      cols_align(align = "center", columns = everything()) %>%
      cols_align(align = "left", columns = player_name) %>%
      sub_missing(missing_text = "-") %>%
      data_color(columns = c(pd_start, tkl_start),
                 palette = c("#FADBD8", "#FFFFFF", "#D4EFDF", "#27AE60"), domain = c(0, 100), na_color = "#34495e") %>%
      data_color(columns = pd_change, target_columns = c(pd_mid, pd_end),
                 palette = c("#E74C3C", "#FADBD8", "#FFFFFF", "#D4EFDF", "#27AE60"),
                 domain = c(-max_change, max_change), na_color = "#34495e") %>%
      data_color(columns = tkl_change, target_columns = c(tkl_mid, tkl_end),
                 palette = c("#E74C3C", "#FADBD8", "#FFFFFF", "#D4EFDF", "#27AE60"),
                 domain = c(-max_change, max_change), na_color = "#34495e") %>%
      data_color(columns = break_oe, target_columns = break_oe_fmt,
                 palette = c("#FADBD8", "#FFFFFF", "#D4EFDF"), domain = c(-30, 30)) %>%
      data_color(columns = frames_oe,
                 palette = c("#D4EFDF", "#FFFFFF", "#FADBD8"), domain = c(-3, 3), na_color = "#34495e") %>%
      tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels()) %>%
      tab_options(table.font.size = px(11), data_row.padding = px(4), table.background.color = "#34495e")
  })
  
  output$percentile_table <- render_gt({
    req(current_play())
    
    summary_stats <- current_play()$summary
    if (nrow(summary_stats) == 0) return(data.frame(Message = "No data") %>% gt())
    
    breakers <- summary_stats %>% filter(broke_on_ball == 1) %>% arrange(desc(pd_end))
    non_breakers <- summary_stats %>% filter(broke_on_ball == 0) %>% arrange(desc(break_oe_pctl))
    combined <- bind_rows(breakers, non_breakers)
    
    combined %>%
      select(headshot, player_name, player_position,
             break_oe_pctl, frames_oe_pctl, pd_gain_pctl, tkl_gain_pctl) %>%
      gt() %>%
      gt_img_rows(columns = headshot, height = 30, img_source = "web") %>%
      cols_label(headshot = "", player_name = "Player", player_position = "Pos",
                 break_oe_pctl = "Break", frames_oe_pctl = "Speed",
                 pd_gain_pctl = "PD Path", tkl_gain_pctl = "TKL Path") %>%
      cols_align(align = "center", columns = everything()) %>%
      cols_align(align = "left", columns = player_name) %>%
      sub_missing(missing_text = "-") %>%
      data_color(columns = c(break_oe_pctl, frames_oe_pctl, pd_gain_pctl, tkl_gain_pctl),
                 palette = c("#E74C3C", "#F5B041", "#F7DC6F", "#82E0AA", "#27AE60"),
                 domain = c(0, 100), na_color = "#34495e") %>%
      tab_style(style = cell_text(weight = "bold", size = px(10)), locations = cells_column_labels()) %>%
      tab_options(table.font.size = px(10), data_row.padding = px(2), table.background.color = "#34495e") %>%
      tab_source_note(source_note = md("*Percentiles vs. players w/ 20+ plays*"))
  })
}

# Run

shinyApp(ui = ui, server = server)