library(tidyverse)
library(worldfootballR)
library(janitor)
library(progress)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(fmsb)

DATA <- readRDS("5BIGLEAGUES.rds")

# ------------------------------------------------------------------------------
# RADARCHARTS (raw)
# ------------------------------------------------------------------------------


SHOTCREATIONv <- c("npxg", "goals", "shots", "shots_on_goal", 
                   "xa", "asist")

PASSINGv <- c("prog_passes","long_p_pct",  "pass_pct",  "pen_area_p", 
              "final_third_p", "key_passes","prog_p_rec")

DRIBBLINGv <- c("prog_carries","take_ons_pct", "take_ons", "def_th_touches", "mid_th_touches", 
                "final_th_touches","touches")

DEFENCEv <- c("interceptions", "tackles", "blocks", 
              "recoveries", "aerial_duels_pct", 
              "aerial_duels", "clearences")

colors_categoria <- c(
  "Shot Creation" = "#264653",  # Verd fosc elegant
  "Passing" = "#2A9D8F",       # Verd turquesa
  "Dribbling" = "#E9C46A",     # Groc mostassa
  "Defense" = "#E76F51"        # Vermell coral
)

lighten_color <- function(color, factor = 0.4) {
  # Convertir hex a RGB
  rgb_vals <- col2rgb(color)
  # Hacer más claro mezclando con blanco
  new_rgb <- rgb_vals + (255 - rgb_vals) * factor
  # Convertir de vuelta a hex
  rgb(new_rgb[1], new_rgb[2], new_rgb[3], maxColorValue = 255)
}


cols_list <- list(
  SHOTCREATION = list(
    pcol = c(colors_categoria["Shot Creation"], lighten_color(colors_categoria["Shot Creation"])),
    pfcol = c(paste0(colors_categoria["Shot Creation"], "4D"), paste0(lighten_color(colors_categoria["Shot Creation"]), "4D"))
  ),
  PASSING = list(
    pcol = c(colors_categoria["Passing"], lighten_color(colors_categoria["Passing"])),
    pfcol = c(paste0(colors_categoria["Passing"], "4D"), paste0(lighten_color(colors_categoria["Passing"]), "4D"))
  ),
  DEFENCE = list(
    pcol = c(colors_categoria["Defense"], lighten_color(colors_categoria["Defense"])),
    pfcol = c(paste0(colors_categoria["Defense"], "4D"), paste0(lighten_color(colors_categoria["Defense"]), "4D"))
  ),
  DRIBBLING = list(
    pcol = c(colors_categoria["Dribbling"], lighten_color(colors_categoria["Dribbling"])),
    pfcol = c(paste0(colors_categoria["Dribbling"], "4D"), paste0(lighten_color(colors_categoria["Dribbling"]), "4D"))
  )
)


##FUNCIONS: create_raw_radar_1 i create_raw_radar_2

create_raw_radar_1 <- function(df, jugador1) {
  select_jugador <- function(vars) {
    subset(df, jugador == jugador1)[, vars, drop = FALSE]
  }
  
  plot_radar <- function(data, vars, title_text, col_scheme) {
    maxs <- apply(df[, vars], 2, max, na.rm = TRUE)
    mins <- rep(0, length(vars))
    rdata <- rbind(maxs, mins, data)
    
    radarchart(rdata, maxmin = TRUE, cglty = 1, cglwd = 1, plwd = 2,
               plty = 1, vlcex = 0.9, axistype = 0, seg = 4,
               pcol = col_scheme$pcol[1], pfcol = col_scheme$pfcol[1], cglcol = "grey40")
    title(title_text, col.main = "grey20", line = -7.5, cex.main = 1.5)
  }
  
  par(mfrow = c(1, 4), font.lab = 2, font.axis = 2, las = 2,
      oma = c(2, 2, 4, 2), mar = c(1.5, 1.5, 2, 1.5))
  
  plot_radar(select_jugador(SHOTCREATIONv), SHOTCREATIONv, "OCASIONS CREADES", cols_list$SHOTCREATION)
  plot_radar(select_jugador(DRIBBLINGv), DRIBBLINGv, "DRIBLATGE", cols_list$DRIBBLING)
  plot_radar(select_jugador(PASSINGv), PASSINGv, "PASE", cols_list$PASSING)
  plot_radar(select_jugador(DEFENCEv), DEFENCEv, "DEFENSA", cols_list$DEFENCE)
  info_jugador <- df[df$jugador == jugador1, c("minutos_jugados", "titularidades", "edad", "equipo")][1, ]
  
  subtitle_text <- paste("Temporada 24/25 \n", info_jugador$edad, "anys |", info_jugador$equipo,
                         "| Min:", info_jugador$minutos_jugados, "| Tit:", info_jugador$titularidades,
                         "\n ",  "Font: FBREF, By: @Maaxva2")
  
  title(main = paste("Radarchart absolut de", jugador1, "per 90'"),
        sub = subtitle_text,
        outer = TRUE, cex.main = 2, font.main = 3, cex.sub = 1, font.sub = 3, line = -1)
}

create_raw_radar_2 <- function(df, jugador1, jugador2) {
  select_jugadores <- function(vars) {
    df %>% 
      filter(jugador %in% c(jugador1, jugador2)) %>% 
      arrange(factor(jugador, levels = c(jugador1, jugador2))) %>%
      select(all_of(vars))
  }
  
  build_rdata <- function(vars) {
    datos <- select_jugadores(vars)
    maxs <- apply(df[, vars], 2, max, na.rm = TRUE)
    mins <- rep(0, length(vars))
    rbind(maxs, mins, datos)
  }
  
  rSHOTCREATION <- build_rdata(SHOTCREATIONv)
  rDEFENCE <- build_rdata(DEFENCEv)
  rDRIBBLING <- build_rdata(DRIBBLINGv)
  rPASSING <- build_rdata(PASSINGv)
  
  info_jugador1 <- df[df$jugador == jugador1, c("minutos_jugados", "titularidades", "edad", "equipo")][1, ]
  info_jugador2 <- df[df$jugador == jugador2, c("minutos_jugados", "titularidades", "edad", "equipo")][1, ]
  
  par(mfrow=c(1,4), font.lab=2, font.axis=2, las=2, oma=c(2,2,4,2), mar=c(1.5,1.5,2,1.5))
  
  radarchart(rSHOTCREATION, maxmin=TRUE, cglty=1, cglwd=1, plwd=2, plty=c(1,2), vlcex=0.9,
             axistype=0, seg=4, 
             pcol=cols_list$SHOTCREATION$pcol, pfcol=cols_list$SHOTCREATION$pfcol, cglcol="grey40")
  title("OCASIONS CREADES", col.main="grey20", line=-7.5, cex.main=1.5)
  
  radarchart(rDRIBBLING, cglty=1, cglwd=1, plwd=2, plty=c(1,2), vlcex=0.9,
             axistype=0, seg=4,
             pcol=cols_list$DRIBBLING$pcol, pfcol=cols_list$DRIBBLING$pfcol, cglcol="grey40")
  title("DRIBLATGE", col.main="grey20", line=-7.5, cex.main=1.5)
  
  radarchart(rPASSING, cglty=1, cglwd=1, plwd=2, plty=c(1,2), vlcex=0.9,
             axistype=0, seg=4,
             pcol=cols_list$PASSING$pcol, pfcol=cols_list$PASSING$pfcol, cglcol="grey40")
  title("PASE", col.main="grey20", line=-7.5, cex.main=1.5)
  
  radarchart(rDEFENCE, cglty=1, cglwd=1, plwd=2, plty=c(1,2), vlcex=0.9,
             axistype=0, seg=4,
             pcol=cols_list$DEFENCE$pcol, pfcol=cols_list$DEFENCE$pfcol, cglcol="grey40")
  title("DEFENSA", col.main="grey20", line=-7.5, cex.main=1.5)
  
  subtitle_text <- paste("Temporada 24/25 \n", jugador1, ":", info_jugador1$edad, "|", info_jugador1$equipo, 
                         "| Min:", info_jugador1$minutos_jugados, "| Tit:", info_jugador1$titularidades,
                         "\n", jugador2, ":", info_jugador2$edad, "|", info_jugador2$equipo,
                         "| Min:", info_jugador2$minutos_jugados, "| Tit:", info_jugador2$titularidades,
                         "\n ",  "Font: FBREF, Autor: @Maaxva2")
  
  title(main = paste("Radarcharts absoluts de", jugador1, "i", jugador2, "per 90'"),
        sub = subtitle_text,
        outer=TRUE, cex.main=2, font.main=3, cex.sub=1, font.sub=3, line=-1)
  
  legend(x = -1.5, y = -1.2, legend = c(jugador1, jugador2),
         lty = c(1, 2), lwd=2, bty="n", col = "black", cex=1.2)
}
# ------------------------------------------------------------------------------
# RADARCHARTS (percentil)
# ------------------------------------------------------------------------------

lighten_color_j2 <- function(color, factor = 0.4) {
  rgb_vals <- col2rgb(color)
  new_rgb <- rgb_vals + (255 - rgb_vals) * factor
  rgb(new_rgb[1], new_rgb[2], new_rgb[3], maxColorValue = 255)
}

lighten_color <- function(color, factor = 0.7) {
  rgb_vals <- col2rgb(color)
  new_rgb <- rgb_vals + (255 - rgb_vals) * factor
  rgb(new_rgb[1], new_rgb[2], new_rgb[3], maxColorValue = 255)
}

##FUNCIONS: create_perc_radar_1 i create_perc_radar_2

create_perc_radar_1 <- function(df, jugador1) {
  library(dplyr)
  library(fmsb)
  
  per_df <- function(df) {
    df_percentiles <- as.data.frame(lapply(df, function(columna) {
      if (is.numeric(columna)) {
        rank(columna, ties.method = "average") / length(columna) * 100
      } else {
        columna
      }
    }))
    return(df_percentiles)
  }
  
  df_perc <- per_df(df)
  
  info_jugador <- df[df$jugador == jugador1, c("minutos_jugados", "titularidades", "edad", "equipo")][1, ]
  
  SHOTCREATIONv <- c("npxg", "goals", "shots", "shots_on_goal", 
                     "xa", "asist")
  
  PASSINGv <- c("prog_passes","long_p_pct",  "pass_pct",  "pen_area_p", 
                "final_third_p", "key_passes","prog_p_rec")
  
  DRIBBLINGv <- c("prog_carries","take_ons_pct", "take_ons", "def_th_touches", "mid_th_touches", 
                  "final_th_touches","touches")
  
  DEFENCEv <- c("interceptions", "tackles", "blocks", 
                "recoveries", "aerial_duels_pct", 
                "aerial_duels", "clearences")
  
  select_jugador <- function(df, variables) {
    df_filtrado <- subset(df, jugador == jugador1)
    df_filtrado <- df_filtrado[, variables, drop = FALSE]
    return(df_filtrado)
  }
  
  plot_radar <- function(data, variables, title_text, col_scheme) {
    rdata <- rbind(rep(100, length(variables)),
                   rep(0, length(variables)),
                   data)
    
    radarchart(rdata, maxmin = TRUE, cglty = 1, cglwd = 1, plwd = 2,
               plty = 1,
               vlcex = 0.9, axistype = 0, seg = 4,
               pcol = col_scheme$pcol[1],
               pfcol = col_scheme$pfcol[1],
               cglcol = "grey40")
    title(title_text, col.main = "grey20", line = -7.5, cex.main = 1.5)
  }
  
  playersSHOTCREATION <- select_jugador(df_perc, SHOTCREATIONv)
  playersPASSING <- select_jugador(df_perc, PASSINGv)
  playersDRIBBLING <- select_jugador(df_perc, DRIBBLINGv)
  playersDEFENCE <- select_jugador(df_perc, DEFENCEv)
  
  cols_list <- list(
    SHOTCREATION = list(
      pcol = colors_categoria["Shot Creation"], 
      pfcol = paste0(colors_categoria["Shot Creation"], "4D")
    ),
    PASSING = list(
      pcol = colors_categoria["Passing"], 
      pfcol = paste0(colors_categoria["Passing"], "4D")
    ),
    DEFENCE = list(
      pcol = colors_categoria["Defense"], 
      pfcol = paste0(colors_categoria["Defense"], "4D")
    ),
    DRIBBLING = list(
      pcol = colors_categoria["Dribbling"], 
      pfcol = paste0(colors_categoria["Dribbling"], "4D")
    )
  )
  
  par(mfrow = c(1, 4), font.lab = 2, font.axis = 2, las = 2,
      oma = c(2, 2, 4, 2), mar = c(1.5, 1.5, 2, 1.5))
  
  plot_radar(playersSHOTCREATION, SHOTCREATIONv, "OCASIONS CREADES", cols_list$SHOTCREATION)
  plot_radar(playersDRIBBLING, DRIBBLINGv, "DRIBLATGE", cols_list$DRIBBLING)
  plot_radar(playersPASSING, PASSINGv, "PASE", cols_list$PASSING)
  plot_radar(playersDEFENCE, DEFENCEv, "DEFENSA", cols_list$DEFENCE)  
  subtitle_text <- paste("Temporada 24/25 \n", info_jugador$edad, "años |", info_jugador$equipo,
                         "| Minuts jugats:", info_jugador$minutos_jugados, "| Titularitats:", info_jugador$titularidades,
                         "\n ",  "Font: FBREF, Autor: @Maaxva2")
  
  title(main = paste("Radarchart de", jugador1, "per 90'"),
        sub = subtitle_text,
        outer = TRUE, cex.main = 2, font.main = 3, cex.sub = 1, font.sub = 3, line = -1)
}

create_perc_radar_2 <- function(df, jugador1, jugador2) {

  info_jugador1 <- df[df$jugador == jugador1, c("minutos_jugados", "titularidades", "edad", "equipo")][1, ]
  info_jugador2 <- df[df$jugador == jugador2, c("minutos_jugados", "titularidades", "edad", "equipo")][1, ]
  
  per_df <- function(df) {
    as.data.frame(lapply(df, function(col) {
      if(is.numeric(col)) rank(col, ties.method = "average") / length(col) * 100 else col
    }))
  }
  
  df_per <- per_df(df)
  
  SHOTCREATIONv <- c("npxg", "goals", "shots", "shots_on_goal", 
                     "xa", "asist")
  
  PASSINGv <- c("prog_passes","long_p_pct",  "pass_pct",  "pen_area_p", 
                "final_third_p", "key_passes","prog_p_rec")
  
  DRIBBLINGv <- c("prog_carries","take_ons_pct", "take_ons", "def_th_touches", "mid_th_touches", 
                  "final_th_touches","touches")
  
  DEFENCEv <- c("interceptions", "tackles", "blocks", 
                "recoveries", "aerial_duels_pct", 
                "aerial_duels", "clearences")
  
  select_jugadores <- function(df, variables) {
    vars_existents <- variables[variables %in% colnames(df)]
    if(length(vars_existents) != length(variables)) {
      cat("Variables no trobades:", setdiff(variables, vars_existents), "\n")
    }
    
    df %>% 
      filter(jugador %in% c(jugador1, jugador2)) %>%
      arrange(factor(jugador, levels = c(jugador1, jugador2))) %>%
      select(all_of(vars_existents))
  }
  
  rSHOTCREATION_data <- select_jugadores(df_per, SHOTCREATIONv)
  rPASSING_data <- select_jugadores(df_per, PASSINGv)
  rDRIBBLING_data <- select_jugadores(df_per, DRIBBLINGv)
  rDEFENCE_data <- select_jugadores(df_per, DEFENCEv)
  
  rSHOTCREATION <- rbind(rep(100, ncol(rSHOTCREATION_data)),
                         rep(0, ncol(rSHOTCREATION_data)),
                         rSHOTCREATION_data)
  
  rPASSING <- rbind(rep(100, ncol(rPASSING_data)),
                    rep(0, ncol(rPASSING_data)),
                    rPASSING_data)
  
  rDRIBBLING <- rbind(rep(100, ncol(rDRIBBLING_data)),
                      rep(0, ncol(rDRIBBLING_data)),
                      rDRIBBLING_data)
  
  rDEFENCE <- rbind(rep(100, ncol(rDEFENCE_data)),
                    rep(0, ncol(rDEFENCE_data)),
                    rDEFENCE_data)
  
  cols_list <- list(
    SHOTCREATION = list(
      pcol = c(colors_categoria["Shot Creation"], lighten_color_j2(colors_categoria["Shot Creation"])),
      pfcol = c(paste0(colors_categoria["Shot Creation"], "33"), paste0(lighten_color_j2(colors_categoria["Shot Creation"]), "33"))
    ),
    PASSING = list(
      pcol = c(colors_categoria["Passing"], lighten_color_j2(colors_categoria["Passing"])),
      pfcol = c(paste0(colors_categoria["Passing"], "33"), paste0(lighten_color_j2(colors_categoria["Passing"]), "33"))
    ),
    DEFENCE = list(
      pcol = c(colors_categoria["Defense"], lighten_color_j2(colors_categoria["Defense"])),
      pfcol = c(paste0(colors_categoria["Defense"], "33"), paste0(lighten_color_j2(colors_categoria["Defense"]), "33"))
    ),
    DRIBBLING = list(
      pcol = c(colors_categoria["Dribbling"], lighten_color_j2(colors_categoria["Dribbling"])),
      pfcol = c(paste0(colors_categoria["Dribbling"], "33"), paste0(lighten_color_j2(colors_categoria["Dribbling"]), "33"))
    )
  )
  
  par(mfrow=c(1,4), font.lab=2, font.axis=2, las=2, oma=c(2,2,4,2), mar=c(2.5,1.5,3,1.5))
  
  radarchart(rSHOTCREATION, maxmin=TRUE, cglty=1, cglwd=1, plwd=3, plty=c(1,2), vlcex=1.1,
             axistype=0, seg=4, 
             pcol=cols_list$SHOTCREATION$pcol, pfcol=cols_list$SHOTCREATION$pfcol, cglcol="grey40")
  title("OCASIONS CREADES", col.main="grey20", line=-7.5, cex.main=1.5)
  
  radarchart(rDRIBBLING, maxmin=TRUE, cglty=1, cglwd=1, plwd=3, plty=c(1,2), vlcex=1.1,
             axistype=0, seg=4,
             pcol=cols_list$DRIBBLING$pcol, pfcol=cols_list$DRIBBLING$pfcol, cglcol="grey40")
  title("DRIBLATGE", col.main="grey20", line=-7.5, cex.main=1.5)
  
  radarchart(rPASSING, maxmin=TRUE, cglty=1, cglwd=1, plwd=3, plty=c(1,2), vlcex=1.1,
             axistype=0, seg=4,
             pcol=cols_list$PASSING$pcol, pfcol=cols_list$PASSING$pfcol, cglcol="grey40")
  title("PASE", col.main="grey20", line=-7.5, cex.main=1.5)
  
  radarchart(rDEFENCE, maxmin=TRUE, cglty=1, cglwd=1, plwd=3, plty=c(1,2), vlcex=1.1,
             axistype=0, seg=4,
             pcol=cols_list$DEFENCE$pcol, pfcol=cols_list$DEFENCE$pfcol, cglcol="grey40")
  title("DEFENSA", col.main="grey20", line=-7.5, cex.main=1.5)  
  subtitle_text <- paste("Temporada 24/25 \n", jugador1, ":", info_jugador1$edad, "|", info_jugador1$equipo, 
                         "| Min:", info_jugador1$minutos_jugados, "| Tit:", info_jugador1$titularidades,
                         "\n", jugador2, ":", info_jugador2$edad, "|", info_jugador2$equipo,
                         "| Min:", info_jugador2$minutos_jugados, "| Tit:", info_jugador2$titularidades,
                         "\n ",  "Font: FBREF, Autor: @Maaxva2")
  
  title(main = paste("Radarcharts de", jugador1, "i", jugador2, "per 90'"),
        sub = subtitle_text,
        outer=TRUE, cex.main=2, font.main=3, cex.sub=1, font.sub=3, line=-1)
  
  legend(x = -1.5, y = -1.2, legend = c(jugador1, jugador2),
         lty = c(1, 2), lwd=2, bty="n", col = "black", cex=1.2)
}


# ------------------------------------------------------------------------------
#BARPLOTS
# ------------------------------------------------------------------------------

calcular_percentils <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ {
      if (all(is.na(.x))) {
        rep(NA_real_, length(.x))
      } else {
        rank(.x, ties.method = "average", na.last = "keep") / sum(!is.na(.x)) * 100
      }
    }))
}

validar_jugador <- function(DATA, jugador) {
  if (!jugador %in% DATA$jugador) {
    stop("El jugador '", jugador, "' no es troba a les dades")
  }
  if (nrow(DATA) == 0) {
    stop("Les dades estan buides")
  }
}

obtenir_dades_jugador <- function(DATAper, DATA, jugador, variables) {
  jugador_row <- which(DATA$jugador == jugador)
  if (length(jugador_row) == 0) {
    stop("No s'han trobat dades per al jugador especificat")
  }
  if (length(jugador_row) > 1) {
    warning("Múltiples entrades trobades per '", jugador, "'. S'utilitzarà la primera.")
    jugador_row <- jugador_row[1]
  }
  
  variables_existents <- intersect(variables, names(DATAper))
  if (length(variables_existents) == 0) {
    stop("Cap de les variables especificades existeix a les dades")
  }
  
  DATAper[jugador_row, variables_existents, drop = FALSE]
}

## FUNCIONS: create_barplot_radar i create_top8_barplot
create_barplot_radar <- function(DATA, jugador) {
  validar_jugador(DATA, jugador)
  
  DATAper <- calcular_percentils(DATA)
  
  variables_manual <- list(
    "Ocasions Creades" = c("npxg", "goals", "shots", "shots_on_goal", "xa", "asist", ),
    "Pase" = c("prog_passes", "long_p_pct", "pass_pct", "pen_area_p", 
               "final_third_p", "key_passes", "prog_p_rec"),
    "Driblatge" = c("prog_carries", "take_ons_pct", "take_ons", "def_th_touches", 
                    "mid_th_touches", "final_th_touches", "touches"),
    "Defensa" = c("interceptions", "tackles", "blocks", "recoveries", 
                  "aerial_duels_pct", "aerial_duels", "clearences")
  )
  
  colors_categoria <- c(
    "Ocasions Creades" = "#264653",
    "Pase" = "#2A9D8F",
    "Driblatge" = "#E9C46A", 
    "Defensa" = "#E76F51"
  )
  
  plots_list <- list()
  
  for (categoria in names(variables_manual)) {
    vars_categoria <- intersect(variables_manual[[categoria]], names(DATAper))
    
    if (length(vars_categoria) == 0) next
    
    dades_categoria <- obtenir_dades_jugador(DATAper, DATA, jugador, vars_categoria)
    
    data_long <- dades_categoria %>%
      pivot_longer(cols = everything(), names_to = "Metric", values_to = "Percentile") %>%
      filter(!is.na(Percentile)) %>%
      mutate(Categoria = categoria)
    
    if (nrow(data_long) == 0) next
    
    p_categoria <- ggplot(data_long, aes(x = Percentile, y = reorder(Metric, Percentile))) +
      geom_col(aes(x = 100), fill = "grey94", width = 0.7, alpha = 0.7) +
      geom_col(fill = colors_categoria[categoria], width = 0.7, alpha = 0.9) +
      geom_vline(xintercept = c(25, 50, 75, 90), color = "white", linewidth = 0.4, alpha = 0.9) +
      geom_text(aes(label = paste0(round(Percentile, 1), "%")), 
                hjust = -0.1, size = 3.5, fontface = "bold", color = "grey20") +
      scale_x_continuous(limits = c(0, 110), expand = c(0, 0),
                         breaks = c(0, 25, 50, 75, 90, 100),
                         labels = c("0%", "25%", "50%", "75%", "90%", "100%")) +
      labs(title = categoria, x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold", color = colors_categoria[categoria], 
                                  hjust = 0.5, margin = margin(b = 10)),
        axis.text.x = element_text(size = 9, color = "grey40"),
        axis.text.y = element_text(size = 10, color = "grey25", hjust = 1),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    plots_list[[categoria]] <- p_categoria
  }
  
  if (length(plots_list) == 0) {
    stop("No hi ha dades vàlides per crear el gràfic")
  }
  
  library(gridExtra)
  
  title_grob <- grid::textGrob(paste("Anàlisi de Rendiment -", jugador), 
                               gp = grid::gpar(fontsize = 18, fontface = "bold", col = "grey15"),
                               hjust = 0.5)
  subtitle_grob <- grid::textGrob("Mètriques seleccionades per categories - Comparació amb percentils de la lliga", 
                                  gp = grid::gpar(fontsize = 12, col = "grey35"),
                                  hjust = 0.5)
  caption_grob <- grid::textGrob("Font: FBRef | Autor: @Maaxva2 | Temporada 24/25", 
                                 gp = grid::gpar(fontsize = 9, col = "grey50", fontface = "italic"),
                                 hjust = 0.5)
  
  # Organitzar gràfics en 2x2
  combined_plot <- grid.arrange(
    title_grob,
    subtitle_grob,
    arrangeGrob(grobs = plots_list, ncol = 2),
    caption_grob,
    heights = c(0.8, 0.4, 8, 0.4),
    ncol = 1
  )
  
  return(combined_plot)
}
create_top8_barplot <- function(DATA, jugador, n_metriques = 8) {
  validar_jugador(DATA, jugador)
  
  # Calcular percentils
  DATAper <- calcular_percentils(DATA)
  
  # Variables a excloure
  vars_excloses <- c("jugador", "equipo", "competicion", "posicion", "edad", 
                     "minutos_jugados", "titularidades", "minutos_90s", 
                     "yellow_cards", "commited_fouls", "recibed_fouls")
  
  # Obtenir totes les variables numèriques vàlides
  vars_numeriques <- names(DATA)[sapply(DATA, is.numeric)]
  vars_valides <- setdiff(vars_numeriques, vars_excloses)
  
  if (length(vars_valides) == 0) {
    stop("No hi ha variables numèriques vàlides per analitzar")
  }
  
  # Obtenir dades del jugador
  dades_jugador <- obtenir_dades_jugador(DATAper, DATA, jugador, vars_valides)
  
  # Convertir a format llarg
  data_long <- dades_jugador %>%
    pivot_longer(cols = everything(), names_to = "Metric", values_to = "Percentile") %>%
    filter(!is.na(Percentile))
  
  # Classificació completa en categories
  categoritzacio_vars <- list(
    "Ocasions Creades" = c("goals", "shots", "shots_on_goal", "xg", "npxg", "xg_per_shot", 
                           "npxg_per_shot", "g_vs_xg", "g_per_shot", "avg_dist_shot", 
                           "xa", "asist", "asist_vs_xa"),
    "Pase" = c("prog_passes", "prog_passes_pct", "pass_pct", "short_p_pct", "mid_p_pct", 
               "long_p_pct", "key_passes", "final_third_p", "pen_area_p", 
               "pen_area_crosses", "prog_p_rec"),
    "Driblatge" = c("touches", "def_th_touches", "mid_th_touches", "final_th_touches", 
                    "pen_area_touches", "take_ons", "take_ons_pct", "prog_carries"),
    "Defensa" = c("tackles", "def_th_tackles", "mid_th_tackles", "final_th_tackles", 
                  "interceptions", "blocks", "clearences", "recoveries", 
                  "aerial_duels", "aerial_duels_pct")
  )
  
  # Assignar categories
  assignar_categoria <- function(metrica) {
    for (categoria in names(categoritzacio_vars)) {
      if (metrica %in% categoritzacio_vars[[categoria]]) {
        return(categoria)
      }
    }
    return(NA_character_)  # Retornar NA per mètriques sense categoria
  }
  
  data_long$Categoria <- sapply(data_long$Metric, assignar_categoria, USE.NAMES = FALSE)
  
  # Filtrar només mètriques amb categoria assignada
  data_long <- data_long %>% 
    filter(!is.na(Categoria))
  
  if (nrow(data_long) == 0) {
    stop("No hi ha mètriques vàlides amb categories assignades per al jugador")
  }
  
  if (nrow(data_long) < n_metriques) {
    n_metriques <- nrow(data_long)
    warning("Només hi ha ", n_metriques, " mètriques disponibles.")
  }
  
  # Crear top N
  top_data <- data_long %>%
    arrange(desc(Percentile)) %>%
    slice_head(n = n_metriques) %>%
    mutate(Metric = factor(Metric, levels = rev(Metric)))
  
  # Colors (sense "Altres")
  colors_categoria <- c(
    "Ocasions Creades" = "#264653",
    "Driblatge" = "#E9C46A",
    "Pase" = "#2A9D8F",
    "Defensa" = "#E76F51"
  )
  
  categories_usades <- unique(top_data$Categoria)
  top_data$Categoria <- factor(top_data$Categoria, levels = names(colors_categoria))
  
  # Crear gràfic
  background_data <- top_data %>% mutate(Percentile_bg = 100)
  
  p <- ggplot(top_data, aes(x = Percentile, y = Metric)) +
    geom_col(data = background_data, aes(x = Percentile_bg, y = Metric),
             fill = "grey94", width = 0.7, alpha = 0.7) +
    geom_col(aes(fill = Categoria), width = 0.7, alpha = 0.9) +
    geom_vline(xintercept = c(25, 50, 75, 90), color = "white", linewidth = 0.4, alpha = 0.9) +
    geom_text(aes(label = paste0(round(Percentile, 1), "%")), 
              hjust = -0.1, size = 3.8, fontface = "bold", color = "grey20") +
    scale_x_continuous(limits = c(0, 110), expand = c(0, 0),
                       breaks = c(0, 25, 50, 75, 90, 100),
                       labels = c("0%", "25%", "50%", "75%", "90%", "100%")) +
    scale_fill_manual(values = colors_categoria) +
    labs(title = paste("TOP", n_metriques, "Percentils -", jugador),
         subtitle = paste("Les", n_metriques, "millors mètriques de totes les disponibles"),
         x = "Percentil", y = NULL, fill = "Categoria",
         caption = "Font: FBRef | Visualització: @Maaxva2 | Temporada 24/25") +
    theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size = 18, face = "bold", color = "grey15", hjust = 0.5, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 12, color = "grey35", hjust = 0.5, margin = margin(b = 25)),
      plot.caption = element_text(size = 9, color = "grey50", hjust = 0.5, margin = margin(t = 15), face = "italic"),
      axis.title.x = element_text(size = 12, face = "bold", color = "grey30", margin = margin(t = 15)),
      axis.text.x = element_text(size = 10, color = "grey40"),
      axis.text.y = element_text(size = 11, color = "grey25", hjust = 1, margin = margin(r = 10)),
      legend.position = "right",
      legend.title = element_text(size = 12, face = "bold", color = "grey25"),
      legend.text = element_text(size = 11, color = "grey30"),
      plot.margin = margin(20, 15, 15, 20)
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# SCATTERPLOTS
# ------------------------------------------------------------------------------
calcular_percentils <- function(df) {
  df %>%
    mutate(across(where(is.numeric), ~ {
      if (all(is.na(.x))) {
        rep(NA_real_, length(.x))
      } else {
        rank(.x, ties.method = "average", na.last = "keep") / sum(!is.na(.x)) * 100
      }
    }))
}

validar_jugador <- function(DATA, jugador) {
  if (!jugador %in% DATA$jugador) {
    stop("El jugador '", jugador, "' no es troba a les dades")
  }
  if (nrow(DATA) == 0) {
    stop("Les dades estan buides")
  }
}

format_var_name <- function(var_name) {
  gsub("_", " ", var_name)
}

## FUNCIONS: create_scatterplot_best_metrics
create_scatterplot_best_metrics <- function(DATA, jugador_focus, posicio_filtrada, usar_minuts = FALSE) {
  
  # Validar jugador
  validar_jugador(DATA, jugador_focus)
  
  # Calcular percentils
  DATAper <- calcular_percentils(DATA)
  
  # Seleccionar totes les variables numèriques excepte les acabades en "_non"
  vars_numeriques <- names(DATA)[sapply(DATA, is.numeric)]
  vars_excloses <- c("jugador", "equipo", "competicion", "posicion", "edad", 
                     "minutos_jugados", "titularidades", "minutos_90s", 
                     "yellow_cards", "commited_fouls", "recibed_fouls")
  
  # Filtrar variables que NO acaben en "_non" i que no estan excloses
  variables_candidates <- vars_numeriques[!grepl("_non$", vars_numeriques)]
  variables_existents <- setdiff(variables_candidates, vars_excloses)
  
  if (length(variables_existents) < 2) {
    stop("No hi ha suficients variables disponibles. Disponibles: ", length(variables_existents))
  }
  
  # Obtenir dades del jugador focus
  jugador_row <- which(DATA$jugador == jugador_focus)
  if (length(jugador_row) == 0) {
    stop("No s'han trobat dades per al jugador especificat")
  }
  if (length(jugador_row) > 1) {
    jugador_row <- jugador_row[1]
  }
  
  dades_jugador <- DATAper[jugador_row, variables_existents, drop = FALSE]
  
  # Convertir a format llarg i seleccionar les 2 millors mètriques
  data_long <- dades_jugador %>%
    pivot_longer(cols = everything(), names_to = "Metric", values_to = "Percentile") %>%
    filter(!is.na(Percentile)) %>%
    arrange(desc(Percentile)) %>%
    slice_head(n = 2)
  
  if (nrow(data_long) < 2) {
    stop("No s'han trobat suficients mètriques vàlides per al jugador")
  }
  
  # Seleccionar les variables per als eixos
  var_x <- data_long$Metric[1]  # Millor mètrica
  var_y <- data_long$Metric[2]  # Segona millor mètrica
  
  # Classificació en categories amb totes les variables
  categoritzacio_vars <- list(
    "Ocasions Creades" = c("goals", "shots", "shots_on_goal", "xg", "npxg", "xg_per_shot", 
                           "npxg_per_shot", "g_vs_xg", "g_per_shot", "avg_dist_shot", 
                           "asist", "xa", "asist_vs_xa"),
    "Pase" = c("key_passes", "final_third_p", "pen_area_p", "pen_area_crosses", 
               "prog_passes", "prog_passes_pct", "pass_pct", "completed_passes", 
               "short_p_pct", "mid_p_pct", "long_p_pct", "prog_p_rec"),
    "Driblatge" = c("touches", "def_th_touches", "mid_th_touches", "final_th_touches", 
                    "pen_area_touches", "take_ons", "take_ons_pct", "prog_carries"),
    "Defensa" = c("tackles", "def_th_tackles", "mid_th_tackles", "final_th_tackles", 
                  "interceptions", "blocks", "clearences", "recoveries", 
                  "aerial_duels", "aerial_duels_pct")
  )
  
  # Variables de referència per cada categoria (acabades en "_non")
  variables_referencia <- list(
    "Ocasions Creades" = "xG_xA_non",
    "Driblatge" = "touches_non", 
    "Pase" = "completed_passes_non",
    "Defensa" = "defensive_actions_non"
  )
  
  # Assignar categoria a la millor mètrica
  assignar_categoria <- function(metrica) {
    for (categoria in names(categoritzacio_vars)) {
      if (metrica %in% categoritzacio_vars[[categoria]]) {
        return(categoria)
      }
    }
    return("Altres")
  }
  
  millor_categoria <- assignar_categoria(var_x)
  
  # Seleccionar variable de mida basada en el paràmetre usar_minuts
  var_size <- NULL
  if (usar_minuts) {
    var_size <- "minutos_jugados"
    if (!var_size %in% names(DATA)) {
      warning("La variable 'minutos_jugados' no existeix a les dades.")
      var_size <- NULL
    }
  } else {
    # Seleccionar variable de mida basada en la millor categoria
    if (millor_categoria %in% names(variables_referencia)) {
      var_size <- variables_referencia[[millor_categoria]]
      # Verificar que la variable existeix a les dades
      if (!var_size %in% names(DATA)) {
        warning("La variable de referència '", var_size, "' per la categoria '", millor_categoria, 
                "' no existeix a les dades.")
        var_size <- NULL
      }
    }
    
    # Si no s'ha trobat variable de referència, buscar la primera disponible
    if (is.null(var_size)) {
      vars_referencia_disponibles <- names(DATA)[grepl("_non$", names(DATA))]
      if (length(vars_referencia_disponibles) > 0) {
        var_size <- vars_referencia_disponibles[1]
        warning("S'utilitza la variable de referència per defecte: ", var_size)
      }
    }
  }
  
  # Filtrar dades per posició
  data_filtrada <- DATA %>%
    filter(posicion %in% posicio_filtrada)
  
  if (nrow(data_filtrada) == 0) {
    stop("No s'han trobat jugadors amb la posició especificada")
  }
  
  # Calcula mitjanes per als quadrants
  mitjana_x <- mean(data_filtrada[[var_x]], na.rm = TRUE)
  mitjana_y <- mean(data_filtrada[[var_y]], na.rm = TRUE)
  
  # Obtenir dades del jugador focus
  jugador_focus_data <- data_filtrada %>%
    filter(jugador == jugador_focus)
  
  # Identificar els millors jugadors per cada eix (top 10)
  millors_x <- data_filtrada %>%
    arrange(desc(!!sym(var_x))) %>%
    slice_head(n = 10) %>%
    pull(jugador)
  
  millors_y <- data_filtrada %>%
    arrange(desc(!!sym(var_y))) %>%
    slice_head(n = 10) %>%
    pull(jugador)
  
  # Combinar millors jugadors de tots dos eixos
  millors_eixos <- unique(c(millors_x, millors_y))
  
  data_millors_eixos <- data_filtrada %>%
    filter(jugador %in% millors_eixos, jugador != jugador_focus)
  
  # Títols
  millors_metriques_text <- paste(format_var_name(var_x), "i", format_var_name(var_y))
  titol <- paste("Millors Mètriques de", jugador_focus, ":", millors_metriques_text)
  
  # Informació per al subtítol
  info_camp <- if (millor_categoria != "Altres") {
    paste("Camp:", millor_categoria)
  } else {
    ""
  }
  
  # Variable de referència per al subtítol
  if (!is.null(var_size)) {
    if (usar_minuts) {
      info_referencia <- "Mida: Minuts Jugats"
    } else {
      info_referencia <- paste("Mida:", format_var_name(gsub("_non$", "", var_size)))
    }
  } else {
    info_referencia <- ""
  }
  
  subtitol <- paste("Mostra:", nrow(data_filtrada), "jugadors | Posició:", paste(posicio_filtrada, collapse = "/"), 
                    "|", info_camp, "|", info_referencia)
  
  caption <- "Font: FBRef | Autor: @Maaxva2"
  
  colors_quad <- c("#FFEBEE", "#FFF8E1","#FFF8E1", "#E8F5E8")
  colors_text <- c("#C62828", "#F57C00","#F57C00", "#2E7D32")
  
  if (!is.null(var_size)) {
    data_filtrada$size_scaled <- scales::rescale(data_filtrada[[var_size]], to = c(2, 8))
    data_millors_eixos$size_scaled <- scales::rescale(data_millors_eixos[[var_size]], to = c(2, 8))
    jugador_focus_data$size_scaled <- scales::rescale(jugador_focus_data[[var_size]], to = c(2, 8))
    
    size_range <- range(data_filtrada[[var_size]], na.rm = TRUE)
    size_breaks <- c(size_range[1], mean(size_range), size_range[2])
    size_labels <- round(size_breaks, 1)
    
    if (usar_minuts) {
      size_var_name <- "Minuts Jugats"
    } else {
      size_var_name <- format_var_name(gsub("_non$", "", var_size))
    }
  }
  
  p <- ggplot(data_filtrada, aes_string(x = var_x, y = var_y)) +
    
    annotate("rect", xmin = -Inf, xmax = mitjana_x, ymin = -Inf, ymax = mitjana_y, 
             fill = colors_quad[1], alpha = 0.6) +
    annotate("rect", xmin = mitjana_x, xmax = Inf, ymin = -Inf, ymax = mitjana_y, 
             fill = colors_quad[2], alpha = 0.6) +
    annotate("rect", xmin = -Inf, xmax = mitjana_x, ymin = mitjana_y, ymax = Inf, 
             fill = colors_quad[3], alpha = 0.6) +
    annotate("rect", xmin = mitjana_x, xmax = Inf, ymin = mitjana_y, ymax = Inf, 
             fill = colors_quad[4], alpha = 0.6) +
    
    geom_vline(xintercept = mitjana_x, linetype = "solid", color = "#37474F", size = 1.5, alpha = 0.8) +
    geom_hline(yintercept = mitjana_y, linetype = "solid", color = "#37474F", size = 1.5, alpha = 0.8) +
    
    {if (!is.null(var_size)) {
      geom_point(aes(size = I(size_scaled)), alpha = 0.6, color = "#37474F")
    } else {
      geom_point(size = 3, alpha = 0.6, color = "#37474F")
    }} +
    
    {if (nrow(data_millors_eixos) > 0) {
      if (!is.null(var_size)) {
        geom_point(data = data_millors_eixos, aes(size = I(size_scaled)), color = "#1565C0", alpha = 0.8, stroke = 1)
      } else {
        geom_point(data = data_millors_eixos, size = 4.5, color = "#1565C0", alpha = 0.8, stroke = 1)
      }
    }} +
    
    {if (!is.null(var_size)) {
      geom_point(data = jugador_focus_data, aes(size = I(size_scaled)), color = "#9370DB", alpha = 0.9, stroke = 2)
    } else {
      geom_point(data = jugador_focus_data, size = 5, color = "#9370DB", alpha = 0.9, stroke = 2)
    }} +
    
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.88, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.88, na.rm = TRUE),
             label = "ELITE", size = 4, fontface = "bold", 
             color = colors_text[4], hjust = 0.5, vjust = 0.5) +
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.12, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.88, na.rm = TRUE),
             label = paste("GOOD", format_var_name(var_y)), size = 4, fontface = "bold", 
             color = colors_text[3], hjust = 0.5, vjust = 0.5) +
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.88, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.12, na.rm = TRUE),
             label = paste("GOOD", format_var_name(var_x)), size = 4, fontface = "bold", 
             color = colors_text[2], hjust = 0.5, vjust = 0.5) +
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.12, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.12, na.rm = TRUE),
             label = "AVERAGE", size = 4, fontface = "bold", 
             color = colors_text[1], hjust = 0.5, vjust = 0.5) +
    
    geom_text_repel(data = jugador_focus_data, aes(label = jugador), 
                    size = 3.5, fontface = "bold", color = "#9370DB",
                    box.padding = 0.5, point.padding = 0.3, 
                    max.overlaps = 15, force = 3,
                    bg.color = "white", bg.r = 0.15, alpha = 0.9) +
    
    {if (nrow(data_millors_eixos) > 0) {
      geom_text_repel(data = data_millors_eixos, aes(label = jugador), 
                      size = 3.3, fontface = "bold", color = "#1565C0",
                      box.padding = 0.5, point.padding = 0.3, 
                      max.overlaps = 10, force = 2.5,
                      bg.color = "white", bg.r = 0.15, alpha = 0.85)
    }} +
    
    # Títols i etiquetes
    labs(
      title = titol,
      subtitle = subtitol,
      x = format_var_name(var_x),
      y = format_var_name(var_y),
      caption = caption
    ) +
    
    # Tema
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                color = "#263238", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 10, color = "#546E7A", hjust = 0.5, 
                                   margin = margin(b = 18)),
      plot.caption = element_text(size = 9, color = "#78909C", hjust = 1, 
                                  margin = margin(t = 12)),
      axis.title = element_text(size = 12, face = "bold", color = "#37474F"),
      axis.text = element_text(size = 10, color = "#455A64"),
      panel.background = element_rect(fill = "#FAFAFA", color = NA),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major = element_line(color = "#ECEFF1", size = 0.6),
      panel.grid.minor = element_line(color = "#F5F5F5", size = 0.4),
      plot.margin = margin(25, 25, 25, 25),
      panel.border = element_rect(color = "#E0E0E0", fill = NA, size = 0.5),
      axis.ticks = element_line(color = "#BDBDBD", size = 0.5),
      axis.ticks.length = unit(0.3, "cm"),
      legend.position = c(0.02, 0.02),  # Posició a baix esquerra
      legend.justification = c(0, 0),   # Justificació a baix esquerra
      legend.title = element_text(size = 10, face = "bold", color = "#37474F"),
      legend.text = element_text(size = 9, color = "#455A64"),
      legend.background = element_rect(fill = "white", color = "#E0E0E0", size = 0.5),
      legend.box = "horizontal",
      legend.margin = margin(5, 5, 5, 5)
    )
  
  # Afegir llegenda de mida si hi ha variable de referència
  if (!is.null(var_size)) {
    p <- p + 
      scale_size_continuous(
        name = paste("Mida:", size_var_name),
        range = c(2, 8),
        breaks = size_breaks,
        labels = size_labels,
        guide = guide_legend(
          override.aes = list(color = "#37474F", alpha = 0.8),
          title.position = "top",
          title.hjust = 0,
          label.position = "right",
          keywidth = unit(0.8, "cm"),
          keyheight = unit(0.8, "cm"),
          ncol = 1,
          byrow = TRUE
        )
      )
  } else {
    p <- p + guides(size = "none")
  }
  
  return(p)
}

## FUNCIONS: create_scatterplot_manual_metrics
create_scatterplot_manual_metrics <- function(DATA, metric1, metric2, jugador_focus, posicio_filtrada) {
  
  # Convertir jugador_focus a vector si es un string único
  if (is.character(jugador_focus) && length(jugador_focus) == 1) {
    jugador_focus <- c(jugador_focus)
  }
  
  # Validar jugadores
  for (jugador in jugador_focus) {
    validar_jugador(DATA, jugador)
  }
  
  # Validar que les mètriques existeixen a les dades
  if (!metric1 %in% names(DATA)) {
    stop("La mètrica '", metric1, "' no existeix a les dades")
  }
  if (!metric2 %in% names(DATA)) {
    stop("La mètrica '", metric2, "' no existeix a les dades")
  }
  
  # Verificar que les mètriques són numèriques
  if (!is.numeric(DATA[[metric1]])) {
    stop("La mètrica '", metric1, "' no és numèrica")
  }
  if (!is.numeric(DATA[[metric2]])) {
    stop("La mètrica '", metric2, "' no és numèrica")
  }
  
  # Assignar variables per als eixos
  var_x <- metric1
  var_y <- metric2
  
  # Classificació en categories amb totes les variables
  categoritzacio_vars <- list(
    "Ocasions Creades" = c("goals", "shots", "shots_on_goal", "xg", "npxg", "xg_per_shot", 
                           "npxg_per_shot", "g_vs_xg", "g_per_shot", "avg_dist_shot", 
                           "asist", "xa", "asist_vs_xa"),
    "Pase" = c("key_passes", "final_third_p", "pen_area_p", "pen_area_crosses", 
               "prog_passes", "prog_passes_pct", "pass_pct", "completed_passes", 
               "short_p_pct", "mid_p_pct", "long_p_pct", "prog_p_rec"),
    "Driblatge" = c("touches", "def_th_touches", "mid_th_touches", "final_th_touches", 
                    "pen_area_touches", "take_ons", "take_ons_pct", "prog_carries"),
    "Defensa" = c("tackles", "def_th_tackles", "mid_th_tackles", "final_th_tackles", 
                  "interceptions", "blocks", "clearences", "recoveries", 
                  "aerial_duels", "aerial_duels_pct")
  )
  
  # Variables de referència per cada categoria (acabades en "_non")
  variables_referencia <- list(
    "Ocasions Creades" = "xG_xA_non",
    "Driblatge" = "touches_non", 
    "Pase" = "completed_passes_non",
    "Defensa" = "defensive_actions_non"
  )
  
  # Assignar categoria a la primera mètrica
  assignar_categoria <- function(metrica) {
    for (categoria in names(categoritzacio_vars)) {
      if (metrica %in% categoritzacio_vars[[categoria]]) {
        return(categoria)
      }
    }
    return("Altres")
  }
  
  categoria_metric1 <- assignar_categoria(var_x)
  
  # Filtrar dades per posició
  data_filtrada <- DATA %>%
    filter(posicion %in% posicio_filtrada)
  
  if (nrow(data_filtrada) == 0) {
    stop("No s'han trobat jugadors amb la posició especificada")
  }
  
  # Calcula mitjanes per als quadrants
  mitjana_x <- mean(data_filtrada[[var_x]], na.rm = TRUE)
  mitjana_y <- mean(data_filtrada[[var_y]], na.rm = TRUE)
  
  # Obtenir dades dels jugadors focus
  jugador_focus_data <- data_filtrada %>%
    filter(jugador %in% jugador_focus)
  
  if (nrow(jugador_focus_data) == 0) {
    stop("Cap dels jugadors especificats es troba a les dades filtrades per posició")
  }
  
  # Verificar que tots els jugadors focus estan presents
  jugadors_no_trobats <- setdiff(jugador_focus, jugador_focus_data$jugador)
  if (length(jugadors_no_trobats) > 0) {
    warning("Els següents jugadors no s'han trobat a les dades filtrades: ", 
            paste(jugadors_no_trobats, collapse = ", "))
  }
  
  # Identificar els millors jugadors per cada eix (top 10)
  millors_x <- data_filtrada %>%
    arrange(desc(!!sym(var_x))) %>%
    slice_head(n = 10) %>%
    pull(jugador)
  
  millors_y <- data_filtrada %>%
    arrange(desc(!!sym(var_y))) %>%
    slice_head(n = 10) %>%
    pull(jugador)
  
  # Combinar millors jugadors de tots dos eixos, excloent els jugadors focus
  millors_eixos <- unique(c(millors_x, millors_y))
  millors_eixos <- millors_eixos[!millors_eixos %in% jugador_focus]
  
  data_millors_eixos <- data_filtrada %>%
    filter(jugador %in% millors_eixos)
  
  # Títols (sense nom de jugador específic)
  metriques_text <- paste(format_var_name(var_x), "vs", format_var_name(var_y))
  titol <- paste("Comparació de jugadors:", metriques_text)
  
  subtitol <- paste("Mostra:", nrow(data_filtrada), "jugadors | Posició:", paste(posicio_filtrada, collapse = "/"))
  
  caption <- "Font: FBRef | Autor: @Maaxva2"
  
  colors_quad <- c("#FFEBEE", "#FFF8E1","#FFF8E1", "#E8F5E8")
  colors_text <- c("#C62828", "#F57C00","#F57C00", "#2E7D32")
  
  # Crear dades per als diferents tipus de punts per evitar superposicions
  data_altres <- data_filtrada %>%
    filter(!jugador %in% millors_eixos, !jugador %in% jugador_focus)
  
  p <- ggplot(data_filtrada, aes_string(x = var_x, y = var_y)) +
    
    annotate("rect", xmin = -Inf, xmax = mitjana_x, ymin = -Inf, ymax = mitjana_y, 
             fill = colors_quad[1], alpha = 0.6) +
    annotate("rect", xmin = mitjana_x, xmax = Inf, ymin = -Inf, ymax = mitjana_y, 
             fill = colors_quad[2], alpha = 0.6) +
    annotate("rect", xmin = -Inf, xmax = mitjana_x, ymin = mitjana_y, ymax = Inf, 
             fill = colors_quad[3], alpha = 0.6) +
    annotate("rect", xmin = mitjana_x, xmax = Inf, ymin = mitjana_y, ymax = Inf, 
             fill = colors_quad[4], alpha = 0.6) +
    
    geom_vline(xintercept = mitjana_x, linetype = "solid", color = "#37474F", size = 1.5, alpha = 0.8) +
    geom_hline(yintercept = mitjana_y, linetype = "solid", color = "#37474F", size = 1.5, alpha = 0.8)
  
  # Afegir punts normals (altres jugadors)
  p <- p + geom_point(data = data_altres, size = 3, alpha = 0.6, color = "#37474F")
  
  # Afegir millors jugadors (sense incloure els jugadors focus)
  if (nrow(data_millors_eixos) > 0) {
    p <- p + geom_point(data = data_millors_eixos, size = 4.5, color = "#1565C0", alpha = 0.8, stroke = 1)
  }
  
  # Afegir jugadors focus (sempre al darrera per ser visibles)
  # Utilitzar el mateix color per tots els jugadors focus
  p <- p + geom_point(data = jugador_focus_data, size = 4.5, color = "#9370DB", alpha = 0.8, stroke = 2)
  
  # Afegir etiquetes dels quadrants i text
  p <- p +
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.88, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.88, na.rm = TRUE),
             label = "ELITE", size = 4, fontface = "bold", 
             color = colors_text[4], hjust = 0.5, vjust = 0.5) +
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.12, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.88, na.rm = TRUE),
             label = paste("GOOD", format_var_name(var_y)), size = 4, fontface = "bold", 
             color = colors_text[3], hjust = 0.5, vjust = 0.5) +
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.88, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.12, na.rm = TRUE),
             label = paste("GOOD", format_var_name(var_x)), size = 4, fontface = "bold", 
             color = colors_text[2], hjust = 0.5, vjust = 0.5) +
    annotate("text", x = quantile(data_filtrada[[var_x]], 0.12, na.rm = TRUE), 
             y = quantile(data_filtrada[[var_y]], 0.12, na.rm = TRUE),
             label = "AVERAGE", size = 4, fontface = "bold", 
             color = colors_text[1], hjust = 0.5, vjust = 0.5)
  
  # Afegir etiquetes dels jugadors focus amb el mateix color
  p <- p + geom_text_repel(data = jugador_focus_data, aes(label = jugador), 
                           size = 3.5, fontface = "bold", color = "#9370DB",
                           box.padding = 0.5, point.padding = 0.3, 
                           max.overlaps = 15, force = 3,
                           bg.color = "white", bg.r = 0.15, alpha = 0.9)
  
  # Afegir etiquetes dels millors jugadors si n'hi ha
  if (nrow(data_millors_eixos) > 0) {
    p <- p + geom_text_repel(data = data_millors_eixos, aes(label = jugador), 
                             size = 3.3, fontface = "bold", color = "#1565C0",
                             box.padding = 0.5, point.padding = 0.3, 
                             max.overlaps = 10, force = 2.5,
                             bg.color = "white", bg.r = 0.15, alpha = 0.85)
  }
  
  p <- p +
    
    # Títols i etiquetes
    labs(
      title = titol,
      subtitle = subtitol,
      x = format_var_name(var_x),
      y = format_var_name(var_y),
      caption = caption
    ) +
    
    # Tema
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                color = "#263238", margin = margin(b = 8)),
      plot.subtitle = element_text(size = 10, color = "#546E7A", hjust = 0.5, 
                                   margin = margin(b = 18)),
      plot.caption = element_text(size = 9, color = "#78909C", hjust = 1, 
                                  margin = margin(t = 12)),
      axis.title = element_text(size = 12, face = "bold", color = "#37474F"),
      axis.text = element_text(size = 10, color = "#455A64"),
      panel.background = element_rect(fill = "#FAFAFA", color = NA),
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major = element_line(color = "#ECEFF1", size = 0.6),
      panel.grid.minor = element_line(color = "#F5F5F5", size = 0.4),
      plot.margin = margin(25, 25, 25, 25),
      panel.border = element_rect(color = "#E0E0E0", fill = NA, size = 0.5),
      axis.ticks = element_line(color = "#BDBDBD", size = 0.5),
      axis.ticks.length = unit(0.3, "cm"),
      legend.position = c(0.02, 0.02),  # Posició a baix esquerra
      legend.justification = c(0, 0),   # Justificació a baix esquerra
      legend.title = element_text(size = 10, face = "bold", color = "#37474F"),
      legend.text = element_text(size = 9, color = "#455A64"),
      legend.background = element_rect(fill = "white", color = "#E0E0E0", size = 0.5),
      legend.box = "horizontal",
      legend.margin = margin(5, 5, 5, 5)
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# SHOTMAPS (understat)
# ------------------------------------------------------------------------------

# Funció per reescalar coordenades
reescalar_coordenades <- function(df, col_x = "X", col_y = "Y") {
  if (!col_x %in% names(df)) {
    stop(paste("La columna", col_x, "no existeix al dataframe"))
  }
  if (!col_y %in% names(df)) {
    stop(paste("La columna", col_y, "no existeix al dataframe"))
  }
  
  df$location.x <- (df[[col_x]]) * 120
  df$location.y <- (df[[col_y]]) * 80
  
  return(df)
}

# Funció per generar shotmap amb colors per resultat i mida per xG
create_shotmap_resultat <- function(shots_df, nom_jugador) {
  
  # Verificar paràmetres d'entrada
  if (!is.data.frame(shots_df)) {
    stop("El primer paràmetre ha de ser un dataframe")
  }
  
  if (!is.character(nom_jugador) || length(nom_jugador) != 1) {
    stop("El nom del jugador ha de ser un string")
  }
  
  # Aplicar reescalat de coordenades si no existeixen les columnes location.x i location.y
  if (!"location.x" %in% names(shots_df) || !"location.y" %in% names(shots_df)) {
    shots_df <- reescalar_coordenades(shots_df)
  }
  
  # Filtrar per jugador
  shots <- shots_df %>%
    filter(player == nom_jugador)
  
  # Verificar si tenim dades
  if(nrow(shots) == 0) {
    stop(paste("No s'han trobat tirs per al jugador:", nom_jugador))
  }
  
  # Calcular estadístiques
  gols <- sum(shots$result == "Goal", na.rm = TRUE)
  
  # Colors per resultat
  shotmapoutcomecolors <- c("Goal" = "#27ae60", "On Target" = "#f39c12", "Out" = "#e74c3c")
  
  # Preparar dades amb categories de resultat
  shots_outcome <- shots %>%
    mutate(
      result = case_when(
        result == "Goal" ~ "Goal",
        result %in% c("SavedShot") ~ "On Target",
        TRUE ~ "Out"
      )
    )
  
  # Crear el gràfic
  p2 <- ggplot() +
    # CAMP DE FUTBOL
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 60, ymin = 0, ymax = 80, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 18, xmax = 0, ymin = 18, ymax = 62, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 114, ymin = 30, ymax = 50, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 120.5, ymin = 36, ymax = 44, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = -0.5, ymin = 36, ymax = 44, 
             fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, 
             colour = "black", size = 0.6) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, 
             colour = "black", size = 0.6) +
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, 
             colour = "black", size = 0.6) +
    
    # ELEMENTS DEL CAMP
    annotate("point", x = 108, y = 40, colour = "black", size = 1.05) +
    annotate("path", colour = "black", size = 0.6,
             x = 60 + 10*cos(seq(0, 2*pi, length.out = 100)),
             y = 40 + 10*sin(seq(0, 2*pi, length.out = 100))) +
    annotate("point", x = 60, y = 40, colour = "black", size = 1.05) +
    annotate("path", x = 12 + 10*cos(seq(-0.3*pi, 0.3*pi, length.out = 30)), size = 0.6,
             y = 40 + 10*sin(seq(-0.3*pi, 0.3*pi, length.out = 30)), col = "black") +
    annotate("path", x = 107.84 - 10*cos(seq(-0.3*pi, 0.3*pi, length.out = 30)), size = 0.6,
             y = 40 - 10*sin(seq(-0.3*pi, 0.3*pi, length.out = 30)), col = "black") +
    
    # TIRS
    geom_point(data = shots_outcome, 
               aes(x = location.x, y = location.y, 
                   color = result, 
                   size = xG),
               alpha = 0.7) +
    
    # TEMA
    theme_minimal() +
    theme(
      # Fons i línies
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      
      # Títols
      plot.title = element_text(
        size = 20, 
        face = "bold", 
        hjust = 0.5, 
        color = "#2c3e50",
        margin = margin(t = 20, b = 5)
      ),
      plot.subtitle = element_text(
        size = 14, 
        hjust = 0.5, 
        color = "#7f8c8d",
        margin = margin(b = 20)
      ),
      plot.caption = element_text(
        size = 11, 
        hjust = 0.5, 
        color = "#95a5a6",
        margin = margin(t = 15, b = 10)
      ),
      
      # Llegenda
      legend.position = "top",
      legend.title = element_text(size = 12, face = "bold", color = "#2c3e50"),
      legend.text = element_text(size = 10, color = "#2c3e50"),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.margin = margin(b = 20),
      
      # Proporcions
      aspect.ratio = 0.6,
      
      # Marges del plot
      plot.margin = margin(20, 20, 20, 20)
    ) +
    
    # TÍTOLS
    labs(
      title = paste("Shotmap -", nom_jugador),
      subtitle = "Anàlisi per resultat i Expected Goals (xG)",
      caption = paste("Font: Understat | Autor:@Maaxva2 | Temporada 24/25 \n",
                      "Total tirs:", nrow(shots_outcome), "| Gols marcats:", gols, 
                      "| Eficàcia:", round((gols/nrow(shots_outcome))*100, 1), "%",
                      "| xG per tir:", round(sum(shots_outcome$xG, na.rm = TRUE)/nrow(shots_outcome), 3),
                      "| xG total:", round(sum(shots_outcome$xG, na.rm = TRUE), 2))
    ) + 
    
    # ESCALES
    scale_color_manual(
      values = shotmapoutcomecolors, 
      name = "Resultat"
    ) +
    
    scale_size_continuous(
      name = "Expected Goals (xG)",
      range = c(2, 8),
      limits = c(0, 1),
      breaks = c(0.1, 0.3, 0.5, 0.7, 0.9), 
      labels = scales::number_format(accuracy = 0.1)
    ) +
    
    # GUIES DE LLEGENDA
    guides(
      color = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(size = 4)
      ),
      size = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(color = "black")
      )
    ) +
    
    # ZOOM AL TERÇ OFENSIU
    coord_flip(xlim = c(85, 125)) +
    scale_y_reverse()
  
  return(p2)
}


# ============================================================
# SAVE SCOUT REPORT TOOL
# ============================================================

# Objects required by the Rmd
objects_to_save <- c(
  
  # -------------------------
  # MAIN DATA
  # -------------------------
  "DATA",
  
  # -------------------------
  # RADAR CONFIGURATION
  # -------------------------
  "SHOTCREATIONv",
  "PASSINGv",
  "DRIBBLINGv",
  "DEFENCEv",
  "colors_categoria",
  "cols_list",
  
  # -------------------------
  # RADAR FUNCTIONS
  # -------------------------
  "lighten_color",
  "lighten_color_j2",
  "create_raw_radar_1",
  "create_raw_radar_2",
  "create_perc_radar_1",
  "create_perc_radar_2",
  
  # -------------------------
  # BARPLOT / SCATTER HELPERS
  # -------------------------
  "calcular_percentils",
  "validar_jugador",
  "obtenir_dades_jugador",
  "format_var_name",
  
  # -------------------------
  # BARPLOTS
  # -------------------------
  "create_barplot_radar",
  "create_top8_barplot",
  
  # -------------------------
  # SCATTERPLOTS
  # -------------------------
  "create_scatterplot_best_metrics",
  "create_scatterplot_manual_metrics",
  
  # -------------------------
  # SHOTMAP
  # -------------------------
  "reescalar_coordenades",
  "create_shotmap_resultat"
)


# Check that every required object exists
missing_objects <- objects_to_save[
  !vapply(objects_to_save, exists, logical(1))
]

if (length(missing_objects) > 0) {
  stop(
    "The following objects are missing and cannot be saved:\n",
    paste(missing_objects, collapse = "\n")
  )
}


# Save in the CURRENT PROJECT DIRECTORY
save(
  list = objects_to_save,
  file = "Funcions Scout Report.Rdata"
)

cat(
  "\nScout Report tool successfully saved as:\n",
  normalizePath("Funcions Scout Report.Rdata"),
  "\n"
)
