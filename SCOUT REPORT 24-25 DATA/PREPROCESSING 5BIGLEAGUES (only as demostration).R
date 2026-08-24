library(tidyverse)
library(worldfootballR)
library(janitor)
library(progress)
library(dplyr)

SEASON_TO_GET <- 2025

STAT_TYPES_TO_GET <- c(
  "standard", #<- Es importante incluir "standard" para obtener métricas clave como Posición y Edad
  "shooting",
  "passing",
  "passing_types",
  "defense",
  "possession",
  "playing_time",
  "misc"
)

message(glue::glue("Iniciando descarga de datos para la temporada {SEASON_TO_GET}..."))

# Inicializar barra de progreso
pb <- progress_bar$new(
  format = "  Descargando :stat_type [:bar] :percent eta: :eta",
  total = length(STAT_TYPES_TO_GET), clear = FALSE, width = 70)

# Usar purrr::map para iterar sobre cada tipo de estadística, 
# descargar los datos y almacenarlos en una lista de dataframes.
list_of_stats_df <- purrr::map(STAT_TYPES_TO_GET, function(stat) {
  
  # Actualizar la barra de progreso
  pb$tick(tokens = list(stat_type = stat))
  
  # Descargar los datos para el tipo de estadística actual
  df <- load_fb_big5_advanced_season_stats(
    season_end_year = SEASON_TO_GET,
    stat_type = stat,
    team_or_player = "player"
  )
  
  # Limpiar los nombres de las columnas para hacerlos consistentes y fáciles de usar
  # Ejemplo: "Cmp%" se convierte en "cmp_percent"
  df <- df %>% 
    janitor::clean_names()
  
  return(df)
})

message("\nDescarga completada. Procediendo a unir los dataframes.")



# Identificar las columnas clave que son comunes en todos los dataframes
# y que identifican de forma única la fila de un jugador en un equipo/temporada.
# 'url' es el identificador único de jugador de FBref, haciéndolo la clave más robusta.
key_columns <- c(
  "season_end_year", "comp", "squad", "player", "nation", "pos", "age", "born", "url"
)

# Utilizar purrr::reduce para aplicar left_join de forma secuencial a la lista
# de dataframes. Empezará con los dos primeros, los unirá, y luego unirá el 
# resultado con el tercero, y así sucesivamente.
player_stats_complete <- list_of_stats_df %>%
  purrr::reduce(
    .f = dplyr::left_join, 
    by = key_columns,
    # Añadimos un sufijo por si alguna columna de métricas se repite (poco probable con esta selección)
    suffix = c("", "_dup") 
  )

# Inspeccionar el dataframe resultante para eliminar columnas duplicadas si las hubiera
# (por ejemplo, si 's_90_dup' se creó, significa que 's_90' estaba en más de un dataframe)
player_stats_complete <- player_stats_complete %>% 
  select(-ends_with("_dup"))

urlsplayers <- player_stats_complete[, c("season_end_year", "player", "url")]


message("Iniciando limpieza y selección de columnas...")

# --- 1. SELECCIONAR Y RENOMBRAR COLUMNAS ---
# Usamos dplyr::select para quedarnos solo con las columnas que necesitamos
# y las renombramos al mismo tiempo para que sean más claras.

stats_limpias <- player_stats_complete %>%
  select(
    # --- Identificadores ---
    temporada = season_end_year,
    equipo = squad,
    competicion = comp,
    jugador = player,
    nacionalidad = nation,
    posicion = pos,
    edad = age,
    url_jugador = url,
    
    # --- Tiempo de Juego ---
    partidos_jugados = mp_playing_time,
    titularidades = starts_starts,
    minutos_jugados = min_playing_time,
    
    # --- Métricas de Tiro ---
    goles = gls_standard,
    tiros = sh_standard,
    tiros_a_puerta = so_t_standard,
    goles_por_tiro = g_per_sh_standard,
    distancia_media_tiro = dist_standard,
    tiros_falta = fk_standard,
    penaltis_anotados = pk_standard,
    penaltis_intentados = p_katt_standard,
    xg = x_g_expected,
    npxg = npx_g_expected,
    npxg_por_tiro = npx_g_per_sh_expected,
    goles_vs_xg = g_minus_x_g_expected,
    
    # --- Métricas de Pase ---
    asistencias = ast,
    xa = x_a_expected,
    asistencias_vs_xa = a_minus_x_ag_expected,
    pases_clave = kp,
    pases_tercio_final = final_third,
    pases_area_penal = ppa,
    centros_area_penal = crs_pa,
    pases_progresivos = prg_p,
    pases_intentados = att_total,
    pases_completados = cmp_total,
    pases_pct = cmp_percent_total,
    pases_dist_total = tot_dist_total,
    pases_dist_progresiva = prg_dist_total,
    
    # --- Tipos de Pase (Completados) ---
    pases_cortos_completados = cmp_short,
    pases_medios_completados = cmp_medium,
    pases_largos_completados = cmp_long,
    
    # --- Tipos de Pase (Intentados) ---
    pases_cortos_intentados = att_short,
    pases_medios_intentados = att_medium,
    pases_largos_intentados = att_long,
    
    # --- Posesión y Regate ---
    toques = touches_touches,
    toques_tercio_def = def_3rd_touches,
    toques_tercio_medio = mid_3rd_touches,
    toques_tercio_ataque = att_3rd_touches,
    toques_area_penal_ataque = att_pen_touches,
    regates_intentados = att_take,
    regates_exitosos = succ_take,
    conducciones = carries_carries,
    conducciones_dist_total = tot_dist_carries,
    conducciones_dist_progresiva = prg_dist_carries,
    conducciones_progresivas = prg_c_carries,
    conducciones_tercio_final = final_third_carries,
    conducciones_area_penal = cpa_carries,
    pases_progresivos_recibidos = prg_r_receiving,
    
    # --- Métricas Defensivas ---
    entradas = tkl_tackles,
    entradas_ganadas = tkl_w,
    entradas_tercio_def = def_3rd_tackles,
    entradas_tercio_medio = mid_3rd_tackles,
    entradas_tercio_ataque = att_3rd_tackles,
    bloqueos = blocks_blocks,
    tiros_bloqueados = sh_blocks,
    pases_bloqueados = pass_blocks,
    intercepciones = int,
    despejes =  clr,
    errores_defensivos = err,
    
    # --- Miscelánea ---
    tarjetas_amarillas = crd_y,
    tarjetas_rojas = crd_r,
    faltas_cometidas = fls,
    faltas_recibidas = fld,
    fueras_de_juego = off,
    recuperaciones = recov,
    duelos_aereos_ganados = won_aerial,
    duelos_aereos_perdidos = lost_aerial,
    duelos_aereos_pct = won_percent_aerial
  )

message("Limpieza finalizada. El nuevo dataframe 'stats_limpias' está listo.")

MINUTES_THRESHOLD <- 400


stats_limpias <- stats_limpias %>%
  filter(!is.na(minutos_jugados) & minutos_jugados > MINUTES_THRESHOLD) 



# --- 1. OBTENER DATOS DE POSESIÓN POR EQUIPO ---
message(glue::glue("Descargando datos de posesión por equipo para la temporada {SEASON_TO_GET}..."))

team_possession_stats_raw <- load_fb_big5_advanced_season_stats(
  season_end_year = SEASON_TO_GET,
  stat_type = "possession",
  team_or_player = "team"
) %>%
  janitor::clean_names()

# --- LA SOLUCIÓN: ASEGURAR LA UNICIDAD ANTES DE UNIR ---
# Usamos distinct() para quedarnos con la primera aparición de cada combinación
# de equipo y competición, eliminando así los duplicados.
team_possession_stats <- team_possession_stats_raw %>%
  select(
    equipo = squad,
    competicion = comp,
    posesion_equipo = poss
  ) %>%
  distinct(equipo, competicion, .keep_all = TRUE)


message("Datos de posesión descargados y duplicados eliminados.")

# --- 2. UNIR DATOS DE POSESIÓN CON LOS DATOS DE JUGADORES ---
# Ahora, esta unión será segura y no creará filas duplicadas.
stats_con_posesion <- stats_limpias %>%
  left_join(team_possession_stats, by = c("equipo", "competicion"))


# --- 3. CREAR EL DATAFRAME FINAL CON VARIABLES TRANSFORMADAS ---
message("Creando el set de datos final con variables transformadas...")

stats_transformadas <- stats_con_posesion %>%
  # Filtrar para evitar divisiones por cero o valores nulos
  filter(minutos_jugados > MINUTES_THRESHOLD, toques > 0, !is.na(posesion_equipo)) %>%
  
  # Usamos TRANSMUTE para crear un nuevo set de variables a partir de las antiguas
  transmute(
    # --- Identificadores (se mantienen) ---
    jugador, equipo, competicion, posicion, edad, minutos_jugados, titularidades,
    
    # --- MÉTRICAS TRANSFORMADAS ---
    
    # Base de cálculo
    minutos_90s = minutos_jugados / 90,
    
    # TIRO: Métricas por 90 min y de eficiencia
    goals = goles / minutos_90s,
    shots = tiros / minutos_90s,
    shots_on_goal= tiros_a_puerta / minutos_90s,
    xg = xg / minutos_90s,
    npxg = npxg / minutos_90s,
    xg_per_shot = if_else(tiros > 0, xg / tiros, 0),
    npxg_per_shot = if_else(tiros > 0, npxg / tiros, 0),
    g_vs_xg= goles_vs_xg / minutos_90s,
    g_per_shot = if_else(tiros > 0, goles / tiros, 0),
    avg_dist_shot = if_else(tiros > 0, distancia_media_tiro, 0),
    
    # PASE: Métricas por 90 min y proporcionales
    asist = asistencias / minutos_90s,
    xa = xa / minutos_90s,
    asist_vs_xa = asistencias_vs_xa / minutos_90s,
    key_passes = pases_clave / minutos_90s,
    final_third_p = pases_tercio_final / minutos_90s,
    pen_area_p = pases_area_penal / minutos_90s,
    pen_area_crosses = centros_area_penal / minutos_90s,
    prog_passes = pases_progresivos /minutos_90s ,
    prog_passes_pct = if_else(pases_intentados > 0, pases_progresivos / pases_intentados, 0),
    pass_pct = pases_pct,
    completed_passes = pases_completados,
    short_p_pct = if_else(pases_intentados > 0, pases_cortos_intentados / pases_intentados, 0),
    mid_p_pct = if_else(pases_intentados > 0, pases_medios_intentados / pases_intentados, 0),
    long_p_pct = if_else(pases_intentados > 0, pases_largos_intentados / pases_intentados, 0),
    
    
    # POSESIÓN Y REGATE: Métricas por 90 min y de acierto
    touches = toques / minutos_90s,
    def_th_touches = toques_tercio_def / minutos_90s,
    mid_th_touches = toques_tercio_medio / minutos_90s,
    final_th_touches = toques_tercio_ataque / minutos_90s,
    pen_area_touches = toques_area_penal_ataque / minutos_90s,
    take_ons = regates_exitosos / minutos_90s,
    take_ons_pct = if_else(regates_intentados > 0, regates_exitosos / regates_intentados, 0),
    prog_carries = conducciones_progresivas / minutos_90s,
    prog_p_rec = pases_progresivos_recibidos / minutos_90s,
    
    # DEFENSA: Métricas por 90 min ajustadas por posesión
    tackles = (entradas / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    def_th_tackles = (entradas_tercio_def / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    mid_th_tackles = (entradas_tercio_medio / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    final_th_tackles = (entradas_tercio_ataque / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    interceptions = (intercepciones / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    blocks = (bloqueos / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    clearences = (despejes / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    recoveries = (recuperaciones / minutos_90s) * (2 / (1 + exp(-0.1 * (posesion_equipo - 50)))),
    
    # MISCELÁNEA: Métricas por 90 min y de acierto
    yellow_cards = tarjetas_amarillas / minutos_90s,
    commited_fouls = faltas_cometidas / minutos_90s,
    recibed_fouls = faltas_recibidas / minutos_90s,
    aerial_duels = duelos_aereos_ganados / minutos_90s,
    aerial_duels_pct = duelos_aereos_pct,
    
    # --- VARIABLES NO NORMALITZADES ---
    completed_passes_non = pases_completados,
    touches_non = toques,
    defensive_actions_non = entradas_ganadas + bloqueos + intercepciones + despejes + recuperaciones + duelos_aereos_ganados,
    xG_xA_non = xg + xa
    
  )


message("¡Proceso de transformación finalizado!")
message(glue::glue("El dataframe 'stats_limpias' tiene {nrow(stats_limpias)} observaciones."))
message(glue::glue("El dataframe final 'stats_transformadas' tiene {nrow(stats_transformadas)} observaciones."))

# --- 4. VERIFICACIÓN FINAL ---
glimpse(stats_transformadas)

saveRDS(
  urlsplayers,
  paste0(
    "urls_BIGLEAGUES_",
    SEASON_TO_GET, "_",
    MINUTES_THRESHOLD, "m.rds"
  )
)

saveRDS(
  stats_transformadas,
  paste0(
    "BIGLEAGUES_",
    SEASON_TO_GET, "_",
    MINUTES_THRESHOLD, "m.rds"
  )
)


