print("Running MCsim"
# =============================================================================
# ESPN NCAA Men's Basketball – Full Season Scraper  (v5)
#
# Changes vs v4:
#  1. Linescores (h1, h2, ot1, ot2...) pulled from SCOREBOARD endpoint —
#     that is where ESPN actually stores them, not in the summary header
#  2. OT detection via shortDetail string parse ("Final/OT", "Final/2OT") —
#     more reliable than status > period for completed games
#  3. collect_game_ids() now returns a data frame with linescore + OT columns
#     already attached, keyed by game_id
#  4. scrape_season() merges that frame with summary stats at the end
# =============================================================================

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)

# ── Config ────────────────────────────────────────────────────────────────────

SEASON_YEAR   <- 2027                                 # change each season
#SEASON_YEAR2 <- 2026
SEASON_START  <- as.Date(paste0(SEASON_YEAR - 1, "-11-01"))
SEASON_END    <- as.Date(format(Sys.Date(), "%Y-%m-%d"))
PAUSE_GET     <- 0.05
OUTPUT_FILE   <- paste0("ncaab_", (SEASON_YEAR-1),"-",SEASON_YEAR, "_games.")

setwd("C:/Users/zdis1/OneDrive - University of Connecticut/CBB/MC")

BASE_SCOREBOARD <- "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard"
BASE_SUMMARY    <- "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary"

# ── Core helpers ──────────────────────────────────────────────────────────────

safe_get <- function(url, query = list(), retries = 3) {
  for (i in seq_len(retries)) {
    resp <- tryCatch(GET(url, query = query, timeout(20)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200)
      return(content(resp, as = "text", encoding = "UTF-8"))
    Sys.sleep(i * 2)
  }
  NULL
}

safe_parse <- function(txt) {
  tryCatch(fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- unlist(x)
  if (length(x) == 0 || (length(x) == 1 && is.na(x))) return(default)
  as.character(x[[1]])
}

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0) x else y

# ── OT helpers ────────────────────────────────────────────────────────────────

# Parse "Final/OT" → 1,  "Final/2OT" → 2,  "Final" → 0
ot_count_from_detail <- function(detail) {
  if (is.na(detail) || !grepl("OT", detail, ignore.case = TRUE)) return(0L)
  m <- regmatches(detail, regexpr("[0-9]+(?=OT)", detail, perl = TRUE))
  if (length(m) == 0) return(1L)   # "Final/OT" with no number = 1 OT
  as.integer(m)
}

# ── Step 1: collect game IDs AND linescores from scoreboard ──────────────────
#
# The scoreboard endpoint is the authoritative source for:
#   - game IDs
#   - per-period linescores (h1, h2, ot1, ot2...)
#   - OT detection via status > type > shortDetail
#   - final scores, date, venue
#
# We grab all of this here so we never need to re-parse it from summary.

parse_scoreboard_event <- function(event) {
  comp        <- event[["competitions"]][[1]] %||% list()
  competitors <- comp[["competitors"]] %||% list()
  if (length(competitors) < 2) return(NULL)
  
  game_id <- scalar(event[["id"]])
  
  # Date
  raw_date  <- scalar(comp[["date"]])
  game_date <- if (!is.na(raw_date) && nchar(raw_date) >= 10) substr(raw_date, 1, 10) else NA_character_
  
  # Status / OT
  short_detail <- scalar(comp[["status"]][["type"]][["shortDetail"]])  # "Final/OT", "Final/2OT", "Final"
  status_name  <- scalar(comp[["status"]][["type"]][["name"]])
  ot_periods   <- ot_count_from_detail(short_detail)
  went_to_ot   <- ot_periods > 0L
  periods_played <- 2L + ot_periods
  
  # Venue
  venue_obj   <- comp[["venue"]]
  venue_name  <- scalar(venue_obj[["fullName"]])
  venue_city  <- scalar(venue_obj[["address"]][["city"]])
  venue_state <- scalar(venue_obj[["address"]][["state"]])
  
  # Identify home/away
  get_side <- function(side) {
    idx <- which(vapply(competitors, function(c) scalar(c[["homeAway"]], ""), character(1)) == side)
    competitors[[if (length(idx)) idx[[1]] else if (side == "home") 2L else 1L]]
  }
  home <- get_side("home")
  away <- get_side("away")
  
  # Linescores — scoreboard returns them as [{value: 38}, {value: 41}, {value: 7}]
  # for a 1OT game; regulation game has 2 entries.
  parse_ls <- function(competitor, prefix) {
    ls_raw <- competitor[["linescores"]] %||% list()
    scores <- suppressWarnings(
      as.integer(vapply(ls_raw, function(p) scalar(p[["value"]], NA_character_), character(1)))
    )
    result <- list()
    result[[paste0(prefix, "_h1")]] <- if (length(scores) >= 1) scores[[1L]] else NA_integer_
    result[[paste0(prefix, "_h2")]] <- if (length(scores) >= 2) scores[[2L]] else NA_integer_
    n_ot <- max(0L, length(scores) - 2L)
    for (ot in seq_len(n_ot))
      result[[paste0(prefix, "_ot", ot)]] <- scores[[2L + ot]]
    result
  }
  
  home_ls <- parse_ls(home, "home")
  away_ls <- parse_ls(away, "away")
  
  row <- c(
    list(
      game_id         = game_id,
      game_date       = game_date,
      status          = status_name,
      final_detail    = short_detail,
      periods_played  = periods_played,
      went_to_ot      = went_to_ot,
      ot_periods      = ot_periods,
      neutral_site    = isTRUE(comp[["neutralSite"]]),
      conference_game = isTRUE(comp[["conferenceCompetition"]]),
      venue           = venue_name,
      venue_city      = venue_city,
      venue_state     = venue_state,
      home_team       = scalar(home[["team"]][["displayName"]]),
      home_abbr       = scalar(home[["team"]][["abbreviation"]]),
      home_id         = scalar(home[["team"]][["id"]]),
      home_score      = suppressWarnings(as.integer(scalar(home[["score"]]))),
      home_winner     = isTRUE(home[["winner"]]),
      away_team       = scalar(away[["team"]][["displayName"]]),
      away_abbr       = scalar(away[["team"]][["abbreviation"]]),
      away_id         = scalar(away[["team"]][["id"]]),
      away_score      = suppressWarnings(as.integer(scalar(away[["score"]]))),
      away_winner     = isTRUE(away[["winner"]])
    ),
    home_ls,
    away_ls
  )
  as_tibble(row)
}

get_scoreboard_page <- function(date_str, page = 1L) {
  raw  <- safe_get(BASE_SCOREBOARD, query = list(
    dates  = date_str,
    groups = 50,
    limit  = 300,
    page   = page
  ))
  safe_parse(raw)
}

collect_games_from_scoreboard <- function(start_date, end_date) {
  dates   <- seq(start_date, end_date, by = "day")
  n       <- length(dates)
  all_rows <- list()
  
  message(sprintf("Collecting game IDs + linescores from scoreboard (%s → %s)...",
                  start_date, end_date))
  
  for (i in seq_along(dates)) {
    d_str <- format(dates[i], "%Y%m%d")
    page  <- 1L
    
    repeat {
      data <- get_scoreboard_page(d_str, page)
      if (is.null(data)) break
      
      events <- data[["events"]] %||% list()
      if (length(events) == 0) break
      
      for (ev in events) {
        row <- tryCatch(parse_scoreboard_event(ev), error = function(e) NULL)
        if (!is.null(row)) all_rows <- c(all_rows, list(row))
      }
      
      page_count <- as.integer(scalar(data[["pageCount"]], "1"))
      if (is.na(page_count) || page >= page_count) break
      page <- page + 1L
      Sys.sleep(PAUSE_GET)
    }
    
    if (i %% 14 == 0 || i == n)
      message(sprintf("  [%d/%d] %s  |  games so far: %d", i, n, d_str, length(all_rows)))
    Sys.sleep(PAUSE_GET)
  }
  
  bind_rows(all_rows)
}

# ── Step 2: stat alias table ──────────────────────────────────────────────────

STAT_ALIASES <- list(
  fg           = list(names  = c("fieldGoalsMade-fieldGoalsAttempted", "FGM-FGA", "fieldGoals"),
                      labels = c("fg", "field goals")),
  fg_pct       = list(names  = c("fieldGoalPct", "fieldGoalPercentage"),
                      labels = c("fg%", "field goal pct")),
  three_pt     = list(names  = c("threePointFieldGoalsMade-threePointFieldGoalsAttempted",
                                 "3PM-3PA", "threePointFieldGoals"),
                      labels = c("3pt", "3-pt", "three point", "3p")),
  three_pt_pct = list(names  = c("threePointFieldGoalPct", "threePointFieldGoalPercentage"),
                      labels = c("3p%", "3pt%", "three point pct")),
  ft           = list(names  = c("freeThrowsMade-freeThrowsAttempted", "FTM-FTA", "freeThrows"),
                      labels = c("ft", "free throws")),
  ft_pct       = list(names  = c("freeThrowPct", "freeThrowPercentage"),
                      labels = c("ft%", "free throw pct")),
  reb_total    = list(names  = c("totalRebounds", "rebounds"),
                      labels = c("reb", "rebounds", "total reb")),
  reb_off      = list(names  = c("offensiveRebounds"),
                      labels = c("oreb", "off reb")),
  reb_def      = list(names  = c("defensiveRebounds"),
                      labels = c("dreb", "def reb")),
  assists      = list(names  = c("assists"),
                      labels = c("ast", "assists")),
  steals       = list(names  = c("steals"),
                      labels = c("stl", "steals")),
  blocks       = list(names  = c("blocks"),
                      labels = c("blk", "blocks")),
  turnovers    = list(names  = c("turnovers"),
                      labels = c("to", "tov", "turnovers")),
  fouls        = list(names  = c("fouls", "personalFouls", "totalFouls"),
                      labels = c("pf", "fouls")),
  pts          = list(names  = c("points", "totalPoints"),
                      labels = c("pts", "points"))
)

extract_team_stats <- function(stat_list) {
  if (length(stat_list) == 0)
    return(setNames(as.list(rep(NA_character_, length(STAT_ALIASES))), names(STAT_ALIASES)))
  
  by_name  <- setNames(
    lapply(stat_list, function(s) scalar(s[["displayValue"]] %||% s[["value"]])),
    vapply(stat_list, function(s) scalar(s[["name"]], ""), character(1))
  )
  by_label <- setNames(
    lapply(stat_list, function(s) scalar(s[["displayValue"]] %||% s[["value"]])),
    vapply(stat_list, function(s) tolower(scalar(s[["label"]] %||% s[["shortDisplayName"]], "")), character(1))
  )
  
  lapply(STAT_ALIASES, function(ali) {
    for (nm in ali$names)  { v <- by_name[[nm]];  if (!is.null(v)) return(v) }
    for (lb in ali$labels) { v <- by_label[[lb]]; if (!is.null(v)) return(v) }
    NA_character_
  })
}

# ── Step 3: fetch box-score stats from summary ────────────────────────────────

fetch_summary_stats <- function(game_id, home_id, away_id) {
  raw  <- safe_get(BASE_SUMMARY, query = list(event = game_id))
  data <- safe_parse(raw)
  if (is.null(data)) return(NULL)
  
  empty      <- setNames(as.list(rep(NA_character_, length(STAT_ALIASES))), names(STAT_ALIASES))
  home_stats <- empty
  away_stats <- empty
  
  bs_teams <- data[["boxscore"]][["teams"]] %||% list()
  
  if (length(bs_teams) >= 1) {
    for (t in bs_teams) {
      t_id <- scalar(t[["team"]][["id"]], "")
      s    <- extract_team_stats(t[["statistics"]] %||% list())
      if      (t_id == home_id) home_stats <- s
      else if (t_id == away_id) away_stats <- s
    }
    # Positional fallback: ESPN returns away first, home second
    if (all(is.na(unlist(home_stats))) && length(bs_teams) >= 2) {
      away_stats <- extract_team_stats(bs_teams[[1]][["statistics"]] %||% list())
      home_stats <- extract_team_stats(bs_teams[[2]][["statistics"]] %||% list())
    }
  }
  
  home_named <- setNames(home_stats, paste0("home_", names(home_stats)))
  away_named <- setNames(away_stats, paste0("away_", names(away_stats)))
  as_tibble(c(list(game_id = game_id), home_named, away_named))
}

# ── Step 4: diagnostic ───────────────────────────────────────────────────────
#
# Run on any completed game — especially useful on a known OT game.
# To find a known OT game ID, scan your scoreboard data:
#   sb <- collect_games_from_scoreboard(as.Date("2024-11-06"), as.Date("2024-11-12"))
#   sb |> filter(went_to_ot) |> select(game_id, home_team, away_team, final_detail)

diagnose_game <- function(game_id) {
  # ── Scoreboard (OT + linescores) ────────────────────────────────────────
  # Pick the date from any date near when you think the game occurred,
  # OR use the summary endpoint's header for a quick check
  raw  <- safe_get(BASE_SUMMARY, query = list(event = game_id))
  data <- safe_parse(raw)
  if (is.null(data)) { message("Fetch failed for ", game_id); return(invisible(NULL)) }
  
  comp <- data[["header"]][["competitions"]][[1]] %||% list()
  
  cat("\n── Date ─────────────────────────────────────────────────\n")
  cat("comp$date =", scalar(comp[["date"]], "NOT FOUND"), "\n")
  
  cat("\n── Status / OT (from summary header) ───────────────────\n")
  cat("shortDetail =", scalar(comp[["status"]][["type"]][["shortDetail"]], "NOT FOUND"), "\n")
  cat("period      =", scalar(comp[["status"]][["period"]], "NOT FOUND"), "\n")
  
  cat("\n── Linescores in summary header competitors ────────────\n")
  for (comp_obj in comp[["competitors"]] %||% list()) {
    tnam  <- scalar(comp_obj[["team"]][["displayName"]], "?")
    ls    <- comp_obj[["linescores"]] %||% list()
    if (length(ls) == 0) {
      cat(sprintf("  %-30s  linescores: NOT PRESENT\n", tnam))
    } else {
      vals <- vapply(ls, function(p) scalar(p[["value"]], "?"), character(1))
      cat(sprintf("  %-30s  linescores: %s\n", tnam, paste(vals, collapse = " | ")))
    }
  }
  
  cat("\n── Venue ────────────────────────────────────────────────\n")
  v <- comp[["venue"]]
  if (!is.null(v) && length(v) > 0) {
    cat("fullName:", scalar(v[["fullName"]], "missing"), "\n")
    cat("city:    ", scalar(v[["address"]][["city"]], "missing"), "\n")
  } else {
    gi <- data[["gameInfo"]][["venue"]]
    if (!is.null(gi)) {
      cat("(gameInfo) fullName:", scalar(gi[["fullName"]], "missing"), "\n")
      cat("(gameInfo) city:    ", scalar(gi[["address"]][["city"]], "missing"), "\n")
    } else {
      cat("Venue NOT found in competition or gameInfo\n")
    }
  }
  
  cat("\n── Boxscore team stats ──────────────────────────────────\n")
  for (idx in seq_along(data[["boxscore"]][["teams"]] %||% list())) {
    t    <- data[["boxscore"]][["teams"]][[idx]]
    tnam <- scalar(t[["team"]][["displayName"]], paste("Team", idx))
    cat(sprintf("\nTeam %d: %s\n", idx, tnam))
    for (s in t[["statistics"]] %||% list()) {
      cat(sprintf("  name=%-55s label=%-20s val=%s\n",
                  scalar(s[["name"]], "?"),
                  scalar(s[["label"]] %||% s[["shortDisplayName"]], "?"),
                  scalar(s[["displayValue"]] %||% s[["value"]], "?")))
    }
  }
  invisible(data)
}

# ── Step 5: orchestrate ───────────────────────────────────────────────────────

scrape_season <- function(start_date  = SEASON_START,
                          end_date    = SEASON_END,
                          output_file = OUTPUT_FILE) {
  
  # Pass 1: scoreboard — game IDs, dates, venue, scores, linescores, OT
  sb <- collect_games_from_scoreboard(start_date, end_date)
  message(sprintf("\nScoreboard: %d games collected. Fetching box-score stats...\n", nrow(sb)))
  
  # Pass 2: summary — box-score stats only
  n      <- nrow(sb)
  stats_rows <- vector("list", n)
  errors     <- character(0)
  
  for (i in seq_len(n)) {
    gid <- sb$game_id[i]
    stats_rows[[i]] <- tryCatch(
      fetch_summary_stats(gid, sb$home_id[i], sb$away_id[i]),
      error = function(e) { errors <<- c(errors, gid); NULL }
    )
    if (i %% 500 == 0 || i == n)
      message(sprintf("  [%d / %d]  errors: %d", i, n, length(errors)))
    Sys.sleep(PAUSE_GET)
  }
  
  stats_df <- bind_rows(Filter(Negate(is.null), stats_rows))
  
  # Merge: scoreboard metadata + linescore columns LEFT JOIN summary stats
  result <- left_join(sb, stats_df, by = "game_id")
  
  # Numeric coercions
  num_cols <- intersect(names(result), c(
    "home_score", "away_score", "periods_played", "ot_periods",
    grep("_pct$",  names(result), value = TRUE),
    grep("^(home|away)_(h[12]|ot[0-9]+|reb|assists|steals|blocks|turnovers|fouls|pts)",
         names(result), value = TRUE)
  ))
  result <- mutate(result, across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))
  
  #write.csv(result, output_file, row.names = FALSE)
  message(sprintf("\nDone. %d rows → '%s'.  %d summary errors.",
                  nrow(result), output_file, length(errors)))
  invisible(result)
}

# =============================================================================
# USAGE
# =============================================================================
#
# Change SEASON_YEAR at the top to switch seasons (currently set to 2025).
#
# 1. Run diagnostic on one game:
#      diagnose_game("401524691")
#
# 2. Smoke test — one week, check OT games populate correctly:
#      df <- scrape_season(as.Date("2024-11-04"), as.Date("2024-11-10"), "test.csv")
#      df |> dplyr::filter(went_to_ot) |>
#        dplyr::select(game_date, home_team, away_team, final_detail,
#                      home_h1, home_h2, home_ot1, away_h1, away_h2, away_ot1)
#
# 3. Full season:
      df6 <- scrape_season()
      write.csv(df6,"UpdatedGamesCSV.csv")
#
# =============================================================================
# OUTPUT COLUMNS
# =============================================================================
#
# From scoreboard (always populated):
#   game_id, game_date, status, final_detail, periods_played, went_to_ot,
#   ot_periods, neutral_site, conference_game, venue, venue_city, venue_state,
#   home_team, home_abbr, home_id, home_score, home_winner,
#   away_team, away_abbr, away_id, away_score, away_winner,
#   home_h1, home_h2, [home_ot1, home_ot2, ...],
#   away_h1, away_h2, [away_ot1, away_ot2, ...]
#
# From summary (box-score stats, NA if game had no box-score data):
#   home_fg, home_fg_pct, home_three_pt, home_three_pt_pct,
#   home_ft, home_ft_pct, home_reb_total, home_reb_off, home_reb_def,
#   home_assists, home_steals, home_blocks, home_turnovers, home_fouls, home_pts
#   away_* (same set)
