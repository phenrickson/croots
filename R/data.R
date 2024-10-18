# function to authenticate to google sheets using email
auth_sheets = function(email = keyring::key_get("googlesheets")) {

  googlesheets4::gs4_auth(email = email)

}

# set url for project
project_url = function() {
  "https://docs.google.com/spreadsheets/d/1B2BWf5Apum6EJq-SCv5pgqbvKBGzYvAlZCK9JnJWzGQ/edit?gid=0#gid=0"
}

# function to load in recruits
load_recruits = function(url = project_url(), sheet = "Recruits by Archetype", skip = 9, ...) {
  
  auth_sheets()
  googlesheets4::read_sheet(url, sheet = sheet, skip = skip, ...)
  
}

# function to tidy recruiting data
tidy_recruits = function(data) {

  tidy_croots = function(data) {
  
    data |>
    # clean up names
    janitor::clean_names() |>
    # select columns
    select(dynasty:attr10) |>
    # rename columns
    rename(
      height = height_in,
      year_signing_day = year_at_signing_day
    ) |>
    rename_positions() |>
    filter_missing() |>
    add_player_id()
  }

  data |> 
    tidy_croots()

}

overall_cols = function() {
  
  c("fr", "rs_fr", "so", "jr", "sr")
  
}

add_player_id = function(data, vars = c("dynasty", "icon", "name", "year_signing_day", "position"), remove = F) {

  data |> 
    unite("player_id", vars, sep = "_", remove = remove) |>
    select(player_id, everything())

}

rename_positions = function(data) {

  data |>
    mutate(
      position_group = case_when(
      position %in% c('LE', 'RE') ~ 'DE',
      position %in% c('RT', 'LT') ~ 'OT',
      position %in% c('LG', 'RG') ~ 'OG',
      TRUE ~ position)
    )
}

filter_missing = function(data, vars = c("dynasty", "name")) {

  data |>
    filter(!is.na(name) & !is.na(dynasty) & !is.na(position))

}

extract_numeric <- function(x) {
  # Match sequences of digits that may start with a negative sign
  matches <- gregexpr("-?\\d+", x)
  regmatches(x, matches)
  as.numeric(unlist(regmatches(x, matches)))
}


clean_overall = function(data, cols = overall_cols()) {
  
  data |>
  mutate(
    fr = map(fr, extract_numeric),
    rs_fr = map(rs_fr, extract_numeric),
    so = map(so, extract_numeric),
    jr = map(jr, extract_numeric),
    sr = map(sr, extract_numeric)
  ) |>
  unnest(cols = cols, keep_empty = T)
  
}

longer_overall = function(data, cols = overall_cols()) {
  
  data |>
  tidyr::pivot_longer(
    cols = c("fr", "rs_fr", "so", "jr", "sr"),
    names_to = c("overall_class"),
    values_to = c("overall_rating")
  ) |>
  mutate(
    overall_class = factor(overall_class, levels = c("fr", "rs_fr", "so", "jr", "sr"))
  ) |>
  mutate(
    overall_rating = case_when(overall_rating <= 0 ~ NA_real_, TRUE ~ overall_rating)
  )
  
}

tidy_overall = function(data) {
  
  data |>
  clean_overall() |>
  longer_overall()
}

