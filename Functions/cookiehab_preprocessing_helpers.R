# Helpers for cookie habituation AnimalPos preprocessing.

cookiehab_read_animalpos_csv <- function(path) {
  readr::read_delim(
    path,
    delim = ";",
    col_types = readr::cols(.default = readr::col_guess()),
    trim_ws = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )
}

cookiehab_parse_datetime <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x <- dplyr::if_else(
    stringr::str_detect(x, "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}$"),
    paste0(x, ":00"),
    x
  )
  as.POSIXct(x, format = "%d.%m.%Y %H:%M:%S", tz = "UTC")
}

cookiehab_position_lookup <- function() {
  tibble::tibble(
    PositionID = 1:8,
    xPos_bin = c(0, 100, 200, 300, 0, 100, 200, 300),
    yPos_bin = c(0, 0, 0, 0, 116, 116, 116, 116)
  )
}

cookiehab_position_id <- function(x_pos, y_pos) {
  x_bin <- dplyr::case_when(
    x_pos < 100 ~ 0,
    x_pos < 200 ~ 100,
    x_pos < 300 ~ 200,
    x_pos >= 300 ~ 300,
    TRUE ~ NA_real_
  )
  y_bin <- dplyr::case_when(
    y_pos < 116 ~ 0,
    y_pos >= 116 ~ 116,
    TRUE ~ NA_real_
  )

  dplyr::left_join(
    tibble::tibble(.row_id = seq_along(x_pos), xPos_bin = x_bin, yPos_bin = y_bin),
    cookiehab_position_lookup(),
    by = c("xPos_bin", "yPos_bin")
  ) |>
    dplyr::arrange(.row_id) |>
    dplyr::pull(PositionID)
}

cookiehab_phase <- function(datetime) {
  hhmm <- format(datetime, "%H:%M", tz = "UTC")
  dplyr::if_else(hhmm >= "18:30" | hhmm < "06:30", "Active", "Inactive")
}

cookiehab_minutes_of_day <- function(datetime) {
  as.integer(format(datetime, "%H", tz = "UTC")) * 60L +
    as.integer(format(datetime, "%M", tz = "UTC"))
}

cookiehab_assign_sex <- function(batch) {
  batch <- toupper(stringr::str_trim(as.character(batch)))
  dplyr::case_when(
    batch %in% c("B1", "B2", "B5") ~ "Male",
    batch %in% c("B3", "B4", "B6") ~ "Female",
    TRUE ~ NA_character_
  )
}

cookiehab_read_id_list <- function(path) {
  if (!file.exists(path)) return(character())
  readr::read_lines(path, progress = FALSE) |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_replace_all("\\s+", "") |>
    toupper() |>
    purrr::discard(~ is.na(.x) || .x == "") |>
    unique()
}

cookiehab_assign_group <- function(animal_id, sus_ids, con_ids) {
  id_norm <- stringr::str_to_upper(stringr::str_replace_all(stringr::str_trim(as.character(animal_id)), "\\s+", ""))
  dplyr::case_when(
    id_norm %in% sus_ids ~ "SUS",
    id_norm %in% con_ids ~ "CON",
    TRUE ~ "RES"
  )
}

cookiehab_add_carried_rows <- function(data, transition_times) {
  transition_times <- sort(unique(as.POSIXct(transition_times, origin = "1970-01-01", tz = "UTC")))
  transition_times <- transition_times[!is.na(transition_times)]
  if (length(transition_times) == 0 || nrow(data) == 0) return(data)

  key_cols <- c("SourceFile", "Batch", "RawParadigm", "AnimalID", "System")
  carried <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::group_modify(function(.x, .y) {
      times <- transition_times[transition_times > min(.x$DateTime, na.rm = TRUE) &
                                  transition_times < max(.x$DateTime, na.rm = TRUE)]
      if (length(times) == 0) return(.x[0, , drop = FALSE])
      purrr::map_dfr(times, function(t) {
        if (any(.x$DateTime == t)) return(.x[0, , drop = FALSE])
        previous <- .x |>
          dplyr::filter(DateTime < t) |>
          dplyr::slice_max(DateTime, n = 1, with_ties = FALSE)
        if (nrow(previous) == 0) return(.x[0, , drop = FALSE])
        previous$DateTime <- t
        previous
      })
    }) |>
    dplyr::ungroup()

  dplyr::bind_rows(data, carried) |>
    dplyr::distinct(SourceFile, AnimalID, System, DateTime, .keep_all = TRUE) |>
    dplyr::arrange(SourceFile, System, AnimalID, DateTime)
}

cookiehab_add_transition_rows <- function(data) {
  dates <- sort(unique(as.Date(data$DateTime, tz = "UTC")))
  phase_times <- unlist(lapply(dates, function(date) {
    as.POSIXct(
      paste(date, c("00:00:00", "06:30:00", "17:00:00", "17:45:00", "18:00:00", "18:30:00")),
      tz = "UTC"
    )
  }))
  half_hours <- unlist(lapply(dates, function(date) {
    seq(
      as.POSIXct(paste(date, "00:00:00"), tz = "UTC"),
      as.POSIXct(paste(date, "23:30:00"), tz = "UTC"),
      by = "30 min"
    )
  }))
  cookiehab_add_carried_rows(data, c(phase_times, half_hours))
}

cookiehab_count_phases <- function(data) {
  data |>
    dplyr::arrange(SourceFile, System, AnimalID, DateTime) |>
    dplyr::group_by(SourceFile, System, AnimalID) |>
    dplyr::mutate(
      PhaseRun = cumsum(dplyr::row_number() == 1L | Phase != dplyr::lag(Phase)),
      ConsecActive = dplyr::if_else(
        Phase == "Active",
        cumsum(Phase == "Active" & (dplyr::row_number() == 1L | Phase != dplyr::lag(Phase))),
        0L
      ),
      ConsecInactive = dplyr::if_else(
        Phase == "Inactive",
        cumsum(Phase == "Inactive" & (dplyr::row_number() == 1L | Phase != dplyr::lag(Phase))),
        0L
      )
    ) |>
    dplyr::select(-PhaseRun) |>
    dplyr::ungroup()
}

cookiehab_crop_target_phases <- function(data) {
  data |>
    dplyr::filter(!(Phase == "Inactive" & ConsecInactive == 1L)) |>
    dplyr::filter(!(Phase == "Inactive" & ConsecInactive > 2L)) |>
    dplyr::filter(!(Phase == "Active" & ConsecActive > 2L))
}

cookiehab_add_annotations <- function(data) {
  data |>
    dplyr::mutate(
      MinutesOfDay = cookiehab_minutes_of_day(DateTime),
      CookieWindowPrimary = Phase == "Inactive" &
        ConsecInactive == 2L &
        MinutesOfDay >= 17L * 60L &
        MinutesOfDay < 18L * 60L,
      CookieSubWindow = dplyr::case_when(
        CookieWindowPrimary & MinutesOfDay < 17L * 60L + 45L ~ "cookie_present_planned_17_1745",
        CookieWindowPrimary & MinutesOfDay >= 17L * 60L + 45L ~ "cookie_likely_gone_1745_1800",
        TRUE ~ NA_character_
      ),
      CookieHabEpoch = dplyr::case_when(
        CookieWindowPrimary ~ "I2_17_18",
        Phase == "Inactive" & ConsecInactive == 2L & MinutesOfDay < 17L * 60L ~ "I2_pre_cookie",
        Phase == "Inactive" & ConsecInactive == 2L & MinutesOfDay >= 18L * 60L ~ "I2_post_cookie",
        TRUE ~ "outside_primary_cookiehab_window"
      )
    )
}

cookiehab_preprocess_one_file <- function(path, sus_ids = character(), con_ids = character()) {
  source_file <- basename(path)
  batch <- stringr::str_extract(source_file, "B[1-6]")

  raw <- cookiehab_read_animalpos_csv(path)
  required <- c("DateTime", "Animal", "xPos", "yPos")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Missing required columns in ", path, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }

  raw |>
    dplyr::select(-dplyr::any_of(c("RFID", "AM", "zPos"))) |>
    dplyr::mutate(
      DateTime = cookiehab_parse_datetime(DateTime),
      Animal = as.character(Animal),
      SourceFile = source_file,
      Dataset = "cookiehab",
      RawParadigm = "EPMaftercagechange",
      CageChange = "EPMaftercagechange",
      Epoch = "EPMaftercagechange",
      Batch = batch,
      AnimalNum = stringr::str_split_fixed(Animal, "[-_]", 2)[, 1],
      AnimalID = AnimalNum,
      System = stringr::str_split_fixed(Animal, "[-_]", 2)[, 2],
      PositionID = cookiehab_position_id(suppressWarnings(as.numeric(xPos)), suppressWarnings(as.numeric(yPos))),
      Sex = cookiehab_assign_sex(Batch),
      Group = cookiehab_assign_group(AnimalID, sus_ids, con_ids)
    ) |>
    dplyr::filter(!is.na(DateTime), !is.na(AnimalID), AnimalID != "", !is.na(System), System != "") |>
    dplyr::select(
      DateTime, AnimalID, AnimalNum, System, PositionID, Batch, Sex, Group,
      Dataset, RawParadigm, CageChange, Epoch, SourceFile
    ) |>
    dplyr::arrange(System, AnimalID, DateTime) |>
    cookiehab_add_transition_rows() |>
    dplyr::mutate(Phase = cookiehab_phase(DateTime)) |>
    cookiehab_count_phases() |>
    cookiehab_crop_target_phases() |>
    dplyr::group_by(SourceFile, System, AnimalID) |>
    dplyr::mutate(HalfHoursElapsed = as.numeric(difftime(DateTime, min(DateTime, na.rm = TRUE), units = "mins")) %/% 30) |>
    dplyr::ungroup() |>
    cookiehab_add_annotations() |>
    dplyr::arrange(System, AnimalID, DateTime)
}
