#' Get school rename from Google sheet
get_school_renames <- function() {
  suppressMessages(googlesheets4::read_sheet(
    ss = "1Q_bbEpuKPa4oN4L_MBPc-CbdhSLpyWXv3rHpvIgGIPY",
    sheet = "School renames",
    col_types = "c"
  )) %>% dplyr::distinct()
}


#' Get school IDs from Google sheet
get_school_ids <- function() {
  suppressMessages(googlesheets4::read_sheet(
    ss = "1Q_bbEpuKPa4oN4L_MBPc-CbdhSLpyWXv3rHpvIgGIPY",
    sheet = "Schools",
    col_types = "c"
  )) %>% dplyr::distinct()
}


#' Get dept rename from Google sheet
get_dept_renames <- function() {
  suppressMessages(googlesheets4::read_sheet(
    ss = "1Q_bbEpuKPa4oN4L_MBPc-CbdhSLpyWXv3rHpvIgGIPY",
    sheet = "Dept renames",
    col_types = "c"
  )) %>% dplyr::distinct()
}


#' Get dept-program map from Google sheet
get_dept_program_map <- function() {
  suppressMessages(googlesheets4::read_sheet(
    ss = "1Q_bbEpuKPa4oN4L_MBPc-CbdhSLpyWXv3rHpvIgGIPY",
    sheet = "Dept-program map",
    col_types = "c"
  )) %>%
    dplyr::distinct() %>%
    tidyr::separate_rows(.data$program_abbr, sep = "\\/")
}


#' Rename courses' school names
#'
#' @param courses (tibble)
#' @param school_renames (tibble)
rename_schools <- function(courses, school_renames) {
  courses %>%
    dplyr::left_join(school_renames, by = c("course_school" = "old_name")) %>%
    dplyr::mutate(school_name = ifelse(is.na(.data$new_name), .data$course_school, .data$new_name)) %>%
    dplyr::select(-.data$course_school, -.data$new_name)
}


#' Join school IDs to courses by school names
#'
#' @param courses (tibble)
#' @param school_ids (tibble)
id_schools <- function(courses, school_ids) {
  d <- dplyr::left_join(
    courses,
    school_ids %>% dplyr::rename(school_name = .data$name, school_id = .data$id),
    by = "school_name"
  )

  stopifnot(all(!is.na(d$school_id)))

  dplyr::select(d, -.data$school_name)
}


#' Rename courses' department names
#'
#' @param courses (tibble)
#' @param dept_renames (tibble)
rename_depts <- function(courses, dept_renames) {
  courses %>%
    dplyr::mutate(course_dept = stringr::str_replace_all(.data$course_dept, "&", "and")) %>%
    dplyr::left_join(dept_renames, by = c("school_id", "course_dept" = "old_name")) %>%
    dplyr::mutate(dept_name = ifelse(is.na(.data$new_name), .data$course_dept, .data$new_name)) %>%
    dplyr::select(-.data$course_dept, -.data$new_name)
}


#' Use incorrect department values to assign correct department and program values
#'
#' @param courses (tibble)
#' @param dept_program_map (tibble)
#' @param course_code_colname (character)
#'
#' The dept_name field often contains a program name (e.g. "Igbo") that needs to
#' moved into a program name column and mapped to a proper department name
#' (e.g. "African and African American Studies"). Other courses have department
#' name and program name that should be equal (e.g. "Linguistics").
deconvolute_program_and_depts <- function(courses, dept_program_map, course_code_colname) {
  # extract program abbreviation from course codes
  courses_todo <- dplyr::mutate(courses, temp_id = seq_len(dplyr::n())) %>%
    dplyr::rename(code = !!course_code_colname) %>%
    dplyr::mutate(program_abbr = stringr::str_extract(.data$code, "^[^[:space:]]+")) %>%
    dplyr::mutate(program_abbr = ifelse(
      stringr::str_detect(.data$code, "[:space:]"),
      .data$program_abbr,
      NA
    ))

  # use school, program name, and program abbreviation to match
  courses_from_program_and_abbr <- courses_todo %>%
    dplyr::left_join(
      dept_program_map,
      by = c("school_id", "dept_name" = "program_name", "program_abbr")
    ) %>%
    dplyr::filter(!is.na(.data$dept_name.y)) %>%
    dplyr::rename(program_name = .data$dept_name, dept_name = .data$dept_name.y)

  # continue with rest of courses
  courses_todo <- dplyr::anti_join(courses_todo, courses_from_program_and_abbr, by = "temp_id")

  # use school and program name to match
  courses_from_program <- courses_todo %>%
    dplyr::left_join(
      dept_program_map %>%
        dplyr::select(-.data$program_abbr) %>%
        dplyr::distinct(),
      by = c("school_id", "dept_name" = "program_name")
    ) %>%
    dplyr::filter(!is.na(.data$dept_name.y)) %>%
    dplyr::rename(program_name = .data$dept_name, dept_name = .data$dept_name.y)

  courses_todo <- dplyr::anti_join(courses_todo, courses_from_program, by = "temp_id")

  # use school and program abbreviation to match
  courses_from_abbr <- courses_todo %>%
    dplyr::select(-.data$dept_name) %>%
    dplyr::left_join(
      dept_program_map,
      by = c("school_id", "program_abbr")
    ) %>%
    dplyr::filter(!is.na(.data$program_name))

  courses_todo <- dplyr::anti_join(courses_todo, courses_from_abbr, by = "temp_id")

  if (nrow(courses_todo) > 0) {
    # stop because some courses couldn't be mapped (the sheet needs to be updated)
    missing_maps <- courses_todo %>%
      dplyr::select(.data$school_id, .data$dept_name, .data$code) %>%
      jsonlite::toJSON()

    stop("Missing dept/program map for: ", missing_maps)
  }

  dplyr::bind_rows(courses_from_program_and_abbr, courses_from_program, courses_from_abbr) %>%
    dplyr::select(-.data$temp_id, -.data$program_abbr) %>%
    dplyr::rename(!!course_code_colname := .data$code)
}


#' Slufigy a name
#'
#' @param x (character)
slugify_sdp_name <- Vectorize(function(x) {
  x %>%
    stringi::stri_trans_general(id = "Latin-ASCII; Lower") %>%
    stringr::str_replace_all("[^[:alnum:]]+", "-")
})


#' Generate school/dept/program IDs
#'
#' @param courses (tibble)
gen_sdp_ids <- function(courses) {
  courses %>%
    dplyr::mutate(
      dept_id = slugify_sdp_name(.data$dept_name),
      program_id = slugify_sdp_name(.data$program_name)
    ) %>%
    dplyr::mutate(dept_id = stringr::str_c(.data$school_id, .data$dept_id, sep = "--")) %>%
    dplyr::mutate(program_id = stringr::str_c(.data$dept_id, .data$program_id, sep = "--"))
}


#' Clean school/dept/program values
#'
#' @param courses (tibble)
#' @param course_code_colname (character)
clean_sdp <- function(courses, course_code_colname = "code") {
  school_renames <- sdpCleaner::get_school_renames()
  school_ids <- sdpCleaner::get_school_ids()
  dept_renames <- sdpCleaner::get_dept_renames()
  dept_program_map <- sdpCleaner::get_dept_program_map()

  courses %>%
    sdpCleaner::rename_schools(school_renames) %>%
    sdpCleaner::id_schools(school_ids) %>%
    sdpCleaner::rename_depts(dept_renames) %>%
    sdpCleaner::deconvolute_program_and_depts(dept_program_map, course_code_colname) %>%
    sdpCleaner::gen_sdp_ids()
}
