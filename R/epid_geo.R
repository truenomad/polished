# EPID-driven geography cleaner.
#
# An EPID is a hierarchical case identifier conventionally shaped as
# COUNTRY-PROVINCE-DISTRICT-YEAR-SERIAL (e.g. "NIE-BOS-XYZ-24-001"). The
# leading three characters are a country code; subsequent dash-delimited
# segments encode province/district abbreviations. The EPID therefore acts
# as a recovery key for administrative geography when admin names/GUIDs are
# missing.
#
# Two capabilities are kept separate:
#   (A) parse the EPID into its component codes (pure, no reference data);
#   (B) infer/clean admin names + GUIDs via a provenance-stamped cascade.
#
# Everything here is standalone: it operates on any data frame carrying an
# `epid` column and never reaches into the rest of the package, except for an
# optional name-canonicalisation hook resolved at call time.

# ---------------------------------------------------------------------
# Pure parsers (capability A)
# ---------------------------------------------------------------------

#' Split an EPID into its component segments
#'
#' Splits each EPID on `sep` into one column per element of `parts`,
#' preserving `NA` and never erroring on malformed input.
#'
#' @param epid Character vector of EPID strings.
#' @param sep Single-character delimiter between segments. Default `"-"`.
#' @param parts Character vector naming the output columns, in order. Default
#'   `c("country", "province", "district", "year", "serial")`.
#' @param extra How to treat segments beyond `length(parts)`: `"drop"`
#'   (default) discards them; `"merge"` collapses the remainder into the last
#'   column.
#' @param fill How to pad EPIDs with fewer segments than `parts`. Only
#'   `"right"` is supported: missing trailing segments become `NA`.
#' @return A [tibble][tibble::tibble] with one character column per `parts`
#'   element. Blank/`NA` EPIDs yield an all-`NA` row.
#' @examples
#' epid_split(c("NIE-BOS-XYZ-24-001", "AGO-LUA", NA))
#' epid_split("NIE-BOS-XYZ-24-001-EXTRA", extra = "merge")
#' @export
epid_split <- function(
  epid,
  sep = "-",
  parts = c("country", "province", "district", "year", "serial"),
  extra = "drop",
  fill = "right"
) {
  if (!is.character(sep) || length(sep) != 1L || !nzchar(sep)) {
    cli::cli_abort("{.arg sep} must be a single non-empty string.")
  }
  if (!extra %in% c("drop", "merge")) {
    cli::cli_abort("{.arg extra} must be one of {.val drop} or {.val merge}.")
  }
  if (!identical(fill, "right")) {
    cli::cli_abort("{.arg fill} only supports {.val right}.")
  }

  trimmed <- trimws(as.character(epid))
  pieces <- stringr::str_split(trimmed, stringr::fixed(sep))
  n_parts <- length(parts)

  columns <- lapply(seq_len(n_parts), function(position) {
    vapply(
      pieces,
      function(segments) {
        if (length(segments) < position) {
          return(NA_character_)
        }
        value <- if (
          identical(extra, "merge") &&
            position == n_parts &&
            length(segments) > n_parts
        ) {
          paste(segments[position:length(segments)], collapse = sep)
        } else {
          segments[[position]]
        }
        if (is.na(value) || !nzchar(trimws(value))) NA_character_ else value
      },
      character(1)
    )
  })
  names(columns) <- parts

  result <- tibble::as_tibble(columns)
  blank_epid <- is.na(trimmed) | !nzchar(trimmed)
  result[blank_epid, ] <- NA_character_
  result
}

#' Extract the country code from an EPID
#'
#' Returns the leading country code: the first run of `n` word-characters,
#' matching how upstream systems parse the code.
#'
#' @param epid Character vector of EPID strings.
#' @param n Number of leading word-characters that form the code. Default `3`.
#' @param upper Whether to upper-case the result. Default `TRUE`.
#' @return Character vector of country codes (`NA` where none is found).
#' @examples
#' epid_country_code(c("NIE-BOS-XYZ-24-001", "ago-lua-01", NA))
#' @export
epid_country_code <- function(epid, n = 3, upper = TRUE) {
  trimmed <- trimws(as.character(epid))
  pattern <- sprintf("\\w{%d}", as.integer(n))
  code <- stringr::str_extract(trimmed, pattern)
  if (isTRUE(upper)) {
    code <- toupper(code)
  }
  code
}

#' Geographic prefix used for prefix-matching
#'
#' Returns the leading `length` characters of the normalised EPID
#' (trimmed, whitespace-collapsed, upper-cased) — the country+province+
#' district stem used to recover geography from sibling records.
#'
#' @param epid Character vector of EPID strings.
#' @param length Number of leading characters in the prefix. Default `11`.
#' @return Character vector of prefixes (`NA` where the EPID is blank).
#' @examples
#' epid_prefix(c("NIE-BOS-XYZ-24-001", NA))
#' @export
epid_prefix <- function(epid, length = 11) {
  normalised <- .epid_normalise(epid)
  prefix <- substr(normalised, 1L, as.integer(length))
  prefix[.epid_blank(prefix)] <- NA_character_
  prefix
}

#' Separate a contact EPID from its base case EPID
#'
#' Contact records reuse a case EPID with a trailing contact marker
#' (`C`, `CC`, `HC`, or `C` followed by digits). This splits the marker off
#' so contacts collapse onto their parent case for matching.
#'
#' @param epid Character vector of EPID strings.
#' @return A [tibble][tibble::tibble] with `epid_base` (marker removed) and
#'   `contact_code` (the extracted marker, `NA` when absent).
#' @examples
#' epid_strip_contact(c("NIE-BOS-XYZ-24-001", "NIE-BOS-XYZ-24-001CC"))
#' @export
epid_strip_contact <- function(epid) {
  normalised <- .epid_normalise(epid)
  marker <- "[-_ ]?(CC|HC|C[0-9]*)$"
  contact_code <- stringr::str_extract(normalised, marker)
  contact_code <- stringr::str_remove(contact_code, "^[-_ ]")
  contact_code[.epid_blank(contact_code)] <- NA_character_
  base <- stringr::str_remove(normalised, marker)
  base[.epid_blank(base)] <- NA_character_
  tibble::tibble(epid_base = base, contact_code = contact_code)
}

# ---------------------------------------------------------------------
# Normalisation helpers
# ---------------------------------------------------------------------

#' Normalise an EPID/string for matching
#'
#' Trims, collapses internal whitespace, upper-cases, and maps blanks to
#' `NA`.
#'
#' @param epid Character vector.
#' @return Normalised character vector.
#' @keywords internal
#' @noRd
.epid_normalise <- function(epid) {
  normalised <- trimws(as.character(epid))
  normalised <- gsub("\\s+", " ", normalised)
  normalised <- toupper(normalised)
  normalised[!nzchar(normalised)] <- NA_character_
  normalised
}

#' Test for blank (`NA`/empty/whitespace) values
#'
#' @param value Atomic vector.
#' @return Logical vector, `TRUE` where blank.
#' @keywords internal
#' @noRd
.epid_blank <- function(value) {
  is.na(value) | !nzchar(trimws(as.character(value)))
}

#' Whole-number scalar guards
#'
#' @param value Candidate value.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.is_count <- function(value) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    value >= 1 &&
    value == as.integer(value)
}

#' @param value Candidate value.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.is_nonneg_int <- function(value) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    value >= 0 &&
    value == as.integer(value)
}

# ---------------------------------------------------------------------
# Reference builders (capability B inputs)
# ---------------------------------------------------------------------

#' Build an EPID -> admin-value reference (most-recent-per-EPID)
#'
#' For each EPID, takes the most recent non-blank value of `admin_col`
#' (ties broken deterministically). Used to fill missing admin values from
#' other records that share the exact same EPID.
#'
#' @param data Data frame containing `epid_var`, `year_var`, and `admin_col`.
#' @param admin_col Name of the admin column to summarise.
#' @param epid_var EPID column name. Default `"epid"`.
#' @param year_var Recency column name. Default `"year"`.
#' @return A [tibble][tibble::tibble] with columns `epid_var` and `admin_col`,
#'   one row per EPID.
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("A-1", "A-1", "B-2"),
#'   year = c(2023, 2024, 2024),
#'   district = c(NA, "BOSSO", "LUANDA")
#' )
#' build_admin_ref(cases, "district")
#' @export
build_admin_ref <- function(
  data,
  admin_col,
  epid_var = "epid",
  year_var = "year"
) {
  required <- c(epid_var, year_var, admin_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing column{?s}: {.val {missing_cols}}.")
  }
  data |>
    dplyr::filter(!.epid_blank(.data[[admin_col]])) |>
    dplyr::arrange(
      dplyr::desc(.data[[year_var]]),
      .data[[admin_col]]
    ) |>
    dplyr::group_by(.data[[epid_var]]) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      !!epid_var := .data[[epid_var]],
      !!admin_col := as.character(.data[[admin_col]])
    ) |>
    dplyr::distinct()
}

#' Build a (prefix, year) -> unique admin-value reference
#'
#' For each geographic prefix and year, returns the admin value only when it
#' is unambiguous (exactly one distinct non-blank value); otherwise the value
#' is `NA` and `n_candidates` records how many distinct values competed.
#'
#' @param data Data frame containing `epid_var`, `year_var`, and `admin_col`.
#' @param admin_col Name of the admin column to summarise.
#' @param epid_var EPID column name. Default `"epid"`.
#' @param year_var Year column name. Default `"year"`.
#' @param prefix_length Prefix length passed to [epid_prefix()]. Default `11`.
#' @return A [tibble][tibble::tibble] with columns `prefix`, `year`,
#'   `n_candidates`, and `admin_col`.
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("NIE-BOS-AAA-1", "NIE-BOS-AAA-2", "NIE-BOS-BBB-1"),
#'   year = c(2024, 2024, 2024),
#'   district = c("BOSSO", "BOSSO", "BIRNI")
#' )
#' build_prefix_ref(cases, "district", prefix_length = 7)
#' @export
build_prefix_ref <- function(
  data,
  admin_col,
  epid_var = "epid",
  year_var = "year",
  prefix_length = 11
) {
  required <- c(epid_var, year_var, admin_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Missing column{?s}: {.val {missing_cols}}.")
  }
  data |>
    dplyr::filter(
      !.epid_blank(.data[[admin_col]]),
      !.epid_blank(.data[[epid_var]])
    ) |>
    dplyr::transmute(
      prefix = epid_prefix(.data[[epid_var]], prefix_length),
      year = .data[[year_var]],
      value = as.character(.data[[admin_col]])
    ) |>
    dplyr::filter(!is.na(prefix)) |>
    dplyr::group_by(prefix, year) |>
    dplyr::summarise(
      n_candidates = dplyr::n_distinct(value, na.rm = TRUE),
      value = dplyr::if_else(
        n_candidates == 1L,
        dplyr::first(sort(value)),
        NA_character_
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(!!admin_col := value) |>
    dplyr::arrange(prefix, year)
}

# ---------------------------------------------------------------------
# Country resolution (capability A -> name)
# ---------------------------------------------------------------------

#' Resolve an EPID country code to a country name
#'
#' Maps the 3-character country code (see [epid_country_code()]) to a name
#' and ISO3 via a caller-supplied crosswalk, matching the code against either
#' the crosswalk's code or ISO3 column. Resolution never guesses: a code that
#' maps to more than one distinct name is flagged ambiguous and left `NA`.
#'
#' Temporal validity is the caller's responsibility — pre-filter `ref` to the
#' period of interest (or pass a `region` with `region_var`) before calling.
#'
#' @param epid Character vector of EPID strings.
#' @param ref Optional crosswalk data frame. When `NULL`, the raw code is
#'   returned with `resolved = FALSE` (no fabrication).
#' @param region Optional region value to filter `ref` by (needs
#'   `region_var`).
#' @param code_var Crosswalk column holding the country code. Default
#'   `"code"`.
#' @param name_var Crosswalk column holding the country name. Default
#'   `"name"`.
#' @param iso3_var Crosswalk column holding the ISO3 code. Default `"iso3"`.
#' @param region_var Optional crosswalk column holding the region. Default
#'   `NULL`.
#' @return A [tibble][tibble::tibble] row-aligned to `epid` with columns
#'   `code`, `name`, `iso3`, `n_matches`, `ambiguous`, `resolved`.
#' @examples
#' crosswalk <- tibble::tibble(
#'   code = c("NIE", "AGO"),
#'   name = c("NIGERIA", "ANGOLA"),
#'   iso3 = c("NGA", "AGO")
#' )
#' resolve_epid_country(c("NIE-BOS-1", "AGO-LUA-1"), ref = crosswalk)
#' @export
resolve_epid_country <- function(
  epid,
  ref = NULL,
  region = NULL,
  code_var = "code",
  name_var = "name",
  iso3_var = "iso3",
  region_var = NULL
) {
  code <- epid_country_code(epid, n = 3, upper = TRUE)

  if (is.null(ref)) {
    return(tibble::tibble(
      code = code,
      name = NA_character_,
      iso3 = NA_character_,
      n_matches = 0L,
      ambiguous = FALSE,
      resolved = FALSE
    ))
  }

  required <- c(code_var, name_var)
  missing_cols <- setdiff(required, names(ref))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "{.arg ref} is missing column{?s}: {.val {missing_cols}}.",
      "i" = "Set {.arg code_var}/{.arg name_var} to match your crosswalk."
    ))
  }

  ref_use <- ref
  if (!is.null(region) && !is.null(region_var) && region_var %in% names(ref)) {
    ref_use <- ref_use[
      toupper(trimws(ref_use[[region_var]])) == toupper(region),
      ,
      drop = FALSE
    ]
  }

  has_iso3 <- iso3_var %in% names(ref_use)
  code_name_tbl <- tibble::tibble(
    key = toupper(trimws(ref_use[[code_var]])),
    name = as.character(ref_use[[name_var]]),
    iso3 = if (has_iso3) as.character(ref_use[[iso3_var]]) else NA_character_
  )
  if (has_iso3) {
    code_name_tbl <- dplyr::bind_rows(
      code_name_tbl,
      tibble::tibble(
        key = toupper(trimws(ref_use[[iso3_var]])),
        name = as.character(ref_use[[name_var]]),
        iso3 = as.character(ref_use[[iso3_var]])
      )
    )
  }
  summary_ref <- code_name_tbl |>
    dplyr::filter(!.epid_blank(key)) |>
    dplyr::distinct() |>
    dplyr::group_by(key) |>
    dplyr::summarise(
      n_matches = dplyr::n_distinct(name, na.rm = TRUE),
      name = dplyr::if_else(n_matches == 1L, dplyr::first(name), NA_character_),
      iso3 = dplyr::if_else(n_matches == 1L, dplyr::first(iso3), NA_character_),
      .groups = "drop"
    )

  resolved <- tibble::tibble(code = code) |>
    dplyr::left_join(summary_ref, by = dplyr::join_by(code == key))
  resolved |>
    dplyr::mutate(
      n_matches = dplyr::coalesce(n_matches, 0L),
      ambiguous = n_matches > 1L,
      resolved = n_matches == 1L
    )
}

# ---------------------------------------------------------------------
# Imputation cascade (capability B)
# ---------------------------------------------------------------------

#' Pick the single defensible candidate, else NA
#'
#' Returns the unique candidate value; on a tie, restricts to candidates whose
#' parent matches the row's known parent and accepts only if that is unique.
#' Never guesses.
#'
#' @param values Candidate values.
#' @param parents Parent values aligned to `values`.
#' @param this_parent The row's own (normalised) parent value.
#' @return A single character value (`NA` on ambiguity).
#' @keywords internal
#' @noRd
.epid_resolve_candidates <- function(values, parents, this_parent) {
  present <- values[!is.na(values)]
  distinct_values <- unique(present)
  if (length(distinct_values) == 1L) {
    return(distinct_values)
  }
  if (length(distinct_values) == 0L) {
    return(NA_character_)
  }
  if (!is.na(this_parent) && nzchar(this_parent)) {
    matched <- !is.na(values) & !is.na(parents) & parents == this_parent
    parent_values <- unique(values[matched])
    if (length(parent_values) == 1L) {
      return(parent_values)
    }
  }
  NA_character_
}

#' Prefix-match candidates for the still-missing rows
#'
#' Joins missing rows to known rows sharing the geographic prefix within
#' `year_window`, then resolves each via `.epid_resolve_candidates()`. Fully
#' vectorised (join + grouped summarise).
#'
#' @param data Working data frame (carries `.epid_prefix`).
#' @param value_col Column being filled.
#' @param parent_col Parent admin column for tie-breaking, or `NULL`.
#' @param year_var Year column name.
#' @param year_window Allowed +/- year distance.
#' @param still_mask Logical mask of rows still missing.
#' @return A list with `value` and `ambiguous`, each length `nrow(data)`.
#' @keywords internal
#' @noRd
.epid_prefix_match <- function(
  data,
  value_col,
  parent_col,
  year_var,
  year_window,
  still_mask
) {
  n_row <- nrow(data)
  empty <- list(
    value = rep(NA_character_, n_row),
    ambiguous = rep(FALSE, n_row)
  )

  known_mask <- !.epid_blank(data[[value_col]]) & !is.na(data[[".epid_prefix"]])
  miss_mask <- still_mask & !is.na(data[[".epid_prefix"]])
  if (!any(known_mask) || !any(miss_mask)) {
    return(empty)
  }

  known_tbl <- tibble::tibble(
    k_prefix = data[[".epid_prefix"]][known_mask],
    k_year = data[[year_var]][known_mask],
    k_parent = if (is.null(parent_col)) {
      NA_character_
    } else {
      toupper(trimws(as.character(data[[parent_col]][known_mask])))
    },
    k_value = as.character(data[[value_col]][known_mask])
  )
  known_tbl <- dplyr::distinct(known_tbl)

  miss_tbl <- tibble::tibble(
    row_id = which(miss_mask),
    m_prefix = data[[".epid_prefix"]][miss_mask],
    m_year = data[[year_var]][miss_mask],
    m_parent = if (is.null(parent_col)) {
      NA_character_
    } else {
      toupper(trimws(as.character(data[[parent_col]][miss_mask])))
    }
  )

  joined <- dplyr::inner_join(
    miss_tbl,
    known_tbl,
    by = dplyr::join_by(m_prefix == k_prefix),
    relationship = "many-to-many"
  )
  joined <- joined[
    abs(joined$m_year - joined$k_year) <= year_window,
    ,
    drop = FALSE
  ]
  if (nrow(joined) == 0L) {
    return(empty)
  }

  resolved <- joined |>
    dplyr::group_by(row_id) |>
    dplyr::summarise(
      accepted = .epid_resolve_candidates(
        k_value,
        k_parent,
        dplyr::first(m_parent)
      ),
      n_distinct_value = dplyr::n_distinct(k_value, na.rm = TRUE),
      .groups = "drop"
    )

  value <- rep(NA_character_, n_row)
  ambiguous <- rep(FALSE, n_row)
  value[resolved$row_id] <- resolved$accepted
  ambiguous[resolved$row_id] <- is.na(resolved$accepted) &
    resolved$n_distinct_value > 1L
  list(value = value, ambiguous = ambiguous)
}

#' Look an external reference table up by EPID or prefix
#'
#' @param data Working data frame (carries `.epid_norm` and `.epid_prefix`).
#' @param reference External reference data frame, keyed on `epid` or
#'   `prefix`.
#' @param value_col Column to pull from `reference`.
#' @return Character vector, length `nrow(data)` (`NA` where unresolved or
#'   ambiguous).
#' @keywords internal
#' @noRd
.epid_reference_lookup <- function(data, reference, value_col) {
  n_row <- nrow(data)
  if (!value_col %in% names(reference)) {
    return(rep(NA_character_, n_row))
  }
  if ("epid" %in% names(reference)) {
    ref_key <- .epid_normalise(reference[["epid"]])
    data_key <- data[[".epid_norm"]]
  } else if ("prefix" %in% names(reference)) {
    ref_key <- toupper(trimws(as.character(reference[["prefix"]])))
    data_key <- data[[".epid_prefix"]]
  } else {
    return(rep(NA_character_, n_row))
  }

  lookup_tbl <- tibble::tibble(
    k = ref_key,
    v = as.character(reference[[value_col]])
  ) |>
    dplyr::filter(!.epid_blank(k), !.epid_blank(v)) |>
    dplyr::distinct() |>
    dplyr::group_by(k) |>
    dplyr::summarise(
      n = dplyr::n_distinct(v, na.rm = TRUE),
      v = dplyr::if_else(n == 1L, dplyr::first(v), NA_character_),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(v))

  lut <- stats::setNames(lookup_tbl$v, lookup_tbl$k)
  unname(lut[data_key])
}

#' Fill one target column through the imputation cascade
#'
#' Applies the requested strategies in order to a single column, stamping the
#' provenance of every fill. Never overwrites a present value; never fabricates
#' on ambiguity.
#'
#' @param data Working data frame.
#' @param value_col Column to fill.
#' @param parent_col Parent admin column (for prefix tie-break), or `NULL`.
#' @param is_admin0 Whether this is the top admin level.
#' @param is_name Whether this is a name column (vs a GUID).
#' @param strategies Ordered strategy vector.
#' @param reference External reference data frame, or `NULL`.
#' @param country_ref Country crosswalk, or `NULL`.
#' @param year_var Year column name.
#' @param prefix_length Prefix length.
#' @param year_window Allowed +/- year distance for prefix-matching.
#' @param canonicalise Whether to canonicalise filled name cells.
#' @return A list with `data`, `source` (provenance vector), and `counts`.
#' @keywords internal
#' @noRd
.epid_fill_target <- function(
  data,
  value_col,
  parent_col,
  is_admin0,
  is_name,
  strategies,
  reference,
  country_ref,
  year_var,
  prefix_length,
  year_window,
  canonicalise
) {
  data[[value_col]] <- as.character(data[[value_col]])
  blank_before <- .epid_blank(data[[value_col]])
  source <- dplyr::if_else(blank_before, NA_character_, "original")
  counts <- list(
    n_missing_before = sum(blank_before, na.rm = TRUE),
    self_ref = 0L,
    prefix_match = 0L,
    reference = 0L,
    country_prefix = 0L,
    n_ambiguous = 0L
  )

  for (strategy in strategies) {
    still <- .epid_blank(data[[value_col]])
    if (!any(still)) {
      break
    }

    if (identical(strategy, "self_ref")) {
      ref_tbl <- build_admin_ref(
        data,
        value_col,
        epid_var = ".epid_norm",
        year_var = year_var
      )
      lut <- stats::setNames(ref_tbl[[value_col]], ref_tbl[[".epid_norm"]])
      candidate <- unname(lut[data[[".epid_norm"]]])
      fill <- still & !is.na(candidate)
      data[[value_col]][fill] <- candidate[fill]
      source[fill] <- "self_ref"
      counts$self_ref <- sum(fill, na.rm = TRUE)
    } else if (identical(strategy, "prefix_match")) {
      matched <- .epid_prefix_match(
        data,
        value_col,
        parent_col,
        year_var,
        year_window,
        still
      )
      fill <- still & !is.na(matched$value)
      data[[value_col]][fill] <- matched$value[fill]
      source[fill] <- "prefix_match"
      counts$prefix_match <- sum(fill, na.rm = TRUE)
      counts$n_ambiguous <- counts$n_ambiguous +
        sum(still & matched$ambiguous & is.na(matched$value), na.rm = TRUE)
    } else if (
      identical(strategy, "reference") &&
        !is.null(reference) &&
        value_col %in% names(reference)
    ) {
      candidate <- .epid_reference_lookup(data, reference, value_col)
      fill <- still & !is.na(candidate)
      data[[value_col]][fill] <- candidate[fill]
      source[fill] <- "reference"
      counts$reference <- sum(fill, na.rm = TRUE)
    } else if (
      identical(strategy, "country_prefix") &&
        is_admin0 &&
        is_name &&
        !is.null(country_ref)
    ) {
      country <- resolve_epid_country(data[[".epid_norm"]], ref = country_ref)
      candidate <- country$name
      fill <- still & !is.na(candidate)
      data[[value_col]][fill] <- candidate[fill]
      source[fill] <- "country_prefix"
      counts$country_prefix <- sum(fill, na.rm = TRUE)
    }
  }

  if (isTRUE(canonicalise) && isTRUE(is_name)) {
    canon_fn <- tryCatch(
      utils::getFromNamespace("polis_fix_geo_names", "polished"),
      error = function(e) NULL
    )
    if (!is.null(canon_fn)) {
      fillable <- source %in%
        c("self_ref", "prefix_match", "reference", "country_prefix")
      if (any(fillable)) {
        fixed <- tryCatch(
          canon_fn(data[[value_col]][fillable]),
          error = function(e) NULL
        )
        if (!is.null(fixed) && length(fixed) == sum(fillable, na.rm = TRUE)) {
          data[[value_col]][fillable] <- as.character(fixed)
        }
      }
    }
  }

  source <- dplyr::if_else(is.na(source), "unresolved", source)
  counts$n_unresolved <- sum(source == "unresolved", na.rm = TRUE)
  list(data = data, source = source, counts = counts)
}

#' Build the per-level target specification
#'
#' @param admin0_var Admin0 name column, or `NULL` to skip.
#' @param admin1_var Admin1 name column, or `NULL` to skip.
#' @param admin2_var Admin2 name column, or `NULL` to skip.
#' @param guid_vars Named character vector of GUID columns, or `NULL`.
#' @return A list of target specification lists, one per column to fill.
#' @keywords internal
#' @noRd
.epid_targets <- function(admin0_var, admin1_var, admin2_var, guid_vars) {
  targets <- list()
  if (!is.null(admin0_var)) {
    targets <- c(
      targets,
      list(list(
        col = admin0_var,
        level = "admin0",
        parent = NULL,
        is_admin0 = TRUE,
        is_name = TRUE
      ))
    )
  }
  if (!is.null(admin1_var)) {
    targets <- c(
      targets,
      list(list(
        col = admin1_var,
        level = "admin1",
        parent = admin0_var,
        is_admin0 = FALSE,
        is_name = TRUE
      ))
    )
  }
  if (!is.null(admin2_var)) {
    targets <- c(
      targets,
      list(list(
        col = admin2_var,
        level = "admin2",
        parent = admin1_var,
        is_admin0 = FALSE,
        is_name = TRUE
      ))
    )
  }
  parent_of <- list(adm0 = NULL, adm1 = admin0_var, adm2 = admin1_var)
  for (key in names(guid_vars)) {
    parent <- parent_of[[key]]
    targets <- c(
      targets,
      list(list(
        col = unname(guid_vars[[key]]),
        level = key,
        parent = parent,
        is_admin0 = identical(key, "adm0"),
        is_name = FALSE
      ))
    )
  }
  targets
}

#' Recover administrative geography from the EPID
#'
#' Fills missing administrative names (and optionally GUIDs) using the EPID as
#' a recovery key, through an ordered, provenance-stamped cascade. Only blank
#' cells are filled; present values are never overwritten and nothing is
#' fabricated on ambiguity.
#'
#' The cascade, per admin level (Admin0, then Admin1, then Admin2):
#' \describe{
#'   \item{original}{Value already present — kept.}
#'   \item{self_ref}{Most-recent non-blank value for the exact same EPID
#'     elsewhere in `data`.}
#'   \item{prefix_match}{Unique value among records sharing the geographic
#'     prefix within `year_window` years; a parent-level tie-break is applied
#'     before declaring ambiguity.}
#'   \item{reference}{An external `reference` table keyed on EPID or prefix.}
#'   \item{country_prefix}{Admin0 only — the country code resolved via
#'     `country_ref`.}
#' }
#' Any cell still blank after the cascade is labelled `unresolved`.
#'
#' @param data Non-empty data frame carrying an EPID column.
#' @param epid_var EPID column name. Default `"epid"`.
#' @param year_var Year/onset column name. Default `"yronset"`.
#' @param admin0_var,admin1_var,admin2_var Admin name columns; `NULL` skips a
#'   level. Defaults `"place.admin.0"`/`.1`/`.2`.
#' @param guid_vars Named character vector mapping levels (`adm0`/`adm1`/
#'   `adm2`) to GUID columns to fill. Default `c(adm2 = "Admin2GUID")`; set
#'   `NULL` to fill no GUIDs.
#' @param reference Optional external table (keyed on `epid` or `prefix`) of
#'   admin names/GUIDs, for data with no names of its own. Default `NULL`.
#' @param country_ref Optional country code -> name/ISO3 crosswalk. Default
#'   `NULL`.
#' @param strategies Ordered subset of `c("self_ref", "prefix_match",
#'   "reference", "country_prefix")`. Default uses all four.
#' @param prefix_length Prefix length for prefix-matching. Default `11`.
#' @param year_window Allowed +/- year distance when prefix-matching. Default
#'   `0`.
#' @param sep EPID segment delimiter. Default `"-"`.
#' @param canonicalise Whether to canonicalise filled name cells when a
#'   canonicaliser is available. Default `TRUE`.
#' @param verbose Whether to print a cli summary. Default `TRUE`.
#' @return A named list:
#' \describe{
#'   \item{data}{`data` with filled admin/GUID columns plus a
#'     `<col>_source` provenance factor per filled column.}
#'   \item{ref}{The self-reference lookups built (for audit).}
#'   \item{qa}{A tibble of per-level fill counts and `pct_resolved`.}
#'   \item{meta}{The settings used.}
#' }
#' @examples
#' cases <- tibble::tibble(
#'   epid = c("NIE-BOS-AAA-24-001", "NIE-BOS-AAA-24-002", "AGO-LUA-BBB-24-001"),
#'   yronset = c(2024, 2024, 2024),
#'   place.admin.0 = c("NIGERIA", NA, "ANGOLA"),
#'   place.admin.1 = c("BORNO", NA, "LUANDA"),
#'   place.admin.2 = c("BOSSO", NA, "LUANDA"),
#'   Admin2GUID = c("g-bosso", NA, "g-luanda")
#' )
#' result <- impute_geo_from_epid(cases, verbose = FALSE)
#' result$qa
#' @export
impute_geo_from_epid <- function(
  data,
  epid_var = "epid",
  year_var = "yronset",
  admin0_var = "place.admin.0",
  admin1_var = "place.admin.1",
  admin2_var = "place.admin.2",
  guid_vars = c(adm2 = "Admin2GUID"),
  reference = NULL,
  country_ref = NULL,
  strategies = c("self_ref", "prefix_match", "reference", "country_prefix"),
  prefix_length = 11,
  year_window = 0,
  sep = "-",
  canonicalise = TRUE,
  verbose = TRUE
) {
  known_strategies <- c(
    "self_ref",
    "prefix_match",
    "reference",
    "country_prefix"
  )

  # validate ----------------------------------------------------------
  if (!is.data.frame(data) || nrow(data) == 0L) {
    cli::cli_abort("{.arg data} must be a non-empty data frame.")
  }
  if (length(strategies) == 0L) {
    cli::cli_abort("{.arg strategies} must name at least one strategy.")
  }
  bad_strategies <- setdiff(strategies, known_strategies)
  if (length(bad_strategies) > 0L) {
    cli::cli_abort(c(
      "Unknown {.arg strategies} value{?s}: {.val {bad_strategies}}.",
      "i" = "Valid strategies: {.val {known_strategies}}."
    ))
  }
  if (!.is_count(prefix_length)) {
    cli::cli_abort("{.arg prefix_length} must be a positive whole number.")
  }
  if (!.is_nonneg_int(year_window)) {
    cli::cli_abort("{.arg year_window} must be a non-negative whole number.")
  }
  if (!is.character(sep) || length(sep) != 1L || !nzchar(sep)) {
    cli::cli_abort("{.arg sep} must be a single non-empty string.")
  }
  if (!is.null(guid_vars)) {
    bad_levels <- setdiff(names(guid_vars), c("adm0", "adm1", "adm2"))
    if (is.null(names(guid_vars)) || length(bad_levels) > 0L) {
      cli::cli_abort(c(
        "{.arg guid_vars} must be named with {.val adm0}/{.val adm1}/{.val adm2}.",
        "x" = if (length(bad_levels) > 0L)
          "Bad name{?s}: {.val {bad_levels}}." else NULL
      ))
    }
  }

  targets <- .epid_targets(admin0_var, admin1_var, admin2_var, guid_vars)
  if (length(targets) == 0L) {
    cli::cli_abort("No target columns: set at least one admin or GUID column.")
  }

  needs_year <- any(c("self_ref", "prefix_match") %in% strategies)
  target_cols <- vapply(targets, function(target) target$col, character(1))
  needed <- c(epid_var, target_cols)
  if (needs_year) {
    needed <- c(needed, year_var)
  }
  missing_cols <- setdiff(unique(needed), names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(c(
      "Missing required column{?s}: {.val {missing_cols}}.",
      "i" = "Set the matching {.arg *_var} argument(s) or add the column(s)."
    ))
  }

  if (!is.null(reference)) {
    if (!is.data.frame(reference)) {
      cli::cli_abort("{.arg reference} must be a data frame or {.code NULL}.")
    }
    if (!any(c("epid", "prefix") %in% names(reference))) {
      cli::cli_abort(c(
        "{.arg reference} needs an {.val epid} or {.val prefix} key column.",
        "i" = "Plus a value column named like the admin/GUID column to fill."
      ))
    }
  }
  if (!is.null(country_ref) && "country_prefix" %in% strategies) {
    if (!is.data.frame(country_ref)) {
      cli::cli_abort("{.arg country_ref} must be a data frame or {.code NULL}.")
    }
    missing_country <- setdiff(c("code", "name"), names(country_ref))
    if (length(missing_country) > 0L) {
      cli::cli_abort(
        "{.arg country_ref} is missing column{?s}: {.val {missing_country}}."
      )
    }
  }

  # normalise ---------------------------------------------------------
  work <- data
  work[[".epid_norm"]] <- .epid_normalise(
    epid_strip_contact(work[[epid_var]])$epid_base
  )
  work[[".epid_prefix"]] <- epid_prefix(work[[".epid_norm"]], prefix_length)

  # run cascade per target --------------------------------------------
  qa_rows <- list()
  for (target in targets) {
    filled <- .epid_fill_target(
      work,
      value_col = target$col,
      parent_col = target$parent,
      is_admin0 = target$is_admin0,
      is_name = target$is_name,
      strategies = strategies,
      reference = reference,
      country_ref = country_ref,
      year_var = year_var,
      prefix_length = prefix_length,
      year_window = year_window,
      canonicalise = canonicalise
    )
    work <- filled$data
    work[[paste0(target$col, "_source")]] <- factor(
      filled$source,
      levels = c(
        "original",
        "self_ref",
        "prefix_match",
        "reference",
        "country_prefix",
        "unresolved"
      )
    )
    counts <- filled$counts
    qa_rows[[target$col]] <- tibble::tibble(
      level = target$level,
      column = target$col,
      n_missing_before = counts$n_missing_before,
      n_filled_self_ref = counts$self_ref,
      n_filled_prefix_match = counts$prefix_match,
      n_filled_reference = counts$reference,
      n_filled_country_prefix = counts$country_prefix,
      n_ambiguous = counts$n_ambiguous,
      n_unresolved = counts$n_unresolved,
      pct_resolved = dplyr::if_else(
        counts$n_missing_before == 0L,
        1,
        (counts$n_missing_before - counts$n_unresolved) /
          counts$n_missing_before
      )
    )
  }
  qa <- dplyr::bind_rows(qa_rows)

  # audit references --------------------------------------------------
  ref_audit <- list()
  if (needs_year) {
    if (!is.null(admin1_var)) {
      ref_audit$admin1 <- build_admin_ref(
        work,
        admin1_var,
        epid_var = ".epid_norm",
        year_var = year_var
      )
    }
    if (!is.null(admin2_var)) {
      ref_audit$admin2 <- build_admin_ref(
        work,
        admin2_var,
        epid_var = ".epid_norm",
        year_var = year_var
      )
    }
  }

  # ambiguous country codes (for the warning) -------------------------
  ambiguous_codes <- character(0)
  if (!is.null(country_ref) && "country_prefix" %in% strategies) {
    country <- resolve_epid_country(work[[".epid_norm"]], ref = country_ref)
    ambiguous_codes <- unique(country$code[
      country$ambiguous & !is.na(country$code)
    ])
  }

  out_data <- work |>
    dplyr::select(-dplyr::any_of(c(".epid_norm", ".epid_prefix")))

  meta <- list(
    strategies = strategies,
    prefix_length = as.integer(prefix_length),
    year_window = as.integer(year_window),
    reference_used = !is.null(reference),
    country_ref_used = !is.null(country_ref)
  )

  if (isTRUE(verbose)) {
    .epid_report(qa, strategies, prefix_length, year_window, ambiguous_codes)
  }

  list(data = out_data, ref = ref_audit, qa = qa, meta = meta)
}

#' Print the cli summary for an imputation run
#'
#' @keywords internal
#' @noRd
.epid_report <- function(
  qa,
  strategies,
  prefix_length,
  year_window,
  ambiguous_codes
) {
  cli::cli_rule(left = "EPID geo imputation")
  cli::cli_text("Strategies (in order): {.val {strategies}}.")
  cli::cli_text(
    "Prefix length {.val {prefix_length}} · year window {.val {year_window}}."
  )
  cli::cli_h3("Results by level")
  for (i in seq_len(nrow(qa))) {
    row <- qa[i, ]
    cli::cli_bullets(c(
      "*" = paste0(
        "{.field {row$column}} ({row$level}): {row$n_missing_before} ",
        "missing → self_ref {row$n_filled_self_ref}, ",
        "prefix_match {row$n_filled_prefix_match}, ",
        "reference {row$n_filled_reference}, ",
        "country_prefix {row$n_filled_country_prefix}; ",
        "ambiguous {row$n_ambiguous}, unresolved {row$n_unresolved} ",
        "({round(row$pct_resolved * 100)}% resolved)"
      )
    ))
  }
  if (length(ambiguous_codes) > 0L) {
    cli::cli_alert_warning(
      "Country code{?s} mapping to >1 name: {.val {ambiguous_codes}}."
    )
  }
  total_unresolved <- sum(qa$n_unresolved, na.rm = TRUE)
  if (total_unresolved > 0L) {
    cli::cli_alert_warning(
      "{total_unresolved} cell{?s} left unresolved — not fabricated."
    )
  } else {
    cli::cli_alert_success("All targeted missing cells resolved.")
  }
  invisible(NULL)
}

utils::globalVariables(c(
  ".data",
  ".epid_norm",
  ".epid_prefix",
  "prefix",
  "year",
  "value",
  "n_candidates",
  "key",
  "name",
  "iso3",
  "n_matches",
  "row_id",
  "k_value",
  "k_parent",
  "m_parent",
  "accepted",
  "n_distinct_value",
  "k",
  "v",
  "n"
))
