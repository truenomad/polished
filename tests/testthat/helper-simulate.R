# the class of each column, for comparing a table against its expected shape
column_classes <- function(df) {
  vapply(df, \(x) class(x)[[1]], character(1))
}

# TRUE when every pair of dates in the given order runs forwards where both
# are recorded
dates_in_order <- function(df, columns) {
  pairs <- tidyr::expand_grid(
    i = seq_along(columns), j = seq_along(columns)
  ) |>
    dplyr::filter(i < j)

  mapply(
    \(i, j) {
      a <- df[[columns[[i]]]]
      b <- df[[columns[[j]]]]
      all(is.na(a) | is.na(b) | b >= a)
    },
    pairs$i,
    pairs$j
  ) |>
    all()
}

# a short list of real countries, as names and as ISO codes
real_country_names <- c(
  "AFGHANISTAN", "PAKISTAN", "NIGERIA", "INDIA", "CHAD", "NIGER", "SOMALIA",
  "ETHIOPIA", "KENYA", "YEMEN", "SYRIA", "IRAQ", "IRAN", "EGYPT", "SUDAN",
  "MALI", "ANGOLA", "CONGO", "UGANDA", "MOZAMBIQUE", "BRAZIL", "PERU",
  "OMAN", "JORDAN", "LEBANON", "LIBYA"
)

real_iso3 <- c(
  "AFG", "PAK", "NGA", "IND", "TCD", "NER", "SOM", "ETH", "KEN", "YEM",
  "SYR", "IRQ", "IRN", "EGY", "SDN", "MLI", "AGO", "COD", "COG", "UGA"
)

# TRUE when no name contains a real country name
no_real_names <- function(names) {
  pattern <- paste0("\\b(", paste(real_country_names, collapse = "|"), ")\\b")
  !any(grepl(pattern, toupper(names)))
}
