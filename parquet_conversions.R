# Convert Data/league1/player_stats.csv into one Parquet file per season:
#   Data/league1/player_stats/S2017.parquet, S2018.parquet, ...
# Each file keeps its own `season` column.
# Run from the project root (where the .Rproj file is).
# The original CSV is NOT modified or deleted.

library(readr)
library(dplyr)
library(arrow)

csv_path <- "Data/SHL/Playoffs/index_schedule.csv"
out_dir  <- "Data/SHL/playoffs_index_schedule"

# 1. Read the CSV ------------------------------------------------------------
# guess_max = Inf makes readr look at EVERY row before deciding each column's
# type. By default it only looks at the first 1,000 rows. A column that was
# added halfway through the seasons is all NA in those rows, so readr would
# guess "logical" and turn every later value into NA (with only a warning).
player_stats <- read_csv(csv_path, guess_max = Inf, show_col_types = FALSE)

# Stop if anything failed to parse
stopifnot(nrow(problems(player_stats)) == 0)

# Drop readr's bookkeeping attributes so the later comparison is like-for-like
attr(player_stats, "spec") <- NULL
attr(player_stats, "problems") <- NULL
player_stats <- as_tibble(player_stats)

# 2. Prepare the season column -----------------------------------------------
# Every row needs a season, or it would end up in a file named "SNA.parquet"
stopifnot(!any(is.na(player_stats$season)))

# Store numeric seasons as whole numbers, so files are "S2017", not "S2017.0"
if (is.numeric(player_stats$season)) {
  player_stats <- player_stats |> mutate(season = as.integer(season))
}

# 3. Start from an empty output folder ---------------------------------------
# This removes the earlier season=XXXX subfolders. Everything in this folder
# is rebuilt from the CSV below, so nothing is lost as long as the CSV exists
# (it was read successfully in step 1).
unlink(out_dir, recursive = TRUE)
dir.create(out_dir)

# 4. Write one file per season ------------------------------------------------
# Re-running is safe: each file is overwritten, never duplicated.
for (s in sort(unique(player_stats$season))) {
  player_stats |>
    filter(season == s) |>
    write_parquet(file.path(out_dir, paste0("S", s, ".parquet")))
}

# 5. Verify the round trip ---------------------------------------------------
parquet_version <- open_dataset(out_dir, unify_schemas = TRUE) |>
  collect() |>
  select(all_of(names(player_stats)))   # match the CSV's column order

# Row order isn't guaranteed when reading a folder, so sort both before comparing
csv_sorted     <- player_stats    |> arrange(across(everything()))
parquet_sorted <- parquet_version |> arrange(across(everything()))

result <- waldo::compare(csv_sorted, parquet_sorted, x_arg = "csv", y_arg = "parquet")
print(result)

if (length(result) == 0) {
  message("Identical: ", nrow(player_stats), " rows across ",
          n_distinct(player_stats$season), " seasons.")
} else {
  warning("Differences found; keep the CSV and check the output above.")
}

# 6. Size check --------------------------------------------------------------
# GitHub warns above 50 MB per file and rejects above 100 MB
files <- list.files(out_dir, full.names = TRUE)
message("Files written: ", length(files),
        " | total ", round(sum(file.size(files)) / 1e6, 1), " MB",
        " | largest ", round(max(file.size(files)) / 1e6, 1), " MB",
        " (CSV was ", round(file.size(csv_path) / 1e6, 1), " MB)")
