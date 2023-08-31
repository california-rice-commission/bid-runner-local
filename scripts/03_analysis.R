# Summary stats



# Load directories and functions
code_dir <-"V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/code/water_tracker/code"
source(file.path(code_dir, "definitions_local.R"))

source(file.path(code_dir, "functions/00_shared_functions.R"))

# Packages
library(rgdal)
library(dplyr)
library(tidyr)

# Directories
output_dir <- imp_stat_dir
processed_files <- c()

# Parameters
overwrite <- TRUE

# Read metadata
shp_fn <- "B4B_22_Fall_fields_all"
shp_file <- file.path(fld_dir, paste0(shp_fn, ".shp"))
fld_shp <- readOGR(fld_dir, shp_fn)
md_df <- fld_shp@data

# Fix incorrect BidID
md_df$BidID[14] <- "B4B-22-F-001-B"
md_df$BidFieldID[14] <- "B4B-22-F-001-B-South-4"

# Fix incorrect start date
# B4B-22-F-011	307.5	125	9/16/2022 to 9/19
md_df$StartDate[md_df$BidID == "B4B-22-F-011" & md_df$FieldID == "Christensen 300"] <- "2022-09-19"

# Fix incorrect end dats
# B4B-22-F-004 to 8-8, 8-22, and 9-5
md_df$EndDate[md_df$BidID == "B4B-22-F-004-A"] <- "2022-08-08"
md_df$EndDate[md_df$BidID == "B4B-22-F-004-B"] <- "2022-08-22"
md_df$EndDate[md_df$BidID == "B4B-22-F-004-C"] <- "2022-09-05"

# Create join column
md_df <- md_df %>%
  mutate(JoinID = ifelse(substr(BidID, nchar(BidID), nchar(BidID)) %in% c("B", "C"),
                         paste0(substr(BidID, 1, nchar(BidID) - 1), "A"),
                         BidID),
         JoinID = paste0(JoinID, "-", clean_string_remove_underscores(FieldID)),
         BidFieldID = gsub(" ", "-", BidFieldID))

# Read bird data
message_ts("Reading and combining data...")
stat_files <- list.files(imp_stat_dir, pattern = ".rds$", full.names = TRUE)
long_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))

# Join to metadata
joined_df <- left_join(long_df, md_df, by = c("FloodingArea" = "JoinID")) #add as arguments

joined_df[joined_df$BidID == "B4B-22-F-001-A",]

# Remove -A- from flooding area names
joined_df <- mutate(joined_df, FloodingArea = gsub("-A-", "-", FloodingArea))

# Overlap
joined_df <- joined_df %>%
  mutate(PredictionDateStart = as.Date(paste0("2022", "-", match(PredictionMonth, month.abb), "-1")), #need to fix year somehow
         PredictionDateEnd = as.Date(paste0("2022", "-", match(PredictionMonth, month.abb), "-", ifelse(PredictionMonth == "Feb", "28",
                                                                                                        ifelse(PredictionMonth %in% c("Mar", "Jun", "Sep", "Nov"), "30", "31")))),
         FloodDateStart = as.Date(StartDate),
         FloodDateEnd = as.Date(EndDate)) %>%
  mutate(OverlapMin = pmax(as.Date(FloodDateStart), PredictionDateStart), OverlapMax = pmin(as.Date(FloodDateEnd), PredictionDateEnd),
         DaysOverlap = pmax(0, difftime(OverlapMax, OverlapMin, unit = "days")))

# Export
long_file <- file.path(output_dir, "00_prediction_summary_long.csv")
if (file.exists(long_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(joined_df, long_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, long_file)
}

# Calculate ensembles
message_ts("Calculating ensembles...")
ens_df <- joined_df %>%
  rename(Bid = BidID, Field = FieldID) %>%
  group_by(BidFieldID, Bid, Field, FloodingArea, FieldName, CoverType, AreaAcres, PricePerAc,
           PredictionMonth, FloodDateStart, FloodDateEnd, DaysOverlap, Species, Model) %>%
  mutate(EnsembleWeights = ifelse(ModelLocation == "S", 1, 2)) %>%
  summarize(SuitabilityMean = weighted.mean(PredictionMean, EnsembleWeights),
            SuitabilitySum = sum(PredictionSum * EnsembleWeights / 3),
            LandscapeMean = weighted.mean(PredictionMean_Landscape, EnsembleWeights),
            LandscapeSum = sum(PredictionSum_Landscape * EnsembleWeights / 3))

# Export
ens_file <- file.path(output_dir, "01_prediction_summary_ensembled.csv")
if (file.exists(ens_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(ens_df, ens_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, ens_file)
}

# -------------------------------
# Summarize by flooding area

message_ts("Summarizing by flooding area...")
fa_df <- ens_df %>%
  ungroup() %>%
  group_by(Bid, FloodingArea, PredictionMonth, FloodDateStart, FloodDateEnd, DaysOverlap, Species, Model) %>%
  summarise(FloodingAreaAcres = sum(AreaAcres), 
            PricePerAcre = weighted.mean(as.numeric(PricePerAc), AreaAcres),
            SuitabilityMean = weighted.mean(SuitabilityMean, AreaAcres),
            SuitabilitySum = sum(SuitabilitySum),
            LandscapeMean = weighted.mean(LandscapeMean, AreaAcres))

# Export
fa_file <- file.path(output_dir, "02_prediction_summary_flooding_area.csv")
if (file.exists(fa_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(fa_df, fa_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, fa_file)
}

# Combine by month
fa_cmb_df <- fa_df %>%
  filter(DaysOverlap > 0) %>%
  group_by(Bid, FloodingArea, FloodingAreaAcres, PricePerAcre, FloodDateStart, FloodDateEnd, Species) %>%
  summarise(PredictionYear = "Combined", PredictionMonth = "Combined", BidLength = sum(DaysOverlap),
            SuitMean = weighted.mean(SuitabilityMean, DaysOverlap),
            SuitSum = sum(SuitabilityMean * DaysOverlap * FloodingAreaAcres), LandscapeMean = weighted.mean(LandscapeMean, DaysOverlap))

# Export
fa_cmb_file <- file.path(output_dir, "03_prediction_summary_flooding_area_across_months.csv")
if (file.exists(fa_cmb_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(fa_cmb_df, fa_cmb_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, fa_cmb_file)
}

# Calculate totals across species
fa_cmb_wide_df <- fa_cmb_df %>%
  select(-PredictionYear, - PredictionMonth, - LandscapeMean) %>%
  pivot_wider(names_from = Species, values_from = c(SuitMean, SuitSum)) %>%
  mutate(SuitMean_Total = mean(c(SuitMean_AMAV, SuitMean_BNST, SuitMean_DOWI, SuitMean_DUNL)),
         SuitSum_Total = sum(c(SuitSum_AMAV, SuitSum_BNST, SuitSum_DOWI, SuitSum_DUNL)))

# Export
fa_cmb_wide_file <- file.path(output_dir, "04_prediction_summary_flooding_area_totals_wide.csv")
if (file.exists(fa_cmb_wide_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(fa_cmb_wide_df, fa_cmb_wide_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, fa_cmb_wide_file)
}


# -------------------------------
# Summarize by bid


message_ts("Summarizing by bid...")
bid_df <- ens_df %>%
  ungroup() %>%
  group_by(Bid, PredictionMonth, FloodDateStart, FloodDateEnd, DaysOverlap, Species, Model) %>%
  summarise(FloodingAreaAcres = sum(AreaAcres), 
            PricePerAcre = weighted.mean(as.numeric(PricePerAc), AreaAcres),
            SuitabilityMean = weighted.mean(SuitabilityMean, AreaAcres),
            SuitabilitySum = sum(SuitabilitySum),
            LandscapeMean = weighted.mean(LandscapeMean, AreaAcres))

# Export
bid_file <- file.path(output_dir, "05_prediction_summary_bid.csv")
if (file.exists(bid_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(bid_df, bid_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, bid_file)
}

# Combine by month
bid_cmb_df <- bid_df %>%
  filter(DaysOverlap > 0) %>%
  group_by(Bid, FloodingAreaAcres, PricePerAcre, FloodDateStart, FloodDateEnd, Species) %>%
  summarise(PredictionYear = "Combined", PredictionMonth = "Combined", BidLength = sum(DaysOverlap),
            SuitMean = weighted.mean(SuitabilityMean, DaysOverlap),
            SuitSum = sum(SuitabilityMean * DaysOverlap * FloodingAreaAcres), LandscapeMean = weighted.mean(LandscapeMean, DaysOverlap))

# Export
bid_cmb_file <- file.path(output_dir, "06_prediction_summary_bid_across_months.csv")
if (file.exists(bid_cmb_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(bid_cmb_df, bid_cmb_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, bid_cmb_file)
}

# Calculate totals across species
bid_cmb_wide_df <- bid_cmb_df %>%
  select(-PredictionYear, - PredictionMonth, - LandscapeMean) %>%
  pivot_wider(names_from = Species, values_from = c(SuitMean, SuitSum)) %>%
  mutate(SuitMean_Total = mean(c(SuitMean_AMAV, SuitMean_BNST, SuitMean_DOWI, SuitMean_DUNL)),
         SuitSum_Total = sum(c(SuitSum_AMAV, SuitSum_BNST, SuitSum_DOWI, SuitSum_DUNL)))

# Export
bid_cmb_wide_file <- file.path(output_dir, "07_prediction_summary_bid_totals_wide.csv")
if (file.exists(bid_cmb_wide_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")
} else {
  write.csv(bid_cmb_wide_df, bid_cmb_wide_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, bid_cmb_wide_file)
}


