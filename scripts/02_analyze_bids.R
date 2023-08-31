# Run auction-level part of the processing

# Load definitions, check parameters, source code, and run setup
setup_dir <- file.path(getwd(), "scripts") #change if needed
source(file.path(setup_dir, "01_setup.R"))

# Load packages (for multi-core processing and reporting)
library(future)
library(doFuture)
library(progressr)

# Split shapefile
floodarea_files <- split_flooding_area(axn_file_clean,
                                       field_column_name = "FieldID",  #created in 01_setup.R
                                       guide_raster = ref_file,
                                       output_dir = spl_dir,           #defined in definitions.R
                                       do_rasterize = FALSE,
                                       overwrite = overwrite_global)

floodarea_files <- list.files(spl_dir, pattern = ".shp$", full.names = TRUE)
fa_rst_files <- list.files(spl_dir, pattern = ".tif$", full.names = TRUE)

# Water files

# Can subset files using the scenarios parameter, which is applied as a regex filter
scenarios_filter <- "imposed"

cores_to_use <- 6
n_sessions <- min(length(flood_areas), cores_to_use, availableCores() - 1)

handlers(global = TRUE)
handlers(handler_progress(
  format = "[:bar] :percent (:current/:total) - Elapsed: :elapsed, ETA: :eta",
  clear = FALSE))

evaluate_auction <- function(flood_areas, overwrite = FALSE, 
                             retry_times = 0, retry_counter = NULL,
                             verbose_level = 1, p = NULL, global_prog_mult = 1) {
  
  # Check/set retry counter
  if (retry_times > 2) stop(add_ts("Only supports two levels of retry upon error."))
  if (is.null(retry_counter)) { 
    retry_counter <- 0
  } else {
    retry_counter <- retry_counter + 1
  }
  
  # Start progress reporter
  if (is.null(p)) p <- progressor(steps = length(flood_areas), auto_finish = FALSE)
  
  # Set reporting level
  if (verbose_level > 1) {
    prg_msg <- TRUE
    fxn_msg <- TRUE
  } else if (verbose_level == 1) {
    prg_msg <- TRUE
    fxn_msg <- FALSE
  } else {
    prg_msg <- FALSE
    fxn_msg <- FALSE
  }
  
  foreach(fa = flood_areas) %dofuture% {
    
    # Labels for messages
    fxn <- "setup"
    lbl <- paste0(fa, " ")
    prg_mult <- 1
    if (prg_msg) p(add_ts(lbl, "started"), class = "sticky", amount = 0)
    
    # Streamlined error catching
    tryCatch({
      
      # Get flood area files for specified fa
      fa_files <- list.files(spl_dir, pattern = ".shp$", full.names = TRUE)
      fa_files <- fa_files[grepl(paste0("((", paste0(fa, collapse = ")|("), "))"), fa_files)]
      
      # Rasterize and buffer flood areas
      fxn <- "rasterize"
      fa_rst_files <- rasterize_flooding_area(fa_files,
                                              guide_raster = ref_file,
                                              output_dir = spl_dir,         #defined in definitions.R
                                              buffer_dist = 10000,
                                              overwrite = overwrite_global,
                                              verbose = TRUE)
      
      if (prg_msg) p(add_ts("Rasterized files (", length(fa_rst_files), "): ", 
                            paste0(basename(fa_rst_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      
      # Impose flooding
      fxn <- "impose-water"
      water_imp_files <- impose_flooding(lt_wtr_files,
                                         fa_rst_files,
                                         output_dir = imp_wtr_dir,
                                         mask = TRUE, #significantly speeds up processing in later steps
                                         overwrite = overwrite,
                                         verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Imposed files (", length(water_imp_files), "): ", 
                            paste0(basename(water_imp_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      
      fxn <- "overlay-water-landcover"
      wxl_files <- overlay_water_landcover(water_imp_files, 
                                           lc_files,
                                           output_dir = imp_wxl_dir,
                                           overwrite = overwrite,
                                           verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("WxL files (", length(wxl_files), "): ", 
                            paste0(basename(wxl_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      #p(amount = 0.4 * prg_mult * global_prog_mult)
      
      fxn <- "mean-neighborhood-water"
      fcl_imp_files <- mean_neighborhood_water(wxl_files, #previously-created water x landcover files
                                               distances = c(250, 5000), #250m and 5km
                                               output_dir = imp_fcl_dir,
                                               trim_extent = TRUE,  #only set for TRUE with splits
                                               overwrite = overwrite,
                                               verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Focal files (", length(fcl_imp_files), "): ", 
                            paste0(basename(fcl_imp_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      #p(amount = 0.5 * prg_mult * global_prog_mult)
      
      fxn <- "predict-bird-rasters"
      prd_files <- predict_bird_rasters(water_files_realtime = fcl_imp_files,
                                        water_files_longterm = fcl_avg_files,
                                        scenarios = scenarios_filter,
                                        water_months = mths,
                                        model_files = shorebird_model_files_reallong,
                                        model_names = shorebird_model_names_reallong,
                                        static_cov_files = bird_model_cov_files,
                                        static_cov_names = bird_model_cov_names,
                                        monthly_cov_files = tmax_files,
                                        monthly_cov_months = tmax_mths,
                                        monthly_cov_names = tmax_names,
                                        output_dir = imp_prd_dir,
                                        overwrite = overwrite,
                                        verbose = fxn_msg) #1)
      
      if (prg_msg) p(add_ts("Pred files (", length(prd_files), "): ", paste0(basename(prd_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      
      fxn <- "extract-predictions"
      stat_files <- extract_predictions(prd_files,
                                        fa_files,
                                        field_column = "BidFieldID",
                                        area_column = "AreaAcres",
                                        output_dir = imp_stat_dir,
                                        overwrite = overwrite,
                                        verbose = fxn_msg)
      
      if (prg_msg) p(add_ts("Stat files (", length(stat_files), "): ", paste0(basename(stat_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      
    }, error = function(e) {
      if (retry_counter == 0) {
        lbl <- ""
      } else if (retry_counter == 1) {
        lbl <- paste0("FA ", fa, " - ")
      } else {
        lbl <- paste0("FA ", fa, ", retry ", retry_counter, " - ")
      }
      
      p(add_ts("ERROR - ", fxn, " - ", lbl, e), class = "sticky", amount = 0)
      
      saveRDS(e, file.path(log_dir, paste0("error_function-", fxn, "_FA-", paste0(unlist(fa), collapse = "-"),
                                           "_retry-", retry_counter, "-of-", retry_times,
                                           "_date-", format(Sys.time(), format = "%Y-%m-%d"), ".rds")))
      
      # If retry request, loop across individual flood areas
      if (retry_times >= 1 & retry_counter == 0) {
        
        p(add_ts("Retrying files in this set individually to find problematic file(s)..."),
          class = "sticky", amount = 0)#.3 * prg_mult * -1)
        evaluate_auction(unlist(fa), overwrite = overwrite,
                         retry_times = min(retry_times, 2), retry_counter = retry_counter,
                         p = p, global_prog_mult = 0, verbose_level = 1)
        
      # If repeating a second time, set overwrite to TRUE
      } else if (retry_times > 1 & retry_counter == 1) {
        
        p(add_ts("Retrying problematic file with overwrite == TRUE..."), class = "sticky", amount = 0)#.3 * prg_mult * -1)
        evaluate_auction(unlist(fa), overwrite = TRUE,
                         retry_times = min(retry_times, 2), retry_counter = retry_counter,
                         p = p, global_prog_mult = 0, verbose_level = 2)
        
      # If failed on last attempt (including an attempt that is first & only), remove progress for it
      } else {
        
        p(amount = -1 * prg_mult)
        
      }
      
    }
    )
    
  }
  
}

# Run sequentially (testing)
plan(sequential)
evaluation <- evaluate_auction(as.list(flood_areas[2]), verbose_level = 2, retry_times = 1)

# Run multisession (evaluation)
plan(multisession, workers = n_sessions)
evaluation <- evaluate_auction(as.list(flood_areas), verbose_level = 1, retry_times = 1, overwrite = FALSE)

# Sumamrize
sum_files <- summarize_predictions(stat_files, 
                                   field_shapefile = axn_file_prj, 
                                   output_dir = imp_stat_dir, 
                                   overwrite = overwrite)


