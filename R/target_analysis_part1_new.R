# Created by use_targets().


# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint
library(clustermq)

Sys.setenv(TAR_PROJECT = "project_analysis_part1_new")

# Packages ----

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# load packages

# to be used outside targets
packages.needed = c("psych","nlme","lme4","gstat","terra","dplyr", "RPostgreSQL",
                    "tidyr", "ggplot2", "data.table", "concaveman","TMB","doSNOW","foreach","readr",
                    "stringr",
                    "visNetwork","sf","raster","fasterize", "ggpubr", 
                    "sf",
                    "ggrepel", # avvoid overlapping labels
                    "cowplot",
                    "ggfortify", # to plot lm diagnostic with ggplot
                    "RColorBrewer", 
                    "ggrepel" # to avoid overlapping labels
)
lapply(packages.needed, library, character.only = TRUE)

# inside targets
tar_option_set(
  packages = packages.needed, # packages that your targets need to run
  format = "rds" # default storage format
)


# Functions----  
lapply(grep("R$", list.files("R/functions", recursive = TRUE), value = TRUE), function(x) source(file.path("R/functions", x)))


# Import objects from the "data preparation" target
db_stand.number <- tar_read(db_stand.number.final, store = "store_data_part1")
db.radiation <- tar_read(db.radiation, store = "store_data_part1")
db_climate.monthly <- tar_read(db_climate_pure_stands, store = "store_data_part1")
db.ifn.complete <- tar_read(db.ifn.complete, store = "store_data_part1")

# Replace the target list below with your own:
list(
  
  # directories
  tar_target(dir.model, "output/calibration_results/simplified"),
  tar_target(dir.data.calibration, "data_prepared/db_species_inference"), # attention, no check if modification in these files, no break the pipeline...
  tar_target(dir.data.raw, "data_prepared/db_species_raw"), # attention, no check if modification in these files, no break the pipeline...
  tar_target(dir.saving, "paper_1/figures"),
  tar_target(dir.saving.create, dir.create(dir.saving, recursive = TRUE)),
  
  # some parameters ----
  
  # last climatic month of the year
  tar_target(last_month, 8),
  
  ## set year from which to begin simulation
  tar_target(year.simulation.initial, 1950),
  tar_target(comparison.age, 70),
  
  ## set some useful periods
  tar_target(year.average.climate.noCC, c(1891:1920)), # before climate change
  tar_target(year.average.climate.afterCC, c(1991:2020)), # recent period
  
  # get species names
  tar_target(dir_db_species.name, "data_prepared/species_names/db_species_name.csv", format = "file"),
  tar_target(db_species.name_initial, fread(dir_db_species.name)),

  ## species to consider

  tar_target(species_list,c(
    "03",
    "51",
    "02",
    "09",
    "52",
    "64",
    "62",
    "05",
    "61",
    "53CO",
    "10",
    "54",
    "17C",
    "57A",
    "63",
    "14",
    "73",
    "04",
    "12V",
    "11"
  )),
  
  # species consider in Charru et al., 2017
  tar_target(
    species_list_charru,
    intersect(c("61", "62", "09", "52", "02", "03", "05", "57A"), species_list)
  ),
  
  # create output directory
  tar_target(creation.output.dir, dir.create(dir.saving, recursive = T)),
  
  # get species abbreviated names
  tar_target(
    db_species.name, 
    get_short.names(
      db_species.name = db_species.name_initial, 
      species_list = species_list
    )
  ),
  
  # Data and results description ----
  
  ## print rmse, stand sensitivity, selected variables ----
  
  tar_target(
    db_rmse,
    get_rmse(
      species = species_list,
      dir.model = dir.model
    )
  ),
  
  tar_target(
    output_summary, 
    get_summary(
      species_list = species_list,
      db_species.name = db_species.name,
      parameter_list_named = parameter_list_named, 
      db_rmse = db_rmse
    )
  ),
  
  ## data description ----
  
  tar_target(
    db_description_species, 
    describe_species(
      species_list = species_list,
      db_species.name = db_species.name,
      dir.data.calibration = dir.data.calibration
    )
  ),
  
  tar_target(
    db_description_nonclimatic.data, 
    describe_nonclimatic.data(
      species_list = species_list,
      db_species.name = db_species.name,
      dir.data.calibration = dir.data.calibration,
      db.ifn.complete = db.ifn.complete
    )
  ),
  
  ## parameter data base ----
  tar_target(
    parameter_list,
    get_parameter_table(
      species = species_list,
      dir.model = dir.model
    ),
    pattern = species_list,
    iteration = "list"
  ),
  
  tar_target(
    parameter_list_named,
    name_list(
      list_to_name = parameter_list,
      species_list = species_list
    )
  ),
  
  ## list of stand per species ----
  tar_target(
    list_stand,
    get_stand.species(
      species_code = species_list,
      dir.model = dir.model
    ),
    pattern = species_list,
    iteration = "list"
  ),
  
  tar_target(
    list_stand.named,
    name_list(
      list_to_name = list_stand,
      species_list = species_list
    )
  ),
  
  # analyse initial climate and climate evolution ----

  ## compute climate ----
  
  ### compute average climate 1991-2020 ----
  tar_target(
    list_climate.virtual_1991_2020,
    compute_climate.virtual(
      year.average.climate.noCC = c(1991:2020),
      db_climate.monthly = db_climate.monthly,
      last_month = last_month,
      species_code = species_list,
      list_stand.named = list_stand.named,
      db.radiation = db.radiation,
      list_variables.varying = list(
        ref_1991_2020 = "none"
      ),
      list_season.varying = list(
        year = 1:12
      )
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_climate.virtual_1991_2020.named,
    name_list(
      list_to_name = list_climate.virtual_1991_2020,
      species_list = species_list
    )
  ),
  
  ### compute average climate 1891-1920 ----
  
  tar_target(
    list_climate.virtual_1891_1920,
    compute_climate.virtual(
      year.average.climate.noCC = c(1891:1920),
      db_climate.monthly = db_climate.monthly,
      last_month = last_month,
      species_code = species_list,
      list_stand.named = list_stand.named,
      db.radiation = db.radiation,
      list_variables.varying = list(
        ref_1891_1920 = "none"
      ),
      list_season.varying = list(
        year = 1:12
      )
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_climate.virtual_1891_1920.named,
    name_list(
      list_to_name = list_climate.virtual_1891_1920,
      species_list = species_list
    )
  ),
  
  ### compute average climate 1931-1960 ----
  tar_target(
    list_climate.virtual_1931_1960,
    compute_climate.virtual(
      year.average.climate.noCC = c(1931:1960),
      db_climate.monthly = db_climate.monthly,
      last_month = last_month,
      species_code = species_list,
      list_stand.named = list_stand.named,
      db.radiation = db.radiation,
      list_variables.varying = list(
        ref_1931_1960 = "none"
      ),
      list_season.varying = list(
        year = 1:12
      )
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_climate.virtual_1931_1960.named,
    name_list(
      list_to_name = list_climate.virtual_1931_1960,
      species_list = species_list
    )
  ),
  
  ### compute average climate 1961-1990 ----
  tar_target(
    list_climate.virtual_1961_1990,
    compute_climate.virtual(
      year.average.climate.noCC = c(1961:1990),
      db_climate.monthly = db_climate.monthly,
      last_month = last_month,
      species_code = species_list,
      list_stand.named = list_stand.named,
      db.radiation = db.radiation,
      list_variables.varying = list(
        ref_1961_1990 = "none"
      ),
      list_season.varying = list(
        year = 1:12
      )
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_climate.virtual_1961_1990.named,
    name_list(
      list_to_name = list_climate.virtual_1961_1990,
      species_list = species_list
    )
  ),
  
  ### compute actual climate ----
  tar_target(
    list_climate.actual,
    compute_climate.virtual(
      year.average.climate.noCC = c(1891:1920),
      db_climate.monthly = db_climate.monthly,
      last_month = last_month,
      species_code = species_list,
      list_stand.named = list_stand.named,
      db.radiation = db.radiation,
      list_variables.varying = list(
        actual = c("Tmean", "precipitation")
      ),
      list_season.varying = list(
        year = 1:12
      )
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_climate.actual.named,
    name_list(
      list_to_name = list_climate.actual,
      species_list = species_list
    )
  ),
  
  
  ## merge climate
  # tar_target(
  #   list_climate_ready,
  #   merge_climate(
  #     list_climate = list(
  #       list_climate.virtual_1891_1920,
  #       list_climate.virtual_1991_2020,
  #       list_climate.actual
  #       ),
  #     species_list = species_list
  #   )
  # ),
  # 
  
  
  ## plot climate ----
  ### climate dynamics 1950-2020 - all year ----
  
  tar_target(
    graph_Tmean.dynamics_year,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(year.simulation.initial: c(year.simulation.initial + comparison.age)), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "Tmean_9_8",
      title = "Annual mean temperature anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_Tmean.dynamics_year",
      dir.saving = dir.saving, 
      color.positive = "red", 
      color.negative  = "blue"
    )
  ),
  
  tar_target(
    graph_precipitation.dynamics_year,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(year.simulation.initial: c(year.simulation.initial + comparison.age)), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "precipitation_9_8",
      title = "Annual precipitation anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_precipitation.dynamics_year",
      dir.saving = dir.saving, 
      color.positive = "green", 
      color.negative  = "orange"
    )
  ),
  
  tar_target(
    graph_cwb.dynamics_year,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(year.simulation.initial: c(year.simulation.initial + comparison.age)), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "cwb_9_8",
      title = "Annual climatic water balance anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_cwb.dynamics_year",
      dir.saving = dir.saving, 
      color.positive = "green", 
      color.negative  = "orange"
    )
  ),

  
  ### climate dynamics 1950-2020 - spring & summer ----
  
  tar_target(
    graph_Tmean.dynamics_spring.summer,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(year.simulation.initial: c(year.simulation.initial + comparison.age)), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "Tmean_3_8",
      title = "Spring and summer mean temperature anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_Tmean.dynamics_spring.summer",
      dir.saving = dir.saving, 
      color.positive = "red", 
      color.negative  = "blue"
    )
  ),
  
  tar_target(
    graph_precitation.dynamics_spring.summer,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(year.simulation.initial: c(year.simulation.initial + comparison.age)), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "precipitation_3_8",
      title = "Spring and summer precipitations anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_precipitation.dynamics_spring.summer",
      dir.saving = dir.saving, 
      color.positive = "green", 
      color.negative  = "orange"
    )
  ),
  
  tar_target(
    graph_cwb.dynamics_spring.summer,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(year.simulation.initial: c(year.simulation.initial + comparison.age)), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "cwb_3_8",
      title = "Spring and summer climatic water balance anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_cwb.dynamics_spring.summer",
      dir.saving = dir.saving, 
      color.positive = "green", 
      color.negative  = "orange"
    )
  ),
  
  
  
  ### climate dynamics 1900-2020 - all year ----
  
  tar_target(
    graph_Tmean.dynamics_year_1900.2020,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(1900:2020), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "Tmean_9_8",
      title = "Annual mean temperature anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_Tmean.dynamics_year_1900.2020",
      dir.saving = dir.saving, 
      color.positive = "red", 
      color.negative  = "blue"
    )
  ),
  
  tar_target(
    graph_precitation.dynamics_year_1900.2020,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(1900:2020), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "precipitation_9_8",
      title = "Annual precipitation anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_precipitation.dynamics_year_1900.2020",
      dir.saving = dir.saving, 
      color.positive = "green", 
      color.negative  = "orange"
    )
  ),
  
  tar_target(
    graph_cwb.dynamics_year_1900.2020,
    plot_climate.dynamics_barplot_new(
      list_climate.ref = list_climate.virtual_1891_1920.named,
      list_climate.interest = list_climate.actual.named,
      list_species = species_list,
      plotting.period = c(1900:2020), # initial period of the comparison
      averaging.period = year.average.climate.noCC,
      variable.focus = "cwb_9_8",
      title = "Annual climatic water balance anomaly compared to 1891-1920 (°C)",
      graph.name = "graph_cwb.dynamics_yea_1900.2020r",
      dir.saving = dir.saving, 
      color.positive = "green", 
      color.negative  = "orange"
    )
  ),
  
  
  
  ### initial climate ----
  
  tar_target(
    list_boxplot_climate.initial,
    plot_climate.distribution(
      list_climate = list_climate.actual.named, # list of climate per species and varying variables
      species_list = species_list,
      period.averaging = year.average.climate.noCC, # initial period of the comparison
      variable.focus = c(
        "Tmean_9_8", "precipitation_9_8","cwb_9_8",
        "Tmean_6_8", "precipitation_6_8","cwb_6_8"
      ),
      db_species.name = db_species.name # db containing the correspondence species name / species code
    )
  ),

  tar_target(
    graph_climate.initial_year,
    arrange_boxplot_climate_year(
      plotlist = list_boxplot_climate.initial,
      graph.name = "graph_climate.initial_year",
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_climate.initial_summer,
    arrange_boxplot_climate_summer(
      plotlist = list_boxplot_climate.initial,
      graph.name = "graph_climate.initial_summer",
      dir.saving = dir.saving
    )
  ),
  
  ### climate evolution ----
  
  tar_target(
    list_boxplot_climate.evolution,
    plot_climate.evolution.distribution(
      list_climate.ref = list_climate.actual.named,
      list_climate.interest = list_climate.actual.named,
      period.averaging.ref = c(1891:1920),
      period.averaging.interest  = c(1991:2020),
      species_list = species_list,
      variable.focus = c(
        "Tmean_9_8", "precipitation_9_8","cwb_9_8",
        "Tmean_6_8", "precipitation_6_8","cwb_6_8"
      ),
      db_species.name = db_species.name # db containing the correspondence species name / species code
    )
  ),
  
  tar_target(
    graph_climate.evolution_year,
    arrange_boxplot_climate_year(
      plotlist = list_boxplot_climate.evolution,
      graph.name = "graph_climate.evolution_year",
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_climate.evolution_summer,
    arrange_boxplot_climate_summer(
      plotlist = list_boxplot_climate.evolution,
      graph.name = "graph_climate.evolution_summer",
      dir.saving = dir.saving
    )
  ),
  
  
  ## #initial climate and climate dynamics ----
  
  tar_target(
    graph_climate.initial_climate.dynamics,
    plot_climate.initial_climate.dynamics(
      list_boxplot_climate.initial = list_boxplot_climate.initial, 
      graph_Tmean = graph_Tmean.dynamics_year,
      graph_precipitation = graph_precipitation.dynamics_year,
      graph_cwb = graph_cwb.dynamics_year,
      graph.name = "graph_climate.initial_climate.dynamics",
      dir.saving = dir.saving
    )
  ),
  
  # Partial effect ----
  ## Direct partial effect ----
  
  # The direct partial effect represents the effect of an increase of Tmean (resp. P, cwb) when P and cwb(resp. the 2 other variables) are constant.
  # It has a sense since the 3 variables do not appear together in the model (even including the semestrial variables)
  # When the model includes T and P, computing the direct partial effect of Tmean therefore implicitely assumes a variation of cwb to be consistent with P constant.
  # When the model includes T and cwb, computing the direct partial effect of Tmean therefore implicitely assumes a variation of P to maintain cwb constant.
  
  tar_target(
    list_direct.partial.impact,
    get_direct.partial.impact(
      species_code = species_list,
      dir.model = dir.model,
      dir.data.calibration = dir.data.calibration,
      comparison.age = comparison.age, 
      quantile.focus = c(0.05, 0.95),
      model.type = "no_batch",
      batch = 1, 
      climate.only = TRUE,
      restrict.to.significant = FALSE
    ),
    pattern = species_list,
    iteration = "list"
  ),
  
  tar_target(
    graph_direct.partial.impact_vertical, 
    plot_direct.partial.impact_vertical(
      species_list = species_list,
      list_variable.impact = list_direct.partial.impact,
      db_species.name = db_species.name,
      dir.saving = dir.saving
    )
  ),
  
  # tar_target(
  #   graph_direct.partial.impact_individual, 
  #   plot_direct.partial.impact_individual(
  #     species_list = species_list,
  #     list_variable.impact = list_direct.partial.impact,
  #     db_species.name = db_species.name,
  #     dir.saving = dir.saving
  #   )
  # ),
  
  ## Indirect partial effect ----
  # not used then
  # The indirect partial effect represents the effect of an increase of Tmean (resp. P) when P (resp. Tmean) is constant.
  # It therefore includes a variation of cwb a a consequence of the Tmean (resp. P) increase.
  # 
  # tar_target(
  #   list_indirect.partial.impact,
  #   get_indirect.partial.impact(
  #     species_code = species_list,
  #     dir.model = dir.model,
  #     dir.data.calibration = dir.data.calibration,
  #     model.type = "no_batch",
  #     batch = 1,
  #     comparison.age = comparison.age,
  #     db_climate.monthly = db_climate.monthly,
  #     db.radiation = db.radiation
  #   ),
  #   pattern = species_list,
  #   iteration = "list"
  # ),
  # 
  # tar_target(
  #   graph_indirect.partial.impact,
  #   plot_indirect.partial.impact(
  #     species_list = species_list,
  #     list_partial.impact = list_indirect.partial.impact,
  #     db_species.name = db_species.name,
  #     ymin = 10,
  #     ymax = 45, 
  #     graph.name = "graph_indirect.partial.impact",
  #     dir.saving = dir.saving,
  #     text.size.main = 4,
  #     height = 12,
  #     width = 17,
  #     dir.model = dir.model,
  #     model.type = "no_batch",
  #     batch = 1
  #   )
  # ),

  # Dynamics analysis (1891-1920 & 1950-2020) ----
  
  ## Dynamics computation ----

  tar_target(
    list_db_dynamics_climate.virtual_1991_2020,
    simulate_height_dynamics(
      species_code = species_list,
      year.simulation.initial = year.simulation.initial,
      comparison.age = comparison.age,
      list_climate = list_climate.virtual_1991_2020.named,
      dir.model = dir.model,
      dir.data.calibration = dir.data.calibration
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_dynamics_climate.virtual_1991_2020.named,
    name_list(
      list_to_name = list_db_dynamics_climate.virtual_1991_2020,
      species_list = species_list
    )
  ),
  
  tar_target(
    list_db_dynamics_climate.virtual_1891_1920,
    simulate_height_dynamics(
      species_code = species_list,
      year.simulation.initial = year.simulation.initial,
      comparison.age = comparison.age,
      list_climate = list_climate.virtual_1891_1920.named,
      dir.model = dir.model,
      dir.data.calibration = dir.data.calibration
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_dynamics_climate.virtual_1891_1920.named,
    name_list(
      list_to_name = list_db_dynamics_climate.virtual_1891_1920,
      species_list = species_list
    )
  ),
  
  tar_target(
    list_db_dynamics_climate.virtual_1931_1960,
    simulate_height_dynamics(
      species_code = species_list,
      year.simulation.initial = year.simulation.initial,
      comparison.age = comparison.age,
      list_climate = list_climate.virtual_1931_1960.named,
      dir.model = dir.model,
      dir.data.calibration = dir.data.calibration
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_dynamics_climate.virtual_1931_1960.named,
    name_list(
      list_to_name = list_db_dynamics_climate.virtual_1931_1960,
      species_list = species_list
    )
  ),
  
  tar_target(
    list_db_dynamics_climate.virtual_1961_1990,
    simulate_height_dynamics(
      species_code = species_list,
      year.simulation.initial = year.simulation.initial,
      comparison.age = comparison.age,
      list_climate = list_climate.virtual_1961_1990.named,
      dir.model = dir.model,
      dir.data.calibration = dir.data.calibration
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_dynamics_climate.virtual_1961_1990.named,
    name_list(
      list_to_name = list_db_dynamics_climate.virtual_1961_1990,
      species_list = species_list
    )
  ),
  
  tar_target(
    list_db_dynamics_climate.actual,
    simulate_height_dynamics(
      species_code = species_list,
      year.simulation.initial = year.simulation.initial,
      comparison.age = comparison.age,
      list_climate = list_climate.actual.named,
      dir.model = dir.model,
      dir.data.calibration = dir.data.calibration
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_dynamics_climate.actual.named,
    name_list(
      list_to_name = list_db_dynamics_climate.actual,
      species_list = species_list
    )
  ),
  
  ## Dynamics graphs----
  
  tar_target(
    list_graph_dynamics_virtual_1891_1920_vs_actual,
    plot_dynamics_group_new(
      species_code = species_list,
      list_db_dynamics.ref = list_db_dynamics_climate.virtual_1891_1920.named,
      list_db_dynamics.interest = list_db_dynamics_climate.actual.named,
      age.productivity = comparison.age,
      label.ref = "average 1891 - 1920",
      label.interest = "actual",
      db_species.name = db_species.name
    ),
    pattern = species_list,
    iteration = "list"
  ),
  
  tar_target(
    list_graph_dynamics_virtual_1891_1920_vs_virtual_1991_2020,
    plot_dynamics_group_new(
      species_code = species_list,
      list_db_dynamics.ref = list_db_dynamics_climate.virtual_1891_1920.named,
      list_db_dynamics.interest = list_db_dynamics_climate.virtual_1991_2020.named,
      age.productivity = comparison.age,
      label.ref = "average 1891 - 1920",
      label.interest = "average 1991 - 2020",
      db_species.name = db_species.name
    ),
    pattern = species_list,
    iteration = "list"
  ),
  
  tar_target(
    list_graph_dynamics_virtual_1931_1960_vs_actual,
    plot_dynamics_group_new(
      species_code = species_list,
      list_db_dynamics.ref = list_db_dynamics_climate.virtual_1931_1960.named,
      list_db_dynamics.interest = list_db_dynamics_climate.actual.named,
      age.productivity = comparison.age,
      label.ref = "average 1931 - 1960",
      label.interest = "actual",
      db_species.name = db_species.name
    ),
    pattern = species_list,
    iteration = "list"
  ),
  
  tar_target(
    list_graph_dynamics_virtual_1961_1990_vs_actual,
    plot_dynamics_group_new(
      species_code = species_list,
      list_db_dynamics.ref = list_db_dynamics_climate.virtual_1961_1990.named,
      list_db_dynamics.interest = list_db_dynamics_climate.actual.named,
      age.productivity = comparison.age,
      label.ref = "average 1961 - 1990",
      label.interest = "actual",
      db_species.name = db_species.name
    ),
    pattern = species_list,
    iteration = "list"
  ),
  

  ## Arrange graph ----
  ### for Abies alba ----
  
  tar_target(
    graph_dynamics_61_virtual_1891_1920_vs_actual,
    plot_dynamics_single(
      plotlist = list_graph_dynamics_virtual_1891_1920_vs_actual,
      species_list = species_list,
      species_code = "61",
      db_species.name = db_species.name,
      graph.name = "graph_dynamics_61_virtual_1891_1920_vs_actual",
      dir.saving = dir.saving,
      text.size.main = 5,
      height = 10,
      width = 15
    )),
  
  ## for all species ----
  

  tar_target(
    graph_dynamics_virtual_1891_1920_vs_actual,
    plot_dynamics_all(
      plotlist = list_graph_dynamics_virtual_1891_1920_vs_actual,
      species_list = species_list,
      db_species.name = db_species.name, 
      graph.name = "graph_dynamics_virtual_1891_1920_vs_actual",
      y.max = 45,
      text.size.main = 4,
      height = 20, 
      width = 17,
      dir.saving = dir.saving
    )),
  
  tar_target(
    graph_dynamics_virtual_1931_1960_vs_actual,
    plot_dynamics_all(
      plotlist = list_graph_dynamics_virtual_1931_1960_vs_actual,
      species_list = species_list,
      db_species.name = db_species.name, 
      graph.name = "graph_dynamics_virtual_1931_1960_vs_actual",
      y.max = 45,
      text.size.main = 4,
      height = 20, 
      width = 17,
      dir.saving = dir.saving
    )),
  
  tar_target(
    graph_dynamics_virtual_1961_1990_vs_actual,
    plot_dynamics_all(
      plotlist = list_graph_dynamics_virtual_1961_1990_vs_actual,
      species_list = species_list,
      db_species.name = db_species.name, 
      graph.name = "graph_dynamics_virtual_1961_1990_vs_actual",
      y.max = 45,
      text.size.main = 4,
      height = 20, 
      width = 17,
      dir.saving = dir.saving
    )),
  
  tar_target(
    graph_dynamics_virtual_1891_1920_vs_virtual_1991_2020,
    plot_dynamics_all(
      plotlist = list_graph_dynamics_virtual_1891_1920_vs_virtual_1991_2020,
      species_list = species_list,
      db_species.name = db_species.name, 
      graph.name = "graph_dynamics_virtual_1891_1920_vs_virtual_1991_2020",
      y.max = 45,
      text.size.main = 4,
      height = 20, 
      width = 17,
      dir.saving = dir.saving
    )),
  
  
  # Impact of CC -----
  
  ## Computation ----
  
  tar_target(
    list_db_impact_virtual_1891_1920_vs_actual,
    compute_CCimpact(
      species_code = species_list,
      list_db_dynamics.ref = list_db_dynamics_climate.virtual_1891_1920.named,
      list_db_dynamics.interest = list_db_dynamics_climate.actual.named,
      db_species.name = db_species.name,
      comparison.age = comparison.age
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_impact_virtual_1891_1920_vs_actual.named,
    name_list(
      list_to_name = list_db_impact_virtual_1891_1920_vs_actual,
      species_list = species_list
    )
  ),
  
  tar_target(
    list_db_impact_virtual_1891_1920_vs_virtual_1991_2020,
    compute_CCimpact(
      species_code = species_list,
      list_db_dynamics.ref = list_db_dynamics_climate.virtual_1891_1920.named,
      list_db_dynamics.interest = list_db_dynamics_climate.virtual_1991_2020.named,
      db_species.name = db_species.name,
      comparison.age = comparison.age
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_impact_virtual_1891_1920_vs_virtual_1991_2020.named,
    name_list(
      list_to_name = list_db_impact_virtual_1891_1920_vs_virtual_1991_2020,
      species_list = species_list
    )
  ),
  
  tar_target(
    list_db_impact_virtual_1961_1990_vs_virtual_1991_2020,
    compute_CCimpact(
      species_code = species_list,
      list_db_dynamics.ref = list_db_dynamics_climate.virtual_1961_1990.named,
      list_db_dynamics.interest = list_db_dynamics_climate.virtual_1991_2020.named,
      db_species.name = db_species.name,
      comparison.age = comparison.age
    ),
    pattern = species_list,
    iteration = "list"
  ),
  tar_target(
    list_db_impact_virtual_1961_1990_vs_virtual_1991_2020.named,
    name_list(
      list_to_name = list_db_impact_virtual_1961_1990_vs_virtual_1991_2020,
      species_list = species_list
    )
  ),
  
  
  ## Interspecific analysis  ----
  
  ### virtual_1891_1920_vs_actual -----
  
  tar_target(
    graph_CCimpact_interspecific_virtual_1891_1920_vs_actual,
    plot_CCimpact_interspecific(
      list_db_impact = list_db_impact_virtual_1891_1920_vs_actual.named,
      comparison.age = comparison.age,
      db_species.name = db_species.name,
      y.min = -0.13,
      y.max = 0.23,
      graph.name = "graph_CCimpact_interspecific_virtual_1891_1920_vs_actual",
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_impact_habitat_virtual_1891_1920_vs_actual,
    plot_CCimpactVShabitat(
      list_db_impact = list_db_impact_virtual_1891_1920_vs_actual.named, 
      list_climate_ref = list_climate.virtual_1891_1920.named,
      graph.name = "graph_impact_habitat_virtual_1891_1920_vs_actual",
      db_species.name = db_species.name,
      variable.focus = c("Tmean_9_8", "precipitation_9_8"),
      label.x.axis = c(
        Tmean_9_8 = "Mean annual temperatures (°C)",
        precipitation_9_8="Total annual precipitation (mm)"
        ),
      species_list = species_list,
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_impact_habitat_virtual_1891_1920_vs_actual_charru,
    plot_CCimpactVShabitat(
      list_db_impact = list_db_impact_virtual_1891_1920_vs_actual.named, 
      list_climate_ref = list_climate.virtual_1891_1920.named,
      graph.name = "graph_impact_habitat_virtual_1891_1920_vs_actual_charru",
      db_species.name = db_species.name,
      variable.focus = c("Tmean_9_8", "precipitation_9_8"),
      label.x.axis = c(
        Tmean_9_8 = "Mean annual temperatures (°C)",
        precipitation_9_8="Total annual precipitation (mm)"
      ),
      species_list = species_list_charru,
      dir.saving = dir.saving
    )
  ),

  
  ### virtual_1891_1920_vs_virtual_1991_2020 -----
  
  tar_target(
    graph_CCimpact_interspecific_virtual_1891_1920_vs_virtual_1991_2020,
    plot_CCimpact_interspecific(
      list_db_impact = list_db_impact_virtual_1891_1920_vs_virtual_1991_2020,
      comparison.age = comparison.age,
      db_species.name = db_species.name,
      y.min = -0.30,
      y.max = 0.30,
      graph.name = "graph_CCimpact_interspecific_virtual_1891_1920_vs_virtual_1991_2020",
      dir.saving = dir.saving
    )
  ),
  
  ### virtual_1961_1990_vs_virtual_1991_2020 -----
  
  tar_target(
    graph_CCimpact_interspecific_virtual_1961_1990_vs_virtual_1991_2020,
    plot_CCimpact_interspecific(
      list_db_impact = list_db_impact_virtual_1961_1990_vs_virtual_1991_2020,
      comparison.age = comparison.age,
      db_species.name = db_species.name,
      y.min = -0.2,
      y.max = 0.2,
      graph.name = "graph_CCimpact_interspecific_virtual_1961_1990_vs_virtual_1991_2020",
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_impact_habitat_virtual_1961_1990_vs_virtual_1991_2020,
    plot_CCimpactVShabitat(
      list_db_impact = list_db_impact_virtual_1961_1990_vs_virtual_1991_2020.named, 
      list_climate_ref = list_climate.virtual_1961_1990.named,
      graph.name = "graph_impact_habitat_virtual_1961_1990_vs_virtual_1991_2020",
      db_species.name = db_species.name,
      variable.focus = c("Tmean_9_8", "precipitation_9_8"),
      label.x.axis = c(
        Tmean_9_8 = "Mean annual temperatures (°C)",
        precipitation_9_8="Total annual precipitation (mm)"
      ),
      species_list = species_list,
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_impact_habitat_virtual_1961_1990_vs_virtual_1991_2020_Charru,
    plot_CCimpactVShabitat(
      list_db_impact = list_db_impact_virtual_1961_1990_vs_virtual_1991_2020.named, 
      list_climate_ref = list_climate.virtual_1961_1990.named,
      graph.name = "graph_impact_habitat_virtual_1961_1990_vs_virtual_1991_2020_Charru",
      db_species.name = db_species.name,
      variable.focus = c("Tmean_9_8", "precipitation_9_8"),
      label.x.axis = c(
        Tmean_9_8 = "Mean annual temperatures (°C)",
        precipitation_9_8="Total annual precipitation (mm)"
      ),
      species_list = species_list_charru,
      dir.saving = dir.saving
    )
  ),

  ## Intraspecific analysis as a function of initial climate ----
  
  tar_target(
    graph_CCimpact_intraspecific_Tmean.initial_virtual_1891_1920_vs_actual,
    plot_CCimpact_intraspecific(
      species_list = setdiff(species_list, c("54", "73")), # exclude species without variations
      db_species.name = db_species.name,
      list_climate = list_climate.actual.named,
      period.initial = year.average.climate.noCC,
      list_db_impact = list_db_impact_virtual_1891_1920_vs_actual.named,
      variable.focus = "Tmean_9_8",
      interval.length = 1,
      label.x = "Annual temperature (°C, average 1891-1920)",
      label.y = "Climate change impact",
      text.size.main = 5,
      text.size.axis = 4,
      angle.x = 45,
      height = 20, 
      width = 17,
      graph.name = "graph_CCimpact_intraspecific_Tmean.initial_virtual_1891_1920_vs_actual",
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_intraspecific_impact_precipitation.initial_virtual_1891_1920_vs_actual,
    plot_CCimpact_intraspecific(
      species_list = setdiff(species_list, c("54", "73")), # exclude species without variations
      db_species.name = db_species.name,
      list_climate = list_climate.actual.named,
      period.initial = year.average.climate.noCC,
      list_db_impact = list_db_impact_virtual_1891_1920_vs_actual.named,
      variable.focus = "precipitation_9_8",
      interval.length = 100,
      label.x = "Annual precipitation (mm, average 1891-1920)",
      label.y = "Climate change impact",
      text.size.main = 5,
      text.size.axis = 3,
      angle.x = 45,
      height = 20, 
      width = 17,
      graph.name = "graph_CCimpact_intraspecific_precipitation.initial_virtual_1891_1920_vs_actual",
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph_intraspecific_impact_cwb.initial_virtual_1891_1920_vs_actual,
    plot_CCimpact_intraspecific(
      species_list = setdiff(species_list, c("54", "73")), # exclude species without variations
      db_species.name = db_species.name,
      list_climate = list_climate.actual.named,
      period.initial = year.average.climate.noCC,
      list_db_impact = list_db_impact_virtual_1891_1920_vs_actual.named,
      variable.focus = "cwb_9_8",
      interval.length = 100,
      label.x = "Annual climatic water balance (mm, average 1891-1920)",
      label.y = "Climate change impact",
      text.size.main = 5,
      text.size.axis = 3,
      angle.x = 45,
      height = 20, 
      width = 17,
      graph.name = "graph_intraspecific_impact_cwb.initial_virtual_1891_1920_vs_actual",
      dir.saving = dir.saving
    )
  ),
  

  
  # Check bias and optimism ----
  
  tar_target(
    list_analysis.rmse.bias,
    analyze.rmse.bias(
      species_code = species_list,
      dir.model = dir.model,
      dir.data.calibration = dir.data.calibration
    ),
    pattern = species_list,
    iteration = "list"
  ),

  tar_target(
    graph.metrics.species,
    plot.metrics.species(
      list_metrics = list_analysis.rmse.bias,
      dir.saving = dir.saving
    )
  ),
  
  tar_target(
    graph.optimism,
    plot.optimism(
      list_metrics = list_analysis.rmse.bias,
      db_species.name = db_species.name, 
      db_stand.number = db_stand.number, 
      dir.saving = dir.saving
    )
  ),
  
  NULL
  
)