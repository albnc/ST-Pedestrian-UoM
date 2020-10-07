# City of Melbourne Pedestrian Count Data Event Analysis
# Jessica Tong

library(tidyverse)
library(lubridate)
library(ggthemes)
# Wavelet Application
library(waveslim)
# Spatial Analysis
library(geoR)

# Data Cleaning and Investigation ####

# Note: Run up until the end of Data Cleaning and Investigation section to obtain two useful dataframes:
# 1. ped_counts_and_locations (Use "Date_Time_Corrected", not "Date_Time")
# 2. complete_ped_counts_and_locations (Use "Complete_Date_Time", "Complete_Hourly_Counts")


# Import Pedestrian Count File
ped_counts_filename <- list.files(pattern = "^Ped.*per_hour_")
ped_counts <- read_csv(ped_counts_filename) %>% 
  mutate(
    Date_Time = parse_date_time(Date_Time, "mdYHMSp"),
    idx = seq_len(n())
  )

# # Check individual counts to determine how faulty sensors are dealt with in large dataset 
# jan18 <- read_csv("January_2018.csv") %>%
#   mutate_at(vars(-c(Date,Hour)), as.numeric) %>%
#   mutate(Date = dmy(Date)) %>%
#   pivot_longer(-c(Date,Hour), names_to="Sensor_Name", values_to="Hourly_Counts") %>%
#   left_join(pedestrian_counts, by=c("Sensor_Name"="Sensor_Name","Date"="Date","Hour"="Time"))
#   # conclude that pedestrian_count file has removed any sensors which were faulty/ gave a value of -1
#   # use pedestrian_count file instead of monthly files

# Find duplicate values where hour value is faulty and input correct date and time values
ped_counts_duplicates <- ped_counts %>%
  group_by(Sensor_ID, Date_Time) %>% 
  filter(n()>1) %>% 
  mutate(Date_Time_Corrected = Date_Time,
         Time = seq_along(Time)-1) %>% 
  ungroup() %>% 
  select(Time, idx, Date_Time_Corrected)
hour(ped_counts_duplicates$Date_Time_Corrected) <- ped_counts_duplicates$Time

# Corrected dataset with corrected date and time values for duplicates
ped_counts_corrected <- ped_counts %>% 
  left_join(ped_counts_duplicates, by=c("idx")) %>% 
  mutate(Date_Time_Corrected = coalesce(Date_Time_Corrected, Date_Time),
         Time_Corrected = coalesce(Time.y, Time.x),
         Date = date(Date_Time_Corrected),
         Season = case_when(
           month(Date_Time_Corrected) %in% c(3:5) ~ "Autumn",
           month(Date_Time_Corrected) %in% c(6:8) ~ "Winter",
           month(Date_Time_Corrected) %in% c(9:11) ~ "Spring",
           TRUE ~ "Summer")
  ) %>% 
  select(-c(Time.x, Time.y))

# Import Pedestrian Counter Locations File, Merge with Pedestrian Counts
location_filename <- list.files(pattern = "^Ped.*Locations")
locations <- read_csv(location_filename)

ped_counts_and_locations <- ped_counts_corrected %>% 
  left_join(locations, by=c("Sensor_ID" = "sensor_id"))

# Completed timeseries dataset for use with pedestrian counts and sensor location information 
complete_ped_counts_and_locations <- ped_counts_and_locations %>%
  # Find first and last instance of sensor Date_Time
  group_by(Sensor_ID) %>% 
  summarise(First_Date = min(Date_Time_Corrected),
            Last_Date = max(Date_Time_Corrected)) %>%
  # Create new dataframe with a complete Date_Time list for all sensors
  rowwise() %>% 
  mutate(Complete_Date_Time = list(seq(First_Date, Last_Date, by="1 hour"))) %>% 
  unnest(Complete_Date_Time) %>% 
  select(Sensor_ID, Complete_Date_Time) %>%
  # Join dataset to complete dataframe
  left_join(ped_counts_and_locations, by=c("Sensor_ID","Complete_Date_Time"="Date_Time_Corrected")) %>% 
  # Set all missing hourly count values as 0
  mutate(Complete_Hourly_Counts = coalesce(Hourly_Counts,0))

# Clear up workspace - only keep the two files:
# 1. ped_counts_and_locations
# 2. complete_ped_counts_and_locations
rm(ped_counts_filename,
   ped_counts,
   ped_counts_duplicates,
   ped_counts_corrected,
   location_filename,
   locations
)

# Anomaly Identification: Manual Analysis and Plots #####
# Does not use completed dataset as non-existant Hourly Counts are not included in averages

# Create an outlier identifier based on the Month and Year for Each Sensor
ped_counts_and_locations <- ped_counts_and_locations %>% 
  group_by(Sensor_ID, Year, Month) %>%
  mutate(counts_avg = mean(Hourly_Counts, na.rm = TRUE), 
         # threshold is calculated using global threshold lambda Donoho, D.L., Johnstone, I.M.: Ideal spatial adaption by wavelet shrinkage.
         counts_threshold = sd(Hourly_Counts, na.rm = TRUE) * sqrt(2*log(sum(!is.na(Hourly_Counts)))),
         outlier_flag = as.integer(Hourly_Counts > (counts_avg + counts_threshold) | Hourly_Counts < (counts_avg - counts_threshold))
  )

# Data Driven Identification: find outliers which maybe events
ped_counts_outliers <- ped_counts_and_locations %>% 
  filter(outlier_flag == 1)

# Event Driven Identification: find and flag events which match with outliers
ped_counts_events <- read_csv("eventexample.csv") %>% 
  mutate(Date = dmy(Date),
         Start_Hour = hms(Start_Hour),
         End_Hour = hms(End_Hour),
  ) %>% 
  left_join(ped_counts_and_locations, by = "Date") %>%
  mutate(outlier_eventtime_flag = as.integer(hour(Date_Time) >= hour(Start_Hour) & hour(Date_Time)<= hour(End_Hour))) %>% 
  group_by(Date_Time) %>% 
  filter(outlier_flag == 1, outlier_eventtime_flag == 1)


# Plot 1: chosen sensors by over the same years/months
chosen_sensors <- c(7)
chosen_years <- c(2019)
chosen_months <- c(12)
chosen_sensor_names <- ped_counts_and_locations %>%
  filter(Sensor_ID %in% chosen_sensors)%>%
  pull(Sensor_Name) %>% 
  unique() %>% 
  sort()
chosen_months_names <- ped_counts_and_locations %>%
  filter( month(Date_Time) %in% chosen_months)%>%
  pull(Month) %>% 
  unique() %>% 
  sort()

ped_counts_and_locations %>% 
  filter(Year %in% chosen_years, 
         month(Date_Time) %in% chosen_months, 
         Sensor_ID %in% chosen_sensors
  ) %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts)) +
  geom_line() +
  geom_line(aes(y = counts_avg, linetype = "mean")) +
  geom_line(aes(y = counts_avg + counts_threshold, linetype = "ubound")) +
  geom_line(aes(y = counts_avg - counts_threshold, linetype = "lbound")) +
  facet_grid(vars(Sensor_Name), vars(Year), scales = "free") +
  ggtitle(paste("Hourly Pedestrian Counts in", chosen_months_names, chosen_years),
          paste(chosen_sensor_names, collapse = ", ")
  ) +
  xlab("Date") +
  ylab("Hourly Pedestrian Count") +
  ylim(0, NA) +
  scale_linetype_manual(
    name = "Legend",
    values = c("mean" = "dashed", "ubound" = "dotted", "lbound" = "dotted"),
    labels = c("mean" = "Mean", "ubound" = "Upper Threshold", "lbound" = "Lower Threshold")
  ) +
  theme_grey(base_family = "sans")

# Plot 2: Recurring Events - plot of sensor over the same month for multiple years
chosen_sensor2 <- c(12)       #input a single sensor ID (numeric)
chosen_years2 <- c(2017:2019) #input years
chosen_month2 <- c(1)         #input month of year (numeric) 
chosen_dates2 <- c(1:31)      #input dates
chosen_sensor2_name <- ped_counts_and_locations %>%
  filter(Sensor_ID %in% chosen_sensor2)%>%
  pull(Sensor_Name) %>%
  unique()
chosen_month2_names <- ped_counts_and_locations %>%
  filter( month(Date_Time) %in% chosen_month2)%>%
  pull(Month) %>% 
  unique() %>% 
  sort()

ped_counts_and_locations %>%
  filter(Year %in% chosen_years2,
         month(Date_Time) %in% chosen_month2,
         Mdate %in% chosen_dates2,
         Sensor_ID %in% chosen_sensor2
  ) %>%
  ggplot(aes(x = Date_Time, y = Hourly_Counts)) +
  geom_line() +
  geom_line(aes(y = counts_avg, linetype = "mean")) +
  geom_line(aes(y = counts_avg + counts_threshold, linetype = "ubound")) +
  geom_line(aes(y = counts_avg - counts_threshold, linetype = "lbound")) +
  facet_wrap(vars(Year), scale="free", ncol = 1) +
  ggtitle(paste("Hourly Pedestrian Counts at",chosen_sensor2_name, "in", chosen_month2_names)) +
  xlab("Date") +
  ylab("Hourly Pedestrian Count") +
  ylim(0, NA) +
  scale_linetype_manual(
    name = "Legend",
    values = c("mean"="dashed", "ubound"="dotted", "lbound"="dotted"),
    labels = c("mean"="Mean", "ubound"="Upper Threshold", "lbound"="Lower Threshold")
  ) +
  theme_grey(base_family = "sans")

# Mother Wavelet Choice: (no need to rerun this section once mother wavelet is chosen) ####
# Choose year, and month to investigate
# wavelet_chosen_sensors <- c()
wavelet_chosen_year <- c(2019)
wavelet_chosen_months <- c(1:12)
wavelet_types <- c("haar","d8")
# wavelet_types <- c("la8","bl14")
wavelet_levels <- 1:5

# Create an expanded dataframe using chosen sensor, year, month to include a column to identify wavelet types and the level of wavelet transform
mother_wavelet_ped_counts <- complete_ped_counts_and_locations %>%
  # Filter by chosen sensor, years, and months
  filter(year(Complete_Date_Time) %in% wavelet_chosen_year,
         # Sensor_ID %in% wavelet_chosen_sensors,
         month(Complete_Date_Time) %in% wavelet_chosen_months
  ) %>%
  # Select on the Sensor ID, Date/Time and Hourly Counts columns
  select(Sensor_ID,Complete_Date_Time,Complete_Hourly_Counts) %>%
  # Create a dataframe with selected data for each chosen wavelet level and wavelet type
  expand_grid(wavelet_levels, wavelet_types)

# Clear Workspace (not enough memory to run decomposition)
rm(ped_counts_and_locations,
   complete_ped_counts_and_locations)

# Compute the maximal overlap discrete wavelet transform for the chosen year+months and mother wavelets+levels
mother_wavelet_choice <- mother_wavelet_ped_counts %>%
  group_by(Sensor_ID, wavelet_levels, wavelet_types) %>%
  # modwt transform by the wavelet type and level of decomposition chosen for all chosen Year+months
  summarise(
    data = list(tibble(Complete_Date_Time, Complete_Hourly_Counts)),
    mod = list(
      as_tibble(
        unclass(
          modwt(Complete_Hourly_Counts, wf = unique(wavelet_types), n.levels = unique(wavelet_levels)))
      )
    )
  ) %>%
  unnest(c(data, mod)) %>%
  # "value" column contains either the Hourly Count, detail, or scaling coefficient ("name")
  pivot_longer(-c(Sensor_ID, wavelet_types, wavelet_levels, Complete_Date_Time), values_drop_na = TRUE)

# Loop through and plot the original data with the scaling coefficients for each level and each wavelet type (by sensor)
for (i in unique(mother_wavelet_choice$Sensor_ID)) {
  mother_wavelet_choice %>%
    filter(Sensor_ID == i &
             (name == "Complete_Hourly_Counts" | grepl("^s\\d+$", name))) %>%
    ggplot(aes(x = Complete_Date_Time, y = value, colour = name)) +
    geom_line() +
    facet_grid(wavelet_levels ~ wavelet_types) +
    xlab("Date") +
    ylab("Hourly Pedestrian Count") +
    ggtitle(paste("Hourly Pedestrian Counts at sensor", i, "in", wavelet_chosen_year,
                  "original data and scaling coefficients for the wavelet decomposition level"
    ))
  # Save Plot
  ggsave(paste("Sensor", i, "in", wavelet_chosen_year, ".pdf"
  ),
  path = "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Mother_wavelets",
  width = 45,
  height = 40
  )
}
# Manual investigation of all plots shows that Haar wavlet best matches the peaks and troughs in the dataset at all levels

# Anomoly Identification: Wavelet Application ####
# Use complete dataset with "0" for Missing Hourly Counts
wavelet_type <- "haar"
wavelet_level <- 6

# Compute the maximal overlap discrete wavelet transform for the chosen year+months and mother wavelet+levels
wavelet_ped_counts <- complete_ped_counts_and_locations %>%
  # Select on the Sensor ID, Date/Time and Hourly Counts columns
  select(Sensor_ID,Complete_Date_Time,Complete_Hourly_Counts) %>%
  group_by(Sensor_ID) %>%
  # modwt transform by the wavelet type and level of decomposition chosen for all chosen Year+months
  summarise(
    data = list(tibble(Complete_Date_Time, Complete_Hourly_Counts)),
    mod = list(
      as_tibble(
        unclass(
          modwt(Complete_Hourly_Counts, wf = unique(wavelet_type), n.levels = unique(wavelet_level)))
      )
    )
  ) %>%
  unnest(c(data, mod))

# Threshold and set outliers for each level of detail
wavelet_outliers <- wavelet_ped_counts %>%
  select(-c(Complete_Hourly_Counts, matches("^s\\d+$"))) %>% 
  pivot_longer(-c(Sensor_ID, Complete_Date_Time), values_drop_na = TRUE) %>% 
  group_by(Sensor_ID, name) %>% 
  mutate(d_avg = mean(value),
         # threshold is calculated using global threshold lambda Donoho, D.L., Johnstone, I.M.: Ideal spatial adaption by wavelet shrinkage.
         d_threshold = sd(value) * sqrt(2*log(sum(!is.na(value)))),
         d_flag = as.integer(value > (d_avg + d_threshold) | value < (d_avg - d_threshold))
  ) %>% 
  pivot_wider(c(Sensor_ID,Complete_Date_Time),
              names_from = name,
              values_from = d_flag) %>% 
  mutate(outlier_d1 = d1,
         outlier_d2 = d2,
         outlier_d3 = d3,
         outlier_d4 = d4,
         outlier_d5 = d5,
         outlier_d6 = d6) %>%
  select(-c(starts_with("d")))

# Join outliers to original dataset for use in exploratory analysis
ped_counts_and_locations_outliers <- ped_counts_and_locations %>% 
  left_join(wavelet_outliers, by = (c("Sensor_ID", "Date_Time_Corrected" = "Complete_Date_Time"))) %>% 
  select(-Date_Time)

# Anomoly/Event: Temporal Span Investigation #### 
temporal_investigation <- ped_counts_and_locations_outliers %>% 
  select(Date_Time_Start = Date_Time_Corrected, Sensor_ID, starts_with("outlier")) %>% 
  pivot_longer(-c(Sensor_ID, Date_Time_Start), names_to = "Level", values_to = "Outlier") %>% 
  filter(Outlier == 1) %>%
  mutate(Date_Time_End = Date_Time_Start + hours((2^as.numeric(str_sub(Level, -1)))),
         Outlier = as.numeric(str_sub(Level, -1))
         ) %>% 
  pivot_longer(-c(Sensor_ID, Level, Outlier), values_to = "Date_Time")

temporal_start <- "2019-08-23 00:00:00"
temporal_end <- "2019-08-25 23:00:00"
temporal_sensors <- c(7,29,28,33,34)

temporal_investigation %>% 
  filter(Sensor_ID %in% temporal_sensors) %>%
  ggplot(aes(x = Date_Time, y = Outlier, colour = Level)) +
  geom_point(aes(shape = name)) +
  facet_wrap(vars(Sensor_ID), ncol = 1) + 
  xlab("Date/Time") +
  xlim(ymd_hms(temporal_start), 
       ymd_hms(temporal_end)
       ) +
  scale_y_discrete(name = "Outlier (# = Level of decomposition)",
                   limits=c("1", "2", "3", "4", "5", "6")) +
  ggtitle(paste0("Outliers for sensors ", paste0(temporal_sensors, collapse = ", "), " between ", ymd_hms(temporal_start), " to ", ymd_hms(temporal_end)))
ggsave(paste0("Outliers for sensors ", paste0(temporal_sensors, collapse = "_")," from ", ymd(ymd_hms(temporal_start)),".pdf"),
       path = "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Outlier_plots",
       width =20,
       height = 5
       )

# Comparison of classified events to anomolies ####
# Waiting for complete event dataset from Marcus

# Spatial Statistical Analysis ####
# Use ped_counts_and_locations data table

# Pivot data so that you only have unique Date_Time_Corrected values in each row
ped_counts_variance <- ped_counts_and_locations %>%
  select(Date_Time_Corrected, Sensor_ID, Hourly_Counts) %>%
  pivot_wider(Date_Time_Corrected,
              names_from = Sensor_ID,
              values_from = Hourly_Counts)

# Check that there are no duplicated Date_Time_Corrected Rows
sum(duplicated(ped_counts_variance$Date_Time_Corrected))

# Create new columns with the variance between each sensor in each time period
sensors <- unique(ped_counts_and_locations$Sensor_ID)
for (i in sensors){
  for (j in sensors){
    ped_counts_variance[[paste0(i,"-",j)]] <- (ped_counts_variance[[paste0(i)]] - ped_counts_variance[[paste0(j)]])^2
    }
}

# Import Sensor Distances
sensor_distances <- read_csv("sensor_straightline_distance.csv") %>% 
  mutate(Sensor_Pair = paste0(InputID,"-",TargetID)) %>% 
  select(Sensor_Pair, Distance)

# Join Sensor Distances to Sensor Variances by Sensor_Pair
# only on 2019 as not enough memory to run on all data
year_var <- 2019
ped_counts_variance_dist <- ped_counts_variance %>% 
  filter(year(Date_Time_Corrected) == year_var) %>% 
  select(Date_Time_Corrected, contains("-")) %>% 
  pivot_longer(-(Date_Time_Corrected), names_to = "Sensor_Pair", values_to = "Variance", values_drop_na = TRUE) %>%
  left_join(sensor_distances, by = "Sensor_Pair") %>%
  drop_na()

# Variogram for all sensors to all sensors in chosen year
ped_counts_semivarianceall <- ped_counts_variance_dist %>%
  filter(hour(Date_Time_Corrected) >= start_hour,
         hour(Date_Time_Corrected) <= end_hour
         ) %>% 
  group_by(Distance) %>% 
  summarise(Semivariance = sum(Variance)/(2*sum(!is.na(Variance)))) %>% 
  select(u = Distance, v = Semivariance)

ped_counts_semivarianceall %>% ggplot(aes(x = u, y = v)) +
  geom_point() +
  xlab("Distance (m)") +
  ylab("log10(Semivarance)") +
  scale_y_log10() +
  ggtitle(paste0("Variance vs Distance plot for all sensors for ", year_var))
# ggsave(paste0("Variance vs Distance plot for all sensors for ", year_var, ".pdf"),
       # path = "C:/Users/Hans/The University of Melbourne/Jessica Tong - ENGR90037-38/04 WIP/Variograms/Data Analysis and R Code/Variogram_Plots",
#        width = 10,
#        height = 5)

eyefit_all <- eyefit(ped_counts_semivarianceall)

# Variogram for chosen base sensor, chosen time of day
start_hour <- 20 # 24 hour time
end_hour <- 24 # 24 hour time
sensor_base <- 60 #Starting sensor

ped_counts_semivariance <- ped_counts_variance_dist %>%
  filter(hour(Date_Time_Corrected) >= start_hour,
         hour(Date_Time_Corrected) <= end_hour,
         grepl(paste0("^", sensor_base, "-.*"), Sensor_Pair)
         ) %>%
  group_by(Distance) %>% 
  summarise(Semivariance = sum(Variance)/(2*sum(!is.na(Variance)))) %>% 
  select(u = Distance, v = Semivariance)

ped_counts_semivariance %>% ggplot(aes(x = u, y = v)) +
  geom_point() +
  xlab("Distance (m)") +
  ylab("log10(Semivarance)") +
  scale_y_log10() +
  ggtitle(paste0("Variance vs Distance plot from Sensor ",sensor_base, " in ", year_var, " between ", start_hour, " to ", paste0(end_hour+1)))
#ggsave(paste0("Variance vs Distance plot from Sensor ",sensor_base, " in ", year_var, " between ", start_hour, " to ", paste0(end_hour+1), ".pdf"),
        path = "C:/Users/Hans/The University of Melbourne/Jessica Tong - ENGR90037-38/04 WIP/Variograms/Data Analysis and R Code/Variogram_Plots",
        width = 10,
        height = 5)

# Put into eyefit function (one sensor to everything for the whole year between chosen time period)
eyefit_chosenbaseandtimes <- eyefit(ped_counts_semivariance, silent=TRUE)
# change into variogram class output
ped_counts_semivariance <-  as.list(ped_counts_semivariance)
ped_counts_semivariance[["max.dist"]] = max(ped_counts_semivariance$u)
ped_counts_semivariance[["output.type"]] = "cloud"
ini.vals <- expand.grid(seq(0,3500000, by =10000),seq(100,3000, by = 25)) 
variofit_chosenbaseandtimes <- variofit(ped_counts_semivariance,
                              fix.nugget = TRUE, weights = "equal", 
                              ini.cov.pars = ini.vals, cov.model = "sph")
xv.wls <- xvalid(ped_counts_semivariance, model = variofit_chosenbaseandtimes)
lines(variofit_chosenbaseandtimes)


# Variogram for a specific event
chosen_event_start_var <- "2019-08-23 18:00:00"
chosen_event_end_var <- "2019-08-23 23:00:00"
chosen_event_base_sensor_var <- 60

ped_counts_semivariance_event <- ped_counts_variance_dist %>% 
  filter(Date_Time_Corrected >= ymd_hms(chosen_event_start_var),
         Date_Time_Corrected <= ymd_hms(chosen_event_end_var),
         grepl(paste0("^", chosen_event_base_sensor_var, "-.*"), Sensor_Pair)
  ) %>%
  group_by(Distance) %>% 
  summarise(Semivariance = sum(Variance)/(2*sum(!is.na(Variance)))) %>% 
  select(u = Distance, v = Semivariance)

ped_counts_semivariance_event %>% ggplot(aes(x = u, y = v)) +
  geom_point() +
  xlab("Distance (m)") +
  ylab("log10(Semivarance)") +
  scale_y_log10() +
  ggtitle(paste0("Variance vs Distance plot from Sensor ", chosen_event_base_sensor_var, " for ", chosen_event_start_var, " to ", chosen_event_end_var))
# ggsave(paste0("Chosen Event - Variance vs Distance plot from Sensor ", chosen_event_base_sensor_var, " for ", date(chosen_event_start_var), ".pdf"),
#        path = "C:/Users/Hans/The University of Melbourne/Jessica Tong - ENGR90037-38/04 WIP/Variograms/Data Analysis and R Code/Variogram_Plots",
#        width = 10,
#        height = 5)

# Put into eyefit function (one sensor to everything for the whole year between chosen time period)
#eyefit_chosenevent <- eyefit(ped_counts_semivariance_event)
ped_counts_semivariance_event <-  as.list(ped_counts_semivariance_event)
ped_counts_semivariance_event[["max.dist"]] = max(ped_counts_semivariance_event$u)
ped_counts_semivariance_event[["output.type"]] = "cloud"
ini.vals <- expand.grid(seq(0,40000000, by =100000),seq(100,3000, by = 25)) 
variofit_chosenbaseandtimes_event <- variofit(ped_counts_semivariance_event,
                                        fix.nugget = TRUE, weights = "equal", 
                                        ini.cov.pars = ini.vals, cov.model = "sph")
lines(variofit_chosenbaseandtimes_event)
  # Empirical Analysis: Event Study ###