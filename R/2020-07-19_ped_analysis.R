# City of Melbourne Pedestrian Count Data Event Analysis
# Jessica Tong

library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggpubr)
library(xtable)
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
  # only when there is more than 2 duplicates (so the duplicate which exists because of daylight savings is not altered)
  filter(n()>2) %>% 
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

# Data Description: (no need to rerun - summary for report) ####
# Reorder levels for day of week (for plotting)
ped_counts_and_locations$Day <- factor(ped_counts_and_locations$Day,
                                       levels = c("Monday",
                                                  "Tuesday",
                                                  "Wednesday",
                                                  "Thursday",
                                                  "Friday",
                                                  "Saturday",
                                                  "Sunday"))


# Plot for all years (investigate trends)
ped_counts_and_locations %>% 
  drop_na(Hourly_Counts) %>% 
  group_by(Day, Time_Corrected, Year) %>% 
  summarise(Mean = mean(Hourly_Counts)) %>% 
  ggplot(aes(x = Time_Corrected, y = Mean, colour = Day)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 23.5), breaks = seq(0,24,4)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  facet_wrap(vars(Year)) +
  labs(x = "Time of Day (Hour)",
       y = "Average Pedestrian Count",
       colour = "Day of Week"
  ) +
  theme_void() +
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15, angle = 90),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
  ) +
  ggtitle(paste0("Average Hourly Pedestrian Counts for All Sensors by Year")
          ) #+
  # ggsave(paste0("daily_summary_all.pdf"),
  #        path = "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Data_summary",
  #        width = 10,
  #        height = 5
  # )

# Summary for 2019 (for report)
summary_year <- 2019

# Daily Average Plot
ped_counts_and_locations %>% 
  drop_na(Hourly_Counts) %>% 
  filter(year(Date_Time_Corrected) == summary_year) %>% 
  group_by(Day, Time_Corrected) %>% 
  summarise(Mean = mean(Hourly_Counts)) %>% 
  ggplot(aes(x = Time_Corrected, y = Mean, colour = Day)) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 23), breaks = seq(0,24,1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA), breaks = seq(0,2000,250)) +
  labs(x = "Time of Day (Hour)",
       y = "Average Pedestrian Count",
       colour = paste0("Day of Week - ", summary_year)
  ) +
  theme_classic() +
  theme(axis.title = element_text(size=15),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.position = c(0.93,0.78),
        legend.background = element_rect(fill = "transparent")
        ) #+
  # ggsave(paste0("daily_summary",summary_year,".pdf"),
  #      path = "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Data_summary",
  #      width = 10,
  #      height = 5
  #      )

# Table showing no. counts, mean, stdDev, min, max
data_description <- ped_counts_and_locations %>%
  filter(year(Date_Time_Corrected) == summary_year) %>%
  drop_na(Hourly_Counts) %>% 
  group_by(Day) %>%
  summarise(N = sum(!is.na(Hourly_Counts)),
            Mean = mean(Hourly_Counts),
            StdDev = sd(Hourly_Counts),
            Min = min(Hourly_Counts),
            Max = max(Hourly_Counts)
            )

data_description2 <- ped_counts_and_locations %>%
  filter(year(Date_Time_Corrected) == summary_year) %>%
  drop_na(Hourly_Counts) %>% 
  summarise(Day = "All",
            N = sum(!is.na(Hourly_Counts)),
            Mean = mean(Hourly_Counts),
            StdDev = sd(Hourly_Counts),
            Min = min(Hourly_Counts),
            Max = max(Hourly_Counts)
  )

data_description <- rbind(data_description, data_description2)

# Print data description table for use in latex
# data_description %>% 
#   xtable(caption= paste0("Summary statistics across all sensors of pedestrian count data in ", summary_year),
#                             label = "tab:data_summary") %>%
#   print(file= paste0("C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Data_summary/data_summary", summary_year, ".tex"),
#         table.placement = "H",
#         caption.placement="top",
#         booktabs = TRUE,
#         include.rownames = FALSE)

# About Sensors
data_description_sensors <- ped_counts_and_locations %>%
  filter(year(Date_Time_Corrected) == summary_year) %>%
  drop_na(Hourly_Counts) %>% 
  group_by(Day, Sensor_ID) %>%
  summarise(Sum = sum(Hourly_Counts)) %>% 
  ggplot(aes(x = Day, y = Sum, colour = as.factor(Sensor_ID), label = Sensor_ID)) +
  geom_label() +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(x = "Day of Week",
       y = "Total Daily Pedestrian Count",
       colour = paste0("Sensor - ", summary_year)
  ) +
  theme_classic() +
  theme(axis.title = element_text(size=15),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.background = element_rect(fill = "transparent"),
        legend.position = "none"
  ) #+
  # ggsave(paste0("sensor_summary",summary_year,".pdf"),
  #    path = "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Data_summary",
  #    width = 10,
  #    height = 5
  #    )


# Mother Wavelet Choice: (no need to rerun this section) ####
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
wavelet_type <- "haar" # wavelet type
wavelet_level <- 5 #level of decomposition (5 = 32hrs span)

# Compute the maximal overlap discrete wavelet transform for the chosen year+months and mother wavelet+levels
wavelet_ped_counts <- complete_ped_counts_and_locations %>%
  # Select on the Sensor ID, Date/Time and Hourly Counts columns
  select(Sensor_ID, Hourly_Counts, Complete_Date_Time,Complete_Hourly_Counts) %>%
  group_by(Sensor_ID) %>%
  # modwt transform by the wavelet type and level of decomposition chosen for all chosen Year+months
  summarise(
    data = list(tibble(Hourly_Counts, Complete_Date_Time, Complete_Hourly_Counts)),
    mod = list(
      as_tibble(
        unclass(
          modwt(Complete_Hourly_Counts, wf = unique(wavelet_type), n.levels = unique(wavelet_level)))
      )
    )
  ) %>%
  unnest(c(data, mod)) %>% 
  mutate(
    s4 = s5 + d5,
    s3 = s4 + d4,
    s2 = s3 + d3,
    s1 = s2 + d2)

# Threshold and find outliers for each level of detail
wavelet_outliers <- wavelet_ped_counts %>%
  select(-c(Complete_Hourly_Counts, Hourly_Counts, matches("^s\\d+$"))) %>% 
  pivot_longer(-c(Sensor_ID, Complete_Date_Time), values_drop_na = TRUE) %>% 
  group_by(Sensor_ID, name) %>% 
  mutate(d_avg = mean(value),
         # 99.5% of data should be within 2.807 standard deviations
         d_threshold = 2.807*sd(value),
         d_flag = as.integer(value > (d_avg + d_threshold) | value < (d_avg - d_threshold))
  ) %>%
  ungroup() %>% 
  pivot_wider(c(Sensor_ID,Complete_Date_Time),
              names_from = name,
              values_from = d_flag) %>% 
  mutate(outlier_d1 = d1,
         outlier_d2 = d2,
         outlier_d3 = d3,
         outlier_d4 = d4,
         outlier_d5 = d5
  ) %>%
  select(-c(starts_with("d")))

# Sensors where outliers have been identified, and the outlier temporal span
# Join outliers to original dataset (remove any outliers that were identified at sensors which had missing values)
ped_counts_and_locations_outliers <- ped_counts_and_locations %>% 
  left_join(wavelet_outliers, by = (c("Sensor_ID", "Date_Time_Corrected" = "Complete_Date_Time"))) %>% 
  select(-Date_Time) %>% 
  select(Sensor_ID, Outlier_Date_Time_Start = Date_Time_Corrected, starts_with("outlier")) %>% 
  pivot_longer(-c(Sensor_ID, Outlier_Date_Time_Start), names_to = "Level", values_to = "Outlier") %>% 
  # Only keep sensors and times where an outlier has been identified
  filter(Outlier == 1) %>%
  # Find the duration of the outlier event, outlier identified is the peak/drop in the haar wavlet
  mutate(Outlier_Date_Time_End = Outlier_Date_Time_Start + hours((2^as.numeric(str_sub(Level, -1)))),
         # Create Wavelet_Outlier as level of decomposition outlier is identified at
         Wavelet_Outlier = as.numeric(str_sub(Level, -1))
  )

# Create complete hour-by-hour outlier list with ped counts and locations
complete_ped_loc_outliers <- ped_counts_and_locations_outliers %>%
  filter(date(Outlier_Date_Time_End) >= date("2018-12-31") & date(Outlier_Date_Time_Start) <= date("2020-01-01")) %>% 
  # Create new dataframe with a complete Date_Time list for all sensors
  rowwise() %>% 
  mutate(Complete_Date_Time = list(seq(Outlier_Date_Time_Start, Outlier_Date_Time_End, by= "1 hour"))) %>% 
  unnest(Complete_Date_Time)

# Clear up workspace - only keep:
# 1. ped_counts_and_locations
# 2. complete_ped_counts_and_locations
# 3. complete_ped_loc_outlier
# 4. wavelet_ped_counts
rm(
  ped_counts_and_locations_outliers,
  wavelet_outliers,
  wavelet_level,
  wavelet_type
)


# Anomaly/Event Exploratory and Temporal Span Analysis ####
# Read in and format 2019 Events List
events_2019 <- read_csv("events_2019.csv") %>% 
  mutate(Date = dmy(Date),
         Start_Date = dmy(Start_Date),
         End_Date = dmy(End_Date),
         Event_Date_Time_Start = parse_date_time(paste(Start_Date, Start_Hour), "ymdHMS"),
         Event_Date_Time_End = parse_date_time(paste(End_Date, End_Hour), "ymdHMS")
  ) %>%
  select(-c(Start_Date, End_Date, Start_Hour, End_Hour), Estimated_Dur = "Duration (estimated, hours)")

# Check number of unique events in event_2019 dataset
unique(events_2019[c("Event","Event_Date_Time_Start")])

# Create Event List with complete hour-by-hour Date_Time values
complete_events_2019 <- events_2019 %>%
  group_by(Event, Event_Date_Time_Start) %>% 
  # Create new dataframe with a complete Date_Time list for all sensors
  rowwise() %>% 
  mutate(Complete_Date_Time = list(seq(Event_Date_Time_Start, Event_Date_Time_End, by= "1 hour"))) %>% 
  unnest(Complete_Date_Time)

# Data Driven Identification: Join Events list to Identified Outliers and Identify when the Event Times/Outliers Overlap
outliers_and_events_2019 <- complete_ped_loc_outliers %>%
  # Keep all outliers, join events on ANY hour where the outlier temporal span and event match
  left_join(complete_events_2019, by = "Complete_Date_Time") %>% 
  group_by(Sensor_ID, Outlier_Date_Time_Start) %>%
  mutate(Outlier_Event_Flag = as.integer(any(!is.na(Event)))) %>%
  # Add sensor and location information
  left_join(ped_counts_and_locations, by = (c("Sensor_ID", "Complete_Date_Time" = "Date_Time_Corrected"))) %>%
  select(-c("Date_Time","Hourly_Counts")) %>%
  # Ignore hour-by-hour duplicates
  group_by(Sensor_ID, Event, Outlier_Date_Time_Start) %>% 
  slice(1) %>%
  select(-c("Complete_Date_Time"))
# Save dataframe
# write.csv(outliers_and_events_2019, "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Event_exploratory/outliers_and_events_2019.csv", row.names = FALSE)

# Explore Events which have been identified by outliers
explore_outliers_and_events_2019 <- outliers_and_events_2019 %>%
  # Change or remove filter as necessary
  filter(
    # Wavelet_Outlier %in% c(1,2,3,4,5),
    Outlier_Event_Flag %in% c(1,0),
  ) %>% 
  group_by(Event, Event_Date_Time_Start) %>% 
  slice(1) %>%
  subset(!is.na(Event))

# Event Driven Identification and Temporal Span: Join Identified Outliers to Events list identify which events are picked up by outliers
events_identified_2019 <- complete_events_2019 %>%
  # Keep all events, join outliers on ANY hour where the event and outlier temporal span match
  left_join(complete_ped_loc_outliers, by = "Complete_Date_Time") %>% 
  # Add sensor and location information
  left_join(ped_counts_and_locations, by = (c("Sensor_ID", "Complete_Date_Time" = "Date_Time_Corrected"))) %>%
  select(-Date_Time) %>%
  # Ignore hour-by-hour duplicates and keep only events overlap with an outlier period
  group_by(Sensor_ID, Event, Event_Date_Time_Start) %>% 
  slice(1) %>%
  select(c("Event","Event_Date_Time_Start","Event_Date_Time_End",
           "Sensor_ID","latitude","longitude",
           "Outlier_Date_Time_Start","Outlier_Date_Time_End",
           "Outlier","Wavelet_Outlier"
  )) %>% 
  drop_na(Outlier) %>% 
  filter(Wavelet_Outlier != 4, Wavelet_Outlier != 5) %>%
  # Find Temporal Span of Event at each sensor (min Outlier_Date_Time_Start and max Outlier_Date_Time_Start) 
  group_by(Sensor_ID, Event, Event_Date_Time_Start) %>% 
  mutate(Temporal_Start = min(Outlier_Date_Time_Start),
         Temporal_End = max(Outlier_Date_Time_End),
         Temporal_Span = as.double(difftime(Temporal_End, Temporal_Start), unit = "hours")
  ) %>% 
  select(-c("Outlier_Date_Time_Start","Outlier_Date_Time_End",
            "Wavelet_Outlier")
  ) %>% 
  # Join back to original events list
  right_join(events_2019, by = (c("Event","Event_Date_Time_Start","Event_Date_Time_End"))) %>% 
  mutate(Outlier_Event_Flag = as.integer(!is.na(Outlier)))
# Save dataframe
# write.csv(events_identified_2019, "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Event_Exploratory/events_identified_2019.csv", row.names = FALSE)

# Explore Events which have NOT been identified by outliers
explore_events_identified_2019 <- events_identified_2019 %>%
  filter(Outlier_Event_Flag %in% c(0)) %>%
  group_by(Event, Event_Date_Time_Start) %>% 
  slice(1)


# Temporal Analysis ####
# Temporal Span of each event at each sensor where an outlier has been identified
events_identified_2019_checked <- read_csv("events_identified_2019_checked.csv") %>% 
  select(Event,	Event_Date_Time_Start,	Event_Date_Time_End,	Sensor_ID, Keep) %>% 
  mutate(Event_Date_Time_Start = parse_date_time(Event_Date_Time_Start,"dmYHMS", truncated = 1),
         Event_Date_Time_End = parse_date_time(Event_Date_Time_End,"dmYHMS",truncated = 1),
  ) %>% 
  right_join(events_identified_2019) %>% 
  # Remove events and outliers that have manually been identified as incorrrect
  filter(Keep == 1) %>% 
  drop_na(Outlier) %>% 
  # Find overall Event Temporal Span (min Outlier_Date_Time_Start and max Outlier_Date_Time_Start) 
  group_by(Event, Event_Date_Time_Start) %>% 
  mutate(Event_Temporal_Start = min(Temporal_Start, Event_Date_Time_Start),
         Event_Temporal_End = max(Temporal_End, Event_Date_Time_End),
         Event_Temporal_Span = as.double(difftime(Event_Temporal_End, Event_Temporal_Start), unit = "hours")
  )

# List of (checked) sensors where event has been identified (for use in before-and-after)
temporal_sensors <- events_identified_2019_checked %>% 
  select(Event, Event_Date_Time_Start, Event_Date_Time_End,
         Sensor_ID, Temporal_Start, Temporal_End, Temporal_Span, Outlier,
         Event_Temporal_Start, Event_Temporal_End, Event_Temporal_Span)
# NEED TO ADD ON SENSORS THAT ARE IDENTIFIED AT LEVEL 4/5 but not used for temporal span calc

# Temporal Span of each event (ignoring sensors)
temporal <- temporal_sensors %>%
  group_by(Event, Event_Date_Time_Start) %>% 
  slice(1) %>% 
  select(-c("Sensor_ID", "Temporal_Start", "Temporal_End", "Temporal_Span", "Outlier"))

# Create complete hour-by-hour event list based on temporal span for overall event
# for use in before-and-after
complete_temporal <- temporal %>% 
  group_by(Event, Event_Date_Time_Start) %>% 
  # Create new dataframe with a complete Date_Time list for all events based on Temporal Span
  rowwise() %>%
  mutate(Complete_Event_Date_Time = list(seq(Event_Temporal_Start, Event_Temporal_End, by= "1 hour"))) %>% 
  unnest(Complete_Event_Date_Time)

# Before-and-After Analysis ####
# Use smoothed signal from wavelets level 1 as most unique events in 2019 have been identified at this level of decomposition
# Use complete dataset to calculate rolling means for each sensor at each date-time
avg4w <- seq(7*24, 7*24*4, by = 7*24)

# Create dataframe of differences for every Sensor_ID and Date-Time
differences <- wavelet_ped_counts %>%
  arrange(Sensor_ID, Complete_Date_Time) %>%
  group_by(Sensor_ID,date(Complete_Date_Time)) %>% 
  mutate(Daily_Count = sum(Hourly_Counts, na.rm = TRUE)) %>% 
  group_by(Sensor_ID) %>% 
  # Rolling average for chosen time periods for every Sensor_ID
  mutate(Avg_4w = rowMeans(bind_cols(data.table::shift(s1, avg4w))),
         Avg_4w_Daily = rowMeans(bind_cols(data.table::shift(Daily_Count, avg4w))),
         W1 = data.table::shift(s1, 7*24),
         W2 = data.table::shift(s1, 7*24*2),
         W3 = data.table::shift(s1, 7*24*3),
         W4 = data.table::shift(s1, 7*24*4),
         StdDev = sqrt(rowSums(cbind((W1-Avg_4w)^2, (W2-Avg_4w)^2, (W3-Avg_4w)^2, (W4-Avg_4w)^2), na.rm = TRUE)/(rowSums(cbind(!is.na(W1), !is.na(W2), !is.na(W3), !is.na(W4))))),
         Difference = Hourly_Counts - Avg_4w,
         Std_Difference = (Hourly_Counts - Avg_4w)/StdDev,
         Percent_Difference = Hourly_Counts/Daily_Count - Avg_4w/Avg_4w_Daily
         )

# Create full differences dataset consisting of:
# 1. Before and after analysis values (differences)
# 2. Temporal span of Event (after manual checks)
# 3. List of sensors identified as outliers during each event #NOTE JT REPLACE THIS WITH SENSOR AT LEVELS 1-5, not just 1-3
before_and_after <- differences %>%
  drop_na(Hourly_Counts) %>% 
  mutate(Date_Time = Complete_Date_Time) %>% 
  left_join(complete_temporal, by = c("Date_Time" = "Complete_Event_Date_Time")) %>% 
  left_join(temporal_sensors)

# Specific Event Magnitude of Difference Plots and Summaries
# Choose Event and Event_Date_Time_Start input from data frame: temporal
chosen_event_name <- "White Night Melbourne Night 3"
chosen_event_start <- "2019-08-24 19:00:00"
chosen_event <- temporal %>% filter(Event == chosen_event_name, Event_Date_Time_Start == ymd_hms(chosen_event_start))
event_before_and_after <- before_and_after %>% 
  filter((Complete_Date_Time) >= (chosen_event$Event_Temporal_Start) & (Complete_Date_Time) <= (chosen_event$Event_Temporal_End),
         Event == chosen_event_name)

# Difference Plot
mag_difference <- event_before_and_after %>% 
  ggplot(aes(x = Date_Time, y = Difference)) +
  geom_line(aes(colour = as.factor(Sensor_ID))) +
  gghighlight::gghighlight(Outlier == 1) +
  # geom_line(aes(y = Avg, linetype = "mean")) +
  # scale_linetype_manual(values=c("dashed"))+
  labs(x = "Time",
       y = "Difference",
       colour = "Sensor ID (wavlet-identified outlier)"
       # linetype = "All Sensors"
       ) +
  ggtitle(paste0("Magnitude Comparison"))

# Standard Deviation of Difference Plot
std_difference <- event_before_and_after %>% 
  ggplot(aes(x = Date_Time, y = Std_Difference)) +
  geom_line(aes(colour = as.factor(Sensor_ID))) +
  gghighlight::gghighlight(Outlier == 1) +
  labs(x = "Time",
       y = "Difference/Std Dev",
       colour = "Sensor ID (wavlet-identified outlier)"
  ) +
  ggtitle(paste0("Standard Deviation Comparison"))


# Percentage Difference based on Daily Total Plot
percent_difference <- event_before_and_after %>% 
  ggplot(aes(x = Date_Time, y = Percent_Difference)) +
  geom_line(aes(colour = as.factor(Sensor_ID))) +
  gghighlight::gghighlight(Outlier == 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Time",
       y = "Difference in % of Daily Total",
       colour = "Sensor ID (wavlet-identified outlier)"
  ) +
  ggtitle(paste0("Percentage of Daily Total Comparison"))

# Overall event plot
before_and_after_plots <- ggarrange(ggarrange(mag_difference, std_difference, ncol = 2,
                                 common.legend = TRUE, legend = "bottom"),
                       percent_difference,
                       nrow = 2, legend = "none") %>% 
  annotate_figure(paste0("Actual Pedestrian Counts Compared to 4 Week Smoothed Average: ", chosen_event_name), fig.lab.size = 50) #+
  # ggsave(paste0(chosen_event_name,".pdf"),
  #      path = "C:/Users/jessi/Google Drive/Work/2020/ENGR90037-8 Engineering Capstone Project/Research Project/Capstone Dataset/Magnitude_plots",
  #      width = 10,
  #      height = 10)

# Spatial Statistical Analysis ####
# Join all sensor information (locations, pairs, distances, angles, anisotropic analysis (Y/N))
# Sensors for anisotropic analysis
sensor_anisotropy <- read_csv("sensor_anisotropy.csv") %>% 
  select(Sensor_ID = sensor_id, Anisotropy = "Sensor locations (anisotropic analysis)_anisotropy") %>% 
  mutate(Anisotropy = coalesce(Anisotropy,0))

# Sensor locations
location_filename <- list.files(pattern = "^Ped.*Locations")
sensor_locations <- read_csv(location_filename)

# Sensor network distances file to join to network distances as categories are not the same as Sensor_IDs
sensor_network_category <- read_csv("sensor_network_category.csv")

# Sensor straightline distances
sensor_straight_distances <- read_csv("sensor_straightline_distance.csv") %>% 
  mutate(Sensor_Pair = paste0(InputID,"-",TargetID),
         Straight_Distance = Distance) %>% 
  select(Sensor_Pair, Straight_Distance)

# Sensor pairs and network distances and angle between sensor pair
sensor_pairs <- read_csv("sensor_network_distance.csv") %>%
  left_join(sensor_network_category, by = c("from_cat" = "Cat")) %>% 
  mutate(Start = Sensor_ID) %>%
  select(-Sensor_ID) %>% 
  left_join(sensor_network_category, by = c("to_cat" = "Cat")) %>% 
  mutate(End = Sensor_ID,
         Sensor_Pair = paste0(Start,"-",End)) %>%
  group_by(Start, End, Sensor_Pair) %>%
  summarise(Distance = sum(length)) %>% 
  left_join(sensor_anisotropy, by = c("Start" = "Sensor_ID")) %>% 
  left_join(sensor_locations, by = c("Start" = "sensor_id")) %>% 
  select(Sensor_Pair, Distance, Anisotropy,
         Start,
         start_point_x = longitude,
         start_point_y = latitude,
         End
  ) %>% 
  left_join(sensor_locations, by = c("End" = "sensor_id")) %>% 
  select(Sensor_Pair, Distance, Anisotropy,
         Start,
         start_point_x,
         start_point_y,
         End,
         end_point_x = longitude,
         end_point_y = latitude,
  ) %>% 
  mutate(Theta = case_when(
    (start_point_x > end_point_x) ~ 270 + atan((end_point_y - start_point_y)/(end_point_x - start_point_x)) * 180/pi,
    TRUE ~ 90 - atan((end_point_y - start_point_y)/(end_point_x - start_point_x)) * 180/pi)
         ) %>%
  left_join(sensor_straight_distances) %>%
  ungroup() %>% 
  select(Start, End, Sensor_Pair, Straight_Distance, Distance, Theta, Anisotropy)

# Clean up section, keep sensor_pairs dataframe
rm(location_filename,
   sensor_locations,
   sensor_network_category,
   sensor_straight_distances)

# Choose variables for variogram analysis
var_chosen_event <- "White Night Melbourne Night 1" # Event and Event_Date_Time_Start input from temporal sheet
var_chosen_event_start <- "2019-08-22 19:00:00" # Event and Event_Date_Time_Start input from temporal sheet
var_input <- "Hourly_Counts" # Choose inputs for variance (Hourly_Counts or Avg_4w)
# STILL NEED TO ADD FILTER FOR DIRECTIONAL VARIOGRAM

# Loop parameters/inputs
# Event info
var_event <- temporal %>% filter(Event == var_chosen_event, Event_Date_Time_Start == ymd_hms(var_chosen_event_start))
# Sensor distances
sensor_distances <- sensor_pairs %>%  select(Sensor_Pair, Distance)
# Active sensors during the chosen event
var_active_sensors <- differences %>%
  filter((Complete_Date_Time) >= (var_event$Event_Temporal_Start) & (Complete_Date_Time) <= (var_event$Event_Temporal_End)) %>% 
  drop_na(Hourly_Counts) %>% group_by(Sensor_ID) %>% slice(1) %>% select(Sensor_ID)
# variogram model type to run
var_model <- c("sph", "exp")
# For loop: row counter and empty dataframe to save results
row_counter = 1
variogram_results <- tibble(Sensor_ID = NA_real_, 
                            Input = NA_character_, 
                            Event = NA_character_, 
                            Event_Start = NA_POSIXct_,
                            Model = NA_character_, 
                            Range = NA_real_, 
                            LOSS = NA_real_)

# Variogram calculation and model
# Filter by event time and pivot data so that you only have unique Date_Time_Corrected values in each row
ped_counts_variance <- differences %>%
  filter((Complete_Date_Time) >= (var_event$Event_Temporal_Start) & (Complete_Date_Time) <= (var_event$Event_Temporal_End)) %>% 
  drop_na(Hourly_Counts) %>%
  select(Date_Time = Complete_Date_Time, Sensor_ID, !!var_input) %>%
  pivot_wider(names_from = Sensor_ID,
              values_from = !!var_input)

# Check that there are no duplicated Date_Time_Corrected Rows
sum(duplicated(ped_counts_variance$Date_Time))

# Create new columns with the variance between each sensor pair in each time period
for (i in var_active_sensors$Sensor_ID){
  for (j in var_active_sensors$Sensor_ID){
    ped_counts_variance[[paste0(i,"-",j)]] <- (ped_counts_variance[[paste0(i)]] - ped_counts_variance[[paste0(j)]])^2
  }
}

# Loop for calculating variance for each "base" sensor
for(k in var_active_sensors$Sensor_ID){
  # Join Sensor Distances to Sensor Variances by Sensor_Pair
  ped_counts_semivariance_event <- ped_counts_variance %>% 
    select(Date_Time, contains("-")) %>% 
    pivot_longer(-(Date_Time), names_to = "Sensor_Pair", values_to = "Variance", values_drop_na = TRUE) %>%
    left_join(sensor_distances, by = "Sensor_Pair") %>%
    drop_na() %>% 
    filter(grepl(paste0("^", k, "-.*"), Sensor_Pair)) %>%
    group_by(Distance) %>% 
    summarise(Semivariance = sum(Variance)/(2*sum(!is.na(Variance)))) %>% 
    select(u = Distance, v = Semivariance)
  
  # Change into variogram class output for use with geoR package functions
  ped_counts_semivariance_event <-  as.list(ped_counts_semivariance_event)
  ped_counts_semivariance_event[["max.dist"]] = max(ped_counts_semivariance_event$u)
  ped_counts_semivariance_event[["output.type"]] = "cloud"
  
  # Loop for variogram model to fit on results
  for(m in var_model){  
    ini.vals <- expand.grid(seq(0, 100000000, by = 100000), seq(0, 3500, by = 100)) 
    variofit_chosenevent <- variofit(ped_counts_semivariance_event,
                                                  fix.nugget = TRUE, weights = "equal", 
                                                  ini.cov.pars = ini.vals, cov.model = m)
    # Save variogram results
    variogram_results[row_counter, "Sensor_ID"] <- k
    variogram_results[row_counter, "Input"] <- var_input
    variogram_results[row_counter, "Event"] <- var_event$Event
    variogram_results[row_counter, "Event_Start"] <- var_event$Event_Date_Time_Start
    variogram_results[row_counter, "Model"]<- variofit_chosenevent$cov.model
    variogram_results[row_counter, "Range"] <- variofit_chosenevent$practicalRange
    variogram_results[row_counter, "LOSS"] <- variofit_chosenevent$value
  
    row_counter = row_counter + 1
    
  }
}