### NOAA Weather Event Data: Analysis of Patterns
## Synopsis

This report explores the 2024 United States NOAA Storm Datasets, joining
files with data pertaining to location, fatality, and event type. The
primary focus on this analysis is to understand the impact of these
severe weather events, particularly in respect to population health,
location, and frequency. In better understanding the patters of severe
weather, the United States government, and it’s people, can best prepare
themselves for future events.

Through analysis, it became evident that excessive heat was the most
harmful event in terms of fatalities, with a total of 4042 recorded in
2024. In addition, floods and thunderstorm winds were th most frequent
events across states. In breaking the data up by month, it can be noted
that various events peak across different months. Lastly, it was learned
that the summer season, especially in May and July, weather events occur
at a higher frequency.

This report provides valuable insights for government officials to
prioritize resources for disaster preparedness and response. In order to
best ensure the public’s safety, a coordinated response that has learned
from past data and patterns is critical. The ability to plan ahead, by
knowing which events are more likely in which months, could save
precious resources.

## Data Processing

    # Load necessary libraries
    library(dplyr)
    library(readr)

    # Define the folder path
    folder_path <- "C:/Users/albin/OneDrive/Canisius/DAT 511"


    # Define the file paths for the unzipped CSV files
    details_file <- file.path(folder_path, "StormEvents_Details.csv")
    fatalities_file <- file.path(folder_path, "StormEvents_Fatalities.csv")

    locations_file <- file.path(folder_path, "StormEvents_Locations.csv")

    file.exists("C:/Users/albin/OneDrive/Canisius/DAT 511/Events_Locations.csv")

    ## [1] FALSE

    # Load the CSV files into R
    details <- read_csv(details_file)

    ## Warning: One or more parsing issues, call `problems()` on your data frame for
    ## details, e.g.:
    ##   dat <- vroom(...)
    ##   problems(dat)

    ## Rows: 70196 Columns: 51
    ## ── Column specification ───────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (26): STATE, MONTH_NAME, EVENT_TYPE, CZ_TYPE, CZ_NAME, WFO, BEG...
    ## dbl (24): BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME, END_YEARMONTH, EN...
    ## lgl  (1): CATEGORY
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    fatalities <- read_csv(fatalities_file)

    ## Rows: 1047 Columns: 11
    ## ── Column specification ───────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): FATALITY_TYPE, FATALITY_DATE, FATALITY_SEX, FATALITY_LOCATION
    ## dbl (7): FAT_YEARMONTH, FAT_DAY, FAT_TIME, FATALITY_ID, EVENT_ID, F...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    locations <- read_csv(locations_file)

    ## Rows: 48112 Columns: 11
    ## ── Column specification ───────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): AZIMUTH, LOCATION
    ## dbl (9): YEARMONTH, EPISODE_ID, EVENT_ID, LOCATION_INDEX, RANGE, LA...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    # Join the datasets by EVENT_ID
    joined_data <- details %>%
      left_join(locations, by = "EVENT_ID") %>%
      left_join(fatalities, by = "EVENT_ID")

    ## Warning in left_join(., fatalities, by = "EVENT_ID"): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 830 of `x` matches multiple rows in `y`.
    ## ℹ Row 441 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

    # Save the joined data to a new CSV file
    output_file <- file.path(folder_path, "StormEvents_joined_data.csv")
    write_csv(joined_data, output_file)

    # Inform the user
    message("Joined data saved to: ", output_file)

    ## Joined data saved to: C:/Users/albin/OneDrive/Canisius/DAT 511/StormEvents_joined_data.csv

    # Optional: View the first few rows of the joined data
    print(head(joined_data))

## Results

    health_impact <- joined_data %>%
      group_by(EVENT_TYPE) %>%
      summarise(
        total_fatalities = sum(DEATHS_DIRECT, DEATHS_INDIRECT, na.rm = TRUE),
        total_injuries = sum(INJURIES_DIRECT, INJURIES_INDIRECT, na.rm = TRUE),
        total_harmed = total_fatalities + total_injuries
      ) %>%
      arrange(desc(total_harmed))

    # Show top 10 most harmful event types
    head(health_impact, 10)

# Excessive heat leads total fatalies and total harmed, with tornado and flash floods with the second and thirdest largest totals for individuals harmed.

    event_counts_by_state <- joined_data %>%
      group_by(STATE, EVENT_TYPE) %>%
      summarise(event_count = n(), .groups = 'drop') %>%
      arrange(desc(event_count))

    # View top 10 most frequent event types by state
    head(event_counts_by_state, 10)

# Listed above are the top ten most frequent combinations of state and event type. One key takeaway is that Texas experienced the most flash floods and hail storms at 1654 and 1613, respectively.

    library(dplyr)
    library(ggplot2)

    # Summarize the number of events by MONTH_NAME
    events_by_month <- joined_data %>%
      group_by(MONTH_NAME) %>%
      summarise(event_count = n(), .groups = "drop")

    # Ensure correct month ordering
    month_levels <- c("January", "February", "March", "April", "May", "June", 
                      "July", "August", "September", "October", "November", "December")
    events_by_month$MONTH_NAME <- factor(events_by_month$MONTH_NAME, levels = month_levels)

    # Bar graph of events by month
    ggplot(events_by_month, aes(x = MONTH_NAME, y = event_count, fill = MONTH_NAME)) +
      geom_bar(stat = "identity") +
      labs(title = "Number of Events by Month", x = "Month", y = "Number of Events") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

![](NOAA-Final_files/figure-markdown_strict/unnamed-chunk-6-1.png)

# The graph above represents each month’s total events. As shown, May experienced the greatest number, followed by July. In general, a peak can be seen during the summer season.

    # Load necessary libraries
    library(dplyr)

    # Group by month and event type, and calculate the count of each event type
    event_month_summary <- joined_data %>%
      group_by(MONTH_NAME, EVENT_TYPE) %>%
      summarise(event_count = n(), .groups = 'drop') %>%
      arrange(MONTH_NAME, desc(event_count))

    # For each month, select the event type with the highest count
    most_common_event_per_month <- event_month_summary %>%
      group_by(MONTH_NAME) %>%
      slice_max(event_count, n = 1) %>%
      select(MONTH_NAME, EVENT_TYPE, event_count)

    # Print the most common event for each month
    print(most_common_event_per_month)

# The figure above shows the event that occurred most commonly, per month. As one example, in the month of April, there were 1832 flash floods across the United States. This analysis is useful in understanding which types of events are most likely to occur, given the month.

    # Most common event -top 10
    # Load necessary libraries
    library(dplyr)

    # Count the occurrences of each event type
    event_type_summary <- joined_data %>%
      group_by(EVENT_TYPE) %>%
      summarise(event_count = n(), .groups = 'drop') %>%
      arrange(desc(event_count))

    # Get the top 10 most common event types
    top_10_event_types <- event_type_summary %>%
      slice_head(n = 10)

    # Print the top 10 most common event types
    print(top_10_event_types)

# The table above highlights which events happen most frequently across the United States. Using this data, government officials could better prepare resources, prioritizing events that occur more frequently and that require more consistent risk mitigation.
