# PM566-Labs
---
title: "Lab 05 - Data Wrangling"
author: Arianna Hernandez
output: html_document
date: 2021-09-23
embed-resources: true
---

# Learning goals

-   Use the `merge()` function to join two datasets.
-   Deal with missings and impute data.
-   Identify relevant observations using `quantile()`.
-   Practice your GitHub skills.

# Lab description

For this lab we will be, again, dealing with the meteorological dataset downloaded from the NOAA, the `met`. In this case, we will use `data.table` to answer some questions regarding the `met` dataset, while at the same time practice your Git+GitHub skills for this project.

This markdown document should be rendered using `github_document` document.

# Part 1: Setup the Git project and the GitHub repository

1.  Go to your documents (or wherever you are planning to store the data) in your computer, and create a folder for this project, for example, "PM566-labs"

2.  In that folder, save [this template](https://raw.githubusercontent.com/USCbiostats/PM566/master/website/content/assignment/05-lab.Rmd) as "README.Rmd". This will be the markdown file where all the magic will happen.

3.  Go to your GitHub account and create a new repository, hopefully of the same name that this folder has, i.e., "PM566-labs".

4.  Initialize the Git project, add the "README.Rmd" file, and make your first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes, and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir PM566-labs
cd PM566-labs

# Step 2
wget https://raw.githubusercontent.com/USCbiostats/PM566/master/website/content/assignment/05-lab.Rmd 
mv 05-lab.Rmd README.md

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/PM566-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("PM566-labs")
setwd("PM566-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/USCbiostats/PM566/master/website/content/assignment/05-lab.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/PM566-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages if you plan to work with those).

```{r, message=FALSE}
library(data.table)
library(dtplyr)
library(dplyr)
```

2.  Load the met data from https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz, and also the station data. For the later, you can use the code we used during lecture to pre-process the stations data:

```{r}
##download met data
met_url <- "https://github.com/USCbiostats/data-science-data/raw/master/02_met/met_all.gz"
tmp <- "met.gz"
if (!file.exists(tmp)) {
  download.file(
    url      = met_url,
    destfile = tmp,
  )
}
dat <- fread(tmp)
head(dat)
dat[temp <= -17.0, temp := NA]
dat[elev == 9999.0, elev := NA]
dat <- as.data.frame(dat)
dat <- dat %>% 
  select(USAFID, WBAN, year, month, day, 
         hour, min, lat, lon, elev, 
         wind.sp, temp, atm.press)

# Download the stations data
stations <- fread("https://noaa-isd-pds.s3.amazonaws.com/isd-history.csv")
stations[, USAF := as.integer(USAF)]


# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keep unique records
stations <- unique(stations[, c('USAF', 'CTRY', 'STATE')])
# Dropping NAs
stations <- stations[!is.na(stations$'USAF'), ]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
stations <- data.frame(stations)
```

3.  Merge the data as we did during the lecture.

```{r}
dat <- merge(
   x     = dat,      
   y     = stations, 
   by.x  = "USAFID",
   by.y  = "USAF", 
   all.x = TRUE,
   all.y = FALSE
   ) 
```

## Question 1: Representative station for the US

What is the median station in terms of temperature, wind speed, and atmospheric pressure? Look for the three weather stations that best represent continental US using the `quantile()` function. Do these three coincide?

```{r}

q_temp<- quantile(dat$temp, 0.5, na.rm=TRUE)
q_wind<- quantile(dat$wind.sp, 0.5, na.rm=TRUE)
q_atm <- quantile(dat$atm.press, 0.5, na.rm = TRUE)

med_temp <- dat[which.min(abs(dat$temp - q_temp)), "USAFID"]
cat("The median temperature station is USAFID:", med_temp, '\n')
med_wind <- dat[which.min(abs(dat$wind.sp - q_wind)), "USAFID"]
cat("The median wind speed station is USAFID:", med_wind, '\n')
med_atm <- dat[which.min(abs(dat$atm.press - q_atm)), "USAFID"]
cat("The median atmospheric pressure station is USAFID:", med_atm)

```

The median wind speed and atmospheric pressure stations are both in California and they match; however, these do not coincide with the median temperature station located in Michigan.

Knit the document, commit your changes, and push it to GitHub. Don't forget to add `README.md` to the tree, the first time you render it.

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the most representative, the median, station per state. This time, instead of looking at one variable at a time, look at the Euclidean distance. If multiple stations show in the median, select the one located at the lowest latitude.

```{r}
#get temp, wind, and pressure medians arranged by state
state_median <- dat %>% 
    group_by(STATE) %>% 
    summarise(
        temp_avg      = median(temp, na.rm=TRUE),
        wind.sp_avg   = median(wind.sp, na.rm=TRUE),
        atm.press_avg = median(atm.press, na.rm = TRUE)
    ) %>% 
    arrange(STATE)

##add the state averages to dat data frame
station_med <- dat %>%
   left_join(state_median, by = "STATE") %>%
  mutate( ##calculate eucledian distance
    distance = sqrt((temp - temp_avg)^2 + 
                    (wind.sp - wind.sp_avg)^2 + 
                    (atm.press - atm.press_avg)^2)
  )

repstations <- station_med %>%
  group_by(STATE) %>%
  slice_min(distance, with_ties = TRUE) %>%  # Select the station for each state with the smallest distance
  arrange(lat) %>%  # Sort by latitude
  slice(1) %>%  # Select the station with the lowest latitude in case of ties
  select(STATE, USAFID, lat, temp, wind.sp, atm.press, distance)
 print(repstations )
```

Most representative station is in Texas (USAFID = 722536), with the lowest eucledian distance and the lowest latitude at 29.53.

Knit the doc and save it on GitHub.

## Question 3: In the middle?

For each state, identify what is the station that is closest to the mid-point of the state. Combining these with the stations you identified in the previous question, use `leaflet()` to visualize all \~100 points in the same figure, applying different colors for those identified in this question.

```{r}

midpoints <- dat %>%
  group_by(STATE) %>%
  summarise(
    lat_mid = mean(lat, na.rm = TRUE),
    lon_mid = mean(lon, na.rm = TRUE))

#create a frame with every state's station closest to midpoint.
station_by_midpoints <- dat %>%
  left_join(midpoints, by = "STATE") %>% 
  # distance of lat and lon by midpoint
  mutate(distance_2 = sqrt((lat - lat_mid)^2 + (lon - lon_mid)^2)) %>%
  group_by(STATE) %>%
  slice_min(distance_2, with_ties = FALSE) %>%
  select(STATE, USAFID, lat, lon, temp, wind.sp, atm.press, distance_2)

both <- repstations %>% mutate(type = "Most representative station") %>%  
    bind_rows(station_by_midpoints %>% mutate(type = "Closest to midpoint")) 

leaflet(both) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    color = ~ifelse(type == "Most representative station", "lightgreen", "pink"),
    radius = 5,
    label = ~paste("State:", STATE,"\n",
                   "USAFID:", USAFID, "\n",
                   "Temp:", temp,"\n",
                   "Wind Speed:", wind.sp,"\n",
                   "Pressure:", atm.press),
    group = ~type
  ) %>%
  addLegend("bottomright", colors = c("lightgreen", "pink"), 
            labels = c("Most representative station", "Closest to midpoint"),
            title = "Station Type") %>%
  setView(lng = -98.5833, lat = 39.8333, zoom = 4)

```

Knit the doc and save it on GitHub.
