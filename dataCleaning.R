
# read matches
match.files <- list.files(path="C:\\Users\\suhai\\Desktop\\All Folders\\R Project\\FootballAnalyticsCourse\\open-data-master\\data\\matches\\11",
                          full.names = TRUE,recursive = TRUE)

# looping over all matches and bind them into one data frame
matches <- data.frame()
for(i in match.files){
        matches  <- bind_rows(matches, jsonlite::fromJSON(txt = i, flatten = TRUE))
}

# eliminating unused data
matches <- matches %>% select(match_id, season.season_name)

# writing the data frame as csv file
write.csv(matches, "data/matches.csv", row.names = F) 

# function for reading one season events
readSeasonEvents <- function(name) {
        
        path = "C:\\Users\\suhai\\Desktop\\All Folders\\R Project\\FootballAnalyticsCourse\\open-data-master\\data\\events"
        
        # find the match id to read it from the path
        season.ids <- matches[matches$season.season_name == name,]$match_id
        
        # looping over the matches and bind them into one data frame
        events <- data.frame()
        for (i in season.ids) {
                file = paste(path,"\\", i, ".json", sep = "")
                events <- bind_rows(events, jsonlite::fromJSON(txt = file, flatten = TRUE))
        }
        events
}

# function for transforming locations from list to two variables x & y
# this function is from Statsbomb repository on github
cleanlocations <- function(dataframe) {
        if("carry.end_location" %in% names(dataframe) == TRUE){
                dataframe <- dataframe %>%
                        mutate(location.x = (map(location, 1)),
                               location.y = (map(location, 2)),
                               carry.end_location.x = (map(carry.end_location, 1)),
                               carry.end_location.y = (map(carry.end_location, 2)),
                               pass.end_location.x = (map(pass.end_location, 1)),
                               pass.end_location.y = (map(pass.end_location, 2)),
                               shot.end_location.x = (map(shot.end_location, 1)),
                               shot.end_location.y = (map(shot.end_location, 2)),
                               shot.end_location.z = (map(shot.end_location, 3)),
                               shot_impact_height = (map(location, 3)))
                dataframe <- dataframe %>%
                        mutate(location.x = as.numeric(ifelse(location.x == "NULL", NA, location.x)),
                               location.y = as.numeric(ifelse(location.y == "NULL", NA, location.y)),
                               carry.end_location.x = as.numeric(ifelse(carry.end_location.x == "NULL", NA, carry.end_location.x)),
                               carry.end_location.y = as.numeric(ifelse(carry.end_location.y == "NULL", NA, carry.end_location.y)),
                               pass.end_location.x = as.numeric(ifelse(pass.end_location.x == "NULL", NA, pass.end_location.x)),
                               pass.end_location.y = as.numeric(ifelse(pass.end_location.y == "NULL", NA, pass.end_location.y)),
                               shot.end_location.x = as.numeric(ifelse(shot.end_location.x == "NULL", NA, shot.end_location.x)),
                               shot.end_location.y = as.numeric(ifelse(shot.end_location.y == "NULL", NA, shot.end_location.y)),
                               shot.end_location.z = as.numeric(ifelse(shot.end_location.z == "NULL", NA, shot.end_location.z)),
                               shot_impact_height = as.numeric(ifelse(shot_impact_height == "NULL", NA, shot_impact_height)))
                
        } else {
                dataframe <- dataframe %>%
                        mutate(location.x = (map(location, 1)),
                               location.y = (map(location, 2)),
                               pass.end_location.x = (map(pass.end_location, 1)),
                               pass.end_location.y = (map(pass.end_location, 2)),
                               shot.end_location.x = (map(shot.end_location, 1)),
                               shot.end_location.y = (map(shot.end_location, 2)),
                               shot.end_location.z = (map(shot.end_location, 3)),
                               shot_impact_height = (map(location, 3)))
                dataframe <- dataframe %>%
                        mutate(location.x = as.numeric(ifelse(location.x == "NULL", NA, location.x)),
                               location.y = as.numeric(ifelse(location.y == "NULL", NA, location.y)),
                               pass.end_location.x = as.numeric(ifelse(pass.end_location.x == "NULL", NA, pass.end_location.x)),
                               pass.end_location.y = as.numeric(ifelse(pass.end_location.y == "NULL", NA, pass.end_location.y)),
                               shot.end_location.x = as.numeric(ifelse(shot.end_location.x == "NULL", NA, shot.end_location.x)),
                               shot.end_location.y = as.numeric(ifelse(shot.end_location.y == "NULL", NA, shot.end_location.y)),
                               shot.end_location.z = as.numeric(ifelse(shot.end_location.z == "NULL", NA, shot.end_location.z)),
                               shot_impact_height = as.numeric(ifelse(shot_impact_height == "NULL", NA, shot_impact_height)))
        }
        
        return(dataframe)
}


# reading different seasons
for (season in unique(matches$season.season_name)) {
        
        events <- readSeasonEvents(season) %>% cleanlocations() %>%
                
                # inverting the y axis because the Statsbomb data assumes that the (0,0)
                # is in the top left and I want the (0,0) in bottom left
                mutate(location.y = 80 - location.y,
                       pass.end_location.y = 80 - pass.end_location.y,
                       carry.end_location.y = 80 - carry.end_location.y,
                       shot.end_location.y = 80 - shot.end_location.y) %>%
                
                # making light version from the events data
                select(c(type.name, 
                         team.name, 
                         player.name, 
                         duration,
                         possession_team.name,
                         location.x, 
                         location.y, 
                         shot.statsbomb_xg, 
                         pass.length,
                         pass.angle,
                         pass.cross,
                         pass.switch,
                         pass.end_location.x,
                         pass.end_location.y,
                         pass.recipient.name,
                         pass.outcome.name,
                         carry.end_location.x,
                         carry.end_location.y))
        
        season.name <- gsub("/", "", season)
        write.csv(events, paste("data/events_", season.name, ".csv", sep = ""), row.names = F) 
        
}
