

####### Read the matches data #######
matches <- read.csv("data/matches.csv")
lineups.df <- read.csv("data/lineups.csv")


####### The Passes to Penalty Plot #######
passes2penalty <- function (events, season) {
        
        # making a data frame for all the passes
        # just the start and end location of a pass is included
        passes <- events %>% filter(type.name == "Pass", team.name == "Barcelona") %>%
                select(x = location.x,
                       y = location.y,
                       xend = pass.end_location.x,
                       yend = pass.end_location.y)
        
        # making a data frame for all the carries
        # just the start and end location of a carry is included
        carries <- events %>% filter(type.name == "Carry", team.name == "Barcelona") %>%
                select(x = location.x,
                       y = location.y,
                       xend = carry.end_location.x,
                       yend = carry.end_location.y)
        
        # keep only the passes and carries from outside to inside the box
        delivers.penalty <- rbind(passes, carries) %>%
                filter(xend > 102, yend > 18, yend < 62, !(x > 102 & y > 18 & y< 62))
        
        # clustering passes and carries for 6 clusters
        set.seed(1111)
        cl <- kmeans(delivers.penalty, 6, iter.max=1000)
        delivers.penalty$cluster <- as.factor(cl$cluster) 
        
        # obtain for each cluster id, the average location of the pass
        clusters <- delivers.penalty %>% group_by(cluster) %>%
                summarise(x = mean(x),
                          y = mean(y), 
                          xend = mean(xend),
                          yend = mean(yend),
                          count = n()) 
        
        ggplot() + 
                # Plot the pitch
                annotate_pitch(dimensions = pitch_statsbomb, fill = "gray7", colour = "#EBF0F2") + 
                theme_pitch() + direction_label(x_label = 60) + 
                
                # Plot the passes start points with different color for each cluster
                geom_point(data = delivers.penalty, aes(x = x , y = y, color = cluster),
                           alpha = 1,
                           size = 4, stroke = 0, shape=16) +
                scale_color_manual(values = c("#ffba08", "#d00000", "#3db2d2", "#3fdec9", "#14796c", "#044166")) +
                theme(legend.position = "none") +
                
                # Putting the average number of delivers per match for each cluster
                geom_label(data =  clusters ,aes(x = x, y = y,
                                                 label = round(count/nrow(matches %>% filter(season.season_name == season)), 1)), size = 6) + 
                
                # titles
                labs(title = "How Barcelona move the ball to the oppunent penalty area?",
                     subtitle = paste("Passes and carries to the box, La liga, season ", season, sep = ""),
                     caption = "This plot shows the start points of passes and carries from outside to inside the box") + 
                
                # customizing the plot
                theme(plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                      plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                      plot.caption = element_text(color = "#D8D8D6", size = 12),
                      plot.title.position = "panel",
                      plot.background = element_rect(fill = "gray7"),
                      legend.position = "top",
                      legend.key = element_rect(fill = "gray7"),
                      legend.background = element_rect(fill = "gray7"),
                      legend.justification = "center", 
                      legend.text = element_text(color = "#D8D8D6"),
                      legend.title = element_text(face = "bold", color = "#D8D8D6"),
                      legend.box.spacing = unit(0, "pt"),
                      legend.margin = margin(20))
        
        
        
}



####### The Pressure Plot #######
pressure.plot <- function (events, season) {
        
        # finding pressure events
        pres <- events %>% filter(type.name == "Pressure", team.name == "Barcelona")
        
        #drawing pressure plots
        # define a palette for heat colors
        heat_colors <- grDevices::colorRampPalette(c("#800026FF", "#FC4E2AFF", "#FEB24CFF", "#FFFFCCFF"))(12)
        
        ggplot() + 
                # density plot
                geom_density_2d_filled(data = pres,
                                       aes(x=location.x, y=location.y),
                                       contour = FALSE, alpha = 1) + 
                # add heat colors
                scale_fill_manual(values = c(heat_colors), aesthetics = c("fill", "color")) +
                theme(legend.position='none') + 
                
                # add the pitch
                annotate_pitch(dimensions = pitch_statsbomb, fill = NA, colour = "#EBF0F2") + 
                theme_pitch() + direction_label(x_label = 60) + 
                # titles
                labs(title = "How Barcelona apply pressure?",
                     subtitle = paste("La liga, season ", season, sep = "")) + 
                
                # customizing the plot
                theme(plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                      plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                      plot.caption = element_text(color = "#D8D8D6", size = 12),
                      plot.title.position = "panel",
                      plot.background = element_rect(fill = "gray7"))
        
        
}

####### The Passing Network Plot #######
passing.network.plot <- function(events, lineups, season) {
        
        lineups <- lineups.df %>% select(contains(gsub("/", "_", season))) %>% select(1:2)
        names(lineups) <- c("player.name", "player.nickname")
        
        # reading passes
        passes <- events %>% filter(type.name == "Pass", team.name == "Barcelona", player.name %in% lineups$player.name)
        passes.rec <- events%>% filter((type.name == "Pass"| type.name == "Ball Receipt*"), team.name == "Barcelona", player.name %in% lineups$player.name)
        
        
        # find different types of passes
        passes.progressive <- passes %>% filter(pass.angle > (-pi/2), pass.angle < (pi/2)) %>% group_by(player.name) %>% summarise(progressive=n())
        passes.switch <- passes %>% filter(pass.switch == T) %>% group_by(player.name) %>% summarise(switches=n())
        passes.cross <- passes %>% filter(pass.cross == T) %>% group_by(player.name) %>% summarise(crosses=n())
        passes.long <- passes %>% filter(pass.length > (35)) %>% group_by(player.name) %>% summarise(long=n())
        passes.count <- passes %>% group_by(player.name) %>% summarise(n=n())
        passes.short <-  passes %>% filter(pass.length < (35)) %>% group_by(player.name) %>% summarise(short=n())
        
        # make one data frame for how much players did passes from each type
        passes.summary <- merge(passes.count, passes.progressive, by = "player.name", all = T) %>%
                merge(passes.switch, by = "player.name", all = T)  %>%
                merge(passes.cross, by = "player.name", all = T) %>% 
                merge(passes.long, by = "player.name", all = T) %>%
                merge(passes.short, by = "player.name", all = T) %>%
                mutate(regular = n - (progressive + switches + crosses))
        
        
        # find the average positions where the player passing 
        passing.network <- passes.rec %>% group_by(player.name) %>% summarise(x= mean(location.x, na.rm = T), y = mean(location.y, na.rm = T))
        
        # merging the summary, locations and nicknames into one data frame
        passing.network <- merge(passing.network, passes.summary, by = "player.name", all = T) %>% merge(lineups, by = "player.name", all = T)
        
        # find the number of passes between the players
        passes.between <- list()
        players <- passing.network$player.name
        for (player in players) {
                passes.between[[player]] <-  passes[passes$player.name == player,] %>% group_by(pass.recipient.name) %>% summarise(n = n())
                names(passes.between[[player]]) <- c("player.name", player)
                passing.network <- merge(passing.network, passes.between[[player]], by = "player.name", all = T)
        }
        
        # replacing NA's with zeroes
        passing.network[is.na(passing.network)] <- 0
        
        # choosing the most 11 starters and adding nicknames 
        passing.network.11 <- passing.network %>% filter(player.name %in% lineups$player.name) %>%
                select("player.name", "x", "y", "n", "progressive", "switches", "crosses", "regular",lineups$player.name , "player.nickname")
        
        
        ### Plotting the Passing network ###
        # making the segments
        
        # initialize the segments from the first player to all other players
        x = rep(passing.network.11$x[1], 11)
        y = rep(passing.network.11$y[1],11)
        xend = passing.network.11$x
        yend = passing.network.11$y
        
        # calculating the number of passes between the two players
        player = passing.network.11$player.name[1]
        t1 <- data.frame(names = row.names(t(passing.network.11[1, 9:19])) ,n1 = t(passing.network.11[1, 9:19]))
        t2 <- data.frame(names = passing.network.11$player.name, n2 = passing.network.11[player])
        t <- merge(t1, t2, by = "names")
        n <- unlist(t[2] + t[3])
        names(n) <- NULL
        
        # Looping over the rest of players 
        for (i in 1:10) {
                p <- passing.network.11[-seq(1:i),]
                x <- append(x, rep(p$x[1], 11-i))
                y <- append(y, rep(p$y[1], 11-i))
                xend <- append(xend, p$x)
                yend <- append(yend, p$y)
                
                player = p$player.name[1]
                t1 <- data.frame(names = row.names(t(p[1, 9:19])) ,n1 = t(p[1, 9:19]))
                t2 <- data.frame(names = p$player.name, n2 = p[player])
                t <- merge(t1, t2, by = "names")
                t <- na.omit(t)
                n1 <- unlist(t[2] + t[3])
                names(n1) <- NULL
                
                n <-append(n, n1)
                
        }
        
        # making one data frame for all segments
        segments <- data.frame(x,y,xend,yend,n)
        
        # segments plot
        ggplot() +
                # plotting the pitch
                annotate_pitch(dimensions = pitch_statsbomb, fill = "gray7", colour = "#EBF0F2") + 
                theme_pitch() + direction_label(x_label = 60) + 
                
                # plotting segments for the passes between players
                geom_segment(aes(x = x,
                                 y = y,
                                 xend = xend,
                                 yend = yend), data = segments,
                             color = "#D8D8D6", size = segments$n/200, alpha = 0.9) + 
                
                # plotting pie charts for both the average position and the types of passes
                geom_scatterpie(aes(x = unlist(x), y = unlist(y), r = unlist(n)/350),
                                data = passing.network.11, cols= c("progressive", "switches", "crosses", "regular"), color = NA) + 
                scale_fill_manual(values = c("progressive" = "#136f63", "switches" =  "#d00000", "crosses" = "#3db2d2", "regular" = "#ffba08")) +
                
                # plotting points and players names inside the pie chart to make it lock better
                geom_point(data = passing.network.11, mapping = aes(x = x, y = y), color  = "gray7", size = (passing.network.11$n/115), alpha = 1) +
                geom_text(data = passing.network.11, aes(x = x, y = y, label = player.nickname), color = "#D8D8D6", fontface = "bold") + 
                
                # titles
                labs(title = "Passing network for Barcelona",
                     subtitle = paste("La liga, season ", season, sep = ""),
                     color = "Type of pass",
                     caption = "in passing networks, the size of the point indicates the total passes for the player, the size of   
                                the line between two players indicates the number of basses between them, and the position
                                of the point is the avarege location where the player pass and recive the ball") + 
                
                # customizing the plot
                theme(plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                      plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                      plot.caption = element_text(color = "#D8D8D6", size = 12),
                      plot.title.position = "panel",
                      plot.background = element_rect(fill = "gray7"),
                      legend.position = "top",
                      legend.key = element_rect(fill = "gray7"),
                      legend.background = element_rect(fill = "gray7"),
                      legend.justification = "center", 
                      legend.text = element_text(color = "#D8D8D6"),
                      legend.title = element_text(face = "bold", color = "#D8D8D6"),
                      legend.box.spacing = unit(0, "pt"),
                      legend.margin = margin(20))
        
        
}


####### The Ball retaining Plot #######

ballRetain <- function(events, season) {
        # adding two columns to identify where the losses and retains of the ball happened 
        events <- events %>% mutate(ball.loss = ifelse((possession_team.name ==  "Barcelona" & lead(possession_team.name) != "Barcelona"), T, F),
                                    ball.retain = ifelse((possession_team.name ==  "Barcelona" & lag(possession_team.name) != "Barcelona"), T, F))
        
        # making one data frame that includes the location of all losses with the
        # locations of the corresponding retains 
        # the row names from the original events data included as numeric to calculate PPDA
        df <- data.frame(losses.rows = as.numeric(rownames(events[events$ball.loss == T,]))) %>% 
                mutate(retains.rows = as.numeric(rownames(events[events$ball.retain == T,]))) %>%
                mutate(loss.location.x = events$location.x[events$ball.loss == T]) %>% 
                mutate(loss.location.y = events$location.y[events$ball.loss == T]) %>% 
                mutate(retain.location.x = events$location.x[events$ball.retain == T]) %>%
                mutate(retain.location.y = events$location.y[events$ball.retain == T]) %>% na.omit()
        
        
        # removing the retain of the ball if it came before loss
        if (df$losses.rows[1] > df$retains.rows[1]) {
                df <- df %>% mutate(retains.rows = data.table::shift(df$retains.rows, type = "lead"),
                                    retain.location.x = data.table::shift(df$retain.location.x , type = "lead"),
                                    retain.location.y = data.table::shift(df$retain.location.y , type = "lead")) %>% na.omit()
        }
        
        # calculating the passes allowed to the opponent between loss and retain of the ball
        # PPDA: passes allowed per defensive action
        retain.ball.df <- df %>% mutate(PPDA = apply(df, 1, function(x) { sum(events$type.name[x[1]:x[2]] == "Pass")}))
        
        # clustering the losses and retains
        cl <- kmeans(retain.ball.df[3:7], 20, iter.max=1000)
        retain.ball.df$cluster <- cl$cluster 
        
        #obtain for each cluster id, the average location of the loss and retain
        retain.ball.df.cluster <- retain.ball.df %>% group_by(cluster) %>% 
                summarise(loss.location.x = mean(loss.location.x),loss.location.y = mean(loss.location.y),
                          retain.location.x = mean(retain.location.x), retain.location.y = mean(retain.location.y),
                          PPDA = mean(PPDA), count = n())  %>% arrange(count) %>% mutate(color = rep(c("First", "Seconed", "Third", "Fourth", "Fifth"), each = 4))
        
        # plot
        set.seed(11110)
        ggplot() +
                # plotting the pitch
                annotate_pitch(dimensions = pitch_statsbomb, fill = "gray7", colour = "#EBF0F2") + 
                theme_pitch() + direction_label(x_label = 60) + 
                
                # starting with segments between the two positions of lose and retain the ball
                geom_segment(data = retain.ball.df.cluster, aes(x = loss.location.x,
                                                                y = loss.location.y,
                                                                xend = retain.location.x,
                                                                yend = retain.location.y, color = PPDA, alpha = color), size = 3) +
                
                # plotting square in the lose position and circle in the retain position
                geom_point(data = retain.ball.df.cluster, aes(x = loss.location.x, y = loss.location.y, color = PPDA), shape = 15, size = 4) + 
                geom_point(data = retain.ball.df.cluster, aes(x = retain.location.x, y = retain.location.y, color = PPDA), shape = 19, size = 4) +
                scale_fill_gradientn(colours = c("#ffba08", "#d00000", "#3db2d2", "#136f63", "#032b43"),
                                     na.value = 'darkgrey', aesthetics = 'color') + 
                
                # titles
                labs(title = "How did Barcelona retains possission of the ball?",
                     subtitle = paste("Clustring where they lose the ball (square) and where they retain it (circle) and how fast, La Liga, season ", season, sep = ""),
                     color = "PPDA",
                     alpha = "Most repeated cluster",
                     caption = "PPDA is Passes allowed for the oppunent before definsive action") + 
                
                # customizing the plot
                theme(plot.title = element_text(face = "bold", color = "#D8D8D6", size = 22),
                      plot.subtitle = element_text(color = "#D8D8D6", size = 16),
                      plot.caption = element_text(color = "#D8D8D6", size = 12),
                      plot.title.position = "panel",
                      plot.background = element_rect(fill = "gray7"),
                      legend.position = "top",
                      legend.key = element_rect(fill = "gray7"),
                      legend.background = element_rect(fill = "gray7"),
                      legend.justification = "center", 
                      legend.text = element_text(color = "#D8D8D6"),
                      legend.title = element_text(face = "bold", color = "#D8D8D6"),
                      legend.box.spacing = unit(0, "pt"),
                      legend.margin = margin(20))
        
}
