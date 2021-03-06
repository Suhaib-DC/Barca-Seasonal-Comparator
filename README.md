# Barca Seasonal Comparator

"When you play a match, it is statistically proven that players actually have the ball 3 minutes on average … So, the most important thing is what do you do during those 87 minutes when you do not have the ball. That is what determines whether you’re a good player or not." - Johan Cruyff

Actually, this quote has nothing to do with this project, but come on, it's Cruyff talking about statistics!

However, in this project I used the Statsbomb open events data, so even though we can't know much about players who do not have the ball, we still can know a lot about the overall performance of Barcelona through seasons from 2004/2005 to 2019/2020.

## It is a Data Product
I built a Shiny web app to explore the events data for Barcelona in 4 main ways. You can choose one of four plots, two seasons to compare, and you can remove some players to see how the team performs without them, and then you can know how much they affected the team.

![](images/UI.PNG) 

## The Passes to Penalty Plot
It is well known that Barcelona likes possession of the ball, but with that been said, the purpose of keeping the ball is the same purpose of any other tactics in football, create chances and prevent the opponent from creating chances, so this plot will tell you how many times Barcelona moved the ball to the penalty area (by either passes or carries) and from where.

This is a comparison between passes and carries to penalty area in the 2010/2011 season and 2014/2015 season

![](images/p2p2011.png) ![](images/p2p2015.png)

You can notice the offensive improvement on the left side for Barca in 2015, also you can see that Barca played more long passes in 2015. It might be surprising, but Barca actually was better in attack in 2015 by an average of 49 balls in the box per match, and only 44 balls per match in 2011. But let’s be honest, who cares about efficiency when you can watch tiki-taka?

## The Pressure Plot
You can either close the spaces behind you, or reduce the opponent's time on the ball, and by that, we mean the time from which the player receives the ball until deciding what to do with it. So, since Barcelona likes to attack, and leaves a lot of space behind, it becomes essential to make high pressure after losing the ball so that the opponent cannot use the space in order to threaten Barcelona's goal. This plot is about where Barcelona applies pressure.


![](images/pres2009.png)

## The Passing Network
The actual reason I made this plot is that it is super cool. However, there are other reasons; you can take many insights from this plot. The passing network consists of two parts, the first is a group of points where each point represents the average location where the player passed or received the ball.  The group's size represents the total passes for the player. The second part is line segments between the points of which the size stands for the number of passes between the two players. A third part was added for the plot which is a donut chart to see the types of passes for each player.
For this plot, we only use the most starting XI; the "remove players" option cannot be used with this plot.

This is a comparison between the seasons 2010/2011 and 2013/2014

![](images/pn2011.png) ![](images/pn2014.png)

The plot gives you an idea of how players connect, for example, there is a very strong connection between Xavi, Alves, and Messi. Also, the offensive shape can be interpreted from the plot. in 2011 the team attack with 3-4-2-1 by Alves goes to midfield and Abidal being conservative. Messi and Pedro are playing a little bit behind Villa. In 2014 the team with the ball plays with something more like 1-1-5-3. However, you should take into account that the plot is for the whole season and not for a single match.

Final note, it's impressive how Sergio rarely passes to behind. and it's even more impressive how large the number of passes was in 2011!

## The Ball Retaining Plot
I need to say that this is a hard-to-read plot, maybe I will make an updated version that is easier. Basically, I have found every location that Barca lose the ball and attached it with the location where they get the ball back, then I calculated how many passes the opponent did before Barca retained possession. To visualize this, I made 20 clusters. You can see what clusters appear the most and how fast Barca retained the ball and find patterns.

To understand the plot, each segment represents a cluster of losing (square) and retain (circle) of the ball, the transparency stands for how many segments in the cluster, the color stands for how many passes the opponent did between losing the ball and retaining it.
For example, here is a comparison between seasons 2011/2012 and 2017/2018.

![](images/br2012.png) ![](images/br2018.png)

While we were counting PPDA from 4 – 10 in 2012 we started to count within 10 – 80 in 2018 which is saying a lot about the change in Barca style between the two seasons.


## Removing Players 

The main idea of removing players option is to find how certain players affected the overall performance of the team. You can choose one or more players and the app will remove their events. by making comparisons between the plot before and after removing the players you can see their effect.

for example, try to see how Xavi and Busquets affected ball retaining, or how Messi and Alves affected passes and carries to the penalty area. notice that the option is not available for passing network plot.

## Data Cleaning 

I have downloaded the data from the Statsbomb repository on GitHub. The original events data was in JSON format and it was extremely big, and this cause two problems, one is the time needed to read the data and transform it from JSON to a data frame, second is the size of the data. to solve these problems I wrote a code to read the data, transform it into a data frame, remove the unused columns (which is almost 80% of the data), and then write it into a CSV file. I made a separate CSV file for each season to make it faster to read.

The events data is not the only data I used. I needed the matches data and lineups data as well. so I made the processes needed on matches and lineups data and then I wrote the processed data into a CSV file.

The code for transforming the original data to the form that I used in the app can be found in "DataCleaning.R". I have used the "cleanlocations" function from the Statsbomb repository on GitHub by Euan Dewar in the process of cleaning the data.




