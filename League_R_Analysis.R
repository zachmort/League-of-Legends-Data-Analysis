###league of legends 2021 data exploration
#[x] data clean up
#[x] visuals
#[x] data exploration
#[x] prediction with logistic regression  
#[x] clustering (which players are most like others)

# Mission:
#   Can we predict what team will win a game based on certain game factors 
#can we find out what factors of a game have statistical significance to wiinning any given match

#Using K Means Clustering if you are the general manager of a team it is imperitive to know what players are like eachother when free agency and player contracts are up
#you might be able to aquire a comprable player to the one you have now on a smaller contract OR certain play styles my not work with your team composition so you my want to stay away from certain players


# Install pacman ("package manager") if needed
# Pacman is a wonderful package installer in R
if (!require("pacman")) install.packages("pacman")   # Tells R to install pacman if it is not already installed

# pacman must already be installed; then load packages
# packages (including pacman) with pacman
pacman::p_load(magrittr, # magrittr: for pipes
               cowplot,
               pacman, # pacman: for loading/unloading packages
               rio, # rio: for importing data
               rmarkdown, 
               tidyverse, 
               lubridate, 
               grid,
               corrplot, 
               xtable, 
               leaps, 
               caret, # caret: for decision trees
               ROCR, 
               plotly, # plotly: for interactive plots
               RColorBrewer)

#loading data in with tidyverse read_csv
data <- read_csv("C:/Users/zman7/OneDrive/League-Legends-data/2021_LoL_esports_match_data_from_OraclesElixir_20210523.csv")

#Looking at an overview of the data
summary(data)

#turning the data frame into a tibble
all_data <- data %>%
  as_tibble()


###################################                                                                     ###################################
###################################              Beginning Analysis of Player specific data             ###################################
###################################                                                                     ###################################

############################----------------------------LOOKING INTO PLAYER SPECIFIC DATA----------------------------############################
#removing "Team" records and team related variables
#the observation holds player data as well as overall team data
#Team related data has player_name = "NA" so removing all records with playername = NA to get only player records
player_data <- all_data %>%
  filter(player != is.na(player) &                                 #getting rid of all of the "team" records in the data
           datacompleteness == "complete") %>%                     #Selecting only records that have "complete" data
  select(-c("url",                                                 #url for each game
            "playerid",                                            #repetitive id for each player in a certain game
            "gameid",                                              #Game Identifier
            "game",                                                #Game number based on order              
            "teamkills",                                           #Total team kills
            "teamdeaths",                                          #Total team deaths
            "patch",                                               #Game patch number (Could look into what champs are played the most grouped by patch)
            "team kpm":"opp_inhibitors",                           #Team descriptors
            "gspd",                                                #Team descriptors
            "ban1":"ban5",                                         #Champion bans in the pregame lobby
            "wcpm":"controlwardsbought",                           #Ward purchasing variables
            "earnedgoldshare","goldspent",                         #Gold (Money) related variables
            "year",                                                #Year covers part of 2020 and 2021
            "split"))


####------------------------------Converting variables------------------------------####

#Converting playoffs to factor as the variable is numeric by default
player_data$playoffs <- as.factor(player_data$playoffs)
#Converting firstblood to factor as the variable is numeric by default
player_data$firstblood <- as.factor(player_data$firstblood)
#Converting firstbloodkill to factor as the variable is numeric by default
player_data$firstbloodkill <- as.factor(player_data$firstbloodkill)
#Converting firstbloodassist to factor as the variable is numeric by default
player_data$firstbloodassist <- as.factor(player_data$firstbloodassist)
#Converting firstbloodvictim to factor as the variable is numeric by default
player_data$firstbloodvictim <- as.factor(player_data$firstbloodvictim)
#Converting result (win = 1, loss = 0) to factor as the variable is numeric by default
player_data$result <- as.factor(player_data$result)
#Converting side (red or blue) to factor as the variable is character by default
player_data$side <- as.factor(player_data$side)
#Converting position (top, jng, mid, bto, sup) to factor as the variable is character by default
player_data$position <- as.factor(player_data$position)


#Changing date variable so that there is no HMS
date_adj <- mdy_hm(player_data$date)                       #Using Tidyverse function to put the data in a more usable format
date <- format(as.POSIXct                                          #Creating date variable to alter current date in data set
               (date_adj,format='%Y/%m/%d %H:%M:%S'),      #First format is the format it is currently in the second format is what format I want it in
               format='%m/%d/%Y')      
date <- as.Date(player_data$date, "%m/%d/%Y")                      #Setting date as a date class
player_data$date <- date
class(player_data$date)


head(player_data)

#Confirming Date and team are in the correct format
class(player_data$date)
class(player_data$team)

#Summary of new dataset
summary(player_data)

#checking all of NAs in the data
sum(is.na(player_data))
which(is.na(player_data))

#Taking out all non major region/leagues (using only tier 1 leagues) (info for the leagues can be found on the LOL Wiki)
# Here is the link: https://en.wikipedia.org/wiki/List_of_League_of_Legends_leagues_and_tournaments

#looking to see the league names that are currently in the data
unique(player_data$league)

#selecting only the major LOL leagues (tier 1) (LPL, LCK, LEC, LCS, PCS, CBLOL, LCO, LCL, LJL, LIA, TCL, VCS)
#pulled from LOL wiki
#creating variable tier_1_leagues to store the leagues I want to keep
tier_1_leagues <- filter(player_data, league %in% c("LPL", "LCK","LEC", "LCS", "PCS", "CBLOL", "LCO", "LCL", "LJL", "LLA","TCL","VCS"))
#making sure I only have the ones I selected
unique(tier_1_leagues$league)

#Creating variable of player ddata for tier1 leagues
player_data_tier1_leagues <- tier_1_leagues

player_data_tier1_leagues$league <- as.factor(player_data_tier1_leagues$league)
#moving "result" to the front of the data, by putting everything() after result it puts the rest of the variables after the first one I selected
player_data_tier1_leagues %<>% select(result, everything())


####------------------------------Data viz and exploration------------------------------####

#Creating histograms of all numeric variables to look at distributions of the data
player_data_tier1_leagues %>%
  keep(is.numeric) %>%                   #keeping only numeric variables for the graphs
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +   #allowing each histogram to be scaled to its own relevant data or else this graphs would be generally scaled
  geom_histogram()                       #Creating histograms

#assits graph looks strange as well
#but it is just a visual error
player_data_tier1_leagues %>%
  ggplot(aes(x = assists)) +
  geom_bar()


#looking at the bimodal peaks in cs at 10, cspm, minion kills
#the 3 graphs below indicate the the position "support" does not get much "CS" in the game whether it be @10 or in the whole game
#This makes sense as the role of Support is to support the bot laner/ADC
player_data_tier1_leagues %>%
  select(position, `total cs`) %>%
  group_by(position) %>%
  ggplot(aes(y = `total cs`,reorder(position, `total cs`, FUN = median))) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Total CS" )

player_data_tier1_leagues %>%
  select(position, cspm) %>%
  group_by(position) %>%
  ggplot(aes(y = cspm ,reorder(position, cspm, FUN = median))) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("CS Per Minute" )

#the jng does not kill minions mostly monsters this is normal
player_data_tier1_leagues %>%
  select(position, minionkills) %>%
  group_by(position) %>%
  ggplot(aes(y = minionkills ,reorder(position, minionkills, FUN = median))) +
  geom_boxplot() +
  theme_bw() +
  ggtitle("Minion Kills" )


#the plot below shows the earned gold vs the CS per minute by position
#Supports tend to have the lowest gold and the lowest CS per minute
#High correlation exists between CS and gold because you get gold from killing creeps
ggplot(data = player_data_tier1_leagues,  mapping = aes(x =`total cs` , y = earnedgold)) +
  geom_point(mapping = aes(color = position), alpha = .6) +
  geom_smooth() +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15))) +
  ggtitle("Total Creep Score VS Earned Gold") + 
  labs(x ="Total Creep Score", y = "Earned Gold")



############-----------------------Making plots for chovy and showmaker since they are the best players in the world and they are both midlaners------------###################
golddiff_chovyVSshowmaker <- player_data_tier1_leagues %>%
  select(player, golddiffat15) %>%
  filter(player == "Chovy" | player == "ShowMaker") %>%
  group_by(player) %>%
  ggplot(mapping = aes(x = player, y = golddiffat15, title = "Boxplot of Chovy and ShowMaker Gold Differnce at 15 Minutes ")) +
  geom_boxplot(mapping = aes(fill = player)) +
  geom_jitter(position = position_jitter(.2)) +
  coord_cartesian( ylim = c(-1500, 1500))

golddiff_chovyVSshowmaker


cspmdiff_chovyVSshowmaker <- player_data_tier1_leagues %>%
  select(player, cspm) %>%
  filter(player == "Chovy" | player == "ShowMaker") %>%
  group_by(player) %>%
  ggplot(mapping = aes(x = player, y = cspm, title = "Boxplot of Chovy and ShowMaker CS")) +
  geom_boxplot(mapping = aes(fill = player)) +
  geom_jitter(position = position_jitter(.2))
#coord_cartesian( ylim = c(-1500, 1500))

cspmdiff_chovyVSshowmaker

xpat15diff_chovyVSshowmaker <- player_data_tier1_leagues %>%
  select(player, xpdiffat15) %>%
  filter(player == "Chovy" | player == "ShowMaker") %>%
  group_by(player) %>%
  ggplot(mapping = aes(x = player, y = xpdiffat15 , title = "Boxplot of Chovy and ShowMaker XP difference @15 minutes")) +
  geom_boxplot(mapping = aes(fill = player)) +
  geom_jitter(position = position_jitter(.2))
#coord_cartesian( ylim = c(-1500, 1500))

xpat15diff_chovyVSshowmaker

plot_grid(golddiff_chovyVSshowmaker,cspmdiff_chovyVSshowmaker,xpat15diff_chovyVSshowmaker + remove(x.text),
          labels = c("Gold Diff @ 15", "CS Diff @ 15", "XP Diff @ 15"),
          ncol = 1, nrow = 3)

#Conclusion:
#Chovy is a better laner than showmaker is based on the gold/cs/xp diff graphs
#--------------------------------------------------------------------------------------------------------------------------------------------#



#Trying to get an idea of who is the "best" player by looking at the average gold at 15 and average xpat 15 for each player
############## sort.val = "asc" for sorting data on a graph ###############

player_goldandxp_diff_10 <- player_data_tier1_leagues %>%
  select(player, xpdiffat10, golddiffat10) %>%
  group_by(player) %>%
  summarise(mean_gold_diff = mean(golddiffat10),
            mean_xp_diff = mean(xpdiffat10))

q <- player_goldandxp_diff_10 %>%
  ggplot(aes(mean_gold_diff, mean_xp_diff, color = player), title = "Mean Gold Diff vs Mean XP Diff") +
  geom_point(alpha = .5) +
  theme_bw() +
  theme(legend.position = "none")


ggplotly(q)


player_goldandxp_15 <- player_data_tier1_leagues %>%
  select(player, goldat15, csat15, xpat15) %>%
  group_by(player) %>%
  summarise(avg_gold_15 = mean(goldat15),
            avg_xp_15 = mean(xpat15))

p <- player_goldandxp_diff_10 %>%
  ggplot(aes(mean_gold_diff, mean_xp_diff, color = player)) +
  geom_point(alpha = .5) +
  theme_bw() +
  theme(legend.position = "none")

ggplotly(p)



#Do certain leagues play more games than other leagues (regular season)
regseason_games_played <- player_data_tier1_leagues %>%
  filter(playoffs ==0) %>%
  group_by(league) %>%
  #try using length(name) to count number of games played
  #get total number of obs for the LCK to check to make sure these numbers are right
  summarise(games_played = n()/5) %>%
  arrange(desc(games_played))

regseason_games_played

#Clearly the LCK and LPL play more regular season games than any other league
ggplot(data = player_data_tier1_leagues, aes(fct_reorder(league,
                                                         games_played),games_played)) +
  geom_col()

#Checking to see if there are an equal number of wins as loses
ggplot(data = player_data_tier1_leagues) +
  geom_bar(mapping = aes(x = result))


#Looking at damage done vs game length by position
###testing out ggplotly function
p <- player_data_tier1_leagues %>%
  ggplot(aes(gamelength, damagetochampions, color = position)) +
  geom_point(alpha = .4) +
  theme_bw()

ggplotly(p)



#ranking the top players based on damage done (averagedpm)

#non playoffs
player_data_tier1_leagues %>%
  filter(playoffs == 0) %>%
  group_by(player) %>%
  summarise(mean_dpm = mean(dpm),
            mean_total_damage = mean(damagetochampions)) %>%
  arrange(desc(mean_dpm))


#playoffs
player_data_majors_leagues_only %>%
  filter(playoffs == 1) %>%
  group_by(player) %>%
  summarise(mean_dpm = mean(dpm),
            mean_total_damage = mean(damagetochampions)) %>%
  arrange(desc(mean_dpm))

#mean_kills/mean_deaths/mean_dpm per player

player_average_damage <- player_data_tier1_leagues %>%
  group_by(player) %>%
  summarise(games_played = n(),
            mean_kills = mean(kills),
            mean_death = mean(deaths),
            mean_dpm = mean(dpm)) %>%
  arrange(desc(mean_dpm))
player_average_damage

#general analysis by region
averages_by_league <- player_data_tier1_leagues %>%
  group_by(league) %>%
  summarise(games_played = n()/5,
            mean_gamelength = mean(gamelength),
            mean_kills = mean(kills),
            mean_death = mean(deaths)) %>%
  arrange(desc(games_played))

averages_by_league

#team with highest winning percentage in reg season
top20_winning_percentage_by_team <- player_data_tier1_leagues %>%
  group_by(team) %>%
  filter(playoffs == 0) %>%
  summarise(win_percentage = sum(result ==1)/n(),
            matches_played = n()/5) %>%
  arrange(desc(win_percentage)) %>%
  slice(1:20)

top20_winning_percentage_by_team


#champ has highest winning percentage (must have 10 game min)
#graph to go along with it
top20_winning_percentage_by_champ <- player_data_tier1_leagues %>%
  group_by(champion) %>%
  filter(n() >=10) %>%
  summarise(win_percentage = sum(result ==1)/n(),
            matches_played = n()) %>%
  arrange(desc(win_percentage)) %>%
  slice(1:20)

top20_winning_percentage_by_champ


#chart showing champion play count
#and graph of champs
top20_played_champs <- player_data_tier1_leagues %>%
  select(champion, result) %>%
  group_by(champion) %>%
  summarise(win_percentage = sum(result ==1)/n(),
            matches_played = n()) %>%
  arrange(desc(matches_played)) %>%
  slice(1:20)

top20_played_champs


top20_played_champs %>% 
  ggplot(aes(fct_reorder(champion,
                         matches_played), 
             matches_played))+
  geom_col() +
  coord_flip()+
  labs(x="Champion", title="Top 20 played champions")

top20_winning_percentage_by_champ %>%
  ggplot(aes(fct_reorder(champion,
                         win_percentage),
             win_percentage)) +
  geom_col() +
  coord_flip() +
  labs(x="Champion", title="Top 20 win % by champion")

#winning percentage on red side vs blue side
#quite a bit higher percentage of winning on blue side
winning_percent_by_side <- player_data_tier1_leagues %>%
  group_by(side) %>%
  summarise(win_percentage = sum(result ==1)/n())

winning_percent_by_side

winning_percent_by_side %>%
  ggplot(aes(fct_reorder(side, 
                         win_percentage),
             win_percentage)) +
  geom_col(aes( fill = win_percentage > .5)) +
  scale_fill_discrete(guide = 'none') +
  labs(x = "Map Side", title = "Winning % by Map Side")

#winning percentage of side by region on looking at playoffs because each region should be 50/50 if we are looking at games played in each
winning_percent_by_side_byregion <- player_data_tier1_leagues %>%
  group_by(league, side) %>%
  filter(playoffs == 1) %>%
  summarise(win_percentage = sum(result ==1)/n())
winning_percent_by_side_byregion

winning_percent_by_side_byregion %>%
  ggplot(aes(fct_reorder(league, 
                         win_percentage),
             win_percentage)) +
  geom_point(mapping = aes(color = side)) +
  labs(x = "League", title = "Winning % by Map Side")


winning_percent_by_side_byregion %>%
  ggplot(mapping = aes(x = league , y = win_percentage)) +
  geom_col((mapping = aes(fill = side)),position = "dodge") +
  labs(x = "League", title = "Winning % by Map Side")


#damage p m by position
player_data_tier1_leagues %>%
  ggplot(mapping = aes(x = gamelength, y = damagetochampions)) +
  geom_point(mapping = aes(color = position),alpha = .6) +
  geom_smooth()

ggplot(data = player_data_tier1_leagues) +
  geom_smooth(mapping = aes(x = gamelength, y = damagetochampions, color = position))

#game length regular season vs playoffs (do games last longer)
player_data_tier1_leagues %>%
  group_by(league, playoffs) %>%
  summarise(mean_gamelength = mean(gamelength))

ggplot(data = player_data_tier1_leagues) +
  geom_boxplot(mapping = aes(x = gamelength, y = league, color = playoffs))



############################----------------------------LOOKING INTO TEAM SPECIFIC DATA----------------------------############################
golddiff <-player_data_tier1_leagues %>% 
  select( team, golddiffat15, league) %>% 
  group_by(team) %>% 
  summarise(avg_goldiff = mean(golddiffat15)) %>%
  arrange(desc(avg_goldiff))

golddiff 

bottom5 <- golddiff[1:5,1]
top5 <- golddiff[115:119,1 ]
top_and_bottom_team <-rbind(bottom5,top5)

top_and_bottom_team_list<- list(top_and_bottom_team$team)

#fix
golddiff_teams <-player_data_tier1_leagues %>% 
  select( team, golddiffat15, league) %>% 
  filter(team %in% .data[[top_and_bottom_team_list[1]]])

golddiff_teams

ggplot() +
  geom_boxplot(data = player_data_tier1_leagues, mapping = aes(x = team, y = golddiffat15, color = league)) + 
  theme(legend.position = "none")
###

golddiff
csdiff <- ggplot(data = player_data_tier1_leagues) +
  geom_boxplot(mapping = aes(x = league, y = csdiffat15, color = league)) + theme(legend.position = "none")

xpdiff <- ggplot(data = player_data_tier1_leagues) +
  geom_boxplot(mapping = aes(x = league, y = xpdiffat15, color = league)) + theme(legend.position = "none")

plot_grid(golddiff_chovyVSshowmaker,golddiff_everyone,
          labels = c("Chovy VS ShowMaker", "All Players"),
          ncol = 2, nrow = 1)



#### logistic regression analysis and prep ##################################################################

#checking to see if there are variables that are highly correlated with each other
#Multicollinearity occurs when independent variables in a regression model are correlated. 
#This correlation is a problem because independent variables should be independent. 
#If the degree of correlation between variables is high enough, 
#it can cause problems when you fit the model and interpret the results.

#creating a dataset for team data
team_data <- all_data


#can do playoffs = 0 and playoffs = 1

team_data_vars <- team_data %>%
  filter(position == "team" & datacompleteness == "complete"     #getting rid of all of the "player" records in the data
  ) %>%                                                         #Selecting only records that have "complete" data
  select(c("teamkills", 
           "teamdeaths",
           "infernals",
           "oceans",
           "clouds",
           "mountains",
           "elders",
           "heralds",
           "barons",
           "visionscore",
           "minionkills",
           "monsterkills",
           "playoffs",
           "position",
           "datacompleteness",
           "result",
           "side",
           "firstdragon",
           "firstblood",
           "firstherald",
           "firstbaron"))



table(is.na(team_data_vars))
sapply(team_data_vars, function(x) sum(is.na(x)))      #showing all columns with NAs

team_data_vars <- na.omit(team_data_vars)
#minion kills has NA values
#vision score has NA values


summary(team_data_vars)
team_data_vars$firstdragon <- as.factor(team_data_vars$firstdragon)
team_data_vars$playoffs <- as.factor(team_data_vars$playoffs)
team_data_vars$firstblood <- as.factor(team_data_vars$firstblood)
team_data_vars$result <- as.factor(team_data_vars$result)
team_data_vars$side <- as.factor(team_data_vars$side)
team_data_vars$firstherald <- as.factor(team_data_vars$firstherald)
team_data_vars$firstbaron <- as.factor(team_data_vars$firstbaron)
team_data_vars$position <- as.factor(team_data_vars$position)
team_data_vars$firstdragon <- as.factor(team_data_vars$firstdragon)
team_data_vars$firstblood <- as.factor(team_data_vars$firstblood)
team_data_vars$firstherald <- as.factor(team_data_vars$firstherald)
team_data_vars$firstbaron <- as.factor(team_data_vars$firstbaron)
team_data_vars$infernals <- as.numeric(team_data_vars$infernals)
team_data_vars$clouds <- as.numeric(team_data_vars$clouds)
team_data_vars$oceans <- as.numeric(team_data_vars$oceans)
team_data_vars$mountains <- as.numeric(team_data_vars$mountains)

str(team_data_vars)

#removing non numeric vars to do a correlation matrix
numeric_vars <- team_data_vars %>%      #using select if from the dplyr package
  select_if(is.numeric)

mcor<-round(cor(numeric_vars),2)
mcor

# Hide upper triangle
upper<-mcor
upper[upper.tri(mcor)]<-""     #setting upper triangle values to blank
upper<-as.data.frame(upper)
upper

corrplot(mcor, method="number")

# checking vars for high correlation
# vision score has a high correlation with minion kills -
# as the more minions you kill the more gold you make the more wards you can buy which contributes to vision score
#These 3 variables of (minion kills, monster kills and vision score are independent but have high correlation as that is the nature of the game)


####### prediction of wins on team based data in the regular season #########################
pacman::p_load(caret, magrittr, pacman, parallel, e1071, rattle)

#putting the response variable of "result" first in the data set
team_data_vars %<>% select(result, everything())
summary(team_data_vars)

#selecting only regular season data, playoffs is different in nature and teams get eliminated so limits the availability of data
team_data_regular_season <- team_data_vars %>%
  filter(playoffs == 0)

#scaling numeric vars - for example vision score and minion kills are on different scales so this helps balance the data

scaled_team_data <- team_data_regular_season %>%
  mutate_if(is.numeric, scale)

numeric_vars_for_hist <- team_data_regular_season %>%      
  select_if(is.numeric)


#this is much more code but the graphs and titles look much better
cowplot::plot_grid(plotlist = list)


par(mfrow=c(3,3))
hist(numeric_vars_for_hist$teamkills)
hist(numeric_vars_for_hist$teamdeaths )
hist(numeric_vars_for_hist$elders)
hist(numeric_vars_for_hist$heralds)
hist(numeric_vars_for_hist$barons)
hist(numeric_vars_for_hist$visionscore)
hist(numeric_vars_for_hist$minionkills)
hist(numeric_vars_for_hist$monsterkills)



par(mfrow=c(3,3))
hist(scaled_team_data$teamkills)
hist(scaled_team_data$teamdeaths )
hist(scaled_team_data$elders)
hist(scaled_team_data$heralds)
hist(scaled_team_data$barons)
hist(scaled_team_data$visionscore)
hist(scaled_team_data$minionkills)
hist(scaled_team_data$monsterkills)
###################################

# Density plots of numeric variables
scaled_team_data %>%
  gather(var, val, teamkills:monsterkills) %>%
  ggplot(aes(val, group = result, fill = result)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~var) +
  theme(legend.position = 'bottom') +
  ggtitle("Scaled Variables")



##################____________________#########################
#variables are now scaled and ready to be compared across models


#setting up normal train and control for machine learning

final_data <- scaled_team_data %>%
  select(-c("position", "datacompleteness", "playoffs"))

# Set random seed
set.seed(313)

#using 80% of the data for train data
train <- final_data %>%
  sample_frac(.8)


test  <- anti_join(final_data, train)

# Bar chart of "result"
train %>%
  ggplot() + 
  geom_bar(aes(x = result, fill = result)) + 
  theme(legend.position = "none")

######logistic regression############################

#first model is a basic model using teamkills, teamdeaths, minionkills and monsterkills
model1 <- glm(result ~teamkills + teamdeaths + minionkills + monsterkills, data = train, family = "binomial")
summary(model1)

#adding the different types of drakes to see if anyone has a statisically significant impact on winning
model2 <- glm(result ~teamkills + teamdeaths + minionkills + monsterkills + oceans + mountains + infernals + clouds, data = train, family = "binomial")
summary(model2)

#using anovoa (analysis of variance to determine if the second model is more significant than the first)
anova(model1,model2,test='LR')
#Clearly adding the different elemental drakes does not add value to this logistic model

#adding elders, heralds and barons to the model and removing the elemental drakes
model3 <- glm(result ~teamkills + teamdeaths + minionkills + monsterkills + elders + heralds + barons, data = train, family = "binomial")
summary(model3)

#using anova to compare model1 asnd model3
anova(model1, model3, test= 'LR')
#model 3 is statistically different than model 1

#adding vision score and side to the model
model4 <- glm(result ~teamkills + teamdeaths + minionkills + monsterkills + elders + heralds + barons + visionscore + side, data = train, family = "binomial")
summary(model4)

anova(model3, model4, test= 'LR')


model5 <- glm(result ~teamkills + teamdeaths + minionkills + monsterkills + elders + heralds + barons + visionscore + side + firstbaron + firstherald + firstblood + firstdragon, data = train, family = "binomial")
summary(model5)

#model 5 seems to be the best model with the lowest AIC thus far 
anova(model4, model5, test= 'LR')


BIC(model4, model5)

#The BIC parameter penalizes models for adding more variables and as we can see the BIC for model 5 actually increases with the addition of the 4 variables
#even though anova states that model 5 was significant looking at BIC it seems that adding the variables actually hurt the model (not by much)
#given the fact that the anova comparison has a 3 star significance level I would go with model 5 over model 4
#We know that, AIC does not penalize the number of parameters included in the model as strongly as BIC.

#testing interaction
#Fit model 5 with the test data
model6 <- glm(result ~(teamkills + teamdeaths + minionkills + monsterkills + elders + heralds + barons + visionscore + side + firstbaron + firstherald + firstblood + firstdragon)^2, data = train, family = "binomial")
summary(model6)

#model 6 has a high p val but is it worth the trade of of adding more variables
anova(model5, model6, test= 'LR')

#model 6 signicantly adds to the BIC so will not be using this model 
BIC(model5, model6)



#certainly could go into more depth to check for interaction for each variable
#didnt look like any of the variables had significant interaction 
#not using interaction for any of the variables



#USING MODEL 5 WITH THE TEST DATA
finalmodel <- glm(result ~teamkills + teamdeaths + minionkills + monsterkills + elders + heralds + barons + visionscore + side + firstbaron + firstherald + firstblood + firstdragon, data = test, family = "binomial")
summary(finalmodel)


#checking accuracy of the final model
glm.probs  <- predict(finalmodel, newdata = test, type = "response")
glm.pred  <- ifelse(glm.probs > .65, "Win", "Loss")              
result.pred <- test$result
table(glm.pred, result.pred)


pred <- prediction(glm.probs, test$result)        
perf <- performance(pred, "acc")
plot(perf)

#accuracy = 1436/1479 ==0.9709263 == 97.092%


roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 


exp(cbind(OR = coef(finalmodel), confint(finalmodel)))



####### K MEANS CLUSTERING#####################
#looking at which players are the most similar

#try it with and without position variable
player.data.final <- all_data %>%
  filter(datacompleteness == "complete" & playoffs == 0 & position != "team") %>%
  select(c("position", "kills":"assists", "firstbloodkill", "dpm", "damagetakenperminute", "vspm", "cspm", "league", "player"))


tier1.leagues.kmeans <- filter(player.data.final, league %in% c("LPL", "LCK","LEC", "LCS", "PCS", "CBLOL", "LCO", "LCL", "LJL", "LLA","TCL","VCS"))

player.data.final <- tier1.leagues.kmeans
  
str(player.data.final)

player.data.final$firstbloodkill <- as.factor(player.data.final$firstbloodkill)
player.data.final$position <- as.factor(player.data.final$position)

str(player.data.final)

player.data.final.labels <- player.data.final$position

player.data.final.scaled <- player.data.final %>%
  mutate_if(is.numeric, scale)

player.data.final.scaled <- player.data.final.scaled %>%
  select(-c("player", "league", "position"))



library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#remove missing values
df.kmeans <- na.omit(player.data.final.scaled)


set.seed(123)

clust.model.1 <- kmeans(player.data.final.scaled, 5, nstart = 100)
print(clust.model.1)

table(clust.model.1$cluster, player.data.final.labels)


library(cluster)
clusplot(player.data.final.scaled, clust.model.1$cluster, color = T, shade = T, labels = 0, lines = 0) 
