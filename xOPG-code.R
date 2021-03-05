# Load packages
library(tidyverse)
library(splines)
library(caret)
library(janitor)
library(viridis)
library(ggrepel)
library(DescTools)
library(gt)

# Load in scouting data
data=read_csv("hackathon_scouting.csv")%>%
  clean_names()%>%
  separate(clock, into=c("minute","second"), sep = ":", convert = T)%>%
  mutate(y_coordinate=-1*(y_coordinate-42.5),
         x_coordinate=x_coordinate-100,
         y_coordinate_2=-1*(y_coordinate_2-42.5),
         x_coordinate_2=x_coordinate_2-100,
         goal=case_when(
           event=="Goal" ~ 1,
           TRUE ~ 0),
         zone=case_when(
           event != "Zone Entry" & x_coordinate > 25 ~ "Offensive",
           event != "Zone Entry" & x_coordinate >= -25 & x_coordinate <= 25 ~ "Neutral",
           event != "Zone Entry" & x_coordinate < -25 ~ "Defensive",
           event == "Zone Entry" ~ "Offensive"),
         seconds=duration(minutes=minute, seconds=second),
         offensive_skaters=ifelse(team==home_team,home_team_skaters,away_team_skaters),
         defending_skaters=ifelse(team==home_team,away_team_skaters,home_team_skaters))

# Function for creating new rows
insert_row <- function(df, newrow, r) {
  df[seq(r+1,nrow(df)+1),] <- df[seq(r,nrow(df)),]
  df[r,] <- newrow
  df
}

# Insert new row for completed passes so we know every position the puck was controlled at 
for(i in 1:200000){
  print(i)
  if(data$event[i]=="Play"){
    r=i+1
    df=data
    newrow=data[i,]%>%
      mutate(player=player_2,
             x_coordinate=x_coordinate_2,
             y_coordinate=y_coordinate_2,
             event="Start",
             x_coordinate_2=NA,
             y_coordinate_2=NA,
             player_2=NA)
    data=insert_row(df, newrow, r)
  }
}

# Combine where the puck is by zone
j=1
for(i in 1:nrow(data)){
  data$play[i]=j
  if (data$zone[i]!=data$zone[i+1]){
    j=j+1
  }
}

# Filter out zone entry because it disrupts possessions 
data=data%>%
  filter(event!="Zone Entry")

# Create unique possessions
k=1
for(i in 1:nrow(data)){
  data$possession[i]=k
  if(data$team[i]!=data$team[i+1] | data$event[i+1]=="Faceoff Win"){
    k=k+1
  }
}
# Possessions that contain a goal create new column
data=data%>%
  filter(home_team_skaters==5 & away_team_skaters ==5)%>%
  group_by(possession)%>%
  mutate(contains_goal=ifelse(max(goal)==1,1,0))%>%
  ungroup()

# Set up model
set.seed(80)
vector = createDataPartition(data$contains_goal, p = 0.8, list= F)%>%as.numeric()
train = data[vector,]
test = data[-vector,]

# Logistic regression model with splines for smoothing
model=glm(contains_goal ~ ns(x_coordinate,11) * ns(y_coordinate,11), 
          data=train, family = "binomial")

# Brier score on test data
BrierScore(test$contains_goal,predict(model, test, type="response"))

# Create prediction column with possession goal probability
data=data%>%
  mutate(pred=predict(model, data, type="response"))

# Rename data because new rows in 'data' will be created
model_data=data

# Create new rows to make it easy to assign values to each player
for(i in 1:200000){
  print(i)
  if(data$event[i]=="Play"){
    r=i+1
    df=data
    newrow=data[i,]%>%
      mutate(x_coordinate=x_coordinate_2,
             y_coordinate=y_coordinate_2,
             event="Completed",
             x_coordinate_2=NA,
             y_coordinate_2=NA,
             player_2=NA)
    data=insert_row(df, newrow, r)
  }
  if(data$event[i]=="Incomplete Play" & data$team[i] != data$team[i+1]){
    r=i+1
    df=data
    new_x=data$x_coordinate[i+1]
    new_y=data$y_coordinate[i+1]
    newrow=data[i,]%>%
      mutate(x_coordinate=new_x,
             y_coordinate=new_y,
             event="Turnover",
             x_coordinate_2=NA,
             y_coordinate_2=NA,
             player_2=NA)
    data=insert_row(df, newrow, r)
  }
  if(data$event[i+1]=="Takeaway" & data$event[i] != "Incomplete Play" & data$event[i] != "Turnover"){
    r=i+1
    df=data
    new_x=data$x_coordinate[i+1]
    new_y=data$y_coordinate[i+1]
    newrow=data[i,]%>%
      mutate(x_coordinate=new_x,
             y_coordinate=new_y,
             event="Turnover",
             x_coordinate_2=NA,
             y_coordinate_2=NA,
             player_2=NA)
    data=insert_row(df, newrow, r)
  }
}

# Create difference in xOPG from passes and skating
data$difference=1
for(i in 2:nrow(data)){
  print(i)
  if(data$player[i]==data$player[i-1]){
    if(data$event[i]=="Turnover"){
      data$difference[i]=-1*(data$pred[i-1])
    }else{
      data$difference[i]=data$pred[i]-data$pred[i-1]
    }
  }else{
    data$difference[i]=NA
  }
}

# Filter out noise from faceoffs and puck recovery's for accurately assigning difference
data=data%>%
  mutate(difference=ifelse(event=="Faceoff Win", NA, difference),
         difference=ifelse(event=="Puck Recovery", NA, difference))

# Separate differences by skating or passing
data$type=1
for(i in 2:nrow(data)){
  print(i)
  if(data$event[i-1]=="Play" | data$event[i-1]=="Incomplete Play"){
    
    data$type[i]="Pass"
  }else{
    data$type[i]="Skate"
  }
}