# Load packages
library(tidyverse)
library(viridis)
library(ggrepel)


# Heat map of xOPG on ice
ggplot()+
  stat_summary_hex(data=model_data,aes(x=x_coordinate,y=y_coordinate, z=pred, color=pred), bins = 70)+
  gg_rink_left()+gg_rink_right()+
  scale_fill_viridis(option = "inferno")+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "bottom")+
  labs(fill="xOPG")+
  theme(plot.title=element_text(hjust=0.5))+
  labs(title = "xOPG Heat Map")+
  guides(fill = guide_colourbar(barwidth = 10, barheight = .5))

# Create leaderboard in xOPG
leaders=data%>%
  group_by(player)%>%
  summarize(goals=round(sum(difference, na.rm=T),2),
            n=length(unique(game_date)),
            team=unique(team))%>%
  select(player, goals,n, team)%>%
  arrange(desc(goals))%>%
  ungroup()

# Visualize the leaderboard 
leaders %>%head(7)%>%
  gt() %>%
  tab_header(title = md("**Top Players in xOPG**"),
             subtitle = md("2019/2020 Erie Otters"))%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>%
  data_color(columns = vars(goals),
             colors = scales::col_numeric(
               c("white", "gray80", "goldenrod"),
               domain = c(-7,8)))%>%
  cols_label(player = "Player",
             goals = "xOPG",
             n = "Games")

# Leaders in xOPG through passing
pass_leaders=data%>%
  group_by(player, type)%>%
  summarize(goals=round(sum(difference, na.rm=T),2),
            n=length(unique(game_date)),
            team=unique(team))%>%
  filter(type=="Pass")%>%
  arrange(desc(goals))%>%
  ungroup()

# Visualize passing leaderboard
pass_leaders %>%head(7)%>%
  gt() %>%
  tab_header(title = md("**Top Players in xOPG**"),
             subtitle = md("2019/2020 Erie Otters"))%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>%
  data_color(columns = vars(goals),
             colors = scales::col_numeric(
               c("white", "gray80", "goldenrod"),
               domain = c(0.25,0.9)))%>%
  cols_label(player = "Player",
             goals = "xOPG",
             n = "Games")

# Leaders in xOPG through skating
skate_leaders=data%>%
  group_by(player, type)%>%
  summarize(goals=round(sum(difference, na.rm=T),2),
            n=length(unique(game_date)),
            team=unique(team))%>%
  filter(type=="Skate")%>%
  arrange(desc(goals))%>%
  ungroup()

# Visualize passing leaderboard
skate_leaders %>%head(7)%>%
  gt() %>%
  tab_header(title = md("**Top Players in xOPG**"),
             subtitle = md("2019/2020 Erie Otters"))%>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>%
  data_color(columns = vars(goals),
             colors = scales::col_numeric(
               c("white", "gray80", "goldenrod"),
               domain = c(1,7)))%>%
  cols_label(player = "Player",
             goals = "xOPG",
             n = "Games")

# Example possession
example=data%>%filter(possession==296)%>%
  mutate(difference=round(difference,4))
example=example[c(1,2,3,5,6,8),]%>%
  rownames_to_column()

for(i in 2:nrow(example)){
  example$x_coordinate_2[i]=example$x_coordinate[i-1]
  example$y_coordinate_2[i]=example$y_coordinate[i-1]
}
example$type[3]="Pass"
example$type[5]="Pass"
example$player[3]="Chase Stillman"
example$player[5]="Quinton Byfield"
example=example%>%mutate(type=factor(type, levels = c("Skate","Pass")))
ggplot(example,aes(x=x_coordinate,y=y_coordinate))+
  gg_rink_left()+gg_rink_right()+
  geom_label_repel(aes(label=difference))+
  geom_segment(aes(x=x_coordinate,y=y_coordinate,xend=x_coordinate_2,yend=y_coordinate_2,
                   linetype=type), size=1)+
  geom_point(aes(size=pred,color=player))+
  guides(size = guide_legend(reverse=T),
         color=F)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5),
        legend.position = "bottom")+
  labs(size="xOPG",linetype="Type",
       title="Difference in xOPG by Play",
       caption="Sudbury Wolves goal by Quinton Byfield")+
  scale_color_manual(values=c("Emmett Serensits"="goldenrod1",
                              "Chase Stillman"="red",
                              "Quinton Byfield"="blue"))