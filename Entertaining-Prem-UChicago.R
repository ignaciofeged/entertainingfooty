#Load Shit #################################################

pacman::p_load(pacman, psych, rio, tidyverse) 
library(worldfootballR)

#Data load + wrangle ############
ENGdata<- fb_advanced_match_stats(match_url=urlENG,stat_type="possession",team_or_player="team")

RTK1<-c(F,F,F,T,F,F,T,F,F,F,T,F,F,T,F,F,F,T,T,T,F,F,F,T,T,F,F,F,F,F,F,F,F,F,F)
ENGdataTouch<-ENGdata[,RTK1]
ENGdataTouch$total_touches<-ENGdataTouch$`Def 3rd_Touches`+ENGdataTouch$`Mid 3rd_Touches`

ENGdataTouchHOME<-filter(ENGdataTouch,Home_Away=="Home")
ENGdataTouchAway<-filter(ENGdataTouch,Home_Away=="Away")

RTK1.1<-c(F,F,F,F,T,T,F,F,F,T)
ENGdataTouchHOME2<-ENGdataTouchHOME[,RTK1.1]
colnames(ENGdataTouchHOME2)[4]<-"total_touchesHOME"

RTK1.2<-c(F,F,F,F,T,T,F,F,F,T)
ENGdataTouchAway2<-ENGdataTouchAway[,RTK1.2]
colnames(ENGdataTouchAway2)[3]<-"total_touchesAWAY"

ENGdataTouchFINAL<-cbind(ENGdataTouchHOME2,ENGdataTouchAway2)


ENGdataDefraw<- fb_advanced_match_stats(match_url=urlENG,stat_type="defense",team_or_player="team")

RTK2<-c(F,F,F,T,F,F,T,F,F,F,T,F,F,T,F,F,F,T,T,T,F,F,F,T,F,F,F,F,F,F,F,F,F,F,T,F,F,F)
ENGdataDef<-ENGdataDefraw[,RTK2]
ENGdataDef$DefActions<-ENGdataDef$`Tkl+Int`-ENGdataDef$`Def 3rd_Tackles`
ENGdataDefHOME<-filter(ENGdataDef,Home_Away=="Home")
ENGdataDefAWAY<-filter(ENGdataDef,Home_Away=="Away")


RTK2.1<-c(F,F,F,F,T,T,T,F,F,T)
ENGdataDefHOME2<-ENGdataDefHOME[,RTK2.1]
colnames(ENGdataDefHOME2)[4]<-"DefActions_HOME"

RTK2.2<-c(F,F,F,F,T,T,F,F,F,T)
ENGdataDefAWAY2<-ENGdataDefAWAY[,RTK2.2]
colnames(ENGdataDefAWAY2)[3]<-"DefActions_AWAY"

ENGdataDEF_FINAL<-cbind(ENGdataDefHOME2,ENGdataDefAWAY2)


ENGCombine<-cbind(ENGdataTouchFINAL,ENGdataDEF_FINAL)
RTK3<-c(T,T,F,T,F,T,T,F,F,F,T,F,F,T)
ENGCombine2<-ENGCombine[,RTK3]
colnames(ENGCombine2)[2]<-"Home_Team"
colnames(ENGCombine2)[4]<-"Away_Team"

ENGCombine2$TPDA<-(ENGCombine2$total_touchesHOME/ENGCombine2$DefActions_AWAY)+(ENGCombine2$total_touchesAWAY/ENGCombine2$DefActions_HOME)



RTK4<-c(T,T,T,T,T,F,T,F,F,F)
ENGdataxG<-ENGdataDef[,RTK4]
ENGdataxG2<-filter(ENGdataxG,Home_Away=="Home")
ENGdataxG2$xGwatch<-(ENGdataxG2$Home_xG+ENGdataxG2$Away_xG)-((abs(ENGdataxG2$Home_xG-ENGdataxG2$Away_xG))*.5)

RTK4.1<-c(F,T,F,T,T,F,T)
ENGdataxG3<-ENGdataxG2[,RTK4.1]

ENGCombine3<-cbind(ENGCombine2,ENGdataxG3)
RTK4.9<-c(F,T,T,T,T,T,T,T,T,T,T,T)
ENGCombine3<-ENGCombine3[,RTK4.9]


ENGTable<-fb_season_team_stats("ENG", "M", 2022, "1st", "league_table")
RTK5<-c(F,F,F,F,T,F,F,F,F,F,F,F,F,F,T,F,F,F,F,F,F,F,F,F)
ENGTable2<-ENGTable[,RTK5]
colnames(ENGTable2)[1]<-"Home_Team"
colnames(ENGTable2)[2]<-"Home_Pts"

##Create Points Scale###############

mapping <- c("Arsenal" = 69, "Aston Villa" = 45, "Brentford" = 46, "Brighton & Hove Albion" = 51,"Burnley"=35,"Chelsea"=74,
             "Crystal Palace"=48,"Everton"=39,"Leeds United"=38,"Leicester City"=52,"Liverpool"=92,
             "Manchester City"=93,"Manchester United"=58,"Newcastle United"=49,"Norwich City"=22,
             "Southampton"=40,"Tottenham Hotspur"=71,"Watford"=23,"West Ham United"=56,"Wolverhampton Wanderers"=51)

ENGCombine3$Home_Club<-ENGCombine3$Home_Team
ENGCombine3$Away_Club<-ENGCombine3$Away_Team

ENGCombine3$Home_Team <- mapping[ENGCombine3$Home_Team]
ENGCombine3$Away_Team <- mapping[ENGCombine3$Away_Team]
ENGCombine3$avgPTS<-(ENGCombine3$Home_Team+ENGCombine3$Away_Team)/2

##Plots############
x_mid<-median(ENGCombine3$xGwatch)
y_mid<-median(ENGCombine3$TPDA)

Eplotraw1.3<-ggplot(data = ENGCombine3, aes(x=xGwatch, y=TPDA,color=avgPTS))+geom_point(size=2)+
  geom_vline(xintercept = x_mid) + 
  geom_hline(yintercept = y_mid)+
  scale_colour_gradientn(colours = heat.colors(10))

Eplotraw1.3        

Eplot1<-Eplotraw1.3+annotate("text", x = 5, y = 200, label = "Lots of Opptunities For Both Teams,",color="#1D9A6C")+ #top right
  annotate("text", x = 5.55, y = 200, label = "Less Chaotic",color="#D22900")+
  annotate("text", x = 5, y = 10, label = "Lots of Opptunities For Both Teams,",color="#1D9A6C")+ #bottom right
  annotate("text", x = 5.55, y = 10, label = "More Chaotic",color="#1D9A6C")+ #bottom right2
  annotate("text", x = 1, y = 200, label = "Fewer Opptunities For Both Teams,",color="#D22900")+ #top left
  annotate("text", x = 1.55, y = 200, label = "Less Chaotic",color="#D22900")+ #top left2
  annotate("text", x = 1, y = 10, label = "Fewer Opptunities For Both Teams,",color="#D22900")+#bottom left
  annotate("text", x = 1.55, y = 10, label = "More Chaotic",color="#1D9A6C")+#bottom left2
  annotate("text", x = 5.2, y = 40, label = "West Ham v Leeds")+
  annotate("text", x = 4.75, y = 52, label = "Crystal Palace v West Ham")+
  annotate("text", x = 4.5, y = 30, label = "Leicester v Watford")+
  annotate("text", x = 1, y = 72, label = "Chelsea v Man City")+
  annotate("text", x = 2.05, y = 66, label = "Liverpool v Man City")


Eplot1


Eplot2<-Eplot1 + labs(x = "Number of Quality Chances for Both Teams \n (Total xG - .5 X the difference in xG)",
                      y="Level of Defensive Pressure in the Attacking and Midfield 3rds \n (TPDA)",
                      title = "What Were Last Year's Most Entertaing Premier League Matches?",
                      color="Quality of the Teams")+
  theme(plot.title = element_text(hjust = 0.5))


Eplot2


# CLEAN UP #################################################
rm(list = ls()) 
detach("package:datasets", unload = TRUE)  # For base
dev.off()  # But only if there IS a plot
cat("\014")
