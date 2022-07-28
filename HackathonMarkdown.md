Delayed Shot Opportunities in Corner Sequences at the UEFA Men’s 2020
EURO
================
Arden Holden
2022-07-12

## Corner Analysis

The first step in my project was to look at all the corners that were
taken at the Euro. I extracted some of the information that I thought
would be important to the project and put it in an excel document. I
looked at the corners on a game by game basis.

``` r
library (StatsBombR)
library(tidyverse)

Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
Matches = Matches %>% filter(competition.competition_id==55)
data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)

events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)
events = events %>% left_join(data360, by = "id")
events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)
```

``` r
#Turkey vs Italy
TUR_ITA<<-Matches %>%
  filter(match_id==3788741)
TUR_ITA_Data<-StatsBombFreeEvents(MatchesDF = TUR_ITA, Parallel = T)

C_TUR_ITA<-TUR_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Turkey")
C1TUR_ITA<- TUR_ITA_Data %>%
  filter(minute>=68 & minute<=73)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1TUR: foul was called on the corner
C2TUR_ITA<-TUR_ITA_Data %>%
  filter(minute>=92 & minute<=97)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2TUR: pass assisted shot, but resulted in goal kick for Italy?
#TUR: 1 mid, 1 long corner

C_ITA_TUR<-TUR_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Italy")
C1ITA_TUR<-TUR_ITA_Data %>%
  filter(minute>=21 & minute<=26)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1ITA: corner assisted shot, presumably saved and out for another corner
#C2ITA: corner assisted shot, foul called on ITA
C3ITA_TUR<-TUR_ITA_Data %>%
  filter(minute>=31 & minute<=36)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3ITA: short, throw in
C4ITA_TUR<-TUR_ITA_Data %>%
  filter(minute>=46 & minute<=51)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4ITA: foul called on Italy on short corner
C5ITA_TUR<-TUR_ITA_Data %>%
  filter(minute>=57 & minute<=62)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5ITA: Turkey cleared, but Italy recovered ball and was able to get a shot off 8 seconds after corner was taken
#C6ITA: Turkey cleared, but Italy recovered ball and was able to get shot off 4 seconds after corner was taken (blocked)
C7ITA_TUR<-TUR_ITA_Data %>%
  filter(minute>=76 & minute<=83)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7ITA: Italy able to keep possession for a 1.5 minutes after corner was taken (but it went back to Donnarumma)
C8ITA_TUR<-TUR_ITA_Data %>%
  filter(minute>=85 & minute<=90)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C8ITA: foul called on corner
#ITA: 2 short corners, 1 mid corner, 5 long corners

#Denmark vs Finland
DEN_FIN<-Matches %>%
  filter(match_id==3788742)
DEN_FIN_Data<-StatsBombFreeEvents(MatchesDF = DEN_FIN, Parallel = T)

C_DEN_FIN<-DEN_FIN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Denmark")
C1DEN_FIN<- DEN_FIN_Data %>%
  filter(minute>=5 & minute<=10)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1DEN: corner sequence ended in foul called on Denmark
#C2DEN: clearance, ball recovery, and shot from corner sequence 
C3DEN_FIN<- DEN_FIN_Data %>%
  filter(minute>=12 & minute<=17)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3DEN: foul called on Denmark
#C4DEN: contested play, but sequence ends with a ball recovery from Finland
C5DEN_FIN<- DEN_FIN_Data %>%
  filter(minute>=21 & minute<=26)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5DEN: shot went out for goal kick
C6DEN_FIN<- DEN_FIN_Data %>%
  filter(minute>=32 & minute<=37)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6DEN: corner cleared by Finland and recoverd by Denmark
C7DEN_FIN<- DEN_FIN_Data %>%
  filter(minute>=47 & minute<=52)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7DEN: corner cleared by Finland & recoverd by Denmark. Sequence ended with a free kick for Denmark in attacking third
#C8DEN: spell of possession that ends in throw in for Denmark
C9DEN_FIN<- DEN_FIN_Data %>%
  filter(minute>=81 & minute<=86)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C9DEN: sequence ended in a throw in for Finland
#DEN: all long corners

#Belgium vs Russia
BEL_RUS<-Matches %>%
  filter(match_id==3788743)
BEL_RUS_Data<-StatsBombFreeEvents(MatchesDF = BEL_RUS, Parallel = T)

C_BEL_RUS<- BEL_RUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Belgium")
C1BEL_RUS<- BEL_RUS_Data %>%
  filter(minute>=23 & minute<=28)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1BEL: corner cleared, Belgium retain possession
C2BEL_RUS<- BEL_RUS_Data %>%
  filter(minute>=61 & minute<=66)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2BEL: Russia recovered ball and sequence ended w/ throw in for Belgium
#BEL: all long corners

C_RUS_BEL<- BEL_RUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Russia")
C1RUS_BEL<- BEL_RUS_Data %>%
  filter(minute>=6 & minute<=9)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1RUS: went out for a Belgium throw in. Likely missed everyone in the box
C2RUS_BEL<- BEL_RUS_Data %>%
  filter(minute>=13 & minute<=16)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2RUS: shot saved by Cortouis
C3RUS_BEL<- BEL_RUS_Data %>%
  filter(minute>=58 & minute<=61)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3RUS: cleared by Belgium for Russia throw in
C4RUS_BEL<- BEL_RUS_Data %>%
  filter(minute>=83 & minute<=86)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4RUS: cleared and sequence ends with foul called on Russia
#RUS: 3 long, 1 mid

#Wales vs Switzerland
WAL_SUI<-Matches %>%
  filter(match_id==3788744)
WAL_SUI_Data<-StatsBombFreeEvents(MatchesDF = WAL_SUI, Parallel = T)

C_WAL_SUI<- WAL_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Wales")
C1WAL_SUI<- WAL_SUI_Data %>%
  filter(minute>=15 & minute<=18)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1WAL: shot was blocked by Switzerland
C2WAL_SUI<- WAL_SUI_Data %>%
  filter(minute>=60 & minute<=63)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2WAL: shot comes after contested play and blocked shot results in corner
#C3WAL: sequence ends with Swiss possession
C4WAL_SUI<- WAL_SUI_Data %>%
  filter(minute>=73 & minute<=76)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4WAL: scored from corner sequence (short corner)
#WAL: 3 long, 1 short

C_SUI_WAL<- WAL_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Switzerland")
C1SUI_WAL<- WAL_SUI_Data %>%
  filter(minute>=6 & minute<=9)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SUI: ball ends up at keeper
C2SUI_WAL<- WAL_SUI_Data %>%
  filter(minute>=19 & minute<=22)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SUI: corner assisted shot, saved out for another corner
#C3SUI: mid corner sequence ends up with gk
C4SUI_WAL<- WAL_SUI_Data %>%
  filter(minute>=33 & minute<=38)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4SUI: corner assisted shot and was blocked, out for corner
#C5SUI: Switzerland had possession after corner, Wales had 3 clearances. Sequence laste 49 seconds
#C6SUI: Switzerland had possession after corner, sequence lasted 1 min 13 sec
C7SUI_WAL<- WAL_SUI_Data %>%
  filter(minute>=44 & minute<=50)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7SUI: corner asssisted shot, sequence ended with throw in for Switzerland
#C8SUI: corner assisted shot, scored from corner
C9SUI_WAL<- WAL_SUI_Data %>%
  filter(minute>=89 & minute<=95)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C9SUI: corner asssisted shot, saved for corner
#C10SUI: cleared by Wales
#C11SUI: sequence ends in foul called on Switzerland
#C12SUI: sequence ends up with gk
#SUI: 11 long, 1 short

#England vs Croatia
ENG_CRO<-Matches %>%
  filter(match_id==3788745)
ENG_CRO_Data<-StatsBombFreeEvents(MatchesDF = ENG_CRO, Parallel = T)

C_ENG_CRO<- ENG_CRO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="England")
C1ENG_CRO<- ENG_CRO_Data %>%
  filter(minute>=8 & minute<=11)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1ENG: long, throw in

C_CRO_ENG<- ENG_CRO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Croatia")
C1CRO_ENG<- ENG_CRO_Data %>%
  filter(minute>=69 & minute<=71)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1CRO: long, shot went out for goal kick

#Netherlands vs Ukraine
NED_UKR<-Matches %>%
  filter(match_id==3788746)
NED_UKR_Data<-StatsBombFreeEvents(MatchesDF = NED_UKR, Parallel = T)

C_NED_UKR<- NED_UKR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Netherlands")
C1NED_UKR<- NED_UKR_Data %>%
  filter(minute>=5 & minute<=7)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1NED: mid, possession (2 shots)
C2NED_UKR<- NED_UKR_Data %>%
  filter(minute>=36 & minute<=38)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2NED: short, goal kick
C3NED_UKR<- NED_UKR_Data %>%
  filter(minute>=43 & minute<=45)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3NED: mid, goal kick
C4NED_UKR<- NED_UKR_Data %>%
  filter(minute>=64 & minute<=66)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4NED: long, throw in
C5NED_UKR<- NED_UKR_Data %>%
  filter(minute>=70 & minute<=72)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5NED: long, lost possession

C_UKR_NED<- NED_UKR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Ukraine")
C1UKR_NED<- NED_UKR_Data %>%
  filter(minute>=3 & minute<=5)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1UKR: long, shot out for goal kick

#North Macedonia vs Austria
NMA_AUS<-Matches %>%
  filter(match_id==3788747)
NMA_AUS_Data<-StatsBombFreeEvents(MatchesDF = NMA_AUS, Parallel = T)

C_AUS_NMA<- NMA_AUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Austria")
C1AUS_NMA<- NMA_AUS_Data %>%
  filter(minute>=34 & minute<=36)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1AUS: long, foul
C2AUS_NMA<- NMA_AUS_Data %>%
  filter(minute>=45 & minute<=45)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2AUS: mid, throw in
C3AUS_NMA<- NMA_AUS_Data %>%
  filter(minute>=66 & minute<=68)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3AUS: mid, shot out for goal kick
C4AUS_NMA<- NMA_AUS_Data %>%
  filter(minute>=84 & minute<=86)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4AUS: long, lost possession

#Poland vs Slovakia
POL_SLO<-Matches %>%
  filter(match_id==3788749)
POL_SLO_Data<-StatsBombFreeEvents(MatchesDF = POL_SLO, Parallel = T)

C_POL_SLO<- POL_SLO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Poland")
C1POL_SLO<- POL_SLO_Data %>%
  filter(minute>=52 & minute<=54)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1POL: mid, shot out for goal kick
C2POL_SLO<- POL_SLO_Data %>%
  filter(minute>=58 & minute<=60)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2POL: long, clearance out for corner
#C3POL: mid, goal kick
C4POL_SLO<- POL_SLO_Data %>%
  filter(minute>=84 & minute<=88)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4POL: long, goal kick
#C5POL: long, goal kick

C_SLO_POL<- POL_SLO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Slovakia")
C1SLO_POL<- POL_SLO_Data %>%
  filter(minute>=26 & minute<=28)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SLO: long, shot out for goal kick
C2SLO_POL<- POL_SLO_Data %>%
  filter(minute>=68 & minute<=70)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SLO: mid, goal

#Sweden vs Spain
SWE_ESP<-Matches %>%
  filter(match_id==3788750)
SWE_ESP_Data<-StatsBombFreeEvents(MatchesDF = SWE_ESP, Parallel = T)

C_SWE_ESP<- SWE_ESP_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Sweden")
C1SWE_ESP<- SWE_ESP_Data %>%
  filter(minute>=36 & minute<=38)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SWE: mid, clearance

C_ESP_SWE<- SWE_ESP_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Spain")
C1ESP_SWE<- SWE_ESP_Data %>%
  filter(minute>=3 & minute<=7)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1ESP: long, possession
#C2ESP: long, goal kick (2 shots)
C3ESP_SWE<- SWE_ESP_Data %>%
  filter(minute>=15 & minute<=17)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3ESP: short (long), possession
C4ESP_SWE<- SWE_ESP_Data %>%
  filter(minute>=24 & minute<=27)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4ESP: short(long), clearance
C5ESP_SWE<- SWE_ESP_Data %>%
  filter(minute>=43 & minute<=47)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5ESP: long, throw in
C6ESP_SWE<- SWE_ESP_Data %>%
  filter(minute>=95 & minute<=97)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6ESP: long, goal kick (shot)

#Scotland vs Czech Republic
SCO_CZE<-Matches %>%
  filter(match_id==3788748)
SCO_CZE_Data<-StatsBombFreeEvents(MatchesDF = SCO_CZE, Parallel = T)

C_SCO_CZE<- SCO_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Scotland")
C1SCO_CZE<- SCO_CZE_Data %>%
  filter(minute>=5 & minute<=7)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SCO: long, throw in
C2SCO_CZE<- SCO_CZE_Data %>%
  filter(minute>=31 & minute<=33)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SCO: long, throw in
C3SCO_CZE<- SCO_CZE_Data %>%
  filter(minute>=61 & minute<=65)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3SCO: long, goal kick
#C4SCO: long, possession
C5SCO_CZE<- SCO_CZE_Data %>%
  filter(minute>=83 & minute<=85)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5: long, keeper claim
C6SCO_CZE<- SCO_CZE_Data %>%
  filter(minute>=94 & minute<=96)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6SCO: long, possession

C_CZE_SCO<- SCO_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Czech Republic")
C1CZE_SCO<- SCO_CZE_Data %>%
  filter(minute>=15 & minute<=20)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1CZE: long, goal kick
#C2CZE: mid, throw in
C3CZE_SCO<- SCO_CZE_Data %>%
  filter(minute>=39 & minute<=42)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3CZE: long, corner
#C4CZE: long, corner
#C5CZE: long, goal
C6CZE_SCO<- SCO_CZE_Data %>%
  filter(minute>=46 & minute<=48)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6CZE: long, lost possession, shot for Scotland

#Germany vs France
GER_FRA<-Matches %>%
  filter(match_id==3788751)
GER_FRA_Data<-StatsBombFreeEvents(MatchesDF = GER_FRA, Parallel = T)

C_GER_FRA<- GER_FRA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Germany")
C1GER_FRA<- GER_FRA_Data %>%
  filter(minute>=61 & minute<=64)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1GER: mid, throw in
#C2GER: long, possession
C3GER_FRA<- GER_FRA_Data %>%
  filter(minute>=69 & minute<=71)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3GER: long, shot saved by keeper
#C4GER: long, possession
C5GER_FRA<- GER_FRA_Data %>%
  filter(minute>=88 & minute<=90)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5GER: mid, possession

C_FRA_GER<- GER_FRA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="France")
C1FRA_GER<- GER_FRA_Data %>%
  filter(minute>=15 & minute<=18)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1FRA: long, shot out for goal kick
#C2FRA: mid, lost possession
C3FRA_GER<- GER_FRA_Data %>%
  filter(minute>=35 & minute<=37)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3FRA: mid, lost possession

#Hungary vs Portugal
HUN_POR<-Matches %>%
  filter(match_id==3788752)
HUN_POR_Data<-StatsBombFreeEvents(MatchesDF = HUN_POR, Parallel = T)

C_POR_HUN<- HUN_POR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Portugal")
C1POR_HUN<- HUN_POR_Data %>%
  filter(minute>=22 & minute<=24)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1POR: mid, free kick/foul
C2POR_HUN<- HUN_POR_Data %>%
  filter(minute>=28 & minute<=30)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2POR: short, throw in
C3POR_HUN<- HUN_POR_Data %>%
  filter(minute>=46 & minute<=47)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3POR: long, throw in
C4POR_HUN<- HUN_POR_Data %>%
  filter(minute>=65 & minute<=69)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4POR: long, throw in
#C5POR: short, goal kick
#C6POR: short, throw in

#Finland vs Russia
FIN_RUS<-Matches %>%
  filter(match_id==3788753)
FIN_RUS_Data<-StatsBombFreeEvents(MatchesDF = FIN_RUS, Parallel = T)

C_FIN_RUS<- FIN_RUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Finland")
C1FIN_RUS<- FIN_RUS_Data %>%
  filter(minute>=20 & minute<=22)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1FIN: short, shot out for goal kick

C_RUS_FIN<- FIN_RUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Russia")
C1RUS_FIN<- FIN_RUS_Data %>%
  filter(minute>=1 & minute<=3)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1RUS: long, possession
C2RUS_FIN<- FIN_RUS_Data %>%
  filter(minute>=30 & minute<=32)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2RUS: long, shot out for goal kick
C3RUS_FIN<- FIN_RUS_Data %>%
  filter(minute>=38 & minute<=40)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3RUS: long, foul
C4RUS_FIN<- FIN_RUS_Data %>%
  filter(minute>=72 & minute<=74)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4RUS: long, keeper claim

#Italy vs Switzerland
ITA_SUI<-Matches %>%
  filter(match_id==3788754)
ITA_SUI_Data<-StatsBombFreeEvents(MatchesDF = ITA_SUI, Parallel = T)

C_ITA_SUI<- ITA_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Italy")
C1ITA_SUI<- ITA_SUI_Data %>%
  filter(minute>=18 & minute<=20)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1ITA: long, free kick/foul
C2ITA_SUI<- ITA_SUI_Data %>%
  filter(minute>=39 & minute<=41)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2ITA: long, corner
#C3ITA: long, possession

C_SUI_ITA<- ITA_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Switzerland")
C1SUI_ITA<- ITA_SUI_Data %>%
  filter(minute>=35 & minute<=37)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SUI: long, counter attack (shot for Italy)
C2SUI_ITA<- ITA_SUI_Data %>%
  filter(minute>=45 & minute<=47)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SUI: long, shot out for goal kick

#Turkey vs Wales
TUR_WAL<-Matches %>%
  filter(match_id==3788755)
TUR_WAL_Data<-StatsBombFreeEvents(MatchesDF = TUR_WAL, Parallel = T)

C_TUR_WAL<- TUR_WAL_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Turkey")
C1TUR_WAL<- TUR_WAL_Data %>%
  filter(minute>=9 & minute<=11)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1TUR: long, lost possession
C2TUR_WAL<- TUR_WAL_Data %>%
  filter(minute>=28 & minute<=30)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2TUR: long, corner
#C3TUR: long, throw in
C4TUR_WAL<- TUR_WAL_Data %>%
  filter(minute>=43 & minute<=45)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4TUR: long, keeper claim
C5TUR_WAL<- TUR_WAL_Data %>%
  filter(minute>=53 & minute<=57)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5TUR: long, shot out for goal kick
#C6TUR: long, foul/free kick
C7TUR_WAL<- TUR_WAL_Data %>%
  filter(minute>=62 & minute<=64)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7TUR: long, shot saved by keeper
C8TUR_WAL<- TUR_WAL_Data %>%
  filter(minute>=75 & minute<=77)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C8TUR: long, throw in
C9TUR_WAL<- TUR_WAL_Data %>%
  filter(minute>=86 & minute<=88)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C9TUR: long, shot deflected out for corner
#C10TUR: long, throw in

C_WAL_TUR<- TUR_WAL_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Wales")
C1WAL_TUR<- TUR_WAL_Data %>%
  filter(minute>=6 & minute<=11)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1WAL: short, lost possession
#C2WAL: long, goal kick
C3WAL_TUR<- TUR_WAL_Data %>%
  filter(minute>=47 & minute<=49)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3WAL: long, lost possession
C4WAL_TUR<- TUR_WAL_Data %>%
  filter(minute>=52 & minute<=54)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4WAL: long, goal kick
C5WAL_TUR<- TUR_WAL_Data %>%
  filter(minute>=66 & minute<=68)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5WAL: long, foul/free kick
C6WAL_TUR<- TUR_WAL_Data %>%
  filter(minute>=93 & minute<=95)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6WAL: short, corner
#C7WAL: short, goal

#Ukraine vs North Macedonia
UKR_NMA<-Matches %>%
  filter(match_id==3788758)
UKR_NMA_Data<-StatsBombFreeEvents(MatchesDF = UKR_NMA, Parallel = T)

C_UKR_NMA<- UKR_NMA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Ukraine")
C1UKR_NMA<- UKR_NMA_Data %>%
  filter(minute>=7 & minute<=12)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1UKR: long, throw in
#C2UKR: long, goal kick
#C3UKR: long, lost possession
C4UKR_NMA<- UKR_NMA_Data %>%
  filter(minute>=28 & minute<=30)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4UKR: long, goal
C5UKR_NMA<- UKR_NMA_Data %>%
  filter(minute>=50 & minute<=52)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5UKR: long, shot saved

C_NMA_UKR<- UKR_NMA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="North Macedonia")
C1NMA_UKR<- UKR_NMA_Data %>%
  filter(minute>=47 & minute<=49)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1NMA: mid, goal kick
C2NMA_UKR<- UKR_NMA_Data %>%
  filter(minute>=53 & minute<=56)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2NMA: short, goal
C3NMA_UKR<- UKR_NMA_Data %>%
  filter(minute>=67 & minute<=70)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3NMA: long, keeper claim
#C4NMA: mid, goal kick
C5NMA_UKR<- UKR_NMA_Data %>%
  filter(minute>=90 & minute<=93)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5NMA: long, foul

#Belgium vs Denmark
BEL_DEN<-Matches %>%
  filter(match_id==3788757)
BEL_DEN_Data<-StatsBombFreeEvents(MatchesDF = BEL_DEN, Parallel = T)

C_BEL_DEN<- BEL_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Belgium")
C1BEL_DEN<- BEL_DEN_Data %>%
  filter(minute>=31 & minute<=33)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1BEL: short, corner
#C2BEL: long, throw in
C3BEL_DEN<- BEL_DEN_Data %>%
  filter(minute>=64 & minute<=66)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3BEL: mid, possession
C4BEL_DEN<- BEL_DEN_Data %>%
  filter(minute>=94 & minute<=97)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4BEL: short, shot for opposition (counter)

C_DEN_BEL<- BEL_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Denmark")
C1DEN_BEL<- BEL_DEN_Data %>%
  filter(minute>=56 & minute<=58)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1DEN: short, foul (for Denmark, not in box)
C2DEN_BEL<- BEL_DEN_Data %>%
  filter(minute>=76 & minute<=78)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2DEN: long, foul

#Netherlands vs Austria
NED_AUS<-Matches %>%
  filter(match_id==3788756)
NED_AUS_Data<-StatsBombFreeEvents(MatchesDF = NED_AUS, Parallel = T)

C_NED_AUS<- NED_AUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Netherlands")
C1NED_AUS<- NED_AUS_Data %>%
  filter(minute>=60 & minute<=62)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1NED: long, corner
#C2NED: long, goal kick

C_AUS_NED<- NED_AUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Austria")
C1AUS_NED<- NED_AUS_Data %>%
  filter(minute>=52 & minute<=54)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1AUS: long, keeper claim
C2AUS_NED<- NED_AUS_Data %>%
  filter(minute>=68 & minute<=72)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2AUS: long, goal kick
#C3AUS: long, corner
#C4AUS: long, lost possession
C5AUS_NED<- NED_AUS_Data %>%
  filter(minute>=85 & minute<=88)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5AUS: long, throw in

#Scotland vs England
SCO_ENG<-Matches %>%
  filter(match_id==3788759)
SCO_ENG_Data<-StatsBombFreeEvents(MatchesDF = SCO_ENG, Parallel = T)

C_SCO_ENG<- SCO_ENG_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Scotland")
C1SCO_ENG<- SCO_ENG_Data %>%
  filter(minute>=38 & minute<=40)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SCO: long, throw in
C2SCO_ENG<- SCO_ENG_Data %>%
  filter(minute>=44 & minute<=46)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SCO: mid, goal
C3SCO_ENG<- SCO_ENG_Data %>%
  filter(minute>=61 & minute<=65)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3SCO: long, foul
#C4SCO: long, throw in
C5SCO_ENG<- SCO_ENG_Data %>%
  filter(minute>=81 & minute<=84)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5SCO: long, lost possession

C_ENG_SCO<- SCO_ENG_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="England")
C1ENG_SCO<- SCO_ENG_Data %>%
  filter(minute>=10 & minute<=13)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1ENG: long, lost possession
C2ENG_SCO<- SCO_ENG_Data %>%
  filter(minute>=19 & minute<=21)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2ENG: long, possession
C3ENG_SCO<- SCO_ENG_Data %>%
  filter(minute>=46 & minute<=49)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3ENG: mid, possession
#C4ENG: mid, goal kick
C5ENG_SCO<- SCO_ENG_Data %>%
  filter(minute>=59 & minute<=61)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5ENG: long, lost possession
C6ENG_SCO<- SCO_ENG_Data %>%
  filter(minute>=65 & minute<=67)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6ENG: mid, foul (for England, not inside area)

#Sweden vs Slovakia
SWE_SLO<-Matches %>%
  filter(match_id==3788761)
SWE_SLO_Data<-StatsBombFreeEvents(MatchesDF = SWE_SLO, Parallel = T)

C_SWE_SLO<- SWE_SLO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Sweden")
C1SWE_SLO<- SWE_SLO_Data %>%
  filter(minute>=6 & minute<=8)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SWE: mid, lost possession
C2SWE_SLO<- SWE_SLO_Data %>%
  filter(minute>=20 & minute<=22)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SWE: long, foul
C3SWE_SLO<- SWE_SLO_Data %>%
  filter(minute>=46 & minute<=49)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3SWE: long, goal kick
#C4SWE: long, lost possession
C5SWE_SLO<- SWE_SLO_Data %>%
  filter(minute>=59 & minute<=62)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5SWE: long, goal kick
#C6SWE: long, throw in
C7SWE_SLO<- SWE_SLO_Data %>%
  filter(minute>=81 & minute<=83)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7SWE: long, lost possession

C_SLO_SWE<- SWE_SLO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Slovakia")
C1SLO_SWE<- SWE_SLO_Data %>%
  filter(minute>=4 & minute<=6)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SLO: long, goal kick
C2SLO_SWE<- SWE_SLO_Data %>%
  filter(minute>=36 & minute<=39)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SLO: long, possession
C3SLO_SWE<- SWE_SLO_Data %>%
  filter(minute>=49 & minute<=51)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3SLO: long, goal kick
C4SLO_SWE<- SWE_SLO_Data %>%
  filter(minute>=86 & minute<=88)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4SLO: long, foul
C5SLO_SWE<- SWE_SLO_Data %>%
  filter(minute>=91 & minute<=95)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5SLO: long, foul
#C6SLO: long, throw in

#Croatia vs Czech Republic
CRO_CZE<-Matches %>%
  filter(match_id==3788760)
CRO_CZE_Data<-StatsBombFreeEvents(MatchesDF = CRO_CZE, Parallel = T)

C_CRO_CZE<- CRO_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Croatia")
C1CRO_CZE<- CRO_CZE_Data %>%
  filter(minute>=6 & minute<=8)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1CRO: short, keeper claim
C2CRO_CZE<- CRO_CZE_Data %>%
  filter(minute>=22 & minute<=24)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2CRO: mid, shot saved
C3CRO_CZE<- CRO_CZE_Data %>%
  filter(minute>=88 & minute<=90)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3CRO: long, corner
#C4CRO: long, lost possession

C_CZE_CRO<- CRO_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Czech Republic")
C1CZE_CRO<- CRO_CZE_Data %>%
  filter(minute>=3 & minute<=5)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1CZE: long, goal kick
C2CZE_CRO<- CRO_CZE_Data %>%
  filter(minute>=32 & minute<=40)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2CZE: long, penalty
C3CZE_CRO<- CRO_CZE_Data %>%
  filter(minute>=60 & minute<=66)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3CZE: long, goal kick
#C4CZE: long, keeper claim

#France vs Hungary
FRA_HUN<-Matches %>%
  filter(match_id==3788763)
FRA_HUN_Data<-StatsBombFreeEvents(MatchesDF = FRA_HUN, Parallel = T)

C_FRA_HUN<- FRA_HUN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="France")
C1FRA_HUN<- FRA_HUN_Data %>%
  filter(minute>=8 & minute<=10)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1FRA: mid, foul
C2FRA_HUN<- FRA_HUN_Data %>%
  filter(minute>=49 & minute<=51)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2FRA: mid, lost possession
C3FRA_HUN<- FRA_HUN_Data %>%
  filter(minute>=80 & minute<=82)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3FRA: long, keeper claim

C_HUN_FRA<- FRA_HUN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Hungary")
C1HUN_FRA<- FRA_HUN_Data %>%
  filter(minute>=10 & minute<=12)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1HUN: long, foul

#Germany vs Portugal
GER_POR<-Matches %>%
  filter(match_id==3788764)
GER_POR_Data<-StatsBombFreeEvents(MatchesDF = GER_POR, Parallel = T)

C_GER_POR<- GER_POR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Germany")
C1GER_POR<- GER_POR_Data %>%
  filter(minute>=14 & minute<=16)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1GER: long, counter attack and goal for opposition
C2GER_POR<- GER_POR_Data %>%
  filter(minute>=23 & minute<=26)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2GER: long, goal kick
C3GER_POR<- GER_POR_Data %>%
  filter(minute>=41 & minute<=43)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3GER: long, goal kick

C_POR_GER<- GER_POR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Portugal")
C1POR_GER<- GER_POR_Data %>%
  filter(minute>=21 & minute<=24)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1POR: mid, goal kick
C2POR_GER<- GER_POR_Data %>%
  filter(minute>=44 & minute<=46)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2POR: short, throw in
C3POR_GER<- GER_POR_Data %>%
  filter(minute>=77 & minute<=80)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3POR: mid, lost possession
C4POR_GER<- GER_POR_Data %>%
  filter(minute>=88 & minute<=91)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4POR: long, corner
#C5POR: mid, throw in
C6POR_GER<- GER_POR_Data %>%
  filter(minute>=93 & minute<=96)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6POR: long, clearance

#Spain vs Poland
ESP_POL<-Matches %>%
  filter(match_id==3788762)
ESP_POL_Data<-StatsBombFreeEvents(MatchesDF = ESP_POL, Parallel = T)

C_ESP_POL<- ESP_POL_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Spain")
C1ESP_POL<- ESP_POL_Data %>%
  filter(minute>=16 & minute<=21)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1ESP: short, possession
#C2ESP: short, possession
#C3ESP: short, lost possession
C4ESP_POL<- ESP_POL_Data %>%
  filter(minute>=37 & minute<=39)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4ESP: short, throw in
C5ESP_POL<- ESP_POL_Data %>%
  filter(minute>=46 & minute<=48)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5ESP: long, clearance
C6ESP_POL<- ESP_POL_Data %>%
  filter(minute>=64 & minute<=69)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6ESP: mid, foul
#C7ESP: long, foul

C_POL_ESP<- ESP_POL_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Poland")
C1POL_ESP<- ESP_POL_Data %>%
  filter(minute>=43 & minute<=45)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1POL: long, lost possession

#Turkey vs Switzerland
TUR_SUI<-Matches %>%
  filter(match_id==3788765)
TUR_SUI_Data<-StatsBombFreeEvents(MatchesDF = TUR_SUI, Parallel = T)

C_TUR_SUI<- TUR_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Turkey")
C1TUR_SUI<- TUR_SUI_Data %>%
  filter(minute>=2 & minute<=4)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1TUR: long, goal kick
C2TUR_SUI<- TUR_SUI_Data %>%
  filter(minute>=31 & minute<=34)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2TUR: long, corner
#C3TUR: mid, lost possession
C4TUR_SUI<- TUR_SUI_Data %>%
  filter(minute>=42 & minute<=45)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4TUR: long, goal kick
C5TUR_SUI<- TUR_SUI_Data %>%
  filter(minute>=82 & minute<=84)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5TUR: long, goal kick
C6TUR_SUI<- TUR_SUI_Data %>%
  filter(minute>=88 & minute<=94)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6TUR: long, shot saved
#C7TUR: short, corner
#C8TUR: long, clearance

C_SUI_TUR<- TUR_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Switzerland")
C1SUI_TUR<- TUR_SUI_Data %>%
  filter(minute>=10 & minute<=12)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1SUI: long, lost possession
C2SUI_TUR<- TUR_SUI_Data %>%
  filter(minute>=28 & minute<=30)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2SUI: long, throw in
C3SUI_TUR<- TUR_SUI_Data %>%
  filter(minute>=44 & minute<=53)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3SUI: long, corner
#C4SUI: long, goal
#C5SUI: mid, foul
C6SUI_TUR<- TUR_SUI_Data %>%
  filter(minute>=60 & minute<=62)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7SUI: long, lost possession
C7SUI_TUR<- TUR_SUI_Data %>%
  filter(minute>=71 & minute<=73)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7SUI: long, goal kick
C8SUI_TUR<- TUR_SUI_Data %>%
  filter(minute>=80 & minute<=82)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C8SUI: mid, counter 

#Wales vs Italy
WAL_ITA<-Matches %>%
  filter(match_id==3788766)
WAL_ITA_Data<-StatsBombFreeEvents(MatchesDF = WAL_ITA, Parallel = T)

C_ITA_WAL<- WAL_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Italy")
C1ITA_WAL<- WAL_ITA_Data %>%
  filter(minute>=29 & minute<=31)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1ITA: long, throw in
C2ITA_WAL<- WAL_ITA_Data %>%
  filter(minute>=41 & minute<=44)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2ITA: mid, lost possession
C3ITA_WAL<- WAL_ITA_Data %>%
  filter(minute>=65 & minute<=73)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3ITA: mid, goal kick
#C4ITA: long, throw in
#C5ITA: mid, goal kick
C6ITA_WAL<- WAL_ITA_Data %>%
  filter(minute>=83 & minute<=86)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C6ITA: long, corner
#C7ITA: short, throw in

C_WAL_ITA<- WAL_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Wales")
C1WAL_ITA<- WAL_ITA_Data %>%
  filter(minute>=26 & minute<=28)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1WAL: long, goal kick

#Belgium vs Finland
BEL_FIN<-Matches %>%
  filter(match_id==3788768)
BEL_FIN_Data<-StatsBombFreeEvents(MatchesDF = BEL_FIN, Parallel = T)

C_BEL_FIN<- BEL_FIN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Belgium")
C1BEL_FIN<- BEL_FIN_Data %>%
  filter(minute>=42 & minute<=49)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1BEL: long, possession
#C2BEL: long, throw in
C3BEL_FIN<- BEL_FIN_Data %>%
  filter(minute>=52 & minute<=58)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3BEL: long, goal kick
#C4BEL: long, shot saved
C5BEL_FIN<- BEL_FIN_Data %>%
  filter(minute>=73 & minute<=75)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5BEL: long, goal

#Ukraine vs Austria
UKR_AUS<-Matches %>%
  filter(match_id==3788767)
UKR_AUS_Data<-StatsBombFreeEvents(MatchesDF = UKR_AUS, Parallel = T)

C_UKR_AUS<- UKR_AUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Ukraine")
C1UKR_AUS<- UKR_AUS_Data %>%
  filter(minute>=17 & minute<=19)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1UKR: mid, foul
C2UKR_AUS<- UKR_AUS_Data %>%
  filter(minute>=48 & minute<=52)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2UKR: long, foul
#C3UKR: short, goal kick
C4UKR_AUS<- UKR_AUS_Data %>%
  filter(minute>=61 & minute<=63)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C4UKR: short, throw in

C_AUS_UKR<- UKR_AUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Austria")
C1AUS_UKR<- UKR_AUS_Data %>%
  filter(minute>=2 & minute<=4)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1AUS: mid, corner
#C2AUS: long, foul
C2AUS_UKR<- UKR_AUS_Data %>%
  filter(minute>=10 & minute<=16)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C3AUS: long, goal kick
#C4AUS: long, goal kick
C5AUS_UKR<- UKR_AUS_Data %>%
  filter(minute>=20 & minute<=25)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C5AUS: long, goal
#C6AUS: long, lost possession
C7AUS_UKR<- UKR_AUS_Data %>%
  filter(minute>=35 & minute<=38)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C7AUS: mid, lost possession
#C8AUS: long, throw in
C9AUS_UKR<- UKR_AUS_Data %>%
  filter(minute>=76 & minute<=78)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C9AUS: long, lost possession

#Russia vs Denmark
RUS_DEN<-Matches %>%
  filter(match_id==3788769)
RUS_DEN_Data<-StatsBombFreeEvents(MatchesDF = RUS_DEN, Parallel = T)

C_RUS_DEN<- RUS_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Russia")
C1RUS_DEN<- RUS_DEN_Data %>%
  filter(minute>=21 & minute<=22)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1RUS: long, foul

C_DEN_RUS<- RUS_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Denmark")
C1DEN_RUS<- RUS_DEN_Data %>%
  filter(minute>=8 & minute<=10)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C1DEN: long, foul
C2DEN_RUS<- RUS_DEN_Data %>%
  filter(minute>=40 & minute<=42)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#C2DEN: long, goal kick
C3DEN_RUS<- RUS_DEN_Data %>%
  filter(minute>=73 & minute<=79)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3DEN: long, lost possession
#4DEN: long, throw in
C5DEN_RUS<- RUS_DEN_Data %>%
  filter(minute>=84 & minute<=86)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5DEN: long, throw in
C6DEN_RUS<- RUS_DEN_Data %>%
  filter(minute>=91 & minute<=93)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6DEN: long, corner
#7DEN: long, clearance

#North Macedonia vs Netherlands
NMA_NED<-Matches %>%
  filter(match_id==3788770)
NMA_NED_Data<-StatsBombFreeEvents(MatchesDF = NMA_NED, Parallel = T)

C_NED_NMA<- NMA_NED_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Netherlands")
C1NED_NMA<- NMA_NED_Data %>%
  filter(minute>=17 & minute<=19)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1NED: long, goal kick
C2NED_NMA<- NMA_NED_Data %>%
  filter(minute>=36 & minute<=38)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2NED: long, foul
#3NED: long, foul
C4NED_NMA<- NMA_NED_Data %>%
  filter(minute>=46 & minute<=50)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4NED: long, goal kick
#5NED: long, corner
#6NED: mid, throw in
C7NED_NMA<- NMA_NED_Data %>%
  filter(minute>=88 & minute<=90)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#7NED: long, lost possession

C_NMA_NED<- NMA_NED_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="North Macedonia")
C1NMA_NED<- NMA_NED_Data %>%
  filter(minute>=38 & minute<=41)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1NMA: short, goal kick
#2NMA: long, goal kick
C3NMA_NED<- NMA_NED_Data %>%
  filter(minute>=60 & minute<=62)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3NMA: mid, goal kick
C4NMA_NED<- NMA_NED_Data %>%
  filter(minute>=81 & minute<=91)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4NMA: long, goal kick
#5NMA: long, lost possession

#Croatia vs Scotland
CRO_SCO<-Matches %>%
  filter(match_id==3788771)
CRO_SCO_Data<-StatsBombFreeEvents(MatchesDF = CRO_SCO, Parallel = T)

C_CRO_SCO<- CRO_SCO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Croatia")
C1CRO_SCO<- CRO_SCO_Data %>%
  filter(minute>=53 & minute<=55)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1CRO: long, throw in
C2CRO_SCO<- CRO_SCO_Data %>%
  filter(minute>=76 & minute<=78)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2CRO: long, corner
#3CRO: long, goal
C4CRO_SCO<- CRO_SCO_Data %>%
  filter(minute>=87 & minute<=90)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4CRO: short, counter attack
#5CRO: mid, possession

C_SCO_CRO<- CRO_SCO_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Scotland")
C1SCO_CRO<- CRO_SCO_Data %>%
  filter(minute>=0 & minute<=7)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1SCO: mid, corner
#2SCO: long, throw in
#3SCO: long, throw in
C4SCO_CRO<- CRO_SCO_Data %>%
  filter(minute>=65 & minute<=69)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4SCO: long, foul
#5SCO: long, possession
C6SCO_CRO<- CRO_SCO_Data %>%
  filter(minute>=80 & minute<=93)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6SCO: long, goal kick
#7SCO: long, goal kick

#32 England vs Czech Republic
ENG_CZE<-Matches %>%
  filter(match_id==3788772)
ENG_CZE_Data<-StatsBombFreeEvents(MatchesDF = ENG_CZE, Parallel = T)

C_ENG_CZE<- ENG_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="England")
C1ENG_CZE<- ENG_CZE_Data %>%
  filter(minute>=16 & minute<=18)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ENG: long, lost possession
C2ENG_CZE<- ENG_CZE_Data %>%
  filter(minute>=25 & minute<=27)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ENG: long, lost possession
C3ENG_CZE<- ENG_CZE_Data %>%
  filter(minute>=32 & minute<=42)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3NG: long, possession
#4ENG: long, counter attack
#5ENG: long, possession
C6ENG_CZE<- ENG_CZE_Data %>%
  filter(minute>=73 & minute<=75)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6ENG: mid, lost possession

C_CZE_ENG<- ENG_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Czech Republic")
C1CZE_ENG<- ENG_CZE_Data %>%
  filter(minute>=27 & minute<=29)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1CZE: long, throw in
#2CZE: long, possession
C3CZE_ENG<- ENG_CZE_Data %>%
  filter(minute>=48 & minute<=50)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3CZE: mid, lost possession
C4CZE_ENG<- ENG_CZE_Data %>%
  filter(minute>=75 & minute<=77)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4CZE: long, throw in

#Germany vs Hungary
GER_HUN<-Matches %>%
  filter(match_id==3788774)
GER_HUN_Data<-StatsBombFreeEvents(MatchesDF = GER_HUN, Parallel = T)

C_GER_HUN<- GER_HUN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Germany")
C1GER_HUN<- GER_HUN_Data %>%
  filter(minute>=20 & minute<=22)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1GER: long, lost possession
C2GER_HUN<- GER_HUN_Data %>%
  filter(minute>=59 & minute<=61)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2GER: long, throw in
C3GER_HUN<- GER_HUN_Data %>%
  filter(minute>=64 & minute<=66)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3GER: mid, foul
C4GER_HUN<- GER_HUN_Data %>%
  filter(minute>=89 & minute<=92)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4GER: short, corner
#5GER: short, corner
#6GER: short, throw in

C_HUN_GER<- GER_HUN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Hungary")
C1HUN_GER<- GER_HUN_Data %>%
  filter(minute>=4 & minute<=6)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1HUN: mid, throw in

#Portugal vs France
POR_FRA<-Matches %>%
  filter(match_id==3788773)
POR_FRA_Data<-StatsBombFreeEvents(MatchesDF = POR_FRA, Parallel = T)

C_FRA_POR<- POR_FRA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="France")
C1FRA_POR<- POR_FRA_Data %>%
  filter(minute>=6 & minute<=8)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1FRA: long, throw in

#Slovakia vs Spain
SLO_ESP<-Matches %>%
  filter(match_id==3788775)
SLO_ESP_Data<-StatsBombFreeEvents(MatchesDF = SLO_ESP, Parallel = T)

C_ESP_SLO<- SLO_ESP_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Spain")
C1ESP_SLO<- SLO_ESP_Data %>%
  filter(minute>=14 & minute<=16)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ESP: long, lost possession
C2ESP_SLO<- SLO_ESP_Data %>%
  filter(minute>=23 & minute<=26)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ESP: long, corner
#3ESP: long, goal kick
C4ESP_SLO<- SLO_ESP_Data %>%
  filter(minute>=47 & minute<=49)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4ESP: long, goal
C6ESP_SLO<- SLO_ESP_Data %>%
  filter(minute>=66 & minute<=78)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5ESP: short, goal
#6ESP: short, corner
#7ESP: short, lost possession

#Poland vs Sweden
POL_SWE<-Matches %>%
  filter(match_id==3788776)
POL_SWE_Data<-StatsBombFreeEvents(MatchesDF = POL_SWE, Parallel = T)

C_POL_SWE<- POL_SWE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Poland")
C1POL_SWE<- POL_SWE_Data %>%
  filter(minute>=9 & minute<=18)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1POL: short, throw in
#2POL: long, possession
C3POL_SWE<- POL_SWE_Data %>%
  filter(minute>=33 & minute<=42)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3POL: long, goal kick
#4POL: long, corner
#5POL: long, foul
#6POL: mid, throw in
#7POL: short, throw in
C8POL_SWE<- POL_SWE_Data %>%
  filter(minute>=45 & minute<=50)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#8POL: long, lost possession
#9POL: short, goal kick
C10POL_SWE<- POL_SWE_Data %>%
  filter(minute>=80 & minute<=82)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#10POL: long, goal kick

C_SWE_POL<- POL_SWE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Sweden")
C1SWE_POL<- POL_SWE_Data %>%
  filter(minute>=14 & minute<=16)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1SWE: long, throw in
C2SWE_POL<- POL_SWE_Data %>%
  filter(minute>=35 & minute<=37)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2SWE: long, throw in
C3SWE_POL<- POL_SWE_Data %>%
  filter(minute>=49 & minute<=51)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3SWE: long, goal kick
C4SWE_POL<- POL_SWE_Data %>%
  filter(minute>=56 & minute<=58)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4SWE: mid, corner
#5SWE: long, goal kick
C6SWE_POL<- POL_SWE_Data %>%
  filter(minute>=70 & minute<=72)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6SWE: mid, foul

#Italy vs Austria
ITA_AUS<-Matches %>%
  filter(match_id==3794685)
ITA_AUS_Data<-StatsBombFreeEvents(MatchesDF = ITA_AUS, Parallel = T)

C_ITA_AUS<- ITA_AUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Italy")
C1ITA_AUS<- ITA_AUS_Data %>%
  filter(minute>=25 & minute<=27)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ITA: long, possession
C2ITA_AUS<- ITA_AUS_Data %>%
  filter(minute>=42 & minute<=44)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ITA: long, goal kick
C3ITA_AUS<- ITA_AUS_Data %>%
  filter(minute>=55 & minute<=57)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ITA: long, goal kick
C4ITA_AUS<- ITA_AUS_Data %>%
  filter(minute>=104 & minute<=106)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4ITA: long, goal

C_AUS_ITA<- ITA_AUS_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Austria")
C1AUS_ITA<- ITA_AUS_Data %>%
  filter(minute>=62 & minute<=64)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1AUS: long, shot saved
C2AUS_ITA<- ITA_AUS_Data %>%
  filter(minute>=80 & minute<=82)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2AUS: short, goal kick
C3AUS_ITA<- ITA_AUS_Data %>%
  filter(minute>=113 & minute<=115)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3AUS: mid, goal

#Wales vs Denmark
WAL_DEN<-Matches %>%
  filter(match_id==3794689)
WAL_DEN_Data<-StatsBombFreeEvents(MatchesDF = WAL_DEN, Parallel = T)

C_WAL_DEN<- WAL_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Wales")
C1WAL_DEN<- WAL_DEN_Data %>%
  filter(minute>=4 & minute<=6)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1WAL: short, throw in

C_DEN_WAL<- WAL_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Denmark")
C1DEN_WAL<- WAL_DEN_Data %>%
  filter(minute>=19 & minute<=24)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1DEN: long, corner
#2DEN: long, corner
#3DEN: long, throw in
#4DEN: long, goal kick
C5DEN_WAL<- WAL_DEN_Data %>%
  filter(minute>=35 & minute<=37)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5DEN: long, foul
C6DEN_WAL<- WAL_DEN_Data %>%
  filter(minute>=41 & minute<=47)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6DEN: short, lost possession
#7DEN: long, throw in
C8DEN_WAL<- WAL_DEN_Data %>%
  filter(minute>=82 & minute<=86)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#8DEN: long, goal kick
#9DEN: short, goal kick

#Belgium vs Portugal
BEL_POR<-Matches %>%
  filter(match_id==3794687)
BEL_POR_Data<-StatsBombFreeEvents(MatchesDF = BEL_POR, Parallel = T)

C_POR_BEL<- BEL_POR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Portugal")
C1POR_BEL<- BEL_POR_Data %>%
  filter(minute>=40 & minute<=42)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1POR: short, foul
C2POR_BEL<- BEL_POR_Data %>%
  filter(minute>=81 & minute<=85)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2POR: long, goal kick
#3POR: mid, throw in

#Netherlands vs Czech Republic
NED_CZE<-Matches %>%
  filter(match_id==3794690)
NED_CZE_Data<-StatsBombFreeEvents(MatchesDF = NED_CZE, Parallel = T)

C_NED_CZE<- NED_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Netherlands")
C1NED_CZE<- NED_CZE_Data %>%
  filter(minute>=7 & minute<=16)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1NED: mid, throw in
#2NED: mid, throw in
#3NED: long, goal kick
#4NED: long, lost possession
C5NED_CZE<- NED_CZE_Data %>%
  filter(minute>=31 & minute<=33)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5NED: long, corner
#6NED: mid, goal kick

C_CZE_NED<- NED_CZE_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Czech Republic")
C1CZE_NED<- NED_CZE_Data %>%
  filter(minute>=33 & minute<=39)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1CZE: mid, lost possession
#2CZE: long, lost possession
C3CZE_NED<- NED_CZE_Data %>%
  filter(minute>=59 & minute<=68)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3CZE: mid, goal kick
#4CZE: mid, lost possession
C5CZE_NED<- NED_CZE_Data %>%
  filter(minute>=90 & minute<=92)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5CZE: short, lost possession

#Croatia vs Spain
CRO_ESP<-Matches %>%
  filter(match_id==3794686)
CRO_ESP_Data<-StatsBombFreeEvents(MatchesDF = CRO_ESP, Parallel = T)

C_ESP_CRO<- CRO_ESP_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Spain")
C1ESP_CRO<- CRO_ESP_Data %>%
  filter(minute>=15 & minute<=17)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ESP: long, corner
#2ESP: mid, lost possession
C3ESP_CRO<- CRO_ESP_Data %>%
  filter(minute>=31 & minute<=37)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ESP: mid, foul
#4ESP: long, goal kick
C5ESP_CRO<- CRO_ESP_Data %>%
  filter(minute>=41 & minute<=43)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5ESP: long, corner
#6ESP: mid, goal kick
C7ESP_CRO<- CRO_ESP_Data %>%
  filter(minute>=97 & minute<=117)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#7ESP: mid, possession
#8ESP: short, possession

C1CRO_ESP<- CRO_ESP_Data %>%
  filter(minute>=106 & minute<=108)%>%
  filter(period==4)
#1CRO: short, lost possession

#France vs Switzerland
FRA_SUI<-Matches %>%
  filter(match_id==3794691)
FRA_SUI_Data<-StatsBombFreeEvents(MatchesDF = FRA_SUI, Parallel = T)

C_FRA_SUI<- FRA_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="France")
C1FRA_SUI<- FRA_SUI_Data %>%
  filter(minute>=1 & minute<=8)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1FRA: long, goal kick
#2FRA: long, lost possession
C3FRA_SUI<- FRA_SUI_Data %>%
  filter(minute>=36 & minute<=38)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3FRA: mid, lost possession
C4FRA_SUI<- FRA_SUI_Data %>%
  filter(minute>=68 & minute<=72)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4FRA: mid, possession
#5FRA: long, corner
#6FRA: long, lost possession
C7FRA_SUI<- FRA_SUI_Data %>%
  filter(minute>=94 & minute<=112)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#7FRA: long, possession
#8FRA: mid, throw in

C_SUI_FRA<- FRA_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Switzerland")
C1SUI_FRA<- FRA_SUI_Data %>%
  filter(minute>=3 & minute<=9)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1SUI: long, throw in
#2SUI: long, throw in
C3SUI_FRA<- FRA_SUI_Data %>%
  filter(minute>=40 & minute<=42)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3SUI: long, goal kick
C4SUI_FRA<- FRA_SUI_Data %>%
  filter(minute>=92 & minute<=103)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4SUI: mid, possession
#5SUI: long, goal kick

#England vs Germany
ENG_GER<-Matches %>%
  filter(match_id==3794688)
ENG_GER_Data<-StatsBombFreeEvents(MatchesDF = ENG_GER, Parallel = T)

C_ENG_GER<- ENG_GER_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="England")
C1ENG_GER<- ENG_GER_Data %>%
  filter(minute>=16 & minute<=18)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ENG: long, shot saved
C2ENG_GER<- ENG_GER_Data %>%
  filter(minute>=26 & minute<=28)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ENG: mid, goal kick
C3ENG_GER<- ENG_GER_Data %>%
  filter(minute>=43 & minute<=45)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ENG: long, throw in

C_GER_ENG<- ENG_GER_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Germany")
C1GER_ENG<- ENG_GER_Data %>%
  filter(minute>=2 & minute<=4)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1GER: mid, throw in
C2GER_ENG<- ENG_GER_Data %>%
  filter(minute>=48 & minute<=50)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2GER: long, throw in
C3GER_ENG<- ENG_GER_Data %>%
  filter(minute>=57 & minute<=59)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3GER: mid, throw in

#Sweden vs Ukraine
SWE_UKR<-Matches %>%
  filter(match_id==3794692)
SWE_UKR_Data<-StatsBombFreeEvents(MatchesDF = SWE_UKR, Parallel = T)

C_SWE_UKR<- SWE_UKR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Sweden")
C1SWE_UKR<- SWE_UKR_Data %>%
  filter(minute>=6 & minute<=13)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1SWE: mid, throw in
#2SWE: long, keeper claim
C3SWE_UKR<- SWE_UKR_Data %>%
  filter(minute>=29 & minute<=31)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3SWE: long, keeper claim
C4SWE_UKR<- SWE_UKR_Data %>%
  filter(minute>=65 & minute<=67)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4SWE: long, corner
#5SWE: mid, lost possession
C6SWE_UKR<- SWE_UKR_Data %>%
  filter(minute>=89 & minute<=91)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6SWE: mid, throw in

C_UKR_SWE<- SWE_UKR_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Ukraine")
C1UKR_SWE<- SWE_UKR_Data %>%
  filter(minute>=106 & minute<=108)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1UKR: mid, corner
#2UKR: short, foul

#Belgium vs Italy
BEL_ITA<-Matches %>%
  filter(match_id==3795107)
BEL_ITA_Data<-StatsBombFreeEvents(MatchesDF = BEL_ITA, Parallel = T)

C_BEL_ITA<- BEL_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Belgium")
C1BEL_ITA<- BEL_ITA_Data %>%
  filter(minute>=2 & minute<=4)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1BEL: mid, throw in
C2BEL_ITA<- BEL_ITA_Data %>%
  filter(minute>=11 & minute<=17)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2BEL: mid, lost possession
#3BEL: short, throw in
C4BEL_ITA<- BEL_ITA_Data %>%
  filter(minute>=22 & minute<=27)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4BEL: short, lost possession
#5BEL: long, lost possession
C6BEL_ITA<- BEL_ITA_Data %>%
  filter(minute>=33 & minute<=35)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6BEL: long, lost possession
C7BEL_ITA<- BEL_ITA_Data %>%
  filter(minute>=54 & minute<=62)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#7BEL: short, lost possession
#8BEL: long, lost possession
C9BEL_ITA<- BEL_ITA_Data %>%
  filter(minute>=96 & minute<=100)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#9BEL: mid, possession

C_ITA_BEL<- BEL_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Italy")
C1ITA_BEL<- BEL_ITA_Data %>%
  filter(minute>=29 & minute<=31)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ITA: long, throw in
C2ITA_BEL<- BEL_ITA_Data %>%
  filter(minute>=40 & minute<=49)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ITA: long, goal kick
#3ITA: mid, corner
#4ITA: mid, goal kick
C5ITA_BEL<- BEL_ITA_Data %>%
  filter(minute>=56 & minute<=58)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5ITA: long, throw in

#Spain vs Switzerland
ESP_SUI<-Matches %>%
  filter(match_id==3795108)
ESP_SUI_Data<-StatsBombFreeEvents(MatchesDF = ESP_SUI, Parallel = T)

C_ESP_SUI<- ESP_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Spain")
C1ESP_SUI<- ESP_SUI_Data %>%
  filter(minute>=7 & minute<=16)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ESP: long, goal
#2ESP: long, foul
C3ESP_SUI<- ESP_SUI_Data %>%
  filter(minute>=24 & minute<=26)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ESP: long, shot saved
C4ESP_SUI<- ESP_SUI_Data %>%
  filter(minute>=59 & minute<=64)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4ESP: short, goal kick
#5ESP: mid, lost possession
C6ESP_SUI<- ESP_SUI_Data %>%
  filter(minute>=75 & minute<=77)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6ESP: short, lost possession
C7ESP_SUI<- ESP_SUI_Data %>%
  filter(minute>=95 & minute<=100)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#7ESP: long, keeper claim
#8ESP: long, goal kick
C9ESP_SUI<- ESP_SUI_Data %>%
  filter(minute>=101 & minute<=104)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#9ESP: long, corner
#10ESP: mid, corner
#11ESP: long, foul
#12ESP: long, foul
C13ESP_SUI<- ESP_SUI_Data %>%
  filter(minute>=115 & minute<=117)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#13ESP: long, shot saved

C_SUI_ESP<- ESP_SUI_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Switzerland")
C1SUI_ESP<- ESP_SUI_Data %>%
  filter(minute>=19 & minute<=21)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1SUI: mid, lost possession
C2SUI_ESP<- ESP_SUI_Data %>%
  filter(minute>=33 & minute<=39)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2SUI: long, goal kick
#3SUI: long, goal kick
#4SUI: long, goal kick
C5SUI_ESP<- ESP_SUI_Data %>%
  filter(minute>=47 & minute<=56)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5SUI: long, lost possession
#6SUI: mid, goal kick
#7SUI: short, corner
#8SUI: long, goal kick
C9SUI_ESP<- ESP_SUI_Data %>%
  filter(minute>=64 & minute<=66)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#9SUI: long, foul

#Ukraine vs England
UKR_ENG<-Matches %>%
  filter(match_id==3795187)
UKR_ENG_Data<-StatsBombFreeEvents(MatchesDF = UKR_ENG, Parallel = T)

C_UKR_ENG<- UKR_ENG_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Ukraine")
C1UKR_ENG<- UKR_ENG_Data %>%
  filter(minute>=16 & minute<=18)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1UKR: long, possession
C2UKR_ENG<- UKR_ENG_Data %>%
  filter(minute>=40 & minute<=42)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2UKR: long, foul
C3UKR_ENG<- UKR_ENG_Data %>%
  filter(minute>=79 & minute<=81)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3UKR: long, possession

C_ENG_UKR<- UKR_ENG_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="England")
C1ENG_UKR<- UKR_ENG_Data %>%
  filter(minute>=62 & minute<=68)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ENG: long, goal
#2ENG: long, goal kick

#48 Czech Republic vs Denmark
CZE_DEN<-Matches %>%
  filter(match_id==3795109)
CZE_DEN_Data<-StatsBombFreeEvents(MatchesDF = CZE_DEN, Parallel = T)

C_CZE_DEN<- CZE_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Czech Republic")
C1CZE_DEN<- CZE_DEN_Data %>%
  filter(minute>=12 & minute<=20)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1CZE: long, shot for opposition
#2CZE: long, goal kick
C3CZE_DEN<- CZE_DEN_Data %>%
  filter(minute>=21 & minute<=26)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3CZE: mid, lost possession
#4CZE: long, goal kick
C5CZE_DEN<- CZE_DEN_Data %>%
  filter(minute>=32 & minute<=37)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5CZE: short, throw in
#6CZE: long, lost possession
C7CZE_DEN<- CZE_DEN_Data %>%
  filter(minute>=46 & minute<=48)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#7CZE: short, possession
C8CZE_DEN<- CZE_DEN_Data %>%
  filter(minute>=71 & minute<=75)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#8CZE: short, goal kick
#9CZE: long, goal kick

C_DEN_CZE<- CZE_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Denmark")
C1DEN_CZE<- CZE_DEN_Data %>%
  filter(minute>=4 & minute<=9)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1DEN: long, goal
#2DEN: mid, corner
#3DEN: long, goal kick
C4DEN_CZE<- CZE_DEN_Data %>%
  filter(minute>=51 & minute<=57)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4DEN: long, lost possession
#5DEN: long, lost possession
C6DEN_CZE<- CZE_DEN_Data %>%
  filter(minute>=78 & minute<=83)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6DEN: long, goal kick
#7DEN: long, goal kick

#Spain vs Italy
ESP_ITA<-Matches %>%
  filter(match_id==3795220)
ESP_ITA_Data<-StatsBombFreeEvents(MatchesDF = ESP_ITA, Parallel = T)

C_ESP_ITA<- ESP_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Spain")
C1ESP_ITA<- ESP_ITA_Data %>%
  filter(minute>=18 & minute<=20)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ESP: long, lost possession
C2ESP_ITA<- ESP_ITA_Data %>%
  filter(minute>=50 & minute<=62)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ESP: long, foul
C3ESP_ITA<- ESP_ITA_Data %>%
  filter(minute>=62 & minute<=64)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ESP: long, throw in
C4ESP_ITA<- ESP_ITA_Data %>%
  filter(minute>=76 & minute<=83)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4ESP: mid, lost possession
#5ESP: mid, goal kick
C6ESP_ITA<- ESP_ITA_Data %>%
  filter(minute>=91 & minute<=93)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6ESP: mid, lost possession

C_ITA_ESP<- ESP_ITA_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Italy")
C1ITA_ESP<- ESP_ITA_Data %>%
  filter(minute>=48 & minute<=50)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ITA: long, goal kick

#England vs Denmark
ENG_DEN<-Matches %>%
  filter(match_id==3795221)
ENG_DEN_Data<-StatsBombFreeEvents(MatchesDF = ENG_DEN, Parallel = T)

C_ENG_DEN<- ENG_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="England")
C1ENG_DEN<- ENG_DEN_Data %>%
  filter(minute>=11 & minute<=13)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ENG: mid, shot saved
C2ENG_DEN<- ENG_DEN_Data %>%
  filter(minute>=59 & minute<=61)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ENG: long, possession
C3ENG_DEN<- ENG_DEN_Data %>%
  filter(minute>=73 & minute<=75)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ENG: short, foul
C4ENG_DEN<- ENG_DEN_Data %>%
  filter(minute>=97 & minute<=101)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4ENG: long, goal kick
#5ENG: long, possession
C6ENG_DEN<- ENG_DEN_Data %>%
  filter(minute>=120 & minute<=122)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#6ENG: short, lost possession

C_DEN_ENG<- ENG_DEN_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Denmark")
C1DEN_ENG<- ENG_DEN_Data %>%
  filter(minute>=15 & minute<=17)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1DEN: long, lost possession
C2DEN_ENG<- ENG_DEN_Data %>%
  filter(minute>=56 & minute<=58)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2DEN: long, goal kick
C3DEN_ENG<- ENG_DEN_Data %>%
  filter(minute>=87 & minute<=89)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3DEN: long, foul
C4DEN_ENG<- ENG_DEN_Data %>%
  filter(minute>=114 & minute<=117)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#4DEN: long, foul
#5DEN: long, lost possession

#Italy vs England
ITA_ENG<-Matches %>%
  filter(match_id==3795506)
ITA_ENG_Data<-StatsBombFreeEvents(MatchesDF = ITA_ENG, Parallel = T)

C_ITA_ENG<- ITA_ENG_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="Italy")
C1ITA_ENG<- ITA_ENG_Data %>%
  filter(minute>=1 & minute<=3)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ITA: long, goal for opposition
C2ITA_ENG<- ITA_ENG_Data %>%
  filter(minute>=66 & minute<=68)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#2ITA: mid, goal
C3ITA_ENG<- ITA_ENG_Data %>%
  filter(minute>=120 & minute<=122)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ITA: long, foul

C_ENG_ITA<- ITA_ENG_Data %>%
  filter(pass.type.name=="Corner")%>%
  filter(team.name=="England")
C1ENG_ITA<- ITA_ENG_Data %>%
  filter(minute>=12 & minute<=15)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#1ENG: long, keeper claim
#2ENG: long, lost possession
C3ENG_ITA<- ITA_ENG_Data %>%
  filter(minute>=63 & minute<=65)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#3ENG: long, corner
#4ENG: long, throw in
C5ENG_ITA<- ITA_ENG_Data %>%
  filter(minute>=96 & minute<=98)%>%
  filter(type.name=="Shot" | type.name=="Pass" | type.name=="Clearance" | type.name=="Block" | type.name=="Ball Recovery")
#5ENG: long, goal kick
```

## Extracted Data in Excel

Once I looked at all the corners and took the data I wanted, I had my
own dataset of the corners in Excel. I was then able to further extract
the corners that had delayed shot opportunities (corner sequences where
a shot was taken 10 seconds or later after a corner).

``` r
library(readxl)
Corners<- read_excel("E:\\Analysis\\Hackathon\\Corners.xlsx", sheet = "Corners")
DelayedC<-read_excel("E:\\Analysis\\Hackathon\\Corners.xlsx", sheet = "Delayed")
```

After I was able to narrow down the corners with the delayed shot
opportunities, I went back and looked at the series of passes and shots
in the original corner analysis that occurred after the corner to know
which passes and shots I needed to get 360 freeze frames for.

## Getting the 360 Freeze Frames for the delayed opportunity corners

``` r
Delayed_Teams<- events %>%
  filter(id=="9ab79462-11c1-4403-a4bc-e08132f05652" | id=="e67b8e5f-8418-4991-a43a-dc5a789dce4f" | id=="c2d4b005-1fe8-4026-a4f5-b2e6dfd5a07b" |
           id=="75a6c11b-8f3c-48a9-bb98-f0c17b959b4f" | id=="c4002551-fa58-4ef5-bead-8a3df79f402c" | id=="bb290ccd-79ba-47e1-8c13-19ec95c8c273" |
           id=="c5f20e2f-6453-4481-b3bf-bba9dcc2f1b6" | id=="1701a359-312f-4649-ae4c-52f2af7fc174" | id=="cccdd38c-a399-4fec-a209-4604e45160fd" |
           id=="18ea3663-46b9-4fa6-a92c-953defd6b8dd" | id=="18c711ba-68ca-465f-9721-70bdc1dc41c1" | id=="422a553b-bfbe-4612-9f4c-4f2986e25a57" |
           id=="b62d2a28-3ddb-4b22-90df-03165bf8aa94" | id=="ecedaeff-afdb-4885-9f31-776f9a57affb" | id=="26ab9360-4eae-4378-969e-95ce55bd9271" |
           id=="29ad3e23-9079-4a8c-96f3-0f38101e23b9" | id=="28d08199-de76-40b8-a352-1538ea517691" | id=="c7998f84-aa47-4dea-9616-6ca135c20e36" |
           id=="8edaae25-2ed3-4fcc-ba91-b5bd7de83a94" | id=="31ee74a4-06a4-4822-824f-48edb3d8923d" | id=="f07e6e8d-4d54-4e1d-850a-c10fef7d1b0f" |
           id=="57bd9769-0f7c-4fd1-b793-d930596a9340")
ffdelayed<-Delayed_Teams %>%
  group_by(team.name)%>%
  select(id,match_id,team.name,OpposingTeam,player.name,type.name,
         minute,second,location.x,location.y,pass.end_location.x,pass.end_location.y,
         pass.type.name,freeze_frame)
ffdelayed = ffdelayed %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x=="NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))
espera<-ffdelayed %>%
  group_by(team.name,OpposingTeam,id)%>%
  summarise(attackers=sum(teammate=="TRUE",na.rm = "TRUE"),
            defenders=sum(teammate=="FALSE",na.rm = "TRUE"),
            att_n_def=attackers+defenders,
            att_v_def=attackers-defenders)%>%
  ungroup()
```

## Getting the 360 Freeze Frames for the passes in the corner sequences

``` r
DPasses<- events %>%
  filter(id=="0d5aacc3-a020-49d1-bc5d-0f4ac6824e8e" | id=="8f0f8fda-f6b0-4443-8404-b90183262de2" | id=="1bf2db28-6201-4427-bc8e-55f8d4f9ff92" |
           id=="6fbe5eb7-5022-4afb-92fd-12b329152305" | id=="ee926eb4-26b0-479d-96d9-51e9edbf998a" | id=="118c088b-7d37-47d8-88db-e1d8a3c352b7" |
           id=="11034bb7-7a0f-4bda-ae4f-390c1391384e" | id=="1c73d03c-eda9-47fe-a042-c6cba8323061" | id=="097903be-aa83-48a2-8edf-1d8e590ffaa6" |
           id=="a38ad360-b441-405f-825d-c159abedfacd" | id=="b481f5ea-93ea-4f84-9f6c-3c04f9a06386" | id=="5e0390b6-e6e5-42bc-a211-5e1a7d70ee14" |
           id=="3cd980ed-9b1b-4138-b2c9-1cb288ccc052" | id=="7fdaed75-5190-42bf-b155-60fe52ddbf57" | id=="d76c8cdc-ee2b-4b36-a65e-4d9f25b18b59" |
           id=="e3ad1686-638e-417f-bc86-a3b1bd2364fb" | id=="f3055991-4713-43ec-bc07-9e7b13a08520" | id=="890a8656-ac35-445b-a41d-d53d6eea13b8" |
           id=="a3f81d5e-4e09-4782-baef-5f433c409ba8" | id=="9d307ee7-2c8d-45ca-af7e-0e50a599ab3e" | id=="6eb76407-f517-4e79-814d-a68505cc1bb5" |
           id=="d99c3d7b-f31d-4af5-8c9d-39be01d90893" | id=="c9fc1306-476d-4ba0-8eef-b5ed70713050" | id=="bb294dab-1894-4d45-874a-e5b32bcdf89a" |
           id=="84ebc4c7-5cd7-4732-bff6-8556404b67ab" | id=="8b0d0e8f-81fb-4097-a5bb-010c8953b8c2" | id=="afb8d5a6-d778-4b0d-8ae4-ecd54504bbcb" |
           id=="841dcb0d-bfe2-4181-8153-a7759c3c7693" | id=="01a0e5dd-a8e0-4fa8-bc4c-d8f7d05e6bec" | id=="16f7c89b-a7cc-4203-9d9d-408bb34b6a47" |
           id=="ec2d18da-d843-4b13-98fd-9ccfa74cb63e" | id=="29b91083-e6d3-4ed6-9b77-2a4c4bc957b8" | id=="6ed7968a-4679-4b4d-bf5c-e26f5ac71106" |
           id=="26218927-dca9-4cd9-a9f0-6521b12af53c" | id=="4d4ca9f7-fead-46f9-a30b-f50ef8eff9c0" | id=="88252ed4-dd8c-47a9-a2dc-17edb81dfd42" |
           id=="331a5818-0f11-4b35-b54d-94737cb21098" | id=="2d8c06cd-800d-48f2-a378-573c118bb30c" | id=="bc169532-b2fd-44a4-addb-7b1f93fc34c7" |
           id=="d029a44c-08bd-40a5-8616-346106d2e67e" | id=="30873083-9186-46a3-adb9-4d2e3140ffec" | id=="5f8a2fdc-d4b6-4554-85de-c93156f65751" |
           id=="d797e823-ade6-418b-a658-3060e666df52" | id=="48654e24-e81f-4cec-859a-549127f795f4" | id=="4d722c2e-5653-4058-a490-7768da557b6c" |
           id=="e291259a-3f4d-4b40-b505-0bb208fff6ab" | id=="a4c0eae7-397f-410b-aa31-8143fc90e85f" | id=="608390e3-505d-491d-8133-a2a90aaa606d" |
           id=="58a54201-a4fb-48d3-bb70-1bdb580fc2d2" | id=="3bc3d649-a0cd-42eb-a7f3-ae31bde56a35" | id=="25016d68-3d37-458c-9d1e-ff2a0b958e6c" |
           id=="5e8c669e-24b1-4b3b-915d-484f0d4decb8" | id=="d138078b-aefd-460b-84f4-e44638cfb9d0" | id=="215f2f4c-fb5a-4d73-9624-938c017c12e7" |
           id=="18a59614-855d-4106-87e0-92887c65819a" | id=="92e20ff6-ea66-471c-a230-f00d56802ca1" | id=="2fb4032a-43fb-48e0-a0a0-4415a93bfda8" |
           id=="08f13e44-7919-496d-96f8-21e32913b893" | id=="a6d8de2f-e694-48b8-b70b-e1ddcc27a233" | id=="8dd1f848-c9e0-49b8-a978-8226579af228" |
           id=="2faceece-b1ff-408e-8a89-6a082713904d" | id=="1626db23-1370-4c41-a0f6-1fdf30539136" | id=="9604e254-9778-4771-a6f2-b37654646341" |
           id=="38d8facb-35dd-488b-ae2d-d987bcea94a0" | id=="3ab0293a-578e-4c0a-ba7d-6888a3962d6a" | id=="d5917167-93da-48a8-a881-06602ae47033" |
           id=="03fc09e1-b354-4108-b044-8843389ffeba" | id=="09a1c14e-57ca-482a-b9b9-6b3b87c270db" | id=="de3b15ae-e0db-4441-8ec3-a283c4fc47a7" |
           id=="8ef6c873-149d-43e6-94be-8e0dfa287825" | id=="cc310b88-b9f2-48b8-ac98-a259dd8c9b97" | id=="a278eef9-da2f-4036-a9b2-4167c5da24bf" |
           id=="3750f336-6c91-4e19-a748-0d7d51ca74f1" | id=="8caec2cc-8a13-4af1-b73d-86d69aa90e93" | id=="c081b5c0-6882-41df-ae09-6c2e3637c7c7" |
           id=="ccdcb4c1-4aa4-44ad-977b-bd9c89975de9" | id=="55e897d8-422d-42f3-8bd0-743521d4c290" | id=="19523c67-ed40-4e98-96e5-c2a711125cdd" |
           id=="ba9ff41f-3fc0-4acc-8175-63eb81046907" | id=="f3ea96d4-10e5-448f-bef6-73bf95cc4981" | id=="ef7a44bb-6655-4af0-8ecb-31d4f19f2680" |
           id=="ef01f3a4-c359-4eb4-8145-6848cc877d2b" | id=="47d8cb0a-9e1c-45ea-9270-244adedb96a1" | id=="81884eb3-5542-424b-8d17-6a3fc8bd10c0" |
           id=="b66e3e86-0a67-4cfc-801e-bfc7b783d7da" | id=="c8e6442f-abfe-4a95-b454-d13f2f87b741" | id=="4cd38988-c2d4-4013-b869-27079eb9ecc0" |
           id=="caddfb9c-3739-465d-aad6-18527efd0cc2" | id=="9f05bae9-e951-4dfc-9e46-fa3d14af70d1" | id=="6d95de57-4e86-46cc-a2ea-b796f9b3c5d4" |
           id=="12b17b87-bbdb-4daa-84b0-26986d3cbd50" | id=="91a81906-1188-4dc5-8744-a8de9caaf034" | id=="a3e00146-08fe-4d55-af91-dc3a3534c3f2" |
           id=="63db13f5-b25e-403f-b980-2036f2c0ee14" | id=="fbb1fb8d-68a2-4e29-be81-469ab8e67018" | id=="abceffc8-86c0-4184-bd93-f79fc6cf036c" |
           id=="5871c752-4a33-43e8-a4c5-1a75f1722312" | id=="bac1c80c-fde7-4545-b481-fec95903c24b" | id=="0bed2e2d-a304-42fd-8f21-8d2f1d9c9d73" |
           id=="56173d10-e242-42a3-a2a7-b76526178211" | id=="e8c3dec9-b0c8-4176-b276-11dfd3d71dd6" | id=="44ad1a76-f1a5-474b-be12-3ccf440eb922" |
           id=="d8808b2e-bd1e-46b8-8e2c-5726fd5255bc" | id=="63a4fb5a-d5b0-46e8-9f80-054d1a2f2f8c" | id=="c09b0d99-6cc2-4b01-939e-c1b0e46a80f8" |
           id=="909dc2ee-7f3c-4c75-b4ac-04d59ac46896" | id=="76cbb6cb-0f80-44d8-ae01-b95b7bcc5d58" | id=="aa116a5c-6053-4bb3-9a4b-541ea0394368" |
           id=="6b4631fe-054e-4836-8e5c-7259d403b268" | id=="d0046789-8ab6-4191-9c4d-9e172c6b1acb" | id=="64ffe8dd-655b-4030-9b63-1ae3b897efcd" |
           id=="085cbc78-9eb2-4b05-bab8-3f71cf44f18b" | id=="a3f81d5e-4e09-4782-baef-5f433c409ba8")
ffdpass<-DPasses %>%
  group_by(team.name)%>%
  select(id,match_id,team.name,OpposingTeam,player.name,type.name,
         minute,second,location.x,location.y,pass.end_location.x,pass.end_location.y,
         pass.type.name,freeze_frame)
ffdpass = ffdpass %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x=="NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))
connections<-ffdpass %>%
  group_by(team.name,OpposingTeam,id)%>%
  summarise(attackers=sum(teammate=="TRUE",na.rm = "TRUE"),
            defenders=sum(teammate=="FALSE",na.rm = "TRUE"),
            att_n_def=attackers+defenders,
            att_v_def=attackers-defenders)%>%
  ungroup()
```

## Getting the 360 Freeze Frames for the shots in the corner sequences

``` r
Dshots<-events %>%
  filter(id=="45602b08-65cd-495c-a261-7a2a5dba54d0" | id=="b581fcb4-53c9-4345-9e8d-d4ba06b7e815" | id=="3bf895cf-55a8-4367-b76a-eb07a85156ad" |
           id=="2ad830f5-8e50-468b-99d2-5acbec5d0ccc" | id=="7c512179-f1fa-4c91-aa07-69cfb0cb9e18" | id=="6d6ece63-0a50-4c0c-9186-78cfb300585b" |
           id=="d03c065b-dc27-4f24-829f-746beec384e7" | id=="ec2d18da-d843-4b13-98fd-9ccfa74cb63e" | id=="4f772f2c-b4eb-4e78-820f-37d8d4964a09" |
           id=="b73448a1-a04b-4c2f-9eac-b4f54717a585" | id=="975b6c3c-043b-4b41-b4af-31058546338d" | id=="1237339e-f825-42b9-8e42-4aaf695ede02" |
           id=="c351c87e-6fb3-4a32-8c26-d97cb4cbfae1" | id=="164224d6-3b39-4032-af5e-376a9e309920" | id=="6e6a699b-a6e4-48df-bff8-d0eda476c4f0" |
           id=="1ee6f3b7-aba7-4bcd-8ff2-359c85cb4161" | id=="cea85a31-f9c4-4d48-a87d-d69e2f488f6e" | id=="ea779639-e6b2-42dc-9141-4a6f5bc3545c" |
           id=="03e9fea5-d2d5-4f72-bb37-e09eaa771914" | id=="17ee2f7a-68ad-4033-a62f-f208971ad9c5" | id=="8953c8ea-8e56-4922-8999-037364ae1896" |
           id=="632a06a3-16ed-4c54-b86d-e55c3330b086" | id=="8aa8ff35-40d3-4224-a3a1-392d5636d8e5" | id=="cc58f290-6272-417e-9f81-d7fa385b26d9" |
           id=="532e7087-f55f-4f88-96a1-9b6a12b4c062" | id=="02ee8ae1-2ce2-4d0f-9254-0d23916c6e9c" | id=="28e7aed2-d8c4-487c-bfa4-6d30f749e953" |
           id=="2ae7f87f-dff7-4f2c-9de0-4cf39d8a69b9" | id=="fd669b96-c078-4411-b60d-e0c39bdd402e" | id=="d1d13791-0bbd-4c53-9c2e-1da17a64c35c" |
           id=="25b14ee0-052e-45ce-afe5-d883e9ccc29f")
ffdshot<-Dshots %>%
  group_by(team.name)%>%
  select(id,match_id,team.name,OpposingTeam,player.name,type.name,
         minute,second,location.x,location.y,pass.end_location.x,pass.end_location.y,
         pass.type.name,freeze_frame)
ffdshot = ffdshot %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x=="NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))
shots<-ffdshot %>%
  group_by(team.name,OpposingTeam,id)%>%
  summarise(attackers=sum(teammate=="TRUE",na.rm = "TRUE"),
            defenders=sum(teammate=="FALSE",na.rm = "TRUE"),
            att_n_def=attackers+defenders,
            att_v_def=attackers-defenders)%>%
  ungroup()
```

## Code for ggplot Setup

After I set up the freeze frames for the important events of each
corner, I then set up code for each corner that would later allow me to
put the data in ggplot.

``` r
#England vs Scotland: 9ab79462-11c1-4403-a4bc-e08132f05652
ENG_SCO1_m<-ffdelayed%>%
  filter(id=="9ab79462-11c1-4403-a4bc-e08132f05652")%>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Mount_pass_sco1<-events%>%
  filter(id=="9ab79462-11c1-4403-a4bc-e08132f05652")
P1ENG_SCO<- ffdpass %>%
  filter(id=="0d5aacc3-a020-49d1-bc5d-0f4ac6824e8e") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Shaw_pass_sco1<-events%>%
  filter(id=="0d5aacc3-a020-49d1-bc5d-0f4ac6824e8e")
P2ENG_SCO<- ffdpass %>%
  filter(id=="8f0f8fda-f6b0-4443-8404-b90183262de2") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Mount_pass_sco2<-events%>%
  filter(id=="8f0f8fda-f6b0-4443-8404-b90183262de2")
P3ENG_SCO<- ffdpass %>%
  filter(id=="1bf2db28-6201-4427-bc8e-55f8d4f9ff92") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Shaw_pass_sco2<-events%>%
  filter(id=="1bf2db28-6201-4427-bc8e-55f8d4f9ff92")
P4ENG_SCO<- ffdpass %>%
  filter(id=="6fbe5eb7-5022-4afb-92fd-12b329152305") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Mount_pass_sco3<-events%>%
  filter(id=="6fbe5eb7-5022-4afb-92fd-12b329152305")
P5ENG_SCO<- ffdpass %>%
  filter(id=="ee926eb4-26b0-479d-96d9-51e9edbf998a") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Rice_pass_sco1<-events%>%
  filter(id=="ee926eb4-26b0-479d-96d9-51e9edbf998a")
P6ENG_SCO<- ffdpass %>%
  filter(id=="118c088b-7d37-47d8-88db-e1d8a3c352b7") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Mount_pass_sco4<-events%>%
  filter(id=="118c088b-7d37-47d8-88db-e1d8a3c352b7")
P7ENG_SCO<- ffdpass %>%
  filter(id=="11034bb7-7a0f-4bda-ae4f-390c1391384e") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Shaw_pass_sco3<-events%>%
  filter(id=="11034bb7-7a0f-4bda-ae4f-390c1391384e")
P8ENG_SCO<- ffdpass %>%
  filter(id=="1c73d03c-eda9-47fe-a042-c6cba8323061") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Rice_pass_sco2<-events%>%
  filter(id=="1c73d03c-eda9-47fe-a042-c6cba8323061")
P9ENG_SCO<- ffdpass %>%
  filter(id=="097903be-aa83-48a2-8edf-1d8e590ffaa6") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Mings_pass_sco<-events%>%
  filter(id=="097903be-aa83-48a2-8edf-1d8e590ffaa6")
P10ENG_SCO<- ffdpass %>%
  filter(id=="a38ad360-b441-405f-825d-c159abedfacd") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Rice_pass_sco3<-events%>%
  filter(id=="a38ad360-b441-405f-825d-c159abedfacd")
P11ENG_SCO<- ffdpass %>%
  filter(id=="b481f5ea-93ea-4f84-9f6c-3c04f9a06386") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Shaw_pass_sco4<-events%>%
  filter(id=="b481f5ea-93ea-4f84-9f6c-3c04f9a06386")
P12ENG_SCO<- ffdpass %>%
  filter(id=="5e0390b6-e6e5-42bc-a211-5e1a7d70ee14") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Phillips_pass_sco1<-events%>%
  filter(id=="5e0390b6-e6e5-42bc-a211-5e1a7d70ee14")
P13ENG_SCO<- ffdpass %>%
  filter(id=="3cd980ed-9b1b-4138-b2c9-1cb288ccc052") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Mings_pass_sco2<-events%>%
  filter(id=="3cd980ed-9b1b-4138-b2c9-1cb288ccc052")
P14ENG_SCO<- ffdpass %>%
  filter(id=="7fdaed75-5190-42bf-b155-60fe52ddbf57") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Shaw_pass_sco5<-events%>%
  filter(id=="7fdaed75-5190-42bf-b155-60fe52ddbf57")
P15ENG_SCO<- ffdpass %>%
  filter(id=="d76c8cdc-ee2b-4b36-a65e-4d9f25b18b59") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Mount_pass_sco5<-events%>%
  filter(id=="d76c8cdc-ee2b-4b36-a65e-4d9f25b18b59")
P16ENG_SCO<- ffdpass %>%
  filter(id=="e3ad1686-638e-417f-bc86-a3b1bd2364fb") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Sterling_pass_sco1<-events%>%
  filter(id=="e3ad1686-638e-417f-bc86-a3b1bd2364fb")
P17ENG_SCO<- ffdpass %>%
  filter(id=="f3055991-4713-43ec-bc07-9e7b13a08520") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Kane_pass_sco1<-events%>%
  filter(id=="f3055991-4713-43ec-bc07-9e7b13a08520")
P18ENG_SCO<- ffdpass %>%
  filter(id=="890a8656-ac35-445b-a41d-d53d6eea13b8") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Sterling_pass_sco2<-events%>%
  filter(id=="890a8656-ac35-445b-a41d-d53d6eea13b8")
P19ENG_SCO<- ffdpass %>%
  filter(id=="a3f81d5e-4e09-4782-baef-5f433c409ba8") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Shaw_pass_sco6<-events%>%
  filter(id=="a3f81d5e-4e09-4782-baef-5f433c409ba8")

S1ENG_SCO<- ffdshot %>%
  filter(id=="45602b08-65cd-495c-a261-7a2a5dba54d0") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Mount_shot_sco1<-events%>%
  filter(id=="45602b08-65cd-495c-a261-7a2a5dba54d0")
#Shaw>Mount>Shaw>Mount>Rice>Mount>Shaw>Mount>Rice>Mings>Rice>Shaw>Phillips>Mings>Shaw>Mount>Sterling>Kane>Sterling>Mount(shot)
#min 46:19, outcome: corner

#England vs Germany: c2d4b005-1fe8-4026-a4f5-b2e6dfd5a07b
ENG_GER_m<- ffdelayed %>%
  filter(id=="c2d4b005-1fe8-4026-a4f5-b2e6dfd5a07b") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Trippier_pass_ger<-events%>%
  filter(id=="c2d4b005-1fe8-4026-a4f5-b2e6dfd5a07b")
P1ENG_GER<- ffdpass %>%
  filter(id=="6eb76407-f517-4e79-814d-a68505cc1bb5") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Walker_pass_ger<-events%>%
  filter(id=="6eb76407-f517-4e79-814d-a68505cc1bb5")
P2ENG_GER<- ffdpass %>%
  filter(id=="d99c3d7b-f31d-4af5-8c9d-39be01d90893") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Phillips_pass<-events%>%
  filter(id=="d99c3d7b-f31d-4af5-8c9d-39be01d90893")
P3ENG_GER<- ffdpass %>%
  filter(id=="c9fc1306-476d-4ba0-8eef-b5ed70713050") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Trippier_pass_ger2<-events%>%
  filter(id=="c9fc1306-476d-4ba0-8eef-b5ed70713050")
S1ENG_GER<- ffdshot %>%
  filter(id=="3bf895cf-55a8-4367-b76a-eb07a85156ad") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Maguire_shot<-events%>%
  filter(id=="3bf895cf-55a8-4367-b76a-eb07a85156ad")
#Trippier>clearance
#Walker>Phillips>Trippier>Maguire shot
#min 26:08, outcome: goal kick

#England vs Denmark: 75a6c11b-8f3c-48a9-bb98-f0c17b959b4f
ENG_DEN_m<- ffdelayed %>%
  filter(id=="75a6c11b-8f3c-48a9-bb98-f0c17b959b4f") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Mount_pass_den<-events%>%
  filter(id=="75a6c11b-8f3c-48a9-bb98-f0c17b959b4f")
P1ENG_DEN<- ffdpass %>%
  filter(id=="bb294dab-1894-4d45-874a-e5b32bcdf89a") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Walker_pass<-events%>%
  filter(id=="bb294dab-1894-4d45-874a-e5b32bcdf89a")
P2ENG_DEN<- ffdpass %>%
  filter(id=="84ebc4c7-5cd7-4732-bff6-8556404b67ab") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Mount_pass_den2<-events%>%
  filter(id=="84ebc4c7-5cd7-4732-bff6-8556404b67ab")
P3ENG_DEN<- ffdpass %>%
  filter(id=="8b0d0e8f-81fb-4097-a5bb-010c8953b8c2") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Kane_pass<-events%>%
  filter(id=="8b0d0e8f-81fb-4097-a5bb-010c8953b8c2")
S1ENG_DEN<- ffdshot %>%
  filter(id=="2ad830f5-8e50-468b-99d2-5acbec5d0ccc") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Sterling_shot<-events%>%
  filter(id=="2ad830f5-8e50-468b-99d2-5acbec5d0ccc")
#Mount>clearance
#Walker>Mount>Kane>Sterling shot
#min 

#England vs Denmark: c4002551-fa58-4ef5-bead-8a3df79f402c
ENG_DEN_l2<- ffdelayed %>%
  filter(id=="c4002551-fa58-4ef5-bead-8a3df79f402c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Mount_pass_denl<-events%>%
  filter(id=="c4002551-fa58-4ef5-bead-8a3df79f402c")
S1ENG_DEN_l2<- ffdshot %>%
  filter(id=="7c512179-f1fa-4c91-aa07-69cfb0cb9e18") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Maguire_shot_den<-events%>%
  filter(id=="7c512179-f1fa-4c91-aa07-69cfb0cb9e18")
P1ENG_DEN_l2<- ffdpass %>%
  filter(id=="841dcb0d-bfe2-4181-8153-a7759c3c7693") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Walker_pass_den4<-events%>%
  filter(id=="841dcb0d-bfe2-4181-8153-a7759c3c7693")
P2ENG_DEN_l2<- ffdpass %>%
  filter(id=="01a0e5dd-a8e0-4fa8-bc4c-d8f7d05e6bec") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Saka_pass_den2<-events%>%
  filter(id=="01a0e5dd-a8e0-4fa8-bc4c-d8f7d05e6bec")
P3ENG_DEN_l2<- ffdpass %>%
  filter(id=="afb8d5a6-d778-4b0d-8ae4-ecd54504bbcb") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Walker_pass_den5<-events%>%
  filter(id=="afb8d5a6-d778-4b0d-8ae4-ecd54504bbcb")
P4ENG_DEN_l2<- ffdpass %>%
  filter(id=="16f7c89b-a7cc-4203-9d9d-408bb34b6a47") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Phillips_pass_den3<-events%>%
  filter(id=="16f7c89b-a7cc-4203-9d9d-408bb34b6a47")
S2ENG_DEN_l2<- ffdshot %>%
  filter(id=="6d6ece63-0a50-4c0c-9186-78cfb300585b") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Mount_shot_den<-events%>%
  filter(id=="6d6ece63-0a50-4c0c-9186-78cfb300585b")
#shot...pass,pass,pass,pass,shot
#Maguire... Walker, Saka, Walker, Phillips, Mount (shot)
#min 59:36, lost possession

#England vs Denmark: bb290ccd-79ba-47e1-8c13-19ec95c8c273
ENG_DEN_l<- ffdelayed %>%
  filter(id=="bb290ccd-79ba-47e1-8c13-19ec95c8c273") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Foden_pass_denl<-events%>%
  filter(id=="bb290ccd-79ba-47e1-8c13-19ec95c8c273")
S1ENG_DEN_l<- ffdshot %>%
  filter(id=="975b6c3c-043b-4b41-b4af-31058546338d") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Stones_shot_den<-events%>%
  filter(id=="975b6c3c-043b-4b41-b4af-31058546338d")
P1ENG_DEN_l<- ffdpass %>%
  filter(id=="29b91083-e6d3-4ed6-9b77-2a4c4bc957b8") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Foden_pass_denl2<-events%>%
  filter(id=="29b91083-e6d3-4ed6-9b77-2a4c4bc957b8")
S2ENG_DEN_l<- ffdshot %>%
  filter(id=="d03c065b-dc27-4f24-829f-746beec384e7") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Grealish_shot_den<-events%>%
  filter(id=="d03c065b-dc27-4f24-829f-746beec384e7")
P2ENG_DEN_l<- ffdpass %>%
  filter(id=="ec2d18da-d843-4b13-98fd-9ccfa74cb63e") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Foden_pass_denl3<-events%>%
  filter(id=="ec2d18da-d843-4b13-98fd-9ccfa74cb63e")
P3ENG_DEN_l<- ffdpass %>%
  filter(id=="6ed7968a-4679-4b4d-bf5c-e26f5ac71106") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Grealish_pass_den<-events%>%
  filter(id=="6ed7968a-4679-4b4d-bf5c-e26f5ac71106")
S3ENG_DEN_l<- ffdshot %>%
  filter(id=="b73448a1-a04b-4c2f-9eac-b4f54717a585") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Sterling_shot_den<-events%>%
  filter(id=="b73448a1-a04b-4c2f-9eac-b4f54717a585")
#shot, pass, shot, pass, pass, shot
#Stones, Foden, Grealish, Foden, Grealish, Sterling
#min 97:25, goal kick

#Germany vs Hungary: c5f20e2f-6453-4481-b3bf-bba9dcc2f1b6
GER_HUN_l<- ffdelayed %>%
  filter(id=="c5f20e2f-6453-4481-b3bf-bba9dcc2f1b6") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Kimmich_pass_hun<-events%>%
  filter(id=="c5f20e2f-6453-4481-b3bf-bba9dcc2f1b6")
S1GER_HUN_l<- ffdshot %>%
  filter(id=="c351c87e-6fb3-4a32-8c26-d97cb4cbfae1") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Mats
Hummels_shot_hun<-events%>%
  filter(id=="c351c87e-6fb3-4a32-8c26-d97cb4cbfae1")
P1GER_HUN_l<- ffdpass %>%
  filter(id=="26218927-dca9-4cd9-a9f0-6521b12af53c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Havertz_pass_hun<-events%>%
  filter(id=="26218927-dca9-4cd9-a9f0-6521b12af53c")
P2GER_HUN_l<- ffdpass %>%
  filter(id=="4d4ca9f7-fead-46f9-a30b-f50ef8eff9c0") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Kroos_pass_hun<-events%>%
  filter(id=="4d4ca9f7-fead-46f9-a30b-f50ef8eff9c0")
P3GER_HUN_l<- ffdpass %>%
  filter(id=="88252ed4-dd8c-47a9-a2dc-17edb81dfd42") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Sane_pass_hun<-events%>%
  filter(id=="88252ed4-dd8c-47a9-a2dc-17edb81dfd42")
P4GER_HUN_l<- ffdpass %>%
  filter(id=="331a5818-0f11-4b35-b54d-94737cb21098") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Gosens_pass_hun<-events%>%
  filter(id=="331a5818-0f11-4b35-b54d-94737cb21098")
P5GER_HUN_l<- ffdpass %>%
  filter(id=="2d8c06cd-800d-48f2-a378-573c118bb30c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Sane_pass_hun2<-events%>%
  filter(id=="2d8c06cd-800d-48f2-a378-573c118bb30c")
P6GER_HUN_l<- ffdpass %>%
  filter(id=="bc169532-b2fd-44a4-addb-7b1f93fc34c7") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Gosens_pass_hun2<-events%>%
  filter(id=="bc169532-b2fd-44a4-addb-7b1f93fc34c7")
P7GER_HUN_l<- ffdpass %>%
  filter(id=="d029a44c-08bd-40a5-8616-346106d2e67e") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Havertz_pass_hun2<-events%>%
  filter(id=="d029a44c-08bd-40a5-8616-346106d2e67e")
P8GER_HUN_l<- ffdpass %>%
  filter(id=="30873083-9186-46a3-adb9-4d2e3140ffec") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Matthias
Ginter_pass_hun<-events%>%
  filter(id=="30873083-9186-46a3-adb9-4d2e3140ffec")
S2GER_HUN_l<- ffdshot %>%
  filter(id=="4f772f2c-b4eb-4e78-820f-37d8d4964a09") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Ginter_shot_hun<-events%>%
  filter(id=="4f772f2c-b4eb-4e78-820f-37d8d4964a09")
#shot... pass, pass, (clearance, ball recovery), shot
#Hummels... (25 seconds) Kai Havertz, Matthias Ginter, Ginter (shot)
#(block)(block)(ball recovery)Gosens>Sane>Gosens>*Havertz>Ginter
#min 20:24, shot saved

#Italy vs Austria: cccdd38c-a399-4fec-a209-4604e45160fd
ITA_AUS_l<- ffdelayed %>%
  filter(id=="cccdd38c-a399-4fec-a209-4604e45160fd") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Insigne_pass_aus<-events%>%
  filter(id=="cccdd38c-a399-4fec-a209-4604e45160fd")
S1ITA_AUS_l<- ffdshot %>%
  filter(id=="1237339e-f825-42b9-8e42-4aaf695ede02") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Giovanni Di Lorenzo
DiLorenzo_shot<-events%>%
  filter(id=="1237339e-f825-42b9-8e42-4aaf695ede02")
P1ITA_AUS_l<- ffdpass %>%
  filter(id=="d797e823-ade6-418b-a658-3060e666df52") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Manuel
Locatelli_pass_aus<-events%>%
  filter(id=="d797e823-ade6-418b-a658-3060e666df52")
P2ITA_AUS_l<- ffdpass %>%
  filter(id=="48654e24-e81f-4cec-859a-549127f795f4") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Insigne_pass_aus2<-events%>%
  filter(id=="48654e24-e81f-4cec-859a-549127f795f4")
P3ITA_AUS_l<- ffdpass %>%
  filter(id=="5f8a2fdc-d4b6-4554-85de-c93156f65751") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Leonardo Spinazzola
Spinazzola_pass_aus<-events%>%
  filter(id=="5f8a2fdc-d4b6-4554-85de-c93156f65751")
P4ITA_AUS_l<- ffdpass %>%
  filter(id=="4d722c2e-5653-4058-a490-7768da557b6c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Insigne_pass_aus3<-events%>%
  filter(id=="4d722c2e-5653-4058-a490-7768da557b6c")
P5ITA_AUS_l<- ffdpass %>%
  filter(id=="e291259a-3f4d-4b40-b505-0bb208fff6ab") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Francesco Acerbi
Acerbi_pass_aus<-events%>%
  filter(id=="e291259a-3f4d-4b40-b505-0bb208fff6ab")
S2ITA_AUS_l<- ffdshot %>%
  filter(id=="164224d6-3b39-4032-af5e-376a9e309920") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Matteo Pessina
Pessina_shot_aus<-events%>%
  filter(id=="164224d6-3b39-4032-af5e-376a9e309920")
#shot... pass, pass, pass, shot
#Giovanni Di Lorenzo... Leonardo Spinazzola, Lorenzo Insigne, Francesco Acerbi, Matteo Pessina (shot)
#min 104:24, goal (second shot 24 seconds after corner)

#North Macedonia vs Netherlands: 18c711ba-68ca-465f-9721-70bdc1dc41c1
NMA_NED_s<- ffdelayed %>%
  filter(id=="18c711ba-68ca-465f-9721-70bdc1dc41c1") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Enis Bardhi
Bardhi_pass<-events %>%
  filter(id=="18c711ba-68ca-465f-9721-70bdc1dc41c1")
P1NMA_NED<-ffdpass %>%
  filter(id=="a4c0eae7-397f-410b-aa31-8143fc90e85f") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Aleksandar Trajkovski
Trajovski_pass<-events%>%
  filter(id=="a4c0eae7-397f-410b-aa31-8143fc90e85f")
S1NMA_NED<- ffdshot%>%
  filter(id=="6e6a699b-a6e4-48df-bff8-d0eda476c4f0") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Bardhi_shot1<-events%>%
  filter(id=="6e6a699b-a6e4-48df-bff8-d0eda476c4f0")
P2NMA_NED<-ffdpass %>%
  filter(id=="58a54201-a4fb-48d3-bb70-1bdb580fc2d2") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ezgjan Alioski
Alioski_pass_ned<-events%>%
  filter(id=="58a54201-a4fb-48d3-bb70-1bdb580fc2d2")
P3NMA_NED<-ffdpass %>%
  filter(id=="3bc3d649-a0cd-42eb-a7f3-ae31bde56a35") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Aleksandar Trajkvoski
Trajkvoski_pass_ned2<-events%>%
  filter(id=="3bc3d649-a0cd-42eb-a7f3-ae31bde56a35")
P4NMA_NED<-ffdpass %>%
  filter(id=="608390e3-505d-491d-8133-a2a90aaa606d") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Arijan Ademi
Ademi_pass_ned<-events%>%
  filter(id=="608390e3-505d-491d-8133-a2a90aaa606d")
P5NMA_NED<-ffdpass %>%
  filter(id=="25016d68-3d37-458c-9d1e-ff2a0b958e6c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Eljif Elmas
Elmas_pass<-events%>%
  filter(id=="25016d68-3d37-458c-9d1e-ff2a0b958e6c")
S2NMA_NED<- ffdshot%>%
  filter(id=="1ee6f3b7-aba7-4bcd-8ff2-359c85cb4161") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Bardhi_shot2<-events%>%
  filter(id=="1ee6f3b7-aba7-4bcd-8ff2-359c85cb4161")
#Bardhi>Aleksandar Trajkovski>Bardhi (shot)
#Later in the sequence: Eljif Elmas>Bardhi (shot)
#min 38:36, outcome: out for goal kick after second shot in sequence
#(block)(ball recovery)Ezgjan Alioski>Aleksandar Trajkvoski>Arijan Ademi
#(clearance)(block)(ball recovery)Eljif Elmas>Enis Bardhi (shot)

#Spain vs Slovakia: 29ad3e23-9079-4a8c-96f3-0f38101e23b9
ESP_SLO_l<- ffdelayed %>%
  filter(id=="29ad3e23-9079-4a8c-96f3-0f38101e23b9") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Koke_pass_slo<-events%>%
  filter(id=="29ad3e23-9079-4a8c-96f3-0f38101e23b9")
P1ESP_SLO_l<- ffdpass %>%
  filter(id=="5e8c669e-24b1-4b3b-915d-484f0d4decb8") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Pedri_pass_slol<-events%>%
  filter(id=="5e8c669e-24b1-4b3b-915d-484f0d4decb8")
P2ESP_SLO_l<- ffdpass %>%
  filter(id=="d138078b-aefd-460b-84f4-e44638cfb9d0") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Moreno_pass<-events%>%
  filter(id=="d138078b-aefd-460b-84f4-e44638cfb9d0")
S1ESP_SLO_l<- ffdshot %>%
  filter(id=="cea85a31-f9c4-4d48-a87d-d69e2f488f6e") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Laporte_shot<-events%>%
  filter(id=="cea85a31-f9c4-4d48-a87d-d69e2f488f6e")
#(clearance), pass, pass, shot
#Pedri, Gerard Moreno, Aymeric Laporte
#min 47:17, goal

#Italy vs Turkey: 1701a359-312f-4649-ae4c-52f2af7fc174
S1ITA_TUR_l<- ffdshot %>%
  filter(id=="ea779639-e6b2-42dc-9141-4a6f5bc3545c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Nicolò Barella
Barella_shot_tur<-events%>%
  filter(id=="ea779639-e6b2-42dc-9141-4a6f5bc3545c")

P1ITA_TUR_l<- ffdpass %>%
  filter(id=="18a59614-855d-4106-87e0-92887c65819a") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Jorge
Filho_pass_tur<-events%>%
  filter(id=="18a59614-855d-4106-87e0-92887c65819a")
P2ITA_TUR_l<- ffdpass %>%
  filter(id=="92e20ff6-ea66-471c-a230-f00d56802ca1") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Leonardo
Spinazzola_pass_tur1<-events%>%
  filter(id=="92e20ff6-ea66-471c-a230-f00d56802ca1")
P3ITA_TUR_l<- ffdpass %>%
  filter(id=="215f2f4c-fb5a-4d73-9624-938c017c12e7") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))

Insigne_pass_tur<-events%>%
  filter(id=="215f2f4c-fb5a-4d73-9624-938c017c12e7")
P4ITA_TUR_l<- ffdpass %>%
  filter(id=="2fb4032a-43fb-48e0-a0a0-4415a93bfda8") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Spinazzola_pass_tur2<-events%>%
  filter(id=="2fb4032a-43fb-48e0-a0a0-4415a93bfda8")
P5ITA_TUR_l<- ffdpass %>%
  filter(id=="08f13e44-7919-496d-96f8-21e32913b893") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Barella_pass_tur<-events%>%
  filter(id=="08f13e44-7919-496d-96f8-21e32913b893")
P6ITA_TUR_l<- ffdpass %>%
  filter(id=="a6d8de2f-e694-48b8-b70b-e1ddcc27a233") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Insigne_pass_tur2<-events%>%
  filter(id=="a6d8de2f-e694-48b8-b70b-e1ddcc27a233")
P7ITA_TUR_l<- ffdpass %>%
  filter(id=="8dd1f848-c9e0-49b8-a978-8226579af228") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
DiLorenzo_pass_tur<-events%>%
  filter(id=="8dd1f848-c9e0-49b8-a978-8226579af228")
S2ITA_TUR_l<- ffdshot %>%
  filter(id=="03e9fea5-d2d5-4f72-bb37-e09eaa771914") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Berardi_shot_tur<-events%>%
  filter(id=="03e9fea5-d2d5-4f72-bb37-e09eaa771914")
#Corner>(clearance)(ball recovery)Nicolo Barella (shot)
#(block)Jorge Filho>Leonardo Spinazzola>Lorenzo Insigne>Spinazzola>Nicolo Barella>Insigne
#(ball recovery)(pass)Giovanni Di Lorenzo
#(clearance)(ball recovery)Domenico Berardi (shot)
#min 58:08, goal kick

#Italy vs Spain: 18ea3663-46b9-4fa6-a92c-953defd6b8dd
P1ESP_ITA_l<- ffdpass %>%
  filter(id=="2faceece-b1ff-408e-8a89-6a082713904d") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Alba_pass_ita<-events%>%
  filter(id=="2faceece-b1ff-408e-8a89-6a082713904d")
P2ESP_ITA_l<- ffdpass %>%
  filter(id=="1626db23-1370-4c41-a0f6-1fdf30539136") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Carvajal_pass_ita<-events%>%
  filter(id=="1626db23-1370-4c41-a0f6-1fdf30539136")
P1ITA_ESP_l<- ffdpass %>%
  filter(id=="9604e254-9778-4771-a6f2-b37654646341") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Bonucci_pass_esp<-events%>%
  filter(id=="9604e254-9778-4771-a6f2-b37654646341")
P2ITA_ESP_l<- ffdpass %>%
  filter(id=="38d8facb-35dd-488b-ae2d-d987bcea94a0") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Emerson Palmieri
Palmieri_pass_esp<-events%>%
  filter(id=="38d8facb-35dd-488b-ae2d-d987bcea94a0")
P3ESP_ITA_l<- ffdpass %>%
  filter(id=="3ab0293a-578e-4c0a-ba7d-6888a3962d6a") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Cesar Azpilicueta
Azpilicueta_pass_ita<-events%>%
  filter(id=="3ab0293a-578e-4c0a-ba7d-6888a3962d6a")
P3ITA_ESP_l<- ffdpass %>%
  filter(id=="d5917167-93da-48a8-a881-06602ae47033") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Jorge Filho
Filho_pass_esp<-events%>%
  filter(id=="d5917167-93da-48a8-a881-06602ae47033")
P4ESP_ITA_l<- ffdpass %>%
  filter(id=="03fc09e1-b354-4108-b044-8843389ffeba") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Laporte_pass_ita<-events%>%
  filter(id=="03fc09e1-b354-4108-b044-8843389ffeba")
S1ITA_ESP_l<- ffdshot %>%
  filter(id=="17ee2f7a-68ad-4033-a62f-f208971ad9c5") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Immobile_shot_esp<-events%>%
  filter(id=="17ee2f7a-68ad-4033-a62f-f208971ad9c5")

#Portugal vs Belgium: 422a553b-bfbe-4612-9f4c-4f2986e25a57
S1POR_BEL_l<- ffdshot %>%
  filter(id=="8953c8ea-8e56-4922-8999-037364ae1896") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Rúben
Dias_shot_bel<-events%>%
  filter(id=="8953c8ea-8e56-4922-8999-037364ae1896")
P1POR_BEL_l<- ffdpass %>%
  filter(id=="09a1c14e-57ca-482a-b9b9-6b3b87c270db") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#João Félix
Felix_pass_bel<-events%>%
  filter(id=="09a1c14e-57ca-482a-b9b9-6b3b87c270db")
P2POR_BEL_l<- ffdpass %>%
  filter(id=="de3b15ae-e0db-4441-8ec3-a283c4fc47a7") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Diogo
Dalot_pass_bel<-events%>%
  filter(id=="de3b15ae-e0db-4441-8ec3-a283c4fc47a7")
P3POR_BEL_l<- ffdpass %>%
  filter(id=="8ef6c873-149d-43e6-94be-8e0dfa287825") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Fernandes_pass_bel2<-events%>%
  filter(id=="8ef6c873-149d-43e6-94be-8e0dfa287825")
P4POR_BEL_l<- ffdpass %>%
  filter(id=="cc310b88-b9f2-48b8-ac98-a259dd8c9b97") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Sérgio Oliveira
Oliveira_pass_bel<-events%>%
  filter(id=="cc310b88-b9f2-48b8-ac98-a259dd8c9b97")
P5POR_BEL_l<- ffdpass %>%
  filter(id=="a278eef9-da2f-4036-a9b2-4167c5da24bf") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Kléper Ferreira
Ferreira_pass_bel<-events%>%
  filter(id=="a278eef9-da2f-4036-a9b2-4167c5da24bf")
S2POR_BEL_l<- ffdshot %>%
  filter(id=="632a06a3-16ed-4c54-b86d-e55c3330b086") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#João Félix
Felix_shot_bel<-events%>%
  filter(id=="632a06a3-16ed-4c54-b86d-e55c3330b086")
#Fernandes (corner)>Rúben Dias (shot)>João Félix (pass)(clearance)
#(ball recovery por)Diogo Dalot>Fernandes>Sérgio Oliveira>Kléper Ferreira (clearance)
#(ball recovery por)João Félix (shot)

#Slovakia vs Poland: b62d2a28-3ddb-4b22-90df-03165bf8aa94
P1SLO_POL_l<- ffdpass %>%
  filter(id=="3750f336-6c91-4e19-a748-0d7d51ca74f1") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Lukás Haraslín
Haraslin_pass_pol<-events%>%
  filter(id=="3750f336-6c91-4e19-a748-0d7d51ca74f1")
P2SLO_POL_l<- ffdpass %>%
  filter(id=="8caec2cc-8a13-4af1-b73d-86d69aa90e93") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ondrej Duda
Duda_pass_pol<-events%>%
  filter(id=="8caec2cc-8a13-4af1-b73d-86d69aa90e93")
P3SLO_POL_l<- ffdpass %>%
  filter(id=="c081b5c0-6882-41df-ae09-6c2e3637c7c7") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Róbert
Mak_pass_pol2<-events%>%
  filter(id=="c081b5c0-6882-41df-ae09-6c2e3637c7c7")
P4SLO_POL_l<- ffdpass %>%
  filter(id=="ccdcb4c1-4aa4-44ad-977b-bd9c89975de9") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Lukás Haraslín
Haraslin_pass_pol2<-events%>%
  filter(id=="ccdcb4c1-4aa4-44ad-977b-bd9c89975de9")
P5SLO_POL_l<- ffdpass %>%
  filter(id=="55e897d8-422d-42f3-8bd0-743521d4c290") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Tomás Hubocan
Hubocan_pass_pol<-events%>%
  filter(id=="55e897d8-422d-42f3-8bd0-743521d4c290")
P1POL_SLO_l<- ffdpass %>%
  filter(id=="19523c67-ed40-4e98-96e5-c2a711125cdd") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Kamil Józwiak
Jozwiak_pass_slo<-events%>%
  filter(id=="19523c67-ed40-4e98-96e5-c2a711125cdd")
S1SLO_POL_l<- ffdshot %>%
  filter(id=="8aa8ff35-40d3-4224-a3a1-392d5636d8e5") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Juraj Kucka
Kucka_pass_pol<-events%>%
  filter(id=="8aa8ff35-40d3-4224-a3a1-392d5636d8e5")
#Róbert Mak(clearance)(clearance)
#(ball recovery slo)Lukás Haraslín>Ondrej Duda>Róbert Mak>Lukás Haraslín>Tomás Hubocan
#(block pol)Kamil Józwiak (pol)
#(ball recovery slo)Juraj Kucka
#min 26:12, goal kick

#Slovakia vs Sweden: ecedaeff-afdb-4885-9f31-776f9a57affb
P1SLO_SWE_l<- ffdpass %>%
  filter(id=="ba9ff41f-3fc0-4acc-8175-63eb81046907") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Juraj Kucka
Kucka_pass_swe<-events%>%
  filter(id=="ba9ff41f-3fc0-4acc-8175-63eb81046907")
P2SLO_SWE_l<- ffdpass %>%
  filter(id=="f3ea96d4-10e5-448f-bef6-73bf95cc4981") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Peter Pekarík
Pekarik_pass_swe<-events%>%
  filter(id=="f3ea96d4-10e5-448f-bef6-73bf95cc4981")
S1SLO_SWE_l<- ffdshot %>%
  filter(id=="cc58f290-6272-417e-9f81-d7fa385b26d9") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ondrej Duda
Duda_shot_swe<-events%>%
  filter(id=="cc58f290-6272-417e-9f81-d7fa385b26d9")
#Róbert Mak (clearance)
#(ball recovery slo)Juraj Kucka>Peter Pekarík>Ondrej Duda (shot)
#min 49:33, goal kick

#Spain vs Sweden: 26ab9360-4eae-4378-969e-95ce55bd9271
P1ESP_SWE_l<- ffdpass %>%
  filter(id=="ef01f3a4-c359-4eb4-8145-6848cc877d2b") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Pedri_pass_swe<-events%>%
  filter(id=="ef01f3a4-c359-4eb4-8145-6848cc877d2b")
P2ESP_SWE_l<- ffdpass %>%
  filter(id=="47d8cb0a-9e1c-45ea-9270-244adedb96a1") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Alba_pass_swe<-events%>%
  filter(id=="47d8cb0a-9e1c-45ea-9270-244adedb96a1")
S1ESP_SWE_l<- ffdshot %>%
  filter(id=="532e7087-f55f-4f88-96a1-9b6a12b4c062") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Carvajal_pass_swe<-events%>%
  filter(id=="532e7087-f55f-4f88-96a1-9b6a12b4c062")
#Pedri>Alba>Carvajal
#min 43:52, throw in

#Spain vs Croatia: 28d08199-de76-40b8-a352-1538ea517691
P1ESP_CRO_l<- ffdpass %>%
  filter(id=="b66e3e86-0a67-4cfc-801e-bfc7b783d7da") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Koke_pass_cro2<-events%>%
  filter(id=="b66e3e86-0a67-4cfc-801e-bfc7b783d7da")
P2ESP_CRO_l<- ffdpass %>%
  filter(id=="c8e6442f-abfe-4a95-b454-d13f2f87b741") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Pedri_pass_cro<-events%>%
  filter(id=="c8e6442f-abfe-4a95-b454-d13f2f87b741")
P3ESP_CRO_l<- ffdpass %>%
  filter(id=="ef7a44bb-6655-4af0-8ecb-31d4f19f2680") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#José Gayà
Gaya_pass_cro<-events%>%
  filter(id=="ef7a44bb-6655-4af0-8ecb-31d4f19f2680")
P4ESP_CRO_l<- ffdpass %>%
  filter(id=="81884eb3-5542-424b-8d17-6a3fc8bd10c0") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ferrán Torres
Torres_pass_cro<-events%>%
  filter(id=="81884eb3-5542-424b-8d17-6a3fc8bd10c0")
S1ESP_CRO_l<- ffdshot %>%
  filter(id=="02ee8ae1-2ce2-4d0f-9254-0d23916c6e9c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Koke_shot_cro<-events%>%
  filter(id=="02ee8ae1-2ce2-4d0f-9254-0d23916c6e9c")
#(clearance)Koke(clearance)(clearance)
#(block esp)Pedri>José Gayà>Ferrán Torres(clearance)
#(ball recovery esp)Koke (shot)
#min 41:31, goal kick

#Switzerland vs Turkey: c7998f84-aa47-4dea-9616-6ca135c20e36
P1SUI_TUR_l<- ffdpass %>%
  filter(id=="4cd38988-c2d4-4013-b869-27079eb9ecc0") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Granit
Xhaka_pass_tur<-events%>%
  filter(id=="4cd38988-c2d4-4013-b869-27079eb9ecc0")
P2SUI_TUR_l<- ffdpass %>%
  filter(id=="caddfb9c-3739-465d-aad6-18527efd0cc2") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Haris Seferovic
Seferovic_pass_tur<-events%>%
  filter(id=="caddfb9c-3739-465d-aad6-18527efd0cc2")
P1TUR_SUI_l<- ffdpass %>%
  filter(id=="9f05bae9-e951-4dfc-9e46-fa3d14af70d1") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ugurcan Çakir
Cakir_pass_sui<-events%>%
  filter(id=="9f05bae9-e951-4dfc-9e46-fa3d14af70d1")
P3SUI_TUR_l<- ffdpass %>%
  filter(id=="6d95de57-4e86-46cc-a2ea-b796f9b3c5d4") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ricardo Rodríguez
Rodriguez_pass_tur<-events%>%
  filter(id=="6d95de57-4e86-46cc-a2ea-b796f9b3c5d4")
P4SUI_TUR_l<- ffdpass %>%
  filter(id=="12b17b87-bbdb-4daa-84b0-26986d3cbd50") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Xhaka_pass_tur2<-events%>%
  filter(id=="12b17b87-bbdb-4daa-84b0-26986d3cbd50")
P5SUI_TUR_l<- ffdpass %>%
  filter(id=="91a81906-1188-4dc5-8744-a8de9caaf034") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Steven Zuber
Zuber_pass_tur<-events%>%
  filter(id=="91a81906-1188-4dc5-8744-a8de9caaf034")
P6SUI_TUR_l<- ffdpass %>%
  filter(id=="a3e00146-08fe-4d55-af91-dc3a3534c3f2") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Haris Seferovic
Seferovic_pass_tur2<-events%>%
  filter(id=="a3e00146-08fe-4d55-af91-dc3a3534c3f2")
P7SUI_TUR_l<- ffdpass %>%
  filter(id=="63db13f5-b25e-403f-b980-2036f2c0ee14") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Xhaka_pass_tur3<-events%>%
  filter(id=="63db13f5-b25e-403f-b980-2036f2c0ee14")
P8SUI_TUR_l<- ffdpass %>%
  filter(id=="fbb1fb8d-68a2-4e29-be81-469ab8e67018") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Remo Freuler
Freuler_pass_tur<-events%>%
  filter(id=="fbb1fb8d-68a2-4e29-be81-469ab8e67018")
S1SUI_TUR_l<- ffdshot %>%
  filter(id=="28e7aed2-d8c4-487c-bfa4-6d30f749e953") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Shaqiri_shot_tur<-events%>%
  filter(id=="28e7aed2-d8c4-487c-bfa4-6d30f749e953")
#Xherdan Shaqiri>Granit Xhaka>Haris Seferovic
#(ball recovery tur)Ugurcan Çakir(ball recovery sui)
#Ricardo Rodríguez>Xhaka>Steven Zuber>Haris Seferovic>Xhaka>Remo Freuler>Shaqiri(shot)(deflection?)
#45:25, goal

#Switzerland vs France: 8edaae25-2ed3-4fcc-ba91-b5bd7de83a94
P1SUI_FRA_l<- ffdpass %>%
  filter(id=="abceffc8-86c0-4184-bd93-f79fc6cf036c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Manuel Akanji
Akanji_pass_tur<-events%>%
  filter(id=="abceffc8-86c0-4184-bd93-f79fc6cf036c")
P2SUI_FRA_l<- ffdpass %>%
  filter(id=="5871c752-4a33-43e8-a4c5-1a75f1722312") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Admir Mehmedi
Mehmedi_pass_tur<-events%>%
  filter(id=="5871c752-4a33-43e8-a4c5-1a75f1722312")
S1SUI_FRA_l<- ffdshot %>%
  filter(id=="2ae7f87f-dff7-4f2c-9de0-4cf39d8a69b9") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Admir Mehmedi
Mehmedi_shot_tur<-events%>%
  filter(id=="2ae7f87f-dff7-4f2c-9de0-4cf39d8a69b9")
#Ruban Vargas(clearance)
#Manuel Akanji>Admir Mehmedi(clearance)
#(ball recovery sui)(clearance)(ball recovery sui)Admir Mehmedi(shot)
#min 102:44, goal kick

#Turkey vs Wales: 31ee74a4-06a4-4822-824f-48edb3d8923d
P1TUR_WAL_l<- ffdpass %>%
  filter(id=="bac1c80c-fde7-4545-b481-fec95903c24b") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Merih Demiral
Demiral_pass_wal<-events%>%
  filter(id=="bac1c80c-fde7-4545-b481-fec95903c24b")
P2TUR_WAL_l<- ffdpass %>%
  filter(id=="0bed2e2d-a304-42fd-8f21-8d2f1d9c9d73") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Kaan Ayhan
Ayhan_pass_wal<-events%>%
  filter(id=="0bed2e2d-a304-42fd-8f21-8d2f1d9c9d73")
S1TUR_WAL_l<- ffdshot %>%
  filter(id=="fd669b96-c078-4411-b60d-e0c39bdd402e") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Burak Yilmaz
Yilmaz_shot_wal<-events%>%
  filter(id=="fd669b96-c078-4411-b60d-e0c39bdd402e")
#Merih Demiral>Kaan Ayhan>Burak Yilmaz (shot)
#min 53:47, goal kick

#Turkey vs Switzerland: f07e6e8d-4d54-4e1d-850a-c10fef7d1b0f
P1TUR_SUI_l<- ffdpass %>%
  filter(id=="56173d10-e242-42a3-a2a7-b76526178211") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Mehmet Çelik
Celik_pass_sui<-events%>%
  filter(id=="56173d10-e242-42a3-a2a7-b76526178211")
P2TUR_SUI_l<- ffdpass %>%
  filter(id=="e8c3dec9-b0c8-4176-b276-11dfd3d71dd6") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Kaan Ayhan
Ayhan_pass_sui<-events%>%
  filter(id=="e8c3dec9-b0c8-4176-b276-11dfd3d71dd6")
P3TUR_SUI_l<- ffdpass %>%
  filter(id=="44ad1a76-f1a5-474b-be12-3ccf440eb922") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Mehmet Çelik
Celik_pass_sui2<-events%>%
  filter(id=="44ad1a76-f1a5-474b-be12-3ccf440eb922")
P4TUR_SUI_l<- ffdpass %>%
  filter(id=="d8808b2e-bd1e-46b8-8e2c-5726fd5255bc") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ugurcan Çakir
Cakir_pass_sui<-events%>%
  filter(id=="d8808b2e-bd1e-46b8-8e2c-5726fd5255bc")
P4SUI_TUR2_l<- ffdpass %>%
  filter(id=="63a4fb5a-d5b0-46e8-9f80-054d1a2f2f8c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Silvan Widmer
Widmer_pass_tur<-events%>%
  filter(id=="63a4fb5a-d5b0-46e8-9f80-054d1a2f2f8c")
P5TUR_SUI_l<- ffdpass %>%
  filter(id=="c09b0d99-6cc2-4b01-939e-c1b0e46a80f8") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Irfan Kahveci
Kahveci_pass_sui<-events%>%
  filter(id=="c09b0d99-6cc2-4b01-939e-c1b0e46a80f8")
P6TUR_SUI_l<- ffdpass %>%
  filter(id=="909dc2ee-7f3c-4c75-b4ac-04d59ac46896") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Burak Yilmaz
Yilmaz_pass_sui<-events%>%
  filter(id=="909dc2ee-7f3c-4c75-b4ac-04d59ac46896")
P7TUR_SUI_l<- ffdpass %>%
  filter(id=="76cbb6cb-0f80-44d8-ae01-b95b7bcc5d58") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Irfan Kahveci
Kahveci_pass_sui2<-events%>%
  filter(id=="76cbb6cb-0f80-44d8-ae01-b95b7bcc5d58")
S1TUR_SUI_l<- ffdshot %>%
  filter(id=="d1d13791-0bbd-4c53-9c2e-1da17a64c35c") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Ozan Tufan
Tufan_pass_sui<-events%>%
  filter(id=="d1d13791-0bbd-4c53-9c2e-1da17a64c35c")
#Hakan Çalhanoglu(clearance)
#Mehmet Çelik>Kaan Ayhan>Mehmet Çelik>Ugurcan Çakir>Silvan Widmer(sui)
#(ball recovery tur)Irfan Kahveci>Burak Yilmaz>Irfan Kahveci>Ozan Tufan(shot)
#min 2:22, goal kick

#Wales vs Turkey: 57bd9769-0f7c-4fd1-b793-d930596a9340
P1WAL_TUR_l<- ffdpass %>%
  filter(id=="6b4631fe-054e-4836-8e5c-7259d403b268") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Joe
Allen_pass_tur<-events%>%
  filter(id=="6b4631fe-054e-4836-8e5c-7259d403b268")
P1TUR_WAL2_l<- ffdpass %>%
  filter(id=="aa116a5c-6053-4bb3-9a4b-541ea0394368") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Kaan
Ayhan_pass_tur<-events%>%
  filter(id=="aa116a5c-6053-4bb3-9a4b-541ea0394368")
P2WAL_TUR_l<- ffdpass %>%
  filter(id=="d0046789-8ab6-4191-9c4d-9e172c6b1acb") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Joe
Rodon_pass_tur<-events%>%
  filter(id=="d0046789-8ab6-4191-9c4d-9e172c6b1acb")
P3WAL_TUR_l<- ffdpass %>%
  filter(id=="64ffe8dd-655b-4030-9b63-1ae3b897efcd") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
#Connor
Roberts_pass_tur<-events%>%
  filter(id=="64ffe8dd-655b-4030-9b63-1ae3b897efcd")
P4WAL_TUR_l<- ffdpass %>%
  filter(id=="085cbc78-9eb2-4b05-bab8-3f71cf44f18b") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Ramsey_pass_tur2<-events%>%
  filter(id=="085cbc78-9eb2-4b05-bab8-3f71cf44f18b")
S1WAL_TUR_l<- ffdshot %>%
  filter(id=="25b14ee0-052e-45ce-afe5-d883e9ccc29f") %>%
  mutate(Player_Type_Key = case_when(actor=="TRUE" & teammate=="TRUE" ~ "Actor",
                                     teammate=="TRUE" ~ "Teammate",
                                     teammate=="FALSE" & keeper=="FALSE" ~ "Opponent",
                                     keeper=="TRUE" & teammate=="FALSE" ~ "Goalkeeper"))
Bale_shot_tur<-events%>%
  filter(id=="25b14ee0-052e-45ce-afe5-d883e9ccc29f")
#Aaron Ramsey>Joe Allen(clearance)Kaan Ayhan(tur)
#Joe Rodon>Connor Roberts>Aaron Ramsey>Bale(shot)
#min 52:17, goal kick
```

## ggplot

I put all of the code for each corner into ggplot to look at the events
in the sequences as they happened. Below are two examples from two
different sequences.

``` r
library(grid)
ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = P1ESP_SLO_l, aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 6, alpha = 0.8, shape=21) +
  geom_segment(data = Pedri_pass_slol, aes(x = location.x, y = location.y,
                                       xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.9, colour = "#000000", arrow = 
                 arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_text(size=14,family="Source Sans Pro"),
        legend.text=element_text(size=14,family="Source Sans Pro"),
        legend.margin = margin(c(20, 10, -65, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 24, family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  labs(title = "Pedri Pass, Corner Sequence", subtitle = "Spain vs Slovakia, UEFA EURO 2020") +
  coord_flip(xlim = c(85, 125))
```

![](HackathonMarkdown_files/figure-gfm/Pedri%20Example-1.png)<!-- -->

``` r
library(SBpitch)
create_Pitch() +
  geom_point(data = P3ITA_ESP_l, aes(x = ff_location.x, y = ff_location.y, fill=Player_Type_Key),
             size = 6, alpha = 0.8, shape=21)+
  geom_segment(data = Filho_pass_esp, aes(x = location.x, y = location.y,
                                             xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.9, colour = "#000000", arrow = 
                 arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "bottom",
        legend.title=element_text(size=14,family="Source Sans Pro"),
        legend.text=element_text(size=14,family="Source Sans Pro"),
        legend.margin = margin(c(20, 10, -65, 50)),
        legend.key.size = unit(0.8, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 24, family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Source Sans Pro")) +
  labs(title = "Jorge Filho Pass, Corner Sequence", subtitle = "Italy vs Spain, UEFA EURO 2020") +
  scale_y_reverse() + #5
  coord_fixed(ratio = 105/100)
```

![](HackathonMarkdown_files/figure-gfm/Filho%20Example-1.png)<!-- -->

## Background Data

I made sure to get some background data such as which team had the most
opportunities and which teams had the highest average xG from these
opportunities.

``` r
table(DelayedC$Team)
```

    ## 
    ##         Belgium  Czech Republic         Denmark         England          France 
    ##               1               2               2               6               1 
    ##         Germany           Italy     Netherlands North Macedonia        Portugal 
    ##               1               4               1               2               1 
    ##        Scotland        Slovakia           Spain     Switzerland          Turkey 
    ##               1               2               4               2               3 
    ##           Wales 
    ##               1

``` r
Team<-c("Denmark","Spain","Italy","England","Czech Republic")
xG<-c(0.41,0.12,0.10,0.09,0.06)
Plot<-data.frame(Team,xG)

ggplot(data = Plot, aes(x=Team,y=xG))+
  geom_bar(stat = "identity")+
  theme_SB()+
  labs(title = "Average xG From Delayed Opportunities on Corners",
       subtitle = "2020 UEFA Men's EURO",
       x="",
       y="Average xG",
       caption = "Delayed defined as a shot taken 10 seconds or later after a corner
       Minimum 2 delayed opportunities
       Data from Statsbomb free data")
```

![](HackathonMarkdown_files/figure-gfm/Background%20Data-1.png)<!-- -->
