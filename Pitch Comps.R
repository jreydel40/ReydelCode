library(RODBC)
library(dplyr)
library(openxlsx)
library(stringr)
library(ggplot2)

library(tidyverse)

conn2 <- odbcDriverConnect("Driver=SQL Server;Server=OPSDBOAK85; Database=master;
                              Uid=ATHLETICS\\jsamuels; trusted_connection=yes")

lev <- 1
season <- 2024

sqlQuery(conn2, paste0("drop table if exists #temppit
select	count(*) TP, e.pitcherid Pitcher, PitHand
                            , PitchType
                            , COUNT(pitchtype) as count
                            , avg(EffectiveVelocity) as avgEffVelo
                            , stdev(EffectiveVelocity) as stdEffVelo
                            , min(EffectiveVelocity) as minEffVelo
                            , max(EffectiveVelocity) as maxEffVelo
                            , avg(spinrate) as avgSpin
                            , min(spinrate) as minSpin
                            , max(spinrate) as maxSpin
                            , avg(SpinAxis) as avgAxis
                            , min(SpinAxis) as minAxis
                            , max(SpinAxis) as maxAxis
                            , avg(VerticalBreakInduced) as avgInduVert
                            , min(VerticalBreakInduced) as minVertBreak
                            , max(VerticalBreakInduced) as maxVertBreak
                            , avg(HorizontalBreak) as avgHorzBreak
                            , min(HorizontalBreak) as minHorzBreak
                            , max(HorizontalBreak) as maxHorzBreak
                            , avg(ReleaseZ) as avgRelHeight
                            , min(ReleaseZ) as minRelHeight
                            , max(ReleaseZ) as maxRelHeight
                            , avg(ReleaseX) as avgRelSide
                            , min(ReleaseX) as minRelSide
                            , max(ReleaseX) as maxRelSide
                            , avg(SpinAxis) as avgSpinAxis
                            , min(SpinAxis) as minSpinAxis
                            , max(SpinAxis) as maxSpinAxis
                            , avg(pd.pfx_x) pfx_x
                            , avg(pd.pfx_z) pfx_z
                            , avg(Extension) as avgExt
							into #temppit
                            from mlb_stats.dbo.pbp_play_event e
                            inner join mlb_stats.dbo.pbp_play_event_pitch pd on e.GamePk=pd.GamePk AND pd.AtBatIndex=e.AtBatIndex AND e.EventIndex=pd.EventIndex
                            INNER join mlb_stats.dbo.pbp_game s
                            on s.GamePk = pd.GamePk
                            INNER join mlb_stats.dbo.mlb_player p
                            on e.Pitcherid = p.playerid
                            where year(s.gamedate) >= ", season, "
                            and (PitchType is not null 
                                 and PitchType <> 'IB'
                                 and pd.StartSpeed is not null
                                 and s.GameType = 'r'
                                 and s.LeagueLevel <= ", lev, ")
                            --and pithand = 'r'
                            and PitchType in ('FC','CU','FS','CH','SL','SI','FA','SW')
                            group by PitchType, Pitcherid, pithand
                            having count(pd.PitchType) >= 45"))

sqlQuery(conn2, paste0("drop table if exists #tempavg
select	PitchType, PitHand
                            , COUNT(pitchtype) as count
                            , avg(EffectiveVelocity) as avgEffVelo
                            , stdev(EffectiveVelocity) as stdEffVelo
                            , min(EffectiveVelocity) as minEffVelo
                            , max(EffectiveVelocity) as maxEffVelo
                            , avg(spinrate) as avgSpin
                            , min(spinrate) as minSpin
                            , max(spinrate) as maxSpin
                            , stdev(spinrate) as stdSpin
                            , avg(SpinAxis) as avgAxis
                            , min(SpinAxis) as minAxis
                            , max(SpinAxis) as maxAxis
                            , avg(VerticalBreakInduced) as avgInduVert
                            , min(VerticalBreakInduced) as minVertBreak
                            , max(VerticalBreakInduced) as maxVertBreak
                            , avg(HorizontalBreak) as avgHorzBreak
                            , min(HorizontalBreak) as minHorzBreak
                            , max(HorizontalBreak) as maxHorzBreak
                            , avg(ReleaseZ) as avgRelHeight
                            , min(ReleaseZ) as minRelHeight
                            , max(ReleaseZ) as maxRelHeight
                            , stdev(ReleaseZ) as stdRelHeight
                            , avg(ReleaseX) as avgRelSide
                            , min(ReleaseX) as minRelSide
                            , max(ReleaseX) as maxRelSide
                            , stdev(ReleaseX) as stdRelSide
                            , avg(SpinAxis) as avgSpinAxis
                            , min(SpinAxis) as minSpinAxis
                            , max(SpinAxis) as maxSpinAxis
                            , stdev(SpinAxis) as stdSpinAxis
                            , avg(pd.pfx_x) pfx_x
                            , avg(pd.pfx_z) pfx_z
							into #tempavg
                            from mlb_stats.dbo.pbp_play_event e
                            inner join mlb_stats.dbo.pbp_play_event_pitch pd on e.GamePk=pd.GamePk AND pd.AtBatIndex=e.AtBatIndex AND e.EventIndex=pd.EventIndex
                            INNER join mlb_stats.dbo.pbp_game s
                            on s.GamePk = pd.GamePk
                            INNER join mlb_stats.dbo.mlb_player p
                            on e.Pitcherid = p.playerid
                            where year(s.gamedate) >= ", season, "
                            and PitchType is not null 
                            and PitchType <> 'IB'
                            and pd.StartSpeed is not null
                            and s.GameType = 'r'
                            and s.LeagueLevel <= ", lev, "
                            --and p.throws = 'r'
                            and PitchType in ('FC','CU','FS','CH','SL','SI','FA','SW')
                            group by PitchType, PitHand
                            having count(pd.PitchType) >= 45"))

league_comp <- sqlQuery(conn2, paste0("select lastname + ', '+ UseName Name, Pitcher, PitHand, TP, PitchType, 
                        sqrt(power(zEffVelo,2) + power(zRelHeight,2) + power(zSpin,2) + power(zSpinAxis,2)) sim
                        ,round(Velo, 1) velo,round(HorzBreak,1) HorzBreak,round(VertBreak,1) VertBreak, round(RelHeight,2) VertBreak, round(Ext,2) Ext
                        --round(spin,0) Spin, round(relHeight,2) RelHeight
                        from (
                          select pit.pitcher, pit.pithand, pit.TP, pit.pitchtype, (pit.avgEffVelo - average.avgEffVelo)/average.stdEffVelo zEffVelo
                          , (pit.avgRelHeight - average.avgRelHeight)/average.stdRelHeight zRelHeight
                          , (pit.avgSpin - average.avgSpin)/average.stdSpin zSpin
                          , (pit.avgSpinAxis - average.avgSpinAxis)/average.avgSpinAxis zSpinAxis, pit.avgHorzBreak HorzBreak, pit.avgInduVert VertBreak, pit.avgEffVelo Velo, pit.avgSpin Spin
                          , pit.avgRelHeight RelHeight, pit.avgExt Ext
                          
                          from #temppit pit
                          
                          INNER JOIN  #tempavg average
                          on average.PitchType = pit.PitchType and average.PitHand = pit.PitHand
                        ) comp
                        inner join mlb_stats.dbo.mlb_player pl
                        on pl.playerid = comp.Pitcher WHERE pl.Position='P'
                        order by
                        PitHand
                        --, HorzBreak
                        , CASE WHEN PitchType = 'FA' THEN 1 
                        WHEN PitchType = 'SI' THEN 2
                        WHEN PitchType = 'FC' THEN 3
                        WHEN PitchType = 'SL' THEN 4
                        WHEN PitchType = 'SW' THEN 5
                        WHEN PitchType = 'CU' THEN 6
                        WHEN PitchType = 'FS' THEN 7
                        WHEN PitchType = 'CH' THEN 8 END aSC
                        , sqrt(power(zEffVelo,2) + power(zRelHeight,2) + power(zSpin,2) + power(zSpinAxis,2))"))


pitcher <- 608718
pit_comp <- league_comp %>% filter(Pitcher == pitcher)
lg_pit <- league_comp %>% filter(PitHand == pit_comp$PitHand[1], PitchType %in% pit_comp$PitchType)
for (i in unique(pit_comp$PitchType)) {
  pit_sim <- pit_comp$sim[which(pit_comp$PitchType == i)]
  lg_pit <- lg_pit %>% filter(PitchType != i | abs(sim - pit_sim) <= .3)
}
print(pit_comp)
View(lg_pit)

