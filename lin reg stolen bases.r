
install.packages("Lahman") #install package

install.packages("data.table") #install package

library("Lahman") #loads package 

library("data.table") #loads package

teams = as.data.table(Teams) #Use data.table to build new data set with teamID, Rank, W, etc. 
#also create WinPercentage formula

teams = teams[, .(yearID, 
                  lgID = as.character(lgID), 
                  teamID = as.character(teamID), 
                  franchID = as.character(franchID),
                  Rank, G, W, L, R, ERA, SO, 
                  WinPercent = W/(W+L))]

salaries = as.data.table(Salaries) #create salaries data set 

salaries = salaries[, c("lgID", "teamID", "salary1M") := 
                      list(as.character(lgID), as.character(teamID), salary / 1e6L)]

payroll = salaries[, .(payroll = sum(salary1M)), by=.(teamID, yearID)]

teamPayroll = merge(teams, payroll, by=c("teamID","yearID")) #merge teams and payroll data sets together

head(teamPayroll) #all payroll data

teamPayrollP <- teamPayroll[yearID >= "2000"] #removes data with any year earlier than 2000

head(teamPayrollP)

install.packages("ggplot2")

library("ggplot2") #load ggplot2 package

ggplot(teamPayrollP, #linear regression graph of payroll and winpercentage
       aes(x= payroll, y = WinPercent)) +geom_point() + stat_smooth(method = "lm", col = "red")

install.packages('ggplot2',repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/')
library('ggplot2')

install.packages("dplyr") #install dplyr package
library('dplyr')

teams1 = teams[, .(yearID, 
                  lgID = as.character(lgID), 
                  teamID = as.character(teamID), 
                  W, L, R, RA, 
                  WPCT= R^1.83/(R^1.83+RA^1.83))] #winning percentage; ratio of runs to the total number of runs; runs+runs allowed

exwin=round(teams1$WPCT*(teams1$W+teams$L)) #exoected number of wins formula

diff= teams1$W-exwin  #difference between Wins and expected number of wins

teams2 = teams[, .(yearID, 
                  lgID = as.character(lgID), 
                  teamID = as.character(teamID), 
                  W, L, R, RA, 
                  WPCT= R^1.83/(R^1.83+RA^1.83), exwin, diff)]

head(teams2)

modelexwin= ggplot(teams2, aes(x= exwin, y = W))+geom_point() 
            + stat_smooth(method = "lm", col = "blue") #plot of predicted expected wins in relations to previous wins
                                                        #utilizing runs / runs+allowed runs, which is a ratio based 
                                                        #on the pythagorean theory of chance

modelexwin

modelew= lm(W ~ exwin, data=teams2)

summary(modelew) #strong model that predicts wins as R-square is .9488 

ggplot(batting, #linear regression graph of batting and winpercentage
       aes(x= batting, y = WinPercent)) +geom_point() + stat_smooth(method = "lm", col = "red")




















