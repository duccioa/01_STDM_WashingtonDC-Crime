library(reshape2)
source('STDM_functions.R')
accu_csv = read.csv('./data/AccuWeather_2R.csv', header = F, stringsAsFactors = FALSE)
accu = get.AccuData(accu_csv)


act_temp = accu[,1:4]
act_temp = act_temp[!is.na(act_temp[,2]), ]
act_temp = melt(act_temp, id = 'Date')
ggplot(act_temp, aes(x = Date, y = value, group = variable, colour = variable)) + 
    geom_line(size = .15) + ylab('Degrees Celsius') + 
    ggtitle('Actual temperatures, Washington DC')
ggsave('./Figures/actual_temp.png', width = 5, height = 3, units = 'cm')

norm_temp = accu[,c(1,5,6,7)]
norm_temp = norm_temp[!is.na(norm_temp[,2]), ]
norm_temp = melt(norm_temp, id = 'Date')
ggplot(norm_temp, aes(x = Date, y = value, group = variable, colour = variable)) + 
    geom_line(size = .15) + ylab('Degrees Celsius') + 
    ggtitle('Normalized temperatures, Washington DC')
ggsave('./Figures/norm_temp.png', width = 5, height = 3, units = 'cm')

precip = accu[,c(1,13,14)]
precip = melt(precip, id = 'Date')
ggplot(precip,(aes(x = Date, y = value, group = variable, colour = variable))) + 
    geom_line()
ggsave('./Figures/precip.png', width = 5, height = 3, units = 'cm')


