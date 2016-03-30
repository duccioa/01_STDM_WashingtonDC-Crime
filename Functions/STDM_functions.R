##### Plot a colour map ##########################################################
### Show all the colour schemes available


plot_spdf_byClassInt <- function(spdf, #SpatialPolygonDataFrame
                                 plot_variable, #Pick the variable to be plotted
                                 n_breaks = 5, #Number of breaks
                                 col_pal = "Reds", #Colour palette
                                 border_col = "black",
                                 manual_int = F,
                                 style_intervals = "jenks",
                                 legend_title = '', legend_size = 0.7,
                                 main_title = '',
                                 subtitle = 'Washington DC'){ #Pick the style for classIntervals
    #display.brewer.all()
    require(sp)
    require(rgeos)
    require(maptools)
    require(RColorBrewer)
    require(classInt)
    col_palette <- brewer.pal(n_breaks, col_pal)
    if(!manual_int){breaks <- classIntervals(spdf@data[,plot_variable], n = n_breaks, style = style_intervals)
    breaks <- breaks$brks}
    else{breaks <- style_intervals}
    plot(spdf,
         col=col_palette[findInterval(spdf@data[,plot_variable],
                                      breaks, 
                                      all.inside = TRUE)], axes=F, lwd = 0.001, border = border_col,
         main = main_title,
         sub = subtitle)
    legend('bottomright', legend=leglabs(round(breaks, 2)), 
           fill=col_palette, bty="n", cex = legend_size, title = legend_title)
}




# Plot Frequency of offences
# dat should have columns 'Offense' and 'Count'
plot.offenses = function(dat, year){
g = ggplot(dat, aes(x = reorder(Offense, Count), y = Count, fill = Offense)) + geom_histogram(stat = 'identity')
g = g + coord_flip() + xlab('') + ggtitle(paste0('Frequency of offenses, Washington DC ', year))
g = g + theme_bw() + theme(axis.ticks = element_blank(),
                           panel.grid.major.y = element_blank(),
                           panel.grid.minor.y = element_blank(), 
                           rect = element_blank(), 
                           axis.line.x = element_line(size=2))
return(g)
}
# Remove the last n characters from a string
rm.last_ch = function(x, n){
    substr(x, 1, nchar(x) - n)
}
# Remove the first n characters from a string
rm.first_ch = function(x, n){
    substr(x, n+1, nchar(x))
}
# Calculate the mode of a vector
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Prepare dataset
prepare.dataset = function(df, y2011 = F, y){
    require(lubridate)
    relevant_columns = c('OFFENSE', 'START_DATE', 'CENSUS_TRACT')
    df = subset(df, select = relevant_columns)
    if(y2011){df$START_DATE = mdy_hms(df$START_DATE)}
    else{
        df$START_DATE = rm.last_ch(df$START_DATE, 5)
        df$START_DATE = gsub('T', ' ',df$START_DATE)
        df$START_DATE = ymd_hms(df$START_DATE)
    }
    df$DAY = as.factor(weekdays(df$START_DATE))
    df$YEAR = as.factor(y)
    df$WEEK = as.factor(week(df$START_DATE))
    df$MONTH = as.factor(month(df$START_DATE))
    df$WDAY = as.factor(ifelse(wday(df$START_DATE) >= 1 & wday(df$START_DATE) < 6, 'WD', 'WE'))
    df$YDAY = yday(df$START_DATE)
    df$OFFENSE = as.factor(df$OFFENSE)
    df = df[!is.na(df$START_DATE),]
    return(df)
}
# Load ACS dataset
load_ACS = function(table, year){
    root = './data/ACS/'
    root_name = paste0('ACS_', year, '_5YR_', table)
    file_name = paste0(root_name, '.csv')
    full_path = paste0(root, root_name, '/',file_name)
    df = read.csv(full_path, skip=2, header = FALSE, stringsAsFactors = FALSE)
    nam = read.csv(full_path, nrows = 1)
    nam = names(nam)
    names(df) = nam
    df$YEAR = paste0(20,year)
    return(df)
}
# Rename variables in the ACS tables
rename_ACS = function(df, table_name){
    n = ncol(df)
    fixed_names = names(df)[1:4]
    var_nam = names(df)[5:n]
    var_nam[grepl('HC01', var_nam)] = paste(table_name,
                                            'VALUE', rm.first_ch(var_nam[grepl('HC01', var_nam)], 5), 
                                            sep = '_')
    var_nam[grepl('HC03', var_nam)] = paste(table_name,
                                            'PCT', rm.first_ch(var_nam[grepl('HC03', var_nam)], 5), 
                                            sep = '_')
    names(df) = c(fixed_names, var_nam)
    return(df)
    
}
# Plor ACF
ggplot.acf = function(acf1, main_title){
    require(ggplot2)
    mu = mean(acf1$acf)
    conf_int = acf_confInt(acf1)
    acf_df = with(acf1, data.frame(lag, acf), data.frame(x = lag, y = acf))
    g = ggplot(data = acf_df, mapping = aes(x = lag, y = acf)) +
        geom_hline(aes(yintercept = 0), size = .1) +
        geom_hline(aes_q(yintercept = -conf_int), linetype = 'dashed', colour = 'blue',size = .08) +
        geom_hline(aes_q(yintercept = conf_int), linetype = 'dashed', colour = 'blue',size = .08) +
        geom_segment(mapping = aes(xend = lag, yend = 0), size = 0.1, colour = 'black')
    g = g + ggtitle(main_title)
    g = g + theme_bw() + theme(axis.ticks = element_blank(),
                               panel.grid.major.y = element_blank(),
                               panel.grid.minor.y = element_blank(), 
                               rect = element_blank(), 
                               #axis.line.x = element_line(size=5), 
                               legend.position = 'none')
    return(g)
}
# Save plot
save.2figures = function(pl, fn, w = 10, h = 5, s = 3, leg = TRUE, leg_size = .2, t_size = 3){
    require(ggplot2)
    pl = pl + theme(text = element_text(size=t_size)) + 
        theme(panel.grid.major.x = element_line(size = .1),
              panel.grid.minor.x = element_line(size = .1))
    if(!leg){pl = pl +theme(legend.position="none")}
    else{pl = pl + theme(legend.key.size = grid::unit(leg_size, "cm"))}
    ggsave(path = './Figures/', plot = pl, filename = fn, width = w, height = h, units = 'cm')
}
###################### WEATHER DATA ######################
# Takes a single column from the AccuWeather_2R.csv

get.AccuMonth = function(month_data){
    month_data = as.numeric(month_data)
    # Build empty dataframe with column names
    col_names = c('Date', 'Act.High', 'Act.Low', 'Act.Avg', 
                  'Norm.High', 'Norm.Low', 'Norm.Avg', 'Norm.Dept',
                  'Rec.High', 'Rec.Year', 'Rec.Low', 'Rec.Year', 
                  'Precip.Amt', 'Snow.Amt', 'Snow.Ground',
                  'Heat.Deg.Day', 'Cool.Deg.Day')
    df_month = data.frame(matrix(1:length(col_names), nrow = 1))[-1,]
    names(df_month) = col_names
    
    i=3 # start from line 3 - line 1 and 2 being year and month
    while(i<=length(month_data)-17){
        j = nrow(df_month) + 1 # Get the last line of the dataframe and add 1
        # Build the data string
        m = formatC(month_data[2], 
                    width = 2, format = "d", flag = "0")
        d = formatC(month_data[i], 
                    width = 2, format = "d", flag = "0")
        date = paste0(month_data[1], m, d)
        # build one ine of observation
        obs = as.numeric(c(date,month_data[i+1:(i+13)]))
        df_month[j,] = obs # append the line
        i = i + 17 # Each 17 rows is an observation, so step 17 by 17
    }
    return(df_month)
}

get.AccuData = function(accu){
    require(lubridate)
    num_months = ncol(accu) # number of months in the data
    df = data.frame()
    for(c in 1:num_months){
        cn = accu[,c]
        df = rbind(df, get.AccuMonth(cn))
    }
    df[,1] = ymd(df[,1])
    return(df)
}

# Convert column to factor
# df a dataframe
# indexes a numeric indexes of the columns to be converterd to factors
col_2factors = function(df, indexes){
    for(i in indexes){
        df[,i] = as.factor(df[,i])
        }
    return(df)
}



