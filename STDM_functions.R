###
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
# Prepare dataset
prepare.dataset = function(df){
    relevant_columns = c(1,2,3,7,8,13,20,22)
    names(relevant_columns) = c('X','Y', 'OBJECT', 'OFFENSE', 'METHOD', 
                                'WARD', 'CENSUS_TRACT', 'START_DATE')
    df = df[,relevant_columns]
    df$START_DATE = rm.last_ch(df$START_DATE, 5)
    df$START_DATE = gsub('T', ' ',df$START_DATE)
    df$START_DATE = ymd_hms(df$START_DATE)
    df$DAY = weekdays(df$START_DATE)
    df$YEAR = year(df$START_DATE)
    df$WEEK = week(df$START_DATE)
    df$MONTH = month(df$START_DATE)
    df$WDAY = ifelse(wday(df$START_DATE) >= 1 & wday(df$START_DATE) < 6, 'WD', 'WE')
    df$YDAY = yday(df$START_DATE)
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
rename_acs = function(vect_names, table_name){
    from = 5
    names = vect_names[from:length(vect_names)]
    names = names[1:(length(names)/2)]
    suff = rm.first_ch(names, 5)
    var_names = paste(table_name, 'VALUE', suff, sep = '_')
    var_names = c(vect_names[1:4], var_names, paste(table_name, 'PCT', suff, sep = '_'))
    return(var_names)
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



