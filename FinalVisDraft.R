setwd('/Users/ishanbiswas/Desktop/Coding/PUBPOL 457/Final Portfolio')

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

# VIS 1 #
# I am looking at the Weekday Frequency and Weeknight Freq. to show the amount of shut down routes at night
DDOT <- read.csv("DDOT_Bus_Routes.csv")


freq_gathered <- DDOT %>% select(BaseFreq1, NightFreq1)
freq_gathered <- gather(freq_gathered, key = "variable", value = "freq", BaseFreq1, NightFreq1)
freq_gathered$freq <- as.numeric(freq_gathered$freq)
freq_gathered$freq <- ifelse(freq_gathered$freq == 0, "No Buses", freq_gathered$freq)

# Side to side histogram showing the distribution of frequencies of routes at night and day on weekdays
freq_histogram <- ggplot(freq_gathered, aes(x = freq)) + geom_histogram(stat = 'count', fill = 'steelblue', color = 'blue') + facet_wrap(vars(variable), nrow = 2, ncol = 2) + theme_light() + labs(title = 'Distribution of Frequencies of Bus Routes per Hour for Weekdays and Weeknights', x = 'Frequency (per Hour)')
freq_histogram # I cannot get the fill to be by variable type, this one has me really confused. I tried to make it clearer what I'm showing by putting No buses at the end however

# VIZ 2 #
# Importing excel sheet from Census of commute characteristics in Detroit. It is a 2 way frequency(?) table, so this is a little tricky to get parsed
COMMUTE <- read_excel("/Users/ishanbiswas/Desktop/Coding/PUBPOL 457/Final Portfolio/DET_TRANS_DATA.xlsx", sheet = 'Data', skip = 3, col_names = c("Label", "Estimate (Total)", "Margin of Error (Total)", "Estimate (Drove Alone)", "Margin of Error (Drove Alone)", "Estimate (Carpooled)", "Margin of Error (Carpooled)", "Estimate (Transit)", "Margin of Error (Transit)"))

# I am now converting the percentage values into proportions for the estimate columns, which I will end up using.
COMMUTE$`Estimate (Total)` <- ifelse(grepl("%", COMMUTE$`Estimate (Total)`), as.numeric(gsub("%", "", COMMUTE$`Estimate (Total)`)) / 100, COMMUTE$`Estimate (Total)`)

COMMUTE$`Estimate (Drove Alone)` <- ifelse(grepl("%", COMMUTE$`Estimate (Drove Alone)`), as.numeric(gsub("%", "", COMMUTE$`Estimate (Drove Alone)`)) / 100, COMMUTE$`Estimate (Drove Alone)`)

COMMUTE$`Estimate (Carpooled)` <- ifelse(grepl("%", COMMUTE$`Estimate (Carpooled)`), as.numeric(gsub("%", "", COMMUTE$`Estimate (Carpooled)`)) / 100, COMMUTE$`Estimate (Carpooled)`)

COMMUTE$`Estimate (Transit)` <- ifelse(grepl("%", COMMUTE$`Estimate (Transit)`), as.numeric(gsub("%", "", COMMUTE$`Estimate (Transit)`)) / 100, COMMUTE$`Estimate (Transit)`)

# I want to look at the indicators pertaining to poverty for this graph, so I am subsetting by the rows that are related to such
poverty_subset <- COMMUTE[c(46:48), ]

# I ultimately want to visualize the proportion estimates, and retain the labels. I ommitted the carpool columns as they are just going to muddy the vis.
poverty_subset <- poverty_subset[, c('Label', 'Estimate (Total)', 'Estimate (Transit)')]
poverty_gathered <- gather(poverty_subset, key = "variables", value = "values", -Label) # I am gathering to make it easier to create a side by side bar plot
poverty_gathered$Label <- factor(poverty_gathered$Label, levels = c('Below 100 percent of the poverty level', '100 to 149 percent of the poverty level', 'At or above 150 percent of the poverty level'))

poverty_transit_bars <- ggplot(data = poverty_gathered, aes(fill = variables, y = values, x = Label)) + geom_bar(position = 'dodge', stat = 'identity') + scale_fill_manual(values = c("steelblue", "skyblue2")) + labs(title = 'Total Distribution of Workers Based on Poverty Level Compared to Commuters by Transit', x = 'Poverty Level', y = 'Proportion')
poverty_transit_bars

# VIS 3 #
# Temporal viz showing ridership over time (MONTHLY RIDERSHIP)

RIDERSHIP <- read_excel("/Users/ishanbiswas/Desktop/Coding/PUBPOL 457/Final Portfolio/DET_TRANS_DATA.xlsx", sheet = 'Monthly Ridership') #added a sheet to this workbook of the data I had to aggregate manually from Detroit Data Portal. Dates are in the perfet format thanks to Excel's automatic function
RIDERSHIP$RIDERSHIP <- replace(RIDERSHIP$RIDERSHIP, RIDERSHIP$RIDERSHIP == "N/A", NA) # Realized "N/A" is of course different than true NA- converting them
RIDERSHIP$RIDERSHIP <- ifelse(is.na(RIDERSHIP$RIDERSHIP), NA, as.numeric(RIDERSHIP$RIDERSHIP)) # Converting rest to numeric values so we can graph them

riders_line <- ggplot(data = RIDERSHIP) + geom_line(aes(x = MONTH, y = RIDERSHIP), size = 1.5, color = 'steelblue') + theme_classic() + labs(title = 'DDOT Ridership by Month, July 2018-Feb. 2023', x = 'Year', y = 'Ridership') + geom_vline(xintercept = as.numeric(RIDERSHIP$MONTH[21]), linetype = 'dashed', alpha = 0.5, color = 'steelblue') + geom_point(aes(x = MONTH, y = RIDERSHIP), size = 1.5) + annotate('text', x = RIDERSHIP$MONTH[25], y = 2130300, label = 'COVID-19 Pandemic', alpha = 0.8, size = 3, color = 'steelblue') 
riders_line


# VIS 5 #
# Side by side bar plots of mean travel times to work by car or public transit (DET_TRANS)
com_time_subset <- COMMUTE[94:102, c('Label', 'Estimate (Drove Alone)', 'Estimate (Transit)')]
com_time_subset$Label = c('0-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-44', '45-59', '60+')
com_time_subset$Label <- factor(com_time_subset$Label, levels = c('0-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-44', '45-59', '60+'))
com_time_subset$`Estimate (Drove Alone)` <- as.numeric(com_time_subset$`Estimate (Drove Alone)`)
com_time_subset$`Estimate (Transit)` <- as.numeric(com_time_subset$`Estimate (Transit)`)
com_time_gathered <- gather(com_time_subset, key = 'variable', value = 'value', -Label)

travel_bars <- ggplot(com_time_gathered, aes(x = Label, y = value, fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') + facet_wrap(~ variable, nrow = 2) + labs(title = 'Distribution of Proportions of Commute Time by Transportation Type', x = 'Minutes', y = 'Proportion', facet = c('Drive Alone', 'Transit')) + theme_classic() + theme(legend.position = 'none') + scale_fill_manual(values = c('steelblue', 'skyblue2'))
travel_bars


# Viz 4 #
# Walk scores population and transit bubble chart
# Data is copy and pasted into excel from WalkScores.com for the neighborhoods of Detroit
WALKING <- read_excel("/Users/ishanbiswas/Desktop/Coding/PUBPOL 457/Final Portfolio/DET_TRANS_DATA.xlsx", sheet = 'Walk Scores')
WALKING$`Walk_Score` <- as.numeric(WALKING$`Walk_Score`)
WALKING$`Transit_Score` <- as.numeric(WALKING$`Transit_Score`)
WALKING$Population <- as.numeric(WALKING$Population)

walk_transit_bubble <- ggplot(data = WALKING) + geom_point(aes(x = Walk_Score, y = Transit_Score, size = Population), color = "steelblue", stroke = 1) + labs(title = 'Transit Score and Walking Score of Detroit Neighborhoods by Population', x = 'Walk Score', y = 'Transit Score') + theme_minimal() + geom_vline(xintercept = 49, color = 'royalblue3', alpha = 0.5, linetype = 2) + geom_text(aes(x = 54, y = 57, label = '0-49: Car Dependent'), size = 3, color = 'royalblue3') + geom_hline(yintercept = 49, color = 'royalblue3', alpha = 0.5, linetype = 2) + geom_text(aes(x = 82, y = 49.5, label = '0-49: Minimal to No Transit'), size = 3, color = 'royalblue3', alpha = 0.6)
walk_transit_bubble


# VIS 6 #
# Income Distribution by Race
# Used Census Data on Population and Income for this Vis
INCOME <- read_excel('/Users/ishanbiswas/Desktop/Coding/PUBPOL 457/Final Portfolio/INCOME.xlsx', sheet = "Data", skip = 2, col_names = c('Label', 'Population', 'Margin of Error', 'Percent', 'Margin of Error', 'Mean Income', 'Margin of Error'))
income_subset <- INCOME[c(26, 33), c('Label', 'Population', 'Mean Income')]
income_subset$Population <- as.numeric(gsub(",", "", income_subset$Population))
income_subset$`Mean Income` <- as.numeric(gsub(",", "", income_subset$`Mean Income`))
income_subset <- gather(income_subset, key = 'variable', value = 'value', -Label)

incomes_bars <- ggplot(data = income_subset, aes(x = Label, fill = Label)) + geom_bar(aes(y = value), stat = 'identity') + facet_wrap(~ variable, scales = 'free_y') + theme(legend.position = 'none') + labs(title = 'Comparative Mean Income and Population Values by Race in Detroit', x = NULL, y = NULL) + scale_fill_manual(values = c('steelblue', 'skyblue2'))
incomes_bars




