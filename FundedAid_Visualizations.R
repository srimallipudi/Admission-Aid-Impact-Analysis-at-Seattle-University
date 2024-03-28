#Remove objects (data) from your workspace
rm(list=ls(all=TRUE))
library(tidyverse)
library(patchwork)
library(RColorBrewer)

#Set working directory by clicking on Session --> Set Working Directory --> To Source File Location
data <- read.csv("UnfundedAiddata_Final.csv", header=TRUE)  # import data
dat <- read.csv("dataQ4.csv", header=TRUE)  # import data

# Question 1: Does unfunded aid make a difference in whether an admitted students accept their offer of admission (confirm/deposit)?

# Calculate the percentage of students who accepted with unfunded aid
with_unfunded_aid <- sum(data$Registered.in.Colleague == 1 & data$Aid == 1)
total_with_unfunded_aid <- sum(data$Aid == 1)
percentage_with_unfunded_aid <- (with_unfunded_aid / total_with_unfunded_aid) * 100

# Calculate the percentage of students who accepted without unfunded aid
without_unfunded_aid <- sum(data$Registered.in.Colleague == 1 & data$Aid == 0)
total_without_unfunded_aid <- sum(data$Aid == 0)
percentage_without_unfunded_aid <- (without_unfunded_aid / total_without_unfunded_aid) * 100

# Create a data frame for the percentages
admission_percentages <- data.frame(
  Unfunded_Aid = c("Without Unfunded Aid", "With Unfunded Aid"),
  Percentage = c(percentage_without_unfunded_aid, percentage_with_unfunded_aid)
)

p <- ggplot(admission_percentages, aes(x = Unfunded_Aid, y = Percentage, fill = Unfunded_Aid)) + geom_col(width = 0.5, alpha = 0.85) +
  labs(title = 'Admitted Students Acceptance by Unfunded Aid', x = 'Students Registered in Colleague', y = 'Percentage of Admitted Students') +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")), hjust = .25, vjust = -.30, family = 'serif', size = 14/.pt, fontface =2) +
  geom_label(data = data.frame(x = 1.81202625734275, y = 91.0819848874438, label = "Acceptance Rates:                                                                                        \nAdmits with Unfunded Aid Offer: 50.81% Students accepted the offer     \nAdmits without Unfunded Aid Offer: 67.24% Students accepted the offer"),
             mapping = aes(x = x, y = y, label = label),
             label.padding = unit(0.5, "lines"),
             label.size = 0.8, label.r = unit(0.5, "lines"),
             inherit.aes = FALSE, family = 'serif', size = 12/.pt)+
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_discrete(limits = c('Without Unfunded Aid', 'With Unfunded Aid')) +
  scale_fill_manual(values = c('Without Unfunded Aid' = 'red', 'With Unfunded Aid' = 'blue')) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif', face = 'bold'),
        axis.text.y = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 16, face = 'bold', margin = margin(0,0,0.5,0,'cm')))

ggannotate::ggannotate(p)

# Question 2: Of those who accepted their offer of admission (confirm/deposit), in what order (most to least) are levels of aid offered (G levels)?

p2_colors = c('#DEEBF7', '#DEEBF7', '#DEEBF7', '#DEEBF7', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6', '#2171B5', '#08519C', '#08306B')

p2 <- data %>%
  group_by(Tier, Registered.in.Colleague) %>%
  summarize(admitted = n()) %>%
  mutate(admitted_percentage = (admitted / sum(admitted)) * 100) %>%
  pivot_wider(names_from = Registered.in.Colleague, values_from = c('admitted', 'admitted_percentage')) %>%
  ggplot(aes(x = admitted_percentage_1, y = reorder(Tier, admitted_percentage_1), fill = reorder(Tier, admitted_percentage_1))) + geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(admitted_percentage_1, 2), "%")), hjust = -.25, vjust = .20, family = 'serif', size = 12/.pt, fontface =2) +
  labs(title = 'Admitted Students Acceptance Rate by Aid Level', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_x_continuous(limits = c(0, 80)) +
  scale_fill_manual(values = p2_colors) +
  theme_minimal() +
  guides(fill = 'none') +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif'),
        axis.text.y = element_text(size = 12,family='serif', face = 'bold'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 18, face = 'bold', margin = margin(0,0,0.5,0,'cm')))

# Question 3: Who (demographic data) is most likely to be influenced by the offer of unfunded aid?

sex <- data %>%
  filter(Aid == 1) %>%
  group_by(Sex, Registered.in.Colleague) %>%
  summarize(admitted = n()) %>%
  mutate(admitted_percentage = (admitted / sum(admitted)) * 100) %>%
  pivot_wider(names_from = Registered.in.Colleague, values_from = c('admitted', 'admitted_percentage'))

citizenship <- data %>%
  filter(Aid == 1) %>%
  group_by(Citizenship.Status, Registered.in.Colleague) %>%
  summarize(admitted = n()) %>%
  mutate(admitted_percentage = (admitted / sum(admitted)) * 100) %>%
  pivot_wider(names_from = Registered.in.Colleague, values_from = c('admitted', 'admitted_percentage'))

race <- data %>%
  filter(Aid == 1) %>%
  group_by(Race, Registered.in.Colleague) %>%
  summarize(admitted = n()) %>%
  mutate(admitted_percentage = (admitted / sum(admitted)) * 100) %>%
  pivot_wider(names_from = Registered.in.Colleague, values_from = c('admitted', 'admitted_percentage'))


p1 <- ggplot(sex, aes(x = admitted_percentage_1, y = reorder(Sex, admitted_percentage_1), fill = Sex)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_text(aes(label = paste0(round(admitted_percentage_1, 2), "%")), hjust = -.10, vjust = -.05, family = 'serif', size = 11/.pt, fontface =2) +
  labs(title = 'Unfunded Aid Student Acceptance Rate by Demographic Data', x = 'Registered in Colleague (%)', y = 'Sex') +
  scale_fill_manual(values = c('M'='#9ECAE1', 'F'='#08519C')) +
  scale_x_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_discrete(labels = c('M' = 'Male', 'F' = 'Female')) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif'),
        axis.text.y = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        axis.title.x = element_blank(),
        plot.title = element_text(family = 'serif', size = 16, face = 'bold', margin = margin(0,0,0.5,0,'cm')))

  
p2 <- ggplot(citizenship, aes(x = admitted_percentage_1, y = Citizenship.Status, fill = Citizenship.Status)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_text(aes(label = paste0(round(admitted_percentage_1, 2), "%")), hjust = -.10, vjust = -.05, family = 'serif', size = 11/.pt, fontface =2) +
  labs(x = 'Registered in Colleague (%)', y = 'Citizenship Status') +
  scale_fill_manual(values = c('US'='#08519C', 'FN'='#9ECAE1')) +
  scale_x_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_discrete(labels = c('US' = 'United States Citizen', 'FN' = 'Foreign National')) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif'),
        axis.text.y = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        axis.title.x = element_blank())


p3 <- ggplot(race, aes(x = admitted_percentage_1, y = Race, fill = reorder(Race, admitted_percentage_1))) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(admitted_percentage_1, 2), "%")), hjust = -.10, vjust = .30, family = 'serif', size = 11/.pt, fontface =2) +
  labs(x = 'Registered in Colleague (%)', y = 'Race') +
  scale_x_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_discrete(limits = c('Black or African American', 'Asian', 'American Indian or Alaska Native', 'White', 'Native Hawaiian or Other Pacific Islander')) +
  scale_fill_manual(values = c('Native Hawaiian or Other Pacific Islander'='#08519C', 'White'='#2171B5', 'American Indian or Alaska Native' = '#6BAED6', 'Asian' = '#9ECAE1', 'Black or African American' = '#C6DBEF')) + 
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif'),
        axis.text.y = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'))

p1 / p2 / p3

# Question 4: By Program- who is the “best” at disseminating unfunded aid (meaning of those who have the most applicants accept their offer (confirm/deposit) how do they typically divide the levels of aid (G levels)?

# Create a custom color palette with one less color for the lowest value
colors <- c('#DEEBF7','#DEEBF7', '#DEEBF7', '#C6DBEF', '#9ECAE1', '#6BAED6', '#4292C6', '#2171B5', '#08519C', '#08306B')

top_admits <- data %>%
  group_by(Application.Program) %>%
  summarize(admitted = n()) %>%
  arrange(-admitted) %>%
  head(10) 

top_admits <-  head(unique(top_admits$Application.Program), 10)
filtered_data <- data[data$Application.Program %in% top_admits, ]

filtered_data %>%
  group_by(Application.Program, Registered.in.Colleague) %>%
  summarize(admitted = n()) %>%
  mutate(admitted_percentage = (admitted / sum(admitted)) * 100) %>%
  pivot_wider(names_from = Registered.in.Colleague, values_from = c('admitted', 'admitted_percentage')) %>%
  arrange(-admitted_percentage_1) %>%
  ggplot(aes(x = admitted_percentage_1, y = reorder(Application.Program, admitted_percentage_1), fill = reorder(Application.Program, admitted_percentage_1))) + geom_col() +
  geom_text(aes(label = paste0(round(admitted_percentage_1, 2), "%")), hjust = -.10, vjust = .30, family = 'serif', size = 12/.pt, fontface =2) +
  labs(title = 'Acceptance Rates of Top 10 Programs with Highest Admit counts', x = 'Registered in Colleague (%)', y = 'Program') +
  scale_x_continuous(limits = c(0,100)) +
  guides(fill = 'none') +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif'),
        axis.text.y = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 16, face = 'bold', margin = margin(0,0,0.5,0,'cm')))

A1 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Business Administration (Professional) - MBA') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity") +
  geom_text(aes(label = count), hjust = -.20, vjust = .20, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Business Administration (Professional) - MBA', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_fill_brewer(palette = 'Blues') +
  scale_x_continuous(limits = c(0, 137)) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))


A2 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Finance - MSF') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity") +
  geom_text(aes(label = count), hjust = -.20, vjust = .20, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Finance - MSF', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_fill_brewer(palette = 'Blues') +
  # scale_fill_manual(values = c('G0 : 0'='#08519C', 'G2 : 2400'='#9ECAE1', 'G1 : 1200'='#C6DBEF')) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))


A3 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Business Analytics - MSBA') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity") +
  geom_text(aes(label = count), hjust = -.20, vjust = .20, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Business Analytics - MSBA', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  # scale_x_continuous(limits = c(0, 260)) +
  # scale_fill_manual(values = c('G0 : 0'='#08519C')) +
  scale_fill_manual(values = colors) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))

A4 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Teacher Preparation - MIT') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity") +
  geom_text(aes(label = count), hjust = -.20, vjust = -.05, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Teacher Preparation - MIT', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_fill_brewer(palette = 'Blues') +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))

(A1 | A2) / (A3 | A4)

A5 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Counseling,  School Counseling specialization - MAED') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity") +
  geom_text(aes(label = count), hjust = -.20, vjust = -.05, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Counseling,  School Counseling specialization - MAED', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_fill_brewer(palette = 'Blues') +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))


A6 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Computer Science Fundamentals Certificate') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity", width = 0.8) +
  geom_text(aes(label = count), hjust = -.20, vjust = .20, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Computer Science Fundamentals Certificate', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_fill_manual(values = c('G0 : 0'='#08519C', 'G2 : 2400'='#9ECAE1', 'G1 : 1200'='#C6DBEF')) +
  scale_x_continuous(limits = c(0, 113)) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))

A7 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Business Analytics - MSBA - Online Instruction') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity", width = 0.25) +
  geom_text(aes(label = count), hjust = -.20, vjust = -.05, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Business Analytics - MSBA - Online Instruction', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_fill_manual(values = c('G0 : 0'='#08519C')) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))


A8 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Computer Science - MSCS') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = count), hjust = -.20, vjust = .20, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Computer Science - MSCS', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_x_continuous(limits = c(0, 110)) +
  scale_fill_manual(values = c('G0 : 0'='#08519C', 'G4 : 4800'='#9ECAE1')) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))

A9 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Business Administration (Professional) - MBA - Online Instruction') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity",width = 0.25) +
  geom_text(aes(label = count), hjust = -.20, vjust = -.05, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Business Administration (Professional) - MBA - Online Instruction', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_fill_manual(values = c('G0 : 0'='#08519C')) +
  scale_x_continuous(limits = c(0, 256)) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))


A10 <- data %>%
  filter(Registered.in.Colleague == 1 & Application.Program == 'Data Science - MSDS') %>%
  group_by(Tier) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = count, y = reorder(Tier, count), fill = reorder(Tier, count))) + geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = count), hjust = -.20, vjust = .20, family = 'serif', size = 10/.pt, fontface =2) +
  labs(title = 'Data Science - MSDS', x = 'Students Registered in Colleague', y = 'Scholarship Tier') +
  scale_x_continuous(limits = c(0, 42)) +
  scale_fill_manual(values = c('G2 : 2400'='#08519C', 'G0 : 0'='#9ECAE1')) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,family='serif'),
        axis.title = element_text(face = 'bold', size = 12,family = 'serif'),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, family = 'serif', size = 12, face = 'bold'))

(A5 | A6) / (A7 | A8) / (A9 | A10)


# Question 5: By Program- how much time, on average does it take from the time an application is submitted until a decision is released?

dat$Decision.Release.time <- as.integer(dat$Decision.Release.time)

dat %>%
  group_by(Application.Program) %>%
  summarize(count = n(),
            avg_time = mean(Decision.Release.time)) %>%
  arrange(-count) %>%
  head(10) %>%
  ggplot(aes(x = avg_time, y = reorder(Application.Program, avg_time), fill = reorder(Application.Program, avg_time))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(avg_time, 2)), hjust = -.25, vjust = .25, family = 'serif', size = 12/.pt, fontface =2) +
  labs(y = "Program", x = "Decision Release Time (in Months)",
       title = "Average Decision Release Time by Program", subtitle = "Top 10 Programs with Highest Applications") +
  scale_x_continuous(limits = c(0,6)) +
  scale_fill_manual(values = colors) +
  guides(fill = 'none') +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
        axis.text.x = element_text(size = 12,family='serif'),
        axis.text.y = element_text(size = 12,family='serif'),
        axis.title = element_text(face = 'bold', size = 14,family = 'serif'),
        plot.title = element_text(family = 'serif', size = 18, face = 'bold', margin = margin(0,0,0.5,0,'cm')),
        plot.subtitle = element_text(family = 'serif', size = 12, face = 'bold', margin = margin(0,0,0.5,0,'cm')))





  
  
  
  

