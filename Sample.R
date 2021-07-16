library(readxl)
library(dplyr)
library(reshape)
library(ggplot2)
library(gganimate)
library(transformr)
file_path <- "D:/Projects/IOWA/UPDATED\ -\ 2021\ Iowa\ BACC\ -\ Data\ Set.xlsx"
excel_sheets(file_path)

# loading population data
un.population <- read_excel(file_path, sheet="UN Population")
ihme.population <- read_excel(file_path, sheet="IHME Population")
head(un.population)
head(ihme.population)

un.pop.data <- un.population %>% 
  filter(age_group %in% c("1 to 4", "<1 year") ) %>%  
  group_by(iso_code, year) %>% 
  summarise(totPop = sum(population, na.rm = TRUE))

head(un.pop.data)
summary(un.pop.data)

ihme.pop.data <- ihme.population %>% 
  filter(age_group_name %in% c("1 to 4", "Early Neonatal", "Late Neonatal", "Post Neonatal") ) %>%  
  group_by(iso_code, year) %>% 
  summarise(totPop = sum(reference, na.rm = TRUE))

head(ihme.pop.data)
summary(ihme.pop.data)


# loading coverage data
un.coverage <- read_excel(file_path, sheet = "WHO Vaccine Coverage")
ihme.coverage <- read_excel(file_path, sheet = "IHME Vaccine Coverage")
head(un.coverage)
head(ihme.coverage)

un.cov.pcv <- filter(un.coverage, un.coverage$vaccine =="PCV3")
ihme.cov.pcv <- filter(ihme.coverage, ihme.coverage$Vaccine == "pcv")
head(un.cov.pcv)
head(ihme.cov.pcv)
summary(un.cov.pcv)
summary(ihme.cov.pcv)

un.cov.dtp3 <- filter(un.coverage, un.coverage$vaccine =="DTP3")
ihme.cov.dtp3 <- filter(ihme.coverage, ihme.coverage$Vaccine == "dtp3")

summary(un.cov.dtp3)
summary(ihme.cov.dtp3)

# understanding relation trends between coverage and population
########### Test work start
a = un.cov.pcv[un.cov.pcv$coverage == "null",]
a = a[1:1000,]
a_ = un.cov.pcv[un.cov.pcv$coverage != "null",]
a_ = a_[1:5,]

a = rbind(a, a_)

b = un.cov.dtp3 %>% filter(iso_code %in% a$iso_code & year %in% a$year)
str(b)

a <- a %>%
  mutate(sCove = case_when( coverage == 'null' ~ b[b$iso_code == iso_code & b$year == year,]$coverage,
                            TRUE ~ coverage))

a$sCove <- as.numeric(a$sCove)
str(a)

########### Test work end

c = un.cov.dtp3 %>% filter(iso_code %in% un.cov.pcv$iso_code & year %in% un.cov.pcv$year)

un.cov.pcv <- un.cov.pcv %>%
  rowwise() %>%
  mutate(sCove = case_when( coverage == 'null' ~ c[c$iso_code == iso_code & c$year == year,]$coverage,
                            TRUE ~ coverage))

un.cov.pcv$sCove <- as.numeric(un.cov.pcv$sCove)

summary(un.cov.pcv)

head(un.cov.pcv)

un.pop.data1 <- un.pop.data %>% filter(year < 2019)
un.cov.pcv1 <- un.cov.pcv %>% filter(year < 2019)
un.cov.pcv2 <- un.pop.data1 %>%
              inner_join(un.cov.pcv1, by = c("iso_code" = "iso_code", "year" = "year"))

un.pcv.plot <- ggplot(un.cov.pcv2, aes(x=year, y=sCove, group=iso_code, fill = as.factor(iso_code), colour=as.factor(iso_code))) +
  geom_line(aes(color=iso_code)) + theme(legend.position ="none") + 
  labs(title = "{closest_state} PCV coverage by Year") +
  transition_states(iso_code, transition_length = 4, state_length = 1) 


un.pcv.plot

animate(un.pcv.plot, fps = 5, duration = 100)
