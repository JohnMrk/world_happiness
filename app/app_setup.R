gun_deaths <- read.csv("~/Documents/Gov 1005/Ms_3/raw-data/gun_deaths2.csv")%>%
  clean_names()%>%
  rename(country_name = location)%>%
  group_by(country_name)%>%
  summarise(total_deaths = sum(val))



education <- read.csv("~/Documents/Gov 1005/Ms_3/raw-data/education2.csv")%>%
  clean_names()

cost_of_living <- read_csv("~/Documents/Gov 1005/Ms_3/raw-data/cost_of_living.csv")%>%
  clean_names()%>%
  rename(country_name = name)%>%
  select(country_name, cost_of_living_index)

happiness <- read.csv("~/Documents/Gov 1005/Ms_3/raw-data/data.csv")%>%
  clean_names()%>%
  rename(country_name = name)

gdpPerCapita <- readxl::read_xls("~/Documents/Gov 1005/Ms_3/raw-data/gdp.xls", skip = 3)%>%
  clean_names()

urbanization <- readxl::read_xls("~/Documents/Gov 1005/Ms_3/raw-data/urbanization.xls", skip =3)%>%
  clean_names()%>%
  rename(urbanization = x2018)%>%
  select(country_name, urbanization)

gdpPerCapita2 <- gdpPerCapita%>%
  select(country_name, country_code, x2018)

HFI <- read.csv("~/Documents/Gov 1005/Ms_3/raw-data/hfi_2018.csv")%>%
  clean_names()%>%
  mutate(myyear = max(year))%>%
  filter(year == myyear)%>%
  select(iso_code, region, hf_score, pf_score, ef_score)%>%
  rename(country_code = iso_code)


gini <- read.csv("~/Documents/Gov 1005/Ms_3/raw-data/gini.csv")%>%
  clean_names()

clean_gini <- gini%>%
  group_by(country_code)%>%
  mutate(myyear = max(year))%>%
  filter(year == myyear)%>%
  select(country_code, value)%>%
  rename(gini_index = value)



#chrstgenpct, judgenpct, islmgenpct, budgenpct, zorogenpct, hindgenpct, #sikhgenpct, shntgenpct, )

religion <- readxl::read_xlsx("~/Documents/Gov 1005/Ms_3/raw-data/Religious_Composition_by_Country_2010-2050.xlsx", sheet =   "rounded_percentage")%>%
  filter(Year == 2010)%>%
  select(Country, Unaffiliated)%>%
  mutate(nonreligpct = as.numeric(Unaffiliated))%>%
  mutate(religpct = 100 - nonreligpct, country_name = Country)%>%
  select(country_name, religpct, nonreligpct)



master1 <- happiness%>%
  full_join(gdpPerCapita2)


cleaned <- read_csv("~/Documents/Gov 1005/Ms_3/raw-data/troubleshoot1.csv")

cleaned1 <- cleaned%>%
  filter(is.na(x2018))%>%
  select(country_name, country_code)%>%
  left_join(gdpPerCapita2, by = "country_code")%>%
  select(country_name.x, country_code, x2018)%>%
  rename(country_name = country_name.x)

master <- cleaned%>%
  filter(!is.na(x2018))%>%
  full_join(cleaned1)%>%
  filter(!is.na(happiness_rank))

master <- master%>%
  left_join(cost_of_living, by = "country_name")

master <- master%>%
  left_join(HFI, by = "country_code")

master <- master%>%
  left_join(clean_gini, by = "country_code")

master <- master%>%
  left_join(religion, by = "country_name")

master <- master%>%
  left_join(gun_deaths, by = "country_name")

