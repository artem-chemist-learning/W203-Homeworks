library(tidyverse)
library(fec16)

df_house <- fec16::results_house
df_camp <- fec16::campaigns


ggplot(data = df_house) + 
  aes(x = general_percent) + 
  geom_histogram(bins = 50)


filter(df_camp, df_camp$ttl_disb < 10000000)%>%
  ggplot() + 
  aes(x = ttl_disb) + 
  geom_histogram(bins = 50) +
  labs(y= "y axis name", x = "x axis name") + 
  ggtitle("Petal and sepal length \nof three species of iris")

joint_df <- inner_join(df_house, df_camp, by = 'cand_id')


party_mutate <- function(x){
  if_else(x == "D", "Dem", "Other")
}


func1 <- function(x){
  return_val <- "Other"
  if (party == "R"){
    return_val <- "Republican"
  }
  if (party == "D"){
    return_val <- "Democrat"
  }
  return(return_val)
}


mutate(gateway, YearlyHit = case_when(Frequency == 'Year' ~ 1,
                                      Frequency == 'Month' ~ 12,
                                      Frequency == 'Week' ~ 48)
)



joint_df <- mutate(joint_df, candidate_party = case_when(party == "D"~"Democrat",
                                                         party == "R"~ "Republican",
                                                         TRUE ~ "Other"))

ggplot(data = joint_df) + 
  aes(x = ttl_disb, y = general_votes, color = candidate_party) + 
  geom_point() +
  labs(y= "Genral votes", x = "Spending") + 
  ggtitle("General votes and spending")

