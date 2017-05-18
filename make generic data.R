library(tidyverse)

#Data from https://www.kaggle.com/mrisdal/fake-news
df <- read_csv("data/fake.csv") %>%
  filter(language == "english") %>%
  filter(country %in%  c("GB", "EU", "US")) %>%
  filter(site_url %in% c("newsbiscuit.com", "thespoof.com", "wnd.com", "thedailymash.co.uk", "thetruthseeker.co.uk", 
                         "blackagendareport.com", "thepoke.co.uk",  "yournewswire.com ", "humansarefree.com", 
                         "investmentwatchblog.com", "madworldnews.com", "liberalamerica.org", "russia-insider.,com",
                         "theonion.com", "truthdig.com", "unz.com", "galacticconnection.com", "guardianlv.com", 
                         "beforeitisnews.com", "ihavethetruth.com", "beforeitsnews.com", "politicususa.com",
                         "westernjournalism.com")) %>%
  mutate(uuid = seq_along(uuid)) %>%
  select(uuid, site_url, country, type, text) %>%
  drop_na

write.csv(df, "data/generic-data.csv", row.names = F)

df %>%
  mutate(country = factor(country),
         type = factor(type),
         site_url = factor(site_url)) %>%
  summary
