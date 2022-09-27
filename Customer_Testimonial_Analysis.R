# Setting up environment
install.packages("tidyverse")
install.packages("janitor")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("reactable")
library(reactable)
library(tidyverse)
library(readr)
library(janitor)
library(dplyr)
library(ggplot2)
library(ggpubr)

#Import csv file
Survey <- read_csv("Survey.csv", 
                         na = "empty", show_col_types = FALSE)
head(Survey)
#Change to data frame
Survey <- as.data.frame(Survey)
class(Survey)
#cleaning data
clean_names(Survey)
#merging columns
data <- Survey %>% 
  unite(col = 'cuisine_type', 
        c('Au','Nhat','Han','Viet','Khac'), sep ='', remove = TRUE)

#Overall summary
glimpse(data) #there is 64 customers attended to the survey

data %>% group_by(cuisine_type) %>% summarise(number_of_cust = n_distinct(SessionID))
data %>% group_by(`Store Type`) %>% summarise(number_of_cust = n_distinct(`Store ID`))

## visual
theme_set(
  theme_light() + theme(legend.position = "top")
)
# 1. Approaching Channel vs Cuisine Type
# Kênh approach khách hiệu quả nhất hiện nay là thông qua Sale trực tiếp tiếp cận.
# Bên cạnh đó thì Facebook Ads cũng mang lại hiệu qua hơn so với Zalo Ads.
data %>% ggplot(mapping = aes(x=`Approaching Channel`, fill = cuisine_type)) +
  geom_bar(stat="count") + coord_flip() + 
  scale_fill_brewer(palette="Spectral") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm'),
        plot.title = element_text(size = 10, face = "bold", color = "darkred")) + 
  ggtitle("Number of customers per Approaching Channel")
# 2. Tools & Customer Satisfaction vs Cuisine Type
# Khách hàng phần lớn là hài lòng với dịch vụ. 
# Tuy nhiên có 13 khách chưa thực sự hài lòng, thậm chí là thất vọng.
data %>% ggplot(mapping = aes(x=`Tools`,y = cuisine_type, color = cuisine_type)) +
  geom_count(show.legend = FALSE) + coord_flip() + facet_grid(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_bw() +
  theme(strip.text.y = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_text(size = 9, color = "black", face = "bold"),
        axis.title.y = element_text(size = 9, color = "black", face = "bold")) +
  labs(x = "Tools", y = "Cuisine Type")
# Price
data %>% ggplot(mapping = aes(x=`Price`,y = cuisine_type, color = cuisine_type)) +
  geom_count() + coord_flip() + facet_wrap(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_bw() + 
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, color = "darkred", face = "bold"),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) 


# Product quality
# Khách hàng dissatisfied vì họ unhappy với product quality
# Statement unhappy xuất hiện cả trong 4 nhóm khách hàng --> cần coi lại về product quality
data %>% ggplot(mapping = aes(x=`Product quality`,y = cuisine_type, color = cuisine_type)) +
  geom_count() + coord_flip() +facet_wrap(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_bw() + 
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, color = "darkred", face = "bold"),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) 

#Fulfillment rate
#Hầu hết đều happy, dù có một nhà unhappy nhưng họ vần cảm thấy hài lòng với những sản phẩm hiện có 
# --> yếu tố này không phải là concern lớn/quan trọng của họ
data %>% ggplot(mapping = aes(x=`Fulfillment rate`,y = cuisine_type, color = cuisine_type)) +
  geom_count() + coord_flip() + facet_wrap(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_bw() + 
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, color = "darkred", face = "bold"),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) 
#Product variety
#Hầu hết đều happy, dù có một nhà unhappy nhưng họ vần cảm thấy hài lòng với những sản phẩm hiện có
# --> yếu tố này không phải là concern lớn/quan trọng của họ
data %>% ggplot(mapping = aes(x=`Product variety`,y = cuisine_type, color = cuisine_type)) +
  geom_count() + coord_flip() + facet_wrap(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_bw() + 
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, color = "darkred", face = "bold"),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) 
#Shipping service
#All happy
data %>% ggplot(mapping = aes(x=`Shipping service`,y = cuisine_type, color = cuisine_type)) +
  geom_count() + coord_flip() + facet_wrap(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_bw() + 
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, color = "darkred", face = "bold"),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) 

#Customer service
#All happy
data %>% ggplot(mapping = aes(x=`Customer service`,y = cuisine_type, color = cuisine_type)) +
  geom_count() + coord_flip() + facet_wrap(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_bw() + 
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9, color = "darkred", face = "bold"),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) 

#Second time order 
data %>% ggplot(mapping = aes(x=`Will you order second time?`, fill = cuisine_type)) +
  geom_bar() + facet_grid(vars(`Customer satisfaction`), scales = 'free') +
  scale_fill_brewer(palette="Spectral") + theme_light() + 
  theme(strip.text.y = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) + ggtitle('Will you order second time?') 
data_fb_no <- data %>% filter(`Will you order second time?` == 'No',
                              `Customer satisfaction` %in% c('Satisfied','Neutral')) %>%
  select(`Store ID`, `Store Name`,`Customer satisfaction`,`Will you order second time?`,`Comments` )
data_fb_no <- arrange(data_fb_no, `Customer satisfaction`)
reactable(data_fb_no, fullWidth = TRUE, outlined = TRUE)
#conclude: common issue:  price + product quality

#Increase volume?
data_fb_yes <- data %>% filter(`Will you order second time?` == 'Yes')
data_fb_yes %>% ggplot(mapping = aes(x=`Will you increase your volume?`, fill = cuisine_type)) +
  geom_bar()  +
  scale_fill_brewer(palette="Spectral") + theme_light() + 
  theme(strip.text.x = element_text(size = 8, color = "darkblue", face = "bold"),
        axis.text.x = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20",
                                   size = 6, hjust = .5, vjust = .5, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "right", legend.title = element_text(size=8, color = "black", face = "bold"),
        legend.text = element_text(size=8, color = "grey20", face = "plain"), 
        legend.key.size = unit(.5, 'cm')) + ggtitle('Will you increase your volume?')
data_fb_yes_no <- data_fb_yes %>% filter(`Will you increase your volume?` == 'No') %>%
  select(`Store ID`, `Store Name`,`Customer satisfaction`,`Will you increase your volume?`,`Comments` )
reactable(data_fb_yes_no)



