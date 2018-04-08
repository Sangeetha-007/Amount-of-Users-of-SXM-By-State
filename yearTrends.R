library(readr)
library(ggplot2)
library(grid)
library(gridExtra)
library(plotly)

setwd("/Users/Sangeetha/Downloads/")

customer_count_by_state_and_date1 <- read_csv("visits_data.csv")
customer_count_by_state_and_date1 <- na.omit(customer_count_by_state_and_date1)

interval=310

i<-1
for(i in 310:nrow(customer_count_by_state_and_date1)){
  
  sectioned_plot <- ggplot( data = customer_count_by_state_and_date1, aes(x=date_extracted, y=cnt_by_state) ) + geom_line() + facet_grid(.~location_region )
  
  interval<-interval+1
}


print(sectioned_plot)


#********************
interval=310
list_plots <- list()
tmp_index=1
list_index <- 1

layout = matrix(c(1:9), 3)

for (i in seq(interval, nrow(customer_count_by_state_and_date1), interval)){
  tmp_plot<-ggplot(data = customer_count_by_state_and_date1[tmp_index:i, ],aes(x=date_extracted, y=cnt_by_state)) +geom_line()+ facet_grid(.~location_region)# + labs(title=location_region)
  list_plots[[list_index]]<-tmp_plot
  
  print(paste(tmp_index, i))
  
  tmp_index <- i+1
  list_index<-list_index+1
}
m <- marrangeGrob(list_plots, ncol = 3, nrow = 17, layout_matrix = layout)
ggsave("YearlyTrend.pdf", m)




#********************
#with count relaxation
count_relaxation <- 307
list_plots <- list()
tmp_index=1
list_index <- 1

layout = matrix(c(1:9), 3)

vector_states <- customer_count_by_state_and_date1$location_region %>% unique()


for(i in 1:length(vector_states)){
  
  each_state <- vector_states[i]
  state_df <- customer_count_by_state_and_date1 %>% filter( location_region ==  each_state) %>% head(n = count_relaxation)
  tmp_plot <- ggplot(data = state_df, aes(x = date_extracted, y = cnt_by_state)) + geom_line() + facet_grid(.~location_region)
  
  list_plots[[list_index]] <- tmp_plot
  list_index <- list_index+1
  
}

m <- marrangeGrob(list_plots, ncol = 3, nrow = 17, layout_matrix = layout)
print(m)
ggsave("YearlyTrend.pdf", m)


for (i in seq(interval, nrow(customer_count_by_state_and_date1), interval)){
  tmp_plot<-ggplot(data = customer_count_by_state_and_date1[tmp_index:i, ],aes(x=date_extracted, y=cnt_by_state)) +geom_line()+ facet_grid(.~location_region)# + labs(title=location_region)
  list_plots[[list_index]]<-tmp_plot
  
  print(paste(tmp_index, i))
  
  tmp_index <- i+1
  list_index<-list_index+1
}

m <- marrangeGrob(list_plots, ncol = 3, nrow = 17, layout_matrix = layout)
ggsave("YearlyTrend.pdf", m)


