library(readr)
library(broom)
library(dplyr)
library(stringr)
library(ggplot2)
library(cluster)

setwd("/Users/Sangeetha/Downloads/")

customer_count_by_state_and_date <- read_csv("customer_count_by_state_and_date.csv")

#group_concat wil dyplr  

grouped_by_state <- customer_count_by_state_and_date %>%  group_by(location_region) %>%  summarise(fields = toString(cnt_by_state))

new_df <- data.frame(state = grouped_by_state$location_region)

#split in columns
for(i in 1:nrow(grouped_by_state)){
  
  vector_fields <- grouped_by_state[i, "fields"] %>% unlist()
  vector_fields <- vector_fields[["fields"]] %>% c()
  
  counts <- str_split(vector_fields, ", ")[[1]] %>% as.integer()
  
  for(j in 1:length(counts)){
    
    new_df[i, paste0("M", j)] <- counts[j]
    
  }
  
}

df_dis <- dist(new_df) 
matrix_dis <- as.matrix(df_dis )


mds_object <- cmdscale(df_dis)
x_coords <- mds_object[, 1]
y_coords <- mds_object[, 2] # reflect so North is at the top

plot_df <- data.frame(x = x_coords, y= y_coords, state_names = new_df$state)


plot_mds <- ggplot( data = plot_df, aes(x= x, y=y)) + geom_point() + geom_text(aes(label = state_names), check_overlap = T)
print(plot_mds)


#********************************************************************
#K-means
stringless<-subset(new_df, select = -c(state))

nb_clusters <- 25
k1 <- kmeans(new_df[, -1], centers = nb_clusters)
cluster_df <- augment(k1, new_df )
grouped_states <- cluster_df %>%  group_by(.cluster) %>%  summarise(same_cluster = toString(state))
write_csv(grouped_states, paste0("grouped_states_", nb_clusters, ".csv"))


#--------------------------------------------------------------------
cluster_iterations <- data.frame(k=5:50) %>% group_by(k) %>% do( k_cluster = kmeans(new_df[,-1], .$k) )
clusterings <- cluster_iterations %>% group_by( k ) %>% do(glance( .$k_cluster[[1]] )) %>% ungroup()

wsse_plot <- ggplot(lol, aes(k,tot.withinss )) + geom_line() + stat_smooth(method="loess")

print(wsse_plot)
#-------------------------------------------------------------------------------------
#this is wrong:
set.seed(50)
k.max<-50
data<-cluster_df
wsse<-sapply(2:k.max,function(k){kmeans(data, k, nstart = 50, iter.max = 15)$tot.withins})

wsse

plot(2:k.max, wsse, type = "b", pch=19, frame=FALSE, xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares")



