#' Function for ESM 262 assignment 4
#' @title Fish function 
#' @description 
#' @param 
#' @param 
#' @param 
#' @return most frequently caught fish ineach location, total revenue for each lcoation, total fisheries revenue sum
#' 


fish_type <- c("cod", "salmon", "tuna",
               "trout", "halibut", 
               "macherel", "sardines")

fish_price <- c(10, 15, 12, 8, 17, 7, 3)

site_A_count = c(100, 130, 200, 56, 89, 240, 310)
site_B_count = c(45, 211, 330, 43, 91, 252, 349)
site_C_count = c(80, 120, 253, 21, 87, 235, 413)
site_D_count = c(143, 90, 146, 60, 101, 210, 291)


fish_cost_df <- data.frame(fish_type, fish_price)

nfish_df <- data.frame(fish_type, site_A_count,
                       site_B_count, site_C_count,
                       site_D_count)
nfish_df


library(reshape2)
library(tidyverse)


fish_frequ <- data.frame(Site, fish_type, count)   


# colnames(revenue_by_fish) <- c("fish", "Site", "Count")

###
# fish_cost_df

# nfish_df

fish_summary <- function(fish_count_df, fish_cost_df, graph = F){
  
# if(fish_count_df[,1] != fish_cost_df[,1]){
# 
#   warning("fish types are not equal")
# 
# }
# ## Verify that fish_type column for fish_count_df and fish_cost_df are same
# 
# if (fish_count_df[,1] == fish_cost_df[,1]){
# 
# sort(as.character(fish_count_df[,1]), decreasing = F)
# sort(as.character(fish_cost_df[,1]), decreasing = F)
# 
# }

### separate by location
  site_A_frequ <- fish_count_df %>%
    select(fish_type, site_A_count) %>% 
    filter(site_A_count == max(site_A_count))
  site_B_frequ <- fish_count_df %>%
    select(fish_type, site_B_count) %>% 
    filter(site_B_count == max(site_B_count))
  site_C_frequ <- fish_count_df %>%
    select(fish_type, site_C_count) %>% 
    filter(site_C_count == max(site_C_count))
  site_D_frequ <- fish_count_df %>%
    select(fish_type, site_D_count) %>% 
    filter(site_D_count == max(site_D_count))
  
max_frequency<-list(site_A_frequ, site_B_frequ, site_C_frequ, site_D_frequ)
  
### total revenue for each location

revenue_by_fish = data.frame(fish = fish_count_df$fish_type,
                    A = fish_count_df[,2] * fish_cost_df[,2],
                    B = fish_count_df[,3] * fish_cost_df[,2], 
                    C = fish_count_df[,4] * fish_cost_df[,2],
                    D = fish_count_df[,5] * fish_cost_df[,2]
                    )

revenue_by_fish = melt(revenue_by_fish, id = "fish")

colnames(revenue_by_fish) <- c("fish", "Site", "Count")

revenue <- revenue_by_fish %>% 
  group_by(Site) %>% 
  summarise(total_revenue = sum(Count))

### Total fish revenue sum 

total_fisheries_revenue = sum(revenue$total_revenue)

### Graph

revenue_graph <- as.data.frame(revenue)

final_graph <- ggplot(revenue_graph, aes(x=Site, y=total_revenue))+
  geom_bar(stat = "identity", col = "blue", fill= "lightblue")+
  theme_classic()+
  ylab("Total Revenue")+
  xlab("Site")


if(graph == T){

  return(list(max_frequency, revenue, total_fisheries_revenue, final_graph))

   }
 else {
 
   return(list(max_frequency, revenue, total_fisheries_revenue))
 
   }

}

fish_summary(fish_count_df = nfish_df, fish_cost_df = fish_cost_df, graph = T)
fish_summary(fish_count_df = nfish_df, fish_cost_df = fish_cost_df, graph = F)

