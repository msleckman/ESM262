#' Function for ESM 262 assignment 4
#' @title Fish function 
#' @description calculated the most frequent fish caught, the total revenue from fish catch per location (with or without graph),    
#' @param fish_count_df a dataframe with count of different fish type at each location.
#' @param fish_cost_df a dataframe with cost of unit fish by type 
#' @param graph if True, the function outputs a graphical representation of the total fisheries revenue for each site based on type and number of fish caught. Default is false.
#' @return Function returns 1.most frequently caught fish ineach location, 2.Total revenue for each lcoation, 3. total fisheries revenue sum for all locations 4. If graph = T, a the final graph as explained above
#' @author Margaux Sleckman 

fish_summary <- function(fish_count_df, fish_cost_df, graph = F){

## Error checking 1: Verify that fish_type column for fish_count_df and fish_cost_df are same and sort
  
  for(i in nrow(fish_count_df)){
    
    if (fish_count_df[,1][i] == fish_cost_df[,1][i]){
      
      sort(as.character(fish_count_df[,1]), decreasing = F)
      sort(as.character(fish_cost_df[,1]), decreasing = F)
      
    }
  }  
  
  
##Error Checking 2: if no fish price for specific fish type, show warning
    
for(i in nrow(fish_count_df)){
  
if(fish_count_df$fish_type[i] != fish_cost_df$fish_type[i]){

  stop("Missing price for 1 ore more of recorded fish types")

}

}
  

### 1. Max Frequency 
## Separate by location
  site_A_frequ <- fish_count_df %>%
    select(fish_type, site_A_count) %>% 
    filter(site_A_count == max(site_A_count)) %>% 
    melt(id.vars = "fish_type")
  site_B_frequ <- fish_count_df %>%
    select(fish_type, site_B_count) %>% 
    filter(site_B_count == max(site_B_count)) %>% 
    melt(id.vars = "fish_type")
  site_C_frequ <- fish_count_df %>%
    select(fish_type, site_C_count) %>% 
    filter(site_C_count == max(site_C_count)) %>% 
    melt(id.vars = "fish_type")
  site_D_frequ <- fish_count_df %>%
    select(fish_type, site_D_count) %>% 
    filter(site_D_count == max(site_D_count)) %>% 
    melt(id.vars = "fish_type")
 
## get df of highest caught fish     
max_frequency <- rbind(site_A_frequ, site_B_frequ, site_C_frequ, site_D_frequ) 
colnames(max_frequency) <- c("fish_type", "site_id", "frequency")

### 2. Total revenue for each location

## df revenue by fish
revenue_by_fish = data.frame(fish = fish_count_df$fish_type,
                    A = fish_count_df[,2] * fish_cost_df[,2],
                    B = fish_count_df[,3] * fish_cost_df[,2], 
                    C = fish_count_df[,4] * fish_cost_df[,2],
                    D = fish_count_df[,5] * fish_cost_df[,2]
                    )
## transpose
revenue_by_fish = melt(revenue_by_fish, id = "fish")

colnames(revenue_by_fish) <- c("fish", "Site", "Count")

## get sum of revenue per location
revenue <- revenue_by_fish %>% 
  group_by(Site) %>% 
  summarise(total_revenue = sum(Count))

### 3. Total fish revenue sum 

total_fisheries_revenue = sum(revenue$total_revenue)

### 4. Graph

revenue_graph <- as.data.frame(revenue)

final_graph <- ggplot(revenue_graph, aes(x=Site, y=total_revenue))+
  geom_bar(stat = "identity", col = "blue", fill= "lightblue")+
  annotate(geom="label", x=3.5, y=12000, label= paste0("Total fisheries revenue\n", "$", total_fisheries_revenue),
           color="black", fill = "grey")+
  theme_classic()+
  ylab("Total Revenue ($)")+
  xlab("Site")+
  labs(title = "Total Revenue by Fishing Location\n")

### Returns 

## return with graph
if(graph == T){

  return(list(`Most frequently_caught fish per site` = max_frequency, `Total Fisheries Revenue per Site` = as.data.frame(revenue), paste0("Total Fisheries Revenue = ", total_fisheries_revenue), `Graph Revenue per site` = final_graph))

}

## return without graph
 else {
 
   return(list(`Most frequently_caught fish per site` = max_frequency, `Total Fisheries Revenue per Site` = as.data.frame(revenue), paste0("Total Fisheries Revenue = ", total_fisheries_revenue)))
   
 
   }

}


