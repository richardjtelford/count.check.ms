#Hershop bog, neotoma datasetID 3132

hershop_plan <- drake_plan(
  hershop_pollen = counts(pollen_data[["3132"]]) %>% 
    rownames_to_column(var = "sampleID") %>% 
    pivot_longer(-sampleID, names_to = "taxa", values_to = "count") %>%
    filter(count > 0),
    
  hershop_pollen_summ1 = hershop_pollen %>% 
    group_by(sampleID) %>% 
    summarise(
      pollen_spore_count = sum(count), 
      pollen_count = sum(count[taxa != "Unknown (trilete)"]), 
      gcd = numbers::mGCD(count)), 
  
  hershop_pollen_summ = hershop_pollen_summ1 %>%
    ungroup() %>% 
    summarise(
    n = n(),
    has_singleton = sum(gcd == 1), 
    pollen_gt_200 = mean(pollen_count >= 200),
    pollen_spore_gt_200 = mean(pollen_spore_count >= 200),
    pollen_spore_200 = mean(pollen_spore_count == 200)
    )
)#end plan

# hershop_pollen %>% distinct(taxa) %>% arrange(taxa) %>% print(n = Inf)
#  
# hershop_pollen %>% 
#   group_by(sampleID) %>% 
#   mutate(percent = count/sum(count) * 100) %>% 
#   group_by(taxa) %>% 
#   filter(n() > 1, max(count) > 4) %>% 
#   ggplot(aes(x = -as.numeric(sampleID), y = percent)) +
#   geom_area() +
#   geom_point(alpha = 0.5, colour = "red") + 
#   coord_flip() +
#   facet_wrap(~taxa, nrow = 1) +
#   theme(strip.text = element_text(angle = 90))

# taxa                         
# <chr>                        
# 1 Acer                  #paper explicitly states not present       
# 2 Alnus                   10     
# 3 Amaranthaceae         22       
# 4 Apiaceae              20       
# 5 Asteraceae            18       
# 6 Betula                2       
# 7 Carya                 5       
# 8 Celtis                8       
# 9 Cyperaceae            19       
# 10 Ericaceae            25        
# 11 Fraxinus             13        
# 12 Ilex                 15        
# 13 Juglans            7          
# 14 Juniperus           3         
# 15 Liquidambar            16      
# 16 Myrica               12        
# 17 Onagraceae             26      
# 18 Pinus                 4       
# 19 Poaceae              17        
# 20 Polygonaceae         24        
# 21 Quercus            1          
# 22 Salix                 9       
# 23 Spermatophyta undiff. (trees)
# 24 Typha                  21      
# 25 Ulmus                 6       
# 26 Unknown (trilete)  

# #missing
# #populus 11
# #Rhus 14
# #Vitis 27
# #Nyctaginaceae 23
