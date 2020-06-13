library(tidyverse)
library(lubridate)
library(data.table)

data_shopee1 <- read.csv("Copy of Copy of order_brush_order.csv") %>% 
  mutate(event_time=ymd_hms(event_time)) %>% 
  mutate(max_time = event_time + hours(1),
         min_time = event_time) 

data_shopee2 <- read.csv("Copy of Copy of order_brush_order.csv") %>% 
  mutate(event_time=ymd_hms(event_time)) %>% 
  mutate(max_time = event_time,
         min_time = event_time - hours(1)) 

data_shopee <- rbind(data_shopee1,data_shopee2)

overall_dt <- NULL

for (shop_id in unique(data_shopee$shopid)) {
  
  ind_shop <- filter(data_shopee, shopid==shop_id) %>% 
    dplyr::select(orderid,shopid,userid,event_time) %>% 
    unique()
  
  ind_shop0 <- filter(data_shopee, shopid==shop_id) 
  
  ind_shop2 <- ind_shop0 %>%
    mutate(fake_index=1:n()) %>% 
    group_by(fake_index) %>% 
    mutate(count_time = sum(ind_shop$event_time <= max_time & ind_shop$event_time >= min_time)) %>% 
    mutate(count_uni_user = length(unique(ind_shop$userid[which(ind_shop$event_time <= max_time & ind_shop$event_time >= min_time)]))) %>% 
    mutate(concentration = count_time / count_uni_user) %>% 
    filter(concentration >= 3) %>% 
    ungroup() %>% 
    dplyr::select(shopid,max_time,min_time) 
  
  if (nrow(ind_shop2)>0) {
    
    ind_shop2 <- ind_shop2 %>% 
      mutate(index=1:n())
  
  store_user <- ind_shop %>% 
    dplyr::select(orderid,shopid,userid,event_time) %>% 
    merge(.,ind_shop2,by=c("shopid"),all.x=TRUE) %>% 
    mutate(check_time=(event_time <= max_time & event_time >= min_time)) %>% 
    filter(check_time==TRUE) %>% 
    group_by(shopid,userid,index) %>% 
    summarise(count=n()) %>% 
    ungroup() %>% 
    group_by(index) %>% 
    filter(count==max(count)) %>% 
    ungroup() %>% 
    dplyr::select(shopid,userid) %>% 
    unique()
  
  overall_dt <- rbind(overall_dt,store_user)
  
  }
  
}



overall_dt2 <- overall_dt %>% 
  dplyr::select(shopid,userid) %>% 
  unique()

final_result <- NULL

for (find_shop in unique(overall_dt2$shopid)) {
  
  order_brush <- filter(overall_dt2,shopid == find_shop)
  
  order_brush_tb <- data.frame(shopid=find_shop,userid=paste(order_brush$userid, collapse ="&"))
  
  final_result <- rbind(final_result,order_brush_tb)
  
  
}

no_brush <- unique(data_shopee$shopid)[which(!(unique(data_shopee$shopid) %in% final_result$shopid))]

no_final_result <- data.frame(shopid=no_brush,userid="0")

overall_result <- rbind(final_result,no_final_result) %>% 
  arrange(shopid)

fwrite(overall_result,file="Jiemei submission week 1.csv")
