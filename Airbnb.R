library(mice)
trncsv = read.csv('analysisData.csv',header = T)
prdcsv = read.csv('scoringData.csv',header = T)

trnset = trncsv[,c(58,20,24,26,30,33,34,38,40,42,46,47,48,49,50,
                   51,52,53,54,55,61,62,63,64,65,66,75,76,77,78,80,
                   81,84,85,86,87,88,89,90,99,100,101,103)]
prdset = prdcsv[,c(20,24,26,30,33,34,38,40,42,46,47,48,49,50,
                   51,52,53,54,55,60,61,62,63,64,65,74,75,76,77,79,
                   80,83,84,85,86,87,88,89,98,99,100,102)]
#host_since
today = Sys.Date()
gtd = as.Date(trnset[,'host_since'])
diff = as.numeric(today - gtd)
trnset$host_since = diff
#host_response_rate
gtd = as.character(trnset[,'host_response_rate'])
for (i in 1:length(gtd)){
  if (gtd[i] == 'N/A'){
    gtd[i] = '0'
  }
}
for (i in 1:length(gtd)){
  if (gtd[i] != '0'){
    gtd[i] = as.numeric(sub("%","",gtd[i]))/100
  }
}
trnset$host_response_rate = as.numeric(gtd)
#host_is_superhost 
gtd = as.character(trnset[,'host_is_superhost'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
trnset$host_is_superhost = as.numeric(gtd)
#host_has_profile_pic
gtd = as.character(trnset[,'host_has_profile_pic'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
trnset$host_has_profile_pic = as.numeric(gtd)
#host_identity_verified
gtd = as.character(trnset[,'host_identity_verified'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
trnset$host_identity_verified = as.numeric(gtd)
#neighbourhood_group_cleansed
gtd = as.character(trnset[,'neighbourhood_group_cleansed'])
for (i in 1:length(gtd)){
  switch (gtd[i],
    'Bronx' = {gtd[i] = 0},
    'Brooklyn' = {gtd[i] = 1},
    'Manhattan' = {gtd[i] = 2},
    'Queens' = {gtd[i] = 3},
    'Staten Island' = {gtd[i] = 4}
  )
}
trnset$neighbourhood_group_cleansed = as.numeric(gtd)
#state
gtd = as.character(trnset[,'state'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'NY' = {gtd[i] = 0},
          'Ny' = {gtd[i] = 0},
          ' ' = {gtd[i] = 0},
          'CA' = {gtd[i] = 1},
          'MP' = {gtd[i] = 2},
          'NJ' = {gtd[i] = 3},
          'US' = {gtd[i] = 4}
  )
}
trnset$state = as.numeric(gtd)
#market
gtd = as.character(trnset[,'market'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'New York' = {gtd[i] = 0},
          ' ' = {gtd[i] = 0},
          'Adirondacks' = {gtd[i] = 1},
          'Atlanta' = {gtd[i] = 2},
          'Agra' = {gtd[i] = 3},
          'Cuba' = {gtd[i] = 4},
          'Lagos, NG' = {gtd[i] = 5},
          'D.C.' = {gtd[i] = 6},
          'Jamaica South Coast' = {gtd[i] = 7},
          'San Francisco' = {gtd[i] = 8},
          'Other (Domestic)' = {gtd[i] = 9}
  )
}
trnset$market = as.numeric(gtd)
#is_location_exact
gtd = as.character(trnset[,'is_location_exact'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
trnset$is_location_exact = gtd
#property_type
gtd = as.character(trnset[,'property_type'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'Apartment' = {gtd[i] = 0},
          'House' = {gtd[i] = 1},
          'Condominium' = {gtd[i] = 2},
          'Townhouse' = {gtd[i] = 3},
          'Serviced apartment' = {gtd[i] = 4},
          'Loft' = {gtd[i] = 5},
          'Hotel' = {gtd[i] = 6},
          'Other' = {gtd[i] = 7},
          'Guest suite' = {gtd[i] = 8},
          'Bungalow' = {gtd[i] = 9},
          'Boutique hotel' = {gtd[i] = 10},
          'Hostel' = {gtd[i] = 11},
          'Bed and breakfast' = {gtd[i] = 12},
          'Guesthouse' = {gtd[i] = 13},
          'Boat' = {gtd[i] = 14},
          'Tiny house' = {gtd[i] = 15},
          'Earth house' = {gtd[i] = 16},
          'Nature lodge' = {gtd[i] = 17},
          'Resort' = {gtd[i] = 18},
          'Cottage' = {gtd[i] = 19},
          'Tent' = {gtd[i] = 20},
          'Villa' = {gtd[i] = 21},
          'Camper/RV' = {gtd[i] = 22},
          'Houseboat' = {gtd[i] = 23},
          'Aparthotel' = {gtd[i] = 24},
          'Cabin' = {gtd[i] = 25},
          'Cave' = {gtd[i] = 26},
          'Casa particular (Cuba)' = {gtd[i] = 27},
          'Farm stay' = {gtd[i] = 28}
  )
}
trnset$property_type = as.numeric(gtd)
#room_type
gtd = as.character(trnset[,'room_type'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'Entire home/apt' = {gtd[i] = 0},
          'Private room' = {gtd[i] = 1},
          'Shared room' = {gtd[i] = 2}
  )
}
trnset$room_type = as.numeric(gtd)
#bed_type
gtd = as.character(trnset[,'bed_type'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'Real Bed' = {gtd[i] = 0},
          'Airbed' = {gtd[i] = 1},
          'Futon' = {gtd[i] = 2},
          'Pull-out Sofa' = {gtd[i] = 3},
          'Couch' = {gtd[i] = 4}
  )
}
trnset$bed_type = as.numeric(gtd)
#security_deposit
gtd = as.character(trnset[,'security_deposit'])
for (i in 1:length(gtd)){
  if (is.na(gtd[i])) {
    gtd[i] = 0
  }
}
trnset$security_deposit = as.numeric(gtd)
#cleaning_fee
gtd = as.character(trnset[,'cleaning_fee'])
for (i in 1:length(gtd)){
  if (is.na(gtd[i])) {
    gtd[i] = 0
  }
}
trnset$cleaning_fee = as.numeric(gtd)



#host_since
today = Sys.Date()
gtd = as.Date(prdset[,'host_since'])
diff = as.numeric(today - gtd)
prdset$host_since = diff
#host_response_rate
gtd = as.character(prdset[,'host_response_rate'])
for (i in 1:length(gtd)){
  if (gtd[i] == 'N/A'){
    gtd[i] = '0'
  }
}
for (i in 1:length(gtd)){
  if (gtd[i] != '0'){
    gtd[i] = as.numeric(sub("%","",gtd[i]))/100
  }
}
prdset$host_response_rate = as.numeric(gtd)
#host_is_superhost 
gtd = as.character(prdset[,'host_is_superhost'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
prdset$host_is_superhost = as.numeric(gtd)
#host_has_profile_pic
gtd = as.character(prdset[,'host_has_profile_pic'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
prdset$host_has_profile_pic = as.numeric(gtd)
#host_identity_verified
gtd = as.character(prdset[,'host_identity_verified'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
prdset$host_identity_verified = as.numeric(gtd)
#neighbourhood_group_cleansed
gtd = as.character(prdset[,'neighbourhood_group_cleansed'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'Bronx' = {gtd[i] = 0},
          'Brooklyn' = {gtd[i] = 1},
          'Manhattan' = {gtd[i] = 2},
          'Queens' = {gtd[i] = 3},
          'Staten Island' = {gtd[i] = 4}
  )
}
prdset$neighbourhood_group_cleansed = as.numeric(gtd)
#state
gtd = as.character(prdset[,'state'])
for (i in 1:length(gtd)){
  gtd[i] = 0
}
prdset$state = as.numeric(gtd)
#market
gtd = as.character(prdset[,'market'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'New York' = {gtd[i] = 0},
          ' ' = {gtd[i] = 0},
          'Adirondacks' = {gtd[i] = 1},
          'Atlanta' = {gtd[i] = 2},
          'Agra' = {gtd[i] = 3},
          'Cuba' = {gtd[i] = 4},
          'Lagos, NG' = {gtd[i] = 5},
          'D.C.' = {gtd[i] = 6},
          'Jamaica South Coast' = {gtd[i] = 7},
          'San Francisco' = {gtd[i] = 8},
          'Other (Domestic)' = {gtd[i] = 9},
          'Paris' = {gtd[i] = 0},
          'Catskills and Hudson Valley' = {gtd[i] = 1},
          'South Bay, CA' = {gtd[i] = 8},
          {gtd[i] = 0}
  )
}
prdset$market = as.numeric(gtd)
#is_location_exact
gtd = as.character(prdset[,'is_location_exact'])
for (i in 1:length(gtd)){
  if (gtd[i] == 't'){
    gtd[i] = 1
  }else if(gtd[i] == 'f'){
    gtd[i] = 0
  }
}
prdset$is_location_exact = gtd
#property_type
gtd = as.character(prdset[,'property_type'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'Apartment' = {gtd[i] = 0},
          'House' = {gtd[i] = 1},
          'Condominium' = {gtd[i] = 2},
          'Townhouse' = {gtd[i] = 3},
          'Serviced apartment' = {gtd[i] = 4},
          'Loft' = {gtd[i] = 5},
          'Hotel' = {gtd[i] = 6},
          'Other' = {gtd[i] = 7},
          'Guest suite' = {gtd[i] = 8},
          'Bungalow' = {gtd[i] = 9},
          'Boutique hotel' = {gtd[i] = 10},
          'Hostel' = {gtd[i] = 11},
          'Bed and breakfast' = {gtd[i] = 12},
          'Guesthouse' = {gtd[i] = 13},
          'Boat' = {gtd[i] = 14},
          'Tiny house' = {gtd[i] = 15},
          'Earth house' = {gtd[i] = 16},
          'Nature lodge' = {gtd[i] = 17},
          'Resort' = {gtd[i] = 18},
          'Cottage' = {gtd[i] = 19},
          'Tent' = {gtd[i] = 20},
          'Villa' = {gtd[i] = 21},
          'Camper/RV' = {gtd[i] = 22},
          'Houseboat' = {gtd[i] = 23},
          'Aparthotel' = {gtd[i] = 24},
          'Cabin' = {gtd[i] = 25},
          'Cave' = {gtd[i] = 26},
          'Casa particular (Cuba)' = {gtd[i] = 27},
          'Farm stay' = {gtd[i] = 28},
          {gtd[i] = 0}
  )
}
prdset$property_type = as.numeric(gtd)
#room_type
gtd = as.character(prdset[,'room_type'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'Entire home/apt' = {gtd[i] = 0},
          'Private room' = {gtd[i] = 1},
          'Shared room' = {gtd[i] = 2},
          {gtd[i] = 1}
  )
}
prdset$room_type = as.numeric(gtd)
#bed_type
gtd = as.character(prdset[,'bed_type'])
for (i in 1:length(gtd)){
  switch (gtd[i],
          'Real Bed' = {gtd[i] = 0},
          'Airbed' = {gtd[i] = 1},
          'Futon' = {gtd[i] = 2},
          'Pull-out Sofa' = {gtd[i] = 3},
          'Couch' = {gtd[i] = 4}
  )
}
prdset$bed_type = as.numeric(gtd)
#security_deposit
gtd = as.character(prdset[,'security_deposit'])
for (i in 1:length(gtd)){
  if (is.na(gtd[i])) {
    gtd[i] = 0
  }
}
prdset$security_deposit = as.numeric(gtd)
#cleaning_fee
gtd = as.character(prdset[,'cleaning_fee'])
for (i in 1:length(gtd)){
  if (is.na(gtd[i])) {
    gtd[i] = 0
  }
}
prdset$cleaning_fee = as.numeric(gtd)

#simple liner regression
colnames(trnset)
lm1 = lm(data = trnset,price~host_since+host_response_rate+host_is_superhost+                          
   host_listings_count+host_has_profile_pic+
   host_identity_verified+neighbourhood_group_cleansed+state+market+                                   
   is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
   bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
   extra_people+minimum_nights+maximum_nights+availability_30+availability_60+                             
   availability_90+availability_365+number_of_reviews+number_of_reviews_ltm+                      
   review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
   review_scores_checkin+review_scores_communication+review_scores_location+                    
   review_scores_value+calculated_host_listings_count+calculated_host_listings_count_entire_homes+ 
   calculated_host_listings_count_private_rooms+reviews_per_month)
summary(lm1)
pred1 = predict(lm1,prdset[,-c(10,11)],interval = "prediction",level = 0.95)
result = data.frame(id = prdcsv[,'id'],price = pred1[,1])
write.csv(result,file = 'result1.csv')

#simple liner regression
colnames(trnset)
lm2 = lm(data = trnset,price~host_response_rate+host_is_superhost+                          
           host_listings_count+host_has_profile_pic+
           neighbourhood_group_cleansed+market+                                   
           is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
           bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
           extra_people+maximum_nights+availability_30+number_of_reviews_ltm+                      
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
           review_scores_checkin+review_scores_communication+review_scores_location+                    
           review_scores_value+reviews_per_month)
summary(lm2)
pred2 = predict(lm2,prdset[,-c(10,11)],interval = "prediction",level = 0.95)
for (i in 1:length(pred2[,1])){
  if (is.na(pred2[i,1])){
    pred2[i,1] = 100
  }
}
result = data.frame(id = prdcsv[,'id'],price = pred2[,1])
write.csv(result,file = 'result2.csv')

#intersection 
colnames(trnset)
lm3 = lm(data = trnset,price~host_response_rate+host_is_superhost+                          
           host_listings_count+host_has_profile_pic+
           neighbourhood_group_cleansed+market+                                   
           is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
           bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
           extra_people+maximum_nights+availability_30+number_of_reviews_ltm+                      
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
           review_scores_checkin+review_scores_communication+review_scores_location+                    
           review_scores_value+reviews_per_month+
           host_is_superhost*host_since+host_is_superhost*review_scores_rating+
           host_is_superhost*review_scores_accuracy+host_is_superhost*review_scores_cleanliness+
           host_is_superhost*review_scores_checkin+host_is_superhost*review_scores_communication+
           host_listings_count*host_since+host_listings_count*review_scores_rating+
           host_listings_count*review_scores_accuracy+host_listings_count*review_scores_cleanliness+
           host_listings_count*review_scores_checkin+host_listings_count*review_scores_communication
           )
summary(lm3)
pred3 = predict(lm3,prdset[,-c(10,11)],interval = "prediction",level = 0.95)
for (i in 1:length(pred3[,1])){
  if (is.na(pred3[i,1])){
    pred3[i,1] = 100
  }
}
result = data.frame(id = prdcsv[,'id'],price = pred3[,1])
write.csv(result,file = 'result3.csv')

#step and diagnosis
Reg_Diag=function(fm){
  n=nrow(fm$model);df=fm$df.residual
  p=n-df-1;t=c()
  res=residuals(fm);t1=t;t1=which(abs(res)==max(abs(res)))
  sta=rstandard(fm);t2=t;t2=which(abs(sta)>3.5)
  stu=rstudent(fm);t3=t;t3=which(abs(stu)>3.5)
  h=hatvalues(fm);t4=t;t4=which(h>2*(p+1)/n)
  d=dffits(fm);t5=t;t5=which(abs(d)>2*sqrt((p+1)/n))
  c=cooks.distance(fm);t6=t;t6=which(c==max(c))
  co=covratio(fm);abs_co=abs(co-1)
  t7=t;t7=which(abs_co==max(abs_co))
  result=list(res=t1,sta=t2,stu=t3,h=t4,d=t5,c=t6,absco=t7)
  opar=par(mfrow=c(2,2),oma=c(0,0,1.1,0),mar=c(4.1,4.1,2.1,1.1))
  plot(fm,1);plot(fm,2);plot(fm,3);plot(fm,4)
  par(opar)
  return(result)
}
resultstep=Reg_Diag(lm3)
deletedata=c(resultstep$res,resultstep$sta,resultstep$stu,
             resultstep$c,resultstep$absco)
datacorrect=trnset[-deletedata,]
lm4 = lm(data = datacorrect,price~host_response_rate+host_is_superhost+                          
           host_listings_count+host_has_profile_pic+
           neighbourhood_group_cleansed+market+                                   
           is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
           bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
           extra_people+maximum_nights+availability_30+number_of_reviews_ltm+                      
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
           review_scores_checkin+review_scores_communication+review_scores_location+                    
           review_scores_value+reviews_per_month+
           host_is_superhost*host_since+host_is_superhost*review_scores_rating+
           host_is_superhost*review_scores_accuracy+host_is_superhost*review_scores_cleanliness+
           host_is_superhost*review_scores_checkin+host_is_superhost*review_scores_communication+
           host_listings_count*host_since+host_listings_count*review_scores_rating+
           host_listings_count*review_scores_accuracy+host_listings_count*review_scores_cleanliness+
           host_listings_count*review_scores_checkin+host_listings_count*review_scores_communication)
summary(lm4)
lmstep = step(lm4)
pred4 = predict(lmstep,prdset[,-c(10,11)],interval = "prediction",level = 0.95)
for (i in 1:length(pred4[,1])){
  if (is.na(pred4[i,1])){
    pred4[i,1] = 100
  }
}
result = data.frame(id = prdcsv[,'id'],price = pred4[,1])
write.csv(result,file = 'result4.csv')

#step log diagnosis
lm5 = lm(data = trnset,log(price)~host_since+host_response_rate+host_is_superhost+                          
           host_listings_count+host_has_profile_pic+
           host_identity_verified+neighbourhood_group_cleansed+state+market+                                   
           is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
           bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
           extra_people+minimum_nights+maximum_nights+availability_30+availability_60+                             
           availability_90+availability_365+number_of_reviews+number_of_reviews_ltm+                      
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
           review_scores_checkin+review_scores_communication+review_scores_location+                    
           review_scores_value+calculated_host_listings_count+calculated_host_listings_count_entire_homes+ 
           calculated_host_listings_count_private_rooms+reviews_per_month)
lm6 = lm(data = trnset,log(price)~host_response_rate+host_is_superhost+                          
           host_listings_count+host_has_profile_pic+
           neighbourhood_group_cleansed+market+                                   
           is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
           bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
           extra_people+maximum_nights+availability_30+number_of_reviews_ltm+                      
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
           review_scores_checkin+review_scores_communication+review_scores_location+                    
           review_scores_value+reviews_per_month)
lm7 = lm(data = trnset,log(price)~host_response_rate+host_is_superhost+                          
           host_listings_count+host_has_profile_pic+
           neighbourhood_group_cleansed+market+                                   
           is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
           bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
           extra_people+maximum_nights+availability_30+number_of_reviews_ltm+                      
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
           review_scores_checkin+review_scores_communication+review_scores_location+                    
           review_scores_value+reviews_per_month+
           host_is_superhost*host_since+host_is_superhost*review_scores_rating+
           host_is_superhost*review_scores_accuracy+host_is_superhost*review_scores_cleanliness+
           host_is_superhost*review_scores_checkin+host_is_superhost*review_scores_communication+
           host_listings_count*host_since+host_listings_count*review_scores_rating+
           host_listings_count*review_scores_accuracy+host_listings_count*review_scores_cleanliness+
           host_listings_count*review_scores_checkin+host_listings_count*review_scores_communication
)
lm8 = lm(data = datacorrect,log(price)~host_response_rate+host_is_superhost+                          
           host_listings_count+host_has_profile_pic+
           neighbourhood_group_cleansed+market+                                   
           is_location_exact+property_type+room_type+accommodates+bathrooms+                                   
           bedrooms+beds+bed_type+security_deposit+cleaning_fee+guests_included+                             
           extra_people+maximum_nights+availability_30+number_of_reviews_ltm+                      
           review_scores_rating+review_scores_accuracy+review_scores_cleanliness+                   
           review_scores_checkin+review_scores_communication+review_scores_location+                    
           review_scores_value+reviews_per_month+
           host_is_superhost*host_since+host_is_superhost*review_scores_rating+
           host_is_superhost*review_scores_accuracy+host_is_superhost*review_scores_cleanliness+
           host_is_superhost*review_scores_checkin+host_is_superhost*review_scores_communication+
           host_listings_count*host_since+host_listings_count*review_scores_rating+
           host_listings_count*review_scores_accuracy+host_listings_count*review_scores_cleanliness+
           host_listings_count*review_scores_checkin+host_listings_count*review_scores_communication)



