LRf=lm(Price ~ .-Landsize-GRaine-GMoonee-CA_Bayside-
         GLITTLE-Gnelson-GSweeney-Ghstuart-CA_Kingston-
         Gbarry-GRay-GStockdale-GNoel-GJas-GBiggin-GYPA-
         CA_PortP-CA_Whitehorse-GRendina-GFletchers-GBrad-
         GHodges-GVillage-GLove-sub_4-GGary-CA_Hume-
         CA_Boroondara-Method_SA-GWilliams-GHarcourts-
         GNick-GGreg-CA_Monash-GWoodards-CA_Stonnington-
         GCayzer-Postcode-sub_3,data=train_75)



Price ~ Rooms + Distance + Postcode + Bedroom2 + Bathroom + Car + 
  Landsize + BuildingArea + YearBuilt + Type_u + Type_h + CouncilArea_Monash + 
  CouncilArea_Whitehorse + CouncilArea_Manningham + CouncilArea_HobsonsBay + 
  CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Yarra + 
  CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_GlenEira + 
  CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Boroondara + 
  CouncilArea_ + Method_PI + Method_S + SellerG_Moonee + SellerG_Kay + 
  SellerG_Miles + SellerG_Greg + SellerG_RT + SellerG_Biggin + 
  SellerG_Ray + SellerG_Marshall + SellerG_hockingstuart + 
  SellerG_Jellis


apply(h_train,2,function(x)sum(is.na(x)))

h_train$Bedroom2 = ifelse(is.na(h_train$Bedroom2), h_train$Rooms,
                          h_train$Bedroom2)

#h_train$Bedroom2[is.na(h_train$Bedroom2)] = median(h_train$Bedroom2, na.rm = T)
h_train$Bathroom[is.na(h_train$Bathroom)]=round(mean(h_train$Bathroom,na.rm=T),0)
h_train$Car[is.na(h_train$Car)]=round(mean(h_train$Car,na.rm=T),0)
h_train$Landsize[is.na(h_train$Landsize)]=round(mean(h_train$Landsize,na.rm=T),0)
h_train$BuildingArea[is.na(h_train$BuildingArea)]=round(mean(h_train$BuildingArea,na.rm=T),0)
h_train$YearBuilt[is.na(h_train$YearBuilt)]=round(mean(h_train$YearBuilt,na.rm=T),0)


h_test$Bedroom2 = ifelse(is.na(h_test$Bedroom2), h_test$Rooms,
                         h_test$Bedroom2)
#h_test$Bedroom2[is.na(h_test$Bedroom2)]=median(h_test$Bedroom2,na.rm=T)
h_test$Bathroom[is.na(h_test$Bathroom)]=round(mean(h_test$Bathroom,na.rm=T),0)
h_test$Car[is.na(h_test$Car)]=round(mean(h_test$Car,na.rm=T),0)
h_test$Landsize[is.na(h_test$Landsize)]=round(mean(h_test$Landsize,na.rm=T),0)
h_test$BuildingArea[is.na(h_test$BuildingArea)]=round(mean(h_test$BuildingArea,na.rm=T),0)
h_test$YearBuilt[is.na(h_test$YearBuilt)]=round(median(h_test$YearBuilt,na.rm=T),0)

h_test$Price=NA
h_train$data='train'
h_test$data='test'
all_data=rbind(h_train,h_test)
apply(all_data,2,function(x)sum(is.na(x)))


t=table(all_data$Suburb)
View(t)
t1=round(tapply(all_data$Price,all_data$Suburb,mean,na.rm=T),0)
View(t1)
t1=sort(t1)

all_data=all_data %>% 
  mutate(
    sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
  select(-Suburb)
all_data=all_data %>% 
  select(-Address)

all_data=all_data %>%
  mutate(Type_t=as.numeric(Type=="t"),
         type_u=as.numeric(Type=="u")) %>% 
  select(-Type)


all_data=all_data %>%
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_SA=as.numeric(Method=="SA"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)

all_data=all_data %>%
  mutate(Gnelson=as.numeric(SellerG=="Nelson"),
         GJellis=as.numeric(SellerG=="Jellis"),
         Ghstuart=as.numeric(SellerG=="hockingstuart"),
         Gbarry=as.numeric(SellerG=="Barry"),
         GMarshall=as.numeric(SellerG=="Marshall"),
         GWoodards=as.numeric(SellerG=="Woodards"),
         GBrad=as.numeric(SellerG=="Brad"),
         GBiggin=as.numeric(SellerG=="Biggin"),
         GRay=as.numeric(SellerG=="Ray"),
         GFletchers=as.numeric(SellerG=="Fletchers"),
         GRT=as.numeric(SellerG=="RT"),
         GSweeney=as.numeric(SellerG=="Sweeney"),
         GGreg=as.numeric(SellerG=="Greg"),
         GNoel=as.numeric(SellerG=="Noel"),
         GGary=as.numeric(SellerG=="Gary"),
         GJas=as.numeric(SellerG=="Jas"),
         GMiles=as.numeric(SellerG=="Miles"),
         GMcGrath=as.numeric(SellerG=="McGrath"),
         GHodges=as.numeric(SellerG=="Hodges"),
         GKay=as.numeric(SellerG=="Kay"),
         GStockdale=as.numeric(SellerG=="Stockdale"),
         GLove=as.numeric(SellerG=="Love"),
         GDouglas=as.numeric(SellerG=="Douglas"),
         GWilliams=as.numeric(SellerG=="Williams"),
         GVillage=as.numeric(SellerG=="Village"),
         GRaine=as.numeric(SellerG=="Raine"),
         GRendina=as.numeric(SellerG=="Rendina"),
         GChisholm=as.numeric(SellerG=="Chisholm"),
         GCollins=as.numeric(SellerG=="Collins"),
         GLITTLE=as.numeric(SellerG=="LITTLE"),
         GNick=as.numeric(SellerG=="Nick"),
         GHarcourts=as.numeric(SellerG=="Harcourts"),
         GCayzer=as.numeric(SellerG=="Cayzer"),
         GMoonee=as.numeric(SellerG=="Moonee"),
         GYPA=as.numeric(SellerG=="YPA")
  ) %>% 
  select(-SellerG)

all_data=all_data %>%
  mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
         CA_Bayside=as.numeric(CouncilArea=="Bayside"),
         CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
         CA_Darebin=as.numeric(CouncilArea=="Darebin"),
         CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         CA_Monash=as.numeric(CouncilArea=="Monash"),
         CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
         CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CA_Manningham=as.numeric(CouncilArea=="Manningham"),
         CA_Kingston=as.numeric(CouncilArea=="Kingston"),
         CA_Hume=as.numeric(CouncilArea=="Hume"),
         CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
         CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
         CA_Moreland=as.numeric(CouncilArea=="Moreland"),
         CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
         CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
  select(-CouncilArea)

h_train=all_data %>% 
  filter(data=='train') %>% 
  select(-data)
#thus train has total obs as 7536 and 70 variables (69+price)

h_test=all_data %>% 
  filter(data=='test') %>% 
  select(-data,-Price)

set.seed(123)
s=sample(1:nrow(h_train),0.75*nrow(h_train))
train_75=h_train[s,] #5652
test_25=h_train[-s,] 


LRf=lm(Price ~ .-Postcode -sub_3,data=train_75)
summary(LRf)

a=vif(LRf)
sort(a,decreasing = T)[1:3]
LRf=lm(Price ~ .-Landsize-GRaine-GMoonee-CA_Bayside-GLITTLE-Gnelson-GSweeney-
         Ghstuart-CA_Kingston-Gbarry-GRay-GStockdale-GNoel-GJas-GBiggin-GYPA-
         CA_PortP-CA_Whitehorse-GRendina-GFletchers-GBrad-GHodges-GVillage-GLove-
         sub_4-GGary-CA_Hume-CA_Boroondara-Method_SA-GWilliams-GHarcourts-GNick-
         GGreg-CA_Monash-GWoodards-CA_Stonnington-GCayzer-Postcode-sub_3,data=train_75)

PP_test_25=predict(LRf,newdata =test_25)
PP_test_25=round(PP_test_25,1)
class(PP_test_25)

res=test_25$Price-PP_test_25 #(real value-predicted value)
#root mean square error is as follows
RMSE_test_25=sqrt(mean(res^2))
RMSE_test_25

PP_test_final=predict(LRf,newdata =h_test)
PP_test_final=round(PP_test_final,1)
class(PP_test_final)

write.table(PP_test_final, "newprice122.csv", row.names = F, col.names = "Price")

