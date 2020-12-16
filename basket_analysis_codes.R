# Data
# In the data set, each observation is characterized by 3 quantitative variables describing the features of the apartments sold.

knitr::opts_chunk$set(fig.align = 'center', out.width = '80%', echo = TRUE)
library(dplyr)
library(factoextra)
library(ggbiplot)
library(arules)
library (arulesViz)
library(knitr)

setwd("/Users/alubis/Desktop/OneDrive/DS")
dane=read.csv("flats.csv", sep=",")
dane$age= 2020-dane$construction_date
dane = dane %>% filter(flat_type== c("1 ROOM", "2 ROOM","3 ROOM", "4 ROOM", "5 ROOM"))
dane$room_numb = as.integer(substring(dane$flat_type, 1, 1))
dane= dane %>% mutate(flat_model = case_when(flat_model == 'Premium Apartment Loft' ~ 'Premium Apartment',
                                             flat_model == 'Model A2' ~ 'Model A', TRUE ~ flat_model))
dane = dane %>% filter(flat_model== c("New Generation", "Standard", "Simplified", "Premium Apartment"))


dane = dane %>% select(town, flat_model, flat_type)

dane$town = as.factor(dane$town)
dane$flat_model= as.factor(dane$flat_model)
dane$flat_type = as.factor(dane$flat_type)
dane= na.omit(dane)
dane = dane %>% mutate(id=row_number(dane$town))
dane$id=as.factor(dane$id)


kable(dane[1:10,], align = "cc", caption = "Table 1. The first 10 rows of data.")


# Basket Analysis

# The purpose of basket market analysis is to determine which combinations of products or services are most often purchased by customers. It is based on association rules. It is based on association rules that create a scheme by which it can be assumed with a certain probability that if A has occurred, then B will also occur.
# They make it possible to predict the simultaneous occurrence of two interdependent phenomena and behaviors.
# 
# The first stage is to build a basket consisting of data describing the apartments sold and their features.

invisible({capture.output({
  as_trans<-dane[c("town","flat_model","flat_type")]
  as_trans$town<- as.factor(as_trans$town)
  as_trans$flat_model<- as.factor(as_trans$flat_model)
  as_trans$flat_type<- as.factor(as_trans$flat_type)
  split(as_trans[,1], as_trans[,2], as_trans[,3])
  basket <- as(as_trans, "transactions")
})})


# Now we can display the basic information of our basket.


basket


# Number of items: `r sum(size(basket))`
# 
# Number of baskets: `r length(basket)`
# 
# The biggest basket consists of `r ncol(basket)` products.
# 
# The chart below presents the 10 most common transactions in the housing market data.

itemFrequencyPlot(basket, topN=10, type="relative", cex.names=0.8) 


# We can see that in the residential market, the greatest number of properties sold belong to the "New Generation" model and four-room apartments.
# 
# Next step of the analysis is an induction of the rules from determined itemsets.
# We set the supp parameter on the 0.05 value.

rules<-apriori(basket, parameter = list(supp=.05))
redundant_rules<-is.redundant(rules)
#summary(redundant_rules)

# Table 2. Inspection of the rules.

inspect(rules)


# We can see that for the value of 0.05 of the parameter supp we get 7 rules.
# 
# On the table above we can see the most frequent itemsets.
# Considering that the value of the support may range from 0 to 1, we can conclude that the basket does not have a one clear purchasing pattern. The most popular model is the purchase of an apartment in Ang Mo Kio with a New Generation standard. The first two columns refer to a set of items which say that if x is purchased then y will be bought. Lift indicates the probability of a purchasing pattern compared to a situation where items are interdependent. Around 10% of all transactions contain flats in New Generation in Ang Mo Kio town. According to the value of confidence, the probability of buying an apartment in New Generation standard in a transaction in Ang Mo Kio is 0.98. Moreover, the total occurrence of these items is 2.21 times greater than we expected, assuming the independence of both housing characteristics.

# Our results we can also presented graphically. 
# Below we can see 7 obtained rules.The arrows shows the direction of the basket rules, the size of circles the support rate and the color the lift value.

topRules<-rules#[1:10]
plot(topRules, method="graph", engine = "htmlwidget")



arulesViz::plotly_arules(topRules, method="matrix", measure=c("support","confidence"))

# The redder the color, the stronger the association rule.The strength of the rule is measured by a lift and the layout depends on the value of your support and confidence.
# 
# The next chart shows the same information but in a different graphical way:


plot(topRules, method = "grouped")

# Individual rule representation

# Now we will focus on particular items of our basket.
# The most common item is the city of Ang Mo Kio, so we will analyze for this level.


one_item <-subset(rules, items %in% "town=ANG MO KIO")
inspect(one_item)


plot(one_item, method="graph", measure="lift",shading="confidence")


# The algorithm returns two rules that confirm that when buying a flat in Ang Mo Kio, they are most likely to choose a New Generation flat.

plot(one_item, method="paracoord")


# The last plot show parallel coordinates. It shows a diagram of the selection of features when buying an apartment.
# 
# # Conclusions and summary
# 
# This paper presents the basket analysis of housing market. This method examines customer buying patterns by identifying associations among various items that customers place in their shopping baskets.The analysis showed that the most frequently sold apartments are those of the 'New Generation' type. The most common rule is that customers who were interested in buying an apartment in Ang Mo Kio were also interested in the 'New Generation' apartment model. Another common dependence was the purchase of Premium Apartments in Punggol or Sengkand.In Punggol the most often bought 4-room apartments, while in Bedok the 3-room apartments, which belonged to the 'Premium' model.The analysis allows us to examine the housing market and consumer behaviour, which allows us to select more interesting proposals for a potential buyer.
# 
# ------------------------------------------------------------------------------------
#   Sources:
#   1.*Statystyczna Analiza Danych z wykorzystaniem programu R. Marek Walesiak. 2012. PWN.*
#   2.*Association Rule – Extracting Knowledge Using Market Basket Analysis. Research Journal of Recent Sciences. Raorane A.A. 2012*
#   3.*A Survey on Association Rule Mining in Market Basket Analysis. Savi Gupta.International Journal of Information and Computation Technology. 2014*
#   