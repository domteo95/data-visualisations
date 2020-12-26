#Data Cleaning 
df = read.csv(file="WV.csv", header=T)
colnames(df)[colnames(df) == "X"] = "Countries"
df = df[-c(5:9)]
colnames(df)[5] = "Firms_Female_Ownership"
colnames(df)[6] = "Female_Share_Middle_Senior_Management"
colnames(df)[7] = "Female_Share_Parliaments"
colnames(df)[8] = "Gender_Non_Discrimination_Constitution"
df = df[-c(3:4)]
df = df[-c(1:3),]
df = df[c(1:230), ]
df[df==".."] = NA

library(ggplot2)

#Visualising Female Share Middle Senior  Management
senior_manage = df[c(1, 2,4)]
senior_manage = na.omit(senior_manage)
attach(senior_manage)
senior_manage$Female_Share_Middle_Senior_Management = as.numeric(levels(Female_Share_Middle_Senior_Management)[Female_Share_Middle_Senior_Management])
senior_manage = senior_manage[order(Female_Share_Middle_Senior_Management),]
senior_manage = na.omit(senior_manage)
senior_manage_top= senior_manage[c(81:93, 1:12),]
senior_manage_top = na.omit(senior_manage_top)

detach(senior_manage)
attach(senior_manage_top)

bar = ggplot(senior_manage_top, aes(x= reorder(Countries, Female_Share_Middle_Senior_Management), y=Female_Share_Middle_Senior_Management, fill = Continent)) + geom_bar(stat = "identity") + coord_flip()
bar = bar + scale_fill_manual("Continents", values = c("Africa" = "palevioletred3", "Asia" = "lightsalmon3", "Europe" = "thistle3", "N.America" = "lightgoldenrod3", "Oceania" = "darkseagreen3"))
bar = bar + geom_hline(aes(yintercept=30.4), linetype = "dashed")
bar = bar + labs(title = "Countries with the highest and lowest proportion \nof female middle and senior managers", subtitle = "Proportion calculated by dividing female managers by total working females, Data from 2011 - 2017", caption = "International Labour Organisation (ILO)")
bar = bar + labs(y="Proportion of Female Middle & Senior Management (%)")
bar = bar + labs(x = NULL)
bar = bar + geom_text(x =1, y =33, label = "Average among 92 countries", size =3)
bar = bar + theme(plot.title = element_text(size = 15, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.89, 0.26))
detach(senior_manage_top)

continent_senior = aggregate(senior_manage[, 3], list(senior_manage$Continent), mean)
continent_senior = as.data.frame(continent_senior)
colnames(continent_senior) = c("Continent", "Female Proportion of Middle/Senior Managers (%)")

attach(continent_senior)
continent_senior$highlight_flag = ifelse(continent_senior$`Female Proportion of Middle/Senior Managers (%)` < 30.4, T, F)
con_senior_bar = ggplot(continent_senior, aes(x = reorder(Continent, `Female Proportion of Middle/Senior Managers (%)`), y= `Female Proportion of Middle/Senior Managers (%)`)) + geom_bar(aes(fill = highlight_flag), stat = "identity") + coord_flip()
con_senior_bar = con_senior_bar + scale_fill_manual(values = c('#595959', 'red'))
con_senior_bar = con_senior_bar + geom_hline(aes(yintercept=30.4), linetype = "dashed")
con_senior_bar = con_senior_bar + theme(legend.position = "none")
con_senior_bar = con_senior_bar + labs(title = "Average of female middle and senior \nmanagers proportion by Continent ", caption = "International Labour Organisation (ILO)")
con_senior_bar = con_senior_bar + labs(x = NULL)
con_senior_bar = con_senior_bar + geom_text(x =1, y =33, label = "Average among 92 countries", size =3)
con_senior_bar = con_senior_bar+ theme(plot.title = element_text(size = 15, face = 'bold'))  + theme(plot.caption = element_text(size = 8)) 
con_senior_bar
detach(continent_senior)


#Visualising Female Ownership of Firms
fem_owner = df[c(1,2,3)]
fem_owner = na.omit(fem_owner)
fem_owner = fem_owner[c(1:108),]
attach(fem_owner)
fem_owner$Firms_Female_Ownership = as.numeric(levels(Firms_Female_Ownership)[Firms_Female_Ownership])
fem_owner = fem_owner[order(Firms_Female_Ownership),]
fem_owner_top= fem_owner[c(96:108, 1:12),]
detach(fem_owner)
attach(fem_owner_top)
bar1 = ggplot(fem_owner_top, aes(x= reorder(Countries, Firms_Female_Ownership), y=Firms_Female_Ownership, fill = Continent)) + geom_bar(stat = "identity") + coord_flip()
bar1 = bar1 + scale_fill_manual("Continent", values = c("Africa" = "palevioletred3", "Asia" = "lightsalmon3", "Europe" = "thistle3", "S.America" = "royalblue3"))
bar1 = bar1 + geom_hline(aes(yintercept=33.7), linetype = "dashed")
bar1 = bar1 + labs(title = "Countries with the highest and lowest share of firms \nwhich counts a female as one of its principal owners",   subtitle = "Data from 2011 to 2018" )
bar1 = bar1 + labs(y="Firms with Female Ownership (%)")
bar1 = bar1 + labs(x = NULL, caption = "Source: World Bank and Enterprise Surveys")
bar1 = bar1 + theme(plot.title = element_text(size = 14.8, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.88, 0.20))
bar1 = bar1 + geom_text(x =1, y =35, label = "Average among 108 countries", size =3)
detach(fem_owner_top)

continent_own = aggregate(fem_owner[, 3], list(fem_owner$Continent), mean)
colnames(continent_own) = c("Continent", "Firms with Female Ownership (%)")
continent_own = continent_own[c(2:7),]

attach(continent_own)
continent_own$highlight_flag = ifelse(`Firms with Female Ownership (%)` < 33.7, T, F)
own_average = ggplot(continent_own, aes(x = reorder(Continent, `Firms with Female Ownership (%)`), y= `Firms with Female Ownership (%)`)) + geom_bar(aes(fill =highlight_flag), stat = "identity") + coord_flip()
own_average = own_average + scale_fill_manual(values = c('#595959', 'red'))
own_average = own_average + geom_hline(aes(yintercept=33.7), linetype = "dashed")
own_average = own_average + labs(title = "Continental average of firms with female ownership", caption = "Source: World Bank and Enterprise Surveys", x = NULL)
own_average = own_average + theme(plot.title = element_text(size = 15, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.88, 0.20))
own_average
detach(continent_own)


#Visualising Female Share in Parliament 
fem_par = df[c(1,2,5)]
fem_par = na.omit(fem_par)
fem_par = fem_par[c(1:193),]
fem_par$Female_Share_Parliaments = as.numeric(levels(fem_par$Female_Share_Parliaments)[fem_par$Female_Share_Parliaments])
fem_par = fem_par[order(fem_par$Female_Share_Parliaments),]
fem_par = na.omit(fem_par)
fem_par_top = fem_par[c(181:193, 1:12),]

attach(fem_par_top)
bar2 = ggplot(fem_par_top, aes(x= reorder(Countries, Female_Share_Parliaments), y=Female_Share_Parliaments, fill = Continent)) + geom_bar(stat = "identity") + coord_flip()
bar2 = bar2 + scale_fill_manual("Continent", values = c("Africa" = "palevioletred3", "Asia" = "lightsalmon3", "Europe" = "thistle3","N.America" = "lightgoldenrod3", "S.America" = "royalblue3", "Oceania" = "darkseagreen3"))
bar2 = bar2 + geom_hline(aes(yintercept=21.8), linetype = "dashed")
bar2 = bar2 + labs(title = "Countries with the highest and lowest female share \nof national parliament seats", subtitle = "Data from 2018" , caption = "Source: Inter-Parliamentary Union")
bar2 = bar2 + labs(y="Female Share of National Parliament Seats (%)")
bar2 = bar2 + labs(x = NULL)
bar2 = bar2 + theme(plot.title = element_text(size = 15.2, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.89, 0.24))
bar2 = bar2 + geom_text(x =1, y =28, label = "Average among 196 countries", size =3)
detach(fem_par_top)

#Visualising the relation between female share of senior/middle management and national parliaments 
common_owner_senior = merge(fem_owner, senior_manage)
common_senior_parliament = merge(fem_par, senior_manage)
attach(common_senior_parliament)

scatter = ggplot(common_senior_parliament, aes(x=Female_Share_Middle_Senior_Management, y = Female_Share_Parliaments, colour = Continent)) + geom_point()
scatter = scatter + scale_color_manual("Continent", values = c("palevioletred3", "lightsalmon3", "thistle3","lightgoldenrod3", "darkseagreen3", "royalblue3"))  
scatter = scatter + geom_smooth(method = "lm", se=FALSE)
scatter = scatter + theme(plot.title = element_text(size = 14.8, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.07, 0.76)) + theme(legend.text = element_text(size = 5), legend.title = element_text(size = 6))
scatter = scatter + labs(title = "Understanding the relationship between female share of \nparliaments and senior management positions by continent", subtitle = "90 countries with data for both national parliament & senior management share", caption = "Source: Inter-Parliamentary Union & Internaitonal Labour Organisation")
scatter = scatter + labs(y = "Female Share of National Parliament Seats (%)", x = "Proportion of Female Middle & Senior Management (%)")

scatter_general = ggplot(common_senior_parliament, aes(x=Female_Share_Middle_Senior_Management, y = Female_Share_Parliaments)) + geom_point()
scatter_general = scatter_general + geom_smooth(method = "lm", se=FALSE, formula = y ~ x) 
lm_eqn <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
x = Female_Share_Middle_Senior_Management
y = Female_Share_Parliaments
scatter_general = scatter_general + geom_text(x = 15, y = 55, label = lm_eqn(df), parse = TRUE)
scatter_general = scatter_general + theme(plot.title = element_text(size = 14.8, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.07, 0.76)) + theme(legend.text = element_text(size = 5), legend.title = element_text(size = 6))
scatter_general = scatter_general + labs(title = "Understanding the relationship between female share of \nparliaments and senior management positions", subtitle = "90 countries with data for both national parliament & senior management share", caption = "Source: Inter-Parliamentary Union & Internaitonal Labour Organisation")
scatter_general = scatter_general + labs(y = "Female Share of National Parliament Seats (%)", x = "Proportion of Female Middle & Senior Management (%)")
detach(common_senior_parliament)


#visualising relationship between ownership & parliament
common_owner_parliament = merge(fem_owner, fem_par)
attach(common_owner_parliament)
scat_owner_par = ggplot(common_owner_parliament, aes(x=Firms_Female_Ownership, y = Female_Share_Parliaments)) + geom_point()
scat_owner_par = scat_owner_par +  geom_smooth(method = "lm", se=FALSE, formula = y ~ x) 
x = common_owner_parliament$Firms_Female_Ownership
y = common_owner_parliament$Female_Share_Parliaments
scat_owner_par = scat_owner_par + geom_text(x = 15, y = 55, label = lm_eqn(df), parse = TRUE)
scat_owner_par = scat_owner_par + theme(plot.title = element_text(size = 14.8, face = 'bold')) + theme(plot.subtitle = element_text(size = 8)) + theme(plot.caption = element_text(size = 8)) + theme(legend.position = c(0.07, 0.76)) + theme(legend.text = element_text(size = 5), legend.title = element_text(size = 6))
scat_owner_par = scat_owner_par + labs(title = "Understanding the relationship between female share of \nparliaments and female firm ownership", subtitle = "105 countries with data for both national parliament & firm firm ownership share", caption = "Source: Inter-Parliamentary Union, World Bank & Enterprise Surveys")
scat_owner_par = scat_owner_par + labs(y = "Female Share of National Parliament Seats (%)", x = "Firms with Female Ownership (%)")

