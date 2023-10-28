df$country=df$group
df$group=as.character(df$group)
df$group[df$group=="No_Mask" & df$colour=="Scenario 1"] = "Abs: Mask -> No_Mask"
df$group[df$group=="No_Mask" & df$colour=="Scenario 2"] = "Abs: No_Mask_2 -> No_Mask"
df$group[df$group=="No_Mask" & df$colour=="Scenario 3"] = "Rel: Mask -> No_Mask"
df$group[df$group=="No_Mask" & df$colour=="Scenario 4"] = "Rel: No_Mask_2 -> No_Mask"
df$group[df$group=="No_Mask" & df$colour=="Original"] = "Fit No_Mask"

df$group[df$group=="Mask" & df$colour=="Scenario 1"] = "Abs: No_Mask_2 -> Mask"
df$group[df$group=="Mask" & df$colour=="Scenario 2"] = "Abs: No_Mask -> Mask"
df$group[df$group=="Mask" & df$colour=="Scenario 3"] = "Rel: No_Mask_2 -> Mask"
df$group[df$group=="Mask" & df$colour=="Scenario 4"] = "Rel: No_Mask -> Mask"
df$group[df$group=="Mask" & df$colour=="Original"] = "Fit Mask"

df$group[df$group=="No_Mask_2" & df$colour=="Scenario 1"] = "Abs: Mask -> No_Mask_2"
df$group[df$group=="No_Mask_2" & df$colour=="Scenario 2"] = "Abs: No_Mask -> No_Mask_2"
df$group[df$group=="No_Mask_2" & df$colour=="Scenario 3"] = "Rel: Mask -> No_Mask_2"
df$group[df$group=="No_Mask_2" & df$colour=="Scenario 4"] = "Rel: No_Mask -> No_Mask_2"
df$group[df$group=="No_Mask_2" & df$colour=="Original"] = "Fit No_Mask_2"

ordering<-c( 
  "Abs: Mask -> No_Mask",
  "Rel: Mask -> No_Mask" , 
  "Fit No_Mask",  
  "Abs: No_Mask_2 -> No_Mask",  
  "Rel: No_Mask_2 -> No_Mask",
  "Abs: No_Mask_2 -> Mask",
  "Rel: No_Mask_2 -> Mask",
  "Fit Mask",
  "Abs: No_Mask -> Mask",
  "Rel: No_Mask -> Mask",
  "Abs: Mask -> No_Mask_2",
  "Rel: Mask -> No_Mask_2",
  "Fit No_Mask_2",
  "Abs: No_Mask -> No_Mask_2",
  "Rel: No_Mask -> No_Mask_2"
)
df$group=factor(df$group,levels=ordering)


df_cf1$country=df_cf1$group
df_cf1$group=as.character(df_cf1$group)
df_cf1$group[df_cf1$group=="No_Mask" & df_cf1$colour=="Scenario 1"] = "Abs: Mask -> No_Mask"
df_cf1$group[df_cf1$group=="No_Mask" & df_cf1$colour=="Scenario 2"] = "Abs: No_Mask_2 -> No_Mask"
df_cf1$group[df_cf1$group=="No_Mask" & df_cf1$colour=="Scenario 3"] = "Rel: Mask -> No_Mask"
df_cf1$group[df_cf1$group=="No_Mask" & df_cf1$colour=="Scenario 4"] = "Rel: No_Mask_2 -> No_Mask"
df_cf1$group[df_cf1$group=="No_Mask" & df_cf1$colour=="Original"] = "Fit No_Mask"

df_cf1$group[df_cf1$group=="Mask" & df_cf1$colour=="Scenario 1"] = "Abs: No_Mask_2 -> Mask"
df_cf1$group[df_cf1$group=="Mask" & df_cf1$colour=="Scenario 2"] = "Abs: No_Mask -> Mask"
df_cf1$group[df_cf1$group=="Mask" & df_cf1$colour=="Scenario 3"] = "Rel: No_Mask_2 -> Mask"
df_cf1$group[df_cf1$group=="Mask" & df_cf1$colour=="Scenario 4"] = "Rel: No_Mask -> Mask"
df_cf1$group[df_cf1$group=="Mask" & df_cf1$colour=="Original"] = "Fit Mask"

df_cf1$group[df_cf1$group=="No_Mask_2" & df_cf1$colour=="Scenario 1"] = "Abs: Mask -> No_Mask_2"
df_cf1$group[df_cf1$group=="No_Mask_2" & df_cf1$colour=="Scenario 2"] = "Abs: No_Mask -> No_Mask_2"
df_cf1$group[df_cf1$group=="No_Mask_2" & df_cf1$colour=="Scenario 3"] = "Rel: Mask -> No_Mask_2"
df_cf1$group[df_cf1$group=="No_Mask_2" & df_cf1$colour=="Scenario 4"] = "Rel: No_Mask -> No_Mask_2"
df_cf1$group[df_cf1$group=="No_Mask_2" & df_cf1$colour=="Original"] = "Fit No_Mask_2"
df_cf1$group=factor(df_cf1$group,levels=ordering)

df_cf2$country=df_cf2$group
df_cf2$group=as.character(df_cf2$group)
df_cf2$group[df_cf2$group=="No_Mask" & df_cf2$colour=="Scenario 1"] = "Abs: Mask -> No_Mask"
df_cf2$group[df_cf2$group=="No_Mask" & df_cf2$colour=="Scenario 2"] = "Abs: No_Mask_2 -> No_Mask"
df_cf2$group[df_cf2$group=="No_Mask" & df_cf2$colour=="Scenario 3"] = "Rel: Mask -> No_Mask"
df_cf2$group[df_cf2$group=="No_Mask" & df_cf2$colour=="Scenario 4"] = "Rel: No_Mask_2 -> No_Mask"
df_cf2$group[df_cf2$group=="No_Mask" & df_cf2$colour=="Original"] = "Fit No_Mask"

df_cf2$group[df_cf2$group=="Mask" & df_cf2$colour=="Scenario 1"] = "Abs: No_Mask_2 -> Mask"
df_cf2$group[df_cf2$group=="Mask" & df_cf2$colour=="Scenario 2"] = "Abs: No_Mask -> Mask"
df_cf2$group[df_cf2$group=="Mask" & df_cf2$colour=="Scenario 3"] = "Rel: No_Mask_2 -> Mask"
df_cf2$group[df_cf2$group=="Mask" & df_cf2$colour=="Scenario 4"] = "Rel: No_Mask -> Mask"
df_cf2$group[df_cf2$group=="Mask" & df_cf2$colour=="Original"] = "Fit Mask"

df_cf2$group[df_cf2$group=="No_Mask_2" & df_cf2$colour=="Scenario 1"] = "Abs: Mask -> No_Mask_2"
df_cf2$group[df_cf2$group=="No_Mask_2" & df_cf2$colour=="Scenario 2"] = "Abs: No_Mask -> No_Mask_2"
df_cf2$group[df_cf2$group=="No_Mask_2" & df_cf2$colour=="Scenario 3"] = "Rel: Mask -> No_Mask_2"
df_cf2$group[df_cf2$group=="No_Mask_2" & df_cf2$colour=="Scenario 4"] = "Rel: No_Mask -> No_Mask_2"
df_cf2$group[df_cf2$group=="No_Mask_2" & df_cf2$colour=="Original"] = "Fit No_Mask_2"
df_cf2$group=factor(df_cf2$group,levels=ordering)

df_cf3$country=df_cf3$group
df_cf3$group=as.character(df_cf3$group)
df_cf3$group[df_cf3$group=="No_Mask" & df_cf3$colour=="Scenario 1"] = "Abs: Mask -> No_Mask"
df_cf3$group[df_cf3$group=="No_Mask" & df_cf3$colour=="Scenario 2"] = "Abs: No_Mask_2 -> No_Mask"
df_cf3$group[df_cf3$group=="No_Mask" & df_cf3$colour=="Scenario 3"] = "Rel: Mask -> No_Mask"
df_cf3$group[df_cf3$group=="No_Mask" & df_cf3$colour=="Scenario 4"] = "Rel: No_Mask_2 -> No_Mask"
df_cf3$group[df_cf3$group=="No_Mask" & df_cf3$colour=="Original"] = "Fit No_Mask"

df_cf3$group[df_cf3$group=="Mask" & df_cf3$colour=="Scenario 1"] = "Abs: No_Mask_2 -> Mask"
df_cf3$group[df_cf3$group=="Mask" & df_cf3$colour=="Scenario 2"] = "Abs: No_Mask -> Mask"
df_cf3$group[df_cf3$group=="Mask" & df_cf3$colour=="Scenario 3"] = "Rel: No_Mask_2 -> Mask"
df_cf3$group[df_cf3$group=="Mask" & df_cf3$colour=="Scenario 4"] = "Rel: No_Mask -> Mask"
df_cf3$group[df_cf3$group=="Mask" & df_cf3$colour=="Original"] = "Fit Mask"

df_cf3$group[df_cf3$group=="No_Mask_2" & df_cf3$colour=="Scenario 1"] = "Abs: Mask -> No_Mask_2"
df_cf3$group[df_cf3$group=="No_Mask_2" & df_cf3$colour=="Scenario 2"] = "Abs: No_Mask -> No_Mask_2"
df_cf3$group[df_cf3$group=="No_Mask_2" & df_cf3$colour=="Scenario 3"] = "Rel: Mask -> No_Mask_2"
df_cf3$group[df_cf3$group=="No_Mask_2" & df_cf3$colour=="Scenario 4"] = "Rel: No_Mask -> No_Mask_2"
df_cf3$group[df_cf3$group=="No_Mask_2" & df_cf3$colour=="Original"] = "Fit No_Mask_2"
df_cf3$group=factor(df_cf3$group,levels=ordering)

df_cf4$country=df_cf4$group
df_cf4$group=as.character(df_cf4$group)
df_cf4$group[df_cf4$group=="No_Mask" & df_cf4$colour=="Scenario 1"] = "Abs: Mask -> No_Mask"
df_cf4$group[df_cf4$group=="No_Mask" & df_cf4$colour=="Scenario 2"] = "Abs: No_Mask_2 -> No_Mask"
df_cf4$group[df_cf4$group=="No_Mask" & df_cf4$colour=="Scenario 3"] = "Rel: Mask -> No_Mask"
df_cf4$group[df_cf4$group=="No_Mask" & df_cf4$colour=="Scenario 4"] = "Rel: No_Mask_2 -> No_Mask"
df_cf4$group[df_cf4$group=="No_Mask" & df_cf4$colour=="Original"] = "Fit No_Mask"

df_cf4$group[df_cf4$group=="Mask" & df_cf4$colour=="Scenario 1"] = "Abs: No_Mask_2 -> Mask"
df_cf4$group[df_cf4$group=="Mask" & df_cf4$colour=="Scenario 2"] = "Abs: No_Mask -> Mask"
df_cf4$group[df_cf4$group=="Mask" & df_cf4$colour=="Scenario 3"] = "Rel: No_Mask_2 -> Mask"
df_cf4$group[df_cf4$group=="Mask" & df_cf4$colour=="Scenario 4"] = "Rel: No_Mask -> Mask"
df_cf4$group[df_cf4$group=="Mask" & df_cf4$colour=="Original"] = "Fit Mask"

df_cf4$group[df_cf4$group=="No_Mask_2" & df_cf4$colour=="Scenario 1"] = "Abs: Mask -> No_Mask_2"
df_cf4$group[df_cf4$group=="No_Mask_2" & df_cf4$colour=="Scenario 2"] = "Abs: No_Mask -> No_Mask_2"
df_cf4$group[df_cf4$group=="No_Mask_2" & df_cf4$colour=="Scenario 3"] = "Rel: Mask -> No_Mask_2"
df_cf4$group[df_cf4$group=="No_Mask_2" & df_cf4$colour=="Scenario 4"] = "Rel: No_Mask -> No_Mask_2"
df_cf4$group[df_cf4$group=="No_Mask_2" & df_cf4$colour=="Original"] = "Fit No_Mask_2"
df_cf4$group=factor(df_cf4$group,levels=ordering)


# dataframe for additional lines
df_originals=df
df_originals$group=as.character(df_originals$group)
df_originals = df_originals[!df_originals$group%in%c("Fit No_Mask","Fit Mask","Fit No_Mask_2"),]
un=unique(df_originals$group)
df_originals$median[grep("-> No_Mask",df_originals$group)] = df$median[grep("^Fit No_Mask$",df$group)]
df_originals$median[grep("-> Mask",df_originals$group)] = df$median[grep("^Fit Mask$",df$group)]
df_originals$median[grep("-> No_Mask_2",df_originals$group)] = df$median[grep("^Fit No_Mask_2$",df$group)]
df_originals$group=as.factor(df_originals$group)


mxs=c(
  max(df$df$deaths_ui[df$group==ordering[1]],df$deaths_ui[df$group==ordering[2]]),
  max(df$deaths_ui[df$group==ordering[1]],df$deaths_ui[df$group==ordering[2]]),
  max(df$deaths_ui[df$group==ordering[3]]),
  max(df$deaths_ui[df$group==ordering[4]],df$deaths_ui[df$group==ordering[5]]),
  max(df$deaths_ui[df$group==ordering[4]],df$deaths_ui[df$group==ordering[5]]),
  
  max(df$deaths_ui[df$group==ordering[6]],df$deaths_ui[df$group==ordering[7]]),
  max(df$deaths_ui[df$group==ordering[6]],df$deaths_ui[df$group==ordering[7]]),
  max(df$deaths_ui[df$group==ordering[8]]),
  max(df$deaths_ui[df$group==ordering[9]],df$deaths_ui[df$group==ordering[10]]),
  max(df$deaths_ui[df$group==ordering[9]],df$deaths_ui[df$group==ordering[10]]),  
  
  max(df$deaths_ui[df$group==ordering[11]],df$deaths_ui[df$group==ordering[12]]),
  max(df$deaths_ui[df$group==ordering[11]],df$deaths_ui[df$group==ordering[12]]),
  max(df$deaths_ui[df$group==ordering[13]]),
  max(df$deaths_ui[df$group==ordering[14]],df$deaths_ui[df$group==ordering[15]]),
  max(df$deaths_ui[df$group==ordering[14]],df$deaths_ui[df$group==ordering[15]])    
)
df_aux <- data.frame(date = rep(as.Date("01/03/2020",format="%d/%m/%Y"),length(ordering)),
                     max=mxs,
                     group=ordering,
                     country=c(rep("No_Mask",5),rep("Mask",5),rep("No_Mask_2",5)))
