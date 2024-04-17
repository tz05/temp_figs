library(geodata)
counties <- gadm(country='USA',level=2,path=tempdir(),resolution=1)
counties$NAME_2[counties$NAME_1=='New Mexico'&counties$NAME_2=='Debaca'] <- 'De Baca'
states <- gadm(country='USA',level=1,path=tempdir(),resolution=2)

db_all <- vect('~/Downloads/InterAgencyFirePerimeterHistory_All_Years_View/InterAgencyFirePerimeterHistory_All_Years_View.shp')
db_all <- project(db_all,'epsg:4326')
db_usfs <- db_all[db_all$SOURCE=='USFS',]
head(db_usfs,2)

fires00 <- db_usfs[db_usfs$FIRE_YEAR_>2016&db_usfs$GIS_ACRES>1000,]
fires0 <- db_usfs[db_usfs$FIRE_YEAR_>2016&db_usfs$GIS_ACRES>10000,]
par(mfrow=c(1,2))
barplot(table(fires00$FIRE_YEAR_))
barplot(table(fires0$FIRE_YEAR_))
dim(fires00)
dim(fires0)

adj_counties <- adjacent(counties)

y <- relate(counties,fires0,'intersects',pairs=T)
zz <- relate(counties,fires0,'contains',pairs=T)
fires <- fires0[zz[,'id.y'],]
adj_fire_counties <- adj_counties[adj_counties[,'from']%in%zz[,'id.x'],]
fire_from_year <- sapply(adj_fire_counties[,'from'], function(cnt) {
  yrs <- unique(values(fires0[zz[zz[,'id.x']==cnt,'id.y'],'FIRE_YEAR_']))
  return(as.integer(unlist(yrs)))
})
fire_to_year <- sapply(adj_fire_counties[,'to'], function(cnt) {
  yrs <- unique(values(fires00[y[y[,'id.x']%in%cnt,'id.y'],'FIRE_YEAR_']))
  if(nrow(yrs)==0) return(NA)
  else return(as.integer(unlist(yrs)))
})


fire_counties <- data.frame(from=adj_fire_counties[,'from'],
                            counties[adj_fire_counties[,'from'],c('NAME_1','NAME_2','HASC_2')],
                            to=adj_fire_counties[,'to'],
                            counties[adj_fire_counties[,'to'],c('NAME_1','NAME_2','HASC_2')])
fire_counties$fire_from_year <- fire_from_year
fire_counties$fire_to_year <- fire_to_year
names(fire_counties) <- c('from','cnt1_state','cnt1_county','cnt1_HASC',
                          'to','cnt2_state','cnt2_county','cnt2_HASC',
                          'fire_year1','fire_year2')
dim(fire_counties)

fire_counties$eligible_year <- apply(fire_counties[,c('fire_year1','fire_year2')], 1, function(yrs) sort(setdiff(yrs[[1]],yrs[[2]])))
options(width=170)
options(setWidthOnResize=TRUE)
d_FireCounty <- fire_counties[sapply(fire_counties$eligible_year,length)>0,-c(4,8)]
dim(d_FireCounty)
head(d_FireCounty)

cnt1 <- unique(d_FireCounty[,c('cnt1_state','cnt1_county')])
cnt2 <- unique(d_FireCounty[,c('cnt2_state','cnt2_county')])
dim(cnt1)
dim(cnt2)
cnts <- unique(data.table::rbindlist(list(cnt1,cnt2),use.names = F))
dim(cnts)

cnt_ind <- c(4, 10, 17, 19, 25, 32, 40, 44, 48, 55, 62, 69,
             76, 77, 84, 86, 95, 100, 105, 112, 120, 122, 126, 129,
             138, 141, 152, 159, 162,165, 169, 181, 182,
             193, 204, 208, 219, 222, 235, 242, 248, 253,
             257, 271, 273, 279, 283, 287, 298, 305, 313, 319, 322, 331, 340, 344, 351,
             356, 364, 369, 379, 381, 394, 401, 409, 415, 426, 427, 434, 442,
             446, 451, 456, 468, 471, 481, 486, 493, 503)

cnt_ind <- c(4, 10, 15 , 19, 25, 32, 41 , 44, 49 , 55, 61 , 69,
             76, 79 , 84, 88 , 95, 100, 106 , 112, 120, 122, 126, 131 ,
             138, 142 , 148 , 159, 162,165, 169, 181, 182,
             193, 204, 208, 220 , 222, 230 , 239 , 247 , 253,
             257, 262 , 271, 273, 279, 283, 287, 298, 305, 313, 319, 322, 331, 340, 344, 351,
             356, 361 , 369, 379, 381, 387 , 394, 401, 409, 415, 426, 427, 434, 442,
             446, 451, 457 , 468, 471, 481, 486, 493, 503)
cnt_ind <- as.character(cnt_ind)
dd_FireCounty <- d_FireCounty[cnt_ind,]


library(readxl)
xlsx1 <- excel_sheets('~/Downloads/temp_figs/GDP_DATA.xlsx')[-(1:2)]
xlsx2 <- excel_sheets('~/Downloads/temp_figs/sup_GDP_DATA.xlsx')[-(1:2)]
ln_sect <- c(all=7, prv=8, agr=9, min=10,utl=11,con=12,mnf=13,whl=16,rtl=17,
             trs=18,inf=19,fin=20,prf=23,edu=27,art=30,oth=33,gov=34)


gdps1 <- lapply(xlsx1, function(cnt) {
  gdp <- read_xlsx('~/Downloads/temp_figs/GDP_DATA.xlsx',cnt,range='E7:J34',
                   col_names=as.character(2017:2022),na='(D)')
  gdp <- as.data.frame(gdp)
  rownames(gdp) <- sprintf('%02d',1:nrow(gdp)+6)
  rownames(gdp)[ln_sect-6] <- names(ln_sect)
  return(gdp)
})
gdps2 <- lapply(xlsx2, function(cnt) {
  gdp <- read_xlsx('~/Downloads/temp_figs/sup_GDP_DATA.xlsx',cnt,range='E7:J34',
                   col_names=as.character(2017:2022),na='(D)')
  gdp <- as.data.frame(gdp)
  rownames(gdp) <- sprintf('%02d',1:nrow(gdp)+6)
  rownames(gdp)[ln_sect-6] <- names(ln_sect)
  return(gdp)
})
gdpss <- append(gdps1,gdps2)
nm_cnt <- strsplit(c(xlsx1,xlsx2),', ')
names(gdpss) <- sapply(nm_cnt, function(x) paste(state.name[state.abb==x[2]],x[1],sep=', '))

t <- df <- p <- NULL
for(sect in names(ln_sect)) {
  print(sect)
  gdps <- sapply(gdpss,function(x) unlist(x[sect,]))
  gdp_rel_rate <- apply(gdps,2,function(x) (x[-1]-x[-length(x)]+1)/(x[-length(x)]+1)*100)  ## +1 is to avoid Inf

  d_FireGDP <- apply(dd_FireCounty,1,function(x) {
    cnt12 <- c(paste(x$cnt1_state, x$cnt1_county, sep=', '), paste(x$cnt2_state, x$cnt2_county, sep=', '))
    yrs <- intersect(x$eligible_year,rownames(gdp_rel_rate))
    if(length(yrs)>0) gdp_rel_rate[as.character(yrs), cnt12, drop=F]
    else NULL
  })
  d1_FireGDP <- data.frame()
  for(i in seq_along(d_FireGDP)) {
    if(is.null(d_FireGDP[[i]])) next
    d <- d_FireGDP[[i]]
    colnames(d) <- c('perc_cnt1','perc_cnt2')
    d1_FireGDP <- rbind(d1_FireGDP,data.frame(ind=names(d_FireGDP)[i],d,
                                              cnt1=colnames(d_FireGDP[[i]])[1],cnt2=colnames(d_FireGDP[[i]])[2],
                                              yr=rownames(d_FireGDP[[i]])))
  }
  rownames(d1_FireGDP) <- NULL
  head(d1_FireGDP)
  
  plot(d1_FireGDP[,2:3],type='n',main=sect)
  text(d1_FireGDP[,2:3],d1_FireGDP[,1])
  abline(0,1)
  tt <- t.test(d1_FireGDP[,'perc_cnt1'],d1_FireGDP[,'perc_cnt2'],paired=T,alternative='less')
  t <- c(t,tt$statistic)
  df <- c(df,tt$parameter)
  p <- c(p,tt$p.value)
}

result <- cbind(t,df,p)
rownames(result) <- names(ln_sect)






# ln <- ln_sect[[sect]]
# gdps1 <- sapply(xlsx1, function(cnt) {
#   gdp <- read_xlsx('~/Downloads/temp_figs/GDP_DATA.xlsx',cnt,range=sprintf('E%d:J%d',ln,ln),
#                    col_names=as.character(2017:2022),na='(D)')
#   gdp[gdp=='(D)'] <- NA
#   gdp <- as.numeric(gdp)
#   return(gdp)
# })
# gdps2 <- sapply(xlsx2, function(cnt) {
#   gdp <- read_xlsx('~/Downloads/temp_figs/sup_GDP_DATA.xlsx',cnt,range=sprintf('E%d:J%d',ln,ln),
#                    col_names=as.character(2017:2022),na='(D)')
#   gdp[gdp=='(D)'] <- NA
#   gdp <- as.numeric(gdp)
#   return(gdp)
# })
# 
# gdps <- cbind(gdps1,gdps2)
# rownames(gdps) <- as.character(2017:2022)
# dim(gdps)
# nm_cnt <- strsplit(colnames(gdps),', ')
# colnames(gdps) <- sapply(nm_cnt, function(x) paste(state.name[state.abb==x[2]],x[1],sep=', '))
# gdps <- gdps[,sort(colnames(gdps))]

gdp_rel_rate <- apply(gdps,2,function(x) (x[-1]-x[-length(x)]+1)/(x[-length(x)]+1)*100)  ## +1 is to avoid Inf

d_FireGDP <- apply(dd_FireCounty,1,function(x) {
  cnt12 <- c(paste(x$cnt1_state, x$cnt1_county, sep=', '), paste(x$cnt2_state, x$cnt2_county, sep=', '))
  yrs <- intersect(x$eligible_year,rownames(gdp_rel_rate))
  if(length(yrs)>0) gdp_rel_rate[as.character(yrs), cnt12, drop=F]
  else NULL
})
d1_FireGDP <- data.frame()
for(i in seq_along(d_FireGDP)) {
  if(is.null(d_FireGDP[[i]])) next
  d <- d_FireGDP[[i]]
  colnames(d) <- c('perc_cnt1','perc_cnt2')
  d1_FireGDP <- rbind(d1_FireGDP,data.frame(ind=names(d_FireGDP)[i],d,
                                            cnt1=colnames(d_FireGDP[[i]])[1],cnt2=colnames(d_FireGDP[[i]])[2],
                                            yr=rownames(d_FireGDP[[i]])))
}
rownames(d1_FireGDP) <- NULL
head(d1_FireGDP)

plot(d1_FireGDP[,2:3],type='n')
text(d1_FireGDP[,2:3],d1_FireGDP[,1])
abline(0,1)
t.test(d1_FireGDP[,'perc_cnt1'],d1_FireGDP[,'perc_cnt2'],paired=T,alternative='less')

