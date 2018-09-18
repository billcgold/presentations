vignette (package='data.table')
example (data.table)


library (data.table)

dt.mtcars <- data.table ( mtcars, keep.rownames = T )


#### i == filters
dt.mtcars [ cyl == 8,  ]                                                 #### filter by column value
dt.mtcars [ cyl == 8  &                                                  #### filter by multiple conditions
              wt < 4  &  
              rn %like% 'Merc' ]
dt.mtcars [ 1:5 ]                                                        #### filter for first 5 rows


#### j == select-list

dt.mtcars [ , rn ]                                                       #### select-clause one variable, vector output
dt.mtcars [['rn']]                                                       #### same as previous, much faster

dt.mtcars [ 1:5 , list (rn) ]                                            #### select-clause one variable, data.table output

dt.mtcars [ 1:5 , list (rn, cyl, hp) ] 
dt.mtcars [ 1:5 , . (rn, cyl, hp) ]                                      #### .()  equals  list()
dt.mtcars [ 1:5 , c('rn', 'cyl', 'hp'), with = F ] 
dt.mtcars [ 1:5 ,    c ( 1, 3, 5) ]

dt.mtcars [ 1:5 , .SD, .SDcols = rn:cyl ]                                #### select columns rn thru cyl

variable.col.name <- 'rn'                                                #### variable column name
dt.mtcars [ 1:5 , ..variable.col.name ]


#### by ==  group by

dt.mtcars [ , . ( mean (mpg) ), by = cyl ]                               #### group by 
dt.mtcars [ , . ( mpg = mean (mpg) ), by = cyl ]                         #### group by, output variable named mpg
dt.mtcars [ , lapply ( .SD, mean )                                       #### group by, all columns mpg thru carb
            , .SDcols = mpg:carb
            , by = cyl ]  

#### chaining - having
dt.mtcars [ , . ( mpg = mean(mpg) ), by=cyl ] [ mpg > 16 ]               #### having, aggregate then filter aggregation via chaining

#### chaining - order by
dt.mtcars [ , . (mpg = mean(mpg)), by=cyl ] [ order (-mpg)  ]            #### order by


#### data.table and vectors

1:2
1:6
1:2 %in% 1:6 
1:6 %in% 1:2 


dt.mtcars [ , cyl ] 
dt.mtcars [ , cyl ] %in% c(4,6)


dt.mtcars [ cyl %in% c(4,6) ]  [ 1:5 ]  [ order (cyl) ]


#### joins

dt.mtcars.cyl.aggr <- dt.mtcars [ , . (mpg.mean.cyl=mean(mpg)            #### create a new aggregated data.table
                                       , mpg.sd.cyl=sd(mpg)
                                       , hp.mean.cyl=mean(hp)
                                       , hp.sd.cyl=sd(hp))
                                  , by = cyl ]                            

dt.mtcars.cyl.aggr

setkeyv (dt.mtcars,c('cyl'))                                             #### sort dt.mtcars by cyl
setkeyv (dt.mtcars.cyl.aggr,c('cyl'))                                    #### sort dt.mtcars.cyl.aggr by cyl

DT <- dt.mtcars [ dt.mtcars.cyl.aggr ]                                   #### joing
DT [ 1:5 ]


#### update

dt.mtcars <- data.table ( mtcars, keep.rownames = T )
dt.mtcars [ , N := 1 ]                                                   #### Add new column N, value is always 1
dt.mtcars [ 1:5 ]


v.manufacturer <- gsub("([A-Za-z]+).*", "\\1", dt.mtcars [ , rn ] )      #### create a manufacture vector with the first word in rn
dt.mtcars [ , manufacturer := v.manufacturer ]                           #### add new variable manufacture
dt.mtcars [ 1:5 ]

dt.mtcars [ manufacturer == 'Merc', is.merc := 1 ]                       #### create is.merc indicator
dt.mtcars [ , .N, by = is.merc ]


#### data.table & plot

library (ggplot2)

dt.mtcars [ , plot ( x = mpg
                     , y = cyl
                     , main = manufacturer )
            , keyby = manufacturer ]


plot.All.XY.by.Z <- function (dt, x, y, z) { 
  #  numerics only
  dt[, (y):= lapply(.SD, function(x) {as.numeric(as.character(x))}), .SDcols = y]      
  dts <- melt(dt, id = c(x,z), measure = y) 
  p <- ggplot(dts, aes_string(x = colnames(dt)[x], y = "value", colours = colnames(dt)[z])) + 
    geom_line() + 
    facet_wrap(~ variable) 
  print (p)
} 

plot.All.XY.by.Z ( dt.mtcars, x=2, y=4:11, z=3)


# venn.diagram ( dt.mtcars [ , carb ], dt.mtcars [ , gear ], 'carb', 'gear' )

