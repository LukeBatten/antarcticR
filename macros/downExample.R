library(raster)

#get some sample data
data(meuse.grid)
gridded(meuse.grid) <- ~x+y
meuse.raster <- raster(meuse.grid)
res(meuse.raster)
                                        #[1] 40 40
plot(meuse.grid)

#aggregate from 40x40 resolution to 120x120 (factor = 3)
meuse.raster.aggregate <- aggregate(meuse.raster, fact=0.2)
res(meuse.raster.aggregate)
#[1] 120 120
plot(meuse.grid)
