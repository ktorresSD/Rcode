library(rgl)
open3d()
comet <- readOBJ(url("http://sci.esa.int/science-e/www/object/doc.cfm?fobjectid=54726"))
shade3d(comet, col="gray")