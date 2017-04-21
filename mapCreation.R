library(raster)
fileH<-nc_open("/nobackup/users/pagani/gsie/output/pagani/wind10min/Fg_1_oper_v0001/2015/03/22/INTER_OPER_R___FG1_____L3__20150322T000000_20150322T001000_0001.nc")

variable <- raster("/nobackup/users/pagani/gsie/output/pagani/wind10min/Fg_1_oper_v0001/2015/03/22/INTER_OPER_R___FG1_____L3__20150322T001000_20150322T002000_0001.nc", varname="grid")
plot(variable)

