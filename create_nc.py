#!/usr/bin/env python
import numpy as np
from netCDF4 import Dataset
from datetime import datetime
from datetime import timedelta
import glob
import argparse
def read_ascii(fname):
    timez = []
    lats,lons,u,v,p = [],[],[],[],[]
    
    with open(fname,'r') as f:
        for line in f:
            aYYYY,aMM,aDD,ahh,amm,ass,alat,alon,ap,au,av = line.split()
            timez.append(datetime(year=int(aYYYY),month=int(aMM),day=int(aDD),hour=int(ahh),minute=int(amm),second=int(ass)))
            lats.append(float(alon))
            lons.append(float(alat))            
            p.append(float(ap))
            u.append(float(au))
            v.append(float(av))
    return np.asarray(lats), np.asarray(lons), np.asarray(p), np.asarray(u), np.asarray(v), timez




def write_variables(fname, lats, lons, p, u, v, timez, w_year, w_month, w_day, w_hr):
    
    fh = Dataset(fname, 'w', format='NETCDF4')
    index  = fh.createDimension('time', len(timez))
    nc_secFromC = fh.createVariable('seconds_from_center', np.float32, ('time',))
    secFromC = []
    time_stamp = datetime(year=w_year,month=w_month,day=w_day,hour=w_hr)
    for t in timez:
        secFromC.append((t-time_stamp).total_seconds())
    secFromC = np.asarray(secFromC)
    nc_secFromC[:] = secFromC
    nc_secFromC.units = "seconds from window center"
    nc_secFromC.long_name = "seconds_increment" ;

    lats_  = fh.createVariable('trjLat', np.float32, ('time',))
    lats_.long_name = "latitude" ;
    lats_.units = "degrees_north" ;
    lons_  = fh.createVariable('trjLon', np.float32, ('time',))
    lons_.long_name = "longitude" ;
    lons_.units = "degrees_east" ;

    u_  = fh.createVariable('U', np.float32, ('time',))
    v_  = fh.createVariable('V', np.float32, ('time',))
    p_  = fh.createVariable('P', np.float32, ('time',))

    lats_[:] =lats[:]
    lons_[:] =lons[:]
    u_[:]  = u[:]
    v_[:]  = v[:]
    p_[:]  = p[:]


    fh.close()
if __name__ == '__main__' :
    parser = argparse.ArgumentParser( description = 'read ascii versio of bufrize and make it NC')
    parser.add_argument('--in', help = 'path to ascii', required = True, dest = 'path')
    parser.add_argument('--out', help = 'path to ncdiag', required = True, dest = 'outpath')
    parser.add_argument('--dtg', help = 'date time group', required = True, dest = 'dtg')
    arg = parser.parse_args()

    lats,lons,p,u,v,timez = read_ascii(arg.path)
    dtg = arg.dtg
    w_year  = int(dtg[0:4])
    w_month = int(dtg[4:6])
    w_day   = int(dtg[6:8])
    w_hr    = int(dtg[8:10])

    write_variables(arg.outpath, lats, lons, p, u, v, timez, w_year, w_month, w_day, w_hr)
    print("done!")



