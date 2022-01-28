#!/bin/csh

#/discover/nobackup/dcarvalh/MISTIC/NEW_CODE_DC/obs_2006062112z_20060806_00z/swath.obs.20060622_00z.0deg.obs_CLOUDS
#/discover/nobackup/dcarvalh/MISTIC/NEW_CODE_DC/obs_2006062112z_20060806_00z/swath.obs.20060622_00z.0deg.obs_WATER_VAPOR
#  1635  15:09   ./mistic_to_prepb.x swath.obs.20060622_00z.0deg.obs_WATER_VAPOR 2006062200 261
#  1636  15:10   ./mistic_to_prepb.x swath.obs.20060622_00z.0deg.obs_CLOUDS 2006062200 262

set startdate=2006062200
set enddate=2006080600

set cldkx=262
set wvkx=261

#set srcdir=/discover/nobackup/dcarvalh/MISTIC/NEW_CODE_DC/obs_2006062112z_20060806_00z/
set srcdir=/discover/nobackup/dcarvalh/MISTIC/obs_20060621_12z_20060806_00z/raw_obs/
set destdir=/discover/nobackup/wrmccart/mistic/wind_bufr/obs_2006062112z_20060806_00z/

mkdir -p $destdir
pwd


while ($startdate <= $enddate)
   set yyyy=`echo $startdate |cut -b1-4`
   set   mm=`echo $startdate |cut -b5-6`
   set   dd=`echo $startdate |cut -b7-8`
   set   hh=`echo $startdate |cut -b9-10`

   set srcfilewv=swath.obs.$yyyy$mm$dd\_$hh\z.0deg.obs_WATER_VAPOR
   set srcfilecld=swath.obs.$yyyy$mm$dd\_$hh\z.0deg.obs_CLOUDS

   set destfilewv=mistic.wv.$yyyy$mm$dd.t$hh\z.bufr
   set destfilecld=mistic.cld.$yyyy$mm$dd.t$hh\z.bufr

   echo $srcfilewv $srcfilecld $destfilewv $destfilecld
   ln -sf $srcdir/$srcfilewv .
   ln -sf $srcdir/$srcfilecld .

   ./mistic_to_prepb.x $srcfilewv $yyyy$mm$dd$hh $wvkx
   ./mistic_to_prepb.x $srcfilecld $yyyy$mm$dd$hh $cldkx

   mv $srcfilewv.bufr $destdir/$destfilewv
   mv $srcfilecld.bufr $destdir/$destfilecld

   rm $srcfilewv $srcfilecld

   set startdate=`ndate +06 $startdate`
end

