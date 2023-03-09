# osse-obgen-bufr
tools to generate bufr files for either a netcdf format or ascii formatted winds

mistic_to_prepb.f90    --> driver for ascii version
mistic_to_prepb_nc.f90 --> driver for netcdf version
m_obsgen_uv.f90 --> contains helper functions for bothe netcdf and ascii version
m_pbmin.f  --> BUFR stuff
m_pbutil.f --> more BUFR stuff
m_errtbl.f --> even more BUFR stuff
prepobs_errtable.global
qscat_tabl

@cmake points to a GEOSadas build to pick up cmake stuff.

to build, source sourceMe.csh  and follow instructions replacing with $

mkdir build
cd build
cmake .. -DBASEDIR=$BASEDIR -DGEOS_WHIR=$GEOS_WHIR
make

The executables with be in build/bin/

mistic_to_prepob_nc.x
mistic_to_prepob.x

You will then want to either symlink or copy prepobs_errtable.global and qscat_tabl to build/bin/

Test files are located in the top level directory, you can symlink them or copy them into bin to test the two programs:

To run the ascii version:
./mistic_to_prepob.x test.wind.profile.uv 2006060100 261

To run the netcdf version:

./mistic_to_prepob_nc.x test.wind.profile.uv.nc4  2006060100 261

You will need to source the sourceMe.csh before running the executable.
