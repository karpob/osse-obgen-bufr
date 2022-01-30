source ~/bin/go_geos.sh
setenv ESMADIR /discover/nobackup/bkarpowi/github/GEOSadas
source $ESMADIR/@env/g5_modules > /dev/null
module list


mpif90 -c m_pbutil.f -I $ESMADIR/install/include/GMAO_mpeu -I $BASEDIR/Linux/include/netcdf -L $ESMADIR/install/lib/ -L$BASEDIR/Linux/lib -lnetcdff -lnetcdf -L$BASEDIR/Linux/lib -L$BASEDIR/Linux/lib -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -L$BASEDIR/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lgpfs -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm -lm -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm   -lGMAO_mpeu -extend-source 132


mpif90 -c m_pbmin.f -I $ESMADIR/install/include/GMAO_mpeu -I $BASEDIR/Linux/include/netcdf -L $ESMADIR/install/lib/ -L$BASEDIR/Linux/lib -lnetcdff -lnetcdf -L$BASEDIR/Linux/lib -L$BASEDIR/Linux/lib -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -L$BASEDIR/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lgpfs -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm -lm -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm   -lGMAO_mpeu -extend-source 132


mpif90 -c m_errtbl.f -I $ESMADIR/install/include/GMAO_mpeu -I $BASEDIR/Linux/include/netcdf -L $ESMADIR/install/lib/ -L$BASEDIR/Linux/lib -lnetcdff -lnetcdf -L$BASEDIR/Linux/lib -L$BASEDIR/Linux/lib -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -L$BASEDIR/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lgpfs -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm -lm -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm   -lGMAO_mpeu  -extend-source 132


mpif90 -c m_obsgen_uv.f90 -I $ESMADIR/install/include/GMAO_mpeu -I $BASEDIR/Linux/include/netcdf -L $ESMADIR/install/lib/ -L$BASEDIR/Linux/lib -lnetcdff -lnetcdf -L$BASEDIR/Linux/lib -L$BASEDIR/Linux/lib -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -L$BASEDIR/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lgpfs -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm -lm -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm   -lGMAO_mpeu  -extend-source 132

 mpif90 -o mistic_to_prepb.x m_obsgen_uv.f90 m_pbutil.f m_pbmin.f m_errtbl.f mistic_to_prepb.f90  -I `pwd` -I $ESMADIR/install/include/GMAO_mpeu -I $BASEDIR/Linux/include/netcdf -L $ESMADIR/install/lib/ -L$BASEDIR/Linux/lib -lnetcdff -lnetcdf -L$BASEDIR/Linux/lib -L$BASEDIR/Linux/lib -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -L$BASEDIR/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lgpfs -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm -lm -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm   -lGMAO_mpeu -lNCEP_bufr_r8i4  -extend-source 132


# mpif90 -o ob_to_prepb.x m_obsgen_uv.f90 m_pbutil.f m_pbmin.f m_errtbl.f ob_to_prepb.f90  -I `pwd` -I $ESMADIR/install/include/GMAO_mpeu -I $BASEDIR/Linux/include/netcdf -L $ESMADIR/install/lib/ -L$BASEDIR/Linux/lib -lnetcdff -lnetcdf -L$BASEDIR/Linux/lib -L$BASEDIR/Linux/lib -lnetcdf -lmfhdf -ldf -lhdf5_hl -lhdf5 -lm -L$BASEDIR/Linux/lib -lmfhdf -ldf -lsz -ljpeg -lgpfs -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm -lm -L$BASEDIR/Linux/lib -lcurl -lz -lrt -lm   -lGMAO_mpeu -lNCEP_bufr_r8i4  -extend-source 132

