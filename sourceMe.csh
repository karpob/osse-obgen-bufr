module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES12
module load GEOSenv
setenv GEOS_WHIR /discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas
source ${GEOS_WHIR}/@env/g5_modules
echo "to build this now mkdir build, cd build, cmake .. -DBASEDIR=BASEDIR -DGEOS_WHIR=GEOS_WHIR"
