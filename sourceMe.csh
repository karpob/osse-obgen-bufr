module use -a /discover/swdev/gmao_SIteam/modulefiles-SLES12
module load GEOSenv
setenv GEOS_WHIR /discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas
source ${GEOS_WHIR}/@env/g5_modules
module load python/GEOSpyD/Ana2019.10_py3.7
echo "to build this now mkdir build, cd build, cmake .. -DBASEDIR=BASEDIR -DGEOS_WHIR=GEOS_WHIR"
