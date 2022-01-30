#####################################################################################
# # Currently not installed in GNU Make                                             #
# ecbuild_add_executable (                                                          #
#   TARGET gsi.x                                                                    #
#   SOURCES gsimain.F90                                                             #
#   LIBS GEOSaana_GridComp GSI_GridComp GEOSgsi_Coupler                             #
#        NCEP_crtm NCEP_sfcio NCEP_bufr_r8i4 NCEP_nemsio NCEP_bacio_r4i4 NCEP_gfsio #
#        NCEP_sp_r8i4 NCEP_w3_r8i4 nc_diag_read nc_diag_write)                      #
#####################################################################################
cmake_minimum_required (VERSION 3.13)
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)


project (
  dafuk
  VERSION 5.27.1
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/@cmake")
include (esma)
ecbuild_declare_project()
set(CMAKE_INCLUDE_PATH "/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas/install/include/GMAO_mpeu" )
# This is equivalent to FOPT=$(FOPT3) in GNU Make
if (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
   set (CMAKE_Fortran_FLAGS_RELEASE "${FOPT3} ${BIG_ENDIAN} ${BYTERECLEN} ${FP_MODEL_STRICT} ${ALIGNCOM}")
endif ()


find_package(MPI REQUIRED)

# Generic DFLAGS
# These should be relocated and/or eliminated.
add_definitions(-Dsys${CMAKE_SYSTEM_NAME} -DESMA64)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
include_directories("/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas/install/include/GMAO_mpeu")
include_directories("/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas/install/include/NCEP_bufr_r8i4")
include_directories("${INC_NETCDF}")
LINK_DIRECTORIES("/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas/install/lib/")


project( tst VERSION 1.0.0 LANGUAGES Fortran )
ecbuild_add_executable (
  TARGET mistic_to_prepob.x
  SOURCES m_errtbl.f  mistic_to_prepb.f90  m_obsgen_uv.f90  m_obsgen_uv.f90_bak  m_pbmin.f  m_pbutil.f  
  LIBS GMAO_mpeu NCEP_bufr_r8i4)
target_link_libraries(mistic_to_prepob.x GMAO_mpeu )
set_target_properties(mistic_to_prepob.x PROPERTIES Fortran_MODULE_DIRECTORY "/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas/install/include/GMAO_mpeu")
target_include_directories(mistic_to_prepob.x PRIVATE "/discover/nobackup/projects/gmao/obsdev/bkarpowi/osseObsGen/GEOSadas/install/include/GMAO_mpeu")
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})
target_link_libraries(mistic_to_prepob.x ${MPI_Fortran_LIBRARIES})
