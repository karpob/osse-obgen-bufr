
cmake_minimum_required (VERSION 3.13)
######
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)

project (
  tst
  VERSION 1.0.0
  LANGUAGES Fortran)  # Note - CXX is required for ESMF



if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
   message(SEND_ERROR "In-source builds are disabled. Please
           issue cmake command in separate build directory.")
endif ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")

# Set the default build type to release
if (NOT CMAKE_BUILD_TYPE)
  message (STATUS "Setting build type to 'Release' as none was specified.")
  set (CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif ()

set (DOING_GEOS5 YES)

# Should find a better place for this - used in Chem component
set (ACG_FLAGS -v)
set (F2PYEXT .so)
set (F2PY_SUFFIX .so)
set (FV_PRECISION R4R8)



######


list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/@cmake")
include (esma)
ecbuild_declare_project()

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


#project( tst VERSION 1.0.0 LANGUAGES Fortran )
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
