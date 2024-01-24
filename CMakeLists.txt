
###############################################################################
### COMIO
###############################################################################

add_library(comio
  src/comio_mod.F90
  src/comio_class.F90
  src/io_prec.F90
  src/io_utils.F90
  src/ioerr_class.F90
  src/hdf_class.F90
  src/pnc_class.F90
  src/io_cutils.c
)

target_compile_definitions(comio PRIVATE "PACKAGE_VERSION='0.0.10'")
target_compile_definitions(comio PRIVATE "PACKAGE_BUGREPORT='ipe_devteam@noaa.gov'")

find_package (PnetCDF COMPONENTS Fortran)
if(PnetCDF_FOUND)
  target_compile_definitions(comio PRIVATE HAVE_PNETCDF)
  target_compile_definitions(comio PRIVATE "PNETCDF_VERSION='${PnetCDF_VERSION}'")
  target_link_libraries(comio PUBLIC PnetCDF::PnetCDF_Fortran)
endif()

find_package (HDF5 COMPONENTS Fortran)
if(HDF5_FOUND)
  message(STATUS "HDF5_VERSION = ${HDF5_VERSION}")
  target_compile_definitions(comio PRIVATE HAVE_HDF5)
  target_compile_definitions(comio PRIVATE "HDF5_VERSION='${HDF5_VERSION}'")
  target_link_libraries(comio PUBLIC hdf5::hdf5_fortran)
endif()

set_target_properties(comio PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(comio INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                           $<INSTALL_INTERFACE:mod>)

###############################################################################
### Install
###############################################################################
install(
  TARGETS comio
  EXPORT comio-config
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod DESTINATION ${CMAKE_INSTALL_PREFIX})

install(EXPORT comio-config
  DESTINATION lib/cmake)
