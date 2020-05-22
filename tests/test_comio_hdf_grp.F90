!test-group-name:HDF5: floating point I/O with subgroups
program test_comio_hdf_grp

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_HDF5)
  filename = "test_comio_grp.hd5"
  call test_comio_flt_write("/group1/group2/group3/data")
  status =  test_comio_flt_validate("/group1/group2/group3/data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_hdf_grp
