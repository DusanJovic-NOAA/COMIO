!test-group-name:HDF5: multiple datasets I/O
program test_comio_hdf_mds

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_HDF5)
  filename = "test_comio_mds.hd5"
  call test_comio_mds_write("data")
  status =  test_comio_mds_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_hdf_mds
