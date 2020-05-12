!test-group-name:HDF5: integer I/O
program test_comio_hdf_dim

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_HDF5)
  filename = "test_comio_dim.hd5"
  call test_comio_int_write("data")
  status =  test_comio_dim_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_hdf_dim
