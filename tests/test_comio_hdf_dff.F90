!test-group-name:HDF5: double-to-single precision I/O with _FillValue
program test_comio_hdf_dff

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_HDF5)
  filename = "test_comio_dff.hd5"
  call test_comio_dff_write("data")
  status =  test_comio_dff_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_hdf_dff
