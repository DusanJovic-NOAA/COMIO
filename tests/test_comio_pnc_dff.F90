!test-group-name:PnetCDF: double-to-single precision I/O with _FillValue
program test_comio_pnc_dff

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_PNETCDF)
  filename = "test_comio_dff.nc"
  call test_comio_dff_write("data")
  status =  test_comio_dff_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_pnc_dff
