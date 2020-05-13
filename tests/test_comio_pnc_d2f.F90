!test-group-name:PnetCDF: double-to-single precision I/O
program test_comio_pnc_d2f

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_PNETCDF)
  filename = "test_comio_d2f.nc"
  call test_comio_d2f_write("data")
  status =  test_comio_dbl_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_pnc_d2f
