!test-group-name:PnetCDF: double precision I/O
program test_comio_pnc_dbl

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_PNETCDF)
  filename = "test_comio_dbl.nc"
  call test_comio_dbl_write("data")
  status =  test_comio_dbl_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_pnc_dbl
