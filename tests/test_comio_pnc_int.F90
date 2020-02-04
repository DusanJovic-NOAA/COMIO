!test-group-name:PnetCDF: integer I/O
program test_comio_pnc_int

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_PNETCDF)
  filename = "test_comio_int.nc"
  call test_comio_int_write("data")
  status =  test_comio_int_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_pnc_int
