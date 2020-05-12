!test-group-name:PnetCDF: double precision I/O with attributes
program test_comio_pnc_att

  use test_comio_mod

  implicit none

  logical :: status

  call test_comio_start(COMIO_FMT_PNETCDF)
  filename = "test_comio_att.nc"
  call test_comio_att_write("data")
  status =  test_comio_dbl_validate("data")
  call test_comio_result(status)
  call test_comio_stop

end program test_comio_pnc_att
