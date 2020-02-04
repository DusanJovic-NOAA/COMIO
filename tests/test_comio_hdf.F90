program test_comio_hdf

  use mpi
  use comio

  implicit none

  integer :: ierr
  integer :: prank, psize

  ! This is the name of the data file we will create.
  character (len = *), parameter :: FILE_NAME = "test_comio.hd5"

  integer, parameter :: MAX_DIMS = 2
  integer, parameter :: NUM_DE_X = 2
  integer, parameter :: NUM_DE_Y = 2
  integer, parameter :: NUM_PROC = NUM_DE_X * NUM_DE_Y
  integer, parameter :: NX = 16, NY = 16
  integer, parameter :: MX = 16, MY = 8
  integer, parameter :: lix = NX / NUM_DE_X
  integer, parameter :: liy = NY / NUM_DE_Y
  integer, parameter :: lrx = MX / NUM_DE_X
  integer, parameter :: lry = MY / NUM_DE_Y

  integer, dimension(lix,liy)  :: idata_in, idata_out
  integer, dimension(lrx,lry)  :: rdata_in, rdata_out
  integer, dimension(MAX_DIMS) :: mstart, mcount
  integer :: x, y

  class(COMIO_T), pointer :: io

  ! -- begin
  call mpi_init(ierr)
  call mpi_comm_rank(MPI_COMM_WORLD, prank, ierr)
  call mpi_comm_size(MPI_COMM_WORLD, psize, ierr)

  ! -- initialize arrays
  idata_in  = 0
  idata_out = 0
  rdata_in  = 0.
  rdata_out = 0.

  ! -- create data
  idata_out = prank
  rdata_out = 10. + prank

  ! -- and data decomposition
  icount = (/ lix, liy /)
  rcount = (/ lrx, lry /)
  select case (prank)
    case (0)
      istart = (/ 1, 1 /)
      rstart = (/ 1, 1 /)
    case (1)
      istart = (/ lix + 1, 1 /)
      rstart = (/ lrx + 1, 1 /)
    case (2)
      istart = (/ 1, liy + 1 /)
      rstart = (/ 1, lry + 1 /)
    case (3)
      istart = (/ lix + 1, liy + 1 /)
      rstart = (/ lrx + 1, lry + 1 /)
    case default
      icount = 0
      rcount = 0
  end select

  io => comio_t(fmt=COMIO_FMT_PNETCDF)

  ! -- write integer data
  mstart = istart
  mcount = icount

  call io % domain_set((/ NX, NY /), mstart, mcount)
  call io % file_open(filename, "c")
  call io % dset_write("data", data_out)
  call io % file_close()

  ! -- write real data
  mstart = rstart
  mcount = rcount

  call io % file_open(filename, "w")
  call io % domain_set((/ MX, MY /), mstart, mcount)
  call io % dset_write("datar", data_outr)
  call io % file_close()
  
  ! -- read in data
  mstart = istart
  mcount = icount
  call io % file_open(filename, "r")
  call io % domain_set((/ NX, NY /), mstart, mcount)
  call io % dset_read("data", data_in)
  call io % file_close()

  match = .true.
  if (prank .LT. 4) then
     match = all(data_in == prank)
  endif

  passed = .false.
  call mpi_allreduce(match, passed, 1, MPI_LOGICAL, MPI_LAND, MPI_COMM_WORLD, ierr)

  if (passed) then
    if (prank == 0) write(6, '(">>> TEST PASSED")')
  else
    if (prank == 0) write(6, '(">>> TEST FAILED")')
  end if

  call mpi_finalize(ierr)

end program test_comio_hdf
