module test_comio_mod

  use mpi
  use comio

  implicit none

  integer, parameter :: sp = kind(1.0)
  integer, parameter :: dp = kind(1.d0)

  integer, parameter :: MAX_DIMS = 2
  integer, parameter :: NUM_DE_X = 2
  integer, parameter :: NUM_DE_Y = 2
  integer, parameter :: NUM_PROC = NUM_DE_X * NUM_DE_Y
  integer, parameter :: NX = 16, NY = 16
  integer, parameter :: MX = 16, MY = 8
  integer, parameter :: DX = 64, DY = 4
  integer, parameter :: lix = NX / NUM_DE_X
  integer, parameter :: liy = NY / NUM_DE_Y
  integer, parameter :: lrx = MX / NUM_DE_X
  integer, parameter :: lry = MY / NUM_DE_Y
  integer, parameter :: ldx = DX / NUM_DE_X
  integer, parameter :: ldy = DY / NUM_DE_Y
  integer, parameter :: LOG_UNIT = 6
  integer, parameter :: RC_FAILURE = 99

  integer :: prank, psize

  integer,  dimension(MAX_DIMS) :: mstart, mcount
  integer,  dimension(lix,liy)  :: idata
  real(sp), dimension(lrx,lry)  :: rdata
  real(dp), dimension(ldx,ldy)  :: ddata

  character(len=1024) :: filename = ""

  class(COMIO_T), pointer :: io => null()

contains

  subroutine test_comio_start(fmt)
    integer, intent(in) :: fmt
    integer :: ierr
    ! -- begin
    call mpi_init(ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, prank, ierr)
    call mpi_comm_size(MPI_COMM_WORLD, psize, ierr)
    ! -- check if MPI size is compatible
    if (psize /= NUM_PROC) then
      if (prank == 0) write(0,'(">>> ERROR: Test must run on 4 MPI tasks!")')
      call mpi_abort(MPI_COMM_WORLD, RC_FAILURE, ierr)
    end if
    ! -- initialize arrays
    idata = 0
    rdata = 0.
    ddata = 0.d0
    ! -- initialize COMIO
    io => COMIO_T(fmt=fmt, comm=MPI_COMM_WORLD, info=MPI_INFO_NULL)
  end subroutine test_comio_start

  subroutine test_comio_stop
    integer :: ierr
    if (associated(io)) then
      call io % shutdown()
      deallocate(io)
    end if
    call mpi_finalize(ierr)
  end subroutine test_comio_stop

  subroutine test_comio_result(success)
    logical,          intent(in) :: success
    logical :: passed
    integer :: ierr
    ! -- gather results from all MPI tasks
    passed = .false.
    call mpi_reduce(success, passed, 1, &
      MPI_LOGICAL, MPI_LAND, 0, MPI_COMM_WORLD, ierr)
    ! -- write test result on MPI rank 0
    if (prank == 0) then
      if (passed) then
        write(LOG_UNIT,'(">>> TEST PASSED")')
      else
        write(LOG_UNIT,'(">>> TEST FAILED")')
      end if
    end if
  end subroutine test_comio_result

  subroutine test_comio_decomp(lx,ly)
    integer, intent(in) :: lx, ly
    ! -- and data decomposition
    mcount = (/ lx, ly /)
    select case (prank)
      case (0)
        mstart = (/ 1, 1 /)
      case (1)
        mstart = (/ lx + 1, 1 /)
      case (2)
        mstart = (/ 1, ly + 1 /)
      case (3)
        mstart = (/ lx + 1, ly + 1 /)
      case default
        mcount = 0
        mstart = (/ 1, 1 /)
    end select
  end subroutine test_comio_decomp

  subroutine test_comio_int_write(name)
    character(len=*), intent(in) :: name
    ! -- create data
    idata = prank
    ! -- and data decomposition
    call test_comio_decomp(lix,liy)
    ! -- write to file
    call io % open(filename, "c")
    call io % domain((/ NX, NY /), mstart, mcount)
    call io % write(name, idata)
    call io % pause(.true.)
    call io % pause(.false.)
    call io % close()
  end subroutine test_comio_int_write

  subroutine test_comio_flt_write(name)
    character(len=*), intent(in) :: name
    ! -- create data
    rdata = 10. + prank
    ! -- and data decomposition
    call test_comio_decomp(lrx,lry)
    ! -- write to file
    call io % open(filename, "c")
    call io % domain((/ MX, MY /), mstart, mcount)
    call io % write(name, rdata)
    call io % close()
  end subroutine test_comio_flt_write

  subroutine test_comio_dbl_write(name)
    character(len=*), intent(in) :: name
    ! -- create data
    ddata = 2.d0 * prank + 1.d0
    ! -- and data decomposition
    call test_comio_decomp(ldx,ldy)
    ! -- write to file
    call io % open(filename, "c")
    call io % domain((/ DX, DY /), mstart, mcount)
    call io % write(name, ddata)
    call io % close()
  end subroutine test_comio_dbl_write

  subroutine test_comio_d2f_write(name)
    character(len=*), intent(in) :: name
    ! -- create data
    ddata = 2.d0 * prank + 1.d0
    ! -- and data decomposition
    call test_comio_decomp(ldx,ldy)
    ! -- write to file
    call io % open(filename, "c")
    call io % domain((/ DX, DY /), mstart, mcount)
    call io % writeas(1.)
    call io % write(name, ddata)
    call io % close()
  end subroutine test_comio_d2f_write

  subroutine test_comio_att_write(name)
    character(len=*), intent(in) :: name
    ! -- create data
    ddata = 2.d0 * prank + 1.d0
    ! -- and data decomposition
    call test_comio_decomp(ldx,ldy)
    ! -- write to file
    call io % open(filename, "c")
    call io % domain((/ DX, DY /), mstart, mcount)
    call io % write(name, ddata)
    call io % describe(name, "units", "kg m-3")
    call io % describe(name, "min", 1.d0)
    call io % describe(name, "max", 9.d0)
    call io % describe(name, "decomp", (/ ldx, ldy /))
    call io % describe("unit_test", "COMIO attributes")
    call io % describe("domain_array", (/ DX, DY /))
    call io % close()
  end subroutine test_comio_att_write

  subroutine test_comio_mds_write(name)
    character(len=*), intent(in) :: name
    character(len=80) :: dname
    integer :: i
    ! -- and data decomposition
    call test_comio_decomp(ldx,ldy)
    ! -- write to file
    call io % open(filename, "c")
    call io % domain((/ DX, DY /), mstart, mcount)
    do i = 1, 5
      write(dname,'(a,i0)') trim(name), i
      ! -- create data
      ddata = i * (2.d0 * prank + 1.d0)
      call io % write(dname, ddata)
    end do
    call io % close()
  end subroutine test_comio_mds_write

  logical function test_comio_int_validate(name)
    character(len=*), intent(in) :: name
    ! -- default
    test_comio_int_validate = .false.
    ! -- set data decomposition
    call test_comio_decomp(lix,liy)
    ! -- read from file
    call io % open(filename, "r")
    call io % domain((/ NX, NY /), mstart, mcount)
    call io % read(name, idata)
    call io % close()
    ! -- validate data
    test_comio_int_validate = all(idata == prank)
  end function test_comio_int_validate

  logical function test_comio_flt_validate(name)
    character(len=*), intent(in) :: name
    integer :: ierr
    ! -- default
    test_comio_flt_validate = .false.
    ! -- set data decomposition
    call test_comio_decomp(lrx,lry)
    ! -- read from file
    call io % open(filename, "r")
    call io % domain((/ MX, MY /), mstart, mcount)
    call io % read(name, rdata)
    call io % close()
    ! -- validate data
    test_comio_flt_validate = all(rdata == 10. + prank)
  end function test_comio_flt_validate

  logical function test_comio_dbl_validate(name)
    character(len=*), intent(in) :: name
    integer :: ierr
    ! -- default
    test_comio_dbl_validate = .false.
    ! -- set data decomposition
    call test_comio_decomp(ldx,ldy)
    ! -- read from file
    call io % open(filename, "r")
    call io % domain((/ DX, DY /), mstart, mcount)
    call io % read(name, ddata)
    call io % close()
    ! -- validate data
    test_comio_dbl_validate = all(ddata == 2.d0*prank+1.d0)
  end function test_comio_dbl_validate

  logical function test_comio_dim_validate(name)
    character(len=*), intent(in) :: name
    integer :: ierr
    integer, pointer :: dims(:)
    ! -- default
    test_comio_dim_validate = .false.
    nullify(dims)
    ! -- set data decomposition
    call test_comio_decomp(ldx,ldy)
    ! -- read from file
    call io % open(filename, "r")
    call io % domain(name, dims)
    call io % close()
    ! -- validate data
    test_comio_dim_validate = all(dims == (/ NX, NY /))
    ! -- free up memory
    deallocate(dims)
  end function test_comio_dim_validate

  logical function test_comio_mds_validate(name)
    character(len=*), intent(in) :: name
    character(len=80) :: dname
    integer :: i, ierr
    ! -- default
    test_comio_mds_validate = .false.
    ! -- set data decomposition
    call test_comio_decomp(ldx,ldy)
    ! -- read from file
    call io % open(filename, "r")
    call io % domain((/ DX, DY /), mstart, mcount)
    i = 0
    test_comio_mds_validate = .true.
    do while (test_comio_mds_validate .and. (i<5))
      test_comio_mds_validate = .false.
      i = i + 1
      ! -- read data
      write(dname,'(a,i0)') trim(name), i
      ddata = 0.d0
      call io % read(dname, ddata)
      ! -- validate data
      test_comio_mds_validate = all(ddata == i*(2.d0*prank+1.d0))
      if (.not.test_comio_mds_validate) exit
    end do
    call io % close()
  end function test_comio_mds_validate

end module test_comio_mod
