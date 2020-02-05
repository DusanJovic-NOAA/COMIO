module comio

  use comio_class
  use hdf_class
  use pnc_class
  
  implicit none

  ! -- format selector
  integer, parameter :: COMIO_FMT_HDF5    = 1, &
                        COMIO_FMT_PNETCDF = 2

  interface COMIO_T
    module procedure comio_constructor
  end interface

  private
  public :: COMIO_T
  public :: COMIO_FMT_HDF5,   &
            COMIO_FMT_PNETCDF
  public :: comio_create

contains

  ! -- constructors

  function comio_constructor(fmt, comm, info) result (this)
    integer,           intent(in) :: fmt
    integer, optional, intent(in) :: comm
    integer, optional, intent(in) :: info
    ! -- local variables
    class(COMIO_T), pointer :: this
    type (IOERR_T)          :: err
    ! -- begin
    nullify(this)
    select case (fmt)
      case (COMIO_FMT_HDF5)
#ifdef HAVE_HDF5
        allocate(HDF5_IO_T::this, stat=err % rc)
        if (err % check(msg="Unable to create HDF5 I/O object")) return
#else
        call err % set(msg="COMIO was built without HDF5 support")
        return
#endif
      case (COMIO_FMT_PNETCDF)
#ifdef HAVE_PNETCDF
        allocate(PNC_IO_T::this)
#else
        call err % set(msg="COMIO was built without PnetCDF support")
        return
#endif
      case default
        call err % set(msg="COMIO: Unsupported I/O Format")
        return
    end select
    ! -- initialize I/O layer
    call this % startup(comm=comm, info=info)
    if (this % err % check(msg="Failed to initialize I/O")) return
  end function comio_constructor

  subroutine comio_create(this, fmt, comm, info)
    class(COMIO_T),   allocatable :: this
    integer,           intent(in) :: fmt
    integer, optional, intent(in) :: comm
    integer, optional, intent(in) :: info
    ! -- local variables
    type (IOERR_T) :: err
    ! -- begin
    select case (fmt)
      case (COMIO_FMT_HDF5)
#ifdef HAVE_HDF5
        allocate(HDF5_IO_T::this, stat=err % rc)
        if (err % check(msg="Unable to create HDF5 I/O object")) return
#else
        call err % set(msg="COMIO was built without HDF5 support")
        return
#endif
      case (COMIO_FMT_PNETCDF)
#ifdef HAVE_PNETCDF
        allocate(PNC_IO_T::this)
#else
        call err % set(msg="COMIO was built without PnetCDF support")
        return
#endif
      case default
        call err % set(msg="COMIO: Unsupported I/O Format")
        return
    end select
    ! -- initialize I/O layer
    call this % startup(comm=comm, info=info)
    if (this % err % check(msg="Failed to initialize I/O")) return
  end subroutine comio_create
  
end module comio
