module comio_class

  use ioerr_class
  use io_prec

  implicit none

  type, abstract :: COMIO_T

    private
    integer :: fmt

    type(IOERR_T), public :: err

  contains

    ! -- Public APIs
    generic :: startup  => io_initialize
    generic :: shutdown => io_finalize
    generic :: open     => io_file_open
    generic :: close    => io_file_close
    generic :: domain   => io_domain_set
    generic :: pause    => io_pause
    generic :: read     => io_dataset_read_1d_int, &
                           io_dataset_read_2d_int, &
                           io_dataset_read_3d_int, &
                           io_dataset_read_1d_sp,  &
                           io_dataset_read_2d_sp,  &
                           io_dataset_read_3d_sp,  &
                           io_dataset_read_1d_dp,  &
                           io_dataset_read_2d_dp,  &
                           io_dataset_read_3d_dp
    generic :: write    => io_dataset_write_1d_int, &
                           io_dataset_write_2d_int, &
                           io_dataset_write_3d_int, &
                           io_dataset_write_1d_sp,  &
                           io_dataset_write_2d_sp,  &
                           io_dataset_write_3d_sp,  &
                           io_dataset_write_1d_dp,  &
                           io_dataset_write_2d_dp,  &
                           io_dataset_write_3d_dp
    generic :: describe => io_describe_string,         &
                           io_describe_int,            &
                           io_describe_sp,             &
                           io_describe_dp,             &
                           io_describe_1d_int,         &
                           io_describe_1d_sp,          &
                           io_describe_1d_dp,          &
                           io_dataset_describe_string, &
                           io_dataset_describe_int,    &
                           io_dataset_describe_sp,     &
                           io_dataset_describe_dp,     &
                           io_dataset_describe_1d_int, &
                           io_dataset_describe_1d_sp,  &
                           io_dataset_describe_1d_dp

    procedure :: version

    ! -- Deferred implementation
    procedure(io_initialize_if),              deferred :: io_initialize
    procedure(io_finalize_if  ),              deferred :: io_finalize
    procedure(io_file_open_if ),              deferred :: io_file_open
    procedure(io_file_close_if),              deferred :: io_file_close
    procedure(io_domain_set_if),              deferred :: io_domain_set
    procedure(io_pause_if     ),              deferred :: io_pause
    procedure(io_dataset_read_1d_int_if),     deferred :: io_dataset_read_1d_int
    procedure(io_dataset_read_2d_int_if),     deferred :: io_dataset_read_2d_int
    procedure(io_dataset_read_3d_int_if),     deferred :: io_dataset_read_3d_int
    procedure(io_dataset_read_1d_sp_if ),     deferred :: io_dataset_read_1d_sp
    procedure(io_dataset_read_2d_sp_if ),     deferred :: io_dataset_read_2d_sp
    procedure(io_dataset_read_3d_sp_if ),     deferred :: io_dataset_read_3d_sp
    procedure(io_dataset_read_1d_dp_if ),     deferred :: io_dataset_read_1d_dp
    procedure(io_dataset_read_2d_dp_if ),     deferred :: io_dataset_read_2d_dp
    procedure(io_dataset_read_3d_dp_if ),     deferred :: io_dataset_read_3d_dp
    procedure(io_dataset_write_1d_int_if),    deferred :: io_dataset_write_1d_int
    procedure(io_dataset_write_2d_int_if),    deferred :: io_dataset_write_2d_int
    procedure(io_dataset_write_3d_int_if),    deferred :: io_dataset_write_3d_int
    procedure(io_dataset_write_1d_sp_if ),    deferred :: io_dataset_write_1d_sp
    procedure(io_dataset_write_2d_sp_if ),    deferred :: io_dataset_write_2d_sp
    procedure(io_dataset_write_3d_sp_if ),    deferred :: io_dataset_write_3d_sp
    procedure(io_dataset_write_1d_dp_if ),    deferred :: io_dataset_write_1d_dp
    procedure(io_dataset_write_2d_dp_if ),    deferred :: io_dataset_write_2d_dp
    procedure(io_dataset_write_3d_dp_if ),    deferred :: io_dataset_write_3d_dp
    procedure(io_describe_string_if),         deferred :: io_describe_string
    procedure(io_describe_int_if   ),         deferred :: io_describe_int
    procedure(io_describe_sp_if    ),         deferred :: io_describe_sp
    procedure(io_describe_dp_if    ),         deferred :: io_describe_dp
    procedure(io_describe_1d_int_if),         deferred :: io_describe_1d_int
    procedure(io_describe_1d_sp_if ),         deferred :: io_describe_1d_sp
    procedure(io_describe_1d_dp_if ),         deferred :: io_describe_1d_dp
    procedure(io_dataset_describe_string_if), deferred :: io_dataset_describe_string
    procedure(io_dataset_describe_int_if   ), deferred :: io_dataset_describe_int
    procedure(io_dataset_describe_sp_if    ), deferred :: io_dataset_describe_sp
    procedure(io_dataset_describe_dp_if    ), deferred :: io_dataset_describe_dp
    procedure(io_dataset_describe_1d_int_if), deferred :: io_dataset_describe_1d_int
    procedure(io_dataset_describe_1d_sp_if ), deferred :: io_dataset_describe_1d_sp
    procedure(io_dataset_describe_1d_dp_if ), deferred :: io_dataset_describe_1d_dp

  end type

  private
  public :: COMIO_T, IOERR_T

  abstract interface
    ! -- Initialize/Finalize
    subroutine io_initialize_if(io, comm, info)
      import
      class(COMIO_T)                :: io
      integer, optional, intent(in) :: comm
      integer, optional, intent(in) :: info
    end subroutine
    subroutine io_finalize_if(io)
      import
      class(COMIO_T) :: io
    end subroutine
    ! -- File access APIs
    subroutine io_file_open_if(io, filename, mode)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: filename
      character(len=*), intent(in) :: mode
    end subroutine
    subroutine io_file_close_if(io)
      import
      class(COMIO_T) :: io
    end subroutine
    ! -- Data layout APIs
    subroutine io_domain_set_if(io, fdims, mstart, mcount)
      import
      class(COMIO_T)                    :: io
      integer, dimension(:), intent(in) :: fdims
      integer, dimension(:), intent(in) :: mstart
      integer, dimension(:), intent(in) :: mcount
    end subroutine
    ! -- Basic I/O APIs
    ! -- pause I/O on local task
    subroutine io_pause_if(io, flag)
      import
      class(COMIO_T)      :: io
      logical, intent(in) :: flag
    end subroutine
    ! -- Dataset I/O APIs
    ! -- reading:
    ! -- integer
    ! -- * 1D
    subroutine io_dataset_read_1d_int_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      integer,          intent(out) :: buffer(:)
    end subroutine
    ! -- * 2D
    subroutine io_dataset_read_2d_int_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      integer,          intent(out) :: buffer(:,:)
    end subroutine
    ! -- * 3D
    subroutine io_dataset_read_3d_int_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      integer,          intent(out) :: buffer(:,:,:)
    end subroutine
    ! -- floating point
    ! -- * 1D
    subroutine io_dataset_read_1d_sp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      real(sp),         intent(out) :: buffer(:)
    end subroutine
    ! -- * 2D
    subroutine io_dataset_read_2d_sp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      real(sp),         intent(out) :: buffer(:,:)
    end subroutine
    ! -- * 3D
    subroutine io_dataset_read_3d_sp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      real(sp),         intent(out) :: buffer(:,:,:)
    end subroutine
    ! -- double
    ! -- * 1D
    subroutine io_dataset_read_1d_dp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      real(dp),         intent(out) :: buffer(:)
    end subroutine
    ! -- * 2D
    subroutine io_dataset_read_2d_dp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      real(dp),         intent(out) :: buffer(:,:)
    end subroutine
    ! -- * 3D
    subroutine io_dataset_read_3d_dp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                :: io
      character(len=*), intent(in)  :: dsetname
      real(dp),         intent(out) :: buffer(:,:,:)
    end subroutine
    ! -- writing:
    ! -- integer
    ! -- * 1D
    subroutine io_dataset_write_1d_int_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      integer,          intent(inout) :: buffer(:)
    end subroutine
    subroutine io_dataset_write_2d_int_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      integer,          intent(inout) :: buffer(:,:)
    end subroutine
    subroutine io_dataset_write_3d_int_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      integer,          intent(inout) :: buffer(:,:,:)
    end subroutine
    ! -- * 2D
    subroutine io_dataset_write_1d_sp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      real(sp),         intent(inout) :: buffer(:)
    end subroutine
    subroutine io_dataset_write_2d_sp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      real(sp),         intent(inout) :: buffer(:,:)
    end subroutine
    subroutine io_dataset_write_3d_sp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      real(sp),         intent(inout) :: buffer(:,:,:)
    end subroutine
    ! -- * 3D
    subroutine io_dataset_write_1d_dp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      real(dp),         intent(inout) :: buffer(:)
    end subroutine
    subroutine io_dataset_write_2d_dp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      real(dp),         intent(inout) :: buffer(:,:)
    end subroutine
    subroutine io_dataset_write_3d_dp_if(io, dsetname, buffer)
      import
      class(COMIO_T)                  :: io
      character(len=*), intent(in)    :: dsetname
      real(dp),         intent(inout) :: buffer(:,:,:)
    end subroutine
    ! -- describing:
    ! -- global
    subroutine io_describe_string_if(io, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: value
    end subroutine
    subroutine io_describe_int_if(io, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: key
      integer,          intent(in) :: value
    end subroutine
    subroutine io_describe_sp_if(io, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: key
      real(sp),         intent(in) :: value
    end subroutine
    subroutine io_describe_dp_if(io, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: key
      real(dp),         intent(in) :: value
    end subroutine
    subroutine io_describe_1d_int_if(io, key, values)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: key
      integer,          intent(in) :: values(:)
    end subroutine
    subroutine io_describe_1d_sp_if(io, key, values)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: key
      real(sp),         intent(in) :: values(:)
    end subroutine
    subroutine io_describe_1d_dp_if(io, key, values)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: key
      real(dp),         intent(in) :: values(:)
    end subroutine
    ! -- dataset
    subroutine io_dataset_describe_string_if(io, dsetname, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: dsetname
      character(len=*), intent(in) :: key
      character(len=*), intent(in) :: value
    end subroutine
    subroutine io_dataset_describe_int_if(io, dsetname, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: dsetname
      character(len=*), intent(in) :: key
      integer,          intent(in) :: value
    end subroutine
    subroutine io_dataset_describe_sp_if(io, dsetname, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: dsetname
      character(len=*), intent(in) :: key
      real(sp),         intent(in) :: value
    end subroutine
    subroutine io_dataset_describe_dp_if(io, dsetname, key, value)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: dsetname
      character(len=*), intent(in) :: key
      real(dp),         intent(in) :: value
    end subroutine
    subroutine io_dataset_describe_1d_int_if(io, dsetname, key, values)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: dsetname
      character(len=*), intent(in) :: key
      integer,          intent(in) :: values(:)
    end subroutine
    subroutine io_dataset_describe_1d_sp_if(io, dsetname, key, values)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: dsetname
      character(len=*), intent(in) :: key
      real(sp),         intent(in) :: values(:)
    end subroutine
    subroutine io_dataset_describe_1d_dp_if(io, dsetname, key, values)
      import
      class(COMIO_T)               :: io
      character(len=*), intent(in) :: dsetname
      character(len=*), intent(in) :: key
      real(dp),         intent(in) :: values(:)
    end subroutine

  end interface

contains

  ! -- Versioning
  subroutine version(io)
    class(COMIO_T) :: io
    write(6,'("COMIO Version ",a)') PACKAGE_VERSION
    write(6,'("Supports:")')
#ifdef HAVE_HDF5
    write(6,'(" - HDF5 Version ",a)') HDF5_VERSION
#endif
#ifdef HAVE_PNETCDF
    write(6,'(" - PnetCDF Version ",a)') PNETCDF_VERSION
#endif
    write(6,'("Report bugs to ",a)') PACKAGE_BUGREPORT
  end subroutine version

end module comio_class
