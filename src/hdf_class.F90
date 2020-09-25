module hdf_class

#ifdef HAVE_HDF5

  use comio_class
  use hdf5
  use io_prec
  use iso_c_binding

  implicit none

  type, extends(COMIO_T) :: HDF5_IO_T

    private
    integer(HID_T) :: dset_id       = -1
    integer(HID_T) :: file_id       = -1
    integer(HID_T) :: grp_id        = -1
    integer(HID_T) :: dcrt_plist_id = -1
    integer(HID_T) :: facc_plist_id = -1
    integer(HID_T) :: fcrt_plist_id = -1
    integer(HID_T) :: xfer_plist_id = -1
    integer(HID_T) :: filespace     = -1
    integer(HID_T) :: memspace      = -1

    integer(HID_T) :: dtype_id            = -1
    integer(HID_T) :: fspace_dtype        = -1
    integer(HID_T) :: fspace_dtype_int    = -1
    integer(HID_T) :: fspace_dtype_float  = -1
    integer(HID_T) :: fspace_dtype_double = -1

    integer(HSIZE_T), dimension(:), pointer :: fdims  => null()
    integer(HSIZE_T), dimension(:), pointer :: mstart => null()
    integer(HSIZE_T), dimension(:), pointer :: mcount => null()

    integer :: mpio_xfer_mode = -1
    logical :: paused         = .false.

  contains

    procedure :: io_initialize
    procedure :: io_finalize

    procedure :: io_file_open
    procedure :: io_file_close
    procedure :: io_domain_set
    procedure :: io_pause

    procedure :: io_datafill
    procedure :: io_datatype

    procedure :: io_dataset_get_dims
    procedure :: io_dataset_read_1d_int, &
                 io_dataset_read_2d_int, &
                 io_dataset_read_3d_int, &
                 io_dataset_read_1d_sp,  &
                 io_dataset_read_2d_sp,  &
                 io_dataset_read_3d_sp,  &
                 io_dataset_read_1d_dp,  &
                 io_dataset_read_2d_dp,  &
                 io_dataset_read_3d_dp
    procedure :: io_dataset_write_1d_int, &
                 io_dataset_write_2d_int, &
                 io_dataset_write_3d_int, &
                 io_dataset_write_1d_sp,  &
                 io_dataset_write_2d_sp,  &
                 io_dataset_write_3d_sp,  &
                 io_dataset_write_1d_dp,  &
                 io_dataset_write_2d_dp,  &
                 io_dataset_write_3d_dp
    procedure :: io_describe_string,         &
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
    procedure :: io_description_string,         &
                 io_description_int,            &
                 io_description_sp,             &
                 io_description_dp,             &
                 io_description_1d_int,         &
                 io_description_1d_sp,          &
                 io_description_1d_dp,          &
                 io_dataset_description_string, &
                 io_dataset_description_int,    &
                 io_dataset_description_sp,     &
                 io_dataset_description_dp,     &
                 io_dataset_description_1d_int, &
                 io_dataset_description_1d_sp,  &
                 io_dataset_description_1d_dp

    procedure :: fs_itype_get => io_filespace_int_datatype_get
    procedure :: fs_ftype_get => io_filespace_fp_datatype_get
    procedure :: ms_ftype_get => io_memspace_fp_datatype_get
    procedure :: ms_itype_get => io_memspace_int_datatype_get

  end type HDF5_IO_T

  private

  public :: HDF5_IO_T

contains

  ! -- Initialize/Finalize APIs

  subroutine io_initialize(io, comm, info)
    class(HDF5_IO_T)               :: io
    integer, optional, intent(in)  :: comm
    integer, optional, intent(in)  :: info

    ! -- HDF5 object identifiers
    io % dset_id       = -1
    io % file_id       = -1
    io % filespace     = H5S_ALL_F
    io % memspace      = H5S_ALL_F
    io % dcrt_plist_id = H5P_DEFAULT_F
    io % facc_plist_id = H5P_DEFAULT_F
    io % fcrt_plist_id = H5P_DEFAULT_F
    io % xfer_plist_id = H5P_DEFAULT_F

    ! -- global and local data decompositions
    io % fdims  => null()
    io % mstart => null()
    io % mcount => null()

    ! -- HDF5 parallel I/O
    ! -- data transfer mode
    io % mpio_xfer_mode = H5FD_MPIO_INDEPENDENT_F
    ! -- individual process control
    io % paused         = .false.

    ! -- HDF5 error handling
    io % err % success  =  0
    io % err % failure  = -1
    io % err % message  = "HDF5 I/O failure"
    io % err % srcfile  = __FILE__

    ! -- initialize HDF5
    call h5open_f(io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- default HDF5 dataset types (types are a
    ! -- NOTE: types are available after initialization
    io % dtype_id            = -1
    io % fspace_dtype_int    = H5T_STD_I32LE
    io % fspace_dtype_float  = H5T_IEEE_F32LE
    io % fspace_dtype_double = H5T_IEEE_F64LE

    if (present(comm) .and. present(info)) then
      ! -- create file property list to enable parallel (MPI) I/O
      call h5pcreate_f(H5P_FILE_ACCESS_F, io % facc_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      call h5pset_fapl_mpio_f(io % facc_plist_id, comm, info, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      ! -- create property for parallel (MPI) I/O raw data transfer
      call h5pcreate_f(H5P_DATASET_XFER_F, io % xfer_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      call h5pset_dxpl_mpio_f(io % xfer_plist_id, io % mpio_xfer_mode, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    ! -- do not track time for created datasets to allow for b4b comparison
    call h5pcreate_f(H5P_DATASET_CREATE_F, io % dcrt_plist_id, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    call h5pset_obj_track_times_f(io % dcrt_plist_id, .false., io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_initialize

  subroutine io_finalize(io)
    class(HDF5_IO_T) :: io

    ! -- close property lists
    if (io % dcrt_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % dcrt_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % dcrt_plist_id = H5P_DEFAULT_F
    end if

    if (io % facc_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % facc_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % facc_plist_id = H5P_DEFAULT_F
    end if

    if (io % fcrt_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % fcrt_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % fcrt_plist_id = H5P_DEFAULT_F
    end if

    if (io % xfer_plist_id /= H5P_DEFAULT_F) then
      call h5pclose_f(io % xfer_plist_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % xfer_plist_id = H5P_DEFAULT_F
    end if

    ! -- close dataset in file space (global)
    if (io % filespace /= H5S_ALL_F) then
      call h5sclose_f(io % filespace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % filespace = H5S_ALL_F
    end if

    ! -- close dataset in memory space (local)
    if (io % memspace /= H5S_ALL_F) then
      call h5sclose_f(io % memspace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % memspace = H5S_ALL_F
    end if

    ! -- shut down HDF5
    call io_shutdown(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_finalize

  subroutine io_shutdown(io)
    class(HDF5_IO_T) :: io

    ! -- close open file
    call io_file_close(io)
    if (io % err % check(line=__LINE__)) return

    ! -- close HDF5
    call h5close_f(io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_shutdown

  ! -- File access APIs

  subroutine io_file_open(io, filename, mode)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: mode

    select case (mode)
      case ("c", "c+", "C", "C+")
        call h5fcreate_f(filename, H5F_ACC_TRUNC_F, io % file_id, &
          io % err % rc, creation_prp = io % fcrt_plist_id,          &
          access_prp = io % facc_plist_id)
        if (io % err % check(line=__LINE__)) return
      case ("w", "W")
        call h5fopen_f(filename, H5F_ACC_RDWR_F, io % file_id, &
          io % err % rc, access_prp = io % facc_plist_id)
        if (io % err % check(line=__LINE__)) return
      case ("r", "R")
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, io % file_id, &
          io % err % rc, access_prp = io % facc_plist_id)
        if (io % err % check(line=__LINE__)) return
    end select

  end subroutine io_file_open

  subroutine io_file_close(io)
    class(HDF5_IO_T) :: io

    integer         :: obj
    integer(SIZE_T) :: obj_count
    integer(HID_T), allocatable :: obj_ids(:)

    if (io % file_id /= -1) then

      ! -- close datasets
      call h5fget_obj_count_f(io % file_id, H5F_OBJ_DATASET_F, obj_count, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      allocate(obj_ids(obj_count), stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return

      call h5fget_obj_ids_f(io % file_id, H5F_OBJ_DATASET_F, obj_count, obj_ids, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      do obj = 1, obj_count
        call h5dclose_f(obj_ids(obj), io % err % rc)
        if (io % err % check(msg="Unable to close dataset", line=__LINE__)) return
      end do

      deallocate(obj_ids, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return

      ! -- close groups
      call h5fget_obj_count_f(io % file_id, H5F_OBJ_GROUP_F, obj_count, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      allocate(obj_ids(obj_count), stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return

      call h5fget_obj_ids_f(io % file_id, H5F_OBJ_GROUP_F, obj_count, obj_ids, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      do obj = 1, size(obj_ids)
        call h5gclose_f(obj_ids(obj), io % err % rc)
        if (io % err % check(msg="Unable to close group", line=__LINE__)) return
      end do

      deallocate(obj_ids, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return

      call h5fclose_f(io % file_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return

      io % file_id = -1

    end if

    ! -- free up data decomposition memory
    call io_domain_clear(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_file_close

  ! -- Data layout APIs

  subroutine io_domain_clear(io)
    class(HDF5_IO_T) :: io

    ! -- free up data decomposition memory
    if (associated(io % fdims)) then
      deallocate(io % fdims, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % fdims)
    end if
    if (associated(io % mstart)) then
      deallocate(io % mstart, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % mstart)
    end if
    if (associated(io % mcount)) then
      deallocate(io % mcount, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free memory")) return
      nullify(io % mcount)
    end if

    ! -- release and terminate access to associated dataspace
    if (io % filespace /= H5S_ALL_F) then
      call h5sclose_f(io % filespace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      io % filespace = H5S_ALL_F
    end if

  end subroutine io_domain_clear

  subroutine io_domain_set(io, fdims, mstart, mcount)
    class(HDF5_IO_T)                  :: io
    integer, dimension(:), intent(in) :: fdims
    integer, dimension(:), intent(in) :: mstart
    integer, dimension(:), intent(in) :: mcount

    ! -- perform sanity check on input
    if (io % err % check(any(fdims < 0), line=__LINE__, &
      msg="Invalid argument: fdims < 0")) return

    if (io % err % check(any(mstart < 1), line=__LINE__, &
      msg="Invalid argument: mstart < 0")) return

    if (io % err % check(any(mcount < 0), line=__LINE__, &
      msg="Invalid argument: mcount < 0")) return

    if (io % err % check(any(mstart + mcount - 1 > fdims), &
      line=__LINE__, &
      msg="Invalid argument: Inconsistent data decomposition")) return

    ! -- clear existing domain decomposition
    call io_domain_clear(io)
    if (io % err % check(line=__LINE__, msg="Unable to clear existing decomposition")) return

    ! -- set dataset global dimensions
    allocate(io % fdims(size(fdims)), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return
    io % fdims = fdims

    ! -- set dataset local dimensions
    allocate(io % mcount(size(mcount)), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return
    io % mcount = mcount

    ! -- set dataset local offsets
    allocate(io % mstart(size(mstart)), stat = io % err % rc)
    if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return
    io % mstart = mstart - 1

    ! -- replace dataset space on file (global)
    if (io % filespace /= H5S_ALL_F) then
      call h5sclose_f(io % filespace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    call h5screate_simple_f(size(io % fdims), io % fdims, &
      io % filespace, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- replace dataset in memory space (local)
    if (io % memspace /= H5S_ALL_F) then
      call h5sclose_f(io % memspace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

    call h5screate_simple_f(size(io % mcount), io % mcount, &
      io % memspace, io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_domain_set

  ! -- Groups APIs

  logical function io_group_inquire(io, grpname, create)
    class(HDF5_IO_T)              :: io
    character(len=*),  intent(in) :: grpname
    logical, optional, intent(in) :: create

    integer :: rc
    integer :: ib, ie
    logical :: grp_create, grp_exists

    io_group_inquire = .false.

    grp_create = .false.
    if (present(create)) grp_create = create

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- traverse path to check for each intermediate group
      ib = 1
      ie = index(grpname(ib:), "/")
      grp_exists = .true.
      do while ((ie >= ib) .and. grp_exists)
        ! -- check if group exists
        call h5lexists_f(io % file_id, grpname(1:ie), &
          grp_exists, rc)
        if (.not.grp_exists .and. grp_create) then
          ! -- create group if requested
          call h5gcreate_f(io % file_id, grpname(1:ie), &
            io % grp_id, io % err % rc)
          if (io % err % check(line=__LINE__)) return
          grp_exists = .true.
        end if
        ib = ie + 1
        ie = index(grpname(ib:), "/") + ib - 1
      end do
      io_group_inquire = grp_exists
    end if

  end function io_group_inquire

  subroutine io_group_create(io, grpname)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: grpname

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- create each intermediate group if needed
      if (io % err % check( &
        .not.io_group_inquire(io, grpname, create=.true.), &
        msg="Unable to create group "//trim(grpname), &
        line=__LINE__)) return
    end if

  end subroutine io_group_create

  ! -- Datasets APIs

  logical function io_dataset_inquire(io, dsetname)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname

    integer :: rc

    io_dataset_inquire = .false.

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- dataset exists if link resolves to actual object
      ! -- HDF5 requires us to traverse the path explicitly
      ! -- thus we check for each intermediate group first
      if (io_group_inquire(io, dsetname)) then
        ! -- check if object exists
        call h5lexists_f(io % file_id, dsetname, &
          io_dataset_inquire, rc)
      end if
    end if

  end function io_dataset_inquire

  subroutine io_dataset_create(io, dsetname, dsettype, fdims, mstart, mcount)
    class(HDF5_IO_T)              :: io
    character(len=*),  intent(in) :: dsetname
    integer(HID_T),    intent(in) :: dsettype
    integer, optional, intent(in) :: fdims(:)
    integer, optional, intent(in) :: mstart(:)
    integer, optional, intent(in) :: mcount(:)

    integer(HID_T) :: plist_id

    ! -- save dataset global dimensions
    if (present(fdims) .and. present(mstart) .and. present(mcount)) then
      call io_domain_set(io, fdims, mstart, mcount)
      if (io % err % check(line=__LINE__)) return
    end if

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call io_dataset_open(io, dsetname)
      if (io % err % check(line=__LINE__)) return
    else
      ! -- create associated group if it does not exist
      call io_group_create(io, dsetname)
      if (io % err % check(line=__LINE__)) return
      ! -- select hyperslab for data I/O
      call h5sselect_hyperslab_f(io % filespace, H5S_SELECT_SET_F, &
        io % mstart, io % mcount, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- create dataset
      call h5dcreate_f(io % file_id, dsetname, dsettype, io % filespace, &
        io % dset_id, io % err % rc, dcpl_id = io % dcrt_plist_id)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_create

  subroutine io_dataset_open(io, dsetname)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname

    ! -- open dataset on file
    call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- get filespace from dataset
    call h5dget_space_f(io % dset_id, io % filespace, io % err % rc)
    if (io % err % check(line=__LINE__)) return

    ! -- set local portion of dataset as hyperslab
    call h5sselect_hyperslab_f(io % filespace, H5S_SELECT_SET_F, &
      io % mstart, io % mcount, io % err % rc)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_open

  subroutine io_dataset_close(io)
    class(HDF5_IO_T) :: io

    if (io % dset_id /= -1) then
      call h5dclose_f(io % dset_id, io % err % rc)
      if (io % err % check(msg="Unable to close dataset", line=__LINE__)) return
      io % dset_id = -1
    end if

  end subroutine io_dataset_close

  subroutine io_dataset_get_dims(io, dsetname, dims)
    class(HDF5_IO_T)              :: io
    character(len=*),  intent(in) :: dsetname
    integer,           pointer    :: dims(:)

    integer :: ndims, rc
    integer(HSIZE_T), dimension(:), allocatable :: dset_dims, dset_maxdims

    ! -- check if pointer argument is associated
    if (associated(dims)) then
      call io % err % set(msg="Pointer argument must not be associated", line=__LINE__)
      return
    end if

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get dataspace
      call h5dget_space_f(io % dset_id, io % filespace, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get dimensions
      call h5sget_simple_extent_ndims_f(io % filespace, ndims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- allocate output and work arrays
      allocate(dims(ndims), dset_dims(ndims), dset_maxdims(ndims), stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to allocate memory")) return
      ! -- retrieve global dimensions
      dims = 0
      dset_dims = 0
      dset_maxdims = 0
      call h5sget_simple_extent_dims_f(io % filespace, dset_dims, dset_maxdims, rc)
      if (rc == io % err % failure) io % err % rc = rc
      if (io % err % check(line=__LINE__)) return
      if (rc /= ndims) then
        call io % err % set(msg="Dataspace rank mismatch", line=__LINE__)
        return
      end if
      ! -- convert to integer
      dims = dset_dims
      ! -- free up memory
      deallocate(dset_dims, dset_maxdims, stat = io % err % rc)
      if (io % err % check(line=__LINE__, msg="Unable to free up memory")) return
      ! -- close dataset and release dataspace
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return

    end if

  end subroutine io_dataset_get_dims

  ! -- Basic I/O APIs

  ! -- pause I/O on local task
  subroutine io_pause(io, flag)
    class(HDF5_IO_T)    :: io
    logical, intent(in) :: flag

    io % paused = flag
  end subroutine io_pause

  ! -- set write data fill value or mode
  subroutine io_datafill(io, dvalue)
    class(HDF5_IO_T)               :: io
    class(*), optional, intent(in) :: dvalue

    if (present(dvalue)) then
      select type (dvalue)
        type is (logical)
          if (.not.dvalue) then
            call h5pset_fill_value_f(io % dcrt_plist_id, io % ms_itype_get(kind(1)), &
              H5D_FILL_VALUE_UNDEFINED_F, io % err % rc)
            if (io % err % check(line=__LINE__)) return
          end if
        type is (integer)
          call h5pset_fill_value_f(io % dcrt_plist_id, io % ms_itype_get(kind(1)), &
            dvalue, io % err % rc)
          if (io % err % check(line=__LINE__)) return
        type is (real(sp))
          call h5pset_fill_value_f(io % dcrt_plist_id, io % ms_ftype_get(sp), &
            dvalue, io % err % rc)
          if (io % err % check(line=__LINE__)) return
        type is (real(dp))
          call h5pset_fill_value_f(io % dcrt_plist_id, io % ms_ftype_get(dp), &
            dvalue, io % err % rc)
          if (io % err % check(line=__LINE__)) return
        class default
          call io % err % set(msg="Datatype unknown", line=__LINE__)
          return
      end select
    end if

  end subroutine io_datafill

  ! -- set write data type for automatic conversion
  subroutine io_datatype(io, dtype)
    class(HDF5_IO_T)               :: io
    class(*), optional, intent(in) :: dtype

    io % fspace_dtype = -1

    if (present(dtype)) then
      select type (dtype)
        type is (integer)
          io % fspace_dtype = io % fs_itype_get(kind(1))
        type is (real(sp))
          io % fspace_dtype = io % fs_ftype_get(sp)
        type is (real(dp))
          io % fspace_dtype = io % fs_ftype_get(dp)
        class default
          call io % err % set(msg="Datatype unknown", line=__LINE__)
          return
      end select
    end if

  end subroutine io_datatype

  ! -- read: integers
  ! --  * 1D arrays
  subroutine io_read_1d_int(io, buffer)
    class(HDF5_IO_T)     :: io
    integer, intent(out) :: buffer(:)

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    buffer = 0
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_1d_int

  ! --  * 2D, integer
  subroutine io_read_2d_int(io, buffer)
    class(HDF5_IO_T)     :: io
    integer, intent(out) :: buffer(:,:)

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    buffer = 0
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_2d_int

  ! --  * 3D, integer
  subroutine io_read_3d_int(io, buffer)
    class(HDF5_IO_T)     :: io
    integer, intent(out) :: buffer(:,:,:)

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    buffer = 0
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_3d_int

  ! -- read: floating point
  ! --  * 1D arrays
  subroutine io_read_1d_sp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(sp), intent(out) :: buffer(:)

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._sp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_1d_sp

  ! --  * 2D arrays
  subroutine io_read_2d_sp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(sp), intent(out) :: buffer(:,:)

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._sp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_2d_sp

  ! --  * 3D arrays
  subroutine io_read_3d_sp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(sp), intent(out) :: buffer(:,:,:)

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._sp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_3d_sp

  ! -- read: double
  ! --  * 1D arrays
  subroutine io_read_1d_dp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(dp), intent(out) :: buffer(:)

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._dp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_1d_dp

  ! --  * 2D arrays
  subroutine io_read_2d_dp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(dp), intent(out) :: buffer(:,:)

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._dp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_2d_dp

  ! --  * 3D arrays
  subroutine io_read_3d_dp(io, buffer)
    class(HDF5_IO_T)      :: io
    real(dp), intent(out) :: buffer(:,:,:)

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    buffer = 0._dp
    call h5dread_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_read_3d_dp

  ! -- write: integers
  ! --  * 1D arrays
  subroutine io_write_1d_int(io, buffer)
    class(HDF5_IO_T)       :: io
    integer, intent(inout) :: buffer(:)

    if (io % paused) return

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_1d_int

  ! --  * 2D arrays
  subroutine io_write_2d_int(io, buffer)
    class(HDF5_IO_T)       :: io
    integer, intent(inout) :: buffer(:,:)

    if (io % paused) return

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_2d_int

  ! --  * 3D arrays
  subroutine io_write_3d_int(io, buffer)
    class(HDF5_IO_T)       :: io
    integer, intent(inout) :: buffer(:,:,:)

    if (io % paused) return

    io % dtype_id = io % ms_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_3d_int

  ! -- floating point
  ! --  * 1D arrays
  subroutine io_write_1d_sp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(sp), intent(inout) :: buffer(:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_1d_sp

  ! --  * 2D arrays
  subroutine io_write_2d_sp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(sp), intent(inout) :: buffer(:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_2d_sp

  ! --  * 3D arrays
  subroutine io_write_3d_sp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(sp), intent(inout) :: buffer(:,:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_3d_sp

  ! -- double
  ! --  * 1D arrays
  subroutine io_write_1d_dp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(dp), intent(inout) :: buffer(:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_1d_dp

  ! --  * 2D arrays
  subroutine io_write_2d_dp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(dp), intent(inout) :: buffer(:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_2d_dp

  ! --  * 3D arrays
  subroutine io_write_3d_dp(io, buffer)
    class(HDF5_IO_T)        :: io
    real(dp), intent(inout) :: buffer(:,:,:)

    if (io % paused) return

    io % dtype_id = io % ms_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call h5dwrite_f(io % dset_id, io % dtype_id, buffer, io % mcount, io % err % rc, &
      file_space_id = io % filespace, mem_space_id = io % memspace, &
      xfer_prp = io % xfer_plist_id)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_write_3d_dp

  ! -- Dataset I/O APIs

  ! -- reading:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_read_1d_int(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_int

  ! -- * 2D
  subroutine io_dataset_read_2d_int(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_int

  ! -- * 3D
  subroutine io_dataset_read_3d_int(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    integer,          intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_int

  ! -- floating point
  ! -- * 1D
  subroutine io_dataset_read_1d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_sp

  ! -- * 2D
  subroutine io_dataset_read_2d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_sp

  ! -- * 3D
  subroutine io_dataset_read_3d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(sp),         intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_sp

  ! -- double
  ! -- * 1D
  subroutine io_dataset_read_1d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_1d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_1d_dp

  ! -- * 2D
  subroutine io_dataset_read_2d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_2d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_2d_dp

  ! -- * 3D
  subroutine io_dataset_read_3d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    real(dp),         intent(out) :: buffer(:,:,:)

    call io_dataset_open(io, dsetname)
    if (io % err % check(line=__LINE__)) return

    call io_read_3d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_read_3d_dp

  ! -- writing:
  ! -- integer
  ! -- * 1D
  subroutine io_dataset_write_1d_int(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_int

  subroutine io_dataset_write_2d_int(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_int

  subroutine io_dataset_write_3d_int(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    integer,          intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_itype_get(kind(1))
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_int(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_int

  ! -- * 2D
  subroutine io_dataset_write_1d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_sp

  subroutine io_dataset_write_2d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_sp

  subroutine io_dataset_write_3d_sp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(sp),         intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_ftype_get(sp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_sp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_sp

  ! -- * 3D
  subroutine io_dataset_write_1d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_1d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_1d_dp

  subroutine io_dataset_write_2d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_2d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_2d_dp

  subroutine io_dataset_write_3d_dp(io, dsetname, buffer)
    class(HDF5_IO_T)                :: io
    character(len=*), intent(in)    :: dsetname
    real(dp),         intent(inout) :: buffer(:,:,:)

    io % dtype_id = io % fs_ftype_get(dp)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_create(io, dsetname, io % dtype_id)
    if (io % err % check(line=__LINE__)) return

    call io_write_3d_dp(io, buffer)
    if (io % err % check(line=__LINE__)) return

    call io_dataset_close(io)
    if (io % err % check(line=__LINE__)) return

  end subroutine io_dataset_write_3d_dp

  ! -- describing:
  ! -- global
  subroutine io_describe_string(io, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, atype_id
    integer(HSIZE_T) :: dims(1)
    type(C_PTR)      :: f_ptr
    character(len=len_trim(value)+1, kind=C_CHAR), target :: cvalue

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute datatype
      call h5tcopy_f(H5T_STRING, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute
      call h5acreate_f(io % file_id, key, atype_id, aspace_id, attr_id, &
        io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      cvalue = trim(value) // C_NULL_CHAR
      f_ptr  = C_LOC(C_LOC(cvalue))
      call h5awrite_f(attr_id, atype_id, f_ptr, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_describe_string

  subroutine io_describe_int(io, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: key
    integer,          intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- get attribute type id
      type_id = io % fs_itype_get(kind(1))
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % file_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_describe_int

  subroutine io_describe_1d_int(io, key, values)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: key
    integer,          intent(in) :: values(:)

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- get attribute type id
      type_id = io % fs_itype_get(kind(1))
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = size(values)
      call h5screate_simple_f(1, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % file_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_describe_1d_int

  subroutine io_describe_sp(io, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- get attribute type id
      type_id = io % fs_ftype_get(sp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % file_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_describe_sp

  subroutine io_describe_1d_sp(io, key, values)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: values(:)

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- get attribute type id
      type_id = io % fs_ftype_get(sp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = size(values)
      call h5screate_simple_f(1, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % file_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_describe_1d_sp

  subroutine io_describe_dp(io, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- get attribute type id
      type_id = io % fs_ftype_get(dp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % file_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_describe_dp

  subroutine io_describe_1d_dp(io, key, values)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: values(:)

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- get attribute type id
      type_id = io % fs_ftype_get(dp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = size(values)
      call h5screate_simple_f(1, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % file_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      dims = size(values)
      call h5awrite_f(attr_id, type_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_describe_1d_dp

  ! -- dataset

  subroutine io_dataset_describe_string(io, dsetname, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, atype_id
    integer(HSIZE_T) :: dims(1)
    type(C_PTR)      :: f_ptr
    character(len=len_trim(value)+1, kind=C_CHAR), target :: cvalue

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute datatype
      call h5tcopy_f(H5T_STRING, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute
      call h5acreate_f(io % dset_id, key, atype_id, aspace_id, attr_id, &
                       io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      cvalue = trim(value) // C_NULL_CHAR
      f_ptr  = C_LOC(C_LOC(cvalue))
      call h5awrite_f(attr_id, atype_id, f_ptr, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_describe_string

  subroutine io_dataset_describe_int(io, dsetname, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    integer,          intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type id
      type_id = io % fs_itype_get(kind(1))
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % dset_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_describe_int

  subroutine io_dataset_describe_sp(io, dsetname, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type id
      type_id = io % fs_ftype_get(sp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % dset_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_describe_sp

  subroutine io_dataset_describe_dp(io, dsetname, key, value)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: value

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type id
      type_id = io % fs_ftype_get(dp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = 0
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % dset_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_describe_dp

  subroutine io_dataset_describe_1d_int(io, dsetname, key, values)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    integer,          intent(in) :: values(:)

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type id
      type_id = io % fs_itype_get(kind(1))
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = size(values)
      call h5screate_simple_f(1, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % dset_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_describe_1d_int

  subroutine io_dataset_describe_1d_sp(io, dsetname, key, values)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(sp),         intent(in) :: values(:)

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type id
      type_id = io % fs_ftype_get(sp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = size(values)
      call h5screate_simple_f(1, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % dset_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_describe_1d_sp

  subroutine io_dataset_describe_1d_dp(io, dsetname, key, values)
    class(HDF5_IO_T)             :: io
    character(len=*), intent(in) :: dsetname
    character(len=*), intent(in) :: key
    real(dp),         intent(in) :: values(:)

    integer(HID_T)   :: aspace_id, attr_id, type_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type id
      type_id = io % fs_ftype_get(dp)
      if (io % err % check(line=__LINE__)) return
      ! -- create attribute dataspace
      dims = size(values)
      call h5screate_simple_f(0, dims, aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- open dataset if present
      call h5acreate_f(io % dset_id, key, type_id, aspace_id, &
        attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- write attribute to file
      call h5awrite_f(attr_id, type_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release dataspace id
      call h5sclose_f(aspace_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_describe_1d_dp

  ! -- get description:
  ! -- global

  subroutine io_description_string(io, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: key
    character(len=*), intent(out) :: value

    integer             :: idx
    integer(HID_T)      :: attr_id
    type(C_PTR)         :: f_ptr
    type(C_PTR), target :: rdata
    character(len=len(value)+1), pointer :: data

    ! -- check if file is open
    if (io % file_id /= -1) then
      ! -- get attribute id
      call h5aopen_f(io % file_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      f_ptr = C_LOC(rdata)
      call h5aread_f(attr_id, H5T_STRING, f_ptr, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- copy attribute string to output string
      call c_f_pointer(rdata, data)
      idx = index(data, C_NULL_CHAR) - 1
      value = data(1:idx)
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_description_string

  subroutine io_description_int(io, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: key
    integer,          intent(out) :: value

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      dims = 0
      ! -- get attribute id
      call h5aopen_f(io % file_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_description_int

  subroutine io_description_1d_int(io, key, values)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: key
    integer,          intent(out) :: values(:)

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      dims = size(values)
      ! -- get attribute id
      call h5aopen_f(io % file_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_description_1d_int

  subroutine io_description_sp(io, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: key
    real(sp),         intent(out) :: value

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      dims = 0
      ! -- get attribute id
      call h5aopen_f(io % file_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_description_sp

  subroutine io_description_1d_sp(io, key, values)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: key
    real(sp),         intent(out) :: values(:)

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      dims = size(values)
      ! -- get attribute id
      call h5aopen_f(io % file_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_description_1d_sp

  subroutine io_description_dp(io, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: key
    real(dp),         intent(out) :: value

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      dims = 0
      ! -- get attribute id
      call h5aopen_f(io % file_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_description_dp

  subroutine io_description_1d_dp(io, key, values)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: key
    real(dp),         intent(out) :: values(:)

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if file is open
    if (io % file_id /= -1) then
      dims = size(values)
      ! -- get attribute id
      call h5aopen_f(io % file_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_description_1d_dp

  ! -- dataset

  subroutine io_dataset_description_string(io, dsetname, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    character(len=*), intent(in)  :: key
    character(len=*), intent(out) :: value

    integer             :: idx
    integer(HID_T)      :: attr_id
    type(C_PTR)         :: f_ptr
    type(C_PTR), target :: rdata
    character(len=len(value)+1), pointer :: data

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute id
      call h5aopen_f(io % dset_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      f_ptr = C_LOC(rdata)
      call h5aread_f(attr_id, H5T_STRING, f_ptr, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- copy attribute string to output string
      call c_f_pointer(rdata, data)
      idx = index(data, C_NULL_CHAR) - 1
      value = data(1:idx)
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_description_string

  subroutine io_dataset_description_int(io, dsetname, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    character(len=*), intent(in)  :: key
    integer,          intent(out) :: value

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      dims = 0
      ! -- get attribute id
      call h5aopen_f(io % dset_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_description_int

  subroutine io_dataset_description_sp(io, dsetname, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    character(len=*), intent(in)  :: key
    real(sp),         intent(out) :: value

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      dims = 0
      ! -- get attribute id
      call h5aopen_f(io % dset_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_description_sp

  subroutine io_dataset_description_dp(io, dsetname, key, value)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    character(len=*), intent(in)  :: key
    real(dp),         intent(out) :: value

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      dims = 0
      ! -- get attribute id
      call h5aopen_f(io % dset_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, value, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_description_dp

  subroutine io_dataset_description_1d_int(io, dsetname, key, values)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    character(len=*), intent(in)  :: key
    integer,          intent(out) :: values(:)

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      dims = size(values)
      ! -- get attribute id
      call h5aopen_f(io % dset_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_description_1d_int

  subroutine io_dataset_description_1d_sp(io, dsetname, key, values)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    character(len=*), intent(in)  :: key
    real(sp),         intent(out) :: values(:)

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      dims = size(values)
      ! -- get attribute id
      call h5aopen_f(io % dset_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_description_1d_sp

  subroutine io_dataset_description_1d_dp(io, dsetname, key, values)
    class(HDF5_IO_T)              :: io
    character(len=*), intent(in)  :: dsetname
    character(len=*), intent(in)  :: key
    real(dp),         intent(out) :: values(:)

    integer(HID_T)   :: attr_id, atype_id
    integer(HSIZE_T) :: dims(1)

    ! -- check if dataset exists
    if (io_dataset_inquire(io, dsetname)) then
      ! -- open dataset if present
      call h5dopen_f(io % file_id, dsetname, io % dset_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      dims = size(values)
      ! -- get attribute id
      call h5aopen_f(io % dset_id, trim(key), attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute type
      call h5aget_type_f(attr_id, atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- get attribute
      call h5aread_f(attr_id, atype_id, values, dims, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release attribute id
      call h5aclose_f(attr_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- release type id
      call h5tclose_f(atype_id, io % err % rc)
      if (io % err % check(line=__LINE__)) return
      ! -- close dataset
      call io_dataset_close(io)
      if (io % err % check(line=__LINE__)) return
    end if

  end subroutine io_dataset_description_1d_dp

  ! -- Utilities

  ! -- Get Data Types
  integer(HID_T) function io_memspace_int_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

    io_memspace_int_datatype_get = 0
    if (datakind == kind(1)) then
      io_memspace_int_datatype_get = H5T_NATIVE_INTEGER
    else
      call io % err % set(msg="Unable to identify memory int data kind", line=__LINE__)
    end if

  end function io_memspace_int_datatype_get

  integer(HID_T) function io_memspace_fp_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

    io_memspace_fp_datatype_get = 0
    if (datakind == kind(1.)) then
      io_memspace_fp_datatype_get = H5T_NATIVE_REAL 
    else if (datakind == kind(1.d0)) then
      io_memspace_fp_datatype_get = H5T_NATIVE_DOUBLE
    else
      call io % err % set(msg="Unable to identify memory fp data kind", line=__LINE__)
    end if
    
  end function io_memspace_fp_datatype_get

  integer(HID_T) function io_filespace_int_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

    if (io % fspace_dtype /= -1) then
      io_filespace_int_datatype_get = io % fspace_dtype
      return
    end if

    io_filespace_int_datatype_get = 0
    if (datakind == kind(1)) then
      io_filespace_int_datatype_get = io % fspace_dtype_int
    else
      call io % err % set(msg="Unable to identify file data kind", line=__LINE__)
    end if
    
  end function io_filespace_int_datatype_get

  integer(HID_T) function io_filespace_fp_datatype_get(io, datakind)
    class(HDF5_IO_T)    :: io
    integer, intent(in) :: datakind

    if (io % fspace_dtype /= -1) then
      io_filespace_fp_datatype_get = io % fspace_dtype
      return
    end if

    io_filespace_fp_datatype_get = 0
    if (datakind == kind(1.)) then
      io_filespace_fp_datatype_get = io % fspace_dtype_float
    else if (datakind == kind(1.d0)) then
      io_filespace_fp_datatype_get = io % fspace_dtype_double
    else
      call io % err % set(msg="Unable to identify file data kind", line=__LINE__)
    end if
    
  end function io_filespace_fp_datatype_get

#endif
end module hdf_class
