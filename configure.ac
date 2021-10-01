#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([comio], [0.0.10], [ipe_devteam@noaa.gov])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_TESTDIR([tests])
AC_CONFIG_FILES([tests/run_comio], [chmod +x tests/run_comio])
AC_CONFIG_FILES([utils/comio-config], [chmod +x utils/comio-config])

AM_INIT_AUTOMAKE
AM_MAINTAINER_MODE

# Checks for programs.
AC_PROG_SED

# Looks for Fortran compiler
# Assume we are cross compiling by default to prevent issues on WCOSS-Cray
save_cross_compiling=${cross_compiling}
cross_compiling=yes
# Silence resulting warning
save_ac_tool_warned=${ac_tool_warned}
ac_tool_warned=yes
AC_PROG_CC([${CC} mpiicc mpicc mpicc_r mpcc_r mpcc mpixlc_r mpixlc mpipgcc mppgcc mpigcc mpgcc mpigcc_r mpgcc_r icc cc gcc])
AM_PROG_CC_C_O
AC_PROG_FC([${FC} mpiifort mpifort mpif90 mpixlf90_r mpxlf90_r mpipgf90 mpipgf95 mppgf90 mppgf95])

# Restore original cross compilation settings
cross_compiling=${save_cross_compiling}
ac_tool_warned=${save_ac_tool_warned}

# Checks if Fortran compiler supports MPI
AC_LANG_PUSH([Fortran])
AC_MSG_CHECKING([whether MPI is supported])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[dnl
       use mpi
       integer, parameter :: comm = MPI_COMM_WORLD
  ])],
  [AC_DEFINE([HAVE_MPI], [1])
   AC_MSG_RESULT(yes)],
  [dnl No MPI found. Disable compiler.
   FC=
   AC_MSG_RESULT(no)
   AC_MSG_ERROR([Cannot find a suitable MPI Fortran compiler.])]
)

AC_FC_PP_SRCEXT([F90])
AX_COMPILER_VENDOR

# Add configure options
# - debug
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug],[enable debug mode (default: no).])],
  [enable_debug=${enableval}], [enable_debug=no])

# Checks for debug/optimization options
AS_IF([test "x$enable_debug" = xyes],[
  AC_MSG_CHECKING([compiler for debug options])
  save_FCFLAGS=${FCFLAGS}
  FCFLAGS=`echo $FCFLAGS | $SED -e 's/-O@<:@0-9s@:>@*//g'`
  FCFLAGS=`echo $FCFLAGS | $SED -e 's/-g@<:@0-9@:>@*//g'`
  debug_opts=
  AS_CASE(["$ax_cv_fc_compiler_vendor"],
    [gnu|clang], [debug_opts="-Wall -Wno-unused -Wno-unused-dummy-argument -fbacktrace"],
    [ibm],       [debug_opts="-qcheck -qfullpath"],
    [intel],     [debug_opts="-traceback -check pointer,stack,uninit"],
    [portland],  [debug_opts="-traceback"],
    []            dnl no debug options available for other platforms
  )
  AS_IF([test "x$ac_cv_prog_fc_g" = xyes], [debug_opts="-g ${debug_opts}"], [])
  FCFLAGS="-O0 ${debug_opts} ${FCFLAGS}"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
    [],
    [FCFLAGS=${save_FCFLAGS}
     debug_opts="unknown. Please add debug flags to FCFLAGS"]
  )
  AC_MSG_RESULT([${debug_opts}])
  ], [dnl default optimization settings, if none present
  AC_MSG_CHECKING([compiler for optimization options])
  fcflags_noopt=
  perf_opts=
  for fc_flag in $FCFLAGS
  do
    AS_CASE([$fc_flag],
      [-O@<:@0-9s@:>@*|-Ofast], [perf_opts=$fc_flag], dnl find optimization settings
      [fcflags_noopt="$fcflags_noopt $fc_flag"]
    )
  done
  AS_IF([test "x$perf_opts" = x], [dnl add default optimization settings
    AS_CASE(["$ax_cv_fc_compiler_vendor"],
      [gnu|clang], [dnl
                    perf_opts="-O2"
                    AS_IF([test "x$enable_safe_fp" != xno],
                           [perf_opts="${perf_opts} -fno-unsafe-math-optimizations"],[])],
      [ibm],       [perf_opts="-O2"],
      [intel],     [dnl
                    perf_opts="-O2"
                    AS_IF([test "x$enable_safe_fp" != xno],
                          [perf_opts="${perf_opts} -fp-model source -ftz -align array64byte -xCORE-AVX2 -qno-opt-dynamic-align"],[])],
      [portland],  [dnl
                    perf_opts="-O2"
                    AS_IF([test "x$enable_safe_fp" != xno],
                          [perf_opts="${perf_opts} -Kieee"],[])],
      []            dnl no debug options available for other platforms
    )], [])
  AS_IF([test "x$perf_opts" = x], [dnl optization flags unknown
    perf_opts=unknown],
    [dnl test optimization flags
    save_FCFLAGS=${fcflags_noopt}
    FCFLAGS="${fcflags_noopt} ${perf_opts}"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
      [],
      [FCFLAGS=${save_FCFLAGS}
       perf_opts=unknown]
    )
    ]
  )
  AC_MSG_RESULT([$perf_opts])
  ]
)

# Checks for compiler characteristics
AC_FC_LINE_LENGTH([unlimited])
AC_FC_MODULE_EXTENSION
AC_FC_MODULE_FLAG

# Checks for MPI run command
AC_ARG_VAR(MPIRUN, [MPI run command for "make check", @<:@default: mpiexec -n 4@:>@])
if test "x${TESTMPIRUN}" = x ; then
   dnl if MPIRUN is unset, attempts to find the MPI run command
   AC_PATH_PROGS([MPIRUN], [mpiexec mpirun srun])
   if test "x${MPIRUN}" != x ; then
      MPIRUN+=" -n 4"
   fi
fi

# Checks for required libraries
AC_LANG_POP
ext_libs=""
# - Parallel HDF5
AX_LIB_HDF5([parallel])
AS_IF([test "x$with_hdf5" != xyes],
  [AC_MSG_RESULT([Unable to find parallel HDF5 library])],
  [ext_libs="${ext_libs} HDF5"
   AC_DEFINE_UNQUOTED([HDF5_VERSION], ["${HDF5_VERSION}"])
])

# - PnetCDF
AX_LIB_PNETCDF
AS_IF([test "x$with_pnetcdf" != xyes],
  [AC_MSG_RESULT([Unable to find parallel NetCDF library])],
  [ext_libs="${ext_libs} pnetcdf"
   AC_DEFINE_UNQUOTED([PNETCDF_VERSION], ["${PNETCDF_VERSION}"])
])

# At least one library is required
AS_IF([test "x$ext_libs" = x],
  [AC_MSG_FAILURE([
Neither HDF5 nor PnetCDF has been found.
At least one library is required to proceed.
])], [])

# Adds libraries include paths
AC_LANG_PUSH([Fortran])
fc_inc=
for incpath in ${HDF5_FFLAGS} ${PNETCDF_FFLAGS}
do
  incpath=`echo "${incpath}" | $SED -e's/^-@<:@A-Z,a-z@:>@*\//\//g'`
  fc_inc="${fc_inc}${FC_MODINC}${incpath} "
done

AC_MSG_CHECKING([whether include flags work])
FCFLAGS="${fc_inc}${FCFLAGS}"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
  [AC_MSG_RESULT([yes])],
  [dnl ESMF-derived include flags may be corrupted. Abort.
   AC_MSG_RESULT([no])
   AC_MSG_FAILURE([unrecognized include flags])]
)

# Checks whether libraries are compatible with Fortran compiler
for lib in ${ext_libs}
do
  AC_MSG_CHECKING([whether ${lib} works])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[dnl
         use ${lib}
    ])],
    [AC_MSG_RESULT(yes)],
    [dnl ESMF is incompatible with selected Fortran compiler or not found. Abort.
     AC_MSG_RESULT(no)
     AC_MSG_FAILURE([${lib} library incompatible or not found])]
  )
done

AC_LANG_POP

# Set linker flags
LIBS="${LIBS} ${HDF5_FLIBS} ${PNETCDF_LIBS}"
LDFLAGS="${LDFLAGS} ${HDF5_LDFLAGS} ${PNETCDF_LDFLAGS}"

# Set F77 flags
FFLAGS="${FFLAGS} ${FCFLAGS}"
AC_PROG_F77([${FC}])

# Checks for auxiliary build tools
AC_PROG_INSTALL
AC_PROG_RANLIB
AM_PROG_AR

# Set Automake conditionals for test suite
AM_CONDITIONAL([HAVE_HDF5],    [test x$with_hdf5 = xyes])
AM_CONDITIONAL([HAVE_PNETCDF], [test x$with_pnetcdf = xyes])

# Set comio-config flags
AC_SUBST([DEP_FC_INC],  [$fc_inc])
AC_SUBST([HAS_HDF5],    [$with_hdf5])
AC_SUBST([HAS_PNETCDF], [$with_pnetcdf])

# Output Makefiles
AC_CONFIG_FILES([Makefile
                 src/Makefile
                 tests/Makefile])
AC_OUTPUT


AS_ECHO("")
AS_ECHO("${PACKAGE_NAME} version ${PACKAGE_VERSION} successfully configured.")
AS_ECHO("")
AS_ECHO("Compiler settings:")
AS_ECHO("  CC      = ${CC}")
AS_ECHO("  CFLAGS  = ${CFLAGS}")
AS_ECHO("  FC      = ${FC}")
AS_ECHO("  F77     = ${F77}")
AS_ECHO("  FCFLAGS = ${FCFLAGS}")
AS_ECHO("  FFLAGS  = ${FCFLAGS}")
AS_ECHO("  LDFLAGS = ${LDFLAGS}")
AS_ECHO("  LIBS    = ${LIBS}")
AS_ECHO("")
AS_ECHO("I/O libraries available:")
AS_IF([test "x$with_hdf5" = xyes],[dnl
  AS_ECHO("* HDF5")
  AS_ECHO("  Version: ${HDF5_VERSION}")
  AS_ECHO("  Type   : ${HDF5_TYPE}")
  AS_ECHO("  FC     : ${HDF5_FC}")
  AS_ECHO("  FFLAGS : ${HDF5_FFLAGS}")
  AS_ECHO("  FLIBS  : ${HDF5_FLIBS}")
  AS_ECHO("  LDFLAGS: ${HDF5_LDFLAGS}")
])
  AS_IF([test "x$with_pnetcdf" = xyes],[dnl
  AS_ECHO("* parallel-netcdf")
  AS_ECHO("  Version: ${PNETCDF_VERSION}")
  AS_ECHO("  FC     : ${PNETCDF_FC}")
  AS_ECHO("  FFLAGS : ${PNETCDF_FFLAGS}")
  AS_ECHO("  FLIBS  : ${PNETCDF_FLIBS}")
  AS_ECHO("  LDFLAGS: ${PNETCDF_LDFLAGS}")
])
AS_ECHO("")
AS_ECHO("Test environment:")
AS_ECHO("  MPIRUN = ${MPIRUN}")
AS_ECHO("")
AS_ECHO(["Type 'make' to build, then 'make check' to build and run unit tests."])
