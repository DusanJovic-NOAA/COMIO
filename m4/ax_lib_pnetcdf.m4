# ===========================================================================
#      https://www.gnu.org/software/autoconf-archive/ax_lib_pnetcdf.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_LIB_PNETCDF()
#
# DESCRIPTION
#
#   This macro provides tests of the availability of the PnetCDF library.
#
#   The macro adds a --with-pnetcdf option accepting one of three values:
#
#     no   - do not check for the PnetCDF library.
#     yes  - do check for PnetCDF library in standard locations.
#     path - installation prefix for PnetCDF.
#
#   If PnetCDF is successfully found, this macro calls
#
#     AC_SUBST(PNETCDF_VERSION)
#     AC_SUBST(PNETCDF_CC)
#     AC_SUBST(PNETCDF_CFLAGS)
#     AC_SUBST(PNETCDF_CPPFLAGS)
#     AC_SUBST(PNETCDF_LDFLAGS)
#     AC_SUBST(PNETCDF_LIBS)
#     AC_SUBST(PNETCDF_FC)
#     AC_SUBST(PNETCDF_FFLAGS)
#     AC_SUBST(PNETCDF_FLIBS)
#     AC_DEFINE(HAVE_PNETCDF)
#
#   It also sets
#
#     with_pnetcdf="yes"
#     with_pnetcdf_fortran="yes"    (if PnetCDF has Fortran support)
#
#   If PnetCDF is disabled or not found, this macros sets
#
#     with_pnetcdf="no"
#     with_pnetcdf_fortran="no"
#
#   Your configuration script can test $with_pnetcdf to take any further
#   actions. PNETCDF_{C,CPP,LD}FLAGS may be used when building with C or
#   C++. PNETCDF_F{FLAGS,LIBS} and PNETCDF_LDFLAGS should be used when
#   building Fortran applications.
#
#   To use the macro, one would code one of the following in "configure.ac"
#   before AC_OUTPUT:
#
#     dnl Check for PnetCDF support
#     AX_LIB_PNETCDF()
#
#   One could test $with_pnetcdf for the outcome or display it as follows
#
#     echo "PnetCDF support:  $with_pnetcdf"
#
#   One could also for example, override the default CC in "configure.ac" to
#   enforce compilation with the compiler that PnetCDF was built with:
#
#     AX_LIB_PNETCDF()
#     if test "$with_pnetcdf" = "yes"; then
#             CC="$PNETCDF_CC"
#     else
#             AC_MSG_ERROR([Unable to find PnetCDF.])
#     fi
#
# LICENSE
#
#   Copyright (c) 2020 Raffaele Montuoro <raffaele.montuoro@noaa.gov>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 0

AC_DEFUN([AX_LIB_PNETCDF], [

AC_REQUIRE([AC_PROG_AWK])

dnl Add a default --with-pnetcdf configuration option.
AC_ARG_WITH([pnetcdf],
  AS_HELP_STRING(
    [--with-pnetcdf=[yes/no/PATH]],
    m4_case(m4_normalize([$1]),
            [base directory of PnetCDF installation])
  ),
  [if test "$withval" = "no"; then
     with_pnetcdf="no"
   elif test "$withval" = "yes"; then
     with_pnetcdf="yes"
   else
     with_pnetcdf="yes"
     PNETCDF_PREFIX="${withval}"
     PNETCDF_CONFIG="${withval}/bin/pnetcdf-config"
   fi],
   [with_pnetcdf="yes"]
)

dnl Set defaults to blank
PNETCDF_CC=""
PNETCDF_VERSION=""
PNETCDF_CFLAGS=""
PNETCDF_CPPFLAGS=""
PNETCDF_LDFLAGS=""
PNETCDF_LIBS=""
PNETCDF_FC=""
PNETCDF_FFLAGS=""
PNETCDF_FLIBS=""

dnl Try and find PnetCDF tools and options.
if test "$with_pnetcdf" = "yes"; then
    if test -z "$PNETCDF_CONFIG"; then
        dnl Check to see if PNETCDF_CONFIG is in the path.
        AC_PATH_PROGS([PNETCDF_CONFIG], [pnetcdf-config], [])
        PNETCDF_PREFIX=$(AS_DIRNAME([$(AS_DIRNAME(["$PNETCDF_CONFIG"]))]))
    else
        AC_MSG_CHECKING([Using provided PnetCDF prefix])
        AC_MSG_RESULT([$PNETCDF_CONFIG])
    fi

    AC_MSG_CHECKING([for PnetCDF libraries])

    if test ! -f "$PNETCDF_CONFIG" || test ! -x "$PNETCDF_CONFIG"; then
        AC_MSG_RESULT([no])
        AC_MSG_WARN([

Unable to locate PnetCDF compilation helper script 'pnetcdf-config'.
Please specify --with-pnetcdf=<LOCATION> as the full path prefix
where PnetCDF has been installed.
PnetCDF support is being disabled (equivalent to --with-pnetcdf=no).
])
        with_pnetcdf="no"
        with_pnetcdf_fortran="no"
    else
        dnl Get the actual compiler used
        PNETCDF_CC=$(eval $PNETCDF_CONFIG --cc | $AWK '{print $[]1}')
        if test "$PNETCDF_CC" = "ccache"; then
            PNETCDF_CC=$(eval $PNETCDF_CONFIG --cc | $AWK '{print $[]2}')
        fi

        dnl Look for version
        PNETCDF_VERSION=$(eval $PNETCDF_CONFIG --version | $AWK '{print $[]2}')

        dnl Set CPPFLAGS
        PNETCDF_CPPFLAGS=-I$(eval $PNETCDF_CONFIG --includedir)

        dnl Look for the CFLAGS
        PNETCDF_CFLAGS=$(eval $PNETCDF_CONFIG --cflags)

        dnl Look for the LIBS and LDFLAGS
        PNETCDF_LDFLAGS=-L$(eval $PNETCDF_CONFIG --libdir)
        PNETCDF_LIBS=-lpnetcdf

        AC_MSG_RESULT([yes (version $[PNETCDF_VERSION])])

        dnl See if we can compile
        ax_lib_pnetcdf_save_CPPFLAGS=$CPPFLAGS
        ax_lib_pnetcdf_save_CPP=$CPP
        ax_lib_pnetcdf_save_LIBS=$LIBS
        ax_lib_pnetcdf_save_LDFLAGS=$LDFLAGS
        CFLAGS=$PNETCDF_CFLAGS
        CPPFLAGS=$PNETCDF_CPPFLAGS
        LIBS=$PNETCDF_LIBS
        LDFLAGS=$PNETCDF_LDFLAGS
        AC_CHECK_HEADER([pnetcdf.h], [ac_cv_pnetcdf_h=yes], [ac_cv_pnetcdf_h=no])
        AC_CHECK_LIB([pnetcdf], [ncmpi_create], [ac_cv_libpnetcdf=yes],
                     [ac_cv_libpnetcdf=no])
        if test "$ac_cv_pnetcdf_h" = "no" && \
           test "$ac_cv_libpnetcdf" = "no" ; then
            AC_MSG_WARN([Unable to compile PnetCDF test program])
        fi

        CFLAGS=$ax_lib_pnetcdf_save_CFLAGS
        CPP=$ax_lib_pnetcdf_save_CPP
        CPPFLAGS=$ax_lib_pnetcdf_save_CPPFLAGS
        LIBS=$ax_lib_pnetcdf_save_LIBS
        LDFLAGS=$ax_lib_pnetcdf_save_LDFLAGS

        AC_MSG_CHECKING([for PnetCDF Fortran support])
        with_pnetcdf_fortran=$(eval $PNETCDF_CONFIG --has-fortran)
        if test "x$with_pnetcdf_fortran" != "xyes"; then
            with_pnetcdf_fortran="no"
        else
            dnl Look for Fortran 90 compiler
            for arg in f90 fc ; do
                pnetcdf_fc=$(eval $PNETCDF_CONFIG --$arg | $AWK '{print $[]1}')
                if test "$pnetcdf_fc" = "ccache"; then
                    pnetcdf_fc=$(eval $PNETCDF_CONFIG --$arg | $AWK '{print $[]2}')
                fi
                if test -x "$pnetcdf_fc" ; then
                  PNETCDF_FC=$pnetcdf_fc
                  break
                fi
            done
            dnl Look for the FFLAGS
            PNETCDF_FFLAGS=$PNETCDF_CPPFLAGS

            dnl Set FLIBS
            PNETCDF_FLIBS=$PNETCDF_LIBS

            with_pnetcdf_fortran="yes"
        fi
        AC_MSG_RESULT([$[with_pnetcdf_fortran]])

        AC_SUBST([PNETCDF_VERSION])
        AC_SUBST([PNETCDF_CC])
        AC_SUBST([PNETCDF_CFLAGS])
        AC_SUBST([PNETCDF_CPPFLAGS])
        AC_SUBST([PNETCDF_LDFLAGS])
        AC_SUBST([PNETCDF_LIBS])
        AC_SUBST([PNETCDF_FC])
        AC_SUBST([PNETCDF_FFLAGS])
        AC_SUBST([PNETCDF_FLIBS])
        AC_DEFINE([HAVE_PNETCDF], [1], [Defined if you have PNETCDF support])
    fi
fi
])
