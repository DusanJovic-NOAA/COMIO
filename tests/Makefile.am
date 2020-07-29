# The `:;' works around a Bash 3.2 bug when the output is not writeable.
$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	:;{ \
		echo '# Signature of the current package.' && \
		echo 'm4_define([AT_PACKAGE_NAME],' && \
		echo '  [$(PACKAGE_NAME)])' && \
		echo 'm4_define([AT_PACKAGE_TARNAME],' && \
		echo '  [$(PACKAGE_TARNAME)])' && \
		echo 'm4_define([AT_PACKAGE_VERSION],' && \
		echo '  [$(PACKAGE_VERSION)])' && \
		echo 'm4_define([AT_PACKAGE_STRING],' && \
		echo '  [$(PACKAGE_STRING)])' && \
		echo 'm4_define([AT_PACKAGE_BUGREPORT],' && \
		echo '  [$(PACKAGE_BUGREPORT)])'; \
		echo 'm4_define([AT_PACKAGE_URL],' && \
		echo '  [$(PACKAGE_URL)])'; \
	} >'$(srcdir)/package.m4'

DISTCLEANFILES = atconfig
EXTRA_DIST = testsuite.at $(srcdir)/package.m4 $(TESTSUITE)
TESTSUITE = $(srcdir)/testsuite

check-local: atconfig $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)

installcheck-local: atconfig $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' \
	$(TESTSUITEFLAGS)

clean-local: clean-modules
	test ! -f '$(TESTSUITE)' || \
	$(SHELL) '$(TESTSUITE)' --clean

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f *.$(FC_MODEXT)

AUTOM4TE = $(SHELL) $(top_srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

$(srcdir)/testsuite.at:
	:;{ \
		echo 'AT_INIT()' && \
		echo AT_COLOR_TESTS && \
		for file in $(check_PROGRAMS); do \
		test_group_name=`$(GREP) test-group-name: '$(srcdir)'/$$file.F90 | cut -d: -f2-` ; \
		echo "AT_SETUP([$$test_group_name])" && \
		echo 'echo ">>> TEST PASSED" > expout' && \
		echo "AT_CHECK([../../run_comio ../../$$file],[],[expout],[ignore])" && \
		echo AT_CLEANUP ; \
		done; \
	} > $@

AM_FCFLAGS = $(FC_MODINC)$(top_builddir)/src
LDADD      = $(top_builddir)/src/libcomio.a

check_PROGRAMS =
if HAVE_HDF5
check_PROGRAMS += test_comio_hdf_int
check_PROGRAMS += test_comio_hdf_flt
check_PROGRAMS += test_comio_hdf_dbl
check_PROGRAMS += test_comio_hdf_d2f
check_PROGRAMS += test_comio_hdf_dff
check_PROGRAMS += test_comio_hdf_att
check_PROGRAMS += test_comio_hdf_dim
check_PROGRAMS += test_comio_hdf_grp
check_PROGRAMS += test_comio_hdf_mds
test_comio_hdf_int_SOURCES = test_comio_mod.F90 test_comio_hdf_int.F90
test_comio_hdf_flt_SOURCES = test_comio_mod.F90 test_comio_hdf_flt.F90
test_comio_hdf_dbl_SOURCES = test_comio_mod.F90 test_comio_hdf_dbl.F90
test_comio_hdf_d2f_SOURCES = test_comio_mod.F90 test_comio_hdf_d2f.F90
test_comio_hdf_dff_SOURCES = test_comio_mod.F90 test_comio_hdf_dff.F90
test_comio_hdf_att_SOURCES = test_comio_mod.F90 test_comio_hdf_att.F90
test_comio_hdf_dim_SOURCES = test_comio_mod.F90 test_comio_hdf_dim.F90
test_comio_hdf_grp_SOURCES = test_comio_mod.F90 test_comio_hdf_grp.F90
test_comio_hdf_mds_SOURCES = test_comio_mod.F90 test_comio_hdf_mds.F90
test_comio_hdf_int.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_flt.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_dbl.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_d2f.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_dff.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_att.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_dim.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_grp.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_hdf_mds.$(OBJEXT) : test_comio_mod.$(OBJEXT)
endif
if HAVE_PNETCDF
check_PROGRAMS += test_comio_pnc_int
check_PROGRAMS += test_comio_pnc_flt
check_PROGRAMS += test_comio_pnc_dbl
check_PROGRAMS += test_comio_pnc_d2f
check_PROGRAMS += test_comio_pnc_dff
check_PROGRAMS += test_comio_pnc_att
check_PROGRAMS += test_comio_pnc_dim
check_PROGRAMS += test_comio_pnc_mds
test_comio_pnc_int_SOURCES = test_comio_mod.F90 test_comio_pnc_int.F90
test_comio_pnc_flt_SOURCES = test_comio_mod.F90 test_comio_pnc_flt.F90
test_comio_pnc_dbl_SOURCES = test_comio_mod.F90 test_comio_pnc_dbl.F90
test_comio_pnc_d2f_SOURCES = test_comio_mod.F90 test_comio_pnc_d2f.F90
test_comio_pnc_dff_SOURCES = test_comio_mod.F90 test_comio_pnc_dff.F90
test_comio_pnc_att_SOURCES = test_comio_mod.F90 test_comio_pnc_att.F90
test_comio_pnc_dim_SOURCES = test_comio_mod.F90 test_comio_pnc_dim.F90
test_comio_pnc_mds_SOURCES = test_comio_mod.F90 test_comio_pnc_mds.F90
test_comio_pnc_int.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_pnc_flt.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_pnc_dbl.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_pnc_d2f.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_pnc_dff.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_pnc_att.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_pnc_dim.$(OBJEXT) : test_comio_mod.$(OBJEXT)
test_comio_pnc_mds.$(OBJEXT) : test_comio_mod.$(OBJEXT)
endif
