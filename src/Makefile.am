lib_LIBRARIES = libcomio.a

libcomio_a_SOURCES  = io_cutils.c
libcomio_a_SOURCES += comio_mod.F90 comio_class.F90 io_prec.F90 io_utils.F90 ioerr_class.F90 hdf_class.F90 pnc_class.F90
libcomio_a_CFLAGS  =
libcomio_a_FCFLAGS = -I .

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f *.$(FC_MODEXT)

clean-local: clean-modules

# dependencies

libcomio_a-comio_class.$(OBJEXT) : libcomio_a-ioerr_class.$(OBJEXT) libcomio_a-io_prec.$(OBJEXT)
libcomio_a-comio_mod.$(OBJEXT)   : libcomio_a-comio_class.$(OBJEXT) \
	libcomio_a-hdf_class.$(OBJEXT) libcomio_a-pnc_class.$(OBJEXT)
libcomio_a-io_utils.$(OBJEXT)    : libcomio_a-io_cutils.$(OBJEXT)
libcomio_a-hdf_class.$(OBJEXT)   : libcomio_a-comio_class.$(OBJEXT) libcomio_a-io_prec.$(OBJEXT) \
	libcomio_a-ioerr_class.$(OBJEXT)
libcomio_a-pnc_class.$(OBJEXT)   : libcomio_a-comio_class.$(OBJEXT) libcomio_a-io_prec.$(OBJEXT) \
	libcomio_a-ioerr_class.$(OBJEXT) libcomio_a-io_utils.$(OBJEXT)
