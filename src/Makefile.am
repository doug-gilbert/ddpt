bin_PROGRAMS = ddpt

# INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/include/scsi

AM_CFLAGS = -I$(top_srcdir)/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wall -W
# AM_CFLAGS = -I$(top_srcdir)/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wall -W -pedantic -std=c99



ddpt_SOURCES =		ddpt.c	\
			ddpt.h	\
			ddpt_win32.c \
			ddpt_wscan.c

sglib_SOURCES =         ../lib/sg_lib.c \
                        ../include/sg_lib.h     \
                        ../lib/sg_lib_data.c    \
                        ../include/sg_lib_data.h        \
                        ../lib/sg_cmds_basic.c  \
                        ../include/sg_cmds_basic.h      \
                        ../lib/sg_cmds_mmc.c    \
                        ../include/sg_cmds_mmc.h        \
                        ../include/sg_pt.h      \
                        ../lib/sg_pt_common.c

if HAVE_SGUTILS
INCLUDES = -I/usr/include/scsi
ddpt_LDADD = @os_libs@ @rt_libs@ @SGUTILS_LIBS@
else
INCLUDES = -I$(top_srcdir)/include
ddpt_SOURCES += $(sglib_SOURCES)
ddpt_LDADD = @os_deps@ @os_libs@ @rt_libs@ @SGUTILS_LIBS@
ddpt_DEPENDENCIES = @os_deps@
endif


EXTRA_ddpt_SOURCES =	../lib/sg_pt_linux.c	\
			../include/sg_linux_inc.h	\
			../lib/sg_pt_freebsd.c	\
			../lib/sg_pt_osf1.c	\
			../lib/sg_pt_solaris.c	\
			../lib/sg_pt_win32.c	\
			../include/sg_pt_win32.h
