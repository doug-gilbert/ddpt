bin_PROGRAMS = sgdd

# INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/include/scsi

AM_CFLAGS = -I$(top_srcdir)/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wall -W
# AM_CFLAGS = -I$(top_srcdir)/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wall -W -pedantic -std=c99

sgdd_SOURCES =		sgdd.c	\
			sgdd.h	\
			../lib/sg_lib.c	\
			../include/sg_lib.h	\
			../lib/sg_lib_data.c	\
			../include/sg_lib_data.h	\
			../lib/sg_cmds_basic.c	\
			../include/sg_cmds_basic.h	\
			../lib/sg_cmds_extra.c	\
			../include/sg_cmds_extra.h	\
			../include/sg_pt.h

sgdd_LDADD = @os_deps@ @GETOPT_O_FILES@ @os_libs@
sgdd_DEPENDENCIES = @os_deps@ @GETOPT_O_FILES@
EXTRA_sgdd_SOURCES =	../lib/sg_pt_linux.c	\
			../include/sg_linux_inc.h	\
			../lib/sg_pt_freebsd.c	\
			../lib/sg_pt_osf1.c	\
			../lib/sg_pt_solaris.c	\
			../lib/sg_pt_win32.c	\
			../include/sg_pt_win32.h	\
			getopt_long.c	\
			port_getopt.h
