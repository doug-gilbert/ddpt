bin_PROGRAMS = ddpt

# for C++/clang testing
## CC = g++ -std=c++11
## CC = g++
## CC = clang
## CC = clang++

# INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/include/scsi

# -std=<s> can be c99, c11, gnu11, etc. Default is gnu89 (gnu90 is the same)
AM_CFLAGS = -I$(top_srcdir)/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wall -W
# AM_CFLAGS = -I$(top_srcdir)/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -Wall -W -pedantic -std=c11



ddpt_SOURCES =		ddpt.c	\
			ddpt.h	\
			ddpt_pt.c \
			ddpt_xcopy.c

sglib_SOURCES =         ../lib/sg_lib.c \
                        ../include/sg_lib.h     \
                        ../lib/sg_lib_data.c    \
                        ../include/sg_lib_data.h        \
                        ../lib/sg_cmds_basic.c  \
                        ../lib/sg_cmds_basic2.c  \
                        ../include/sg_cmds_basic.h      \
                        ../lib/sg_cmds_extra.c  \
                        ../include/sg_cmds_extra.h      \
                        ../include/sg_pt.h      \
                        ../lib/sg_pt_common.c

if OS_WIN32_MINGW
ddpt_SOURCES +=         ddpt_win32.c \
			ddpt_wscan.c
endif

if OS_WIN32_CYGWIN
ddpt_SOURCES +=         ddpt_win32.c \
			ddpt_wscan.c
endif

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
