bin_PROGRAMS = ddpt ddptctl

# for C++/clang testing
CC = gcc-8
## CC = g++
## CC = clang
## CC = clang++

# INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/include/scsi

# -std=<s> can be c99, c11, c14, gnu11, etc. Default is gnu89 (gnu90 is the same)
AM_CPPFLAGS = -iquote ${top_srcdir}/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64
##AM_CFLAGS = -Wall -W
AM_CFLAGS = -Wall -W -Wextra -Wmisleading-indentation -Wduplicated-cond -Wduplicated-branches -Wlogical-op -Wnull-dereference -Wshadow -Wjump-misses-init
# AM_CFLAGS = -Wall -W -Werror=misleading-indentation
# AM_CFLAGS = -Wall -W -pedantic -std=c11 --analyze
# AM_CFLAGS = -Wall -W -pedantic -std=c++14



ddpt_SOURCES =		ddpt.c	\
			ddpt.h	\
			ddpt_cl.c \
			ddpt_com.c \
			ddpt_pt.c \
			ddpt_xcopy.c

ddptctl_SOURCES =	ddptctl.c \
			ddpt.h	\
			ddpt_com.c \
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
                        ../include/sg_pt_nvme.h      \
                        ../lib/sg_pt_common.c

if OS_WIN32_MINGW
ddpt_SOURCES +=         ddpt_win32.c \
			ddpt_wscan.c

ddptctl_SOURCES +=	ddpt_win32.c \
			ddpt_wscan.c
endif

if OS_WIN32_CYGWIN
ddpt_SOURCES +=         ddpt_win32.c \
			ddpt_wscan.c

ddptctl_SOURCES +=	ddpt_win32.c \
			ddpt_wscan.c
endif

if HAVE_SGUTILS
INCLUDES = -I/scsi
ddpt_LDADD = @SGUTILS_LIBS@
else
INCLUDES = -I$(top_srcdir)/include
ddpt_SOURCES += $(sglib_SOURCES)
ddpt_LDADD = @os_deps@ @SGUTILS_LIBS@
ddpt_DEPENDENCIES = @os_deps@
endif

EXTRA_ddpt_SOURCES =	../lib/sg_pt_linux.c	\
			../lib/sg_pt_linux_nvme.c	\
			../include/sg_pt_linux.h	\
			../include/sg_linux_inc.h	\
			../lib/sg_pt_freebsd.c	\
			../lib/sg_pt_osf1.c	\
			../lib/sg_pt_solaris.c	\
			../lib/sg_pt_win32.c	\
			../include/sg_pt_win32.h


if HAVE_SGUTILS
INCLUDES = -I/scsi
ddptctl_LDADD = @SGUTILS_LIBS@
else
INCLUDES = -I$(top_srcdir)/include
ddptctl_SOURCES += $(sglib_SOURCES)
ddptctl_LDADD = @os_deps@ @SGUTILS_LIBS@
ddptctl_DEPENDENCIES = @os_deps@
endif

EXTRA_ddptctl_SOURCES =	../lib/sg_pt_linux.c	\
			../include/sg_linux_inc.h	\
			../lib/sg_pt_freebsd.c	\
			../lib/sg_pt_osf1.c	\
			../lib/sg_pt_solaris.c	\
			../lib/sg_pt_win32.c	\
			../include/sg_pt_win32.h


