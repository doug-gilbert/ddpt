libsgutils2_la_SOURCES = \
	sg_lib.c \
	sg_json.c \
	sg_json_sg_lib.c \
	sg_pr2serr.c \
	sg_lib_data.c \
	sg_lib_names.c \
	sg_cmds_basic.c \
	sg_cmds_basic2.c \
	sg_cmds_extra.c \
	sg_cmds_mmc.c \
	sg_pt_common.c \
	sg_snt.c \
	sg_json_builder.c

if OS_LINUX
if PT_DUMMY
libsgutils2_la_SOURCES += sg_pt_dummy.c
else
libsgutils2_la_SOURCES += \
	sg_pt_linux.c \
	sg_io_linux.c \
	sg_pt_linux_nvme.c
endif
endif

if OS_WIN32_MINGW
libsgutils2_la_SOURCES += sg_pt_win32.c
endif

if OS_WIN32_CYGWIN
libsgutils2_la_SOURCES += sg_pt_win32.c
endif

if OS_FREEBSD
if PT_DUMMY
libsgutils2_la_SOURCES += sg_pt_dummy.c
else
libsgutils2_la_SOURCES += sg_pt_freebsd.c
endif
endif

if OS_SOLARIS
libsgutils2_la_SOURCES += sg_pt_solaris.c
endif

if OS_OSF
libsgutils2_la_SOURCES += sg_pt_osf1.c
endif

if OS_HAIKU
if PT_DUMMY
libsgutils2_la_SOURCES += sg_pt_dummy.c
else
libsgutils2_la_SOURCES += sg_pt_haiku.c
endif
endif

if OS_NETBSD
libsgutils2_la_SOURCES += sg_pt_netbsd.c
endif

if OS_OPENBSD
libsgutils2_la_SOURCES += sg_pt_dummy.c
endif

if OS_OTHER
libsgutils2_la_SOURCES += sg_pt_dummy.c
endif

if DEBUG
# This is active if --enable-debug given to ./configure
# removed -Wduplicated-branches because needs gcc-8
DBG_CFLAGS = -Wextra -Wmisleading-indentation -Wduplicated-cond -Wlogical-op -Wnull-dereference -Wshadow -Wjump-misses-init -Wunused -Wsizeof-array-argument
DBG_CXXFLAGS = -Wextra -Wmisleading-indentation -Wduplicated-cond -Wlogical-op -Wnull-dereference -Wshadow -Wunused -Wsizeof-array-argument
DBG_CPPFLAGS = -DDEBUG
else
DBG_CFLAGS =
DBG_CXXFLAGS =
DBG_CPPFLAGS =
endif

# For C++/clang testing
## CC = gcc-9
## CXX = g++
## CC = g++
## CC = clang
## CXX = clang++
## CC = clang++
## CC = powerpc64-linux-gnu-gcc

# -std=<s> can be c99, c11, gnu11, etc. Default is gnu11 for C code
# -Wall is no longer all warnings. Add -W (since renamed to -Wextra) for more
AM_CPPFLAGS = -iquote ${top_srcdir}/include -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 $(DBG_CPPFLAGS)
AM_CFLAGS = -Wall -W -std=c99 $(DBG_CFLAGS)
# AM_CFLAGS = -Wall -W -flto=auto $(DBG_CFLAGS)
# AM_CFLAGS = -Wall -W $(DBG_CFLAGS) -fanalyzer
# AM_CFLAGS = -Wall -W -pedantic -std=c99
# AM_CFLAGS = -Wall -W -pedantic -std=c11
# AM_CFLAGS = -Wall -W -pedantic -std=c11 --analyze
# AM_CFLAGS = -Wall -W -pedantic -std=c++11
# AM_CFLAGS = -Wall -W -pedantic -std=c++14
# AM_CFLAGS = -Wall -W -pedantic -std=c++17 $(DBG_CXXFLAGS)
# AM_CFLAGS = -Wall -W -pedantic -std=c++20 $(DBG_CXXFLAGS)
# AM_CFLAGS = -Wall -W -pedantic -std=c++20 -fanalyzer $(DBG_CXXFLAGS)
# AM_CFLAGS = -Wall -W -pedantic -std=c++20 --analyze $(DBG_CXXFLAGS)
# AM_CFLAGS = -Wall -W -pedantic -std=c++23 $(DBG_CXXFLAGS)
# AM_CFLAGS = -Wall -W -pedantic -std=c++23 -fanalyzer $(DBG_CXXFLAGS)
# AM_CFLAGS = -Wall -W -pedantic -std=c++23 --analyze $(DBG_CXXFLAGS)

lib_LTLIBRARIES = libsgutils2.la

libsgutils2_la_LDFLAGS = -version-info 2:0:0 -no-undefined -release ${PACKAGE_VERSION}

libsgutils2_la_LIBADD = @GETOPT_O_FILES@
libsgutils2_la_DEPENDENCIES = @GETOPT_O_FILES@

EXTRA_DIST = \
	sg_json_builder.h \
	BSD_LICENSE
