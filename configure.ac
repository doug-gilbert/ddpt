AC_INIT(ddpt, 0.98, dgilbert@interlog.com)

AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE
AM_CONFIG_HEADER(config.h)

AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL

# AM_PROG_AR is supported and needed since automake v1.12+
ifdef([AM_PROG_AR], [AM_PROG_AR], [])

# check for headers
AC_HEADER_STDC
AC_CHECK_HEADERS([byteswap.h], [], [], [])

AC_CHECK_FUNCS(posix_fadvise)
AC_CHECK_FUNCS(fsync)
AC_CHECK_FUNCS(fdatasync)
AC_CHECK_LIB(rt, clock_gettime, [LIBS="$LIBS -lrt"])
AC_CHECK_FUNCS(clock_gettime)
AC_CHECK_FUNCS(gettimeofday)
AC_CHECK_FUNCS(nanosleep)
AC_CHECK_FUNCS(siginterrupt)
AC_CHECK_FUNCS(fallocate)
AC_CHECK_FUNCS(posix_fallocate)
AC_CHECK_FUNCS(posix_memalign)
AC_CHECK_FUNCS(sysconf)
AC_CHECK_FUNCS(srand48_r)

AC_CANONICAL_HOST

AC_DEFINE_UNQUOTED(SG_LIB_BUILD_HOST, "${host}", [ddpt Build Host])

check_for_getrandom() {
	AC_CHECK_HEADERS([sys/random.h])
	AC_MSG_CHECKING([for getrandom()])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#ifdef HAVE_SYS_RANDOM_H
#include <sys/random.h>
#endif
]], [[
char buf[16];
return getrandom(buf, sizeof(buf), 0);
]])], [
		AC_MSG_RESULT(yes)
		AC_DEFINE_UNQUOTED(HAVE_GETRANDOM, 1, [Define to 1 if you have the `getrandom' function])], [
		AC_MSG_RESULT(no)
		])
}

check_for_linux_nvme_headers() {
	AC_CHECK_HEADERS([linux/nvme_ioctl.h], [AC_DEFINE_UNQUOTED(HAVE_NVME, 1, [Found NVMe])], [], [])
	AC_CHECK_HEADERS([linux/types.h linux/bsg.h linux/kdev_t.h], [], [],
		     [[#ifdef HAVE_LINUX_TYPES_H
		     # include <linux/types.h>
		     #endif
		     ]])
	AC_CHECK_HEADERS([linux/major.h], [AC_DEFINE_UNQUOTED(HAVE_LINUX_MAJOR_H, 1,
		     [Found linux/major.h])], [], [])
	AC_CHECK_HEADERS([linux/types.h], [AC_DEFINE_UNQUOTED(HAVE_LINUX_TYPES_H, 1,
		     [Found linux/types.h])], [], [])
}

check_for_linux_sg_v4_hdr() {
	AC_EGREP_CPP(found,
		[ # include <scsi/sg.h>
		  #ifdef SG_IOSUBMIT
		   found
		  #endif
		],
		[AC_DEFINE_UNQUOTED(HAVE_LINUX_SG_V4_HDR, 1, [Have Linux sg v4 header]) ])
}

check_for___u64() {
	AC_MSG_CHECKING([for __u64])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
	#ifdef HAVE_LINUX_TYPES_H
	#include <linux/types.h>
	#endif
	]],[[
	__u64 var = 0;
	return var;
	]])],
	[
	AC_MSG_RESULT(yes)
	AC_DEFINE(HAVE___U64, 1, [__u64 is defined in <linux/types.h>])
	],
	[AC_MSG_RESULT(no)])
}

case "${host}" in
	*-*-android*)
		AC_DEFINE_UNQUOTED(SG_LIB_ANDROID, 1, [sg3_utils on android])
		AC_DEFINE_UNQUOTED(SG_LIB_LINUX, 1, [sg3_utils on linux])
		check_for_linux_sg_v4_hdr
		check_for_getrandom
		check_for_linux_nvme_headers
		check_for___u64;;
        *-*-freebsd*|*-*-kfreebsd*-gnu*)
                AC_SUBST([os_deps], ['sg_pt_freebsd.o'])
                AC_DEFINE_UNQUOTED(SG_LIB_FREEBSD, 1, [ddpt on FreeBSD])
                AC_DEFINE_UNQUOTED(HAVE_NVME, 1, [Found NVMe])
                check_for_getrandom;
                LIBS="$LIBS -lcam";;
        *-*-solaris*)
                AC_SUBST([os_deps], ['sg_pt_solaris.o'])
                AC_DEFINE_UNQUOTED(SG_LIB_SOLARIS, 1, [ddpt on Solaris]);;
        *-*-netbsd*)
                AC_SUBST([os_deps], ['sg_pt_netbsd.o'])
                AC_DEFINE_UNQUOTED(SG_LIB_NETBSD, 1, [sdparm on NetBSD]);;
        *-*-osf*)
                AC_SUBST([os_deps], ['sg_pt_osf1.o'])
                AC_DEFINE_UNQUOTED(SG_LIB_OSF1, 1, [ddpt on Tru64 UNIX]);;
        *-*-cygwin*)
                AC_SUBST([os_deps], ['sg_pt_win32.o'])
                AC_DEFINE_UNQUOTED(SG_LIB_WIN32, 1, [ddpt on Win32])
                AC_DEFINE_UNQUOTED(HAVE_NVME, 1, [Found NVMe])
                check_for_getrandom;
                CFLAGS="$CFLAGS -Wno-char-subscripts";;
        *-*-mingw*)
                AC_SUBST([os_deps], ['sg_pt_win32.o'])
                AC_DEFINE_UNQUOTED(SG_LIB_WIN32, 1, [ddpt on Win32])
                AC_DEFINE_UNQUOTED(SG_LIB_MINGW, 1, [also MinGW environment])
                AC_DEFINE_UNQUOTED(HAVE_NVME, 1, [Found NVMe])
                check_for_getrandom;
		CFLAGS="$CFLAGS -D__USE_MINGW_ANSI_STDIO";;
        *-*-linux-gnu* | *-*-linux* | *)
                AC_SUBST([os_deps], ['sg_pt_linux.o sg_pt_linux_nvme.o'])
                AC_DEFINE_UNQUOTED(SG_LIB_LINUX, 1, [ddpt on linux])
                check_for_getrandom;
                check_for_linux_nvme_headers;;
esac

# Define platform-specific symbol.
AM_CONDITIONAL(OS_FREEBSD, [echo $host_os | grep 'freebsd' > /dev/null])
AM_CONDITIONAL(OS_LINUX, [echo $host_os | grep '^linux' > /dev/null])
AM_CONDITIONAL(OS_NETBSD, [echo $host_os | grep '^netbsd' > /dev/null])
AM_CONDITIONAL(OS_SOLARIS, [echo $host_os | grep '^solaris' > /dev/null])
AM_CONDITIONAL(OS_WIN32_MINGW, [echo $host_os | grep '^mingw' > /dev/null])
AM_CONDITIONAL(OS_WIN32_CYGWIN, [echo $host_os | grep '^cygwin' > /dev/null])
AM_CONDITIONAL(OS_ANDROID, [echo $host_os | grep 'android' > /dev/null])

AC_ARG_ENABLE([debug],
              [  --enable-debug    Turn on debugging],
              [case "${enableval}" in
                  yes) debug=true ;;
                  no)  debug=false ;;
                  *) AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
               esac],[debug=false])
AM_CONDITIONAL([DEBUG], [test x$debug = xtrue])

AC_ARG_ENABLE([linuxbsg],
  AC_HELP_STRING([--disable-linuxbsg], [option ignored, this is placeholder]),
  [AC_DEFINE_UNQUOTED(IGNORE_LINUX_BSG, 1, [option ignored], )], [])

AC_ARG_ENABLE([libsgutils],
  AC_HELP_STRING([--disable-libsgutils], [ignore libsgutils if present]),
          [have_sgutils=no],
          [AC_CHECK_LIB(sgutils2, sg_ll_inquiry,
  	[SGUTILS_LIBS="-lsgutils2"; have_sgutils=yes], have_sgutils=no)])
AC_SUBST(SGUTILS_LIBS)
AM_CONDITIONAL(HAVE_SGUTILS, test x"$have_sgutils" = xyes)

AC_ARG_ENABLE([scsistrings],
  [AS_HELP_STRING([--disable-scsistrings],
                  [Disable full SCSI sense strings])],
  [], [AC_DEFINE_UNQUOTED(SG_SCSI_STRINGS, 1, [full SCSI sense strings], )])

AC_ARG_ENABLE([nvme-supp],
  AC_HELP_STRING([--disable-nvme-supp], [remove all or most NVMe code]),
  [AC_DEFINE_UNQUOTED(IGNORE_NVME, 1, [compile out NVMe support], )], [])

AC_ARG_ENABLE([fast-lebe],
  AC_HELP_STRING([--disable-fast-lebe], [use generic little-endian/big-endian code instead]),
  [AC_DEFINE_UNQUOTED(IGNORE_FAST_LEBE, 1, [use generic little-endian/big-endian instead], )], [])

# AC_PROG_LIBTOOL
AC_OUTPUT(Makefile src/Makefile doc/Makefile)
