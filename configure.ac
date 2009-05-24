AC_INIT(ddpt, 0.90, dgilbert@interlog.com)

AM_INIT_AUTOMAKE
AM_CONFIG_HEADER(config.h)

AC_PROG_CC
# AC_PROG_CXX
AC_PROG_INSTALL

# check for headers
AC_HEADER_STDC
AC_CHECK_HEADERS([linux/types.h linux/bsg.h], [], [],
     [[#ifdef HAVE_LINUX_TYPES_H
     # include <linux/types.h>
     #endif
     ]])

AC_CHECK_FUNCS(posix_fadvise)

AC_CANONICAL_HOST

AC_DEFINE_UNQUOTED(SG_LIB_BUILD_HOST, "${host}", [ddpt Build Host])

case "${host}" in
        *-*-linux-gnu*)
                AC_SUBST([os_deps], ['sg_pt_linux.o'])
		AC_DEFINE_UNQUOTED(SG_LIB_LINUX, 1, [ddpt on linux])
                AC_SUBST([os_libs], ['']) ;;
        *-*-linux*)
                AC_SUBST([os_deps], ['sg_pt_linux.o'])
		AC_DEFINE_UNQUOTED(SG_LIB_LINUX, 1, [ddpt on linux])
                AC_SUBST([os_libs], ['']) ;;
        *-*-freebsd*)
                AC_SUBST([os_deps], ['sg_pt_freebsd.o'])
		AC_DEFINE_UNQUOTED(SG_LIB_FREEBSD, 1, [ddpt on FreeBSD])
                AC_SUBST([os_libs], ['-lcam']);;
        *-*-solaris*)
                AC_SUBST([os_deps], ['sg_pt_solaris.o'])
		AC_DEFINE_UNQUOTED(SG_LIB_SOLARIS, 1, [ddpt on Solaris])
                AC_SUBST([os_libs], ['']);;
        *-*-cygwin*)
                AC_SUBST([os_deps], ['sg_pt_win32.o'])
		AC_DEFINE_UNQUOTED(SG_LIB_WIN32, 1, [ddpt on Win32])
		AC_DEFINE_UNQUOTED(WIN32_SPT_DIRECT, 1, [allow large buffers, aligned?])
                AC_SUBST([os_libs], ['']) ;;
        *-*-mingw*)
                AC_SUBST([os_deps], ['sg_pt_win32.o'])
		AC_DEFINE_UNQUOTED(SG_LIB_WIN32, 1, [ddpt on Win32])
		AC_DEFINE_UNQUOTED(WIN32_SPT_DIRECT, 1, [allow large buffers, aligned?])
		AC_DEFINE_UNQUOTED(SG_LIB_MINGW, 1, [also MinGW environment])
                AC_SUBST([os_libs], ['']) ;;
        *)
                AC_SUBST([os_deps], ['sg_pt_linux.o'])
		AC_DEFINE_UNQUOTED(SG_LIB_LINUX, 1, [assume ddpt on linux])
                AC_SUBST([os_libs], ['']) ;;
esac

AC_ARG_ENABLE([no-linux-bsg],
  AC_HELP_STRING([--enable-no-linux-bsg], [ignore linux bsg (sgv4) if present]),
  AC_DEFINE_UNQUOTED(IGNORE_LINUX_BSG, 1, [ignore linux bsg], )
)

# AC_PROG_LIBTOOL
AC_OUTPUT(Makefile src/Makefile doc/Makefile)
