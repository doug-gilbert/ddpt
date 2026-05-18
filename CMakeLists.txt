cmake_minimum_required ( VERSION 3.10 )

project(ddpt VERSION 0.97.9 LANGUAGES C CXX)
# Use version number: x.y.9 to indicate pre-release of x.(y+1) 
# Example: VERSION 0.97.9 is pre-release of 0.98

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

option ( BUILD_SHARED_LIBS "Build using shared libraries" ON)
option ( ENABLE_DEBUG "Turn on debugging (extra warnings)" OFF)
option ( DISABLE_FAST_LEBE "Use generic little-endian/big-endian code instead" OFF)
option ( DISABLE_SCSISTRINGS "Save space in executable using numeric error codes instead of strings" OFF)
option ( DISABLE_LINUXBSG "Remove support for /dev/bsg/* devices" OFF)
option ( DISABLE_LIBSGUTILS "Same as -DBUILD_SHARED_LIBS=OFF" OFF)
option ( DISABLE_NVME_SUPP "Remove support for NVMe devices (/dev/nvme*)" OFF)

if (DISABLE_LIBSGUTILS)
    set(BUILD_SHARED_LIBS OFF)
endif ()

set(FEATURE_COMMENT "//")
    
if ( ENABLE_DEBUG )
    MESSAGE( STATUS ">> Compile with DEBUG set" )
endif ( ENABLE_DEBUG )

set(IGNORE_FAST_LEBE 0)
if (DISABLE_FAST_LEBE)
    set(IGNORE_FAST_LEBE 1)
endif (DISABLE_FAST_LEBE)

set(IGNORE_LINUX_BSG 0)
if (DISABLE_LINUXBSG)
    set(IGNORE_LINUX_BSG 1)
endif ()

set(SG_SCSI_STRINGS 1)
if (DISABLE_SCSISTRINGS)
    set(SG_SCSI_STRINGS 0)
endif()

set(IGNORE_NVME 0)
if (DISABLE_NVME_SUPP)
    set(IGNORE_NVME 1)
endif()

    
include ( CheckIncludeFile )
CHECK_INCLUDE_FILE( "byteswap.h" HAVE_BYTESWAP_H )
    
include(CheckFunctionExists)

check_function_exists(posix_memalign
    HAVE_POSIX_MEMALIGN
)

set(OS_LINUX 0)
set(SMP_LIB_LINUX 0)
set(OS_ANDROID 0)
set(OS_FREEBSD 0)
set(SMP_LIB_FREEBSD 0)
set(OS_SOLARIS 0)
set(SMP_LIB_SOLARIS 0)
set(OS_AIX 0)
set(OS_WINDOWS 0)
set(OS_CYGWIN 0)
set(OS_MINGW 0)

if(CYGWIN)
    message(STATUS "Cygwin")
    set(OS_CYGWIN 1)
elseif(MINGW)
    message(STATUS "MinGW")
    set(OS_MINGW 1)
elseif(WIN32)
    message(STATUS "Windows")
    set(OS_WINDOWS 1)
elseif(ANDROID)
    message(STATUS "Android")
    set(OS_ANDROID 1)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    message(STATUS "Linux")
    set(OS_LINUX 1)
    set(SMP_LIB_LINUX 1)
    CHECK_INCLUDE_FILE( "linux/types.h" HAVE_LINUX_TYPES_H )
    CHECK_INCLUDE_FILE( "linux/bsg.h" HAVE_LINUX_BSG_H )
    CHECK_INCLUDE_FILE( "linux/kdev_t.h" HAVE_LINUX_KDEV_T_H )
elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    message(STATUS "FreeBSD")
    set(OS_FREEBSD 1)
    set(SMP_LIB_FREEBSD 1)
    target_include_directories( smp_utils PRIVATE /usr/local/include )
elseif(CMAKE_SYSTEM_NAME STREQUAL "SunOS")
    message(STATUS "Solaris")
    set(OS_SOLARIS 1)
    set(SMP_LIB_SOLARIS 1)
elseif(CMAKE_SYSTEM_NAME STREQUAL "AIX")
    message(STATUS "AIX")
    set(OS_AIX 1)
elseif(APPLE)
    message(STATUS "macOS")
    set(OS_MACOS 1)
else()
    message(STATUS "Unknown system: ${CMAKE_SYSTEM_NAME}")
endif()

configure_file( ${CMAKE_SOURCE_DIR}/config.h.in.cmake ${CMAKE_SOURCE_DIR}/config.h )

add_subdirectory( src )

add_subdirectory( doc )

install(FILES ChangeLog DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES README DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES AUTHORS DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES CREDITS DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES COPYING DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES ${CMAKE_SOURCE_DIR}/doc/ddpt.8.html DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES ${CMAKE_SOURCE_DIR}/doc/ddpt_examples.txt DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES ${CMAKE_SOURCE_DIR}/doc/scatter_file.txt DESTINATION "${CMAKE_INSTALL_DOCDIR}")

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VENDOR "Douglas Gilbert")
set(CPACK_PROJECT_VERSION "${PROJECT_VERSION}")

SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "dd clone that can use pass-through storage device interfaces")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Douglas Gilbert") #required
SET(CPACK_DEBIAN_PACKAGE_SECTION "admin")
if ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Build using shared libraries (default)" )
    if ( LIBSGUTILS )
        MESSAGE( ">> Found shared library: ${LIBSGUTILS}" )
        SET(CPACK_DEBIAN_PACKAGE_DEPENDS "libsgutils2 (= ${PROJECT_VERSION})")
        SET(CPACK_RPM_PACKAGE_DEPENDS "libsgutils2 (= ${PROJECT_VERSION})")
    else ( LIBSGUTILS )
        MESSAGE( ">> Did not find shared library: ${LIBSGUTILS}" )
	SET(CPACK_DEBIAN_PACKAGE_DEPENDS "")
	SET(CPACK_RPM_PACKAGE_DEPENDS "")
    endif ( LIBSGUTILS )
else ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Trying to build a static executable" )
    SET(CPACK_DEBIAN_PACKAGE_DEPENDS "")
    SET(CPACK_RPM_PACKAGE_DEPENDS "")
endif ( BUILD_SHARED_LIBS )
set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)

SET(CPACK_RPM_PACKAGE_DESCRIPTION "dd clone that can use pass-through storage device interfaces")
SET(CPACK_RPM_PACKAGE_LICENSE "BSD-2-clause")
SET(CPACK_RPM_PACKAGE_VENDOR "Douglas Gilbert")
set(CPACK_RPM_FILE_NAME RPM-DEFAULT)

SET(CPACK_GENERATOR "DEB;RPM")

include(CPack)
include(CTest)

return()

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


set (CMAKE_CXX_STANDARD 20 )
set (CMAKE_CXX_STANDARD_REQUIRED ON)
set (CMAKE_CXX_EXTENSIONS OFF)

include ( CheckIncludeFile )
include ( CheckIncludeFileCXX )

set ( sourcefiles_ddpt
	src/ddpt.c
	src/ddpt_cl.c
	src/ddpt_com.c
	src/ddpt_pt.c
	src/ddpt_xcopy.c
)

set ( sourcefiles_ddptctl
	src/ddptctl.c
	src/ddpt_com.c
	src/ddpt_pt.c
	src/ddpt_xcopy.c
)

set ( sourcefiles_ddpt_sgl
	src/ddpt_sgl.cpp
	src/ddpt_com.c
	src/ddpt_pt.c
	src/ddpt_xcopy.c
)

set ( headerfiles
	src/ddpt.h
	include/sg_lib.h
)

set ( src_nolib_ddpt
	lib/sg_lib.c
	lib/sg_lib_data.c
	lib/sg_cmds_basic.c
	lib/sg_cmds_basic2.c
	lib/sg_pr2serr.c
	lib/sg_pt_common.c
	lib/sg_pt_linux.c
	lib/sg_pt_linux_nvme.c
	lib/sg_cmds_extra.c
	lib/sg_snt.c
)

if ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Build using shared libraries (default)" )
    find_library(LIBSGUTILS NAMES sgutils2 PATH_SUFFIXES lib)
    if ( LIBSGUTILS )
        MESSAGE( ">> Found shared library: ${LIBSGUTILS}" )
        add_executable ( ddpt ${sourcefiles_ddpt} ${headerfiles} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} ${headerfiles} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${headerfiles} )
        target_include_directories(ddpt PUBLIC include)
        target_include_directories(ddptctl PUBLIC include)
        target_include_directories(ddpt_sgl PUBLIC include)
	target_link_libraries(ddpt PRIVATE ${LIBSGUTILS})
	target_link_libraries(ddptctl PRIVATE ${LIBSGUTILS})
	target_link_libraries(ddpt_sgl PRIVATE ${LIBSGUTILS})
    else ( LIBSGUTILS )
        MESSAGE( ">> Did not find shared library: ${LIBSGUTILS}" )
        add_executable ( ddpt ${sourcefiles_ddpt} ${src_nolib_ddpt} ${headerfiles} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} ${src_nolib_ddpt} ${headerfiles} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${src_nolib_ddpt} ${headerfiles} )
        target_include_directories(ddpt PUBLIC include)
        target_include_directories(ddptctl PUBLIC include)
        target_include_directories(ddpt_sgl PUBLIC include)
        target_link_libraries(ddpt)
        target_link_libraries(ddptctl)
        target_link_libraries(ddpt_sgl)
    endif ( LIBSGUTILS )
else ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Trying to build a static executable" )
    add_executable ( ddpt ${sourcefiles_ddpt} ${src_nolib_ddpt} ${headerfiles} )
    add_executable ( ddptctl ${sourcefiles_ddptctl} ${src_nolib_ddpt} ${headerfiles} )
    add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${src_nolib_ddpt} ${headerfiles} )
    add_definitions ( -static )
    target_include_directories(ddpt PUBLIC include)
    target_include_directories(ddptctl PUBLIC include)
    target_include_directories(ddpt_sgl PUBLIC include)
    target_link_libraries(ddpt -static)
    target_link_libraries(ddptctl -static)
    target_link_libraries(ddpt_sgl -static)
endif ( BUILD_SHARED_LIBS )

install(TARGETS ddpt ddptctl ddpt_sgl RUNTIME DESTINATION bin)

include(GNUInstallDirs)
file(ARCHIVE_CREATE OUTPUT ddpt.8.gz PATHS doc/ddpt.8 FORMAT raw COMPRESSION GZip)
file(ARCHIVE_CREATE OUTPUT ddptctl.8.gz PATHS doc/ddptctl.8 FORMAT raw COMPRESSION GZip)
file(ARCHIVE_CREATE OUTPUT ddpt_sgl.8.gz PATHS doc/ddpt_sgl.8 FORMAT raw COMPRESSION GZip)
install(FILES ddpt.8.gz DESTINATION "${CMAKE_INSTALL_MANDIR}/man8")
install(FILES ddptctl.8.gz DESTINATION "${CMAKE_INSTALL_MANDIR}/man8")
install(FILES ddpt_sgl.8.gz DESTINATION "${CMAKE_INSTALL_MANDIR}/man8")
install(FILES ChangeLog DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES README DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES AUTHORS DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES CREDITS DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES COPYING DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES doc/ddpt.html DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES doc/ddpt_examples.txt DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES doc/scatter_file.txt DESTINATION "${CMAKE_INSTALL_DOCDIR}")

if ( ${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD" )
    MESSAGE( ">> Detected FreeBSD" )
    target_include_directories( ddpt PRIVATE /usr/local/include )
    target_include_directories( ddptctl PRIVATE /usr/local/include )
    target_include_directories( ddpt_sgl PRIVATE /usr/local/include )
endif ( )


set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION "${PROJECT_VERSION}")
SET(CPACK_GENERATOR "DEB;RPM")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Douglas Gilbert") #required
SET(CPACK_DEBIAN_PACKAGE_SECTION "admin")
SET(CPACK_RPM_PACKAGE_DESCRIPTION "dd clone that can use pass-through storage device interfaces")
SET(CPACK_RPM_PACKAGE_LICENSE "BSD-2-clause")
SET(CPACK_RPM_PACKAGE_VENDOR "Douglas Gilbert")

include(CPack)
include(CTest)




# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


cmake_minimum_required ( VERSION 3.10 )

project(smp_utils VERSION 0.99.9 LANGUAGES C)
# Use version number: x.y.9 to indicate pre-release of x.(y+1)
# Example: VERSION 0.99.9 is pre-release of 1.00

include(GNUInstallDirs)
include(CMakePackageConfigHelpers)

set ( CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -g" )

string(TIMESTAMP BUILD_TIME "%Y%m%d %H:%M:%S")

option ( BUILD_SHARED_LIBS "Build using shared libraries" ON)
option ( ENABLE_DEBUG "Turn on debugging (extra warnings)" OFF)
option ( DISABLE_FAST_LEBE "Use generic little-endian/big-endian code instead" OFF)

# The intention is to support both cmake _and_ GNU Autotools build systems.
# To this end (and to shorten the actual compile line) a dynamic config.h
# is constructed to pass user options and detected system characteristics
# (e.g. is <byteswap.h> present on on the standard header search path? ).
# In Autotools the initial call to autoconf (often in a script called
# autogen.sh or bootstrap) creates an artifact file call config.h.in
# (and a script called configure). The next step is to execute that configure
# script (typically with './configure'), perhaps with some options (to list
# those options use './configure --help'). Part of the work of configure is
# to use config.h.in to create config.h . The next step is to compile the
# code and that code looks for that config.h file. With cmake the goal is to
# also generate a config.h file which is functional equivalent to what
# Autotools generates, given the same environment and command line options
# (if any). To achieve this for cmake, a _permanent_ config.h.in.cmake file
# has been added to the project for this cmake line (in CMakeLists.txt):
#    configure_file( config.h.in.cmake config.h )
# and that generates a suitable config.h . Clear?
#
# Currently the only parameter (a define in C) that goes through the compile
# line is DEBUG (more precisely '-DDEBUG' will appear on the compile line if
# ENABLE_DEBUG=ON is given (e.g. 'cmake . -DENABLE_DEBUG=ON'). The rest of
# the parameters/defines are sent through config.h .
#
# So config.h.in.cmake is a "magic" file associated with this cmake
# implementation. The other cmake related files are a small hierarchy of
# CMakeList.txt files. The main CMakeList.txt is in the top-level directory
# of this package. The other magic file is lib/cmake/smputils1Config.cmake.in
# whose purpose remains a mystery.

set(FEATURE_COMMENT "//")

if ( ENABLE_DEBUG )
    MESSAGE( STATUS ">> Compile with DEBUG set" )
endif ( ENABLE_DEBUG )

set(IGNORE_FAST_LEBE 0)
if (DISABLE_FAST_LEBE)
    set(IGNORE_FAST_LEBE 1)
endif (DISABLE_FAST_LEBE)

include ( CheckIncludeFile )
CHECK_INCLUDE_FILE( "byteswap.h" HAVE_BYTESWAP_H )

include(CheckFunctionExists)

check_function_exists(posix_memalign
    HAVE_POSIX_MEMALIGN
)

set(OS_LINUX 0)
set(SMP_LIB_LINUX 0)
set(OS_ANDROID 0)
set(OS_FREEBSD 0)
set(SMP_LIB_FREEBSD 0)
set(OS_SOLARIS 0)
set(SMP_LIB_SOLARIS 0)
set(OS_AIX 0)
set(OS_WINDOWS 0)
set(OS_CYGWIN 0)
set(OS_MINGW 0)

if(CYGWIN)
    message(STATUS "Cygwin")
    set(OS_CYGWIN 1)
elseif(MINGW)
    message(STATUS "MinGW")
    set(OS_MINGW 1)
elseif(WIN32)
    message(STATUS "Windows")
    set(OS_WINDOWS 1)
elseif(ANDROID)
    message(STATUS "Android")
    set(OS_ANDROID 1)
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    message(STATUS "Linux")
    set(OS_LINUX 1)
    set(SMP_LIB_LINUX 1)
    CHECK_INCLUDE_FILE( "linux/types.h" HAVE_LINUX_TYPES_H )
    CHECK_INCLUDE_FILE( "linux/bsg.h" HAVE_LINUX_BSG_H )
    CHECK_INCLUDE_FILE( "linux/kdev_t.h" HAVE_LINUX_KDEV_T_H )
elseif(CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    message(STATUS "FreeBSD")
    set(OS_FREEBSD 1)
    set(SMP_LIB_FREEBSD 1)
    target_include_directories( smp_utils PRIVATE /usr/local/include )
elseif(CMAKE_SYSTEM_NAME STREQUAL "SunOS")
    message(STATUS "Solaris")
    set(OS_SOLARIS 1)
    set(SMP_LIB_SOLARIS 1)
elseif(CMAKE_SYSTEM_NAME STREQUAL "AIX")
    message(STATUS "AIX")
    set(OS_AIX 1)
elseif(APPLE)
    message(STATUS "macOS")
    set(OS_MACOS 1)
else()
    message(STATUS "Unknown system: ${CMAKE_SYSTEM_NAME}")
endif()

include ( CheckIncludeFile )

CHECK_INCLUDE_FILE( "byteswap.h" HAVE_BYTESWAP_H )


configure_file( ${CMAKE_SOURCE_DIR}/config.h.in.cmake ${CMAKE_SOURCE_DIR}/config.h )

add_subdirectory( include )

add_subdirectory( lib )

add_subdirectory( src )

add_subdirectory( doc )

install(FILES ChangeLog DESTINATION "${CMAKE_INSTALL_DOCDIR}" COMPONENT utilities)
install(FILES README DESTINATION "${CMAKE_INSTALL_DOCDIR}" COMPONENT utilities)
install(FILES COVERAGE DESTINATION "${CMAKE_INSTALL_DOCDIR}" COMPONENT utilities)
install(FILES CREDITS DESTINATION "${CMAKE_INSTALL_DOCDIR}" COMPONENT utilities)
install(FILES AUTHORS DESTINATION "${CMAKE_INSTALL_DOCDIR}" COMPONENT utilities)

#
#  CPACK (build packages for some distros)
#
set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VENDOR "Douglas Gilbert")
set(CPACK_PROJECT_VERSION "${PROJECT_VERSION}")

set(CPACK_COMPONENTS_ALL
    runtime
    development
    utilities
)

set(CPACK_COMPONENT_RUNTIME_DESCRIPTION
    "libsmputils1 runtime library for smp_utils")
set(CPACK_COMPONENT_DEVELOPMENT_DESCRIPTION
    "libsmputils1 development library and include files for smp_utils")
set(CPACK_COMPONENTS_GROUPING IGNORE)

#
# DEB
#

set(CPACK_DEBIAN_UTILITIES_PACKAGE_NAME "smp-utils")
set(CPACK_DEBIAN_RUNTIME_PACKAGE_NAME "libsmputils1")
set(CPACK_DEBIAN_DEVELOPMENT_PACKAGE_NAME "libsmputils1-dev")
set(CPACK_DEBIAN_DEVELOPMENT_PACKAGE_DEPENDS
    "libsmputils1 (= ${PROJECT_VERSION})")
SET(CPACK_DEBIAN_PACKAGE_DESCRIPTION "SMP is the SAS Management Protocol. These utilities interact with s SMP device (e.g. in an enclosure)")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Douglas Gilbert")
SET(CPACK_DEBIAN_PACKAGE_SECTION "admin")
SET(CPACK_DEBIAN_PACKAGE_DEPENDS " ")
set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)

SET(CPACK_DEB_COMPONENT_INSTALL YES)

#
# RPM
#

set(CPACK_RPM_UTILITIES_PACKAGE_NAME "smp_utils")
set(CPACK_RPM_RUNTIME_PACKAGE_NAME "smputils1-libs")
set(CPACK_RPM_DEVELOPMENT_PACKAGE_NAME "smputils1-devel")
SET(CPACK_RPM_PACKAGE_DESCRIPTION "SMP is the SAS Management Protocol. These utilities interact with s SMP device (e.g. in an enclosure)")
SET(CPACK_RPM_PACKAGE_DEPENDS "")
SET(CPACK_RPM_PACKAGE_LICENSE "BSD-2-clause")
SET(CPACK_RPM_PACKAGE_VENDOR "Douglas Gilbert")
set(CPACK_RPM_FILE_NAME RPM-DEFAULT)

set(CPACK_RPM_COMPONENT_INSTALL YES)

SET(CPACK_GENERATOR "DEB;RPM")

include(CPack)
include(CTest)
