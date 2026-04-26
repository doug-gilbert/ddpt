cmake_minimum_required ( VERSION 3.10 )

project(ddpt VERSION 0.97.9)
# Use version number: x.y.9 to indicate pre-release of x.(y+1) 
# Example: VERSION 0.97.9 is pre-release of 0.98

option ( BUILD_SHARED_LIBS "Build using shared libraries" ON)

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

if ( ${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD" )
    MESSAGE( ">> Detected FreeBSD" )
    target_include_directories( ddpt PRIVATE /usr/local/include )
    target_include_directories( ddptctl PRIVATE /usr/local/include )
    target_include_directories( ddpt_sgl PRIVATE /usr/local/include )
endif ( )


set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION "${PROJECT_VERSION}")
SET(CPACK_GENERATOR "DEB")
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Douglas Gilbert") #required
SET(CPACK_DEBIAN_PACKAGE_SECTION "admin")
include(CPack)
