
include ( CheckIncludeFileCXX )

set ( sourcefiles_ddpt
	ddpt.c
	ddpt_cl.c
	ddpt_com.c
	ddpt_pt.c
	ddpt_xcopy.c
)

set ( sourcefiles_ddptctl
	ddptctl.c
	ddpt_com.c
	ddpt_pt.c
	ddpt_xcopy.c
)

set ( sourcefiles_ddpt_sgl
	ddpt_sgl.cpp
	ddpt_com.c
	ddpt_pt.c
	ddpt_xcopy.c
)

set ( src_nolib_ddpt
	${CMAKE_SOURCE_DIR}/lib/sg_lib.c
	${CMAKE_SOURCE_DIR}/lib/sg_lib_data.c
	${CMAKE_SOURCE_DIR}/lib/sg_cmds_basic.c
	${CMAKE_SOURCE_DIR}/lib/sg_cmds_basic2.c
	${CMAKE_SOURCE_DIR}/lib/sg_pr2serr.c
	${CMAKE_SOURCE_DIR}/lib/sg_pt_common.c
	${CMAKE_SOURCE_DIR}/lib/sg_pt_linux.c
	${CMAKE_SOURCE_DIR}/lib/sg_pt_linux_nvme.c
	${CMAKE_SOURCE_DIR}/lib/sg_cmds_extra.c
	${CMAKE_SOURCE_DIR}/lib/sg_snt.c
)

function(common_inc_lib util_name)
    target_include_directories(${util_name} PRIVATE ${CMAKE_BINARY_DIR})
    target_include_directories(${util_name} PUBLIC ${CMAKE_SOURCE_DIR}/include)
    if ( SG_LIB_FREEBSD )
        target_include_directories( ${util_name} PRIVATE /usr/local/include )
        target_link_libraries( ${util_name} PRIVATE cam )
    endif ( SG_LIB_FREEBSD )
endfunction ()

if ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Build using shared libraries (default)" )
    find_library(LIBSGUTILS NAMES sgutils2 PATH_SUFFIXES lib)
    if ( LIBSGUTILS )
        MESSAGE( ">> Found shared library: ${LIBSGUTILS}" )
        add_executable ( ddpt ${sourcefiles_ddpt} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} )
	common_inc_lib(ddpt)
	common_inc_lib(ddptctl)
	common_inc_lib(ddpt_sgl)
	target_link_libraries(ddpt PRIVATE ${LIBSGUTILS})
	target_link_libraries(ddptctl PRIVATE ${LIBSGUTILS})
	target_link_libraries(ddpt_sgl PRIVATE ${LIBSGUTILS})
    else ( LIBSGUTILS )
        MESSAGE( ">> Did not find shared library: ${LIBSGUTILS}" )
        add_executable ( ddpt ${sourcefiles_ddpt} ${src_nolib_ddpt} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} ${src_nolib_ddpt} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${src_nolib_ddpt} )
	common_inc_lib(ddpt)
	common_inc_lib(ddptctl)
	common_inc_lib(ddpt_sgl)
        target_link_libraries(ddpt)
        target_link_libraries(ddptctl)
        target_link_libraries(ddpt_sgl)
    endif ( LIBSGUTILS )
else ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Trying to build a static executable" )
    add_executable ( ddpt ${sourcefiles_ddpt} ${src_nolib_ddpt} )
    add_executable ( ddptctl ${sourcefiles_ddptctl} ${src_nolib_ddpt} )
    add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${src_nolib_ddpt} )
    add_definitions ( -static )
    common_inc_lib(ddpt)
    common_inc_lib(ddptctl)
    common_inc_lib(ddptsgl)
    target_link_libraries(ddpt -static)
    target_link_libraries(ddptctl -static)

    ## target_link_libraries(ddpt_sgl -static)
    ## Wanted to build fully static executables but for g++ version 16 on
    ## Ubuntu 26.04 link fails saying in can't find libgcc_s_asneeded.so
    ## which is present (for gcc version 16) so it seems like a bug.
    ## If switch back to gcc-15 problem goes away. So ...
    ## Fully static C++ executables are very difficult in practice, go part way
    ## Also note that musl-gcc and clang do not have this problem.
    target_link_options(ddpt_sgl PRIVATE
        -static-libstdc++
        -static-libgcc
    )

    target_link_libraries(ddpt_sgl)
endif ( BUILD_SHARED_LIBS )

install(TARGETS ddpt ddptctl ddpt_sgl RUNTIME DESTINATION bin)

if ( OS_FREEBSD )
    target_include_directories( ddpt PRIVATE /usr/local/include )
    target_include_directories( ddptctl PRIVATE /usr/local/include )
    target_include_directories( ddpt_sgl PRIVATE /usr/local/include )
endif ( )


