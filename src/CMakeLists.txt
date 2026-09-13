
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
	ddpt_cpp_com.cpp
	ddpt_com.c
	ddpt_pt.c
	ddpt_xcopy.c
)

set ( sourcefiles_ddpt_sparse
	ddpt_sparse.cpp
	ddpt_cpp_com.cpp
	ddpt_cl.c
	ddpt_com.c
	ddpt_pt.c
	ddpt_xcopy.c
)

set ( src_nolib_ddpt_com
	${CMAKE_SOURCE_DIR}/lib/sg_lib.c
	${CMAKE_SOURCE_DIR}/lib/sg_lib_data.c
	${CMAKE_SOURCE_DIR}/lib/sg_cmds_basic.c
	${CMAKE_SOURCE_DIR}/lib/sg_cmds_basic2.c
	${CMAKE_SOURCE_DIR}/lib/sg_pr2serr.c
	${CMAKE_SOURCE_DIR}/lib/sg_pt_common.c
	${CMAKE_SOURCE_DIR}/lib/sg_cmds_extra.c
	${CMAKE_SOURCE_DIR}/lib/sg_snt.c
)

# Select Hardware Abstraction Layer (HAL)
if ( PT_DUMMY )
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_dummy.c
    )
    MESSAGE( STATUS "<<>> PT_DUMMY configured, no HAL")
elseif ( SG_LIB_LINUX )
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_linux.c
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_linux_nvme.c
    )
elseif ( SG_LIB_FREEBSD )
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_freebsd.c
    )
elseif ( SG_LIB_WIN32 )
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_win32.c
    )
elseif ( SG_LIB_HAIKU )
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_haiku.c
    )
elseif ( SG_LIB_NETBSD )
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_netbsd.c
    )
elseif ( SG_LIB_SOLARIS )
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_solaris.c
    )
else ( )
    MESSAGE( STATUS "<<>> PT_DUMMY set due to fall through, no HAL")
    set ( ddpt_lib_hal
        ${CMAKE_SOURCE_DIR}/lib/sg_pt_dummy.c
    )
endif ( PT_DUMMY )

function(l_common_inc_lib util_name)
    target_include_directories(${util_name} PRIVATE ${CMAKE_BINARY_DIR})
    target_include_directories(${util_name} PRIVATE ${CMAKE_SOURCE_DIR}/include)
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
        if ( PkgConfig_FOUND )
            pkg_check_modules(SGUTILS sgutils2)
        endif ()
        if ( NOT SGUTILS_FOUND )
            find_library(SGUTILS_LIB NAMES sgutils2 PATH_SUFFIXES lib)
            find_path(SGUTILS_INC sg_lib.h PATH_SUFFIXES scsi)
            if ( SGUTILS_LIB AND SGUTILS_INC )
                set(SGUTILS_FOUND TRUE)
                set(SGUTILS_LIBRARIES ${SGUTILS_LIB})
                set(SGUTILS_INCLUDE_DIRS ${SGUTILS_INC})
            endif ()
        endif ()
        add_executable ( ddpt ${sourcefiles_ddpt} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} )
        add_executable ( ddpt_sparse ${sourcefiles_ddpt_sparse} )
	l_common_inc_lib(ddpt)
	l_common_inc_lib(ddptctl)
	l_common_inc_lib(ddpt_sgl)
	l_common_inc_lib(ddpt_sparse)
	# get following includes via svn:externals of sg3_utils include dir
	# target_include_directories(ddpt PRIVATE ${SGUTILS_INCLUDE_DIRS})
	target_link_libraries(ddpt PRIVATE ${SGUTILS_LIBRARIES})
	target_link_libraries(ddptctl PRIVATE ${SGUTILS_LIBRARIES})
	target_link_libraries(ddpt_sgl PRIVATE ${SGUTILS_LIBRARIES})
	target_link_libraries(ddpt_sparse PRIVATE ${SGUTILS_LIBRARIES})
        target_compile_definitions(ddpt_sparse PRIVATE DDPT_SPARSE=1)
    else ( LIBSGUTILS )
        MESSAGE( ">> Did not find shared library: ${LIBSGUTILS}" )
        add_executable ( ddpt ${sourcefiles_ddpt} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
        add_executable ( ddpt_sparse ${sourcefiles_ddpt_sparse} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
	l_common_inc_lib(ddpt)
	l_common_inc_lib(ddptctl)
	l_common_inc_lib(ddpt_sgl)
	l_common_inc_lib(ddpt_sparse)
        target_link_libraries(ddpt)
        target_link_libraries(ddptctl)
        target_link_libraries(ddpt_sgl)
        target_link_libraries(ddpt_sparse)
    endif ( LIBSGUTILS )
else ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Trying to build a static executable" )
    add_executable ( ddpt ${sourcefiles_ddpt} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
    add_executable ( ddptctl ${sourcefiles_ddptctl} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
    add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
    add_executable ( ddpt_sparse ${sourcefiles_ddpt_sparse} ${src_nolib_ddpt_com} ${ddpt_lib_hal} )
    add_definitions ( -static )
    l_common_inc_lib(ddpt)
    l_common_inc_lib(ddptctl)
    l_common_inc_lib(ddpt_sgl)
    l_common_inc_lib(ddpt_sparse)
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
    target_link_options(ddpt_sparse PRIVATE
        -static-libstdc++
        -static-libgcc
    )
    target_link_libraries(ddpt_sparse)

endif ( BUILD_SHARED_LIBS )

function(l_util_extras util_name)
    ## For testing it can be useful to compile C code with a C++ compiler.
    ## That can be done by uncommenting the following line:
    set_source_files_properties( ${util_name} PROPERTIES LANGUAGE CXX )

    if ( ENABLE_DEBUG )
        target_compile_definitions(${util_name} PRIVATE DEBUG)
        target_compile_options(${util_name} PRIVATE
            -Wextra -Wmisleading-indentation -Wduplicated-cond -Wlogical-op
            -Wnull-dereference -Wshadow -Wunused -Wsizeof-array-argument
            -Wduplicated-branches -Wjump-misses-init -Wparentheses)
    endif ( ENABLE_DEBUG )

endfunction ()

l_util_extras(ddpt)
l_util_extras(ddptctl)
l_util_extras(ddpt_sgl)
l_util_extras(ddpt_sparse)

install(TARGETS ddpt ddptctl ddpt_sgl ddpt_sparse RUNTIME DESTINATION bin)

