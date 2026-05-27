
set (CMAKE_CXX_STANDARD 20 )
set (CMAKE_CXX_STANDARD_REQUIRED ON)
set (CMAKE_CXX_EXTENSIONS OFF)

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

if ( BUILD_SHARED_LIBS )
    MESSAGE( ">> Build using shared libraries (default)" )
    find_library(LIBSGUTILS NAMES sgutils2 PATH_SUFFIXES lib)
    if ( LIBSGUTILS )
        MESSAGE( ">> Found shared library: ${LIBSGUTILS}" )
        add_executable ( ddpt ${sourcefiles_ddpt} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} )
	target_include_directories(ddpt PRIVATE ${CMAKE_BINARY_DIR})
        target_include_directories(ddpt PUBLIC ${CMAKE_SOURCE_DIR}/include)
	target_include_directories(ddptctl PRIVATE ${CMAKE_BINARY_DIR})
        target_include_directories(ddptctl PUBLIC ${CMAKE_SOURCE_DIR}/include)
	target_include_directories(ddpt_sgl PRIVATE ${CMAKE_BINARY_DIR})
        target_include_directories(ddpt_sgl PUBLIC ${CMAKE_SOURCE_DIR}/include)
	target_link_libraries(ddpt PRIVATE ${LIBSGUTILS})
	target_link_libraries(ddptctl PRIVATE ${LIBSGUTILS})
	target_link_libraries(ddpt_sgl PRIVATE ${LIBSGUTILS})
    else ( LIBSGUTILS )
        MESSAGE( ">> Did not find shared library: ${LIBSGUTILS}" )
        add_executable ( ddpt ${sourcefiles_ddpt} ${src_nolib_ddpt} )
        add_executable ( ddptctl ${sourcefiles_ddptctl} ${src_nolib_ddpt} )
        add_executable ( ddpt_sgl ${sourcefiles_ddpt_sgl} ${src_nolib_ddpt} )
	target_include_directories(ddpt PRIVATE ${CMAKE_BINARY_DIR})
        target_include_directories(ddpt PUBLIC ${CMAKE_SOURCE_DIR}/include)
	target_include_directories(ddptctl PRIVATE ${CMAKE_BINARY_DIR})
        target_include_directories(ddptctl PUBLIC ${CMAKE_SOURCE_DIR}/include)
	target_include_directories(ddpt_sgl PRIVATE ${CMAKE_BINARY_DIR})
        target_include_directories(ddpt_sgl PUBLIC ${CMAKE_SOURCE_DIR}/include)
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
    target_include_directories(ddpt PRIVATE ${CMAKE_SOURCE_DIR})
    target_include_directories(ddpt PUBLIC ${CMAKE_SOURCE_DIR}/include)
    target_include_directories(ddptctl PRIVATE ${CMAKE_SOURCE_DIR})
    target_include_directories(ddptctl PUBLIC ${CMAKE_SOURCE_DIR}/include)
    target_include_directories(ddpt_sgl PRIVATE ${CMAKE_SOURCE_DIR})
    target_include_directories(ddpt_sgl PUBLIC ${CMAKE_SOURCE_DIR}/include)
    target_link_libraries(ddpt -static)
    target_link_libraries(ddptctl -static)
    # Fully static C++ executables are very difficult in practice, go part way
    ## target_link_libraries(ddpt_sgl -static)
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


