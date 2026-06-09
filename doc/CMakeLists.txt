
MESSAGE( STATUS ">> Entering doc/CMakeLists.txt" )

set(DDPT_MP
	ddpt
	ddpt_sgl
	ddptctl
)

foreach(util ${DDPT_MP})

    file(ARCHIVE_CREATE
         OUTPUT ${CMAKE_BINARY_DIR}/doc/${util}.8.gz
         PATHS ${CMAKE_SOURCE_DIR}/doc/${util}.8
         FORMAT raw
         COMPRESSION GZip
    )
    install(FILES ${CMAKE_BINARY_DIR}/doc/${util}.8.gz
            DESTINATION "${CMAKE_INSTALL_MANDIR}/man8"
            COMPONENT utilities
    )

endforeach()

install(FILES ddpt.html DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES ddpt.8.html DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES ddpt_examples.txt DESTINATION "${CMAKE_INSTALL_DOCDIR}")
install(FILES scatter_file.txt DESTINATION "${CMAKE_INSTALL_DOCDIR}")
