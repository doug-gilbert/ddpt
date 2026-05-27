#!/bin/sh

echo "Entering src/cmake_del_artifacts.sh"

rm -rf \
	CMakeCache.txt \
	CMakeFiles \
	CPackConfig.cmake \
	CPackSourceConfig.cmake \
	CMakeFiles \
	cmake_install.cmake \
	DartConfiguration.tcl \
	ddpt \
	ddptctl \
	ddpt_sgl \
	Testing \
	_CPack_Packages \
	Makefile

