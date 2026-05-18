#!/bin/sh

cd src || exit
./cmake_del_artifacts.sh
cd ..

cd doc || exit
./cmake_del_artifacts.sh
cd ..

rm -rf \
	build \
	CMakeCache.txt \
	CMakeFiles \
	CPackConfig.cmake \
	CPackSourceConfig.cmake \
	CMakeFiles \
	cmake_install.cmake \
	config.h \
	CTestTestfile.cmake \
	DartConfiguration.tcl \
	install_manifest.txt \
	ddpt.8.gz \
	ddptctl.8.gz \
	ddpt_sgl.8.gz \
	Testing \
	_CPack_Packages \
	Makefile

