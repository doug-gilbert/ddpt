#!/bin/sh

echo "Entering doc/cmake_del_artifacts.sh"

rm -rf \
	CMakeCache.txt \
	CMakeFiles \
	CMakeFiles \
	cmake_install.cmake \
	ddpt.8.gz \
	ddptctl.8.gz \
	ddpt_sgl.8.gz \
	_CPack_Packages \
	Makefile

