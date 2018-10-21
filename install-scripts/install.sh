#!/usr/bin/env bash

# Intalling various packages
sudo apt-get install bison re2c scons gcc libtbb-dev python2.7-dev lua5.2-dev wget

cd ..
mkdir clingo
cd clingo
wget https://sourceforge.net/projects/potassco/files/clingo/4.5.4/clingo-4.5.4-source.tar.gz
tar -zxf clingo-4.5.4-source.tar.gz
rm clingo-4.5.4-source.tar.gz
cd clingo-4.5.4-source
cp ../../clingo-install-scripts/solve-multi.patch.0 .
cp ../../clingo-install-scripts/include-math.patch.0 .
patch -p0 < include-math.patch.0
patch -p0 < solve-multi.patch.0
scons configure --build-dir=release
scons --build-dir=release
sed -i 's/CPPPATH.*/CPPPATH = ['\''\/usr\/include\/python2.7'\'','\''\/usr\/include\/lua5.1'\'']/' build/release.py 
sed -i 's/WITH_PYTHON.*/WITH_PYTHON = ['\''python2.7'\'']/' build/release.py
sed -i 's/WITH_LUA.*/WITH_LUA = ['\''lua5.2'\'']/' build/release.py
scons --build-dir=release pyclingo
scons --build-dir=release luaclingo
