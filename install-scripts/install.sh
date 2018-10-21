#!/usr/bin/env bash

# Intalling various packages
sudo apt-get install bison re2c scons gcc libtbb-dev python2.7-dev lua5.2-dev wget
cd ..
mkdir clingo
cd clingo
wget https://sourceforge.net/projects/potassco/files/clingo/4.5.4/clingo-4.5.4-source.tar.gz
#wget http://users.iit.demokritos.gr/~nkatz/oled/clingo-4.5.4-source.tar.gz
tar -zxf clingo-4.5.4-source.tar.gz
rm clingo-4.5.4-source.tar.gz
cd clingo-4.5.4-source
cp ../../install-scripts/solve-multi.patch.0 .
cp ../../install-scripts/include-math.patch.0 .
patch -p0 < include-math.patch.0
patch -p0 < solve-multi.patch.0
scons configure --build-dir=release
scons --build-dir=release
sed -i 's/CPPPATH.*/CPPPATH = ['\''\/usr\/include\/python2.7'\'','\''\/usr\/include\/lua5.1'\'']/' build/release.py 
sed -i 's/WITH_PYTHON.*/WITH_PYTHON = ['\''python2.7'\'']/' build/release.py
sed -i 's/WITH_LUA.*/WITH_LUA = ['\''lua5.2'\'']/' build/release.py
scons --build-dir=release pyclingo
scons --build-dir=release luaclingo
cd ../../
wget http://users.iit.demokritos.gr/~nkatz/oled/LoMRF.tar.xz
wget http://users.iit.demokritos.gr/~nkatz/oled/lpsolve55.tar.xz
wget http://users.iit.demokritos.gr/~nkatz/oled/auxlib.tar.xz
tar xf LoMRF.tar.xz
tar xf lpsolve55.tar.xz
tar xf auxlib.tar.xz
cd LoMRF
sbt +publishLocal
cd ..
cd auxlib
sbt +publishLocal
cd ..
rm LoMRF.tar.xz
rm lpsolve55.tar.xz
rm auxlib.tar.xz
cd OLED
mkdir lib
cd lib
wget http://users.iit.demokritos.gr/~nkatz/oled/IntervalTree.jar
cd ..
sbt assembly
cp target/scala-2.11/oled.jar ../
rm target/scala-2.11/oled.jar
cd .. 
rm -r install-scripts