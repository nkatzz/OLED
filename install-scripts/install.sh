#!/usr/bin/env bash

# Link logger
source logger.sh

# Installing useful packages
log_info "Installing various packages."
sudo apt-get install bison re2c scons gcc libtbb-dev python2.7-dev lua5.2-dev wget

# Check if external dependencies directory exists
if [ ! -d ../external_dependencies ]; then

    # Create install directory
    log_info "External dependencies directory not found. Creating ..."
    mkdir ../external_dependencies
    cd ../external_dependencies # Move into the directory

    # Installing clingo
    log_info "Installing clingo 4.5.4"
    mkdir clingo
    cd clingo
    wget https://sourceforge.net/projects/potassco/files/clingo/4.5.4/clingo-4.5.4-source.tar.gz
    tar -zxf clingo-4.5.4-source.tar.gz
    rm clingo-4.5.4-source.tar.gz
    cd clingo-4.5.4-source
    cp ../../../install-scripts/solve-multi.patch.0 .
    cp ../../../install-scripts/include-math.patch.0 .
    patch -p0 < include-math.patch.0
    patch -p0 < solve-multi.patch.0
    scons configure --build-dir=release
    scons --build-dir=release
    sed -i 's/CPPPATH.*/CPPPATH = ['\''\/usr\/include\/python2.7'\'','\''\/usr\/include\/lua5.1'\'']/' build/release.py
    sed -i 's/WITH_PYTHON.*/WITH_PYTHON = ['\''python2.7'\'']/' build/release.py
    sed -i 's/WITH_LUA.*/WITH_LUA = ['\''lua5.2'\'']/' build/release.py
    scons --build-dir=release pyclingo
    scons --build-dir=release luaclingo
    cd ../..

    # Installing LoMRF
    log_info "Installing LoMRF."
    wget http://users.iit.demokritos.gr/~nkatz/oled/LoMRF.tar.xz
    wget http://users.iit.demokritos.gr/~nkatz/oled/lpsolve55.tar.xz
    wget http://users.iit.demokritos.gr/~nkatz/oled/auxlib.tar.xz
    tar xf LoMRF.tar.xz
    tar xf lpsolve55.tar.xz
    tar xf auxlib.tar.xz
    cd auxlib
    sbt +publishLocal
    cd ..
    cd LoMRF
    sbt +publishLocal
    cd ..
    rm LoMRF.tar.xz
    rm lpsolve55.tar.xz
    rm auxlib.tar.xz
    cd ..

    # Installing Interval Tree
    log_info "Installing Interval Tree."
    mkdir lib
    cd lib
    wget http://users.iit.demokritos.gr/~nkatz/oled/IntervalTree.jar

    cd ../install-scripts # Done, go back into install-scripts directory
else
    log_warn "External dependencies directory exists! Moving on."
fi

# Get version from 'version.sbt'
version=`/bin/grep "^version[ ][ ]*in[ ][ ]*ThisBuild[ ][ ]*:=[ ][ ]*" "../version.sbt" | sed 's/version[ ][ ]*in[ ][ ]*ThisBuild[ ][ ]*:=[ ][ ]*\"\(.*\)\"/\1/g'`

log_info "Building OLED ${version} ..."
cd ..
sbt assembly
mv target/scala-2.11/oled-${version}.jar .
log_info "Done building OLED. The jar is located at: `pwd`"
