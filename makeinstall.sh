#! /usr/bin/env bash

# Raw makeinstall.sh for Linux users

echo "Make & install aflex"
cd ./aflex/current
gprbuild -p -P aflex
chmod +x aflex
sudo mv aflex /usr/local/bin

echo "Make & install ayacc"
cd ../../ayacc/current
gprbuild -p -P ayacc
chmod +x ayacc
sudo mv ayacc /usr/local/bin

echo "Make & install newp2ada"
cd ../../newp2ada/current
gprbuild -p -P p2ada
chmod +x p2ada
chmod +x bp2p
sudo mv p2ada /usr/local/bin
sudo mv bp2p  /usr/local/bin
