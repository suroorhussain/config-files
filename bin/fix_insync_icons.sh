#!/bin/bash

ICON_SIZES=(8x8 16x16 22x22 24x24 32x32 36x36 42x42 48x48 72x72 96x96)

temp=$(mktemp -d)
pushd "$temp"
for icon_size in "${ICON_SIZES[@]}"; do
    mkdir $icon_size
    cp /usr/share/caja/icons/hicolor/64x64/emblems/emblem-insync-* .
    for emblem in $(ls *.png); do
      convert "$emblem" -resize $icon_size $icon_size/"$emblem"
    done
    sudo mkdir -p /usr/share/icons/hicolor/$icon_size/emblems/
    sudo cp $icon_size/* /usr/share/icons/hicolor/$icon_size/emblems/
done
popd
rm -r "$temp"

sudo gtk-update-icon-cache /usr/share/icons/hicolor
