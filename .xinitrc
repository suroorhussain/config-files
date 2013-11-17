#!/bin/sh

xrdb -merge ~/.Xresources

~/bin/mmouse.sh
~/bin/gogosocks


trayer --edge left --align right --widthtype pixel --width 100 --SetDockType true --SetPartialStrut true --transparent true --alpha 0 --tint 0x000000 --expand true --heighttype pixel --height 10 &

nm-applet &
konversation &
pidgin &
