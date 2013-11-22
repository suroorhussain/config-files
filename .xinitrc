#!/bin/sh

xrdb -merge ~/.Xresources

~/bin/mmouse.sh
~/bin/gogosocks


trayer --edge left --align right --SetDockType true --SetPartialStrut true --transparent true --tint 0x000000 --expand true --heighttype pixel --height 18 &

nm-applet &
konversation &
pidgin &
