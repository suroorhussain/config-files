#!/bin/sh

xrdb -merge ~/.Xresources

#~/bin/mmouse.sh &
#~/bin/gogosocks &


#trayer --edge left --align right --SetDockType true --SetPartialStrut true --transparent true --tint 0x000000 --expand true --heighttype pixel --height 18 &

#nm-applet &
#konversation &
#pidgin &
xinput set-prop "PS/2 Synaptics TouchPad" "Evdev Wheel Emulation Button" 2
xinput set-prop "PS/2 Synaptics TouchPad" "Evdev Wheel Emulation" 1

exec xmonad --replace
