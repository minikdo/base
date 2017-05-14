#!/bin/bash

ACTION=$*

if [ -n "${ACTION}" ];then
	case $ACTION in
	  Shutdown)
	  dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
	  COMPREPLY=($cur)
	  ;;
	  Reboot)
	  dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart
	  ;;
	  Suspend)
          # remove cached ssh keys
	  ssh-add -D
	  # remove cached gpg keys    
	  gpgconf --reload gpg-agent
	  dbus-send --print-reply --system --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.Suspend boolean:true	  ;;
	  LockScreen)
          # remove cached ssh keys
	  ssh-add -D
          # remove cached gpg keys
	  gpgconf --reload gpg-agent
	  light-locker-command -l      
	  #slock
	  ;;
	esac
fi
