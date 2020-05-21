#!/bin/bash

RET=$(echo -e "lock\nsuspend\nreboot\npoweroff" | dmenu -nf white -sb brown -sf yellow)

case $RET in
    suspend  ) systemctl suspend  ;;
    lock     ) slock              ;;
    reboot   ) systemctl reboot   ;;
    poweroff ) systemctl poweroff ;;
    *        )                    ;;
esac
