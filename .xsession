echo "=== $(date) ===" >> $HOME/.xsession-errors

# source my env
if [ -f ~/.zsh_my_env ]; then
    . ~/.zsh_my_env
fi

xset s 1200 1200

xset dpms 1200 1200 1200

# Turn off system beep in console:
xset b off
xset b 0 0 0

xss-lock slock &
systemctl --user import-environment XDG_SESSION_ID

# disable ctrl
#xmodmap -e "keycode 37="

# needed ?
xrdb -merge ~/.Xdefaults 

synclient TapButton1=0 TapButton2=0 TapButton3=0 PalmDetect=1 AccelFactor=2 MaxSpeed=2 &

XAUTH=/tmp/container_xauth
DISPLAY=:0
xauth nextract - "$DISPLAY" | sed -e 's/^..../ffff/' | xauth -f "$XAUTH" nmerge -

pidof picom 1> /dev/null || picom &

exec $HOME/.xmonad/xmonad-x86_64-linux
