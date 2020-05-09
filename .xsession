# source my env

if [ -f ~/.zsh_my_env ]; then
    . ~/.zsh_my_env
fi

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

xset s 1200 1200

xset dpms 1200 1200 1200

xss-lock slock &

xrdb -merge ~/.Xdefaults 

# urxvtd -q -o -f &

synclient TapButton1=0 TapButton2=0 TapButton3=0 PalmDetect=1 &

XAUTH=/tmp/container_xauth
DISPLAY=:0
xauth nextract - "$DISPLAY" | sed -e 's/^..../ffff/' | xauth -f "$XAUTH" nmerge -

# $(sleep 3s && urxvtc -title "jrnl" -e sh -c "journalctl -f") &

exec xmonad
