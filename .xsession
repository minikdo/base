# source my env

if [ -f ~/.zsh_my_env ]; then
    . ~/.zsh_my_env
fi

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

xset s 1200 1200

xset dpms 1200 1200 1200

xss-lock slock &

# compton &  # moved to service

xrdb -merge ~/.Xdefaults 

urxvtd -q -o -f && \
    urxvtc -title 'jrnl' -e sh -c "~/bin/my_tmux.sh" &

synclient TapButton1=0 TapButton2=0 TapButton3=0 PalmDetect=1 &

XAUTH=/tmp/container_xauth
DISPLAY=:0
xauth nextract - "$DISPLAY" | sed -e 's/^..../ffff/' | xauth -f "$XAUTH" nmerge -

exec xmonad
