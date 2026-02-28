echo "=== $(date) ===" >> $HOME/.xsession-errors

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# source my env
if [ -f ~/.zsh_my_env ]; then
    . ~/.zsh_my_env
fi

# Set screensaver values
xset s 240 540
xset dpms 1200 1200 1200

# Turn off system beep in console:
xset b off
xset b 0 0 0

# 1. Export environment variables to systemd
dbus-update-activation-environment --systemd DISPLAY XAUTHORITY

# 2. Start your proxy target (which pulls in graphical-session.target)
systemctl --user start xsession.target

#xss-lock -n dim slock &
systemctl --user import-environment XDG_SESSION_ID && \
  systemctl --user start xss-lock.service

# needed ?
xrdb -merge ~/.Xdefaults 

synclient TapButton1=0 TapButton2=0 TapButton3=0 PalmDetect=1 AccelFactor=2 MaxSpeed=2 &

#XAUTH=/tmp/container_xauth
#DISPLAY=:0
#xauth nextract - "$DISPLAY" | sed -e 's/^..../ffff/' | xauth -f "$XAUTH" nmerge -

pidof picom 1> /dev/null || picom --backend xrender &

exec $HOME/.xmonad/xmonad-x86_64-linux
