#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# Run xinitrc.d scripts
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for i in /etc/X11/xinit/xinitrc.d/* ; do
        if [ -x "$i" -a ! -d "$i" ]; then
            . "$i"
        fi
    done
fi

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

setxkbmap -layout "us,us(intl)" -option "grp:shifts_toggle"
if [[ $(uname) == "Linux" ]]; then
  xmodmap -e 'clear Lock' -e 'keycode 0x42 = Escape'
fi

xset +fp /usr/share/fonts/local
xset fp rehash
# Disable pc speaker
xset -b
xrandr --dpi 96

# Start a D-Bus session
source /etc/X11/xinit/xinitrc.d/30-dbus
# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID

syndaemon -t -k -i 2 -d &

dropboxd &
nm-applet --sm-disable &
keepassx &
parcellite &

exec ck-launch-session awesome
