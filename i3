# i3 config file (v4)
#
# Please see http://i3wm.org/docs/userguide.html for a complete reference!

set $mod Mod4

font pango:Source Sans Pro 9

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# start a terminal
bindsym $mod+Return exec i3-sensible-terminal

# kill focused window
bindsym $mod+Shift+q kill

# lock screen
bindsym $mod+Shift+x exec slock

# hibernate
bindsym $mod+Shift+Control+x exec "sudo pm-suspend | slock"

# start rofi
bindsym $mod+d exec "rofi -combi-modi window,drun,run -show combi -modi combi"

# change focus
bindsym $mod+j focus left
bindsym $mod+k focus down
bindsym $mod+l focus up
bindsym $mod+semicolon focus right
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# move focused window
bindsym $mod+Shift+j move left
bindsym $mod+Shift+k move down
bindsym $mod+Shift+l move up
bindsym $mod+Shift+semicolon move right
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# split in horizontal orientation
bindsym $mod+h split h

# split in vertical orientation
bindsym $mod+v split v

# enter fullscreen mode for the focused container
bindsym $mod+f fullscreen toggle

# change container layout (stacked, tabbed, toggle split)
bindsym $mod+s layout stacking
bindsym $mod+w layout tabbed
bindsym $mod+e layout toggle split

# toggle tiling / floating
bindsym $mod+Shift+space floating toggle

# change focus between tiling / floating windows
bindsym $mod+space focus mode_toggle

# focus the parent container
bindsym $mod+a focus parent

# focus the child container
#bindsym $mod+d focus child

# workspace vars
set $workspace1 "1:"
set $workspace2 "2:"
set $workspace3 "3:"
set $workspace4 "4:"
set $workspace5 "5:"
set $workspace6 "6"
set $workspace7 "7"
set $workspace8 "8"
set $workspace9 "9"
set $workspace10 "10:🎜"

# switch to workspace
bindsym $mod+1 workspace $workspace1
bindsym $mod+2 workspace $workspace2
bindsym $mod+3 workspace $workspace3
bindsym $mod+4 workspace $workspace4
bindsym $mod+5 workspace $workspace5
bindsym $mod+6 workspace $workspace6
bindsym $mod+7 workspace $workspace7
bindsym $mod+8 workspace $workspace8
bindsym $mod+9 workspace $workspace9
bindsym $mod+0 workspace $workspace10

# move focused container to workspace
bindsym $mod+Shift+1 move container to workspace $workspace1
bindsym $mod+Shift+2 move container to workspace $workspace2
bindsym $mod+Shift+3 move container to workspace $workspace3
bindsym $mod+Shift+4 move container to workspace $workspace4
bindsym $mod+Shift+5 move container to workspace $workspace5
bindsym $mod+Shift+6 move container to workspace $workspace6
bindsym $mod+Shift+7 move container to workspace $workspace7
bindsym $mod+Shift+8 move container to workspace $workspace8
bindsym $mod+Shift+9 move container to workspace $workspace9
bindsym $mod+Shift+0 move container to workspace $workspace10

# workspace assignments
assign [class="Emacs25"] $workspace1
assign [class="Xfce4-terminal" title="main"] $workspace1
assign [class="Xfce4-terminal"] $workspace2
assign [class="Firefox"] $workspace3
assign [class="Google-chrome"] $workspace4
assign [class="robo3t"] $workspace5
assign [class="Google Play Music Desktop Player"] $workspace10
for_window [class="Spotify"] move window to workspace $workspace10

# screen assignments
workspace $workspace1 output HDMI3
workspace $workspace2 output HDMI2
workspace $workspace3 output HDMI2
workspace $workspace4 output HDMI2
workspace $workspace5 output HDMI2
workspace $workspace10 output HDMI2

# media keys
bindsym XF86AudioRaiseVolume exec --no-startup-id pactl set-sink-volume 0 +5% #increase sound volume
bindsym XF86AudioLowerVolume exec --no-startup-id pactl set-sink-volume 0 -5% #decrease sound volume
bindsym XF86AudioMute exec --no-startup-id pactl set-sink-mute 0 toggle # mute sound
bindsym XF86AudioPlay exec playerctl play-pause
bindsym XF86AudioNext exec playerctl next
bindsym XF86AudioPrev exec playerctl previous

# i3 commands
bindsym $mod+Shift+c reload
bindsym $mod+Shift+r restart
bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

# resize window (you can also use the mouse for that)
mode "resize" {
        bindsym j resize shrink width 10 px or 10 ppt
        bindsym k resize grow height 10 px or 10 ppt
        bindsym l resize shrink height 10 px or 10 ppt
        bindsym semicolon resize grow width 10 px or 10 ppt

        bindsym Left resize shrink width 10 px or 10 ppt
        bindsym Down resize grow height 10 px or 10 ppt
        bindsym Up resize shrink height 10 px or 10 ppt
        bindsym Right resize grow width 10 px or 10 ppt

        bindsym Return mode "default"
        bindsym Escape mode "default"
}

bindsym $mod+r mode "resize"

# wallpaper
exec_always feh --bg-scale "$(find ~/Dropbox/wallpapers/gradients/|shuf -n1)"

# colors
set $base00 #101218
set $base01 #1f222d
set $base02 #252936
set $base03 #7780a1
set $base04 #C0C5CE
set $base05 #d1d4e0
set $base06 #C9CCDB
set $base07 #ffffff
set $base08 #ee829f
set $base09 #f99170
set $base0A #ffefcc
set $base0B #a5ffe1
set $base0C #97e0ff
set $base0D #97bbf7
set $base0E #c0b7f9
set $base0F #fcc09e

client.focused $base0D $base0D $base00 $base01
client.focused_inactive $base02 $base02 $base03 $base01
client.unfocused $base01 $base01 $base03 $base01
client.urgent $base02 $base08 $base07 $base08

# bar
bar {
    status_command i3blocks -c ~/.config/i3/i3blocks.conf
    tray_output primary
    font pango:Source Code Pro Light 9

    colors {
        separator $base03
        background $base01
        statusline $base05
        focused_workspace $base0C $base0D $base00
        active_workspace $base02 $base02 $base07
        inactive_workspace $base01 $base01 $base03
        urgent_workspace $base08 $base08 $base07
    }
}

# screenshots
bindsym $mod+Control+3 exec shutter -f -o '/home/sam/screenshots/%y-%m-%d-%T.png' -e
bindsym $mod+Control+4 exec shutter -s -o '/home/sam/screenshots/%y-%m-%d-%T.png' -e

# startup applications
exec ~/.dropbox-dist/dropboxd
exec xrandr --output HDMI3 --auto --right-of HDMI2
exec eval `ssh-agent -s`
exec redshift -c ~/.config/redshift.conf
exec xbindkeys
exec xautolock -locker slock -time 5

exec xfce4-terminal --title main
exec xfce4-terminal
exec google-chrome
exec firefox
exec google-play-music-desktop-player
exec /opt/robo3t/bin/robo3t
