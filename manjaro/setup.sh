#!/bin/bash

# upgrade
sudo pacman -Syu

# folder setup
mkdir -p ~/work/other-repos/

# i3wm-themer pt 1
git clone https://github.com/unix121/i3wm-themer ~/work/other-repos/i3wm-themer
cd ~/work/other-repos/i3wm-themer
./install_arch.sh

# configs pt 1
cd ~
git clone https://github.com/sambrenner/configs.git ~/work/configs
rm /home/sam/.gtkrc-2.0
ln -s /home/sam/work/configs/manjaro/gtk2 /home/sam/.gtkrc-2.0
rm /home/sam/.config/i3/config
ln -s /home/sam/work/configs/manjaro/i3 /home/sam/.config/i3/config
rm /home/sam/.config/polybar/config
ln -s /home/sam/work/configs/manjaro/polybar /home/sam/.config/polybar/config
rm /home/sam/.config/gtk-3.0/settings.ini
ln -s /home/sam/work/configs/manjaro/gtk3 /home/sam/.config/gtk-3.0/settings.ini

# i3wm-themer pt 2
cd ~/work/other-repos/i3wm-themer
yay -s polybar-git
python i3wm-themer.py --config config.yaml --load themes/010.json

# applications
yay -S xbindkeys google-chrome dropbox drive-bin brave-bin pepper-flash
sudo pacman -S emacs xorg-xmodmap otf-fira-code clementine firefox xfce4-terminal keepassxc xclip feh redshift
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# node
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | bash
nvm install --lts
npm install --global eslint_d typescript

# configs pt 2
cd ~
rm /home/sam/.emacs
ln -s /home/sam/work/configs/common/emacs .emacs
rm /home/sam/.Xmodmap
ln -s /home/sam/work/configs/manjaro/Xmodmap .Xmodmap
rm /home/sam/.xbindkeysrc
ln -s /home/sam/work/configs/manjaro/xbindkeysrc .xbindkeysrc
ln -s /home/sam/work/configs/manjaro/terminalrc /home/sam/.config/xfce4/terminal/terminalrc
rm /home/sam/.zshrc
ln -s /home/sam/work/configs/common/zsh .zshrc

# github
ssh-keygen -t rsa -b 4096 -C "samjbrenner@gmail.com"
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa
xclip -sel clip < ~/.ssh/id_rsa.pub

echo "Done, though you still need to log in to everything."
echo "The SSH key for Github is on the clipboard, or you can re-copy it with xclip -sel clip < ~/.ssh/id_rsa.pub"
