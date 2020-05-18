#!/bin/bash

# upgrade
sudo pacman -Syu

# folder setup
mkdir -p ~/work/other-repos/

# load in configs
git clone https://github.com/sambrenner/configs.git ~/work/configs

# polybar
git clone https://github.com/unix121/i3wm-themer ~/work/other-repos/i3wm-themer
cd ~/work/other-repos/i3wm-themer
./install_arch.sh
perl -i -p -e 's/USER/sam/g;' defaults/config.yaml

cd ~
