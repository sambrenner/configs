export GOPATH=$HOME/work/gopath
export PATH=$GOPATH:$GOPATH/bin:$PATH
export ZSH=/home/sam/.oh-my-zsh
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"

ZSH_THEME="blennel"

plugins=(git)

source $ZSH/oh-my-zsh.sh

alias rezsh="source ~/.zshrc"
alias xr="xrdb ~/.Xresources"
alias xs="source ~/.xsessionrc"

alias emax="emacsclient -t"                      # used to be "emacs -nw"
alias semac="sudo emacsclient -t"                # used to be "sudo emacs -nw"
alias emacsc="emacsclient -c -a emacs"           # new - opens the GUI with alternate non-daemon

alias mon="xrandr --output HDMI2 --scale 1.5x1.5 --panning 2880x1620+2560+0 --fb 5440x1620 --right-of eDP1 --auto"
alias moff="xrandr --output HDMI2 --off"

alias wifion="nmcli con up id 'WeWork'"
alias wifioff="nmcli con down id 'WeWork'"

alias undock="moff && wifion"
alias dock="mon && wifioff && xs"

alias xamppon="sudo /opt/lampp/lampp start"
alias xamppoff="sudo /opt/lampp/lampp stop"

# nvm init
export NVM_DIR="/home/sam/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# .nvmrc detection
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" != "N/A" ] && [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm install
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc
