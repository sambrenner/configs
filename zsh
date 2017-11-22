export GOPATH=$HOME/work/gopath
export PATH=$GOPATH:$GOPATH/bin:$PATH
export ZSH=/home/sam/.oh-my-zsh

ZSH_THEME="blennel"

plugins=(git)

source $ZSH/oh-my-zsh.sh

alias rezsh="source ~/.zshrc"
alias xr="xrdb ~/.Xresources"

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
