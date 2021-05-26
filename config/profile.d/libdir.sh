LD_LIBRARY_PATH="$HOME/.local/$(uname -s)/lib:$HOME/.local/lib:$LD_LIBRARY_PATH"
LD_RUN_PATH="$HOME/.local/$(uname -s)/lib:$HOME/.local/lib:$LD_RUN_PATH"
PKG_CONFIG_PATH="$HOME/.local/$(uname -s)/lib/pkgconfig:$HOME/.local/lib/pkgconfig:$PKG_CONFIG_PATH"
export LD_LIBRARY_PATH LD_RUN_PATH PKG_CONFIG_PATH
