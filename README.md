#My settings
These are my settings file for linux.
Feel free to use them.

make sure that the files are placd in ``~/.gitfiles/dotfiles``
and then run ``./create_symlink``

the ``.gitignore`` file (not the global one) is not one of my settings, but rather
something for this git repo in particular.

###Install script
```
mkdir ~/.gitfiles; cd ~/.gitfiles; git clone https://github.com/hugoNikanor/gitfiles.git; cd ~/.gitfiles/dotfiles; ./create_symlink
```

---

*As if anyone ever would want to use my settings for anything...*

## Problems
### Kill Window
Focus isn't set to the same place as xmonad show where it is, causing multiple
kill calls to be misdirected.

If a window that isn't a tmux client has a tmux client name, then the wrong kill
script is called. This can happen if a *"regular"* terminal is attached to a
tmux session, but later detached.
