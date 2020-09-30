# .dotfiles

To import your home directory dotfiles or create symlinks to them, clone this
repo into your home directory.

Cloning into your home directory and not elsewhere allows you to run the `stow`
command without setting `--target` each time, because by default `stow` sets
the target directory as the parent of the current directory but there isn't yet
a convenient way to overwrite the target directory using `~` or `$HOME` in
`.stowrc`.

## Installing files using stow

All work is done after entering your cloned git directory, `~/.dotfiles`.

To install files e.g. for emacs, run:

```sh
stow emacs
```

## Importing a file into stow

To import a file, touch the file inside your stow package and use `--adopt`.
We will demonstrate with an example of importing `my_new_file.el`

```sh
# *** Ensure the working directory is the root of this git repository.
echo "${PWD/#$HOME/\~}"
~/.dotfiles

# *** Generate a toy file to import.
mkdir -p ~/.emacs.d
echo '(message "test")' >> ~/.emacs.d/my_new_file.el

# *** Create an empty placeholder in our stow directory.
mkdir -p emacs/.emacs.d
touch emacs/.emacs.d/my_new_file.el

# *** Import our toy file into stow.
file ../.emacs.d/my_new_file.el
# ../.emacs.d/my_new_file.el: ASCII text
stow --adopt emacs
file ../.emacs.d/my_new_file.el
# ../.emacs.d/my_new_file.el: symbolic link to ../.dotfiles/emacs/.emacs.d/my_new_file.el
cat emacs/.emacs.d/my_new_file.el
# (message "test")

# *** Remove all demo files.
rm {emacs,~}/.emacs.d/my_new_file.el
```
