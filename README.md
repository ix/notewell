# bene

# macos build instructions

First, make sure you have installed homebrew, then follow the steps below:

1. Install GTK+ via brew by running `brew install gobject-introspection gtk+ gtk+3`

2. Set the environment variable `PKG_CONFIG_PATH` via `export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"`

3. Run `stack build`

4. Once it has finished compiling use `stack run` and it should run the app.
