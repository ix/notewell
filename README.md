# Notewell

![](meta/screenshot.png)
_Notewell rendering its own README._

Notewell is a cross-platform Markdown editor written in Haskell using `gi-gtk-declarative`.

It uses `cmark-gfm` to parse Markdown blazingly fast and renders to a TextView -- so there are no browsers involved here.

Notewell is a __work in progress__ and so things might break or simply not be implemented at all. 

Releases will be made when the software meets a criteria of basic usability.

## macOS build instructions

First, make sure you have installed homebrew, then follow the steps below:

1. Install GTK+3 and necessary libraries via brew by running `brew install gobject-introspection gtk+ gtk+3 librsvg libffo`

2. Set the environment variable `PKG_CONFIG_PATH` via `export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"`

3. Run `stack build`

4. Once it has finished compiling use `stack run` and it should run the app.
