# lvznumbers-mode

lvznumbers-mode is a minor mode for working with numbers and math on emacs.

## Installation

### Build package

```bash
## Create empty file "lvznumbers-mode-autoloads"
echo "" > lvznumbers-mode-autoloads

## Clear previous package build files
make clean

## Make package
make
```

### Installing package

```bash

cd ..

## Install package
tar -xvf "./<TAR_FILE_PACKAGE>"  -C "~/.emacs.d/elpa/"

```

## Usage

### Increments, Decrements

With cursor right before, in, or after a number pressing the combination `C-c <UP>` will increace it by one decimal unit.