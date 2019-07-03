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
Using the universal argument without adding a number `C-u C-c <UP>` the number will be increased by 4 and with double universal argument `C-u C-u C-c <UP>` the number will be increased by 16.
Using the universal argument by adding a number `C-u 30 C-c <UP>` the number will be increased by that number.

### Shortcuts

Key | Description
--- | ---
`C-c <up>` | Increase the decimal number under cursor. This shortcut is defined by the `lvznumbers-increment-keycomb` variable.
`C-x +` | Increase the decimal number under cursor.
`C-c <down>` | Decrease the decimal under cursor. This shortcut is defined by the `lvznumbers-decrement-keycomb`.
`C-x -` | Decrease the decimal number under cursor.
`C-x <up>` | Increase the value of the digit under(before) cursor. This shortcut is defined by the `lvznumbers-increment-digit-keycomb`.
`C-x <down>` | Decrease the value of the digit under(before) cursor. This shortcut is defined by the `lvznumbers-decrement-digit-keycomb`.
`C-c H` |  Increase the hexadecimal number under cursor. This shortcut is defined by the `lvznumbers-increment-hex-keycomb`.
`C-x H` |  Decrease the hexadecimal number under cursor. This shortcut is defined by the `lvznumbers-decrement-hex-keycomb`.
`C-c C-n C-n` | Send cursor at the begining of the next decimal number. This shortcut is defined by the `lvznumbers-goto-next-dec-number-keycomb`.
`C-c C-n C-p` | Send cursor at the begining of the previous decimal number. This shortcut is defined by the `lvznumbers-goto-previous-dec-number-keycomb`.
`C-c C-v +` | Add the number in the kill-ring with the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-addition-with-paste-keycomb`.
`C-c C-v -` | Subtract the number in the kill-ring from the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-subtract-paste-keycomb`.
`C-c C-v *` | Multiply the number in the kill-ring with the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-multiply-paste-keycomb`.
`C-c C-v /` | Divide the number under cursor with  the number in the kill-ring and replace the first with the result. This shortcut is defined by the `lvznumbers-divide-paste-keycomb`.
`C-c C-v C-c +` | Add the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-addition-and-copy-keycomb`.
`C-c C-v C-c -` | Subtract the number in the kill-ring from the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-subtract-copy-keycomb`.
`C-c C-v C-c *` | Multiply the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-multiply-copy-keycomb`.
`C-c C-v C-c /` | Divide the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-divide-copy-keycomb`.
`C-M-z m` | Do all math in selected area.  This shortcut is defined by the `lvznumbers-do-math-on-region-keycomb`.