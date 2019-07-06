# lvznumbers-mode #

[![License](https://img.shields.io/:license-gpl3-blue.svg)](./COPYING)

lvznumbers-mode is a minor mode for working with numbers and math on emacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [lvznumbers-mode](#lvznumbers-mode)
    - [Installation](#installation)
        - [Build package](#build-package)
        - [Installing package](#installing-package)
    - [Usage](#usage)
        - [Increments, Decrements](#increments-decrements)
        - [Math](#math)
            - [Examples](#examples)
        - [Shortcuts](#shortcuts)

<!-- markdown-toc end -->

## Installation ##

### Build package ###

```bash
## Create empty file "lvznumbers-mode-autoloads"
echo "" > lvznumbers-mode-autoloads

## Clear previous package build files
make clean

## Make package
make
```

### Installing package ###

```bash

cd ..

## Install package
tar -xvf "./<TAR_FILE_PACKAGE>"  -C "~/.emacs.d/elpa/"

```

## Usage ##

### Increments, Decrements ###

With cursor right before, in, or after a number pressing the combination `C-c <UP>` will increace it by one decimal unit.
Using the universal argument without adding a number `C-u C-c <UP>` the number will be increased by 4 and with double universal argument `C-u C-u C-c <UP>` the number will be increased by 16.
Using the universal argument by adding a number `C-u 30 C-c <UP>` the number will be increased by that number.

### Math ###

By selecting an area and pressing `C-M-z m` all math in selection will be replaced by their result.

#### Examples ####

By selecting the string `2 + 2 * 2 + a` and pressing `C-M-z m`, the result will be `6.0 + a`.

By selecting the string `(2 + 2) * 2 + a` and pressing `C-M-z m`, the result will be `8.0 + a`.

By selecting the string `2 + 2 * 2 * a` and pressing `C-M-z m`, the error message `Args out of range: " a", 4, 8` will appear in minibuffer.

Pressing `C-M-z m` on no selection the error message `There is no selection` will appear.

### Shortcuts ###

Key | Function | Description
--- | --- | ---
`C-c <up>` | `increment-number-at-point` | Increase the decimal number under cursor. This shortcut is defined by the `lvznumbers-increment-keycomb` variable.
`C-x +` | `increment-number-at-point` | Increase the decimal number under cursor.
`C-c <down>` | `decrement-number-at-point` | Decrease the decimal under cursor. This shortcut is defined by the `lvznumbers-decrement-keycomb`.
`C-x -` | `decrement-number-at-point` | Decrease the decimal number under cursor.
`C-x <up>` | `increment-digit-at-point` | Increase the value of the digit under(before) cursor. This shortcut is defined by the `lvznumbers-increment-digit-keycomb`.
`C-x <down>` | `decrement-digit-at-point` | Decrease the value of the digit under(before) cursor. This shortcut is defined by the `lvznumbers-decrement-digit-keycomb`.
`C-c H` | `increment-hex-at-point` | Increase the hexadecimal number under cursor. This shortcut is defined by the `lvznumbers-increment-hex-keycomb`.
`C-x H` | `decrement-hex-at-point` | Decrease the hexadecimal number under cursor. This shortcut is defined by the `lvznumbers-decrement-hex-keycomb`.
`C-c C-n C-n` | `goto-next-number` | Send cursor at the begining of the next decimal number. This shortcut is defined by the `lvznumbers-goto-next-dec-number-keycomb`.
`C-c C-n C-p` | `goto-previous-number` | Send cursor at the begining of the previous decimal number. This shortcut is defined by the `lvznumbers-goto-previous-dec-number-keycomb`.
`C-c C-v +` | `addition-with-paste` | Add the number in the kill-ring with the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-addition-with-paste-keycomb`.
`C-c C-v -` | `subtract-paste` | Subtract the number in the kill-ring from the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-subtract-paste-keycomb`.
`C-c C-v *` | `multiply-paste` | Multiply the number in the kill-ring with the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-multiply-paste-keycomb`.
`C-c C-v /` | `divide-paste` | Divide the number under cursor with  the number in the kill-ring and replace the first with the result. This shortcut is defined by the `lvznumbers-divide-paste-keycomb`.
`C-c C-v C-c +` | `addition-and-copy` | Add the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-addition-and-copy-keycomb`.
`C-c C-v C-c -` | `subtract-copy` | Subtract the number in the kill-ring from the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-subtract-copy-keycomb`.
`C-c C-v C-c *` | `multiply-copy` | Multiply the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-multiply-copy-keycomb`.
`C-c C-v C-c /` | `divide-copy` | Divide the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-divide-copy-keycomb`.
`C-M-z m` | `do-math-on-region` | Do all math in selected area.  This shortcut is defined by the `lvznumbers-do-math-on-region-keycomb`.
