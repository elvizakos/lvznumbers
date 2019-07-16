# lvznumbers-mode #

[![License](https://img.shields.io/:license-gpl3-blue.svg)](./COPYING)

lvznumbers-mode is a minor mode for working with numbers and math on emacs.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [lvznumbers-mode](#lvznumbers-mode)
    - [Installation](#installation)
        - [Build package](#build-package)
        - [Installing package](#installing-package)
        - [Simple Installation](#simple-installation)
    - [Usage](#usage)
        - [Increments, Decrements](#increments-decrements)
        - [Math](#math)
            - [Math format](#math-format)
            - [Math Operations](#math-operations)
                - [Powers](#powers)
                - [Roots](#roots)
                - [Divisions](#divisions)
                    - [Float Divisions](#float-divisions)
                    - [Integer Divisions](#integer-divisions)
                    - [Division remander](#division-remander)
                - [Multiplications](#multiplications)
                - [Subtractions](#subtractions)
                - [Additions](#additions)
            - [Order of operations](#order-of-operations)
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

### Simple Installation ###
This can also be installed by adding somewhere in emacs init file the following line:
```lisp
(load "/path/to/lvznumbers-mode.el")
```

## Usage ##

### Increments, Decrements ###

With cursor right before, in, or after a number pressing the combination <kbd>C-c &lt;UP&gt;</kbd> will increace it by one decimal unit.
Using the universal argument without adding a number <kbd>C-u C-c &lt;UP&gt;</kbd> the number will be increased by 4 and with double universal argument <kbd>C-u C-u C-c &lt;UP&gt;</kbd> the number will be increased by 16.
Using the universal argument by adding a number <kbd>C-u 30 C-c &lt;UP&gt;</kbd> the number will be increased by the given number.

### Math ###
By selecting an area and pressing <kbd>C-M-z m</kbd> all math in selection will be replaced by their result.

#### Math format ####

#### Math Operations ####

##### Powers #####
Examples:
`3 ** 2 = 9.0`
`3 ^ 2 = 9.0`
`3 ** 3 = 27.0`
`3 ^ 3 = 27.0`

##### Roots #####
Examples:
`9 √ 2 = 3.0`
`9 // 2 = 3.0`
`9 ^ (1 / 2) =  3.0`
`9 ** (1 / 2) =  3.0`

##### Divisions #####

###### Float Divisions ######
Examples:
`10 / 3 = 3.3333333333333335`
`10 ÷ 3 = 3.3333333333333335`
`10 : 3 = 3.3333333333333335`

###### Integer Divisions ######
Examples:
`10 \ 3 = 3`

###### Division remander ######
Examples:
`10 % 3 = 1.0`

##### Multiplications #####
Examples:
`3 * 3 = 9.0`
`3 × 3 = 9.0`
`3 · 3 = 9.0`

##### Subtractions #####
Examples:
`10 - 1 = 9.0`
`10 — 1 = 9.0`

##### Additions #####
Examples:
`10 + 1 = 11.0`


#### Order of operations ####
The order of operations following the same order as in mathematics and computer programming:

  1. parenthesis
  2. powers and roots
  3. divisions, integer divisions, division remanders and multiplications
  4. additions and subtractions


#### Math Examples ####

By selecting the string `2 + 2 * 2 + a` and pressing <kbd>C-M-z m</kbd>, the result will be `6.0 + a`.

By selecting the string `(2 + 2) * 2 + a` and pressing <kbd>C-M-z m</kbd>, the result will be `8.0 + a`.

By selecting the string `2 + 2 * 2 * a` and pressing <kbd>C-M-z m</kbd>, the error message `Args out of range: " a", 4, 8` will appear in minibuffer.

Pressing <kbd>C-M-z m</kbd> when there is no selection, the error message `There is no selection` will appear in minibuffer.

![math examples](./math-examples-1.gif)

### Shortcuts ###

Key | Function | Universal argument | Description
--- | --- | --- | ---
<kbd>C-c &lt;up&gt;</kbd> | `increment-number-at-point` | If universal argument used, the number will be increased by universal arguments value. | Increase the decimal number under cursor. This shortcut is defined by the `lvznumbers-increment-keycomb` variable.
<kbd>C-x +</kbd> | `increment-number-at-point` | If universal argument used, the number will be increased by universal arguments value. | Increase the decimal number under cursor.
<kbd>C-c &lt;down&gt;</kbd> | `decrement-number-at-point` | If universal argument used, the number will be decreased by universal arguments value. |Decrease the decimal under cursor. This shortcut is defined by the `lvznumbers-decrement-keycomb`.
<kbd>C-x -</kbd> | `decrement-number-at-point` | If universal argument used, the number will be decreased by universal arguments value. |Decrease the decimal number under cursor.
<kbd>C-x &lt;up&gt;</kbd> | `increment-digit-at-point` |  | Increase the value of the digit under(before) cursor. This shortcut is defined by the `lvznumbers-increment-digit-keycomb`.
<kbd>C-x &lt;down&gt;</kbd> | `decrement-digit-at-point` | | Decrease the value of the digit under(before) cursor. This shortcut is defined by the `lvznumbers-decrement-digit-keycomb`.
<kbd>C-c H</kbd> | `increment-hex-at-point` | If universal argument used, the number will be increased by universal arguments value. | Increase the hexadecimal number under cursor. This shortcut is defined by the `lvznumbers-increment-hex-keycomb`.
<kbd>C-x H</kbd> | `decrement-hex-at-point` | If universal argument used, the number will be decreased by universal arguments value. |Decrease the hexadecimal number under cursor. This shortcut is defined by the `lvznumbers-decrement-hex-keycomb`.
<kbd>C-c C-n C-n</kbd> | `goto-next-number` | If universal argument used, it will jump to the Nth number. | Send cursor at the begining of the next decimal number. This shortcut is defined by the `lvznumbers-goto-next-dec-number-keycomb`.
<kbd>C-c C-n C-p</kbd> | `goto-previous-number` | If universal argument used, it will jump to the Nth number. | Send cursor at the begining of the previous decimal number. This shortcut is defined by the `lvznumbers-goto-previous-dec-number-keycomb`.
<kbd>C-c C-v +</kbd> | `addition-with-paste` | | Add the number in the kill-ring with the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-addition-with-paste-keycomb`.
<kbd>C-c C-v -</kbd> | `subtract-paste` | | Subtract the number in the kill-ring from the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-subtract-paste-keycomb`.
<kbd>C-c C-v *</kbd> | `multiply-paste` | | Multiply the number in the kill-ring with the number under cursor and replace the second with the result. This shortcut is defined by the `lvznumbers-multiply-paste-keycomb`.
<kbd>C-c C-v /</kbd> | `divide-paste` | | Divide the number under cursor with  the number in the kill-ring and replace the first with the result. This shortcut is defined by the `lvznumbers-divide-paste-keycomb`.
<kbd>C-c C-v C-c +</kbd> | `addition-and-copy` | | Add the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-addition-and-copy-keycomb`.
<kbd>C-c C-v C-c -</kbd> | `subtract-copy` | | Subtract the number in the kill-ring from the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-subtract-copy-keycomb`.
<kbd>C-c C-v C-c *</kbd> | `multiply-copy` | | Multiply the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-multiply-copy-keycomb`.
<kbd>C-c C-v C-c /</kbd> | `divide-copy` | | Divide the number in the kill-ring with the number under cursor and replace the first with the result. This shortcut is defined by the `lvznumbers-divide-copy-keycomb`.
<kbd>C-M-z m</kbd> | `do-math-on-region` | | Do all math in selected area.  This shortcut is defined by the `lvznumbers-do-math-on-region-keycomb`.
