# function-args

**GNU Emacs package for showing an inline arguments hint for the C/C++ function at point**

## What and why

The traditional way of showing function arguments in Emacs is to show
them in the minibuffer.  This approach *isn't optimal*, since I have
to traverse the *whole* screen just to see the hint. After that
traverse the *whole* screen back to *find* the cursor.

Other environments such as Qt Creator and Eclipse implement the hint
as a popup located exactly where the function call is. This is the
behavior that `function-args` implements for [Emacs][emacs].

Along the way, it fixes the problem of hints for overridden functions
by offering to cycle though the available hints.

Cursor tracking, i.e. highlighting the current argument in bold and
disposing the popup when the point has left the arguments list, is
implemented for change hooks only at the moment. This means that you
have to type a char in order for the current argument to update.

### [quasi-]Related completion functionality

This package also extends `c++-mode` completion provided by [CEDET][cedet].

This functionality is unrelated logically, but related programmatically:
much of the same hacks on [CEDET][cedet] to get a better function arguments hint
can be used to get better symbol completion as well.

## Installation

### Requirements

The baseline requirement is Emacs24. It's recommended if you want to quickly try the package.

You can go as low as Emacs23.2 (the first version where CEDET is bundled with Emacs)
if you don't want to install CEDET on your own. This however isn't recommended since
C++ completion is a CPU heavy task and newer libraries are better optimized.

You can get a better experience than the baseline Emacs24 by getting
[Emacs from bzr][emacs-bzr].

You might improve it by a bit more by keeping up with
[CEDET from bzr][cedet-bzr].

### Easy start

It's easiest to install from [MELPA][melpa].  No further setup
necessary to get the basic functions working: just `M-x
function-args-mode` when you have a C/C++ file open.

After it becomes annoying to call `function-args-mode` for each file,
use this to do it automatically:

    (fa-config-default)

### Or install from here
Clone this repository:

    $ cd ~/.emacs.d/
    $ git clone https://github.com/abo-abo/function-args

Add to `.emacs`:

    (add-to-list 'load-path "~/.emacs.d/function-args")
    (require 'function-args)
    (fa-config-default)

### Additional setup (optional)

Put c++-mode as default for *.h files (improves parsing):

    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

Enable case-insensitive searching:

    (set-default 'semantic-case-fold t)

If your includes aren't located in default dirs e.g. /usr/include/
etc, then you have to do something like this:

    (semantic-add-system-include "~/Software/deal.II/include/" 'c++-mode)
    (semantic-add-system-include "/usr/local/boost_1_54_0/" 'c++-mode)

You can add this to improve the parse of macro-heavy code:

    (require 'semantic/bovine/c)
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
        "/usr/lib/gcc/x86_64-linux-gnu/4.8/include/stddef.h")

## Main functions

### `fa-show`

Show an overlay hint with current function arguments like so:

![screenshot](https://raw.github.com/abo-abo/function-args/master/doc/screenshot-1.png)

The point position is tracked and the current hint argument is updated accordingly.
After you've called it with `M-i`, you can cycle the overloaded functions with `M-n`/`M-h`.
You can dismiss the hint with `M-u` or by editing anywhere outside the function arguments.

### `fa-jump`

While the overlay hint is active, jump to the current function.
The default shortcut is `M-j`.
If the overlay isn't active,
call whatever was bound to `M-j` before (usually it's `c-indent-new-comment-line`).

### `moo-complete`

It's essentially a c++-specific version of `semantic-ia-complete-symbol`.
It behaves better, because it accounts more for function overloading and inheritance.
Also it's prettier (type parts are fontified) and faster (`helm` is used for completion).
You can invoke it with `M-o` by default.

![screenshot](https://raw.github.com/abo-abo/function-args/master/doc/screenshot-6.png)

This screenshot demonstrates the compatibility between `fa-show` and `moo-complete`, as
well as the matching power of `helm`:

![screenshot](https://raw.github.com/abo-abo/function-args/master/doc/screenshot-7.png)

### `moo-propose-virtual`

Lists the virtual members of current class' parents. The selected
candidate will be inserted at point.

![screenshot](https://raw.github.com/abo-abo/function-args/master/doc/screenshot-3.png)

### `moo-propose-override`

Lists all member functions of current class' parents. The selected
candidate will be inserted at point.

![screenshot](https://raw.github.com/abo-abo/function-args/master/doc/screenshot-2.png)

### `moo-jump-local`

Offers to jump to any tag in current buffer.
This function works for all modes supported by CEDET.
It has a specialization for C++ that flattens namespaces.
![screenshot](https://raw.github.com/abo-abo/function-args/master/doc/screenshot-4.png)

## Bugs

### Reporting

If you wish to report a bug, please try to reproduce it first
with the newest Emacs like so:

    $ emacs -q -l ./function-args.el ~/test.cc

### bzr CEDET

The latest version of [CEDET][cedet-bzr] can sometimes give more completion candidates.
I recommend to install it if you're having problems with the CEDET that comes with Emacs.

### Semantic refresh

If you're getting a completion only sometimes under the same conditions,
try `M-x semantic-force-refresh`.

### `moo-complete` offers extra (false) candidates

This comes into play when looking for a first level completion,
i.e. not prefixed with `::`, `.` or `->`. Extra candidates are
merged with correct candidates to give more completion candidates.
This is to alleviate the problem of *no* completion candidates when
parser becomes confused, i.e. it's better to have a few false
candidates in one case, than none in another case.

This isn't a severe bug, it rarely comes up depending on the naming style
and the compiler finds it really quick.

In any case, there's always `semantic-ia-complete-symbol` if you want
a different behavior.

[cedet]: http://cedet.sourceforge.net/intellisense.shtml
[cedet-bzr]: http://cedet.sourceforge.net/bzr-repo.shtml
[emacs]: http://www.gnu.org/software/emacs/
[emacs-bzr]: https://savannah.gnu.org/bzr/?group=emacs
[melpa]: http://melpa.milkbox.net/
