# Personal Emacs Configuration Files

To be used with the [Railwaycat Emacs Mac Port](https://github.com/railwaycat/homebrew-emacsmacport).

Requirements:

- brew install [pandoc](https://pandoc.org)
- brew install [w3m](https://w3m.sourceforge.net)
- mkdir ./slipbox
- [sbcl](https://www.sbcl.org) (when using `Sly`)
- `touch ~/.sly-mrepl-history` (prevent Sly printing the message 'Cannot read history file ...')
- [guile](https://www.gnu.org/software/guile/) (when using`Geiser`)
- [racket](https://racket-lang.org) (when using `racket-mode`)
- [Bitstream Vera Sans Mono font](http://legionfonts.com/fonts/bitstream-vera-sans-mono) font (the `teletext-nos` package depends on it)
- brew install sdcv; mkdir -p $HOME/.stardict/dic # afterwards copy [dictionaries](http://download.huzheng.org/babylon/) to $HOME/.stardict/dic
- if [installed](https://medium.com/@shashikant.jagtap/getting-apples-sf-mono-font-in-macos-1de5183add84), the prefered font `SF Mono` is used
- an [~/.authinfo](https://linil.wordpress.com/2008/01/18/gnus-gmail/) file.
