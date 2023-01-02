An adaptation of Guillaume Savaton's TinyHDL from:

https://github.com/aumouvantsillage/Tiny-HDL-Racket

and

http://guillaume.baierouge.fr/2020/11/08/my-first-domain-specific-language-with-racket/index.html

This version implements everything except:

- Checking that all ports are assigned
- The #langs in blog post 6
- The `use` syntax; you can just use Racket's require.

Because this is a derivative work of the original TinyHDL it is available under the same MPL 2.0 license as per the LICENSE file in this directory.
