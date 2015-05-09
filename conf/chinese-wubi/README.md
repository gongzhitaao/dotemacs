Chinese-Wubi
============

Chinese Wubi (五笔) input method for Emacs based on `quail` package.

## How to use

Place the `chinese-wubi.el` and the `chinese-wubi-rules.el` in your
load path, and add the following in your `.emacs` file:

```lisp
(require 'chinese-wubi)
(register-input-method "chinese-wubi" "Chinese-GB" 'quail-use-package "wubi" "wubi")
```

## TODO

1. [DONE] Currently I use the table the previous author built himself,
   which is only a fraction of the wubi rules.  So I'm converting and
   merging the the wubi tables from IBus, haifeng86 and jidian86.  The
   resulting table will cover most of the rules.
2. The previous author implemented the *user add word* function, but
   I'm not sure whether it works OK, so I've got to check that.
3. Frequency adjusting if possible
4. Submit package to ELPA.

## How I extract the tables

If you have Ibus installed with Chinese input methods,
there are two widely used wubi tables under this
directory`/usr/share/ibus-table/tables/`, **wubi-haifeng86.db** and
**wubi-jidian86.db**.  Both are in sqlite3 format, so I used

```Bash
sqlite wubi-haifeng86.db .dump > haifeng86.txt
```

This gives me all mapping rules in wubi.  There are four tables in the
db file, the one that is useful is `phrases` table, schema of which
is:

```SQL
CREATE TABLE phrases (id INTEGER PRIMARY KEY AUTOINCREMENT,
mlen INTEGER, clen INTEGER, m0 INTEGER, m1 INTEGER, m2 INTEGER, m3
INTEGER, category INTEGER, phrase TEXT, freq INTEGER, user_freq
INTEGER);
```

The `m0` through `m3` are the four keys used, with `1` being `a`, `2`
`b`, so on and so forth.

The extraction code could be found in `tables/proc.py`.

---

The package was originally created by
[Yuwen Dai](mailto:daiyuwen@freeshell.org) and
[William Xu](mailto:william.xwl@gmail.com).  A brief description by
the original author could be found
[here](http://daiyuwen.freeshell.org/gb/wubi/wubi.html).  I cleaup the
package, and add more mapping rules.
