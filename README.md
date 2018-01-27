Living in Emacs
===============

- [Package Manager](#package-manager)
- [Separate Editing from Viewing](#separate-editing-from-viewing)
- [Bibliography Manager](#bibliography-manager)
- [Editing Enhancement](#editing-enhancement)
  - [Builtin Packages](#builtin-packages)
  - [MELPA Packages](#melpa-packages)

## Package Manager

**Dependencies**: Cask, pallet.

Third party libraries are managed by Cask and pallet. Every morning I open up the terminal and enter the `emacs.d` directory, type `cask update` will update and byte-compile the packages listed in a file named `Cask`. When you install packages inside Emacs, `pallet` makes sure that `Cask` is updated accordingly.

## Separate Editing from Viewing

View mode is a minor mode for **viewing** text but **not editing** it.

> When View mode is enabled, commands that do not change the buffer contents are available as usual. Kill commands insert text in kill buffers but do not delete. Most other commands beep and tell the user that the buffer is read-only.

Via `view-mode`, we can separate *editing mode* from *viewing mode* in Emacs, similar to editing/command mode separation in Vim. The main purpose is to avoid accidental modification of the buffer when we only want to do non-modifying operations, e.g., view, search, navigate, copy, etc. The major advantage over `evil-mode` is that `view-mode` is lightweight, builtin and non-obstructive.

## Bibliography Manager

**Dependencies**: helm, helm-bibtex, org-ref, PDF-tools, deft.

My configuration provide the following functionalities:

-   Viewing PDF inside Emacs.
-   Fuzzy-search bibtex entries by authors name, title, year and keywords.
-   Associate PDF file, note file with a bibtex entry, and jump among them. Specifically, <kbd>C-c r p</kbd> opens the PDF file, <kbd>C-c r n</kbd> opens the note file and <kbd>C-c r c</kbd> jumps to the bibtex entry. For example, when you are viewing the PDF in Emacs, <kbd>C-c r n</kbd> opens the note file (create one if necessary), <kbd>C-c r c</kbd> jumps to the bibtex entry for this PDF file.
-   Fuzzy-search notes.
-   Quick insert citations to org files, which can be exported to TeX, PDF or HTML with appropriate reference list.
-   Cleanup/format bibtex entries, auto construct keys for bibtex entries.

## Editing Enhancement

### Builtin Packages

-   **`whitespace`:** automatically cleans up trailing spaces, empty lines at the beginning of files and etc.

### MELPA Packages

-   **`writeroom-mode`:** the buffer content is shown in the center, especially useful when you have a wide screen. This is not compatible with `linum-mode`, which I never use.
-   **`volatile-highlights`:** highlights the change region. For example, when you redo or paste some texts to current buffer, the newly inserted texts are highlighted.
-   **`multiple-cursors`:** edit multiple lines all at ones.
-   **`rainbow-delimiters`:** change parenthesis colors according to the nested depth. Especially useful when debugging source codes with unbalanced parenthesis.
-   **`smartparens`:** close the parenthesis or any custom pairs in a smart way.
