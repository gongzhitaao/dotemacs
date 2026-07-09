#!/usr/bin/env python3
"""Summarise key frequency from Emacs dribble files (see `open-dribble-file').

Emacs writes each key event to the dribble file in one of three encodings
(keyboard.c, `record_char'):

  * a character below 0x100  -> that raw byte
  * any other character      -> " 0x<hex>", including modifier bits
  * a non-character event    -> "<symbol>", e.g. "<f5>", "<down-mouse-1>"

Nothing separates those from ordinary typed text, and the hex form has no
terminator, so "M-w" followed by typing "abc" lands in the file as
" 0x8000077abc".  We resolve that by taking the longest prefix that decodes to
a real character, and only treat "<...>" as an event when the name looks like
one (see EVENT_NAME) and " 0x.." as a character when the value is >= 0x100,
which raw bytes never reach.  A few typed sequences will still be misread; the
counts are meant to be read as proportions, not as an audit.

Usage:
  keyfreq.py [FILE|DIR ...] [--top N] [--chars] [--ngram N] [--noise]

With no arguments, reads every *.log under ~/.local/share/emacs/keylog/.
"""

from __future__ import annotations

import argparse
import re
import sys
from collections import Counter
from pathlib import Path

DEFAULT_DIR = Path.home() / ".local/share/emacs/keylog"

# Modifier bits Emacs ORs into a character (see `Character Type' in the manual).
MODIFIERS = [
    (1 << 27, "M-"),
    (1 << 26, "C-"),
    (1 << 25, "S-"),
    (1 << 24, "H-"),
    (1 << 23, "s-"),
    (1 << 22, "A-"),
]

# Base names of real, non-character events.  A leading run of modifier prefixes
# ("C-", "M-", "S-", "H-", "s-", "A-") is allowed on top of these.
EVENT_NAME = re.compile(
    r"""^(?:[CMSHsA]-)*(?:
          f\d{1,2}                        # function keys
        | up|down|left|right
        | home|end|prior|next|insert|insertchar|deletechar
        | begin|kp-[a-z0-9-]+
        | return|tab|backtab|escape|backspace|delete|space|linefeed
        | menu|print|pause|cancel|clear|help|undo|redo|again
        | XF86[A-Za-z]+
        | (?:double-|triple-|down-|drag-)?mouse-\d+
        | wheel-(?:up|down|left|right)
        | switch-frame|select-window|focus-in|focus-out|delete-frame
        | iconify-frame|make-frame-visible|move-frame|config-changed-event
        | help-echo|mouse-movement|language-change|ns-[a-z-]+|dbus-event
        | sigusr1|sigusr2|touchscreen-[a-z-]+|pinch|end-session
      )$""",
    re.VERBOSE,
)

# Events that are not keys the user pressed.
NOISE = {
    "switch-frame", "select-window", "focus-in", "focus-out", "help-echo",
    "mouse-movement", "config-changed-event", "iconify-frame", "move-frame",
    "make-frame-visible", "language-change", "delete-frame", "dbus-event",
    "sigusr1", "sigusr2", "end-session",
}

CONTROL_NAMES = {0: "C-@", 9: "TAB", 13: "RET", 27: "ESC", 127: "DEL"}

MAX_CHAR = 0x110000
ALL_MODIFIERS = sum(bit for bit, _ in MODIFIERS)

# The hex form is open-ended, so match generously and shorten until it decodes.
# Eight digits is the widest a modified character can be (0xFC10FFFF).
HEX = re.compile(rb" 0x([0-9a-f]{1,8})")
SYMBOL = re.compile(rb"<([A-Za-z][A-Za-z0-9-]{0,31})>")


def name_char(code: int) -> str:
    """Name a character code the way `key-description' would."""
    prefix = ""
    for bit, mod in MODIFIERS:
        if code & bit:
            prefix += mod
            code &= ~bit
    if code in CONTROL_NAMES:
        return prefix + CONTROL_NAMES[code]
    if code < 27:  # C-a .. C-z, minus TAB/RET carved out above
        return prefix + "C-" + chr(code + 96)
    if code < 32:
        return prefix + "C-" + r"\]^_"[code - 28]
    if code == 32:
        return (prefix + "SPC") if prefix else " "
    return prefix + chr(code)


def parse_hex(digits: str) -> tuple[int, int] | None:
    """Decode DIGITS as a character, dropping trailing digits that cannot be
    part of it.  Returns (code, digits consumed), or None if nothing decodes."""
    for end in range(len(digits), 0, -1):
        code = int(digits[:end], 16)
        # A character below 0x100 would have been written as a raw byte, so a
        # short prefix like "0x8" is never a token on its own.
        if code >= 0x100 and code & ~ALL_MODIFIERS < MAX_CHAR:
            return code, end
    return None


def tokenize(data: bytes) -> list[str]:
    """Split a dribble file's bytes into key names."""
    keys: list[str] = []
    i, n = 0, len(data)
    while i < n:
        byte = data[i]
        if byte == 0x20:
            match = HEX.match(data, i)
            parsed = parse_hex(match.group(1).decode("ascii")) if match else None
            if parsed:
                code, digits = parsed
                keys.append(name_char(code))
                i = match.start(1) + digits
                continue
        elif byte == 0x3C:  # '<'
            match = SYMBOL.match(data, i)
            if match:
                name = match.group(1).decode("ascii")
                if EVENT_NAME.match(name):
                    keys.append(f"<{name}>")
                    i = match.end()
                    continue
        keys.append(name_char(byte))
        i += 1
    return keys


def is_noise(key: str) -> bool:
    return key.startswith("<") and key[1:-1].lstrip("CMSHsA-") in NOISE


def is_plain_char(key: str) -> bool:
    """True for a key that just inserts itself: a single printable character."""
    return len(key) == 1 and (key.isprintable() or key == " ")


def collect(paths: list[Path]) -> list[str]:
    files: list[Path] = []
    for path in paths:
        if path.is_dir():
            files.extend(sorted(path.glob("*.log")))
        else:
            files.append(path)
    keys: list[str] = []
    for file in files:
        try:
            keys.extend(tokenize(file.read_bytes()))
        except OSError as err:
            print(f"skipping {file}: {err}", file=sys.stderr)
    return keys


def report(title: str, counts: Counter, total: int, top: int) -> None:
    if not counts:
        return
    print(f"\n{title}")
    width = max(len(key) for key, _ in counts.most_common(top))
    for key, count in counts.most_common(top):
        share = 100 * count / total if total else 0
        print(f"  {key:<{width}}  {count:>7,}  {share:5.2f}%")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("paths", nargs="*", type=Path, default=[DEFAULT_DIR],
                        help="dribble files or directories (default: %(default)s)")
    parser.add_argument("--top", type=int, default=40, help="rows per table")
    parser.add_argument("--chars", action="store_true",
                        help="also rank self-inserting characters individually")
    parser.add_argument("--ngram", type=int, metavar="N",
                        help="also rank the commonest runs of N consecutive keys")
    parser.add_argument("--noise", action="store_true",
                        help="keep non-key events such as <switch-frame>")
    args = parser.parse_args()

    keys = collect(args.paths)
    if not args.noise:
        keys = [key for key in keys if not is_noise(key)]
    if not keys:
        print("no key events found", file=sys.stderr)
        return 1

    total = len(keys)
    plain = Counter(key for key in keys if is_plain_char(key))
    special = Counter(key for key in keys if not is_plain_char(key))

    print(f"{total:,} key events, "
          f"{sum(plain.values()):,} self-inserting, "
          f"{sum(special.values()):,} chords and special keys")

    report("Chords and special keys", special, total, args.top)
    if args.chars:
        report("Self-inserting characters", plain, total, args.top)

    if args.ngram:
        runs = Counter(
            " ".join(keys[i:i + args.ngram])
            for i in range(len(keys) - args.ngram + 1)
            # A run of plain characters is a word, not a key sequence.
            if not all(is_plain_char(key) for key in keys[i:i + args.ngram])
        )
        report(f"Commonest runs of {args.ngram} keys", runs,
               sum(runs.values()), args.top)
    return 0


if __name__ == "__main__":
    sys.exit(main())
