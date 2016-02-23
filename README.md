Zeno
=====
Cursor movement with Vim key bindings. Implemented with Xlib.

Usage
-----
Cursor mode is only activated by holding down the Super mod key. There are two methods of interacting with the
cursor: search mode and move mode.

In search mode, the cursor will warp to a position halfway between a boundary
and the current cursor position(H, J, K, L are mapped to left, down, up, right).
At first (or upon reset), the boundary will be at the edges of the screen.
The search area halves each time (e.g. if you move the cursor left, then the right boundary will be at
the cursor's original position). The cursor can also be reset to the center of the screen by pressing E.

After switching to move mode,
the cursor can be manipulated at a more finer grained level using the same bindings. (Not implemented yet)

Clicking is done by pressing Enter (with the mod key held down).

To do
-----
- Refactor code and reset boundaries when the mod key is released. (can we reduce key grabbing?)
- Implement move mode.

Notes
-----
- Build depends on the C Xtst library. On Debian systems, this can be installed with `sudo apt-get install libxtst-dev`
