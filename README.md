Zeno
=====
Cursor movement with Vim key bindings. Implemented with Xlib.

Intro
-----
Cursor mode is only activated by holding down the Super mod key. There are two methods of interacting with the
cursor: search mode and move mode.

In search mode, the cursor will warp to a position halfway between a boundary
and the current cursor position(H, J, K, L are mapped to left, down, up, right).
At first (or upon reset), the boundary will be at the edges of the screen.
The search area halves each time (e.g. if you move the cursor left, then the right boundary will be at
the cursor's original position). The cursor can also be reset to the center of the screen by pressing E.

Clicking is done with N (or Enter) and M (for left and right clicking).

After switching to move mode,
the cursor can be manipulated at a more finer grained level using the same bindings. (Not implemented yet)

Command Table
-----
Note: All commands must have the Win key (Super mod key) held down.

| Command | Description |
| :-----: | ----------- |
| E | Centers the mouse cursor in the screen |
| J | Warps cursor upwards, halfway between the top bound and current position. |
| K | Warps cursor downwards, halfway between the bottom bound and current position. |
| H | Warps cursor leftwards, halfway between the left bound and current position. |
| L | Warps cursor rightwards, halfway between the right bound and current position. |
| N or Enter | Left click |
| M | Right click |

After each left/right/up/down move, the right/left/bottom/top bound moves left/right/up/down to your previous y/y/x/x coordinate.

To do
-----
- Refactor code and reset boundaries when the mod key is released. (can we reduce key grabbing?)
- Implement move mode.
- Known issue: holding LMB button (currently 'N') causes repeated clicking and not the desired 'click-and-hold' behavior.

Notes
-----
- Build depends on the C Xtst library. On Debian systems, this can be installed with `sudo apt-get install libxtst-dev`

