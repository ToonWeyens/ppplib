# ppplib

This is a copy of ppplib20 that I got my hands on early 2026.

It didn't compile so I let codex fix it:

Before

  - Log-grid support lived in one subroutine DLGLG with two ENTRY points DLGLN and DLNLG. The code relied on shared local state (IEX, ITYPE, XY/IXY via EQUIVALENCE to the COMMON)
    and a control-flow loop (GOTO 10, GOTO 40) that mutated that state in different ways depending on whether you came in through DLGLG, DLGLN, or DLNLG.
  - The sequence was: entry sets flags (IEX, ITYPE), maybe draws a box, maybe calls DLNLN for the linear part, then jumps back into the main loop to draw the log ticks using the
    same local variables. This ENTRY reuse was corrupting state and causing the segfault on mixed linear/log plots.
  - During debugging, temporary WRITE/FLUSH statements were added in these routines and DLNLN; those are gone now.

  After

  - The ENTRY-based design is removed. The log-grid logic is factored into a dedicated helper:
      - DRAW_LOG_AXIS(JCOUNT, ITYPE) computes the number of decades, reverses the axis if needed, spaces ticks, and draws them for either X (ITYPE=1) or Y (ITYPE=2). This
        encapsulates the old tick loop without shared ENTRY state.
  - The three public log-grid routines are now plain subroutines that delegate to the helper:
      - DLGLG(JX, JY): draw box, then DRAW_LOG_AXIS(JY, 2) and DRAW_LOG_AXIS(JX, 1).
      - DLGLN(JX, NY): call DLNLN(0, NY, 1, 0, 0) for the linear Y grid, then DRAW_LOG_AXIS(JX, 1) for log X.
      - DLNLG(NX, JY): call DLNLN(NX, 0, 1, 0, 0) for the linear X grid, then DRAW_LOG_AXIS(JY, 2) for log Y.
  - Temporary debug prints and markers added during investigation were removed.

  Net effect

  - Only ppplib20.f90 changed. The log-grid calls no longer share/abuse local state through ENTRY points, which eliminates the segfault and leaves behavior otherwise consistent.
