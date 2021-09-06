So, this is very, very rough and it doesn't really quite work, but here is a preliminary sketch of what the room definitions might look like as a DSL:

room: default
  width 3
  height 3
  tiles (-1 -1 -1  -1 -1 -1  -1 -1 -1)
  enter:
     tell "Something BAD happened.\nPlease report this bug!\nTHNX 1.0E6!!!"
  ;enter
;room

room: title
  width 3
  height 3
  tiles (48 49 50 51 52 53  64 65 66 67 68 69  15 15 15 15 15 15  15 15 54 55 15 15  -1 -1 -1 -1 -1 -1)
  enter:
     play title
     in 150
        no? has-started
        tell "Use the right arrow key to\nmove right and start the story...\n\n(space) or (return) to go on."
  ;enter
  trap: 0x4
     tell "Verrrry funny.\nTry going the other way.\n\n(space) or (return) to go on."
  ;trap
  trap: 5x4
     set! has-started
     tell "Welcome to Zode-trip..."
     jump hometown 6x2 right
  ;trap
;room

This corresponds closely with %rooms.rkt in the source you have. The "in 150" and "no? ..." stuff is very problematic. So I don't think this will work as it stands. The "X:/;X" block definition pairs are something I picked up from Forth. Not sure if I'm too happy about it in this context but at least it does indicate what's going on.

What do you think?
