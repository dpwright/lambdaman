{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude   hiding (and, or)
import Data.Bits hiding (xor, bit)
import Data.Word
import Control.Monad (replicateM_)

import qualified Data.ByteString as BS

import Z80
import ZXSpectrum

data Direction = DUp | DDown | DLeft | DRight

coords :: (Word8, Word8) -> Word16
coords (x, y) = fromIntegral x .|. fromIntegral y `shiftL` 8

main = defaultMain "lambdaman" "lvtc.scr" . org 0x6000 $ mdo
  -- Load UDGs
  ldVia HL [UDG_LOC] =<< udgs

  -- Play intro sound
  call sfxStart
  gameStart <- label

  -- Set up colours
  setBorderColour Black
  setAttrs AttrDefault NoFlash Bright (Paper Black) (Ink White)
  call CL_ALL

  -- Set up score text
  ld A 2
  call CHAN_OPEN
  setCursorPos (0, 21)
  ld DE lmName
  ld BC . fromIntegral $ BS.length lambdaman
  call PR_STRING
  lambdamanScore >>= \score -> ld BC [score]
  call OUT_NUM_1
  setCursorPos (fromIntegral $ 31 - 3 - BS.length centipede, 21)
  ld HL centipedeScore
  ld HL =<< centipedeScore
  call OUT_NUM_2
  ld DE cpName
  ld BC . fromIntegral $ BS.length centipede
  call PR_STRING

  -- Initialise
  xor A               -- zeroise accumulator.
  ld [dead] A         -- set flag to say player is alive.
  ldVia A [segsLeft] numseg  -- Set segsLeft to equal numseg
  ldVia HL [plx] $ coords (15, 20) -- Set ply/plx to starting coords.

  ld HL segmnt        -- segment table.
  decLoopB 10 $ do
    ld [HL] 1         -- start off moving right.
    inc HL
    ld [HL] B         -- use B register as x coordinate.
    inc HL
    ld [HL] 0         -- start at top.
    inc HL

  -- Now we want to fill the play area with mushrooms.
  setAttrs AttrTemp NoFlash Bright (Paper Black) (Ink Green)
  decLoopB 50 $ do
    printVal AT         -- control code for AT character.
    call =<< random     -- get a 'random' number.
    and 0x0f            -- want vertical in range 0 to 15.
    printA
    call =<< random     -- want another pseudo-random number.
    and 0x1f            -- want horizontal in range 0 to 31.
    printA
    printVal 0x91       -- UDG 'B' is the mushroom graphic.

  loopForever $ do
    -- Delete the player
    call basexy
    call wspace

    -- Now we've deleted the player we can move him before redisplaying him
    -- at his new coordinates.
    call input          -- Standard speccy controls
    call vimput         -- Vim controls

    -- Now he's moved we can redisplay the player.
    call basexy         -- set the x and y positions of the player.
    call splayr         -- show player.

    -- Now for the bullet.  First let's check to see if it's hit anything.
    call bchk           -- check bullet position.
    call defbull          -- delete bullets.
    call moveb          -- move bullets.
    call bchk           -- check new position of bullets.
    call pbull          -- printVal bullets at new position.

    -- Now for the centipede segments.
    call processSegments

    halt                -- delay.

    ld A [dead]         -- was the player killed by a segment?
    and A
    jp NZ gameOver      -- player killed - lose a life.

    ld A [segsLeft]     -- was the centipede killed by the player?
    cp 0
    jp Z gameWon        -- centipede killed -- we won this time.

  vimput <- labelled $ do
    ld BC KEYS_HJKLret  -- vim keys
    in_ A [C]           -- see what keys are pressed.

    rra                 -- Enter to fire
    push AF             -- remember the value.
    call NC fire        -- it's being pressed, fire
    pop AF              -- restore accumulator.
    rra                 -- outermost bit = key 1.
    push AF             -- remember the value.
    call NC mpr         -- it's being pressed, move left.
    pop AF              -- restore accumulator.
    rra                 -- next bit along (value 2) = key 2.
    push AF             -- remember the value.
    call NC mpu         -- being pressed, so move right.
    pop AF              -- restore accumulator.
    rra                 -- next bit (value 4) = key 3.
    push AF             -- remember the value.
    call NC mpd         -- being pressed, so move down.
    pop AF              -- restore accumulator.
    rra                 -- next bit (value 8) reads key 4.
    call NC mpl         -- it's being pressed, move up.
    ret

  input <- labelled $ do
    -- QA, OP, Space.  Aggravatingly all on different parts of the keyboard...
    ld BC KEYS_TREWQ
    in_ A [C]           -- see what keys are pressed.
    rra
    call NC mpu
    ld BC KEYS_GFDSA
    in_ A [C]
    rra
    call NC mpd
    ld BC KEYS_YUIOP
    in_ A [C]
    rra
    push AF
    call NC mpr
    pop AF
    rra
    call NC mpl
    ld BC KEYS_BNMsssp
    in_ A [C]
    rra
    call NC fire
    ret

  processSegments <- labelled $ do
    ld IX segmnt        -- table of segment data.
    decLoopB 10 $ do
      push BC
      ld A [IX]         -- is segment switched on?
      inc A             -- 255=off  increments to zero.
      call NZ proseg    -- it's active  process segment.
      pop BC
      ld DE 3           -- 3 bytes per segment.
      add IX DE         -- get next segment in ix registers.
    ret

  let checkMushroom direction = do
        let move DLeft  = dec C; move DRight = inc C
            move DUp    = dec B; move DDown  = inc B
        ld BC [plx]     -- current coords.
        move direction  -- move to the position we want to check.
        call atadd      -- get address of attribute at this position.
        cp 0x44         -- mushrooms are bright (0x40) + green (0x04).
        ret Z           -- there's a mushroom - we can't move there.

  -- Move player left.
  mpl <- labelled $ do
    ld HL plx           -- remember, y is the horizontal coord!
    ld A [HL]           -- what's the current value?
    and A               -- is it zero?
    ret Z               -- yes - we can't go any further left.
    checkMushroom DLeft -- Check for mushrooms one space to the left
    dec [HL]            -- subtract 1 from y coordinate.
    ret

  -- Move player right.
  mpr <- labelled $ do
    ld HL plx           -- remember, y is the horizontal coord!
    ld A [HL]           -- what's the current value?
    cp 31               -- is it at the right edge (31)?
    ret Z               -- yes - we can't go any further right.
    checkMushroom DRight-- Check for mushrooms one space to the right
    inc [HL]            -- add 1 to y coordinate.
    ret

  -- Move player up.
  mpu <- labelled $ do
    ld HL ply           -- remember, x is the vertical coord!
    ld A [HL]           -- what's the current value?
    cp 0                -- is it at upper limit (0)?
    ret Z               -- yes - we can go no further then.
    checkMushroom DUp   -- Check for mushrooms one space up.
    dec [HL]            -- subtract 1 from x coordinate.
    ret

  -- Move player down.
  mpd <- labelled $ do
    ld HL ply           -- remember, x is the vertical coord!
    ld A [HL]           -- what's the current value?
    cp 20               -- is it already at the bottom (20)?
    ret Z               -- yes - we can't go down any more.
    checkMushroom DDown -- Check for mushrooms one space down.
    inc [HL]            -- add 1 to x coordinate.
    ret

  -- Fire a missile.
  fire <- labelled $ do
    ld A [pby]          -- bullet vertical coord.
    inc A               -- 255 is default value  increments to zero.
    ret NZ              -- bullet on screen  can't fire again.
    ld HL [plx]         -- player coordinates.
    dec H               -- 1 square higher up.
    ld [pbx] HL         -- set bullet coords.
    ret

  bchk <- labelled $ do
    ld A [pby]          -- bullet vertical.
    inc A               -- is it at 255 [default]?
    ret Z               -- yes  no bullet on screen.
    ld BC [pbx]         -- get coords.
    call atadd          -- find attribute here.
    cp 0x44             -- mushrooms are bright [64] + green [4].
    jr Z hmush          -- hit a mushroom!
    ret

  hmush <- labelled $ do
    setCursorPos ([pbx], [pby])
    call wspace          -- set INK colour to white.
    call sfxHitM         -- play "hit mushroom" sound.

  kilbul <- labelled $ do
    ldVia A [pby] 0xff   -- x coord of 255 = switch bullet off.
    ret

  -- Move the bullet up the screen 1 character position at a time.
  moveb <- labelled $ do
    ld A [pby]          -- bullet vertical.
    inc A               -- is it at 255 [default]?
    ret Z               -- yes  no bullet on screen.
    sub 2               -- 1 row up.
    ld [pby] A
    ret

  -- Set up the x and y coordinates for the player's gunbase position,
  -- this routine is called prior to display and deletion of gunbase.
  basexy <- labelled $ do
    setCursorPos ([plx], [ply])
    ret

  -- Show player at current printVal position.
  splayr <- labelled $ do
    setAttrs AttrTemp NoFlash Bright (Paper Black) (Ink Cyan)
    printVal 0x90           -- ASCII code for User Defined Graphic 'A'.
    ret

  pbull <- labelled $ do
    ld A [pby]          -- bullet vertical.
    inc A               -- is it at 255 [default]?
    ret Z               -- yes  no bullet on screen.
    call bullxy
    printVal INK
    printVal YELLOW
    printVal 0x93          -- UDG 'D' is used for player bullets.
    ret

  defbull <- labelled $ do
    ld A [pby]          -- bullet vertical.
    inc A               -- is it at 255 [default]?
    ret Z               -- yes  no bullet on screen.
    call bullxy         -- set up bullet coordinates.
  wspace <- labelled $ do
    setAttrs AttrTemp NoFlash Bright (Paper Black) (Ink White)
    printVal $ chr ' '
    ret

  -- Set up the x and y coordinates for the player's bullet position
  -- this routine is called prior to display and deletion of bullets.
  bullxy <- labelled $ do
    setCursorPos ([pbx], [pby])
    ret

  segxy <- labelled $ do
    setCursorPos ([IX+1], [IX+2])
    ret
  proseg <- labelled $ do
    call segcol             -- segment collision detection
    ld A [IX]               -- check if segment was switched off
    inc A                   -- by collision detection routine.
    ret Z                   -- it was  so this segment is now dead.
    call segxy              -- set up segment coordinates.
    call wspace             -- display a space  white ink on black.
    call segmov             -- move segment.
    call segcol             -- new segment position collision check.
    ld A [IX]               -- check if segment was switched off
    inc A                   -- by collision detection routine.
    ret Z                   -- it was  so this segment is now dead.
    call segxy              -- set up segment coordinates.
    setAttrs AttrTemp NoFlash NotBright (Paper Black) (Ink Red)
    printVal 0x92              -- UDG 'C' to display segment.
    ret
  segmov <- labelled $ do
    ld A [IX+1]         -- x coord.
    ld C A              -- GP x area.
    ld A [IX+2]         -- y coord.
    ld B A              -- GP y area.
    ld A [IX]           -- status flag.
    and A               -- is the segment heading left?
    jr Z segml          -- going left - jump to that bit of code.
  -- so segment is going right then!
  segmr <- labelled $ do
    ld A [IX+1]         -- x coord.
    cp 31               -- already at right edge of screen?
    jr Z segmd          -- yes - move segment down.
    inc A               -- look right.
    ld C A              -- set up GP y coord.
    call atadd          -- find attribute address.
    cp 0x44             -- mushrooms are bright [64] + green [4].
    jr Z segmd          -- mushroom to right  move down instead.
    inc [IX+1]          -- no obstacles  so move right.
    ret
  -- so segment is going left then!
  segml <- labelled $ do
    ld A [IX+1]         -- x coord.
    and A               -- already at left edge of screen?
    jr Z segmd          -- yes - move segment down.
    dec A               -- look right.
    ld C A              -- set up GP y coord.
    call atadd          -- find attribute address at [dispx dispy].
    cp 0x44             -- mushrooms are bright [64] + green [4].
    jr Z segmd          -- mushroom to left  move down instead.
    dec [IX+1]          -- no obstacles  so move left.
    ret
  -- so segment is going down then!
  segmd <- labelled $ do
    ld A [IX]           -- segment direction.
    xor 1               -- reverse it.
    ld [IX] A           -- store new direction.
    ld A [IX+2]         -- y coord.
    cp 20               -- already at bottom of screen?
    jr Z segmt          -- yes - move segment to the top.
  -- At this point we're moving down regardless of any mushrooms that
  -- may block the segment's path.  Anything in the segment's way will
  -- be obliterated.
    inc [IX+2]          -- haven't reached the bottom  move down.
    ret
  -- moving segment to the top of the screen.
  segmt <- labelled $ do
    xor A               -- same as ld a 0 but saves 1 byte.
    ld [IX+2] A         -- new y coordinate = top of screen.
    ret

  -- Segment collision detection.
  -- Checks for collisions with player and player's bullets.
  segcol <- labelled $ do
    ld A [plx]          -- bullet y position.
    cp [IX+1]           -- is it identical to segment x coord?
    jr NZ bulcol        -- y coords differ  try bullet instead.
    ld A [ply]          -- player y coord.
    cp [IX+2]           -- same as segment?
    jr NZ bulcol        -- x coords differ  try bullet instead.

  -- So we have a collision with the player.
  killpl <- labelled $ do
    ld [dead] A         -- set flag to say that player is now dead.
    ret

  -- Let's check for a collision with the player's bullet.
  bulcol <- labelled $ do
    ld A [pby]          -- bullet y coords.
    inc A               -- at default value?
    ret Z               -- yes  no bullet to check for.
    cp [IX+2]           -- is bullet y coord same as segment y coord?
    ret NZ              -- no  so no collision.
    ld A [pbx]          -- bullet y position.
    cp [IX+1]           -- is it identical to segment x coord?
    ret NZ              -- no - no collision this time.

    -- So we have a collision with the player's bullet.
    call defbull          -- delete bullet.
    printVal AT
    ld A [pby]          -- player bullet vertical coord.
    inc A               -- 1 line down.
    printA              -- set vertical position of mushroom.
    printVal [pbx]      -- bullet's horizontal position.
    printVal INK        -- ASCII code for INK control.
    printVal GREEN      -- 4 = colour green.
    printVal 0x91       -- UDG 'B' is the mushroom graphic.
    call kilbul         -- kill the bullet.
    ld [IX] A           -- kill the segment.
    ld HL segsLeft      -- number of segments.
    dec [HL]            -- decrement it.
    push IX
    call NZ sfxHitC     -- Play segment hit sound if this wasn't the last segment.
    pop IX
    ret

  -- Calculate address of attribute for character at (dispx, dispy).
  -- (source: https://chuntey.wordpress.com/2013/02/28/how-to-write-zx-spectrum-games-chapter-5/)
  atadd <- labelled $ do
    ld A B              -- vertical coordinate.
    rrca                -- multiplx by 32.
    rrca                -- Shifting right with carry 3 times is
    rrca                -- quicker than shifting left 5 times.
    ld E A
    and 3
    add A 88            -- 88x256=address of attributes.
    ld D A
    ld A E
    and 0xe0            -- mask low byte.
    ld E A
    ld A C              -- horizontal position.
    add A E
    ld E A              -- de=address of attributes.
    ld A [DE]           -- return with attribute in accumulator.
    ret

  gameOver <- labelled $ do
    call sfxLost
    ld HL . (+1) =<< centipedeScore
    inc [HL]
    jp gameStart
  gameWon <- labelled $ do
    call sfxWon
    ld HL =<< lambdamanScore
    inc [HL]
    jp gameStart

  -- Sounds
  sfxStart <- labelled $ do -- Start game
    playSeq [ (note F_  4, 0.111)
            , (note AS_ 4, 0.222)
            , (note C_  5, 0.111)
            , (note D_  5, 0.222)
            , (note C_  5, 0.111)
            , (note AS_ 4, 0.222)
            , (note F_  5, 1.000) ]
    ret
  sfxHitC  <- labelled $ do -- Hit centipede segment
    playSeq [ (note E_  4, 0.033)
            , (note G_  4, 0.033)
            , (note C_  5, 0.066) ]
    ret
  sfxHitM  <- labelled $ do -- Hit mushroom
    playSeq [ (note F_  3, 0.033)
            , (note FS_ 3, 0.033) ]
    ret
  sfxLost  <- labelled $ do -- Lambdaman died
    playSeq [ (note GS_ 4, 0.083)
            , (note C_  5, 0.083)
            , (note A_  4, 0.083)
            , (note B_  4, 0.083)
            , (note GS_ 4, 0.083)
            , (note AS_ 4, 0.083)
            , (note G_  4, 0.083)
            , (note A_  4, 0.083)
            , (note FS_ 4, 0.083)
            , (note GS_ 4, 0.083)
            , (note F_  4, 0.083)
            , (note G_  4, 0.083)
            , (note E_  4, 0.748) ]
    ret
  sfxWon   <- labelled $ do -- Centipede died
    playSeq [ (note G_  5, 0.222)
            , (note F_  5, 0.111)
            , (note D_  5, 0.222)
            , (note C_  5, 0.111)
            , (note D_  5, 0.222)
            , (note F_  5, 0.333)
            , (note AS_ 4, 0.777) ]
    ret

  -- Data
  let lambdaman = "lambdaman "
      centipede = " centipede"
  lmName <- labelled $ defb lambdaman
  cpName <- labelled $ defb centipede

  plx  <- labelled $ defb [0]    -- player's x coordinate.
  ply  <- labelled $ defb [0]    -- player's y coordinate.
  pbx  <- labelled $ defb [0xff] -- player's bullet coordinates.
  pby  <- labelled $ defb [0xff]
  dead <- labelled $ defb [0]    -- flag - player dead when non-zero.

  -- Table of segments.
  -- Format: 3 bytes per entry, 10 segments.
  -- byte 1: 255=segment off, 0=left, 1=right.
  -- byte 2 = x (vertical) coordinate.
  -- byte 3 = y (horizontal) coordinate.
  let numseg = 10
  segmnt <- labelled . replicateM_ (fromIntegral numseg) $ defb [0,0,0]
  segsLeft <- labelled $ defb [numseg]

  end

lambdamanScore :: Z80 Location
lambdamanScore = static "lambdamanScore" $ defb [0, 0]

centipedeScore :: Z80 Location
centipedeScore = static "centipedeScore" $ defb [0, 0]

-- | Simple pseudo-random number generator.
-- Steps a pointer through the ROM (held in seed), returning
-- the contents of the byte at that location.
-- (source: https://chuntey.wordpress.com/2013/02/28/how-to-write-zx-spectrum-games-chapter-4/)
random :: Z80 Location
random = static "random" $ mdo
  ld HL [seed]        -- Pointer
  ld A H
  and 31              -- keep it within first 8k of ROM.
  ld H A
  ld A [HL]           -- Get "random" number from location.
  inc HL              -- Increment pointer.
  ld [seed] HL
  ret
  seed <- labelled $ defb [0,0] -- TODO dw command
  end

udgs :: Z80 Location
udgs = static "udgs" $ do
  udg [ "        "
      , " ##     "
      , "   #    "
      , "   #    "
      , "   ##   "
      , "  # #   "
      , " #   #  "
      , " #    # "]
  udg [ "        "
      , "        "
      , "   ##   "
      , "  ####  "
      , " ###### "
      , "   ##   "
      , "        "
      , "        "]
  udg [ "        "
      , "   ##   "
      , "  ####  "
      , " ###### "
      , " ###### "
      , "  ####  "
      , "   ##   "
      , "        "]
  udg [ "   ##   "
      , "   ##   "
      , "   ##   "
      , "   ##   "
      , "   ##   "
      , "   ##   "
      , "  ####  "
      , " # ## # "]
