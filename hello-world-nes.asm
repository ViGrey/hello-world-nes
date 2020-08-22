; Copyright (C) 2020, Vi Grey
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS ``AS IS'' AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
; ARE DISCLAIMED. IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
; SUCH DAMAGE.

; Constants
PPU_CTRL            = $2000
PPU_MASK            = $2001
PPU_STATUS          = $2002
PPU_OAM_ADDR        = $2003
PPU_OAM_DATA        = $2004
PPU_SCROLL          = $2005
PPU_ADDR            = $2006
PPU_DATA            = $2007
APU_DMC             = $4010
OAM_DMA             = $4014
APU_FRAME_COUNTER   = $4017
CALLBACK            = $BFFA

; Variables
.enum $0000
  addr  dsb 2
.ende

; iNES header
  .db "NES", $1A
  .db $01 ; 16KB PRG
  .db $01 ; 8KB CHR
  .db $00 ; Horizontal Mirroring
  .db $00
  .db 0, 0, 0, 0, 0, 0, 0, 0

; Set PRG start to 0xC000
.base $8000

RESET:
  sei ; Disable Interrupts
  cld ; Set Decimal Mode to 0
  lda #%01000000
  sta APU_FRAME_COUNTER ; Inhibit IRQ
  ldx #$FF
  txs ; Set stack register to 0xFF
  inx ; Increment X from 0xFF to 0x00
  stx PPU_MASK ; No sprites or background tiles on leftmost 8 pixels
  stx PPU_CTRL
  stx APU_DMC ; Disable APU DMC

InitialVWait:
  lda PPU_STATUS
  bpl InitialVWait ; Wait for vblank to continue
InitialVWait2:
  lda PPU_STATUS
  bpl InitialVWait2 ; Wait for vblank to continue
; PPU is now ready

InitializePPUNametableRAM:
  lda PPU_STATUS
  lda #$20
  sta PPU_ADDR
  lda #$00
  sta PPU_ADDR
  ldy #$08
InitializePPUNametableRAMLoop:
  sta PPU_DATA
  inx
  bne InitializePPUNametableRAMLoop
    dey
    bne InitializePPUNametableRAMLoop

SetPalette:
  lda PPU_STATUS
  lda #$3F
  sta PPU_ADDR
  ldx #$00
  stx PPU_ADDR
  lda #$0F
  sta PPU_DATA
  sta PPU_DATA
  sta PPU_DATA
  lda #$20
  sta PPU_DATA

DrawHelloWorld:
  ldy PPU_STATUS
  sta PPU_ADDR
  lda #$42
  sta PPU_ADDR
DrawHELLoop:
  inx ; H E L
  stx PPU_DATA
  cpx #$03
  bcc DrawHELLoop
DrawLOCommaLoop:
  stx PPU_DATA
  inx ; O L , W
  cpx #$06
  bcc DrawLOCommaLoop
DrawHelloWorldContinue:
  lda #$00 ; Blank Space
  sta PPU_DATA
  stx PPU_DATA
  ldy #$04 ; O
  sty PPU_DATA
  inx ; R
  stx PPU_DATA
  dey ; L
  sty PPU_DATA
  inx ; D
  stx PPU_DATA
  inx ; !
  stx PPU_DATA

  lda #%10000000
  sta PPU_CTRL

; Infinite Loop
Forever:
  jmp Forever

NMI:
  ldx #$00
  stx PPU_OAM_ADDR
  lda PPU_STATUS
Draw:
  lda #%00011110
  sta PPU_MASK
ResetScroll:
  stx PPU_SCROLL
  stx PPU_SCROLL
EnableNMI:
  lda #%10000000
  sta PPU_CTRL
  rti

  ; Pad everything up to FFFA with 0x00 bytes
  .pad CALLBACK, #$00

  .dw  NMI    ; NMI Address
  .dw  RESET  ; Reset Address
  .dw  0      ; IRQ Address

.base $0000
  .incbin "tileset.chr"
