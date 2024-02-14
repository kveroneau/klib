.export print, detecthw, halt

.export segcard

__CARDIO__ = $c800

termcard: .byte $0
syscard: .byte $0
segcard: .byte $0

.proc halt: near
  lda #$40
  sta $c004
.endproc

.proc hexout: near
  tya
  pha
  ldy #5
  pla
  tay
  rts
.endproc

.proc termout: near
  lda termcard
  sta $c105
  rts
.endproc

.proc chrout: near
  ldy termcard
  cpy #0
  bne :+
  lda #'X'
  sta $c104
  rts
: jmp termout
.endproc

.proc print: near
  sta $c6
  stx $c7
  lda termcard
  cmp #0
  bne :+
  rts
: sta $c9
  lda #4
  sta $c8
  ldx #0
  ldy #0
: lda ($c6),Y
  beq :+
  sta ($c8,X)
  iny
  bne :-
: rts  
.endproc

.proc setcard: near
  tya
  clc
  adc #$c0
  rts
.endproc

.proc detecthw: near
  lda #<__CARDIO__
  sta $c8
  lda #>__CARDIO__
  sta $c9
  ldy #0
: lda ($c8),Y
  cmp #$42
  beq :++
  cmp #$99
  beq :+++
  cmp #42
  beq :++++
: cpy #7
  beq :++++
  iny
  bne :--
: jsr setcard
  sta termcard
  bne :--
: jsr setcard
  sta syscard
  bne :---
: jsr setcard
  sta segcard
  bne :----
: rts
.endproc
