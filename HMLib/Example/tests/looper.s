.ORG $2000
jmp main

.import halt, print, detecthw, segcard

msg:
  .byte "Hello World!",$a,$0

digits:
  .byte $0,$0,$0,$0
  
.proc segstuff: near
  sta $a1
  lda #1
  sta $a0
  lda #<digits
  sta $a2
  lda #>digits
  sta $a3
  inc digits+3
  ldy #3
  lda ($a2),Y
  cmp #10
  beq :+
  sta ($a0),Y
  rts
: lda #0
  sta ($a2),Y
  sta ($a0),Y
  inc digits+2
  ldy #2
  lda ($a2),Y
  cmp #10
  beq :+
  sta ($a0),Y
  rts
: lda #0
  sta ($a2),Y
  sta ($a0),Y
  inc digits+1
  ldy #1
  lda ($a2),Y
  cmp #10
  beq :+
  sta ($a0),Y
  rts
: lda #0
  sta ($a2),Y
  sta ($a0),Y
  inc digits
  ldy #0
  lda ($a2),Y
  cmp #10
  beq :+
  sta ($a0),Y
: rts
.endproc
  
main:
  jsr detecthw
loop:
  lda #<msg
  ldx #>msg
  jsr print
  lda segcard
  beq :+
  jsr segstuff
: jmp loop
