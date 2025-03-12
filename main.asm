
 processor 6502
	org $801
StartBlock801:
	; Starting new memory block at $801
	.byte $b ; lo byte of next line
	.byte $8 ; hi byte of next line
	.byte $0a, $00 ; line 10 (lo, hi)
	.byte $9e, $20 ; SYS token and a space
	.byte   $32,$30,$36,$34
	.byte $00, $00, $00 ; end of program
	; Ending memory block at $801
EndBlock801:
	org $810
StartBlock810:
	; Starting new memory block at $810
kotek1
	jmp block1
str_p1	= $02
str_p2	= $04
str_p3	= $08
str_i	dc.b	0
str_j	dc.b	0
str_li	dc.b	0
str_b	dc.b	0
str_c	dc.b	0
str_num	dc.w	0
str_chars		dc.b	"0123456789ABCDEF"
	dc.b	0
Screen_p1	= $16
Screen_sp	= $0B
Screen_cp	= $0D
Screen_p2	= $10
Screen_i	dc.b	0
Screen_j	dc.b	0
Screen_x	dc.b	0
Screen_y	dc.b	0
Screen_c	dc.b	0
Screen_i2	dc.w	0
Screen_i1	dc.w	0
Screen_tab40	dc.w $00, $28, $50, $78, $a0, $c8, $f0, $118
	dc.w $140, $168, $190, $1b8, $1e0, $208, $230, $258
	dc.w $280, $2a8, $2d0, $2f8, $320, $348, $370, $398
	dc.w $3c0
Screen_numStr	dc.b	 
	org Screen_numStr+16
Memory_p	= $12
Memory_v	dc.b	0
Memory_v2	dc.b	0
i	dc.b	$00
sprite_x	dc.w	$a0
sprite_y	dc.b	$64
joystick_dir	dc.b	0
playerMoveSprite	dc.b $0, $0, $0, $0, $0, $1, $1, $1
	dc.b $1, $1
playerMoveSpriteLeft	dc.b $2, $2, $2, $2, $2, $3, $3, $3
	dc.b $3, $3
playerMovement	dc.b	$00
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
initdiv16x8_divisor = $4C     ;$59 used for hi-byte
initdiv16x8_dividend = $4E	  ;$fc used for hi-byte
initdiv16x8_remainder = $50	  ;$fe used for hi-byte
initdiv16x8_result = $4E ;save memory by reusing divident to store the result
divide16x8
	lda #0	        ;preset remainder to 0
	sta initdiv16x8_remainder
	sta initdiv16x8_remainder+1
	ldx #16	        ;repeat for each bit: ...
divloop16:	asl initdiv16x8_dividend	;dividend lb & hb*2, msb -> Carry
	rol initdiv16x8_dividend+1
	rol initdiv16x8_remainder	;remainder lb & hb * 2 + msb from carry
	rol initdiv16x8_remainder+1
	lda initdiv16x8_remainder
	sec
	sbc initdiv16x8_divisor	;substract divisor to see if it fits in
	tay	        ;lb result -> Y, for we may need it later
	lda initdiv16x8_remainder+1
	sbc initdiv16x8_divisor+1
	bcc skip16	;if carry=0 then divisor didn't fit in yet
	sta initdiv16x8_remainder+1	;else save substraction result as new remainder,
	sty initdiv16x8_remainder
	inc initdiv16x8_result	;and INCrement result cause divisor fit in 1 times
skip16
	dex
	bne divloop16
	rts
end_procedure_init16x8div
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init16x8mul
	;    Procedure type : Built-in function
	;    Requires initialization : no
mul16x8_num1Hi = $4C
mul16x8_num1 = $4E
mul16x8_num2 = $50
mul16x8_procedure
	lda #$00
	ldy #$00
	beq mul16x8_enterLoop
mul16x8_doAdd
	clc
	adc mul16x8_num1
	tax
	tya
	adc mul16x8_num1Hi
	tay
	txa
mul16x8_loop
	asl mul16x8_num1
	rol mul16x8_num1Hi
mul16x8_enterLoop
	lsr mul16x8_num2
	bcs mul16x8_doAdd
	bne mul16x8_loop
	rts
end_procedure_init16x8mul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : init8x8div
	;    Procedure type : Built-in function
	;    Requires initialization : no
div8x8_c = $4C
div8x8_d = $4E
div8x8_e = $50
	; Normal 8x8 bin div
div8x8_procedure
	lda #$00
	ldx #$07
	clc
div8x8_loop1
	rol div8x8_d
	rol
	cmp div8x8_c
	bcc div8x8_loop2
	sbc div8x8_c
div8x8_loop2
	dex
	bpl div8x8_loop1
	rol div8x8_d
	lda div8x8_d
div8x8_def_end
	rts
end_procedure_init8x8div
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initeightbitmul
	;    Procedure type : Built-in function
	;    Requires initialization : no
multiplier = $4C
multiplier_a = $4E
multiply_eightbit
	cpx #$00
	beq mul_end
	dex
	stx $4E
	lsr
	sta multiplier
	lda #$00
	ldx #$08
mul_loop
	bcc mul_skip
mul_mod
	adc multiplier_a
mul_skip
	ror
	ror multiplier
	dex
	bne mul_loop
	ldx multiplier
	rts
mul_end
	txa
	rts
initeightbitmul_multiply_eightbit2
	rts
end_procedure_initeightbitmul
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initjoystick
	;    Procedure type : Built-in function
	;    Requires initialization : no
joystickup: .byte 0
joystickdown: .byte 0
joystickleft: .byte 0
joystickright: .byte 0
joystickbutton: .byte 0
callJoystick
	lda #0
	sta joystickup
	sta joystickdown
	sta joystickleft
	sta joystickright
	sta joystickbutton
	lda #%00000001 ; mask joystick up mment
	bit $50      ; bitwise AND with address 56320
	bne joystick_down       ; zero flag is not set -> skip to down
	lda #1
	sta joystickup
joystick_down
	lda #%00000010 ; mask joystick down movement
	bit $50      ; bitwise AND with address 56320
	bne joystick_left       ; zero flag is not set -> skip to down
	lda #1
	sta joystickdown
joystick_left
	lda #%00000100 ; mask joystick left movement
	bit $50      ; bitwise AND with address 56320
	bne joystick_right       ; zero flag is not set -> skip to down
	lda #1
	sta joystickleft
joystick_right
	lda #%00001000 ; mask joystick up movement
	bit $50      ; bitwise AND with address 56320
	bne joystick_button       ; zero flag is not set -> skip to down
	lda #1
	sta joystickright
joystick_button
	lda #%00010000 ; mask joystick up movement
	bit $50      ; bitwise AND with address 56320
	bne callJoystick_end       ; zero flag is not set -> skip to down
	lda #1
	sta joystickbutton
callJoystick_end
	rts
	rts
end_procedure_initjoystick
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initmoveto
	;    Procedure type : Built-in function
	;    Requires initialization : no
	jmp initmoveto_moveto3
screenmemory =  $fe
colormemory =  $fb
screen_x = $4C
screen_y = $4E
SetScreenPosition
	sta screenmemory+1
	lda #0
	sta screenmemory
	ldy screen_y
	beq sydone
syloop
	clc
	adc #40
	bcc sskip
	inc screenmemory+1
sskip
	dey
	bne syloop
sydone
	ldx screen_x
	beq sxdone
	clc
	adc screen_x
	bcc sxdone
	inc screenmemory+1
sxdone
	sta screenmemory
	rts
initmoveto_moveto3
	rts
end_procedure_initmoveto
	; NodeProcedureDecl -1
	; ***********  Defining procedure : initprintstring
	;    Procedure type : Built-in function
	;    Requires initialization : no
print_text = $4C
print_number_text: .dc "    ",0
printstring
	ldy #0
printstringloop
	lda (print_text),y
	cmp #0 ;keep
	beq printstring_done
	cmp #64
	bcc printstring_skip
	sec
	sbc #64
printstring_skip
	sta (screenmemory),y
	iny
	dex
	cpx #0
	beq printstring_done
	jmp printstringloop
printstring_done
	rts
end_procedure_initprintstring
	;*
; //
; //	Returns the length of a string. Note that
; //	this will only work for strings <256 bytes. 
; //
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : str_strlen
	;    Procedure type : User-defined procedure
str_strlen_block4
str_strlen
	lda #$0
	; Calling storevariable on generic assign expression
	sta str_li
str_strlen_while5
str_strlen_loopstart9
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy str_li
	lda (str_p3),y
	; cmp #$00 ignored
	beq str_strlen_edblock8
str_strlen_ctb6: ;Main true block ;keep 
	; Test Inc dec D
	inc str_li
	jmp str_strlen_while5
str_strlen_edblock8
str_strlen_loopend10
	lda str_li
	rts
end_procedure_str_strlen
	;*
; // Reverses a string
; // 
; // *
	; NodeProcedureDecl -1
	; ***********  Defining procedure : str_reverse
	;    Procedure type : User-defined procedure
str_reverse_block13
str_reverse
	lda str_p2
	ldx str_p2+1
	sta str_p3
	stx str_p3+1
	jsr str_strlen
	; Calling storevariable on generic assign expression
	sta str_c
	; 8 bit binop
	; Add/sub where right value is constant number
	sec
	sbc #$1
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta str_j
	lda #$0
	; Calling storevariable on generic assign expression
	sta str_i
str_reverse_while14
str_reverse_loopstart18
	; Binary clause Simplified: LESS
	lda str_i
	; Compare with pure num / var optimization
	cmp str_j;keep
	bcs str_reverse_edblock17
str_reverse_ctb15: ;Main true block ;keep 
	; Load pointer array
	ldy str_i
	lda (str_p2),y
	; Calling storevariable on generic assign expression
	sta str_b
	; Load pointer array
	ldy str_j
	lda (str_p2),y
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_i ; optimized, look out for bugs
	sta (str_p2),y
	lda str_b
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_j ; optimized, look out for bugs
	sta (str_p2),y
	; Test Inc dec D
	dec str_j
	; Test Inc dec D
	inc str_i
	jmp str_reverse_while14
str_reverse_edblock17
str_reverse_loopend19
	rts
end_procedure_str_reverse
	;*
; //	Converts a number to a string in base b
; //	example:
; //	<code>
; //itoa(1234, p1, 16); 
; // coverts "1234" to a hexadecimal string stored in p1
; //	</code>
; //
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : str_itoa
	;    Procedure type : User-defined procedure
str_itoa_block22
str_itoa
	lda #$0
	; Calling storevariable on generic assign expression
	sta str_i
	; Binary clause INTEGER: EQUALS
	lda str_num+1   ; compare high bytes
	cmp #$00 ;keep
	bne str_itoa_edblock26
	lda str_num
	cmp #$00 ;keep
	bne str_itoa_edblock26
	jmp str_itoa_ctb24
str_itoa_ctb24: ;Main true block ;keep 
	lda #$30
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (str_p1),y
	
; // Simply 0
	lda #$0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$1
	sta (str_p1),y
	rts
str_itoa_edblock26
str_itoa_while29
str_itoa_loopstart33
	; Binary clause INTEGER: NOTEQUALS
	lda str_num+1   ; compare high bytes
	cmp #$00 ;keep
	beq str_itoa_pass138
	jmp str_itoa_ctb30
str_itoa_pass138
	lda str_num
	cmp #$00 ;keep
	beq str_itoa_edblock32
	jmp str_itoa_ctb30
str_itoa_ctb30: ;Main true block ;keep 
	; Load Byte array
	; CAST type NADA
	; Modulo
	lda str_b
str_itoa_val_var40 = $54
	sta str_itoa_val_var40
	ldy str_num+1 ;keep
	lda str_num
	sec
str_itoa_modulo41
	sbc str_itoa_val_var40
	bcs str_itoa_modulo41
	adc str_itoa_val_var40
	tax
	lda str_chars,x 
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_i ; optimized, look out for bugs
	sta (str_p1),y
	; Test Inc dec D
	inc str_i
	; 16x8 div
	ldy str_num+1 ;keep
	lda str_num
	sta initdiv16x8_dividend
	sty initdiv16x8_dividend+1
	ldy #0
	lda str_b
	sta initdiv16x8_divisor
	sty initdiv16x8_divisor+1
	jsr divide16x8
	lda initdiv16x8_dividend
	ldy initdiv16x8_dividend+1
	; Calling storevariable on generic assign expression
	sta str_num
	sty str_num+1
	jmp str_itoa_while29
str_itoa_edblock32
str_itoa_loopend34
	lda #$0
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy str_i ; optimized, look out for bugs
	sta (str_p1),y
	
; // null-term string    
	lda str_p1
	ldx str_p1+1
	sta str_p2
	stx str_p2+1
	jsr str_reverse
	rts
end_procedure_str_itoa
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Screen_PrintString
	;    Procedure type : User-defined procedure
Screen_PrintString_block42
Screen_PrintString
	; Generic 16 bit op
	ldy #0
	ldx #0 ; Fake 24 bit
	lda Screen_x
Screen_PrintString_rightvarInteger_var45 = $54
	sta Screen_PrintString_rightvarInteger_var45
	sty Screen_PrintString_rightvarInteger_var45+1
	; HandleVarBinopB16bit
	; RHS is pure, optimization
	; Load Integer array
	; CAST type INTEGER
	lda Screen_y
	asl
	tax
	lda Screen_tab40,x 
	ldy Screen_tab40+1,x 
	clc
	adc Screen_p2
	; Testing for byte:  Screen_p2+1
	; RHS is word, no optimization
	pha 
	tya 
	adc Screen_p2+1
	tay 
	pla 
	; Low bit binop:
	clc
	adc Screen_PrintString_rightvarInteger_var45
Screen_PrintString_wordAdd43
	sta Screen_PrintString_rightvarInteger_var45
	; High-bit binop
	tya
	adc Screen_PrintString_rightvarInteger_var45+1
	tay
	lda Screen_PrintString_rightvarInteger_var45
	sta Screen_sp
	sty Screen_sp+1
		ldy #0
printstring_loop1:
		lda (Screen_p1),y
		beq printstring_endd
		
		sta (Screen_sp),y
		iny
		jmp printstring_loop1
printstring_endd:
		
	
	
Screen_PrintString_while47
Screen_PrintString_loopstart51
	; Binary clause Simplified: NOTEQUALS
	clc
	; Load pointer array
	ldy #$0
	lda (Screen_p1),y
	; cmp #$00 ignored
	beq Screen_PrintString_edblock50
Screen_PrintString_ctb48: ;Main true block ;keep 
	; Load pointer array
	ldy #$0
	lda (Screen_p1),y
	; Calling storevariable on generic assign expression
	sta Screen_j
	; Binary clause Simplified: GREATEREQUAL
	; Compare with pure num / var optimization
	cmp #$41;keep
	bcc Screen_PrintString_edblock67
Screen_PrintString_localsuccess69: ;keep
	; ; logical AND, second requirement
	; Binary clause Simplified: LESS
	lda Screen_j
	; Compare with pure num / var optimization
	cmp #$60;keep
	bcs Screen_PrintString_edblock67
Screen_PrintString_ctb65: ;Main true block ;keep 
	; Optimizer: a = a +/- b
	; Load16bitvariable : Screen_j
	lda Screen_j
	sec
	sbc #$40
	sta Screen_j
Screen_PrintString_edblock67
	lda Screen_j
	; Calling storevariable on generic assign expression
	; Storing to a pointer
	ldy #$0
	sta (Screen_sp),y
	lda Screen_sp
	clc
	adc #$01
	sta Screen_sp+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Screen_PrintString_WordAdd71
	inc Screen_sp+1
Screen_PrintString_WordAdd71
	lda Screen_p1
	clc
	adc #$01
	sta Screen_p1+0
	; Optimization : A := A op 8 bit - var and bvar are the same - perform inc
	bcc Screen_PrintString_WordAdd72
	inc Screen_p1+1
Screen_PrintString_WordAdd72
	jmp Screen_PrintString_while47
Screen_PrintString_edblock50
Screen_PrintString_loopend52
	rts
end_procedure_Screen_PrintString
	;*
; //
; //	Clears 40*25 = 1000 bytes at position i2 with value i
; //
; 

	; NodeProcedureDecl -1
	; ***********  Defining procedure : Screen_PrintNumber
	;    Procedure type : User-defined procedure
Screen_PrintNumber_block73
Screen_PrintNumber
	ldy Screen_i1+1 ;keep
	lda Screen_i1
	; Calling storevariable on generic assign expression
	sta str_num
	sty str_num+1
	lda #<Screen_numStr
	ldx #>Screen_numStr
	sta str_p1
	stx str_p1+1
	lda Screen_c
	; Calling storevariable on generic assign expression
	sta str_b
	jsr str_itoa
	lda #<Screen_numStr
	ldx #>Screen_numStr
	sta Screen_p1
	stx Screen_p1+1
	lda Screen_cp
	ldx Screen_cp+1
	sta Screen_p2
	stx Screen_p2+1
	jsr Screen_PrintString
	rts
end_procedure_Screen_PrintNumber
	; NodeProcedureDecl -1
	; ***********  Defining procedure : Memory_Fill_override_2
	;    Procedure type : User-defined procedure
Memory_Fill_override_2_block74
Memory_Fill_override_2
	lda Memory_v
	ldy #0
memory_fill_loop:
	sta (Memory_p),y
	iny
	cpy Memory_v2
	bne memory_fill_loop
	
	rts
end_procedure_Memory_Fill_override_2
	
; //const useKernal : byte = 0;
; //polozenie kota
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitScreen
	;    Procedure type : User-defined procedure
InitScreen
	; Assigning memory location
	lda #$d
	; Calling storevariable on generic assign expression
	sta $d020
	; Assigning memory location
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d021
	; *** Inline procedure :Screen_Clear
	lda #$20
	ldx #$fa
generated_label_0
	dex
	sta $400 + $0000,x
	sta $400 + $00fa,x
	sta $400 + $01f4,x
	sta $400 + $02ee,x
	bne generated_label_0
	
	; *** End of inline procedure :Screen_Clear
	; Assigning a string : Screen_p1
	;has array index
	lda #<InitScreen_stringassignstr78
	ldy #>InitScreen_stringassignstr78
	sta Screen_p1
	sty Screen_p1+1
	lda #$1
	; Calling storevariable on generic assign expression
	sta Screen_x
	lda #$17
	; Calling storevariable on generic assign expression
	sta Screen_y
	lda #$00
	ldx #$04
	sta Screen_p2
	stx Screen_p2+1
	jsr Screen_PrintString
	rts
end_procedure_InitScreen
	
; // This method initializes the sprites
	; NodeProcedureDecl -1
	; ***********  Defining procedure : InitSprites
	;    Procedure type : User-defined procedure
InitSprites
	
; // Set all sprites to be multicolor
	; Assigning memory location
	lda #$ff
	; Calling storevariable on generic assign expression
	sta $d01c
	
; // Set common sprite multicolor #1 
	; Assigning memory location
	lda #$0
	; Calling storevariable on generic assign expression
	sta $d025
	
; // Set  common sprite multicolor #2 
	; Assigning memory location
	lda #$1
	; Calling storevariable on generic assign expression
	sta $d026
	
; // Set sprite "0" individual color value 
	lda #$c
	; Calling storevariable on generic assign expression
	sta $D027+$0
	
; // Turn on sprite 0 (or @useSprite)
	; Toggle bit with constant
	lda $d015
	ora #%1
	sta $d015
	ldx #$0 ; optimized, look out for bugs
	lda #1
InitSprites_shiftbit80
	cpx #0
	beq InitSprites_shiftbitdone81
	asl
	dex
	jmp InitSprites_shiftbit80
InitSprites_shiftbitdone81
InitSprites_bitmask_var82 = $54
	sta InitSprites_bitmask_var82
	lda $d015
	ora InitSprites_bitmask_var82
	sta $d015
	rts
end_procedure_InitSprites
	
; // This method is called one time per raster cycle
	; NodeProcedureDecl -1
	; ***********  Defining procedure : UpdateSprite
	;    Procedure type : User-defined procedure
UpdateSprite
	
; // Update sprite position based on joystick values
	; Generic 16 bit op
	ldy sprite_x+1 ;keep
	lda sprite_x
UpdateSprite_rightvarInteger_var86 = $54
	sta UpdateSprite_rightvarInteger_var86
	sty UpdateSprite_rightvarInteger_var86+1
	; HandleVarBinopB16bit
	ldy #0 ; ::HandleVarBinopB16bit 0
	; RHS is pure, optimization
	lda joystickright
	sec
	sbc joystickleft
	; Testing for byte:  #0
	; RHS is byte, optimization
	bcs UpdateSprite_skip88
	dey
UpdateSprite_skip88
	; Low bit binop:
	clc
	adc UpdateSprite_rightvarInteger_var86
UpdateSprite_wordAdd84
	sta UpdateSprite_rightvarInteger_var86
	; High-bit binop
	tya
	adc UpdateSprite_rightvarInteger_var86+1
	tay
	lda UpdateSprite_rightvarInteger_var86
	; Calling storevariable on generic assign expression
	sta sprite_x
	sty sprite_x+1
	; 8 bit binop
	; Add/sub where right value is constant number
	; 8 bit binop
	; Add/sub where right value is constant number
	lda joystickdown
	sec
	sbc joystickup
	 ; end add / sub var with constant
	clc
	adc sprite_y
	 ; end add / sub var with constant
	; Calling storevariable on generic assign expression
	sta sprite_y
	
; // Update the sprite position on screen for sprite number @useSprite	
	; Setting sprite position
	; isi-pisi: value is constant
	ldy sprite_x+1 ;keep
	lda sprite_x
	ldx #0
	sta $D000,x
	cpy #0
	beq UpdateSprite_spritepos89
	lda $D010
	ora #%1
	sta $D010
	jmp UpdateSprite_spriteposcontinue90
UpdateSprite_spritepos89
	lda $D010
	and #%11111110
	sta $D010
UpdateSprite_spriteposcontinue90
	inx
	txa
	tay
	lda sprite_y
	sta $D000,y
	; Binary clause Simplified: NOTEQUALS
	clc
	lda joystickright
	; cmp #$00 ignored
	beq UpdateSprite_edblock94
UpdateSprite_ctb92: ;Main true block ;keep 
	
; // Set left/right offset pointer for sprite
	lda #$1
	; Calling storevariable on generic assign expression
	sta joystick_dir
	; Test Inc dec D
	inc playerMovement
	; Load Byte array
	; CAST type NADA
	ldx playerMovement
	lda playerMoveSprite,x 
	; Calling storevariable on generic assign expression
	sta i
	
; //SetSpriteLoc(useSprite, 8192/64 + joystick_dir, useBank);
	; Set sprite location
	lda #$0
	sta $50
	; Generic 16 bit op
	ldy #0
	lda i
UpdateSprite_rightvarInteger_var108 = $54
	sta UpdateSprite_rightvarInteger_var108
	sty UpdateSprite_rightvarInteger_var108+1
	lda #128
	ldy #0
	; Low bit binop:
	clc
	adc UpdateSprite_rightvarInteger_var108
UpdateSprite_wordAdd106
	sta UpdateSprite_rightvarInteger_var108
	; High-bit binop
	tya
	adc UpdateSprite_rightvarInteger_var108+1
	tay
	lda UpdateSprite_rightvarInteger_var108
	ldx $50
	sta $07f8 + $0,x
	; Binary clause Simplified: GREATEREQUAL
	lda playerMovement
	; Compare with pure num / var optimization
	cmp #$9;keep
	bcc UpdateSprite_edblock112
UpdateSprite_ctb110: ;Main true block ;keep 
	
; //joystick_dir := 0;
	lda #$0
	; Calling storevariable on generic assign expression
	sta playerMovement
UpdateSprite_edblock112
UpdateSprite_edblock94
	; Binary clause Simplified: NOTEQUALS
	clc
	lda joystickleft
	; cmp #$00 ignored
	beq UpdateSprite_edblock118
UpdateSprite_ctb116: ;Main true block ;keep 
	lda #$0
	; Calling storevariable on generic assign expression
	sta joystick_dir
	; Test Inc dec D
	inc playerMovement
	; Load Byte array
	; CAST type NADA
	ldx playerMovement
	lda playerMoveSpriteLeft,x 
	; Calling storevariable on generic assign expression
	sta i
	
; //SetSpriteLoc(useSprite, 8192/64 + joystick_dir, useBank);
	; Set sprite location
	lda #$0
	sta $50
	; Generic 16 bit op
	ldy #0
	lda i
UpdateSprite_rightvarInteger_var132 = $54
	sta UpdateSprite_rightvarInteger_var132
	sty UpdateSprite_rightvarInteger_var132+1
	lda #128
	ldy #0
	; Low bit binop:
	clc
	adc UpdateSprite_rightvarInteger_var132
UpdateSprite_wordAdd130
	sta UpdateSprite_rightvarInteger_var132
	; High-bit binop
	tya
	adc UpdateSprite_rightvarInteger_var132+1
	tay
	lda UpdateSprite_rightvarInteger_var132
	ldx $50
	sta $07f8 + $0,x
	; Binary clause Simplified: GREATEREQUAL
	lda playerMovement
	; Compare with pure num / var optimization
	cmp #$9;keep
	bcc UpdateSprite_edblock136
UpdateSprite_ctb134: ;Main true block ;keep 
	
; //joystick_dir := 0;
	lda #$0
	; Calling storevariable on generic assign expression
	sta playerMovement
UpdateSprite_edblock136
UpdateSprite_edblock118
	
; //if (joystick_dir<>0 or (joystickup=1 or joystickdown=1)) then
; //begin
; //	if (playerMovementCounter=0) then
; //	begin
; //		playerMovementCounter := playerMovementCounterMax;
; //		playerMovement := mod(playerMovement+1,8);
; //	end			
; //	else
; //		dec(playerMovementCounter);
; //end;
	ldy #0 ; Fake 16 bit
	lda playerMovement
	; Calling storevariable on generic assign expression
	; Casting from byte to integer
	sta Screen_i1
	sty Screen_i1+1
	lda #$1
	; Calling storevariable on generic assign expression
	sta Screen_x
	lda #$16
	; Calling storevariable on generic assign expression
	sta Screen_y
	lda #$14
	; Calling storevariable on generic assign expression
	sta Screen_c
	lda #$00
	ldx #$04
	sta Screen_cp
	stx Screen_cp+1
	jsr Screen_PrintNumber
	
; // Set the sprite pointer to point to the sprite data + direction offset
; //i:=playerMoveSprite[playerMovement];
; //SetSpriteLoc(useSprite, 8192/64 + joystick_dir, useBank);
	; Set sprite location
	lda #$0
	sta $50
	; Generic 16 bit op
	ldy #0
	lda i
UpdateSprite_rightvarInteger_var141 = $54
	sta UpdateSprite_rightvarInteger_var141
	sty UpdateSprite_rightvarInteger_var141+1
	lda #128
	ldy #0
	; Low bit binop:
	clc
	adc UpdateSprite_rightvarInteger_var141
UpdateSprite_wordAdd139
	sta UpdateSprite_rightvarInteger_var141
	; High-bit binop
	tya
	adc UpdateSprite_rightvarInteger_var141+1
	tay
	lda UpdateSprite_rightvarInteger_var141
	ldx $50
	sta $07f8 + $0,x
	rts
end_procedure_UpdateSprite
	
; //joystick_dir := 0;
; //if (playerMovement > 8) then playerMovement:=0; 
; // This interrupt is triggered one time pr raster cycle
	; NodeProcedureDecl -1
	; ***********  Defining procedure : MainRaster
	;    Procedure type : User-defined procedure
MainRaster
	; StartIRQ
	asl $d019
	
; // Update joystick here
	lda #<joystickup
	ldx #>joystickup
	sta Memory_p
	stx Memory_p+1
	lda #$0
	; Calling storevariable on generic assign expression
	sta Memory_v
	lda #$5
	; Calling storevariable on generic assign expression
	sta Memory_v2
	jsr Memory_Fill_override_2
	lda #%11111111  ; CIA#1 port A = outputs 
	sta $dc03             
	lda #%00000000  ; CIA#1 port B = inputs
	sta $dc02             
	lda $dc00
	sta $50
	jsr callJoystick
	
; // Update sprites
	jsr UpdateSprite
	; CloseIRQ
	pla
	tay
	pla
	tax
	pla
	rti
end_procedure_MainRaster
block1
main_block_begin_
	
; // Main program
	jsr InitScreen
	jsr InitSprites
	sei
	; Disable interrupts
	ldy #$7f    ; $7f = %01111111
	sty $dc0d   ; Turn off CIAs Timer interrupts
	sty $dd0d   ; Turn off CIAs Timer interrupts
	; RasterIRQ : Hook a procedure
	lda #$0
	sta $d012
	lda #<MainRaster
	sta $0314
	lda #>MainRaster
	sta $0315
	; Enable raster IRQ
	lda $d01a
	ora #$01
	sta $d01a
	lda #$1B
	sta $d011
	asl $d019
	cli
	jmp * ; loop like (�/%
main_block_end_
	; End of program
	; Ending memory block at $810
InitScreen_stringassignstr78		dc.b	"JOYSTI8CK W 2 PORCIE"
	dc.b	0
EndBlock810:
	org $2000
StartBlock2000:
	org $2000
kotek:
	incbin	 "E:/trse_projekty/kotek///sprites/kotek.bin"
end_incbin_kotek:
EndBlock2000:

