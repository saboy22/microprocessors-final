
;**********************************************************************
;   This file is a basic code template for object module code         *
;   generation on the PIC16F877A. This file contains the              *
;   basic code building blocks to build upon.                         *
;                                                                     *
;   Refer to the MPASM User's Guide for additional information on     *
;   features of the assembler and linker (Document DS33014).          *
;                                                                     *
;   Refer to the respective PIC data sheet for additional             *
;   information on the instruction set.                               *
;                                                                     *
;**********************************************************************
;                                                                     *
;    Filename: final.asm					      *
;    Date: 12/06/2019                                                 *
;    File Version: finished version 1.0                               *                   
;                                                                     *
;    Author: Samuel Boyle                                             *
;    Company: CPE 303                                                 *
;                                                                     * 
;                                                                     *
;**********************************************************************
;                                                                     *
;    Files required: P16F877A.INC                                     *
;                                                                     *
;                                                                     *
;                                                                     *
;**********************************************************************
;                                                                     *
;    Notes:   Run a motor with a keypad and it will display the       *                          
;      rpms and it will change rpm based upon which value you enter   *                                                            
;                                                                     *
;                                                                     *
;                                                                     *
;**********************************************************************


    list        p=16f877a   ; list directive to define processor
    #include    <p16f877a.inc>  ; processor specific variable definitions
    
  __CONFIG _CP_OFF & _WDT_OFF & _BODEN_OFF & _PWRTE_ON & _HS_OSC & _WRT_OFF & _LVP_OFF & _CPD_OFF & _DEBUG_ON

; '__CONFIG' directive is used to embed configuration data within .asm file.
; The labels following the directive are located in the respective .inc file.
; See respective data sheet for additional information on configuration word.

;***** VARIABLE DEFINITIONS 
  
;###################EXTERN######################
 extern LCDInit
 extern temp_wr
 extern iwrite
 extern dwrite
 extern LCDLine_1
 extern LCDLine_2
 extern run_motor

; Shared Uninitialized Data Section
INT_VAR UDATA_SHR      
w_temp RES 1       ; variable used for context saving 
status_temp RES 1       ; variable used for context saving
pclath_temp RES 1       ; variable used for context saving
total_value RES 1 ;the total value used to change motor speed
decimal	RES 1 ;used
hundreds_place RES 1
tens_place RES 1
ones_place RES 1
 
 GLOBAL total_value

;Uninitialized Data Section
TEMP_VAR UDATA           ; explicit address specified is not required
val_temp RES 1       ; table counter for keypad
col_flag RES 1	;tells when column is pushed
row_flag RES 1 ;tells when row is pushed
special_flag RES 1 ;tells when special character is pushed
special_press RES 1 ;used to tell when special character is pushed
temp_count RES 1 ; for start up letters
number_hold RES 1 ;used to temporarily hold a value 
pos_count RES 1 ;keeps track position when writing % drive
right_value RES 1 ;stores right value of % drive
past_right_value RES 1 ;stores the old right value
left_value RES 1 ;stores left value of % drive
past_left_value RES 1 ;stores the old left value
multiply_counter RES 1 ;used to mulitply the left value by 10
left_value_multiply RES 1 ;multiplied left value
add_value RES 1 ;used to add the left and right value
delcntr1 RES 1 ;counter for smaller delay
delcntr2 RES 1 ;counter for larger delay
h_number RES 1 ;used to store the upper 8 bits of tmr1
number RES 1 ;used to store the lower 8 bits of tmr1
t_counter RES 1 ; counter for the delay
d_flag RES 1 ;flag to test if interrupt happened


;Overlayed Uninitialized Data Section
; in this example both variables are assigned the same GPR location by linker
G_DATA UDATA_OVR       ; explicit address can be specified
flag RES 2           ; temporary variable (shared locations - G_DATA)

G_DATA UDATA_OVR   
count RES 2           ; temporary variable (shared locations - G_DATA)

;**********************************************************************
RESET_VECTOR    CODE    0x0000 ; processor reset vector
    nop                        ; nop for icd
    pagesel start
    goto    start              ; go to beginning of program


INT_VECTOR      CODE    0x0004 ; interrupt vector location

INTERRUPT

    movwf   w_temp          ; save off current W register contents
    movf    STATUS,w        ; move status register into W register
    movwf   status_temp     ; save off contents of STATUS register
    movf    PCLATH,w        ; move pclath register into w register
    movwf   pclath_temp     ; save off contents of PCLATH register

    movlw 0x01
    banksel d_flag
    movwf d_flag ;move 1 to flag to signal interrupt occured
    banksel INTCON
    bcf INTCON,2 ;clear TMR0 flag

    movf    pclath_temp,w   ; retrieve copy of PCLATH register
    movwf   PCLATH          ; restore pre-isr PCLATH register contents
    movf    status_temp,w   ; retrieve copy of STATUS register
    movwf   STATUS          ; restore pre-isr STATUS register contents
    swapf   w_temp,f
    swapf   w_temp,w        ; restore pre-isr W register contents
    retfie                  ; return from interrupt

MAIN_PROG       CODE

start

    nop
    pagesel LCDInit	;set up PCLATH for sub call
    call LCDInit ; setup the LCD
	;now clear the sceen and set DDRAM to address 0 
	;by executing clear display instruction = 0x01
    banksel temp_wr	;set up status register for variable write
    movlw 0x01	;This value can be confirmed in Hitatchi 44780 data sheet
					
    movwf temp_wr	;This is a global variable that passes the instruction value
					;to i_write
    pagesel iwrite	;setup PCLATH correctly
    call iwrite

    movlw 0x0F		;Display on, cursor on, blink on
    movwf temp_wr
    pagesel iwrite	;setup PCLATH correctly
    call iwrite
      
    banksel temp_count	;set status register
    clrf temp_count	;initialize variable
loop	
	movf	temp_count,W	;load working register with table index
	pagesel	 Msg_Lookup	;this asembler directive only uses bsf and bsc so will not destroy Wreg
	call	Msg_Lookup	;returns character in Wreg
	banksel 	temp_wr
	movwf	temp_wr
	movf	temp_wr,F	;see if the next character to write is the null character
	pagesel	ExitLoop	;pagesel uses bcf and bsf on plcath that does not change state of Z bit
	btfsc	STATUS,Z	;test to see if null character is returned
	goto	ExitLoop	;if it is null then exit the loop
	pagesel	 dwrite
	call	dwrite		;write the value in temp_wr to DDRAM in the display 
	banksel	 temp_count
	movlw	d'15'
	subwf	temp_count,W ; if the 16 character then skp to the second line
	pagesel 	LCDLine_2
	btfsc	STATUS,Z	;if zero then set cursor to the seond line
	call	LCDLine_2
	incf	temp_count	;keeps track of the position that is being written.
	pagesel	loop	
	goto	loop
ExitLoop
	
	;moves the cursor to the starting spot on the lcd
	banksel temp_wr
	movlw	0x8D
	movwf	temp_wr
	pagesel	iwrite
	call	iwrite
	
	; go through and clear out variables before using them
	banksel total_value
	clrf total_value
	pagesel run_motor
	call run_motor
	banksel number_hold
	clrf number_hold
	banksel pos_count
	clrf pos_count
	banksel right_value
	clrf right_value
	banksel left_value
	clrf left_value
	banksel past_right_value
	clrf past_right_value
	banksel past_left_value
	clrf past_left_value
	
	;set portc bit 0 as an input so we can count using tmr1
	banksel TRISC
       bsf TRISC,0
       
       ;intialize tmr0
       banksel OPTION_REG
        movlw B'10000111'
     movwf OPTION_REG ;set the prescalar to 256
   ;turn on timer1, set up rc0 to count in tmr1, don't 
     banksel T1CON
     bsf T1CON,0
     bsf T1CON,1
     bsf T1CON,2
	
 ;start by checking the input of the columns on the keypad      
column
    banksel TRISD
    movlw B'00000111' ;set column 1,2,3 as inputs
    movwf TRISD
    banksel PORTD 
    pagesel wait5 ;make sure no buttons are held 
    call wait5
    pagesel wait6
    call wait6
    pagesel wait7
    call wait7
    banksel col_flag
    clrf col_flag ;clear flag that tests if column button is pressed
delay
    movlw d'40'; load 40 into the delay counter 5e-7s * 256 * 256 * 40 = 1.3 seconds
    banksel t_counter
    movwf t_counter
    banksel TMR0
    clrf TMR0 ;clear tmr0 because we want all 256 loops
    banksel TMR1L
    clrf TMR1L ;reset tmr1 value
    clrf TMR1H
del_reset
    banksel d_flag
    clrf d_flag ;clear the flag that tests if interrupt occured
    banksel INTCON ;set up interrupts
    bsf INTCON,7 ;set GIE
    bsf INTCON,5 ;set tmr0 interrupt
del
    banksel PORTD 
    clrf PORTD ;make sure that no previous values are stored in portd b/c it will mess up the keypad
    pagesel col3
    btfss PORTD,0 ;is column 3 pressed
    call col3
    pagesel col2
    btfss PORTD,1 ;is column 2 pressed
    call col2
    pagesel col1
    btfss PORTD,2 ;is colum 1 pressed
    call col1
    banksel col_flag
    pagesel row
    btfsc col_flag,0 ;test if one of the buttons was pressed if yes goto row
    goto row
    banksel d_flag
    pagesel del
    btfss d_flag,0 ;did the interrupt occur is the flag 1
    goto del ;if not sit here and wait
    banksel t_counter
    pagesel del_reset
    decfsz t_counter ;interrupt occured decrement the delay counter and when it hits 0 return
    goto del_reset
    pagesel rpm
    goto rpm; when 1.3 seconds occurs then update the number of rpms
	
       ;load a value of 0 into table counter
col1
    banksel val_temp
    clrf val_temp
    banksel col_flag
    movlw 0x01
    movwf col_flag ;indicate when the button is pushed
    return
       
       ;load a value of 1 into table counter
col2
    movlw d'1'
    banksel val_temp
    movwf val_temp
    banksel col_flag
    movlw 0x01
    movwf col_flag ;indicate when the button is pushed
    return
       
       ;load a value of 2 into table counter
col3
    movlw d'2'
    banksel val_temp
    movwf val_temp
    banksel col_flag
    movlw 0x01
    movwf col_flag ;indicate when the button is pushed
    return
    
       ;after checking the columns we will now check the rows for inputs
row
    banksel TRISD
    movlw B'01111000';set up the rows as inputs
    movwf TRISD
    banksel row_flag
    clrf row_flag
r_loop
    banksel PORTD
    pagesel row1
    btfss PORTD,6 ;is row one pressed
    call row1
    pagesel row2
    btfss PORTD,5 ;is row two pressed
    call row2
    pagesel row3
    btfss PORTD,4 ;is row three pressed
    call row3
    pagesel row4
    btfss PORTD,3 ;is row four pressed
    call row4
    banksel row_flag
    pagesel column
    btfsc row_flag,0
    goto column
    pagesel r_loop
    goto r_loop
      
      
       
       ;move table counter value to the working register
       ;write to the board
       ;wait for the button to be released go back to the start
row1
    pagesel large_delay
    call large_delay
    banksel val_temp
    movf val_temp,0
    pagesel keypad_table
    call keypad_table
    banksel temp_wr
    movwf temp_wr
    banksel number_hold
    movwf number_hold
    pagesel lcd_write
    call lcd_write
    pagesel wait1
    call wait1
    banksel row_flag
    movlw 0x01
    movwf row_flag
    return
	
	;add the value of 3 to the table counter to give all numbers in row 2
	;move table counter value to the working register
       ;write to the board
       ;wait for the button to be released go back to the start
row2
    pagesel large_delay
    call large_delay
    banksel val_temp
    movlw d'3'
    addwf val_temp,0
    pagesel keypad_table
    call keypad_table
    banksel temp_wr
    movwf temp_wr
    banksel number_hold
    movwf number_hold
    pagesel lcd_write
    call lcd_write
    pagesel wait2
    call wait2
    banksel row_flag
    movlw 0x01
    movwf row_flag
    return
       
	;add the value of 6 to the table counter to give values in the third row
	;move table counter value to the working register
       ;write to the board
       ;wait for the button to be released go back to the start
row3
    pagesel large_delay
    call large_delay
    banksel val_temp
    movlw d'6'
    addwf val_temp,0
    pagesel keypad_table
    call keypad_table
    banksel temp_wr
    movwf temp_wr
    banksel number_hold
    movwf number_hold
    pagesel lcd_write
    call lcd_write
    pagesel wait3
    call wait3
    banksel row_flag
    movlw 0x01
    movwf row_flag
    return
	
	;add the value of 9 to the table counter to give values in the fourth row
	;move table counter value to the working register
       ;write to the board if 0 then either load past value or change motor speed
       ;wait for the button to be released go back to the start
row4
    pagesel large_delay
    call large_delay
    banksel special_flag
    clrf special_flag ;reset the flag that checks the if a special character is pressed
    banksel special_press
    clrf special_press ;reset the value that checks either star or pound is pressed
    banksel val_temp
    movlw d'9'
    addwf val_temp,0
    pagesel keypad_table
    call keypad_table
    banksel special_press
    movwf special_press
    pagesel enter
    btfsc special_press,1 ;if the value 2 is called back then # was pressed
    call enter
    pagesel star
    btfsc special_press,0 ;if the value of 1 is called back then * was pressed
    call star
    banksel special_flag
    pagesel zero_write
    btfss special_flag,0 ;if neither button was pressed then 0 was pressed
    call zero_write
    pagesel wait4
    call wait4
    banksel row_flag
    movlw 0x01
    movwf row_flag
    return
     
;take the previous value that was entered to change the motor speed
;and load it back to the display
star
    banksel temp_wr
    movlw 0x8D
    movwf temp_wr
    pagesel iwrite
    call iwrite
    banksel past_left_value
    movf past_left_value,W
    movwf past_left_value
    banksel left_value
    movwf left_value ;load past value entered into left value
    banksel temp_wr
    movwf temp_wr
    bsf temp_wr,4 ;set the upper 4 bits to 0x30 to make it ascii
    bsf temp_wr,5
    pagesel dwrite
    call dwrite
    banksel past_right_value
    movf past_right_value,W
    movwf past_right_value
    banksel right_value
    movwf right_value ;load past value entered into right value
    banksel temp_wr
    movwf temp_wr
    bsf temp_wr,4 ;set the upper 4 bits to 0x30 to make it ascii
    bsf temp_wr,5
    pagesel dwrite
    call dwrite
    movlw 0x8D
    movwf temp_wr
    pagesel iwrite
    call iwrite
    movlw 0x01
    movwf special_flag
    return

;take the current value on the lcd and based on that number cause the motor to 
;spin at that speed
enter
    movlw 0x8D
    movwf temp_wr
    pagesel iwrite
    call iwrite
    pagesel total_up ;call a subroutine that will total up the actual value of the 
    call total_up ;left and right value into decimal
    pagesel run_motor
    call run_motor ;will go and set up the tmr2 and pwm so that the motor can spin
    banksel right_value
    movf right_value,0
    banksel past_right_value
    movwf past_right_value ;loads the right value into past right value
    banksel left_value
    movf left_value,0
    banksel past_left_value
    movwf past_left_value ;loads the left value into past left value
    movlw 0x01
    movwf special_flag
    return

;used to write the value 0 to the lcd
zero_write
    movlw '0'
    banksel number_hold
    clrf number_hold
    banksel temp_wr
    movwf temp_wr
    pagesel lcd_write
    call lcd_write
    return
      
	;wait for button to be released
wait1
    banksel PORTD
    btfss PORTD,6
    goto wait1
    return
	
	;wait for button to be released
wait2
    banksel PORTD
    btfss PORTD,5
    goto wait2
    return
	
	;wait for button to be released
wait3
    banksel PORTD
    btfss PORTD,4
    goto wait3
    return
	
	;wait for button to be released
wait4
    banksel PORTD
    btfss PORTD,3
    goto wait4
    return
	
    ;wait for button to be released
wait5
    banksel PORTD
    btfss PORTD,0
    goto wait5
    return
	
	;wait for button to be released
wait6
    banksel PORTD
    btfss PORTD,1
    goto wait6
    return
	
    ;wait for button to be released
wait7
    banksel PORTD
    btfss PORTD,2
    goto wait7
    return
	
    ;used to write to the lcd
lcd_write
    banksel pos_count
    pagesel lcd_write_left
    btfss pos_count,0 ;test to see which position the cursor is in
    call lcd_write_left ; if pos_count is clear then in left spot
    pagesel lcd_write_right
    btfsc pos_count,0 ;if pos_count is set to one then write in right spot
    goto lcd_write_right
    movlw 0x01
    movwf pos_count
lcd_write0
    return
	
    ;write a value in the left spot
lcd_write_left
    pagesel dwrite
    call dwrite ;write to lcd
    banksel number_hold
    bcf number_hold,4 ; convert from ascii to decimal number
    bcf number_hold,5
    movf number_hold,0
    banksel left_value
    movwf left_value ;load decimal value into left_value
    return

    ;write a value in right spot
lcd_write_right
    pagesel dwrite
    call dwrite ;write to lcd
    banksel temp_wr
    movlw 0x8D
    movwf temp_wr
    pagesel iwrite
    call iwrite ; move cursor back to left cursor spot
    banksel number_hold
    bcf number_hold,4 ;convert ascii to decimal
    bcf number_hold,5
    movf number_hold,0
    banksel right_value
    movwf right_value ;load decimal value into right value
    banksel pos_count
    clrf pos_count ;clear pos_count to indicate we are in the left spot
    pagesel lcd_write0
    goto lcd_write0

;used to total up the true decimal value 
;that was displayed on the lcd so it could be loaded correctly to spin the motor
; subroutine is only called when enter or # is pressed
total_up
    pagesel multiply
    call multiply
    banksel left_value_multiply
    movf left_value_multiply,0
    banksel add_value
    movwf add_value
    banksel right_value
    movf right_value,0
    movwf right_value
    banksel add_value
    addwf add_value,0
    banksel total_value
    movwf total_value
    return
	
;this subroutine is used to multiply the left value on the lcd by 10
; so the correct value can be loaded to spin the motor at the right speed
multiply
    banksel left_value_multiply
    clrf left_value_multiply
    movlw 0x0A ;move decimal 10 into the counter
    banksel multiply_counter
    movwf multiply_counter
    banksel left_value
    movf left_value,0
    movwf left_value
multiply_loop ;loop through ten times adding to itself 10 times
    banksel left_value_multiply
    addwf left_value_multiply,1
    banksel multiply_counter
    pagesel multiply_loop
    decfsz multiply_counter
    goto multiply_loop
    return

; takes the value from tmr1 and divides it by 2
; then seperates each individual value so that it can 
; be written to the lcd
rpm
    banksel TMR1L
    movf TMR1L,0
    banksel number
    movwf number ;move the value from the lower 8bits of tmr1 to number
    banksel TMR1H
    movf TMR1H,0 ;move the value from the upper 8bits of tmr1 to h_number
    banksel h_number
    movwf h_number
    clrf hundreds_place ;clear each place so we can load new values into them 
    clrf tens_place
    clrf ones_place
    clrf decimal
    banksel number
    movlw d'5'
    btfsc number,0 ;test to see if the number is even or odd if its odd 
    movwf decimal ;then load 5 to decimal so we can have .5
    rrf number ;this divides the number from tmr1 by 2
    banksel h_number
    pagesel number_high
    btfsc h_number,0 ;test if tmr1 counted higher than 255
    call number_high ;if yes call subroutine that sets the far left bit to 1
    pagesel hundreds
    call hundreds ;seperate the hundereds place
    pagesel tens
    call tens ;seperate the tens place
    pagesel ones
    call ones ;seperate the ones place
    pagesel rpm_write
    call rpm_write ;write the rpm to the board
    pagesel column
    goto column ;return to column to wait for 1.3 seconds or wait for button push
    
number_high
    banksel number
    bsf number,7 ;set the far left bit to 1
    return
    
;used to sepearate the hundreds place
;set the carry bit in status to 1 and subtract 100
;for every time you can subtract 100 incrment hundreds place
;once carry bit is set back to zero "negative" number results
;add one hundred back and return
hundreds
    movlw d'100'
    banksel STATUS
    bsf STATUS,0
    banksel number
    subwf number,1
    banksel STATUS
    btfsc STATUS,0
    incf hundreds_place
    pagesel hundreds
    btfsc STATUS,0
    goto hundreds
    btfsc STATUS,2
    incf hundreds_place
    pagesel add_back
    btfss STATUS,2
    call add_back
    return

;used to sepearate the tens place
;set the carry bit in status to 1 and subtract 10
;for every time you can subtract 10 incrment tens place
;once carry bit is set back to zero "negative" number results
;add ten and return
tens
    movlw d'10'
    banksel STATUS
    bsf STATUS,0
    banksel number
    subwf number,1
    banksel STATUS
    btfsc STATUS,0
    incf tens_place
    pagesel tens
    btfsc STATUS,0
    goto tens
    btfsc STATUS,2
    incf tens_place
    pagesel add_back
    btfss STATUS,2
    call add_back
    return
    
;used to sepearate the ones place
;set the carry bit in status to 1 and subtract 1
;for every time you can subtract 1 incrment ones place
;once carry bit is set back to zero "negative" number results
;add one and return    
ones
    movlw d'1'
    banksel STATUS
    bsf STATUS,0
    banksel number
    subwf number,1
    banksel STATUS
    btfsc STATUS,0
    incf ones_place
    pagesel ones
    btfsc STATUS,0
    goto ones
    btfsc STATUS,2
    incf ones_place
    pagesel add_back
    btfss STATUS,2
    call add_back
    return

;used to add back to number
add_back
    banksel number
    addwf number
    return
    
;used to write the rpm to the board     
rpm_write  
    movlw 0xC4
    banksel temp_wr
    movwf temp_wr
    pagesel iwrite
    call iwrite ;set cursor to the right spot
    movf hundreds_place,0
    movwf temp_wr
    movlw 0x30
    addwf temp_wr,1 ;add 30 to value to make it ascii character
    pagesel dwrite
    call dwrite
    movf tens_place,0
    movwf temp_wr
    movlw 0x30
    addwf temp_wr,1 ;add 30 to value to make it ascii character
    pagesel dwrite
    call dwrite
    movf ones_place,0
    movwf temp_wr
    movlw 0x30
    addwf temp_wr,1 ;add 30 to value to make it ascii character
    pagesel dwrite
    call dwrite
    movlw '.'
    movwf temp_wr ;write a . to the board to show decimal
    pagesel dwrite
    call dwrite
    movf decimal,0
    movwf temp_wr
    movlw 0x30
    addwf temp_wr,1 ;add 30 to value to make it ascii character
    pagesel dwrite
    call dwrite
    pagesel move_cursor
    call move_cursor ;move the cursor to the spot it was in
    return

;tests pos_count and depending on its value it will move the cursor
; to that specific spot
move_cursor
    movlw 0x8E
    btfss pos_count,0
    movlw 0x8D
    banksel temp_wr
    movwf temp_wr
    pagesel iwrite
    call iwrite
    return
 
;used to determine what value to call for the keypad
    
keypad_table		;subroutine is entered with index value in Wreg
    banksel 	count
    movwf	count
    pageselw Key_Table ;Special assembly directive used 'pageselw' so that 
						;all 5 upper bits are written. 
						;Get the byte read and use it to
    movlw Key_Table    ;index into our jump table. If
    addwf count,w      ;we crossed a 256-byte boundary,
    btfsc STATUS,C                ;then increment PCLATH. Then load the
    incf PCLATH,f                 ;program counter with computed goto.
    movwf PCL
Key_Table
	retlw	'1'	;0
	retlw	'2'
	retlw	'3'
	retlw	'4'
	retlw	'5'
	retlw	'6'
	retlw	'7'
	retlw	'8'
	retlw	'9'
	retlw	'1' ;*
	retlw	'0'
	retlw	'2' ;#
	
;used to write the startup writing to the board
Msg_Lookup		;subroutine is entered with index value in Wreg
    banksel 	count
    movwf	count
    pageselw Msg_Table ;Special assembly directive used 'pageselw' so that 
						;all 5 upper bits are written. 
						;Get the byte read and use it to
    movlw Msg_Table    ;index into our jump table. If
    addwf count,w      ;we crossed a 256-byte boundary,
    btfsc STATUS,C                ;then increment PCLATH. Then load the
    incf PCLATH,f                 ;program counter with computed goto.
    movwf PCL
Msg_Table
	retlw	'P'	;0
	retlw	'e'	;1
	retlw	'r' 	;2
	retlw	'c'	;3
	retlw	'e'	;4
	retlw	'n'	;5
	retlw	't'	;6
	retlw	' ' 	;7
	retlw	'd'	;8
	retlw	'r'	;9
	retlw	'i'	;10
	retlw	'v'	;11
	retlw	'e'	;12
	retlw	'0'	;13
	retlw	'0'	;14
	retlw	'%'	;15
	retlw	'R'
	retlw	'P'
	retlw	'M'
	retlw	'='
	retlw	'0'
	retlw	'0'
	retlw	'0'
	retlw	'.'
	retlw	'0'
	retlw	0	;null character is a flag that the string is done

; delays used to prevent the keypad from bouncing
delay_small		
    movlw D'255'
    banksel delcntr1
    movwf delcntr1
dell		
    nop
    nop
    decfsz delcntr1,1
    goto dell
    return
    
large_delay	
    movlw D'255'
    banksel delcntr2
    movwf delcntr2
dell2		
    pagesel delay_small
    call delay_small
    banksel delcntr2
    pagesel dell2
    decfsz delcntr2,1
    goto dell2
    return

    goto $
    END                       ; directive 'end of program'

