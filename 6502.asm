; TODO next
; 1. add SYS command to jump to a memory location in hex
; 1. add SD card to breadboard
; 1. add LOAD command to load from an SD card
; 1. figure out if there's a way to shift display to absolute position

; MEMORY MAP
; ram               $0000-$3fff
;   stack           $0100-$01ff
;   keyboard buffer $0200-$02ff
;   rs232 tx buffer $0300-$0379
;   rs232 rx buffer $0380-$03ff
;   screen buffer   $0400-$06cf
; UART/ACIA         $4800-$4803
; VIA2 (6522)       $5000-$500f
; VIA1 (6522)       $6000-$600f
;
; OUTPUT
; There are two VIA chips (VIA1, VIA2 above).
; There are also two shift registers that are wired
; up to via1_portb.
;  sr1 - used to send instructions to the lcd
;  sr2 - unused
;
; KEYBOARD BUFFER
;
; The keyboard buffer is used to buffer keys that were typed in from
; the keyboard. It consists of a set of alternating bytes. The first
; byte is the actual key that was pressed. The second byte contains
; the control flags (shift, ctrl, c=, etc) with an 1 bit meaning
; that ctrl flag is currently set. E.g. if left shift is pressed down,
; the left shift bit will be set to 1.
;
; SCREEN BUFFER
; Each time a new row is added to the buffer, the head of the buffer
; moves down by 40 characters in memory. When it gets to zero, it wraps around.

  ORG $8000

; VIA memory locations
via1_portb = $6000 ; portb on via1 chip
via1_porta = $6001 ; porta on via1 chip
via1_ddrb  = $6002 ; data direction register via1_portb
via1_ddra  = $6003 ; data direction register via1_porta
via1_t1cl  = $6004 ; timer 1 counter lo
via1_t1ch  = $6005 ; timer 1 counter hi
via1_t2cl  = $6008 ; timer 2 counter lo
via1_t2ch  = $6009 ; timer 2 counter hi
via1_acr   = $600b ; auxiliary control  register
via1_pcr   = $600c ; peripheral control register
via1_ifr   = $600d ; interrupt flag register
via1_ier   = $600e ; interrupt enable register

via2_portb = $5000 ; portb on via2 chip
via2_porta = $5001 ; porta on via2 chip
via2_ddrb  = $5002 ; data direction register via2_portb
via2_ddra  = $5003 ; data direction register via2_porta
via2_t1cl  = $5004 ; timer 1 counter lo
via2_t1ch  = $5005 ; timer 1 counter hi
via2_t2cl  = $5008 ; timer 2 counter lo
via2_t2ch  = $5009 ; timer 2 counter hi
via2_acr   = $500b ; auxiliary control  register
via2_pcr   = $500c ; peripheral control register
via2_ifr   = $500d ; interrupt flag register
via2_ier   = $500e ; interrupt enable register

acia_data   = $4800
acia_status = $4801
acia_cmd    = $4802
acia_ctrl   = $4803

; the two bits being set in each of these is on purpose. they get and'd
; with the above flags depending on which outputs are enabled.
LCD_OUTPUT_E  = %00001001 ; enable, starts data read or write
LCD_OUTPUT_RW = %00010010 ; selects read/write, 0=write, 1=read
LCD_OUTPUT_RS = %00100100 ; register select, 0=instr (for write or read busy flag), 1=data

ROW_LENGTH      = 40 ; must be a factor of 2
LCD_SCREEN_WIDTH = 16 ; number of chars on the lcd screen

; keyboard flags
KBF_RELEASING     = %00000001 ; flag to ignore next scancode
KBF_LSHIFT        = %00000010 ; left shift key pressed
KBF_RSHIFT        = %00000100 ; right shift key pressed
KBF_RELEASING_INV = %11111110 ; flag to ignore next scancode, inverse
KBF_LSHIFT_INV    = %11111101 ; left shift key pressed, inverse
KBF_RSHIFT_INV    = %11111011 ; right shift key pressed, inverse

; keyboard scancodes
KBSC_RELEASE    = $f0 ; keyboard scancode - release
KBSC_RETURN     = $5a ; keyboard scancode - return
KBSC_LR_CURSORS = $0c ; keyboard scancode - left/right cursor
KBSC_LSHIFT     = $12 ; keyboard scancode - left shift
KBSC_RSHIFT     = $59 ; keyboard scancode - right shift

; variables stored at specific memory locations
ticks                     = $00 ;4 bytes, number of interrupts, each tick is 10ms
screen_buf_ptr_lo         = $04 ;1 byte, ptr to current start of screen buffer, lo byte
screen_buf_ptr_hi         = $05 ;1 byte, ptr to current start of screen buffer, hi byte
screen_buf_ptr_tmp_lo     = $06 ;1 byte, tmp ptr used when finding screen buf locations
screen_buf_ptr_tmp_hi     = $07 ;1 byte, tmp ptr used when finding screen buf locations
screen_buf_cursor_row_lo  = $08 ;1 byte, mem location of the row that the cursor is on in screen buffer
screen_buf_cursor_row_hi  = $09 ;1 byte, mem location of the row that the cursor is on in screen buffer
screen_buf_bottom_row_lo  = $0a ;1 byte, mem location of the bottom row in screen buffer
screen_buf_bottom_row_hi  = $0b ;1 byte, mem location of the bottom row in screen buffer
screen_buf_top_row_lo     = $0c ;1 byte, mem location of the top row in screen buffer
screen_buf_top_row_hi     = $0d ;1 byte, mem location of the top row in screen buffer
input_buf_lo              = $0e ;1 byte, mem location of the row that contains the current user input
input_buf_hi              = $0f ;1 byte, mem location of the row that contains the current user input
display_offset            = $10 ;1 byte, offset of far left character on screen
cursor_col_offset         = $11 ;1 byte, offset of lcd cursor from left side of screen
cursor_tmp                = $12 ;1 byte, tmp var used by some cursor subroutines
rows_scrolled_up          = $13 ;1 byte, how far up we've scrolled
cursor_current_lcd_row    = $14 ;1 byte, the row on the lcd that the cursor is currently on
cursor_row_start_addr     = $15 ;1 byte, lcd address of first character in current row
zbb1                      = $16 ;1 byte, zero byte buffer var
zbb2                      = $17 ;1 byte, zero byte buffer var
zbb3                      = $18 ;1 byte, zero byte buffer var
zbb4                      = $19 ;1 byte, zero byte buffer var
kb_wptr                   = $1a ;1 byte, keyboard write pointer
kb_rptr                   = $1b ;1 byte, keyboard read pointer
kb_flags_i                = $1c ;1 byte, keyboard flags used by interrupt handler
kb_flags_k                = $1d ;1 byte, keyboard flags used by keyboard processor
ascii_char_typed          = $1e ;1 byte, ascii code of a character typed by a user
output_data_sr1           = $1f ;1 byte, data to be sent to shift register 1
output_data_sr2           = $20 ;1 byte, data to be sent to shift register 2
shift_data_tmp            = $21 ;1 byte, tmp var for shift data subroutine
display_flags             = $22 ;1 byte, flags used in display routines
mon_mem_lo                = $23 ;1 byte, lo byte location used by the memory monitor
mon_mem_hi                = $24 ;1 byte, hi byte location used by the memory monitor

kb_buf = $0200 ;256 byte kb buffer 0200-02ff

decimal_value     = $0300 ;2 bytes, for use with printing numbers
decimal_mod10     = $0302 ;2 bytes, for use with printing numbers
decimal_value_buf = $0304 ;6 bytes, buf to hold number for printing
hexit             = $030a ;1 byte hex digit used for input parsing
mon_tmp_lo        = $030b ;1 byte, used by the memory monitor as a read/write address
mon_tmp_hi        = $030c ;1 byte
start_index       = $030d ;1 byte, used by command line parsing
end_index         = $030e ;1 byte, used by command line parsing
hex_found         = $030f ;1 byte, used by command line parsing

SCREEN_BUF_NUM_ROWS    = 18    ;0 bytes, number of rows in the screen buffer
SCREEN_BUF_ADDR_LO     = $00   ;0 bytes, address of screen buffer first row, lo byte
SCREEN_BUF_ADDR_HI     = $04   ;0 bytes, address of screen buffer first row, hi byte
SCREEN_BUF_ADDR_END_LO = $a8   ;0 bytes, address of screen buffer last row, lo byte
SCREEN_BUF_ADDR_END_HI = $06   ;0 bytes, address of screen buffer last row, hi byte
INPUT_BUF_ADDR_LO      = $d0   ;0 bytes, address of input buffer, lo byte
INPUT_BUF_ADDR_HI      = $06   ;0 bytes, address of input buffer, hi byte
cmd_args_indices       = $06f8 ;16 bytes, indices of command line arguments in screen row, first arg is command
CMD_LINE_MAX_ARGS      = 16    ;0 bytes, max number of command line args

LCD_BTM_ROW_START_ADDR = $40       ;0 bytes, lcd address of row 0, start
LCD_BTM_ROW_END_ADDR   = $67       ;0 bytes, lcd address of row 0, end
LCD_TOP_ROW_START_ADDR = $00       ;0 bytes, lcd address of row 1, start
LCD_TOP_ROW_END_ADDR   = $27       ;0 bytes, lcd address of row 1, end
LCD_DISPF_NOSHIFT      = %00000001 ;0 bytes, don't shift display when moving or printing

nmi:
  RTI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; interrupt handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
irq:
  PHA
  TXA
  PHA

  LDA via2_ifr
  AND #%00000010
  BNE irq_kb
irq_ticks:
  ; if here, timer went off
  BIT via1_t1cl
  INC ticks
  BNE irq_done
  INC ticks+1
  BNE irq_done
  INC ticks+2
  BNE irq_done
  INC ticks+3
  JMP irq_done

; got a keyboard scancode. if pressed, put the scancode
; into the keyboard buffer. if released, set a flag
; to ignore the next key.
irq_kb:
  LDA kb_flags_i
  AND #KBF_RELEASING
  BNE irq_kb_key_released ; deal with key that was released

  ; if here, new key is being pressed
  LDA via2_porta         ; read to clear the interrupt and get value
  CMP #KBSC_RELEASE
  BEQ irq_kb_sc_release
  CMP #KBSC_LSHIFT
  BEQ irq_kb_sc_lshift
  CMP #KBSC_RSHIFT
  BEQ irq_kb_sc_rshift

  ; if here, this is a non-special character

  ; first store the scan code in the keyboard buffer
  LDX kb_wptr
  STA kb_buf, x
  INC kb_wptr

  ; now store the flags in the kb buffer
  INX
  LDA kb_flags_i
  STA kb_buf, x
  INC kb_wptr
  JMP irq_done
irq_kb_sc_release:
  ; set a flag that the next scancode should be ignored
  LDA kb_flags_i
  ORA #KBF_RELEASING
  STA kb_flags_i
  JMP irq_done
irq_kb_sc_lshift:
  LDA kb_flags_i
  ORA #KBF_LSHIFT
  STA kb_flags_i
  JMP irq_done
irq_kb_sc_rshift:
  LDA kb_flags_i
  ORA #KBF_RSHIFT
  STA kb_flags_i
  JMP irq_done
irq_kb_key_released:
  ; if here, we're processing a key that was released
  LDA kb_flags_i
  AND #KBF_RELEASING_INV ; flip releasing bit back off
  STA kb_flags_i

  LDA via2_porta      ; read to clear the interrupt and get value
  CMP #KBSC_LSHIFT
  BEQ irq_kb_lshift_up
  CMP #KBSC_RSHIFT
  BEQ irq_kb_rshift_up
  JMP irq_done
irq_kb_lshift_up:
  LDA kb_flags_i
  AND #KBF_LSHIFT_INV
  STA kb_flags_i
  JMP irq_done
irq_kb_rshift_up:
  LDA kb_flags_i
  AND #KBF_RSHIFT_INV
  STA kb_flags_i
  JMP irq_done
irq_done:
  PLA
  TAX
  PLA
  RTI

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main reset loop called when cpu starts up
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
reset:
  ; reset stack pointer
  LDX #$ff
  TXS

  LDA #%11111111
  STA via1_ddrb ; outputs to control shift registers
  STA via1_ddra ; data output to lcd is on via1 porta
  STA via2_ddrb ; all of via2 portb to output, currently unused

  LDA #%00000000
  STA via2_ddra ; input from keyboard

  LDA #0
  STA kb_wptr
  STA kb_rptr
  STA kb_flags_i
  STA kb_flags_k
  STA output_data_sr1
  STA output_data_sr2
  STA cursor_col_offset
  STA rows_scrolled_up
  STA cursor_current_lcd_row
  STA display_offset
  STA display_flags
  STA cursor_tmp
  STA hexit
  STA mon_tmp_lo
  STA mon_tmp_hi
  STA start_index
  STA end_index
  STA acia_cmd

  LDA #INPUT_BUF_ADDR_LO
  STA input_buf_lo
  LDA #INPUT_BUF_ADDR_HI
  STA input_buf_hi

  JSR init_ticks_timer

  JSR reset_screen_buf
  JSR reset_screen_pointers

  ; enable output for shift registers
  LDA via1_portb
  AND #%11110111 ; bring output enable low
  STA via1_portb

  JSR lcd_init_loop
  JSR reprint_screen

  LDA #$00
  STA acia_status ; soft reset

  ; 1 stop bit, 8 bit word length, 19200 baud
  LDA #%00011111
  STA acia_ctrl

  ; no parity, no echo, no interrupts
  LDA #%00001011
  STA acia_cmd


  

  ; init keyboard interrupt trigger on rising edge
  LDA #$01
  STA via2_pcr
  LDA #%10000010 ; CA1
  STA via2_ier
  CLI

  JMP loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
loop:
  JSR rx_data
  JSR updkb
  JMP loop
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sleeps the given amount of time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sleep_10us:
  PHA
  LDA #$0a
  STA via1_t2cl
  LDA #$00
  STA via1_t2ch
  JMP sleep_loop
sleep_45us:
  PHA
  LDA #$2d
  STA via1_t2cl
  LDA #$00
  STA via1_t2ch
  JMP sleep_loop
sleep_50us:
  PHA
  LDA #$32
  STA via1_t2cl
  LDA #$00
  STA via1_t2ch
  JMP sleep_loop
sleep_520us:
  PHA
  LDA #$08
  STA via1_t2cl
  LDA #$02
  STA via1_t2ch
  JMP sleep_loop
sleep_1ms:
  PHA
  LDA #$e8
  STA via1_t2cl
  LDA #$03
  STA via1_t2ch
  JMP sleep_loop
sleep_10ms:
  PHA
  LDA #$10
  STA via1_t2cl
  LDA #$27
  STA via1_t2ch
  JMP sleep_loop
sleep_50ms:
  PHA
  LDA #$50
  STA via1_t2cl
  LDA #$c3
  STA via1_t2ch
  JMP sleep_loop
sleep_loop:
  LDA via1_ifr
  AND #%00100000
  BEQ sleep_loop
  LDA via1_t2cl ; ack expired timer
sleep_done:
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; initialize timer-based interrupt
; interrupts happen every n+2 cycles, so for interrupt every 10ms
; we want 9998 microseconds, $270e clock cycles at 1MHz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
init_ticks_timer:
  LDA #0
  STA ticks
  STA ticks+1
  STA ticks+2
  STA ticks+3

  ; put timer into continuous interrupt mode
  ; timer2 in 1-shot mode
  LDA #%01000000
  STA via1_acr

  ; 10ms = $270e
  ; 5ms  = $1386
  LDA #$0e
  STA via1_t1cl
  LDA #$27
  STA via1_t1ch

  ; Enable interrupts for Timer 1 in via1
  LDA #%11000000
  STA via1_ier
  CLI

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fills current row with the character in ascii_char_typed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fill_cursor_row:
  PHA
  TYA
  PHA

  LDA ascii_char_typed
  LDY #0
fill_cursor_row_loop:
  STA (screen_buf_cursor_row_lo), y
  INY
  CPY #ROW_LENGTH
  BNE fill_cursor_row_loop

  PLA
  TAY
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gets the location where the previous line was at based on
; the provided screen buf pointer
; inputs and outputs:
;   screen_buf_ptr_tmp_lo
;   screen_buf_ptr_tmp_hi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_older_screen_buf_row:
  PHA

  LDA screen_buf_ptr_tmp_lo
  CMP #SCREEN_BUF_ADDR_END_LO
  BNE get_older_screen_buf_row_nowrap

  LDA screen_buf_ptr_tmp_hi
  CMP #SCREEN_BUF_ADDR_END_HI
  BNE get_older_screen_buf_row_nowrap

  ; if here, we wrapped
  LDA #SCREEN_BUF_ADDR_LO
  STA screen_buf_ptr_tmp_lo
  LDA #SCREEN_BUF_ADDR_HI
  STA screen_buf_ptr_tmp_hi
  JMP get_older_screen_buf_row_done

get_older_screen_buf_row_nowrap:
  ; move the screen buf pointer
  LDA screen_buf_ptr_tmp_lo
  CLC
  ADC #ROW_LENGTH
  STA screen_buf_ptr_tmp_lo
  BCC get_older_screen_buf_row_done

  INC screen_buf_ptr_tmp_hi

get_older_screen_buf_row_done:
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gets the location where a newer screen buf line would go based on
; the current screen buf pointer
; inputs and outputs:
;   screen_buf_ptr_tmp_lo
;   screen_buf_ptr_tmp_hi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_newer_screen_buf_row:
  PHA

  LDA screen_buf_ptr_tmp_lo
  CMP #SCREEN_BUF_ADDR_LO
  BNE get_newer_screen_buf_row_nowrap

  LDA screen_buf_ptr_tmp_hi
  CMP #SCREEN_BUF_ADDR_HI
  BNE get_newer_screen_buf_row_nowrap

  ; if here, we wrapped
  LDA #SCREEN_BUF_ADDR_END_LO
  STA screen_buf_ptr_tmp_lo
  LDA #SCREEN_BUF_ADDR_END_HI
  STA screen_buf_ptr_tmp_hi
  JMP get_newer_screen_buf_row_done

get_newer_screen_buf_row_nowrap:
  ; move the screen buf pointer
  LDA screen_buf_ptr_tmp_lo
  SEC 
  SBC #ROW_LENGTH
  STA screen_buf_ptr_tmp_lo
  ; TODO test this
  BCS get_newer_screen_buf_row_done

  DEC screen_buf_ptr_tmp_hi

get_newer_screen_buf_row_done:
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; moves the screen buf down by a row and blank out the line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
add_screen_buf_row:
  PHA
  TYA
  PHA

  LDA screen_buf_ptr_lo
  STA screen_buf_ptr_tmp_lo
  LDA screen_buf_ptr_hi
  STA screen_buf_ptr_tmp_hi

  JSR get_newer_screen_buf_row

  LDA screen_buf_ptr_tmp_lo
  STA screen_buf_ptr_lo
  LDA screen_buf_ptr_tmp_hi
  STA screen_buf_ptr_hi

  ; now blank out the row
  LDA #' '
  LDY #0
add_screen_buf_row_blank:
  STA (screen_buf_ptr_lo),y
  INY
  CPY #ROW_LENGTH
  BNE add_screen_buf_row_blank

add_screen_buf_row_done:
  PLA
  TAY
  PLA
  RTS
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; resets the entire screen buffer and fills with ' '
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
reset_screen_buf:
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA #SCREEN_BUF_ADDR_LO
  STA screen_buf_ptr_tmp_lo
  LDA #SCREEN_BUF_ADDR_HI
  STA screen_buf_ptr_tmp_hi

  LDA #' '
  LDX #0
reset_screen_buf_row:
  LDY #0
reset_screen_buf_col:
  STA (screen_buf_ptr_tmp_lo),y
  INY
  CPY #ROW_LENGTH
  BNE reset_screen_buf_col
  
  INX
  CPX #SCREEN_BUF_NUM_ROWS
  BEQ reset_screen_buf_done

  JSR get_older_screen_buf_row
  JMP reset_screen_buf_row
reset_screen_buf_done:
  LDA #SCREEN_BUF_ADDR_LO
  STA screen_buf_ptr_lo
  LDA #SCREEN_BUF_ADDR_HI
  STA screen_buf_ptr_hi

  PLA
  TAY
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; resets the screen pointers:
;   screen_buf_cursor_row_lo
;   screen_buf_cursor_row_hi
;   screen_buf_bottom_row_lo
;   screen_buf_bottom_row_hi
;   screen_buf_top_row_lo
;   screen_buf_top_row_hi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
reset_screen_pointers:
  PHA

  ; bottom row should point at beginning of buffer
  LDA screen_buf_ptr_lo
  STA screen_buf_cursor_row_lo
  STA screen_buf_bottom_row_lo
  STA screen_buf_ptr_tmp_lo
  LDA screen_buf_ptr_hi
  STA screen_buf_cursor_row_hi
  STA screen_buf_bottom_row_hi
  STA screen_buf_ptr_tmp_hi

  ; top row is one older than bottom row
  JSR get_older_screen_buf_row
  LDA screen_buf_ptr_tmp_lo
  STA screen_buf_top_row_lo
  LDA screen_buf_ptr_tmp_hi
  STA screen_buf_top_row_hi

  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reprints both lines of the display from the beginning of the lines
; leaves the cursor at home on the bottom row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
reprint_screen:
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA #0
  STA cursor_col_offset
  STA cursor_tmp
  STA display_offset

  JSR row_home

  JSR move_cursor_to_top_row
  JSR reprint_from_cursor_tmp
  JSR move_cursor_to_bottom_row
  JSR reprint_from_cursor_tmp

  PLA
  TAY
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; prints rest of row from provided cursor_tmp, returning cursor
; to cursor_col_offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
reprint_from_cursor_tmp:
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA display_flags
  ORA #LCD_DISPF_NOSHIFT
  STA display_flags

  ; disable cursor on lcd
  LDA #%00001100
  STA via1_porta
  JSR lcd_send_instruction

  ; save to reposition cursor at end
  LDX cursor_col_offset

  LDY cursor_tmp
  STY cursor_col_offset
  JSR lcd_move_cursor_to_offset
reprint_from_cursor_tmp_loop:
  LDA (screen_buf_cursor_row_lo), y 
  STA ascii_char_typed
  JSR cursor_row_print_char
  INY
  CPY #ROW_LENGTH
  BNE reprint_from_cursor_tmp_loop

  ; restore the cursor to original position
  STX cursor_col_offset
  JSR lcd_move_cursor_to_offset

reprint_from_cursor_tmp_done:
  ; re-enable cursor on lcd
  LDA #%00001110
  STA via1_porta
  JSR lcd_send_instruction

  LDA display_flags
  EOR #LCD_DISPF_NOSHIFT
  STA display_flags

  PLA
  TAY
  PLA
  TAX
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; event triggered after the user clicked backspaced key.
; in this case, we move the cursor to the left and then move all the 
; characters that were to the right over one to the left.
; Assumes the cursor was in the input row when this occurred.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
backspace:
  PHA
  TYA
  PHA

  LDA cursor_col_offset
  BEQ backspace_done

  DEC cursor_col_offset
  JSR lcd_move_cursor_left
  JSR test_and_shift_display

  LDY cursor_col_offset
backspace_loop:
  INY
  LDA (screen_buf_cursor_row_lo), y 
  DEY
  STA (screen_buf_cursor_row_lo), y 

  INY
  CPY #(ROW_LENGTH-1)
  BCC backspace_loop

  ; replace last char with a blank
  LDA #' '
  STA (screen_buf_cursor_row_lo), y

  LDA cursor_col_offset
  STA cursor_tmp
  JSR reprint_from_cursor_tmp
backspace_done:
  PLA
  TAY
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; event triggered after the user clicked insert key
; insert a space at the cursor, then shift the rest
; of the row to the right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
insert:
  PHA
  TYA
  PHA

  LDA cursor_col_offset
  CMP #(ROW_LENGTH-1)
  BEQ insert_done

  LDY #(ROW_LENGTH-1)
insert_loop:
  DEY
  LDA (screen_buf_cursor_row_lo), y 
  INY
  STA (screen_buf_cursor_row_lo), y

  DEY
  CPY cursor_col_offset
  BNE insert_loop

  ; replace char at insert point with blank
  LDA #' '
  LDY cursor_col_offset
  STA (screen_buf_cursor_row_lo), y

  LDA cursor_col_offset
  STA cursor_tmp
  JSR reprint_from_cursor_tmp
insert_done:
  PLA
  TAY
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; returns the cursor to home on current row and scrolls
; the display back as well
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
row_home:
  PHA
  LDA #0
  STA cursor_col_offset
  STA display_offset

  JSR lcd_home

  JSR lcd_move_cursor_to_offset
  JSR test_and_shift_display

  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fills the current row with ' ', moves home, reprints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
row_home_with_clear:
  PHA

  LDA #0
  STA cursor_col_offset
  STA cursor_tmp
  STA display_offset

  JSR lcd_home

  LDA #' '
  STA ascii_char_typed
  JSR fill_cursor_row
  JSR lcd_move_cursor_to_offset
  JSR reprint_from_cursor_tmp
  JSR test_and_shift_display

  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; parses the value in A to see if it is a hex digit. modifies A
; output:
;   hexit - will contain the parsed hex in bits 0-3 if valid, otherwise #%10000000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
parse_hexit:
  PHA
  SEC
  SBC #'0'
  BCC parse_hexit_not_digit      ; < '0', not a digit
  CMP #10
  BCC parse_hexit_is_digit       ; between 0 and 9
  SBC #17
  BCC parse_hexit_not_digit      ; between 9 and 'A'-1, not a digit
  CMP #6
  BCC parse_hexit_is_char_digit  ; 'A' to 'F', is a digit
  SBC #32
  BCC parse_hexit_not_digit      ; between 'Z' and 'a', not a digit
  CMP #6                       ; 'a' to 'f', is a digit
  BCC parse_hexit_is_char_digit
  JMP parse_hexit_not_digit

parse_hexit_is_char_digit:
  CLC
  ADC #10
parse_hexit_is_digit:
  AND #%00001111
  STA hexit
  JMP parse_hexit_done
parse_hexit_not_digit:
  LDA #%10000000
  STA hexit
parse_hexit_done:
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; writes the digit in A to the screen buf at location Y, modifies Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
write_hexit_to_screen_buf:
  PHA
  ROR
  ROR
  ROR
  ROR
  AND #%00001111
  CMP #10
  BCC write_digit1
  BCS write_chardigit1
write_digit1:
  CLC
  ADC #'0'
  STA (screen_buf_ptr_lo),y
  JMP write_hexit_to_screen_buf_hexit2
write_chardigit1:
  CLC
  ADC #'a'
  SEC
  SBC #10
  STA (screen_buf_ptr_lo),y

write_hexit_to_screen_buf_hexit2:
  INY

  PLA
  PHA
  AND #%00001111
  CMP #10
  BCC write_digit2
  BCS write_chardigit2
write_digit2:
  CLC
  ADC #'0'
  STA (screen_buf_ptr_lo),y
  JMP write_hexit_to_screen_buf_done
write_chardigit2:
  CLC
  ADC #'a'
  SEC
  SBC #10
  STA (screen_buf_ptr_lo),y

write_hexit_to_screen_buf_done:
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; dumps the memory at the given location to the fist screen buf row
; inputs:
;   mon_mem_lo - lo byte location of memory to dump
;   mon_mem_hi - hi byte location of memory to dump
;   Y - index in screen buffer row to write the data, modified
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mon_dump_mem_screen_buf:
  PHA
  TXA
  PHA
  LDA zbb1
  PHA
  LDA zbb2
  PHA

  LDA #'$'
  STA (screen_buf_ptr_lo),y
  INY

  ; first write the requested memory location to the output buffer
  LDA mon_mem_hi
  JSR write_hexit_to_screen_buf
  INY
  LDA mon_mem_lo
  JSR write_hexit_to_screen_buf

  INY
  LDA #':'
  STA (screen_buf_ptr_lo),y

  LDX #0
mon_dump_mem_screen_buf_dump_loop:
  INY

  LDA #' '
  STA (screen_buf_ptr_lo),y


  ; have to use y for the type of indexing we are doing
  ; but we want to grab the data from the x offset
  STX zbb1
  STY zbb2
  TXA
  TAY
  LDA (mon_mem_lo),y
  LDX zbb1
  LDY zbb2

  INY
  JSR write_hexit_to_screen_buf

  INX
  CPX #8
  BNE mon_dump_mem_screen_buf_dump_loop
  
mon_dump_mem_screen_buf_done:
  PLA
  STA zbb2
  PLA
  STA zbb1
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; gets the next hex value from input buffer starting at start_index
; stores in two bytes. if it reads more than two bytes, it will keep the far
; right bytes.
; outputs:
;   mon_tmp_lo
;   mon_tmp_hi
;   hex_found   - #0 if at least one hex digit was found
;   start_index
;     - end_index+1 if we reach end of the row
;     - location of first non-hex digit
; inputs:
;   start_index - first index to search
;   end_index   - one past last index to search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
parse_hex:
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA #1
  STA hex_found

  LDA #0
  STA mon_tmp_lo
  STA mon_tmp_hi

  LDY start_index
parse_hex_next:
  LDA (input_buf_lo), y
  JSR parse_hexit
  BIT hexit
  BMI parse_hex_done
  
  LDA hexit
  ; update the value we're parsing
  ; some code from wozmon
  ASL
  ASL
  ASL
  ASL

  LDX #4
parse_hex_shift:
  ASL
  ROL mon_tmp_lo
  ROL mon_tmp_hi
  DEX
  BNE parse_hex_shift

  LDX #0
  STX hex_found

  INY
  STY start_index
  CPY end_index
  BCC parse_hex_next
parse_hex_done:
  PLA
  TAY
  PLA
  TAX
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; finds the next argument on the input line
; inputs:
;   input_buf_lo - the command line
;   start_index  - where to start searching on the line
; outputs:
;   start_index  - if arg found, start index of next argument, else #$ff
;   end_index    - if arg found, one past end index of argument, else #$ff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
get_next_arg:
  PHA
  TXA
  PHA
  TYA
  PHA

  LDY start_index
  CPY #ROW_LENGTH
  BCC get_next_arg_find_start

  LDY #$ff
  STY start_index
  STY end_index
  JMP get_next_arg_done

get_next_arg_find_start:
  LDA (input_buf_lo), y
  CMP #'$'
  BEQ get_next_arg_single
  CMP #':'
  BEQ get_next_arg_single
  CMP #' '
  BNE get_next_arg_normal

  ; if here, ' ' char
  INY
  CPY #ROW_LENGTH
  BNE get_next_arg_find_start

  LDA #$ff
  STA start_index
  STA end_index
  JMP get_next_arg_done
get_next_arg_single:
  STY start_index
  STY end_index
  INC end_index
  JMP get_next_arg_done
get_next_arg_normal:
  STY start_index
get_next_arg_find_end_loop:
  INY
  CPY #ROW_LENGTH ; todo test this
  BEQ get_next_arg_set_end

  LDA (input_buf_lo),y
  CMP #'$'
  BEQ get_next_arg_set_end
  CMP #':'
  BEQ get_next_arg_set_end
  CMP #' '
  BEQ get_next_arg_set_end
  JMP get_next_arg_find_end_loop

get_next_arg_set_end:
  STY end_index
get_next_arg_done:
  PLA
  TAY
  PLA
  TAX
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; handle $ memory command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
handle_mon_mem:
  PHA
  TXA
  PHA
  TYA
  PHA

  ; get the address we're going to read or write from
  JSR get_next_arg
  LDY start_index
  CPY #$ff
  BEQ handle_mon_mem_syntax_error

  JSR parse_hex
  LDA hex_found
  BNE handle_mon_mem_syntax_error ; wasn't a hex value

  ; starting address to read or write from
  LDA mon_tmp_lo
  STA mon_mem_lo
  LDA mon_tmp_hi
  STA mon_mem_hi

  LDA end_index
  STA start_index
  JSR get_next_arg

  LDY start_index
  CPY #$ff
  BEQ handle_mon_mem_dump; no args, just dump memory

  LDA (input_buf_lo),y
  CMP #':'
  BEQ handle_mon_mem_write

  JMP handle_mon_mem_dump; weird arg, just dump memory
handle_mon_mem_write:
  LDY #0 ; index off mem to write to
handle_mon_mem_write_loop:
  LDA end_index
  STA start_index

  JSR get_next_arg
  LDA start_index
  CMP #$ff
  BEQ handle_mon_mem_dump

  JSR parse_hex
  LDA hex_found
  BNE handle_mon_mem_dump ; not a hex value, just dump memory

  LDA mon_tmp_lo
  STA (mon_mem_lo),y
  INY
  JMP handle_mon_mem_write_loop
handle_mon_mem_syntax_error:
  JSR syntax_error
  JMP handle_mon_mem_done
handle_mon_mem_dump:
  JSR add_screen_buf_row
  LDY #0
  JSR mon_dump_mem_screen_buf
handle_mon_mem_done:
  PLA
  TAY
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; handle $N and $P commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
handle_mon_next:
  PHA
  LDA mon_mem_lo
  CLC
  ADC #8
  STA mon_mem_lo
  BCC handle_mon_np_dump
  INC mon_mem_hi
  JMP handle_mon_np_dump
handle_mon_prev:
  PHA
  LDA mon_mem_lo
  SEC
  SBC #8
  STA mon_mem_lo
  BCS handle_mon_np_dump
  DEC mon_mem_hi
  JMP handle_mon_np_dump
handle_mon_np_dump:
  JSR add_screen_buf_row
  LDY #0
  JSR mon_dump_mem_screen_buf
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; print a syntax error
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
syntax_error:
  PHA
  TXA
  PHA
  TYA
  PHA

  JSR add_screen_buf_row
  LDY #0
  LDX #0
syntax_error_loop:
  LDA STR_SYNTAX_ERROR,x
  BEQ syntax_error_done

  STA (screen_buf_ptr_lo),y
  INX
  INY
  JMP syntax_error_loop

syntax_error_done:
  PLA
  TAY
  PLA
  TAX
  PLA

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; process user input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
exec_cmd:
  PHA
  TXA
  PHA
  TYA
  PHA

  LDY #0
  STY start_index
  JSR get_next_arg

  LDY start_index
  CPY #$ff
  BEQ exec_cmd_done ; no command

  LDA (input_buf_lo), y
  CMP #'$'
  BEQ exec_cmd_DOLLAR

  JMP exec_cmd_unknown
exec_cmd_DOLLAR:
  INC start_index
  JSR get_next_arg
  LDY start_index
  CPY #$ff
  BEQ exec_cmd_syntax_error

  LDA (input_buf_lo),y
  CMP #'N'
  BEQ exec_cmd_DOLLAR_N
  CMP #'n'
  BEQ exec_cmd_DOLLAR_N
  CMP #'P'
  BEQ exec_cmd_DOLLAR_P
  CMP #'p'
  BEQ exec_cmd_DOLLAR_P

  JSR handle_mon_mem
  JMP exec_cmd_done
exec_cmd_DOLLAR_N:
  JSR handle_mon_next
  JMP exec_cmd_done
exec_cmd_DOLLAR_P:
  JSR handle_mon_prev
  JMP exec_cmd_done
exec_cmd_syntax_error:
  JSR syntax_error
  JMP exec_cmd_done
exec_cmd_unknown:
  JSR add_screen_buf_row
  LDX #0
exec_cmd_unknown_loop:
  LDA STR_UNKNOWN_CMD,x
  BEQ exec_cmd_done

  STA (screen_buf_ptr_lo),y
  INX
  INY
  JMP exec_cmd_unknown_loop
exec_cmd_done:
  PLA
  TAY
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; copies the line the cursor is on to the input buffer and the screen buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
commit_cursor_line:
  PHA
  TYA
  PHA
  ; copy the row the cursor is on to the input buffer.
  ; might be inefficient, but this keeps us safe if a command writes
  ; out more rows than we have in our screen buffer.
  ; also copy it to the screen buffer for history.
  ; TODO this is an unnecessary copy if the cursor is on the same row
  ; as the first row in the screen buffer. optimize later.
  LDY #0
commit_cursor_line_loop:
  LDA (screen_buf_cursor_row_lo),y
  STA (screen_buf_ptr_lo),y
  STA (input_buf_lo),y
  INY
  CPY #ROW_LENGTH
  BNE commit_cursor_line_loop

  PLA
  TAY
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; handle return key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
return:
  PHA

  JSR commit_cursor_line
  JSR exec_cmd
  JSR add_screen_buf_row

  JSR reset_screen_pointers

  LDA #0
  STA rows_scrolled_up
  STA cursor_tmp
  JSR move_cursor_to_top_row
  JSR reprint_from_cursor_tmp

  LDA #0
  STA cursor_tmp
  JSR move_cursor_to_bottom_row
  JSR reprint_from_cursor_tmp
  JSR row_home

return_done:
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; unimplemented
; just prints whatever is in decimal_value
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
runstop:
  PHA
  JSR print_decimal
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rs232 send
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
tx_data_rs232:
  PHA
  STA acia_data
  ; workaround ACIA hw bug where we can't trust tx status
  ; 520us works for 19200
  JSR sleep_520us
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rs232 rcv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
rx_data:
  PHA
  LDA acia_status
  AND #%00001000 ; rx buffer full
  BEQ rx_data_done

  LDA acia_data
  STA ascii_char_typed
  JSR cursor_row_store_char
  JSR cursor_row_print_char

rx_data_done:
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; keyboard handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
updkb:
  SEI
  LDA kb_rptr
  CMP kb_wptr
  CLI
  BNE updkb_process_key
  JMP updkb_done
updkb_process_key:
  ; read control flags
  LDX kb_rptr
  LDA kb_buf+1,x
  STA kb_flags_k

  ; read scancode
  LDA kb_buf, x
  CMP #$0c
  BEQ updkb_cursor_leftright
  CMP #$07
  BEQ updkb_cursor_updown
  CMP #$66
  BEQ updkb_backspace
  CMP #$5a
  BEQ updkb_return
  CMP #$0b
  BEQ updkb_home
  CMP #$7e
  BEQ updkb_runstop

  ; if here, we're dealing with a key we want to print
  TAX ; move scancode to x register
  LDA kb_flags_k
  AND #(KBF_LSHIFT | KBF_RSHIFT)
  BNE updkb_shifted_key

  LDA keymap, x         ; map to character code
  JMP updkb_print
updkb_shifted_key:
  LDA keymap_shifted, x ; map to character code
updkb_print:
  STA ascii_char_typed
  JSR cursor_row_store_char
  JSR cursor_row_print_char
  JMP updkb_scancode_done
updkb_cursor_leftright:
  LDA kb_flags_k
  AND #(KBF_LSHIFT | KBF_RSHIFT)
  BNE updkb_move_cursor_left
  JSR move_cursor_right
  JMP updkb_scancode_done
updkb_move_cursor_left:
  JSR move_cursor_left
  JMP updkb_scancode_done
updkb_cursor_updown:
  LDA kb_flags_k
  AND #(KBF_LSHIFT | KBF_RSHIFT)
  BNE updkb_move_cursor_up
  JSR move_cursor_down
  JMP updkb_scancode_done
updkb_move_cursor_up:
  JSR move_cursor_up
  JMP updkb_scancode_done
updkb_backspace:
  LDA kb_flags_k
  AND #(KBF_LSHIFT | KBF_RSHIFT)
  BNE updkb_insert
  JSR backspace
  JMP updkb_scancode_done
updkb_insert:
  JSR insert
  JMP updkb_scancode_done
updkb_return:
  JSR return
  JMP updkb_scancode_done
updkb_home:
  LDA kb_flags_k
  AND #(KBF_LSHIFT | KBF_RSHIFT)
  BNE updkb_clear_home
  JSR row_home
  JMP updkb_scancode_done
updkb_clear_home:
  JSR row_home_with_clear
  JMP updkb_scancode_done
updkb_runstop:
  JSR runstop
  JMP updkb_scancode_done
updkb_scancode_done:
  ; processed scancode, let's see if there are more
  INC kb_rptr
  INC kb_rptr
  JMP updkb
updkb_done:
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; stores a key typed at the location of the current cursor_col_offset
; inputs:
;   cursor_col_offset - position in current row of character
;   ascii_char_typed  - ascii char that was pressed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cursor_row_store_char:
  PHA
  TYA
  PHA

  LDY cursor_col_offset
  LDA ascii_char_typed
  STA (screen_buf_cursor_row_lo), y 
cursor_row_store_char_done:
  PLA
  TAY
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; prints the character on current row at cursor_col_offset
; inputs:
;   cursor_col_offset - position in current row of character
;   ascii_char_typed  - ascii char that was pressed
; outputs:
;   cursor_col_offset - incremented if needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cursor_row_print_char:
  PHA
  TYA
  PHA

  LDY cursor_col_offset
  LDA (screen_buf_cursor_row_lo), y 
  STA via1_porta
  JSR lcd_send_data

   ; cursor moved already, so match offset to real position
  INC cursor_col_offset
  LDA cursor_col_offset
  CMP #ROW_LENGTH
  BCS cursor_row_print_char_wrapped

  JSR test_and_shift_display
  JMP cursor_row_print_char_done

cursor_row_print_char_wrapped:
  ; move the cursor back one to end of row
  DEC cursor_col_offset
  JSR lcd_move_cursor_left
cursor_row_print_char_done:
  PLA
  TAY
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; prints a decimal value to the screen.
; whatever is in decimal_value and decimal_value+1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print_decimal:
  PHA
  TXA
  PHA
  TYA
  PHA
  LDA zbb1
  PHA
  LDA zbb2
  PHA

  LDA #0
  STA zbb1

print_decimal_loop_outer:
  LDA #0
  STA decimal_mod10
  STA decimal_mod10+1
  CLC

  LDX #16
print_decimal_loop_inner:
  ; rotate quotient and remainder
  ; how ROL works:
  ;  each bit is shifted left,
  ;  bit 7 shifted into carry bit
  ;  previous carry bit shifted into
  ;  bit 0.
  ;  C76543210 -> 76543210C
  ROL decimal_value
  ROL decimal_value+1
  ROL decimal_mod10
  ROL decimal_mod10+1

  ; a,y = dividend - divisor
  SEC ;set carry bit to know if borrow
  LDA decimal_mod10
  SBC #10
  TAY ; save lo byte of sub in Y
  LDA decimal_mod10+1
  SBC #0
  ; branch if dividend < divisor
  BCC print_decimal_ignoreresult
  STY decimal_mod10
  STA decimal_mod10+1
print_decimal_ignoreresult:
  DEX
  BNE print_decimal_loop_inner
  ; shift in last bit of quotient
  ROL decimal_value
  ROL decimal_value+1

  LDA decimal_mod10
  CLC
  ADC #'0' ;convert to ascii

  STX zbb2 ; push x into zbb2 to retain it
  LDX zbb1
  STA decimal_value_buf, x 
  INC zbb1
  LDX zbb2 ; grab x back from zbb2

  ; done when result of div is zero
  LDA decimal_value
  ; if any bits of lo or hi byte
  ; are set then not done
  ORA decimal_value+1
  BNE print_decimal_loop_outer; branch if not 0

  LDX zbb1
print_decimal_output:
  ; now output the decimal to the lcd
  LDA decimal_value_buf-1, x
  STA ascii_char_typed
  JSR cursor_row_store_char
  JSR cursor_row_print_char
  DEX
  BNE print_decimal_output
print_decimal_done:
  PLA
  STA zbb2
  PLA
  STA zbb1
  PLA
  TAY
  PLA
  TAX
  PLA

  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; shifts the bytes in shift_data_tmp to the output registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
shift_byte_to_output:
  PHA
  TXA
  PHA

  LDX #0
shift_bit_to_output_loop:
  ; via1_portb bits:
  ; 0 - SER
  ; 1 - SRCLK
  ; 2 - RCLK

  ; set SER bit with data to write
  PHA
  LDA shift_data_tmp
  AND #%00000001 ; keep right most bit only
  ORA via1_portb
  STA via1_portb
  PLA

  ; now signal the shift with the srclk bit
  LDA via1_portb
  ORA #%00000010 ; set SRCLK bit
  STA via1_portb

  ; now lower the srclk
  LDA via1_portb
  AND #%11110001 ; set SER bit only
  STA via1_portb

  ; now lower all the clk and ser bits
  LDA via1_portb
  AND #%11110000 ; lower all SR bits 
  STA via1_portb

  ; rotate to the next bit
  ROR shift_data_tmp
  INX
  CPX #8
  BNE shift_bit_to_output_loop

  ; rotate final bit into C and then back into bit 7
  ROR shift_data_tmp
  ROR shift_data_tmp

shift_byte_to_output_done:
  PLA
  TAX
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; shifts data to the three output shift registers
; 
; inputs
;   output_data_sr1 - used for lcd instructions
;   output_data_sr2 - currently unused
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
shift_output:
  PHA

  ; make sure the latch and all bits are low
  LDA via1_portb
  AND #%11110000 ; lower all shift register bits
  STA via1_portb

  LDA output_data_sr2
  STA shift_data_tmp
  JSR shift_byte_to_output

  LDA output_data_sr1
  STA shift_data_tmp
  JSR shift_byte_to_output

  ; raise the latch
  LDA via1_portb
  ORA #%00000100 ; raise the latch
  STA via1_portb

  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; tests and shifts display to put the cursor back on screen if needed
; e.g. if the cursor is to the left of the start of the screen, move
; the display left. If off the right side, shift display right.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
test_and_shift_display:
  PHA
  LDA display_flags
  AND #LCD_DISPF_NOSHIFT 
  CMP #LCD_DISPF_NOSHIFT 
  BEQ test_and_shift_display_done

tasd_loop:
  LDA cursor_col_offset
  CMP display_offset
  BEQ test_and_shift_display_done
  BCC tasd_left ; cursor off left side of screen

  CLC
  LDA cursor_col_offset
  SBC display_offset
  CMP #(LCD_SCREEN_WIDTH-1)
  BCS tasd_right ; cursor past right side of screen

  JMP test_and_shift_display_done

tasd_left:
  DEC display_offset
  JSR lcd_shift_display_left
  JMP tasd_loop
tasd_right:
  INC display_offset
  JSR lcd_shift_display_right
  JMP tasd_loop
test_and_shift_display_done:
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move the cursor left if allowed, shift display if needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
move_cursor_left:
  PHA

  LDA cursor_col_offset
  BEQ move_cursor_left_done

  DEC cursor_col_offset
  JSR lcd_move_cursor_left

  JSR test_and_shift_display

move_cursor_left_done:
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move the cursor right if allowed, shift display if needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
move_cursor_right:
  PHA

  LDA cursor_col_offset
  CMP #(ROW_LENGTH-1)
  BEQ move_cursor_right_done ; at end of row already

  INC cursor_col_offset
  JSR lcd_move_cursor_right

  JSR test_and_shift_display

move_cursor_right_done:
  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; updates the top row to point to the provided screen buf ptr and reprints
; the row. Returns the cursor to its previous location.
; inputs:
;   screen_buf_ptr_tmp_(lo/hi) - location of the row in the screen buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
update_top_row:
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX screen_buf_cursor_row_lo
  LDY screen_buf_cursor_row_hi

  LDA screen_buf_ptr_tmp_lo
  STA screen_buf_top_row_lo
  STA screen_buf_cursor_row_lo
  LDA screen_buf_ptr_tmp_hi
  STA screen_buf_top_row_hi
  STA screen_buf_cursor_row_hi

  LDA #0
  STA cursor_tmp
  JSR reprint_from_cursor_tmp

  STX screen_buf_cursor_row_lo
  STY screen_buf_cursor_row_hi

  PLA
  TAY
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move cursor up. if on top row of lcd, scrolls back through screen buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
move_cursor_up:
  PHA
  TXA
  PHA

  ; if cursor is on the bottom row, just move the cursor up
  LDA cursor_current_lcd_row
  BEQ move_cursor_up_just_move

  ; if we have scrolled to the end of history, ignore
  LDA rows_scrolled_up
  CMP #(SCREEN_BUF_NUM_ROWS-2)
  BEQ move_cursor_up_done
  
  INC rows_scrolled_up

  ; bottom row becomes previous top row
  LDA screen_buf_top_row_lo
  STA screen_buf_ptr_tmp_lo
  STA screen_buf_bottom_row_lo
  LDA screen_buf_top_row_hi
  STA screen_buf_ptr_tmp_hi
  STA screen_buf_bottom_row_hi

  ; top row scrolls back into history
  JSR get_older_screen_buf_row
  LDA screen_buf_ptr_tmp_lo
  STA screen_buf_top_row_lo
  LDA screen_buf_ptr_tmp_hi
  STA screen_buf_top_row_hi

  LDA #0
  STA cursor_tmp
  JSR move_cursor_to_bottom_row
  JSR reprint_from_cursor_tmp
  JSR move_cursor_to_top_row
  JSR reprint_from_cursor_tmp

  JMP move_cursor_up_done
move_cursor_up_just_move:
  JSR move_cursor_to_top_row
move_cursor_up_done:
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; move cursor down. scrolls through screen buffer until back to bottom
; of input row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
move_cursor_down:
  PHA
  TXA
  PHA

  ; if cursor is on the top row, just move the cursor down 
  LDA cursor_current_lcd_row
  BNE move_cursor_down_just_move

  ; if we haven't scrolled up at all, just ignore
  LDA rows_scrolled_up
  BEQ move_cursor_down_done

  DEC rows_scrolled_up

  ; top row becomes previous bottom row
  LDA screen_buf_bottom_row_lo
  STA screen_buf_ptr_tmp_lo
  STA screen_buf_top_row_lo
  LDA screen_buf_bottom_row_hi
  STA screen_buf_ptr_tmp_hi
  STA screen_buf_top_row_hi

  ; bottom row is newer than our previous bottom row
  JSR get_newer_screen_buf_row
  LDA screen_buf_ptr_tmp_lo
  STA screen_buf_bottom_row_lo
  LDA screen_buf_ptr_tmp_hi
  STA screen_buf_bottom_row_hi

  LDA #0
  STA cursor_tmp
  JSR move_cursor_to_top_row
  JSR reprint_from_cursor_tmp
  JSR move_cursor_to_bottom_row
  JSR reprint_from_cursor_tmp

  JMP move_cursor_down_done
move_cursor_down_just_move:
  JSR move_cursor_to_bottom_row
move_cursor_down_done:
  PLA
  TAX
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; moves the cursor to top row on the lcd display.
; updates all necessary offsets and whatnot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
move_cursor_to_top_row:
  PHA

  LDA screen_buf_top_row_lo
  STA screen_buf_cursor_row_lo
  LDA screen_buf_top_row_hi
  STA screen_buf_cursor_row_hi

  CLC
  LDA #LCD_TOP_ROW_START_ADDR
  STA cursor_row_start_addr
  ADC cursor_col_offset
  ORA #%10000000
  STA via1_porta
  JSR lcd_send_instruction

  LDA #1
  STA cursor_current_lcd_row

  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; moves the cursor to bottom row on the lcd display.
; updates all necessary offsets and whatnot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
move_cursor_to_bottom_row:
  PHA

  LDA screen_buf_bottom_row_lo
  STA screen_buf_cursor_row_lo
  LDA screen_buf_bottom_row_hi
  STA screen_buf_cursor_row_hi

  CLC
  LDA #LCD_BTM_ROW_START_ADDR
  STA cursor_row_start_addr
  ADC cursor_col_offset
  ORA #%10000000
  STA via1_porta
  JSR lcd_send_instruction

  LDA #0
  STA cursor_current_lcd_row
 
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wait on the lcd to finish instruction
;  This was originally implemented by checking the busy signal, but for
;  simplicity it was re-implemented to use sleeps. Makes wiring easier
;  and means we don't have to deal with R/W pins and all that
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_wait:
  JSR sleep_45us
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; moves the cursor to screen home, doesn't update any cursor or other
; offsets. Just moves the cursor to home
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_home:
  PHA

  LDA #%00000010 ; home command
  STA via1_porta
  JSR lcd_send_instruction

  ; minimum 1.52ms sleep, we'll sleep 3
  JSR sleep_1ms
  JSR sleep_1ms
  JSR sleep_1ms

  PLA
  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sends instruction to the lcd to move the cursor to our currently
; set offset on the current row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_move_cursor_to_offset:
  PHA
  CLC
  LDA cursor_row_start_addr
  ADC cursor_col_offset
  ORA #%10000000
  STA via1_porta
  JSR lcd_send_instruction
lcd_move_cursor_to_offset_done:
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sends instruction to lcd to move the cursor left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_move_cursor_left:
  PHA
  LDA #%00010000
  STA via1_porta
  JSR lcd_send_instruction
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sends instruction to lcd to move the cursor right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_move_cursor_right:
  PHA
  LDA #%00010100
  STA via1_porta
  JSR lcd_send_instruction
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sends data to the display.
; inputs:
;   output_data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_send_data:
  PHA

  JSR lcd_wait

  ; prep for write data
  LDA #LCD_OUTPUT_RS
  STA output_data_sr1
  JSR shift_output

  ; send the data
  LDA #LCD_OUTPUT_RS
  ORA #LCD_OUTPUT_E
  STA output_data_sr1
  JSR shift_output

  ; clear bits
  LDA #0
  STA output_data_sr1
  JSR shift_output

  PLA

  RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sends an instruction to the display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_send_instruction:
  PHA

  JSR lcd_wait

  ; clear the RS/RW/E bits
  LDA #0
  STA output_data_sr1
  JSR shift_output

  LDA #LCD_OUTPUT_E
  STA output_data_sr1
  JSR shift_output

  ; clear the RS/RW/E bits
  LDA #0
  STA output_data_sr1
  JSR shift_output

  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; perform reset sequence and wait for lcd to be ready
; Following page 46:
;  https://www.sparkfun.com/datasheets/LCD/HD44780.pdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_init_loop:
  PHA

  ; start reset sequence

  ; Wait for at least 40ms before communicating with LCD
  ; after reset. Doing 100 to be safe
  JSR sleep_50ms
  JSR sleep_50ms

  LDA #%00110000
  STA via1_porta
  JSR lcd_send_instruction

  ; sleep at least 4.1ms, we'll do 10
  JSR sleep_10ms
 
  ; send again
  LDA #%00110000
  STA via1_porta
  JSR lcd_send_instruction

  ; sleep at least 100us, we'll do 200
  JSR sleep_50us
  JSR sleep_50us
  JSR sleep_50us
  JSR sleep_50us

  ; send again
  LDA #%00110000
  STA via1_porta
  JSR lcd_send_instruction

  ; end reset sequence, now set up display functions

  ; 8bit mode, 2 line disp, 5x8 font
  LDA #%00111000
  STA via1_porta
  JSR lcd_send_instruction

  ; display on, cursor on, blink off
  LDA #%00001110
  STA via1_porta
  JSR lcd_send_instruction

  ; incr and shift cursor, not disp
  LDA #%00000110
  STA via1_porta
  JSR lcd_send_instruction

  ; reset and clear display
  LDA #%00000001
  STA via1_porta
  JSR lcd_send_instruction

  ; sleep a little just to give some time just in case
  JSR sleep_10ms

  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sends instruction to lcd to shift the display left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_shift_display_left:
  PHA
  LDA #%00011100
  STA via1_porta
  JSR lcd_send_instruction
  PLA
  RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sends instruction to lcd to shift the display right
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
lcd_shift_display_right:
  PHA
  LDA #%00011000
  STA via1_porta
  JSR lcd_send_instruction
  PLA
  RTS


strings:
STR_SYNTAX_ERROR:
  STRING "syntax error"
STR_UNKNOWN_CMD:
  STRING "unknown command"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ORG $fd00
keymap:
  BYTE "?C????$??@:?? `?" ; 00-0F
  BYTE "?????q1???zsaw2?" ; 10-1F
  BYTE "?cxde43?? vftr5?" ; 20-2F
  BYTE "?nbhgy6???mju78?" ; 30-3F
  BYTE "?,kio09??./l;p-?" ; 40-4F
  BYTE "??'?[=?????]?\??" ; 50-5F
  BYTE "?????????1?47???" ; 60-6F
  BYTE "0.2568??^+3-*9??" ; 70-7F
  BYTE "????????????????" ; 80-8F
  BYTE "????????????????" ; 90-9F
  BYTE "????????????????" ; A0-AF
  BYTE "????????????????" ; B0-BF
  BYTE "????????????????" ; C0-CF
  BYTE "????????????????" ; D0-DF
  BYTE "????????????????" ; E0-EF
  BYTE "??$?????????????" ; F0-FF

keymap_shifted:
  BYTE "?C????????[?? `?" ; 00-0F
  BYTE '?????Q!???ZSAW"?' ; 10-1F
  BYTE "?CXDE$#?? VFTR%?" ; 20-2F
  BYTE "?NBHGY&???MJU'(?" ; 30-3F
  BYTE "?<KIO?)??>?L]P??" ; 40-4F
  BYTE "????????????????" ; 50-5F
  BYTE "?????????!?$'???" ; 60-6F
  BYTE '?<"%&(????3??)??' ; 70-7F
  BYTE "????????????????" ; 80-8F
  BYTE "????????????????" ; 90-9F
  BYTE "????????????????" ; A0-AF
  BYTE "????????????????" ; B0-BF
  BYTE "????????????????" ; C0-CF
  BYTE "????????????????" ; D0-DF
  BYTE "????????????????" ; E0-EF
  BYTE "????????????????" ; F0-FF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reset vectors

  ORG $fffa

  WORD nmi
  WORD reset
  WORD irq
