IR Vector table

; PREPROCESSOR OPTIONS

.EQU      AVR_STUDIO_6 = 0     ; 0 means AVR STUDIO 4.xx  -  1 means AVR STUDIO 6.xx

.IF       AVR_STUDIO_6
.ELSE
.INCLUDE "tn861def.inc"         ; used in AVR STUDIO 4.xx  Version 2 only
.ENDIF



; ERRATA:

; no known errata

;####################################################################################################################
;####################################################################################################################
;##                                                                                                                ##
;##     Titel:          7 Segment GEBER for example with ATtiny861    3V0..5V5                                     ##
;##                                                                                                                ##
;##     V 0.0.3         05.01.2020                                                                                 ##
;##                     DD.MM.YYYY                                                                                 ##
;##     Origin date:    06.07.2018                                                                                 ##
;##                                                                                                                ##
;##     Author:         Leon                                                                                    ##
;##                                                                                                                ##
;##     Pgm size:       x,x Kbytes                                                                                 ##
;.Device ATtiny861              ;                                                                                  ##
;##     System clock    4,000 MHz     RC 8 MHz/2                                                                   ##
;##                                                                                                                ##
.EQU    FEB   = 0xFF           ;  default                                                                          ##
.EQU    FHB   = 0xDF           ;  default                                                                          ##
.EQU    FLB   = 0x62           ;  default                                                                          ##
.EQU    LOCKS = 0xFC           ;  modified                                                                         ##
;##                                                                                                                ##
;####################################################################################################################
;####################################################################################################################


; Ports
.EQU  PA_O  = PORTA            ; SFR PA out      ; 8 bit       0:7
.EQU  PA_D  = DDRA             ; SFR PA DDR      ; 8 bit       0:7
.EQU  PA_I  = PINA             ; SFR PA in       ; 8 bit       0:7

.EQU  PB_O  = PORTB            ; SFR PB out      ; 8 bit       0:7
.EQU  PB_D  = DDRB             ; SFR PB DDR      ; 8 bit       0:7
.EQU  PB_I  = PINB             ; SFR PB in       ; 8 bit       0:7

;--------------   ##76543210   ; Bit order --- PA-Pin order, in top view and  top   row of HW, is 0123-+4567 
;                 ..xxxxxxxx   ;           --- PA-Pin order, in top view and bottom row of HW, is xxxx+-xxxx 
.EQU  PA_dirc  =  0b11111111   ; DDR A settings  0=input  1=output

;                 ..xxxxxxxx
.EQU  PA_lvl   =  0b11111111   ; PA   out level  0=L      1=H     - H at input means that internal pullups are active

;                 ..xxxxxxxx
.EQU  PB_dirc  =  0b00000000   ; DDR B settings  0=input  1=output

;                 ..xxxxxxxx
.EQU  PB_lvl   =  0b11111111   ; PB   out level  0=L      1=H     - H at input means that internal pullups are active
;--------------   ##76543210   ; Bit order --- PB-Pin order, in top view and  top   row of HW, is xxxx-+xxxx 
;                              ;           --- PB-Pin order, in top view and bottom row of HW, is 0123+-4567 
  
.EQU SYSCO     =  GPIOR0       ; system control
.EQU TASKCO    =  GPIOR1       ;  task  control
.EQU S_SREG    =  GPIOR2       ;  saved PSW

.EQU S_res0    =  0            ; SYSCO
.EQU S_res1    =  1            ; SYSCO
.EQU S_res2    =  2            ; SYSCO
.EQU S_res3    =  3            ; SYSCO
.EQU S_res4    =  4            ; SYSCO
.EQU S_res5    =  5            ; SYSCO
.EQU S_res6    =  6            ; SYSCO
.EQU S_res7    =  7            ; SYSCO

.EQU T_res0    =  0            ; TASKCO
.EQU T_res1    =  1            ; TASKCO
.EQU T_res2    =  2            ; TASKCO
.EQU T_res3    =  3            ; TASKCO
.EQU T_res4    =  4            ; TASKCO
.EQU T_res5    =  5            ; TASKCO
.EQU T_res6    =  6            ; TASKCO
.EQU T_res7    =  7            ; TASKCO

; port A bits

.EQU PA_res0   =  0            ; 
.EQU PA_res1   =  1            ; 
.EQU PA_res2   =  2            ; 
.EQU PA_res3   =  3            ; 
.EQU PA_res4   =  4            ; 
.EQU PA_res5   =  5            ; 
.EQU PA_res6   =  6            ; 
.EQU PA_res7   =  7            ; 

; port B bits

.EQU PB_res0   =  0            ; 
.EQU SIGNAL    =  1            ; 
.EQU PB_res2   =  2            ; 
.EQU PB_res3   =  3            ; 
.EQU PB_res4   =  4            ; 
.EQU PB_res5   =  5            ; 
.EQU PB_res6   =  6            ; 
.EQU PB_res7   =  7            ; Reset

; register pairs

.DEF  WL       =  R24          ; reg pair W
.DEF  WH       =  R25          ; reg pair W 
;____________________________________________________________________________________________________________________  
;____________________________________________________________________________________________________________________

; SRAM            stack and buffers           

.dseg 
;
.org $0060
;                        
STK_end:  .byte  $1F           ; stack area is from $0060 to $007F       ; STACK
STACK:    .byte  $01           ; - starting stackpointer  at $007F -     ; STACK
;---------------------------------------------------------------------------------------------------

SEND_BUF: .byte $10            ; 
SRAM:     .byte $1D0           ; start address    = $0080 to $025F       ; free space 
SRAM_end: .byte  $00           ;  end  address +1 = $0260                ; end marker
;____________________________________________________________________________________________________________________  

; EEPROM   512 bytes
.eseg 
;
.org $0000

EEPROM:   .byte $200           ; start address    = $0000 to $01FF       ; free space 
EEP_end:  .byte $00            ;  end  address +1 = $0200                ; end marker
;____________________________________________________________________________________________________________________

; FLASH
;
.cseg
;
.org $0000

; IR Vector table

IV0:       rjmp init         ; reset HW,POR,BOR,WDT Vector - skip IR area  
IV1:       reti              ; IRV1  INT0 pin 9
IV2:       reti              ; IRV2  PCINT all pins
IV3:       reti              ; IRV3  T1 COMP A
IV4:       reti              ; IRV4  T1 COMP B
IV5:       reti              ; IRV5  T1 OVF
IV6:       reti              ; IRV6  T0 OVF
IV7:       reti              ; IRV7  USI START
IV8:       reti              ; IRV8  USI OVF
IV9:       reti              ; IRV9  EE_RDY
IVA:       reti              ; IRVA  ANALOG COMP
IVB:       reti              ; IRVB  ADC
IVC:       reti              ; IRVC  WDT
IVD:       reti              ; IRVD  INT1 pin 18
IVE:       reti              ; IRVE  T0 COMP A
IVF:       reti              ; IRVF  T0 COMP B
IV10:      reti              ; IRV10 T0 COMP CAPT
IV11:      reti              ; IRV11 T1 COMP D
IV12:      reti              ; IRV12 T1 COMP fault prot.

;--------------------------------------------------------------------------------------------------------------------
;####################################################################################################################
;####################################             IR handler                  #######################################
;####################################################################################################################
;--------------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------------------------        
init:      cli                 ; disable global IRs
;-------------------------------------------------------------  
; prepare MCUSR - clear WDRF
           wdr                 ; reset watchdog
           in r16,MCUSR        ; read MCUSR
           andi r16,$07        ; mask WDRF off
           out MCUSR,r16       ; clear WDRF

; settings only valid when WDTON is unprogrammed

; disable watchdog - as long as WDTON is unprogrammed
; *** NOTE         always disable WD this way, when not used    ***   
           ldi r16, $18        ; watchdog change enable setting
           ldi r17, $00        ; watchdog WDE clear setting
           out WDTCSR,r16      ; watchdog change enable
           out WDTCSR,r17      ; watchdog WDE clear
;-------------------------------------------------------------------------------
; sys clock change
           ldi r16, $80        ; prepare sys clock change
           ldi r17, $01        ; set 4 MHz clock
           out CLKPR,r16       ; prepare out  
           out CLKPR,r17       ; setting out
           nop                 ; must be nop for settling
;---------------------------------------------------------------
           ldi r16, high(STACK) ; init stack high
           out SPH, r16         ; SPH
           ldi r16, low (STACK) ; stack is below SRAM $0080
           out SPL, r16         ; SPL
                      
           ldi r16,  PA_dirc   ; PA DDR           Port A settings
           out PA_D, r16       ; set PA_D
           ldi r16,  PA_lvl    ; PA out level
           out PA_O, r16       ; set PA_O
           
           ldi r16,  PB_dirc   ; PB DDR           Port B settings
           out PB_D, r16       ; set PB_D
           ldi r16,  PB_lvl    ; PB out level
           out PB_O, r16       ; set PB_O    
;-------------------------------------------------------------------------------------------------------------------
       ;    rcall dly_100mS     ; power on delay
;-------------------------------------------------------------------------------------------------------------------

           rcall auto_lock     ; Program Flashmemory >>>  Schreib-Lese-Sperre (2 Lockbits) automatisch setzen

;-------------------------------------------------------------------------------------------------------------------
            
inits:     ldi r16,  $00       ; clear
           out GPIOR0,r16      ; GPR0
           
           ldi r16,  $00       ; clear
           out GPIOR1,r16      ; GPR1
           
           in r16, SREG        ; init GPR2
           out GPIOR2,r16      ; with SREG
           
           ldi r16,$00
           out TC1H,r16
           
           ldi r16,$70
           out OCR1A,r16	                     
              
           ldi r16,$00
           out TCCR1A,r16	                     
          
           ldi r16,$02
           out TCCR1B,r16	

           ldi r16,$80
           out TCCR0A,r16	                     
          
           ldi r16,$01
           out TCCR0B,r16		   
;      interrupt enables can be here
      
           ldi r16,$40
           out TIMSK,r16	                     
          
           ldi r16,$40
           out TIFR,r16	

           clr r0

           rjmp Main           ; start main loop  
		   
;--------------------------------------------------------------------------------------------------------------------
;####################################################################################################################
;############################            E N D      INIT              ###############################################
;####################################################################################################################
;-------------------------------------------------------------------------------------------------------------------- 

;--------------------------------------------------------------------------------------------------------------------
;####################################################################################################################
;####################################################################################################################
;#                                                                                                                  #  
;#         M       M       AA      IIIII    N     N                                                                 # 
;#         MM     MM      A  A       I      NN    N                                                                 #
;#         M M   M M     A    A      I      N N   N                                                                 #
;#         M  M M  M    AAAAAAAA     I      N  N  N                                                                 #
;#         M   M   M    A      A     I      N   N N                                                                 # 
;#         M       M    A      A     I      N    NN                                                                 #
;#         M       M    A      A   IIIII    N     N                                                                 #
;#                                                                                                                  #
;####################################################################################################################
;####################################################################################################################
;--------------------------------------------------------------------------------------------------------------------

MAIN:    

; samples
           ldi XL,$00
           inc r0 
           mov XL,r0
           cpi XL,$10
           brlo go
           clr r0
go:        rcall draw
           sts SEND_BUF+0,XL
           ldi XL,$01
           rcall draw
           sts SEND_BUF+1,XL
           ldi XL,$02
           rcall draw
           sts SEND_BUF+2,XL
           ldi XL,$03
           rcall draw
           sts SEND_BUF+3,XL
           ldi XL,$04
           rcall draw
           sts SEND_BUF+4,XL
           ldi XL,$05
           rcall draw
           sts SEND_BUF+5,XL
           ldi XL,$06
           rcall draw
           sts SEND_BUF+6,XL
           ldi XL,$07
           rcall draw
           sts SEND_BUF+7,XL
           ldi XL,$08
           rcall draw
           sts SEND_BUF+8,XL
;end samples
           
           rcall Q_L_HS_SEND   ; 
           
           rcall dly_100mS

           rjmp MAIN           ; loop

;--------------------------------------------------------------------------------------------------------------------
;####################################################################################################################
;####################################               Subroutines               #######################################
;####################################################################################################################
;-------------------------------------------------------------------------------------------------------------------- 
;--------------------------------------------------------------------------------------------------------------------
EEP_read:  cli                 ; issues read data at R16
           sbic EECR,1         ; poll EEPROM busy bit
           rjmp EEP_read       ; wait when busy
           out EEARH,ZH        ; use EEPROM high address
           out EEARL,ZL        ; use EEPROM low  address
           sbi EECR,0          ; set read mode bit 0
           in  r16,EEDR        ; get data to R16
r_busy:    sbic EECR,1         ; poll busy bit
           rjmp r_busy         ; wait when busy
           ret                 ; back
;--------------------------------------------------------------------------
EEP_wrt:   cli                 ; write data are prepared in R16 
           sbic EECR,1         ; poll EEPROM busy bit
           rjmp EEP_wrt        ; wait when busy
           out EEARH,ZH        ; use EEPROM high address
           out EEARL,ZL        ; use EEPROM low  address
           out EEDR,r16        ; put write data
           sbi EECR,2          ; set prepare writing bit
           sbi EECR,1          ; set write bit
w_busy:    sbic EECR,1         ; poll busy bit
           rjmp w_busy         ; wait when busy
           ret                 ; back
;-----------------------------------------------------------------------------------------------------------------------
auto_lock: cli                 ; disable IRs 
           ldi r16,$FF         ; prepare value
           mov r0, r16         ; set output register different to expected result
           ldi ZH, $00         ; set Z high
           ldi ZL, $01         ; set Z low for reading lock bits
           ldi r17,$09         ; set SPMCSR to read lock and fuse and enable it 
           out SPMCSR,r17      ; set SFR
           lpm                 ; readout to R0
l_busy:    in r16,SPMCSR       ; poll busy bit
           sbrc r16,0          ; copy busy to r16
           rjmp l_busy         ; wait when busy           
           ldi r16,LOCKS       ; set expected compare value (both locks enabled)
           cp r0, r16          ; compare with result
           breq A_locked       ; continue if unit is locked 
apply_LCK: ldi ZH, $00         ; set Z high
           ldi ZL, $01         ; set Z low for reading lock bits
           ldi r16,LOCKS       ; $FC is fully locked
           mov r0, r16         ; set data reg0 for write
           ldi r17,$09         ; set SPMCSR to read lock and fuse and enable it 
           out SPMCSR,r17      ; set SFR
           spm                 ; write lock bits (autolock done)
p_busy:    in r16,SPMCSR       ; poll busy bit
           sbrc r16,0          ; copy busy to r16
           rjmp p_busy         ; wait when busy
A_locked:  ret                 ; back
;--------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------

;********************************************************************************************************************
;***********************   SEND PART - NEW ALPINA PROTOCOL **********************************************************
;********************************************************************************************************************
;********************************************************************************************************************

.EQU Q_LSEND_BIT   = SIGNAL
.EQU Q_LSEND_PIN   = PINB
.EQU Q_LSEND_DIRC  = DDRB
.EQU Q_LSEND_PORT  = PORTB
.EQU Q_LSEND_SPEED = $03   ; normally 0x03
.EQU Q_LSEND_LEN   = $09

; speed rules

; sender and receiver at same sys clock can handle send_speed from 0x03 (fastest) to 0x0F (slowest)
; preferrable SEND SPEED = 0x08
; with SEND_SPEED 0x11 to 0x5E the receiver is allowed to be eight times slower, than the sender
; receiver is never allowed to have less than 25% clock speed than sender 
; so max clock configurations for bidirectionality is 4:1 or 1:4


Q_L_HS_SEND: ; cli                   ; disable interrupts after saving SREG
           
; save used regs

           push r16
           push r17
           push r18
           push r19
           push r20
           push r21
           push  ZH
           push  ZL

           ldi ZH,high (SEND_BUF)  ; declare data source
           ldi ZL, low (SEND_BUF)  ; declare data source
           
           ldi r20,$A5             ; init CRC field  (including len field)  - xor
           ldi r21,$C3             ; init SUM field  (including len field)  - sub

; wakeup, sync
           
           sbi Q_LSEND_DIRC,Q_LSEND_BIT ; define line as output
           nop                     ; adjust
           cbi Q_LSEND_PORT,Q_LSEND_BIT ; drop line
           rcall Q_L_dly_UNIT      ; wake up timing 1 unit
           sbi Q_LSEND_PORT,Q_LSEND_BIT ;  set line
           rcall Q_L_dly_UNIT      ; wake up timing 1 unit
           cbi Q_LSEND_PORT,Q_LSEND_BIT ; drop line
           rcall Q_L_dly_UNIT      ; sync timing 1st unit
           rcall Q_L_dly_UNIT      ; sync timing 2nd unit
           rcall Q_L_dly_UNIT      ; sync timing 3rd unit
           rcall Q_L_dly_UNIT      ; sync timing 4th unit
           sbi Q_LSEND_PORT,Q_LSEND_BIT ;  set line
           rcall Q_L_dly_UNIT      ; timing 1 unit
 
; data transfer

           ldi r16,Q_LSEND_LEN     ; get count registers to be sent + this count
           mov r18,r16             ; load byte counter 
           eor r20,r16             ; do CRC 
           sub r21,r16             ; do checksum
           rcall Q_L_SNDbyte       ; send len parameter
           
Q_L__neby: ld r16,Z+               ; get next send data
           eor r20,r16             ; do CRC 
           sub r21,r16             ; do checksum
           rcall Q_L_SNDbyte       ; send
           dec r18                 ; length count 
           brne Q_L__neby          ; next byte to send
 
           mov r16,r20             ; get  CRC
           rcall Q_L_SNDbyte       ; send CRC
           
           mov r16,r21             ; get  checksum
           rcall Q_L_SNDbyte       ; send checksum
           
; EOM

           cbi Q_LSEND_PORT,Q_LSEND_BIT ; drop line
           rcall Q_L_dly_UNIT      ; EOM mark timing 1 unit
           sbi Q_LSEND_PORT,Q_LSEND_BIT ;  set line
           rcall Q_L_dly_UNIT      ; free bus - now idle

; restore used regs           

           pop  ZL 
           pop  ZH
           pop r21
           pop r20
           pop r19
           pop r18
           pop r17
           pop r16
           
           cbi Q_LSEND_DIRC,Q_LSEND_BIT ; define line as input
           nop
           ret                     ; send ends
;--------------------------------------------------------------------------------------------------------------------
; send timing including rcall and ret
; 20 cc at SEND_SPEED 0x00 and 0x01
; each SEND_SPEED count more, adds 20 cc  = 20 uS at 1 MHz

Q_L_dly_UNIT: 
           push r20
           ldi r20,Q_LSEND_SPEED   ; send speed parameter
           cpi r20,$02
           brlo Q_L_dret0
Q_L_rep20: dec r20
           breq Q_L_dret1 
           rjmp Q_L_dtd0
Q_L_dtd0:  rjmp Q_L_dtd1
Q_L_dtd1:  rjmp Q_L_dtd2
Q_L_dtd2:  rjmp Q_L_dtd3
Q_L_dtd3:  rjmp Q_L_dtd4
Q_L_dtd4:  rjmp Q_L_dtd5
Q_L_dtd5:  rjmp Q_L_dtd6
Q_L_dtd6:  rjmp Q_L_dtd7
Q_L_dtd7:  rjmp Q_L_rep20 
Q_L_dret0: rjmp Q_L_dret1
Q_L_dret1: rjmp Q_L_dret2
Q_L_dret2: nop 
           pop r20    
           ret
;---------------------------------------------------------           
; send r16  -  starts with down line and ends with up line          
           
Q_L_SNDbyte: ldi r17,$08  
Q_L__nebi: sbi Q_LSEND_PORT,Q_LSEND_BIT ; initial up line
           ror r16                 ; get bit to carry
           brcc Q_L__lowbit        ; decide
           rjmp Q_L__hghbit        ; is highbit
Q_L__lowbit: nop                   ; delay
           cbi Q_LSEND_PORT,Q_LSEND_BIT ; drop line      
           rcall Q_L_dly_UNIT      ; hold single unit 
           sbi Q_LSEND_PORT,Q_LSEND_BIT ;  set line  
           rcall Q_L_dly_UNIT      ; hold  first unit 
           rcall Q_L_dly_UNIT      ; hold second unit 
           dec r17                 ; next bit
           brne Q_L__nebi          ; last bit ?  
           ret                     ; done - last bit was low 
Q_L__hghbit: cbi Q_LSEND_PORT,Q_LSEND_BIT ; drop line      
           rcall Q_L_dly_UNIT      ; hold  first unit
           rcall Q_L_dly_UNIT      ; hold second unit
           sbi Q_LSEND_PORT,Q_LSEND_BIT ; drop line  
           rcall Q_L_dly_UNIT      ; hold single unit
           dec r17                 ; next bit
           brne Q_L__nebi          ; last bit ?  
           ret                     ; done - last bit was high
;--------------------------------------------------------------------------------------------------------------------
; delay call routines - valid for AVR clock 4 MHz

dly_10uS:  push r22            ;  10uS    including call and return
           wdr
           rjmp djm1
djm1:      ldi r22,$09
dd10:      dec r22
		   brne dd10
		   pop  r22
           ret
         
dly_100uS: push r22            ; 100uS
           push r21
           push r20 
           ldi r22,$01     
           ldi r21,$01
           ldi r20,$5D
           rjmp djm2
djm2:      rjmp dela

dly_1mS:   push r22            ;   1mS
           push r21
           push r20 
           ldi r22,$01     
           ldi r21,$04
           ldi r20,$E0
           rjmp dela

dly_10mS:  push r22            ;  10mS
           push r21
           push r20 
           ldi r22,$01     
           ldi r21,$27
           ldi r20,$F6
           rjmp djm3
djm3:      rjmp dela

dly_100mS: push r22            ; 100mS
           push r21
           push r20 
           ldi r22,$02     
           ldi r21,$86
           ldi r20,$D6
           rjmp djm4
djm4:      rjmp dela

dly_1S:    push r22            ;    1S
           push r21
           push r20 
           ldi r22,$10     
           ldi r21,$3B
		   ldi r20,$95
           rjmp dela

dela:      wdr                 ; used for all
           dec r20   
           brne dela
           dec r21
           brne dela
           dec r22
           brne dela
           pop r20
           pop r21
           pop r22
           ret 
;--------------------------------------------------------------------------------------------------------------------
; draw character by XL in and out   - change XL format from ascii to seven segment code                         *****
;                                                                                                               *****
DRAW:      ; do not push pop XL                                                                                 *****
           push r23            ;                                                                                *****
           in r23,SREG         ;                                                                                *****
           push r23            ;                                                                                *****
           push XH             ;                                                                                *****
           push ZL             ;                                                                                *****
           push ZH             ;                                                                                *****
;                                                                                                               *****
           ldi r23,$00         ; set 16-bit zero value                                                          *****
           ldi ZH, high(CHAR_TAB *2 ) ; set char table pointer  *2 means byte format                            *****
           ldi ZL,  low(CHAR_TAB *2 ) ; set char table pointer  *2 means byte format                            *****
           clt                 ; clear decimal point marker                                                     *****
           andi XL,$7F         ; mask character                                                                 *****
           cpi  XL,$50         ; 0x50 or higher ?                                                               *****
           brlo nodeci         ; no decimal point                                                               *****
           set                 ; set decimal point marker                                                       *****
           subi XL,$50         ; remove decimal point offset                                                    *****
nodeci:    add  ZL,XL          ; character offset within table                                                  *****
           adc  ZH,r23         ; carry                                                                          *****
           lpm  XL,Z           ; get from char table                                                            *****
           brtc ndci           ; no decimal point                                                               *****
           ori XL,$80          ; add decimal point                                                              *****
                               ; do not push pop XL                                                             *****
ndci:      pop ZH              ;                                                                                *****
           pop ZL              ;                                                                                *****
           pop XH              ;                                                                                *****
           pop r23             ;                                                                                *****
           out SREG,r23        ;                                                                                *****
           pop r23             ;                                                                                *****
           ret                 ; XL contains char byte result                                                   *****
;--------------------------------------------------------------------------------------------------------------------
;                                                                                                               *****
;;; dp=80,a=01,b=02,c=04,d=08,e=10,f=20,g=40                                                                    *****
;                                                                                                               *****
;                                                                                                               *****
; CHARACTER TABLE - how characters look                                                                         *****
;                                                                                                               *****
; CHAR GEN      X0  X1  X2  X3  X4  X5  X6  X7  X8  X9  XA  XB  XC  XD  XE  XF    ; <<<< column                 *****
;                                                                                 ; V  row V                    *****
;                0   1   2   3   4   5   6   7   8   9   A   b   C   d   E   F    ; 0X     5X                   *****
CHAR_TAB:   .db $3F,$06,$5B,$4F,$66,$6D,$7D,$07,$7F,$6F,$77,$7C,$39,$5E,$79,$71   ; 0X     5X                   *****
;                                                                                                               *****
;                G   H   I   J   K   L   M   n   o   P   Q   r   S   T   U   V    ; 1X     6X                   *****
            .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; 1X     6X                   *****
;                                                                                                               *****
;                W   X   Y   Z  " "  TM low hgh  ?   #   _   "   h   >   <   =    ; 2X     7X                   *****
            .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; 2X     7X                   *****
;                                                                                                               *****
;                N  top  ö   ü   !  "s" "z"  c   )  ")"  j   l   R   x   µ  Ohm   ; 3X     8X                   *****
            .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; 3X     8X                   *****
;                                                                                                               *****
;                TB  Sa  Sb  Sc  Sd  Se  Sf  Sg B(b)D(d)res res res res res res   : 4X     9X                   *****
            .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00   ; 4X     9X                   *****
;                                                                                                               *****
;--------------------------------------------------------------------------------------------------------------------