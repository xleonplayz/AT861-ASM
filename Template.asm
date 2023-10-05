; PREPROCESSOR OPTIONS

.EQU      AVR_STUDIO_6 = 0     ; 0 means AVR STUDIO 4.xx  -  1 means AVR STUDIO 6.xx

.IF       AVR_STUDIO_6
.ELSE
.INCLUDE "m328def.inc"         ; used in AVR STUDIO 4.xx  Version 2 only
.ENDIF

; Autolock will only run, when into boot loader area

; ERRATA:

; Analog MUX can be turned off when setting ACME (analog comparator multiplexer enable) bit. See manual for detailes
; 32 KHz external OSC can not be used as system clock

;####################################################################################################################
;####################################################################################################################
;##                                                                                                                ##
;##     Titel:          Template ATmega328 also valid for ATmega168PA                                              ##
;##                                                                                                                ##
;##     V 0.0.0         26.08.2019                                                                                 ##
;##                     DD.MM.YYYY                                                                                 ##
;##     Origin date:    26.08.2019                                                                                 ##
;##                                                                                                                ##
;##                                                                                                                ##
;##     Pgm size:       x,x Kbytes                                                                                 ##
;.Device ATmega328      ;                                                                                          ##
;##     System clock    1,000 MHz     RC 8 MHz/8                                                                   ##
;##                                                                                                                ##
.EQU    FEB   = 0xFF           ;  default                                                                          ##
.EQU    FHB   = 0xD9           ;  default                                                                          ##
.EQU    FLB   = 0x62           ;  default                                                                          ##
.EQU    LOCKS = 0xCC           ;  modified                                                                         ##
;##                                                                                                                ##
;####################################################################################################################
;####################################################################################################################


;--------------   ##76543210   ; Bit order --- Pin order, in top view of HW, is ******-*+54321
;--------------   ##76543210   ; Bit order --- Pin order, in top view of HW, is ******+-67***0 
.EQU  PB_dirc  =  0b00000000   ; DDR B settings  0=input  1=output
.EQU  PB_lvl   =  0b11111111   ; PB   out level  0=L      1=H     - H at input means that internal pullups are active

;--------------   ##76543210   ; Bit order --- Pin order, in top view of HW, is 543210-*+*****
;--------------   ##76543210   ; Bit order --- Pin order, in top view of HW, is 6*****+-****** 
.EQU  PC_dirc  =  0b00000000   ; DDR C settings  0=input  1=output
.EQU  PC_lvl   =  0b11111111   ; PC   out level  0=L      1=H     - H at input means that internal pullups are active

;--------------   ##76543210   ; Bit order --- Pin order, in top view of HW, is ******-*+*****
;--------------   ##76543210   ; Bit order --- Pin order, in top view of HW, is *01234+-**567* 
.EQU  PD_dirc  =  0b00000000   ; DDR D settings  0=input  1=output
.EQU  PD_lvl   =  0b11111111   ; PD   out level  0=L      1=H     - H at input means that internal pullups are active

  
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


; port B bits

.EQU PB_res0   =  0            ; 
.EQU PB_res1   =  1            ; 
.EQU PB_res2   =  2            ; 
.EQU PB_res3   =  3            ; 
.EQU PB_res4   =  4            ; 
.EQU PB_res5   =  5            ; 
.EQU PB_res6   =  6            ; XTAL1
.EQU PB_res7   =  7            ; XTAL2

; port C bits

.EQU PC_res0   =  0            ; 
.EQU PC_res1   =  1            ; 
.EQU PC_res2   =  2            ; 
.EQU PC_res3   =  3            ; 
.EQU PC_res4   =  4            ; 
.EQU PC_res5   =  5            ; 
.EQU PC_res6   =  6            ; Reset
;  PC7 not present

; port D bits

.EQU PD_res0   =  0            ; 
.EQU PD_res1   =  1            ; 
.EQU PD_res2   =  2            ; 
.EQU PD_res3   =  3            ; 
.EQU PD_res4   =  4            ; 
.EQU PD_res5   =  5            ; 
.EQU PD_res6   =  6            ; 
.EQU PD_res7   =  7            ;



.DEF  WL    =  R24 ; reg pair W
.DEF  WH    =  R25 ; reg pair W 

;____________________________________________________________________________________________________________________  
;____________________________________________________________________________________________________________________
; stack and buffers  SRAM 2Kb

.dseg 
;
.org $0100
;                        
STK_end:  .byte $1F            ; stack area is from $0100 to $011F       ; STACK
STACK:    .byte $01            ; - starting stackpointer  at $011F -     ; STACK
;-------------------------------------------------------------------------------
SRAM:     .byte $7E0           ; start address    = $0120 to $08FF       ; free space 
SRAM_end: .byte $00            ;  end  address +1 = $0900                ; end marker
;____________________________________________________________________________________________________________________  
;
; EEPROM   1Kb        
.eseg 
;
.org $0000

EEPROM:   .byte $400           ; start address    = $0000 to $0400       ; free space 
EEP_end:  .byte $00            ;  end  address +1 = $0400                ; end marker
;____________________________________________________________________________________________________________________

.cseg
;
.org $0000

IV0:       jmp INIT            ; RESET External Pin, POR, BOR, WDR
IV1:       jmp unused_ir                ; INT0
IV2:       jmp unused_ir                ; INT1
IV3:       jmp unused_ir                ; PCINT0
IV4:       jmp unused_ir                ; PCINT1
IV5:       jmp unused_ir                ; PCINT2
IV6:       jmp unused_ir                ; WDT 
IV7:       jmp unused_ir                ; TIMER2 COMPA
IV8:       jmp unused_ir                ; TIMER2 COMPB
IV9:       jmp unused_ir                ; TIMER2 OVF 
IVA:       jmp unused_ir                ; TIMER1 CAPT 
IVB:       jmp unused_ir                ; TIMER1 COMPA 
IVC:       jmp unused_ir                ; TIMER1 COMPB
IVD:       jmp unused_ir                ; TIMER1 OVF
IVE:       jmp unused_ir                ; TIMER0 COMPA
IVF:       jmp unused_ir                ; TIMER0 COMPB
IV10:      jmp unused_ir                ; TIMER0 OVF
IV11:      jmp unused_ir                ; SPI, STC
IV12:      jmp unused_ir                ; USART, RX USART Rx Complete
IV13:      jmp unused_ir                ; USART, UDRE USART, Data Register Empty
IV14:      jmp unused_ir                ; USART, TX USART, Tx Complete
IV15:      jmp unused_ir                ; ADC Conversion Complete
IV16:      jmp unused_ir                ; EE READY
IV17:      jmp unused_ir                ; ANALOG COMP
IV18:      jmp unused_ir                ; TWI 2-wire Serial Interface
IV19:      jmp unused_ir                ; SPM READY

unused_ir: reti                ; unused IRs

;####################################################################################################################
;####################################        S T A R T    C O N F I G         #######################################
;####################################################################################################################
;--------------------------------------------------------------------------------------------------------------------        
init:      cli                 ; disable global IRs
;-----------------------------------  
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
           sts WDTCSR,r16      ; watchdog change enable
           sts WDTCSR,r17      ; watchdog WDE clear

; enable watchdog reset only                    
; *** NOTE                 no WD IR - reset only                ***  
       ;    ldi  r16, $1E       ; set timed sequence - including 1 second setting
       ;    sts  WDTCSR,r16     ; enable change watchdog settings
       ;    andi r16, $2F       ; parameter filter
       ;    sts  WDTCSR,r16     ; set watchdog reset timing
 
 ; enable watchdog interrupt only
; *** NOTE          no reset, even IR's are disabled            ***    
; *** NOTE reenable WDIE in WDTCSR at the end of WDT IR routine ***   
       ;    ldi  r16, $DE       ; set timed sequence - including 1 second setting
       ;    sts  WDTCSR,r16     ; enable change watchdog settings
       ;    andi r16, $67       ; parameter filter
       ;    sts  WDTCSR,r16     ; set watchdog reset timing

; enable watchdog interrupt and alternative reset
; *** NOTE              resets if IR's disabled                 ***    
; *** NOTE  reenable WDIE in WDTCSR at the end of WDT IR routine to avoid reset at next timeout  ***   
       ;    ldi  r16, $DE       ; set timed sequence - including 1 second setting
       ;    sts  WDTCSR,r16     ; enable change watchdog settings
       ;    andi r16, $EF       ; parameter filter
       ;    sts  WDTCSR,r16     ; set watchdog reset timing
;-------------------------------------------------------------------------------
; sys clock change
           ldi r16, $80        ; prepare sys clock change - do not change the 0x80
           sts CLKPR,r16       ; prepare out  
           ldi r16, $00        ; set 8MHz RC clock
           sts CLKPR,r16       ; setting out
           nop
;-----------------------------------                
           ldi r16, high(STACK) ; init stack high
           out $3E, r16        ; SPH
           ldi r16, low (STACK) ; stack is at SRAM $011F      
           out $3D, r16        ; SPL
;-----------------------------------                     
; Autolock will only work, when set into boot area
     ;;;;;       rcall auto_lock         
;-----------------------------------                     
config:    ldi r16,  PB_dirc   ; PB DDR           Port B settings
           out DDRB, r16       ; set PB_D
           nop                 ; sync
           ldi r16,  PB_lvl    ; PB out level
           out PORTB, r16      ; set PB_O
                
           ldi r16,  PC_dirc   ; PC DDR           Port C settings
           out DDRC, r16       ; set PC_D
           nop                 ; sync
           ldi r16,  PC_lvl    ; PC out level
           out PORTC, r16      ; set PC_O
           
           ldi r16,  PD_dirc   ; PD DDR           Port D settings
           out DDRD, r16       ; set PD_D
           nop                 ; sync
           ldi r16,  PD_lvl    ; PD out level
           out PORTD, r16      ; set PD_O
                   
           ; interrupt enables can be here
           
           rjmp Main           
;--------------------------------------------------------------------------------------------------------------------
;####################################################################################################################
;####################################          E N D      C O N F I G         #######################################
;####################################################################################################################
;-------------------------------------------------------------------------------------------------------------------- 

;-------------------------------------------------------------------------------------------------------------------
;###################################################################################################################
;###################################################################################################################
;
;          M       M       AA      IIIII    N     N
;          MM     MM      A  A       I      NN    N
;          M M   M M     A    A      I      N N   N
;          M  M M  M    AAAAAAAA     I      N  N  N
;          M   M   M    A      A     I      N   N N
;          M       M    A      A     I      N    NN
;          M       M    A      A   IIIII    N     N
;
;###################################################################################################################
;###################################################################################################################
;-------------------------------------------------------------------------------------------------------------------

MAIN:      
           rjmp MAIN

;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------
; subroutines:

;-----------------------------------
EEP_read:  cli                 ; issues read data at R16
           sbic $1C,1          ; poll EEPROM busy bit
           rjmp EEP_read       ; wait when busy
           out EEARH,ZH        ; use EEPROM high address
           out EEARL,ZL        ; use EEPROM low  address
           sbi EECR,0          ; set read mode bit 0
           in  r16,EEDR        ; get data to R16
r_busy:    sbic EECR,1         ; poll busy bit
           rjmp r_busy         ; wait when busy
           ret                 ; back
;-------------------------------------
EEP_wrt:   cli                 ; write data are prepared in R16 
           sbic $1C,1          ; poll EEPROM busy bit
           rjmp EEP_wrt        ; wait when busy
           out  EEARH,ZH       ; use EEPROM high address
           out  EEARL,ZL       ; use EEPROM low  address
           out  EEDR,r16       ; put write data
           sbi  EECR,2         ; set prepare writing bit
           sbi  EECR,1         ; set write bit
w_busy:    sbic EECR,1         ; poll busy bit
           rjmp w_busy         ; wait when busy
           ret                 ; back
;-------------------------------------------------------------------------------------------------------------------- 
;---------------------------------------------------------------------------------------------------------------------
; delay call routines - valid for AVR clock 1 MHz

dly_10uS:  wdr                 ;  10uS    including call and return
           nop
           rjmp djm1
djm1:      ret
         
dly_100uS: push r22            ; 100uS
           push r21
           push r20 
           ldi r22,$01     
           ldi r21,$01
           ldi r20,$12
           rjmp djm2
djm2:      rjmp dela

dly_1mS:   push r22            ;   1mS
           push r21
           push r20 
           ldi r22,$01     
           ldi r21,$01
           ldi r20,$F3
           rjmp djm3
djm3:      rjmp dela

dly_10mS:  push r22            ;  10mS
           push r21
           push r20 
           ldi r22,$01     
           ldi r21,$0A
           ldi r20,$B9
           rjmp dela

dly_100mS: push r22            ; 100mS
           push r21
           push r20 
           ldi r22,$01     
           ldi r21,$62
           ldi r20,$71
           rjmp dela

dly_1S:    push r22            ;    1S
           push r21
           push r20 
           ldi r22,$04     
           ldi r21,$CF
           ldi r20,$A1
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
;-----------------------------------------------------------------------------------------------------------------------
; Autolock will only run, when into boot loader area
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
;----------------------------------------------------------------------------------------------------------