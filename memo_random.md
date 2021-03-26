Random and old memo
=========================

## Nomenclature ##

* W16 means "Word-16", where a word is equivalent to a byte and it starts from W0.
* Hence W16 is the 17-th word (aka byte) in a telemetry frame.
* Frame is similar; F0, F1, ... (see, for a real example, Table 5.1.14, where F0 exists.)
* SABU-Frame starts from 1, seemingly (though there is no strict definition).

## DESCRIPTION (for the variable telems(:, :)) ##

Format overview: Sec.5.5 pp.198

* Word starts from 0: W0, W1, ...
* Frame starts from 0: F0, F1, ... (because of Table 5.1.14, where F0 exists.)

ASM Data: `((W16*n+4)..(W16*n+15))`, i.e., 5th to 16th, 21th to 32nd, etc.

* cf. Table 5.1.1, pp.200 (for Basics)
*   AND, 48..51, 112..115 (when n starts from 0).

## Common Part ##

* sync code (W0-2): Always &xFAF320
* FI (W3): Frame-info(SABU-frame No., Frame No.)
* CMD (W32): CMD/AGC (whatever it means...)
* ACS data (W33-35): Attitude, STT, IRU, NSAS, etc (depending on the frame)
* ASM data (W48-51)
* AHK (W64): House-keeping (HV, temperature, ...)
* status (W65): (Slew360 Mode: F32n+10 W65 B3)
* DP (W66): (ASM Mode: F8n+4 W66 B3; (ON/OFF <=> 1/0)) (B4: PHA/Time <-> 0/1)
* PI MON (W67) / principal-instrument monitor??
* ASM data (W112-115)

### ASM in Common Part: (W48-51, 112-115, and PI-MON (W67 in F8n+1,5)) ###

mentioned at Sec.5.1 Item-6b (pp.197)

         cf. Table 5.1.14, pp.216 (for ASM-Y1/2 CAL-PH/FW[1-3]-PC )
         i.e., F16: W48( Y1 CAL-PH 0ch),  W49(Y1 FW1-PC),  W50(Y1 FW2-PC),  W51(Y1 FW3-PC)
                  : W112(Y2 CAL-PH 0ch), W113(Y2 FW1-PC), W113(Y2 FW2-PC), W114(Y2 FW3-PC)
         i.e., F17: W48( Y1 CAL-PH 1ch),  W49(Y1 FW1-PC),  W50(Y1 FW2-PC),  W51(Y1 FW3-PC)
                  : W112(Y2 CAL-PH 1ch), W113(Y2 FW1-PC), W113(Y2 FW2-PC), W114(Y2 FW3-PC)
          ===QUESTION===: FW1/2/3 has channels, too??
         cf. Table 5.1.17, pp.220 (for ASM-CAL-PH, ASM-FW-PC)
         cf. Table 5.1.17, pp.221 (for ASM-Y1/2 at W67(PI-MON) in F8n+1(Y1), F8n+5(Y2)) 

* DP Block Command (PICM): Sec.4.2 Item-4 Table 4.2.4 DV-01/Bit4
  * cf. Table 4.2.5, pp.191 for W66 for details of OS (PI block commands) (I don't understand this!)

## Mode of operation ##

* Table 5.1.11, pp.207 => F8n+4 W66(=DP) describes the Mode
* Table 5.1.12, pp.214 "MODE" => B3(ASM ON/OFF), B4(ASM TIME/PHA)
  * NOTE: ON/OFF <=> 1/0, TIME/PHA <=> 1/0
* Also, F56 W66(=DP) (="DP OS1")  B4: TIME/PHA <=> 1/0  (redundant?)

## Misc ##

* ASM-Mode Byte-bit: Table 5.5.7 (pp.235), describing how a byte consists of bits (the most significant bit is the largest digit.)
* ASM PHA Mode Data: (0..15ch) F8n+4 W66 Bit3,4=="10"
  *  cf. Table 5.5.5, pp.233
* ASM Time Mode Data: (0..15ch) F8n+4 W66 Bit3,4=="11"
  *  cf. Table 5.5.6, pp.234
* Frame: W3 (4th-word) means SABU_frame(2bits) and Frame(6bits) (0..255)
  * cf. Table 5.1.3, pp.201
* ASM status: F32n+15 (W65 (66th word), DP) Flags of ASM ON/OFF etc
  * cf. Table 5.1.10, pp.207 (frame numbers) => F32n+15
  * cf. Table 5.1.12, pp.213
* ASM DP: F32n+14 (W66 (67th word), DP) Flags of ASM ON/OFF etc
* ASM OS (what is OS?): F16n+15 (W66 (67th word), DP) Flags of ASM OS
  * cf. Table 5.1.11, pp.207 (frame numbers) => F16n+15
  * cf. Table 5.1.12, pp.214
* PI monitor (W67): Table 5.1.16, pp.218 (for ASM Y1/2)
* ASM Mode: Sec.6.7, pp.246
  * sends DV-1. and sets OS-4 (=5th bit), depending on PHA/Time modes
  * cf, also Sec.8.3, pp.259

## Telemetry header (16 bytes=words) ##

* Byte00=month
* Byte01=day
* Byte02=hour
* Byte03=minute
* Byte04=second
* Byte05+06=millisecond
  *  `millisec = byte05 * 256 + byte06` (`readfits_SF_WD.c` による)
* Byte07=Counter_A1
* Byte08=Counter_A2
* Byte09=Counter_B1
* Byte10=Counter_B2
* Byte11=real(1) or stored(2)
* Byte12=bit-rate-low(0) or high(1)
* Byte13, 14, 15: not used

------------------

## Conditions ##
  
* ASM mode must be switched on only once.
* Slew mode must be on while ASM mode. 

