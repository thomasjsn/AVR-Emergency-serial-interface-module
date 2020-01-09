'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_EMG_STOP_INTERFACE_v.1.0
'  date: 27/10/2007
'  prot: 1.2
'--------------------------------------------------------------
$regfile = "m8def.dat"
$crystal = 8000000
$baud = 9600
Config Portb = Output
Config Portd.5 = Output
Config Portd.6 = Output
Config Portd.7 = Output
Config Portc.1 = Input
Config Portc.2 = Input
Config Portc.3 = Input

'serial
'PD0: Rx
'PD1: Tx

Dim W1 As Word , Lifesignal As Integer
Dim Life As Integer , Send As String * 20
Dim Inn(4) As Integer , A As Byte , A2 As String * 1
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 15 , Input_nr As String * 3
Dim Input_com As String * 1 , Input_ut As String * 2
Dim Input_stat As String * 1 , Led As Byte , Ut(5) As Byte
Dim Ut_t(2) As Word , B As Byte

Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc

Const Id = "004"
Life = 1000

For A = 1 To 10
Portb.3 = 1
Portb.4 = 1
Portb.5 = 0
Portd.6 = 0
Waitms 250
Portb.3 = 0
Portb.4 = 0
Portb.5 = 1
Portd.6 = 1
Waitms 250
Next A
Portb = 0
Portd.6 = 0
Portd.7 = 0
Portd.5 = 0

Top:
Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then
   Serialchar = Inkey()
      Goto Myroutine
   End If

Goto Main

Myroutine:
Select Case Serialchar
Case 42                                                     '*
Goto Set_value
End Select

Main:
'input send off signal
For A = 2 To 4
   If Inn(a) = 1 Then
      Led = 103
      A2 = Str(a)
      If Len(a2) < 2 Then A2 = "0" + A2
      Send = Id + ":i:" + A2 + ":0:"
      Print Send ; Checksum(send)
      End If
Next A

'input send on signal
If Inn(1) = 1 Then                                          'input 1
   W1 = Getadc(0)
   If W1 > 999 Then W1 = 999
   Led = 103
   Send = Id + ":i:01:" + Str(w1) + ":"
   Print Send ; Checksum(send)
   Inn(1) = 0
End If

If Pinc.1 = 0 Then                                          'input 2
   If Inn(2) = 0 Then
   Led = 103
   Send = Id + ":i:02:1:"
   Print Send ; Checksum(send)
   End If
   Inn(2) = 2500
End If

If Pinc.2 = 0 Then                                          'input 3
   If Inn(3) = 0 Then
   Led = 103
   Send = Id + ":i:03:1:"
   Print Send ; Checksum(send)
   End If
   Inn(3) = 250
End If

If Pinc.3 = 0 Then                                          'input 4
   If Inn(4) = 0 Then
   Led = 103
   Send = Id + ":i:04:1:"
   Print Send ; Checksum(send)
   End If
   Inn(4) = 250
End If

'set input counters
For A = 2 To 4
   If Inn(a) > 0 Then Decr Inn(a)
Next A

'set output counters
For B = 1 To 2
   If Ut_t(b) > 0 Then Decr Ut_t(b)
Next B

'handle outputs
If Ut(1) = 1 And Ut_t(1) = 0 Then Ut_t(1) = 1000
If Ut(2) = 1 And Ut_t(2) = 0 Then Ut_t(2) = 10000

If Ut_t(1) = 1000 Then Portb.0 = 1
If Ut_t(1) = 1 Then
   Portb.0 = 0
   Ut(1) = 0
   Send = Id + ":o:01:" + Str(ut(1)) + ":"
   Print Send ; Checksum(send)
   End If

If Ut_t(2) = 10000 Then Portb.1 = 1
If Ut_t(2) = 1 Then
   Portb.1 = 0
   Ut(2) = 0
   Send = Id + ":o:02:" + Str(ut(2)) + ":"
   Print Send ; Checksum(send)
   End If

'led timer
If Led > 0 Then Decr Led
If Led = 100 Then Portd.7 = 1
If Led = 0 Then Portd.7 = 0

'lifestring
If Life > 0 Then Decr Life
If Life = 0 Then
   Led = 103
   Send = Id + ":s:01:1:"
   Print Send ; Checksum(send)
   Life = 20000
   End If

'lifesignal
If Lifesignal > 0 Then Decr Lifesignal
If Lifesignal = 500 Then Portd.5 = 1
If Lifesignal = 0 Then
   Portd.5 = 0
   Lifesignal = 2100
   End If

Waitms 1
Goto Top
End

Set_value:
Input Comminput Noecho                                      'read serialport

Input_nr = Left(comminput , 3)                              'id check
Input_com = Mid(comminput , 5 , 1)                          'command check
Input_ut = Mid(comminput , 7 , 2)                           'output nr check
Input_stat = Mid(comminput , 10 , 1)                        'output stat check

'output
If Input_nr = Id Then

If Input_com = "o" Then
Led = 103
Select Case Input_ut

Case "01"                                                   'output 1
If Input_stat = "1" Then Ut(1) = 1
If Input_stat = "0" Then Ut(1) = 0
Send = Id + ":o:01:" + Str(ut(1)) + ":"
Print Send ; Checksum(send)

Case "02"                                                   'output 2
If Input_stat = "1" Then Ut(2) = 1
If Input_stat = "0" Then Ut(2) = 0
Send = Id + ":o:02:" + Str(ut(2)) + ":"
Print Send ; Checksum(send)

Case "03"                                                   'output 3
If Input_stat = "1" Then Portb.2 = 1
If Input_stat = "0" Then Portb.2 = 0
Send = Id + ":o:03:" + Str(portb.2) + ":"
Print Send ; Checksum(send)

Case "04"                                                   'output 4
If Input_stat = "2" Then
   Portb.4 = 1
   Portb.3 = 0
   Ut(4) = 2
   End If
If Input_stat = "1" Then
   Portb.4 = 0
   Portb.3 = 1
   Ut(4) = 1
   End If
If Input_stat = "0" Then
   Portb.4 = 0
   Portb.3 = 0
   Ut(4) = 0
   End If
Send = Id + ":o:04:" + Str(ut(4)) + ":"
Print Send ; Checksum(send)

Case "05"                                                   'output 5
If Input_stat = "2" Then
   Portd.6 = 1
   Portb.5 = 0
   Ut(5) = 2
   End If
If Input_stat = "1" Then
   Portd.6 = 0
   Portb.5 = 1
   Ut(5) = 1
   End If
If Input_stat = "0" Then
   Portd.6 = 0
   Portb.5 = 0
   Ut(5) = 0
   End If
Send = Id + ":o:05:" + Str(ut(5)) + ":"
Print Send ; Checksum(send)

End Select
End If

If Input_com = "i" Then
Select Case Input_ut

Case "01"
If Inn(1) = 0 Then Inn(1) = 1                               'status input 1
Case "02"
If Inn(2) = 0 Then Inn(2) = 2                               'status input 2
Case "03"
If Inn(3) = 0 Then Inn(3) = 2                               'status input 3
Case "04"
If Inn(4) = 0 Then Inn(4) = 2                               'status input 4

End Select
End If

End If
Goto Main
End