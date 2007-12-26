inherited Form10: TForm10
  Left = 630
  Top = 563
  Caption = 'Form10'
  OldCreateOrder = True
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Memo1: TMemo
    Lines.Strings = (
      
        'In this step we'#39'll do basicly the same as in step 7(Code Injecti' +
        'on) but now a little bit more difficult.'
      
        'Now you have to edit the code that decreases health with a piece' +
        ' of code that sets the health to 1000 if the current '
      'second is equal to or bigger than 30, and 2000 if it'#39's smaller'
      ''
      
        'This can be done using a auto assembler scripts that does some a' +
        'pi calls to some routines to get the current time, '
      'but it may be easier to use a C-SCRIPT injection here'
      ''
      
        'Find the address of health and go to the script engine in Cheat ' +
        'Engine (ctrl+alt+a in memory view, or tools->script '
      'engine)'
      ''
      
        'then opposed to the other tutorials I'#39'll provide you with a big ' +
        'hint (in case you'#39've never coded in C)'
      '----------------'
      '#include <time.h>'
      ''
      'struct tm *timep;'
      'time_t c;'
      'c=time(0);'
      ''
      'timep=localtime(&c);'
      ''
      'if (timep->tm_sec>=30)'
      '  *(int *)addresstochange=1000;'
      'else'
      '  *(int *)addresstochange=2000;'
      '-------------'
      
        'Here change addresstochange with the address of health. Don'#39't fo' +
        'rget to add 0x in front of it. So if the address was '
      '0012345 then fill in 0x0012345'
      ''
      
        'Select inject->Inject into current process and it'#39'll open an aut' +
        'o assembler script with a call inside it.'
      
        'Now, just like in step 7 go to the address that decreases health' +
        ' and do autoassembler->template->code injection. '
      
        'And fill in as code the call instruction you got. Note that the ' +
        'call will change the value of EAX and some flags may '
      
        'change as wel;l, so if you want to save them, push them before a' +
        'nd pop them after.  And remove the original code, '
      'it'#39's not used and only makes things '
      'harder.'
      'Click Execute and then click "Hit me" in the trainer.'
      
        'If all went right the clickign of the button caused your c-scrip' +
        't to be executed and changed the value of health '
      'according to the current time.'
      ''
      'Bonus:'
      
        'As said before it can also be done with a normal assembler scrip' +
        't. CE allows you to fill in functionnames for call '
      'instructions so that should make things easier'
      
        'And you could also just use a dll injection with an aa script. E' +
        '.G:'
      'injectdll(mydll.dll) //dll written in any languge you like'
      ''
      'codecave:'
      'call functionofmydll'
      'jmp exit')
  end
end
