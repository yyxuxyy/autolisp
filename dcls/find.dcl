

find : dialog {
   label = "�������滻";
   : column {
      : edit_box {
         label = "����:";
         key = "find";
         mnemonic = "F";
         edit_width = 30;
      }  
      : edit_box {
         label = "�滻:";
         key = "replace";
         mnemonic = "R";
         edit_width = 30;
      }  
   }   
   spacer_1 ;
   : row {
      : toggle {
         label = "���ִ�Сд";
         key = "case";
         mnemonic = "C";
         alignment = left;
      }   
      : toggle {
         label = "ȫ���滻";
         key = "global";
         mnemonic = "G";
         alignment = right;
      }   
   }   
   spacer_1 ;
   : column {
      ok_cancel;
      errtile;
   }   
}
find2 : dialog {
   label = "�������滻";
   : row {
      : button {
         label = "�滻";
         fixed_width = true;
         alignment = centered;
         key = "accept";
         mnemonic = "R";
      }  
      : spacer { width = 2; }
      : button {
         label = " �Զ�  ";
         fixed_width = true;
         alignment = centered;
         key = "auto";
         mnemonic = "A";
      }  
      : spacer { width = 2; }
      : button {
         label = " ����  ";
         fixed_width = true;
         alignment = centered;
         key = "skip";
         mnemonic = "S";
      }  
      : spacer { width = 2; }
      cancel_button;
   }   
   errtile;
}
