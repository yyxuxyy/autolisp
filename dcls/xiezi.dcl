tuzhixz : dialog{
label="ͼֽд��  --xyp@bsedi.com";
:list_box{
label="���ôʿ⾫ѡ";
key="klist";
width=30;
height=10;
fixed_width_font=true;
}
:edit_box{label="ͼ��";key="wordstr";}
:row{
fixed_width=true;
alignment=centered;
:edit_box{label="�ָ�";key="txthh";edit_width=4;}
:popup_list{	//����ѡ��1
key="pophh";
edit_width=4;
}
:edit_box{label="�Ƕ�";key="txtang";edit_width=4;}
:popup_list{	//����ѡ��2
key="popang";
edit_width=4;
}
}
spacer_1;
ok_cancel;
}

tzxztsfh:dialog{
	label="ͼֽд�֣��������";
	:row{
		:boxed_radio_column{
			label="��������";
			:radio_button{label="һ���ֽ� HPB235";key="rb1";}
			:radio_button{label="�����ֽ� HRB335";key="rb2";}
			:radio_button{label="�����ֽ� HRB400";key="rb3";}
			:radio_button{label="�ļ��ֽ�";key="rb4";}
			:radio_button{label="5�Ÿֽ�";key="rb5";}
			:radio_button{label="RRB400";key="rb6";}
			:radio_button{label="�±�";key="rb7";}
			:radio_button{label="�ϱ�";key="rb8";}
			:radio_button{label="ƽ��";key="rb9";}
			:radio_button{label="����";key="rb10";}
			:radio_button{label="��ǰһ���ַ���СȦ";key="rb11";}
			:radio_button{label="��ǰ�����ַ���Ȧ";key="rb12";}
			:radio_button{label="��ǰһ���ַ�����Ȧ";key="rb13";}
			}
		:image {
			key = "kimage";
			color = -2;
			width = 30;
			aspect_ratio = 0.66;
			}
		}
		:row{
                        :edit_box{label="�ı�";key="ktx";}
		        :edit_box{label="�ָ�";key="khh";}
			:edit_box{label="�ֽ�";key="kan";}
			}
	spacer_1;
	ok_cancel;
}
