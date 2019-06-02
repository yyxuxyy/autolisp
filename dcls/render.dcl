// Next available MSG number is   485 
// MODULE_ID RENDER_DCL_

//     Copyright (C) 1991-1997 by Autodesk, Inc.
//
//     Permission to use, copy, modify, and distribute this software
//     for any purpose and without fee is hereby granted, provided
//     that the above copyright notice appears in all copies and
//     that both that copyright notice and the limited warranty and
//     restricted rights notice below appear in all supporting
//     documentation.
//
//     AUTODESK PROVIDES THIS PROGRAM "AS IS" AND WITH ALL FAULTS.
//     AUTODESK SPECIFICALLY DISCLAIMS ANY IMPLIED WARRANTY OF
//     MERCHANTABILITY OR FITNESS FOR A PARTICULAR USE.  AUTODESK, INC.
//     DOES NOT WARRANT THAT THE OPERATION OF THE PROGRAM WILL BE
//     UNINTERRUPTED OR ERROR FREE.
//
//     Use, duplication, or disclosure by the U.S. Government is subject to
//     restrictions set forth in FAR 52.227-19 (Commercial Computer
//     Software - Restricted Rights) and DFAR 252.227-7013(c)(1)(ii)
//     (Rights in Technical Data and Computer Software), as applicable.
//
//.

//***************************************************************************
//
// Render Dialogue Control Language (DCL)
//
//***************************************************************************

// Change level to 3 for new DCL auditing.
dcl_settings : default_dcl_settings { }

@include "rendcomm.dcl"

//***************************************************************************
//

// Sub-assemblies common to the render and preferences dialog

render_quality : popup_list {
    label = "��Ⱦ����(R):";
    key = "pf_st";
    mnemonic = "R";
    list = "һ����Ⱦ\n��Ƭ����ʵ����Ⱦ\n��Ƭ�����߸�����Ⱦ";
    render_types = "crender\nautovis\nraytrace";
    edit_width = 20;
    fixed_width = true;
}

render_query : toggle {
    key = "pf_rp";
    label = "��ѯѡ��(Q)";
    mnemonic = "Q";
}

render_crop : toggle {
    key = "pf_cropwin";
    label = "�޼�����(W)";
    mnemonic = "W";
}

render_procedure : column {
    : boxed_column {
        label = "��Ⱦ����";
        render_query;
        render_crop;
        : toggle {
            key =  "pf_sd";
            //  FIXME
            label = "������Ⱦ�Ի���(K)";
            mnemonic = "K";
        }
    }
    : edit_box_4 {
        label = "��Դͼ�����(L):";
        key = "pf_ic_tx";
        mnemonic = "L";
    }
    : edit_box_4 {
        label = "ƽ���Ƕ�(G):";
        key = "pf_sa";
        mnemonic = "G";
    }
}

render_options : boxed_column {
    label = "��Ⱦѡ��";
    : toggle {
        key = "pf_ss";
        label = "ƽ����ɫ(M)";
        mnemonic = "M";
    }
    : toggle {
        key = "pf_af";
        label = "Ӧ�ò���(A)";
        mnemonic = "A";
    }
    : toggle {
        key = "pf_sh";
        label = "��Ӱ(D)";
        mnemonic = "D";
    }
    : toggle {
        key = "pf_ca";
        label = "��Ⱦ���ٻ���(C)";
        mnemonic = "C";
    }
    : button {
        key = "Options";
        label = "����ѡ��(O)...";
        mnemonic = "O";
    }
}

sub_sample : boxed_column {
    label = "������(U)";
    mnemonic = "U";
    : popup_list {
        key = "pf_subs";
        mnemonic = "U";
        list = "1:1����ѣ�\n2:1\n3:1\n4:1\n5:1\n6:1\n7:1\n8:1 ����죩";
        subsample_types = "1\n2\n3\n4\n5\n6\n7\n8";
        edit_width = 15;
    }
}

render_env : column {
    sub_sample;
    : button {
        key = "pf_env_back";
        label = "����(B)...";
        mnemonic = "B";
    }
    : button {
        key = "pf_env_fog";
        label = "��/�������(F)...";
        mnemonic = "F";
    }
}

render_destination : boxed_column {
    label = "Ŀ��(N)";
    mnemonic = "N";
    : popup_list {
        key = "pf_ds";
        list = "�ӿ�\n��Ⱦ����\n�ļ�";
        mnemonic = "N";

    }
    spacer;
    : column {
        : var_text {
            key = "pf_width";
            alignment = left;
            label = "   ���    :  640";
        }
        : var_text {
            key = "pf_height";
            alignment = left;
            label = "   �߶�    :  480";
        }
        : var_text {
            key = "pf_colors";
            alignment = left;
            label = "   ��ɫ      :  8λ";
        }
    }
    spacer;
    : button {
        key = "FOptions";
        label = "����ѡ��(P)...";
        mnemonic = "P";
    }
}

pref : dialog {
    label = "��Ⱦϵͳ����";
    render_quality;
    spacer_1;
    : row {
        render_scene_list;
        render_procedure;
    }
    spacer_1;
    : row {
        render_options;
        render_destination;
        render_env;
    }
    spacer_1_ok_cancel_help_errtile;
}


render : dialog {
    label = "��Ⱦ";
    render_quality;
    spacer_1;
    : row {
        render_scene_list;
        render_procedure;
    }
    spacer_1;
    : row {
        render_options;
        render_destination;
        render_env;
    }
    spacer_1;
    : row {
        fixed_width = true;
        alignment = centered;
        : button {
            key = "pf_re_scene";
            label = "��Ⱦ";
            is_default = true;
        }
        : spacer { width = 2; }
        cancel_button;
        : spacer { width = 2; }
        help_button;
    }
    errtile;
}

//**************************************************************************
// Rendering Options

rp_alias_mode : boxed_radio_column {
    label = "������";
    key = "rpaliasmode";
    fixed_height = true;
    : radio_button {
        key = "ahnv";
        label = "��С(I)";
        mnemonic = "I";
    }
    : radio_button {
        key = "ahsv";
        label = "��(&O)";
    }
    : radio_button {
        key = "ahsv3";
        label = "��(&E)";
    }
    : radio_button {
        key = "ahsv4";
        label = "��(&G)";
    }
}

rp_adaptive_toggle : toggle {
    label = "����(E)";
    key = "adaptive_enable";
    mnemonic = "E";
}

rp_ray_adaptive : boxed_column {
    key = "adapt_samp_box";
    label = "����Ӧ����";
    fixed_height = true;
    rp_adaptive_toggle;
    : edit_box_6 {
        label = "�Ա���ֵ(C): ";
        key = "contrast_threshold";
        mnemonic = "C";
    }

}


rp_raydepth_mode : boxed_column {
    label = "���������";
    fixed_height = true;
    : edit_box_4 {
        label = "������(D): ";
        key = "ray_depth";
        mnemonic = "D";
    }
    : edit_box_6 {
        label = "�ض���ֵ(T):";
        key = "ray_threshold";
        mnemonic = "T";
    }
}


rp_shadow_mode : boxed_column {
    label = "�����ͼ��Ӱ����";
    key = "rpshadowmode";
    : edit_box_8  {
        key = "minbias";
        label = "��Сƫ��(B): ";
        mnemonic = "B";
    }
    : edit_box_8  {
        key = "maxbias";
        label = "���ƫ��(X): ";
        mnemonic = "X";
    }
}

rp_texmap_mode : boxed_radio_column {
    label = "��ͼ����";
    key = "rptexmapmode";
    : radio_button  {
        key = "txpoint";
        label = "�����(P)";
        mnemonic = "P";
    }
    : radio_button  {
        key = "txlinterp";
        label = "���Բ���(L)";
        mnemonic = "L";
    }
    : radio_button  {
        key = "txmip";
        label = "Mip ��ͼ����(M)";
        mnemonic = "M";
    }
}

scanline_options : dialog {
    label = "��Ƭ����ʵ����Ⱦѡ��";
    spacer_1;
    : row {
        : column {
            fixed_height = true;
            alignment = top;
            rp_alias_mode;
        }
        spacer_1;
        : column {
            fixed_height = true;
            alignment = top;
            other_options;
            spacer;
            rp_shadow_mode;
            spacer;
            spacer;
            rp_texmap_mode;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

raytrace_options : dialog {
    label = "��Ƭ�����߸�����Ⱦѡ��";
    spacer_1;
    : row {
        : column {
            rp_alias_mode;
            rp_ray_adaptive;
            rp_raydepth_mode;
        }
        spacer_1;
        : column {
            other_options;
            spacer;
            rp_shadow_mode;
            spacer;
            spacer;
            rp_texmap_mode;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}


//**************************************************************************
// Statistics dialog box

avis_stats : dialog {
    label = "ͳ����Ϣ";
    width = 50;
    : list_box { 
        key = "stats_list";
        height = 16;
        tabs = "24";
    }
    : row {
        alignment = centered;
        fixed_width = true;
        width = 39;
        : toggle {
            label = "��ͳ����Ϣ���浽�ļ�(S): ";
            mnemonic = "S";
            key = "save_stats";
            height = 1;
        }
        : edit_box {
            label = "";
            key = "stats_name";
            edit_width = 14;
            edit_limit = 132;
        }
        : button {
            label = "�����ļ�(F)...";
            mnemonic = "F";
            key = "findfile";
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Main light dialog.

avis_light : dialog {                       // identical to ave_light, plus North Location button
    label = "��Դ";
    key = "dialog";
    dialog_kind = "main";
    : row {
        : column {
            : row {
                : list_box_8x8 {
                    key = "list";
                    label = "��Դ(L): ";
                    mnemonic = "L";
                }
                : column {
                    spacer_0;
                    button_mod;
                    button_del;
                    button_pkt;
                    spacer_0;
                }
            }
            : row {
                button_new;
                : popup_list { 
                    key = "light_type_popup";
                    edit_width = 15;
                    mnemonic = "T";
                    list = "���Դ\nƽ�й�\n�۹��";
                    light_types = "overhead\ndirect\nsh_spot";
                }
            }
            : button {
                key = "northloc";
                label = "����λ��(O)...";
                mnemonic = "O";
                fixed_width = true;
                alignment = centered;
            }
        }
        spacer;
        : column {
            children_alignment = centered;
            : boxed_column {
                label = "������";
                : edit_box_4 {
                    label = "ǿ��(I): ";
                    key = "ambient_t";
                    mnemonic = "I";
                }
                : slider_0_1 { key = "ambient_s"; }
                : boxed_column {
                    label = "��ɫ";
                    rgb_edit_slider;
                    light_color_panel;
                }
            }
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Dialog to edit the currently selected Point light definition.

avis_point_light : dialog {
    key = "dialog";
    dialog_kind = "point";
    : row {
        ave_basic_lights;
        : column {
            alignment = top;
            fixed_height = true;
            attenuation_panel;
            shadow_panel;
            spacer_0;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Dialog to edit the currently selected Spot light definition.

avis_spotlight : dialog {
    key = "dialog";
    dialog_kind = "spot";
    : row {
        ave_basic_lights;
        : column {
            alignment = top;
            fixed_height = true;
            ave_spot_lights;
            attenuation_panel;
            shadow_panel;
            spacer_0;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
// Dialog to edit distant lights (sun).

avis_distant_light : dialog {
    key = "dialog";
    dialog_kind = "distant";
    : row {
        : column {
            distant_light_parameters_panel;
            shadow_panel;
            : button {
                key = "sa_calc";
                label = "̫���Ƕȼ�����(S)...";
                mnemonic = "S";
                fixed_width = true;
                alignment = centered;
            }
        }
        : column {
            azimuth_altitude_panel;
            light_source_panel;
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

sun_angle_calculator_panel : column {
    : boxed_column {
        : row {
        : column {
                : edit_box {
                    label = "����(D):      ";
                    key = "date_edit";
                    mnemonic = "D";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : edit_box {
                    label = "ʱ��ʱ��(C):";
                    key = "time_edit";
                    mnemonic = "C";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : popup_list { 
                    key = "timezone_popup";
                    list = "PST:8\nMST:7\nCST:6\nEST:5\nNewfoundland:4\nYukon:9";
                }
                : edit_box {
                    label = "γ��(L):  ";
                    key = "latitude_edit";
                    mnemonic = "L";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : edit_box {
                    label = "����(O): ";
                    key = "longitude_edit";
                    mnemonic = "O";
                    edit_width = 6;
                    edit_limit = 6;
                }
        }
        : column {
                : slider {
                    key = "date_slider";
                    min_value = 1;
                    max_value = 365;
                    small_increment = 1;
                    big_increment = 31;
                    width = 10;
                }
                : slider {
                    key = "time_slider";
                    min_value = 0;
                    max_value = 1440;
                    small_increment = 10;
                    big_increment = 60;
                    width = 10;
                }
                : toggle {
                    label = "����ʱ(S)";
                    key = "daylight_toggle";
                    mnemonic = "S";
                }
                : slider {
                    key = "latitude_slider";
                    min_value = 0;
                    max_value = 90;
                    small_increment = 1;
                    big_increment = 10;
                    width = 10;
                }
                : slider {
                    key = "longitude_slider";
                    min_value = 0;
                    max_value = 180;
                    small_increment = 1;
                    big_increment = 10;
                    width = 10;
                }
        }
        }
        : row {
            : popup_list { 
                key = "latitude_popup";
                list = "��\n��";
            }
            : popup_list { 
                key = "longitude_popup";
                list = "��\n��";
            }
        }
        : button {
            alignment = centered;
            fixed_width = true;
            key = "location_button";
            label = "����λ��(G)...";
            mnemonic = "G";
        }
    }
}
        

azimuth_altitude_panel_write_only : boxed_column {
    : row {
        : column {
            : row {
                : text {
                    label = "��λ��:";
                }
                : text {
                    label = "      ";
                    key = "azimuth_t";
                    width = 6;
                }
            }
            : image_button {
                key = "azimuth_image";
                color = dialog_background;
                height = 7;
                aspect_ratio = 1;
                fixed_height = true;
                fixed_width = true;
                alignment = centered;
                is_enabled = false;
            }
        }
        : column {
            : row {
                : text {
                    label = "����:";
                }
                : text {
                    label = "      ";
                    key = "altitude_t";
                    width = 6;
                }
            }
            : image_button {
                key = "altitude_image";
                color = dialog_background;
                height = 7;
                aspect_ratio = 1;
                fixed_height = true;
                fixed_width = true;
                alignment = centered;
                is_enabled = false;
            }
        }
    }
    : row {
        : text {
            label = "̫��ʱ��:";
        }
        : text {
            label = "     ";
            key = "solar_time";
        }
    }
}

ave_sun_angle_calc : dialog {
    label = "̫���Ƕȼ�����";
    initial_focus = "accept";
    : row {
        sun_angle_calculator_panel;
        azimuth_altitude_panel_write_only;
    }
    spacer_1_ok_cancel_help_errtile;
}

shadow_panel : boxed_column {
    label = "��Ӱ:";
    : toggle {
        alignment = left;
        label = "��Ӱ��(W)";
        key = "shadow_on";
        mnemonic = "W";
    }
    : button {
        key = "shadow_dialog";
        label = "��Ӱѡ��(P)...";
        mnemonic = "P";
        fixed_width = true;
    }
}


//***************************************************************************
// Tile used only by Spotlights.

ave_spot_lights : column {
    fixed_height = true;
    : edit_box_8 {
        label = "�۹��(T):";
        key = "conea_t";
        mnemonic = "T";
    }
    : slider {
        key = "conea_s";
        min_value = 0;
        max_value = 160;
        small_increment = 1;
        big_increment = 10;
    }
    : edit_box_8 {
        label = "�����(F):";
        key = "coned_t";
        mnemonic = "F";
    }
    : slider {
        key = "coned_s";
        min_value = 0;
        max_value = 160;
        small_increment = 1;
        big_increment = 10;
    }
}

//***************************************************************************
// geographic location dialog

ave_geographic_location : dialog {
    key = "dialog";
    initial_focus = "accept";
    label = "����λ��";
    : column { 
        key = "top_key";
        : row {
            : column {
                fixed_width = true;
                : list_box {
                    key = "cities";
                    height = 8;
                    width = 21;
                    label = "����(C):";
                    mnemonic = "C";
                }
                : boxed_column {
                    : text {
                        label = " ";
                        key = "city";
                        width = 21;
                    }
                    : edit_box_8 {
                        label = "γ��(A): ";
                        value = "0.";
                        edit_width = 8;
                        key = "latitude";
                        mnemonic = "A";
                    }
                    : edit_box_8 {
                        label = "����(O): ";
                        value = "0.";
                        edit_width = 8;
                        key = "longitude";
                        mnemonic = "O";
                    }
                    spacer_0;
                }
            }
            : boxed_column {
                : row {
                    : popup_list { 
                        key = "map_name";
                        list = 
"����\n���ô�\nŷ��\n����\n����\n�Ĵ�����\n���޴δ�½\n����";
                        files = 
"namer.map\ncanada.map\neurope.map\nsamer.map\nasia.map\naust.map\nindia.map\nafrica.map";
                        width = 18;
                    }
                    : toggle {
                        label = "����Ĵ����(N)";
                        value = "1";
                        key = "nearest";
                        mnemonic = "N";
                    }
                }
                : image_button {
                    key = "map_image";
                    color = -15;             /* background color */
                    height = 15;
                    aspect_ratio = 1.5;
                }
            }
        }
//      spacer_1_ok_cancel_help_errtile;
        ok_cancel_help_errtile;
    }
}

northLocator : dialog {
    label = "����λ��";
    spacer;
    spacer;
    : row {
        : boxed_column {
            label = "X/Y ƽ��:";
            fixed_width = true;
            : image_button {
                key = "northImage";
                color = dialog_background;             /* background color */
                width = 20;
                aspect_ratio = 1;
//              is_enabled = true;
                fixed_width = true;
            }
            : column {
                : edit_box {
                    label = "�Ƕ�(A):";
                    key = "northEdit";
                    mnemonic = "A";
                    edit_width = 6;
                    edit_limit = 6;
                }
                : slider {
                    key = "northSlider";
                    min_value = 0;
                    max_value = 360;
                    small_increment = 1;
                    big_increment = 10;
                    width = 20;
                }
            }
        }
        : list_box {
            width = 20;
            fixed_width = true;
            key = "useUCS";
            label = "ʹ�� UCS(U):";
            mnemonic = "U";
//          list = "��Ӱ��Ͷ�(O):";
        }
    }
    ok_cancel_help_errtile;
}

shadowMapOptions : dialog {
    label = "��Ӱѡ��";
    spacer;
    spacer;
    : toggle {
        label = "��Ӱ��С/���߸��ٵ���Ӱ(S)";
        key = "shadowVolumes";
        mnemonic = "S";
    }
    : popup_list { 
        key = "shadowMapSize";
        fixed_width = true;
        label = "��Ӱ��ͼ�ߴ�(M)";
        list = "64\n128\n256\n512\n1024\n2048\n4096";
    mnemonic = "M";
    }
    : row {
        : edit_box {
            label = "��Ӱ��Ͷ�(O):";
            edit_width = 6;
            edit_limit = 6;
            key = "beamd_t";
            mnemonic = "O";
        }
        : slider {
            key = "beamd_s";
            min_value = 1;
            max_value = 10;
            small_increment = 1;
            big_increment = 2;
            width = 20;
        }
    }
    : button {
        key = "pick";
        label = "��ӰԼ������(B) <";
        alignment = centered;
        fixed_width = true;
        width = 3;
        mnemonic = "B";
    }
    spacer;
    spacer;
    ok_cancel_help_errtile;
}

ave_make_map : dialog {
    key = "dialog";
    label = "������ͼ";
    : row {
        : column {
            : edit_box_8 {
                label = "����(X): ";
                value = "-99.";
                key = "longitude";
                mnemonic = "X";
            }
            : edit_box_8 {
                label = "γ��(Y): ";
                value = "38.";
                key = "latitude";
                mnemonic = "Y";
            }
            : edit_box_8 {
                label = "��ȣ��ȣ�(S): ";
                value = "70.";
                key = "width";
                mnemonic = "S";
            }
            : edit_box_8 {
                label = "����(R): ";
                value = "1";
                key = "rank";
                mnemonic = "R";
            }
            : edit_box_8 {
                label = "�ļ�(F): ";
                value = "bdy.cbd";
                key = "file";
                mnemonic = "F";
            }
            : edit_box {
                label = "·��(T): ";
                value = "/files1/map/namer.map/";
//              value = "e:\\map\\namer.map\\";
                edit_width = 16;
                edit_limit = 30;
                key = "path";
                mnemonic = "T";
            }
            : edit_box {
                label = "���(O): ";
                value = "namer.map";
                edit_width = 12;
                edit_limit = 12;
                key = "save";
                mnemonic = "O";
            }
            : row {
                : button {
                    key = "plot_button";
                    label = "��ӡ(P)";
                    mnemonic = "P";
                    fixed_width = true;
                }
                : button {
                    key = "header_button";
                    label = "��ͷ(H)";
                    mnemonic = "H";
                    fixed_width = true;
                }
                : button {
                    key = "save_button";
                    label = "����(S)";
                    mnemonic = "S";
                    fixed_width = true;
                }
                : button {
                    key = "erase_button";
                    label = "ɾ��(E)";
                    mnemonic = "E";
                    fixed_width = true;
                }
            }
        }
        : boxed_column {
            : image_button {
                key = "map_image";
                color = -15;             /* background color */
                height = 13;
                aspect_ratio = 1.5;
            }
        }
    }
    spacer_1_ok_cancel_help_errtile;
}

//***************************************************************************
//Materials list dialog

avis_list : dialog {
    label = "���ʿ�";
    : column { 
        key = "top_key";
        spacer_1;
        : row {
            material_column;
            : column {
                avis_mat_preview;
                spacer_0;
                import_export_delete;
                spacer_0;
            }
            library_column;
        }
        spacer_0_5;
        ok_cancel_help_custom;
    }
}


//***************************************************************************
// Main material dialog


ave_material : dialog {
  label = "����";
  : column { 
    key = "top_key";
    : row {
        material_list;
        avis_material_preview_and_select;
        : column {
            width = 18;
            spacer_0;
            button_mod;
            button_dup;
            : boxed_column {
                button_new;
                : popup_list { 
                    key = "template";
                    list = "��׼\n����ʯ\n����ʯ\nľ��";
                }
            }
            spacer_1;
            material_attach;
        }
    }
    spacer_1_ok_cancel_help_errtile;
  }
}


avis_material_preview_and_select : column {
    avis_mat_preview;
    spacer_0_5;
    button_imp;
    button_pkt;
    spacer_0_5;
}


//
// Dialogs to edit a minimal, standard, granite, marble, or wood material.
//

ave_standard_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        standard_attributes;
        standard_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


ave_granite_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        granite_attributes;
        template_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


ave_marble_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        marble_attributes;
        template_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


ave_wood_material_mod : dialog {
  key = "dialog";
  : column { 
    key = "top_key";
    material_name;
    : row {
        wood_attributes;
        template_value_color_bitmap_preview;
    }
    spacer_0_1_ok_cancel_help_errtile;
  }
}


standard_attributes : boxed_column {
    label = "����";
    : row {
        : radio_column {
            key = "attribute";
            alignment = top;
            : radio_button {
                label = "��ɫ/ͼ��(C)";
                key = "diffuse";
                mnemonic = "C";
            }
            : radio_button {
                label = "����(A)";
                key = "ambient";
                mnemonic = "A";
            }
            : radio_button {
                label = "����(E)";
                key = "reflection";
                mnemonic = "E";
            }
            : radio_button {
                label = "�ֲڶ�(O)";
                key = "roughness";
                mnemonic = "O";
            }
            : radio_button {
                label = "͸����(T)";
                key = "transparency";
                mnemonic = "T";
            }
            : radio_button {
                label = "����(N)";
                key = "refraction";
                mnemonic = "N";
            }
            : radio_button {
                label = "��͹��ͼ(U)";
                key = "bump";
                mnemonic = "U";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


granite_attributes : boxed_column {
    label = "����";
    : row {
        : radio_column {
            alignment = top;
            key = "attribute";
            : radio_button {
                label = "��һ����ɫ(C)";
                key = "first_color";
                mnemonic = "C";
            }
            : radio_button {
                label = "�ڶ�����ɫ(N)";
                key = "second_color";
                mnemonic = "N";
            }
            : radio_button {
                label = "��������ɫ(T)";
                key = "third_color";
                mnemonic = "T";
            }
            : radio_button {
                label = "���ĸ���ɫ(F)";
                key = "fourth_color";
                mnemonic = "F";
            }
            : radio_button {
                label = "����(E)";
                key = "reflection";
                mnemonic = "E";
            }
            : radio_button {
                label = "�ֲڶ�(O)";
                key = "roughness";
                mnemonic = "O";
            }
            : radio_button {
                label = "�����(R)";
                key = "sharpness";
                mnemonic = "R";
            }
            : radio_button {
                label = "����(S)";
                key = "scale";
                mnemonic = "S";
            }
            : radio_button {
                label = "��͹��ͼ(U)";
                key = "bump";
                mnemonic = "U";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            : avis_sample_image { key = "sample_3"; }
            : avis_sample_image { key = "sample_4"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


marble_attributes : boxed_column {
    label = "����";
    : row {
        : radio_column {
            alignment = top;
            key = "attribute";
            : radio_button {
                label = "ʯ����ɫ(C)";
                key = "stone_color";
                mnemonic = "C";
            }
            : radio_button {
                label = "������ɫ(N)";
                key = "vein_color";
                mnemonic = "N";
            }
            : radio_button {
                label = "����(E)";
                key = "reflection";
                mnemonic = "E";
            }
            : radio_button {
                label = "�ֲڶ�(O)";
                key = "roughness";
                mnemonic = "O";
            }
            : radio_button {
                label = "�Ŷ�(T)";
                key = "turbulence";
                mnemonic = "T";
            }
            : radio_button {
                label = "�����(R)";
                key = "sharpness";
                mnemonic = "R";
            }
            : radio_button {
                label = "����(S)";
                key = "scale";
                mnemonic = "S";
            }
            : radio_button {
                label = "��͹��ͼ(U)";
                key = "bump";
                mnemonic = "U";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


wood_attributes : boxed_column {
    label = "����";
    : row {
        : radio_column {
            alignment = top;
            key = "attribute";
            : radio_button {
                label = "ǳɫ(C)";
                key = "light_color";
                mnemonic = "C";
            }
            : radio_button {
                label = "��ɫ(K)";
                key = "dark_color";
                mnemonic = "K";
            }
            : radio_button {
                label = "����(E)";
                key = "reflection";
                mnemonic = "E";
            }
            : radio_button {
                label = "�ֲڶ�(O)";
                key = "roughness";
                mnemonic = "O";
            }
            : radio_button {
                label = "ǳ/��(T)";
                key = "light_dark_ratio";
                mnemonic = "T";
            }
            : radio_button {
                label = "�����ܶ�(N)";
                key = "ring_density";
                mnemonic = "N";
            }
            : radio_button {
                label = "���ֿ��(H)";
                key = "ring_width";
                mnemonic = "H";
            }
            : radio_button {
                label = "������״(A)";
                key = "ring_shape";
                mnemonic = "A";
            }
            : radio_button {
                label = "����(S)";
                key = "scale";
                mnemonic = "S";
            }
            : radio_button {
                label = "��͹��ͼ(U)";
                key = "bump";
                mnemonic = "U";
            }
        }
        : column {
            : avis_sample_image { key = "sample_0"; }
            : avis_sample_image { key = "sample_1"; }
            : avis_sample_image { key = "sample_2"; }
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
            spacer_1_25;
        }
    }
}


avis_sample_image : image {
    color = -15;
    height = 1.25;
    aspect_ratio = 1;
    fixed_height = true;
    fixed_width = true;
    is_tab_stop = false;
}


standard_value_color_bitmap_preview : column {
    : row {
        : column {
            value_edit_slider;
            standard_color_widget;
        }
        avis_mat_preview;
    }
    spacer_0_1;
    bitmap_widget;
}


template_value_color_bitmap_preview : column {
    : row {
        : column {
            value_edit_slider;
            template_color_widget;
        }
        avis_mat_preview;
    }
    spacer_0_1;
    bitmap_widget;
}


standard_color_widget : boxed_column {
    label = "��ɫ";
    : row {
        : toggle {
            label = "�� ACI(Y)";
            key = "by_aci";
            mnemonic = "Y";
        }
        lock; 
        raytrace_mirror_toggle;
    }
    avis_variable_color_model;
}


template_color_widget : boxed_column {
    label = "��ɫ";
    : row {
        : toggle {
            label = "�� ACI(Y)";
            key = "by_aci";
            mnemonic = "Y";
        }
        raytrace_mirror_toggle;
    }
    avis_variable_color_model;
}


avis_variable_color_model : row { 
    : column {
        : row {
            : column {
                : text {
                    key = "top_text";
                    width = 10;
                    value = "��:";
                }
                : text {
                    key = "middle_text";
                    width = 10;
                    value = "��:";
                }
                : text {
                    key = "bottom_text";
                    width = 10;
                    value = "��:";
                }
            }
            : column {
                : edit_box_4 {
                    label = "";
                    key = "top_edit";
                } 
                : edit_box_4 {
                    label = "";
                    key = "middle_edit";
                } 
                : edit_box_4 {
                    label = "";
                    key = "bottom_edit";
                }
            }
            : column {
                : slider_0_1_fixed { key = "top_slider"; }
                : slider_0_1_fixed { key = "middle_slider"; }
                : slider_0_1_fixed { key = "bottom_slider"; }
            }
        }
        : row {
            color_system;
            : image_button {
                key = "set_color_image";
                color = -15;             /* background color */
                height = 1;
                aspect_ratio = 1;
            }
        }
    }
}


raytrace_mirror_toggle : toggle {
    label = "����(O)";
    key = "raytrace_mirror";
    mnemonic = "O";
}


bitmap_widget : row {
    : column {
        : row {
            : edit_box_4 {
                label = "λͼ�ϳ�(D):";
                key = "blend_edit";
                mnemonic = "D";
                fixed_width = true;
            } 
            : slider_0_1 { 
                key = "blend_slider"; 
                width = 16;
            }
        }
        : edit_box {
            label = "�ļ���(L):";
            key = "bitmap_edit";
            mnemonic = "L";
        }
    }
    : column {
        fixed_width = true;
        : button {
            label = "����λͼ(J)...";
            key = "adjust_map";
            mnemonic = "J";
            fixed_width = true;
        }
        : button {
            label = "�����ļ�(I)...";
            key = "find_bitmap";
            mnemonic = "I";
        }
    }
}



//
// Material attach by ACI dialogue.
//

avis_material_aci : dialog {
  label = "���� AutoCAD ��ɫ��������";
  : column {
    key = "top_key";
    : row {
        : list_box {
            key = "list";
            label = "ѡ�����(M):";
            mnemonic = "M";
            width = 18;
        }
        : column {
            avis_mat_preview;
            : button {
                label = "����(T) ->";
                key = "attach";
                mnemonic = "T";
            }
            : button {
                label = "����(D)";
                key = "detach";
                mnemonic = "D";
            }
        }
        : list_box {
            key = "index";
            label = "ѡ�� ACI(A):";
            mnemonic = "A";
            width = 31;
            tabs = "4 12";
            multiple_select = true;
        }
    }
    spacer_1_ok_cancel_help_errtile;
  }
}



//
// Material attach by layer dialogue.
//

avis_material_layer : dialog {
  label = "����ͼ�㸽��";
  : column {
    key = "top_key";
    : row {
        : list_box {
            key = "list";
            label = "ѡ�����(M):";
            mnemonic = "M";
            width = 18;
        }
        : column {
            avis_mat_preview;
            : button {
                label = "����(T) ->";
                key = "attach";
                mnemonic = "T";
            }
            : button {
                label = "����(D)";
                key = "detach";
                mnemonic = "D";
            }
        }
        : list_box {
            key = "layer";
            label = "ѡ��ͼ��(L):";
            mnemonic = "L";
            width = 38;
            tabs = "22";
            multiple_select = true;
        }
    }
    spacer_1_ok_cancel_help_errtile;
  }
}



avis_mat_preview : boxed_column {
    children_alignment = centered;
    : image_button {
        key = "image";
        color = graphics_background;
        height = 8;
        aspect_ratio = 1;
        fixed_height = true;
        fixed_width = true;
        is_tab_stop = false;
    }
    : button {
        key = "object";
        label = "Ԥ��(P)";
        mnemonic = "P";
    }
    : popup_list {
        key = "geometry";
//        list = "����λͼ(B)...";
        list = "����\n������";
    }
    spacer_0;
}

avis_mat_preview_LBitmap : boxed_column {
    children_alignment = centered;
    : image_button {
        key = "image";
        color = graphics_background;
        height = 8;
        aspect_ratio = 1;
        fixed_height = true;
        fixed_width = true;
        is_tab_stop = false;
    }
    : button {
        key = "object";
        label = "Ԥ��(P)";
        mnemonic = "P";
    }
    : edit_box {
            label = "�����С:";
            key   = "fixedScalingPreviewSize";
            value = "1";
            edit_limit = 8;
    }
    : popup_list {
        key = "geometry";
        list = "����\n������";
    }
    spacer_0;
}

//***************************************************************************
//
// Morderd UV mapper dialog
//
//***************************************************************************
//***************************************************************************
//
//***************************************************************************
OffsetArea : boxed_column {
    label = "ƫ������ת";
    : row {
        : edit_box {
            label = "X ƫ��(&O):";
            key   = "Xoff";
            value = "0";
            edit_limit = 8;
        }
        : edit_box {
            label = "Y ƫ��(&O):";
            key   = "Yoff";
            value = "0";
            edit_limit = 8;
        }
    }
    : row {
        : edit_box {
            label = "��ת(&R):  ";
            key   = "Rot";
            value = "0";
            edit_limit = 5;
        }
        : slider {
            key             = "RotSl";
            min_value       = -180;
            max_value       = 180;
            small_increment = 1;
            big_increment   = 10;
            value = "0";
            is_tab_stop = false;
        }
    }
}


//***************************************************************************
//
//***************************************************************************
PickAdjustGroup : row {
    : button {
        label = "����λͼ(B)...";
        mnemonic = "B";
        key   = "AdjMap";
    }
    spacer_1;
    : button {
        label = "ʰȡ��(I) <";
        mnemonic = "I";
        key   = "PickPts";
    }
}

//***************************************************************************
//
//
//***************************************************************************
uv : dialog {
  label = "��ͼ";
  : column { 
    key = "top_key";
    : row {
        : column {
            : boxed_radio_column {
                label = "ͶӰ";
                key   = "MapTypes";
                : radio_button {
                    label = "ƽ��(L)";
                    mnemonic = "L";
                    key   = "Pl";
                }
                : radio_button {
                    label = "������(C)";
                    mnemonic = "C";
                    key   = "Cyl";
                }
                : radio_button {
                    label = "������(S)";
                    mnemonic = "S";
                    key   = "Sph";
                }
                : radio_button {
                    label = "ʵ��(O)";
                    mnemonic = "O";
                    key   = "Sol";
                }
            }

            spacer_1;

            : button {
                label = "��������(A)...";
                mnemonic = "A";
                key   = "AdjCoor";
            }

            spacer_1;

            : button {
                label = "ȡ��(F) <";
                mnemonic = "F";
                key   = "Acquire";
            }
            : button {
                label = "���Ƶ�(T) <";
                mnemonic = "T";
                key   = "Copy";
            }
        }

        : spacer { width = 5; }

        finish_preview;
    }
    spacer_1_ok_cancel_help_errtile;
  }
}





//***************************************************************************
//
//***************************************************************************
AdjustPlanar : dialog {
  label = "����ƽ������";
  : column { 
    key = "top_key";
    : column {
        : row {
            children_alignment = top;
            : boxed_radio_column {
                label = "ƽ����";
                key   = "Orient";
                : radio_button {
                    label = "WCS XY ƽ��(X)";
                    mnemonic = "X";
                    key   = "WCS_Z";
                }
                : radio_button {
                    label = "WCS XZ ƽ��(Z)";
                    mnemonic = "Z";
                    key   = "WCS_Y";
                }
                : radio_button {
                    label = "WCS YZ ƽ��(Y)";
                    mnemonic = "Y";
                    key   = "WCS_X";
                }
                : radio_button {
                    label = "ʰȡ��ƽ��(A)";
                    mnemonic = "A";
                    key   = "UCS_Z";
                }
            }
            spacer_1;
            : column {
                : boxed_column {
                    label = "���ĵ�λ��";
                    fixed_height = true;
                    : row {
                        fixed_width = true;
                        : column {
                            fixed_height = true;
                            : image_button {
                                key = "CenterImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "XSlide";
                                min_value = 0;
                                max_value = 10000;
                                is_tab_stop = false;
                            }
                        }
                        : slider {
                            layout = vertical;
                            key = "YSlide";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                }
            }

            spacer_1;

            finish_preview;
        }

        PickAdjustGroup;

        OffsetArea;
    }

    spacer_1_ok_cancel_help_errtile;
  }
}




//***************************************************************************
//
//***************************************************************************
AdjustCyl : dialog {
  label = "����������";
  : column { 
    key = "top_key";
    : column {
        : row {
            children_alignment = top;
            : boxed_radio_column {
                label = "ƽ����";
                key   = "Orient";
                : radio_button {
                    label = "WCS Z ��";
                    mnemonic = "Z";
                    key   = "WCS_Z";
                }
                : radio_button {
                    label = "WCS Y ��";
                    mnemonic = "Y";
                    key   = "WCS_Y";
                }
                : radio_button {
                    label = "WCS X ��";
                    mnemonic = "X";
                    key   = "WCS_X";
                }
                : radio_button {
                    label = "ʰȡ����(A)";
                    mnemonic = "A";
                    key   = "UCS_Z";
                }
            }
            spacer_1;
            : column {
                : boxed_column {
                    label = "������λ��";
                    fixed_height = true;
                    : row {
                        fixed_width = true;
                        : column {
                            fixed_height = true;
                            : image_button {
                                key = "CenterImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "XSlide";
                                min_value = 0;
                                max_value = 10000;
                                is_tab_stop = false;
                            }
                        }
                        : slider {
                            layout = vertical;
                            key = "YSlide";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                }
            }

            spacer_1;

            finish_preview;
        }

        PickAdjustGroup;

        OffsetArea;
    }
    spacer_1_ok_cancel_help_errtile;
  }
}




//***************************************************************************
//
//***************************************************************************
AdjustSph : dialog {
    label = "����������";
  : column { 
    key = "top_key";
    : column {
        : row {
            children_alignment = top;
            : boxed_radio_column {
                label = "ƽ����";
                key   = "Orient";
                : radio_button {
                    label = "WCS Z ��";
                    mnemonic = "Z";
                    key   = "WCS_Z";
                }
                : radio_button {
                    label = "WCS Y ��";
                    mnemonic = "Y";
                    key   = "WCS_Y";
                }
                : radio_button {
                    label = "WCS X ��";
                    mnemonic = "X";
                    key   = "WCS_X";
                }
                : radio_button {
                    label = "ʰȡ����(A)";
                    mnemonic = "A";
                    key   = "UCS_Z";
                }
            }
            spacer_1;
            : column {
                : boxed_column {
                    label = "����λ��";
                    fixed_height = true;
                    : row {
                        fixed_width = true;
                        : column {
                            fixed_height = true;
                            : image_button {
                                key = "CenterImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "XSlide";
                                min_value = 0;
                                max_value = 10000;
                                is_tab_stop = false;
                            }
                        }
                        : slider {
                            layout = vertical;
                            key = "YSlide";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                }
            }

            spacer_1;

            finish_preview;
        }

        PickAdjustGroup;

        OffsetArea;
    }
    spacer_1_ok_cancel_help_errtile;
  }
}




//***************************************************************************
//
//***************************************************************************

BitMapAdjustE : column {
    : row {
        bitmap_image;
        finish_preview;
    } /* end of row */
    offset_scale;
} /* end column */

BitMapAdjustL : column {
    : row {
        bitmap_image;
        avis_mat_preview_LBitmap;
    } /* end of row */
    offset_scale;
} /* end column */

bitmap_image : boxed_column {
    label = "ƫ��";
    fixed_height = true;
    : row {
        children_alignment = top;
        fixed_width = true;
        : boxed_column {
            fixed_height = true;
            label = "����";
            : slider {
        key = "X_SIZE";
        min_value = 0;
        max_value = 102;
        is_tab_stop = false;
            }
            : row {
        fixed_width = true;
        : slider {
            layout = vertical;
            key = "Y_SIZE";
            fixed_width = true;
            min_value = 0;
            max_value = 102;
            is_tab_stop = false;
        }
        : image {
            key   = "BitImage";
            color = graphics_background;
            height = 8;
            aspect_ratio = 1;
            fixed_width = true;
            fixed_height = true;
            is_tab_stop = false;
        }
            } /* end row */
        }
        : column {
            : slider {
        layout = vertical;
        key = "Y_OFFSET";
        min_value = 0;
        max_value = 10000;
        is_tab_stop = false;
            }
        }
    } /* end of row */
    : row {
        : slider {
            key = "X_OFFSET";
            min_value = 0;
            max_value = 10000;
            is_tab_stop = false;
        }
    } /* end of row */
} /* end column */


offset_scale : column {
    : row {
        : column {
            : text {
                label = "ƫ��:";
            }
            : text {
                label = "����:";
            }
        }
        : column {
            : edit_box {
                key   = "OU";
                edit_limit = 8;
            }
            : edit_box {
                key   = "SU";
                edit_limit = 8;
            }
        }
        : column {
            : text {
                label = "U";
            }
            : text {
                label = "U";
            }
        }
        : column {
            : edit_box {
                key   = "OV";
                edit_limit = 8;
            }
            : edit_box {
                key   = "SV";
                edit_limit = 8;
            }
        }
            : column {
            : text {
                label = "V";
            }
            : text {
                label = "V";
            }
        }
    } /* end row */

    : toggle {
        key   = "Aspect";
        label = "���ֿ�߱�(&M)";
    }
} /* end column */


//***************************************************************************
//
//***************************************************************************
AdjustEBitmap : dialog {
  label = "��������λͼλ��";
  : column { 
    key = "top_key";

    : row {
        BitMapAdjustE;

        : column {
            : boxed_radio_column {
                key   = "Tiling";
                label = "���з�ʽ";
                spacer_1;
                spacer_1;
                : radio_button {
                    key   = "T_DEF";
                    label = "Ĭ��(&D)";
                }
                : radio_button {
                    key   = "T_TILE";
                    label = "ƽ��(&T)";
                }
                : radio_button {
                    key   = "T_CROP";
                    label = "�޼�(&C)";
                }
                spacer_1;
                spacer_1;
            }
            spacer;
        }
    }
    ok_cancel_help_errtile;
  }
}

//***************************************************************************
//
//***************************************************************************
AdjustLBitmap : dialog {
  label = "��������λͼλ��";
  : column { 
    key = "top_key";

    : row {
        BitMapAdjustL;

        : column {
            : boxed_radio_column {
                spacer_1;
                spacer_1;
                key   = "Tiling";
                label = "���з�ʽ";
                : radio_button {
                    key   = "T_TILE";
                    label = "ƽ��(&T)";
                }
                : radio_button {
                    key   = "T_CROP";
                    label = "�޼�(&C)";
                }
                spacer_1;
                spacer_1;
            }
            : boxed_radio_column {
                key   = "MaterialMapping";
                label = "��ͼ��ʽ";
                spacer_1;
                : radio_button {
                    key   = "FixedScaling";
                    label = "�̶�����(&F)";
                }
                : radio_button {
                    key   = "FitToEntity";
                    label = "����������(&O)";
                }
                spacer_1;
            }
            : toggle {
                key = "AutoAxis";
                label = "ʹ���Զ���(&A)";
            }
            spacer_1;
            spacer_1;
        }
    }
    ok_cancel_help_errtile;
  }
}

//***************************************************************************
//
//***************************************************************************
AdjustRST : dialog {
  label = "���� UVW ����";
  : column { 
    key = "top_key";
    : row {
        : column {
            : row {
                : edit_box {
                    label = "U ����(&U):";
                    key   = "RS";
                    value = "1.0";
                    edit_limit = 10;
                }
                : slider {
                    key = "R_SLIDE";
                    min_value = 0;
                    max_value = 100;
                    width = 15;
                    is_tab_stop = false;
                }
            }
            : row {
                : edit_box {
                    label = "V ����(&V):";
                    key   = "SS";
                    value = "1.0";
                    edit_limit = 10;
                }
                : slider {
                    key = "S_SLIDE";
                    min_value = 0;
                    max_value = 100;
                    width = 15;
                    is_tab_stop = false;
                }
            }
            : row {
                : edit_box {
                    label = "W ����(&W):";
                    key   = "TS";
                    value = "1.0";
                    edit_limit = 10;
                }
                : slider {
                    key = "T_SLIDE";
                    min_value = 0;
                    max_value = 100;
                    width = 15;
                    is_tab_stop = false;
                }
            }
            : row {
                : button {
                    label = "ʰȡ��(I) <";
                    mnemonic = "I";
                    key   = "Pick4Pts";
                }
                : toggle {
                    key   = "ARatio";
                    label = "���ֿ�߱�(&M)";
                }
            }
        }

        finish_preview;

    }
 
    spacer_1_ok_cancel_help_errtile;
  }
}

//***************************************************************************
// Support for Background image dialog
//***************************************************************************
background : dialog {
    label = "����";
    : row { 
        key = "top_key";
        : column {
            : radio_row {
                key = "bg_type";
                : radio_button {
                    key = "solid";
                    label = "��ɫ(S)";
                    mnemonic = "S";
                }
                : radio_button {
                    key = "gradient";
                    label = "����ɫ(G)";
                    mnemonic = "G";
                }
                : radio_button {
                    key = "image";
                    label = "ͼ��(A)";
                    mnemonic = "A";
                }
                : radio_button {
                    key = "merge";
                    label = "�ϲ�(M)";
                    mnemonic = "M";
                }
            }
            : row {
                : boxed_column {
                    key = "top_colors";
                    label = "��ɫ";
                    : row {
                        : column {
                            : row {
                                : column {
                                    : text {
                                        key = "color1_txt";
                                        label = "��";
                                    }
                                    spacer;
                                    : text {
                                        key = "color2_txt";
                                        label = "��";
                                    }
                                    spacer;
                                    : text {
                                        key = "color3_txt";
                                        label = "��";
                                    }
                                } /* column */
                                spacer;
                                : column {
                                    alignment = top;
                                    : image_button {
                                        key = "color1";
                                        color = graphics_background;
                                        fixed_width = true;
                                        aspect_ratio = 1;
                                        width = 2;
                                    }
                                    spacer;
                                    : image_button {
                                        key = "color2";
                                        color = graphics_background;
                                        fixed_width = true;
                                        aspect_ratio = 1;
                                        width = 2;
                                    }
                                    spacer;
                                    : image_button {
                                        key = "color3";
                                        color = graphics_background;
                                        fixed_width = true;
                                        aspect_ratio = 1;
                                        width = 2;
                                    }
                                } /* column */
                            } /* row */
                        } /* column */
                        spacer;
                        : column {
                            color_system;
                            color_system_set;
                        } /* column */
                    } /* row */
                    : column {
                        : row {
                            : toggle {
                                key = "use_acad";
                                label = "AutoCAD ����(X)";
                                mnemonic = "X";
                            }
                            : button {
                               label = "ѡ����ɫ(C)";
                               key = "set_color";
                               fixed_width = true;
                               mnemonic = "C";
                            }
                        }
                    }
                } /* Color row */
                : boxed_column {
                    key = "top_preview";
                    children_alignment = centered;
                    : image_button {
                        key = "preview_img";
                        color = graphics_background;
                        width = 12;
                        height = 6;
                        fixed_height = true;
                        fixed_width = true;
                        is_tab_stop = false;
                    }
                    : button {
                        key = "bg_p_button";
                        label = "Ԥ��(P)";
                        mnemonic = "P";
                    }
                    spacer_0;
                }
            } /* row */
            : row {
                : boxed_column {
                    key = "top_image";
                    label = "ͼ��";
                    : edit_box {
                        label = "����(N):";
                        key = "bg_filename";
                        edit_width = 12;
                        mnemonic = "N";
                    }
                    : button {
                        label = "�����ļ�(F)...";
                        key = "bg_find_bitmap";
                        mnemonic = "F";
                    }
                    : button {
                        label = "����λͼ(B)...";
                        key = "adjust_map";
                        mnemonic = "B";
                    }
                }
                : boxed_column {
                    key = "top_ray";
                    label = "����";
                    : edit_box {
                        label = "����(N):";
                        key = "env_filename";
                        edit_width = 12;
                        mnemonic = "N";
                    }
                    : button {
                        label = "�����ļ�(I)...";
                        key = "env_find_bitmap";
                        mnemonic = "I";
                    }
                    : toggle {
                        label = "ʹ�ñ���(U)";
                        key = "lock_env_to_bg";
                        mnemonic = "U";
                    }
                }
                : boxed_column {
                    key = "top_sliders";
                    : row {
                        : column {
                            : edit_box_4 {
                                label = "ˮƽ(H):";
                                key = "horizon_edt";
                                min_value = 0;
                                max_value = 100;
                                value = "0.5";
                                mnemonic = "H";
                            }
                            : edit_box_4 {
                                label = "�߶�(E):";
                                key = "height_edt";
                                min_value = 0;
                                max_value = 100;
                                value = "0.5";
                                mnemonic = "E";
                            }
                            : edit_box_4 {
                                label = "��ת(R):";
                                key = "angle_edt";
                                value = "90";
                                mnemonic = "R";
                            }
                        } /* edit column */
                        : column {
                            : slider {
                                key = "horizon_sld";
                                min_value = 0;
                                max_value = 100;
                                value = "50";
                                width = 14;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "height_sld";
                                min_value = 0;
                                max_value = 100;
                                value = "33";
                                width = 14;
                                is_tab_stop = false;
                            }
                            : slider {
                                key = "angle_sld";
                                min_value = -90;
                                max_value = 90;
                                value = "0";
                                width = 14;
                                is_tab_stop = false;
                            }
                        } /* slider column */
                   } /* row */
                } /* boxed column */
            } /* bottom row */
        ok_cancel_help_errtile;
        }
    }
}


//***************************************************************************
// Support for adjusting the Background image
//***************************************************************************

AdjustBGBitmap : dialog {
  label = "��������λͼλ��";
  : column { 
    key = "top_key";
    : column {
        : row {
            : boxed_column {
                key = "Offset_box";
                label = "ƫ��";
                fixed_height = true;
                : row {
                    children_alignment = top;
                    fixed_width = true;
                    : boxed_column {
                        fixed_height = true;
                        label = "����";
                        : slider {
                            key = "X_SIZE";
                            min_value = 0;
                            max_value = 102;
                            is_tab_stop = false;
                        }
                        : row {
                            fixed_width = true;
                            : slider {
                                layout = vertical;
                                key = "Y_SIZE";
                                fixed_width = true;
                                min_value = 0;
                                max_value = 102;
                                is_tab_stop = false;
                            }
                            : image {
                                key   = "BitImage";
                                color = graphics_background;
                                height = 8;
                                aspect_ratio = 1;
                                fixed_width = true;
                                fixed_height = true;
                                is_tab_stop = false;
                            }
                        } /* end row */
                    }
                    : column {
                        : slider {
                            layout = vertical;
                            key = "Y_OFFSET";
                            min_value = 0;
                            max_value = 10000;
                            is_tab_stop = false;
                        }
                    }
                } /* end of row */
                : row {
                    : slider {
                        key = "X_OFFSET";
                        min_value = 0;
                        max_value = 10000;
                        is_tab_stop = false;
                    }
                } /* end of row */
            } /* end column */
    
            : column {
                : toggle {
                    label = "������Ļ(F)";
                    mnemonic = "F";
                    key = "fit_to_screen";
                    value = "1";
                }
    
                : toggle {
                    key   = "BGAspect";
                    label = "ʹ��ͼ���߱�(U)";
                    mnemonic = "U";
                }
    
                spacer_1;
                : boxed_radio_column {
                    alignment = centered;
                    key   = "Tiling";
                    label = "���з�ʽ";
                    : radio_button {
                        key   = "T_TILE";
                        label = "ƽ��(T)";
                        mnemonic = "T";
                    }
                    : radio_button {
                        key   = "T_CROP";
                        label = "�޼�(C)";
                        mnemonic = "C";
                    }
                }
                spacer_1;
                : button {
                    alignment = centered;
                    label = "����(E)";
                    mnemonic = "E";
                    key = "center";
                }
            }
        } /* end of row */
    
        : column {
            : row {
                key = "Edit_Boxes";
                : column {
                    : text {
                        label = "ƫ��:";
                    }
                    : text {
                        label = "����:";
                    }
                }
                : column {
                    : edit_box {
                        label = "X:";
                        mnemonic = "X";
                        key   = "OX";
                        edit_limit = 8;
                    }
                    : edit_box {
                        label = "X:";
                        mnemonic = "X";
                        key   = "SX";
                        edit_limit = 8;
                    }
                }
                : column {
                    : edit_box {
                        label = "Y:";
                        mnemonic = "Y";
                        key   = "OY";
                        edit_limit = 8;
                    }
                    : edit_box {
                        label = "Y:";
                        mnemonic = "Y";
                        key   = "SY";
                        edit_limit = 8;
                    }
                }
            } /* end row */
            : toggle {
                key   = "Aspect";
                label = "���ֿ�߱�(M)";
                mnemonic = "M";
            }
        } /* end of boxed column */
    } /* end column */
    ok_cancel_help_errtile;
  }
}

//***************************************************************************
//***************************************************************************
//***************************************************************************
//***************************************************************************
// Support for Landscape objects
//***************************************************************************
//***************************************************************************
landscape_library : dialog {
    label = "�侰��";
    : row {
        : column {
            : text {
                key = "lib_name";
                width = 25;
                fixed_width = true;
            }
            : list_box {
                key = "plant_list";
                width = 25;
            }
        }
        : column {
            spacer_1;
            : button {
                label = "�޸�(&M)...";
                key = "plant_lib_modify";
            }
            : button {
                label = "�½�(&N)...";
                key = "plant_lib_new";
            }
            : button {
                label = "ɾ��(&D)";
                key = "plant_lib_delete";
            }
            : button {
                label = "��(&O)...";
                key = "plant_lib_open";
            }
            : button {
                label = "����(&S)...";
                key = "plant_lib_save";
            }
            spacer_1;
        }
    }
    ok_cancel_help_errtile;
}

landscape_lib_delete : dialog {
    label = "ɾ���侰��";
    : column {
        : text {
             key = "name";
             width = 30;
             alignment = centered;
        }
        : text {
            label = "�Ƿ�ȷ����";
            alignment = centered;
        }
    }
    ok_cancel;
}

landscape_lib_mod : dialog {
    key = "dialog";
    : column {
        key = "top_key";
        : row {
            : boxed_column {
                label = "Ĭ�ϼ���ͼ��";
                alignment = top;
                fixed_height = true;
                : radio_column {
                    key = "align_radio";
                    fixed_height = true;
                    : radio_button {
                        key = "align_single";
                        label = "����(S)";
                        mnemonic = "S";
                    }
                    : radio_button {
                        key = "align_cross";
                        label = "��Խ����(C)";
                        mnemonic = "C";
                    }
                }
                spacer_1;
                : toggle {
                    key = "align_toggle";
                    label = "�������(&V)";
                }
           }  
           finish_preview;
        }
        : row {
            : column {
                :  text {
                    label = "����:";
                }
                : text {
                    label = "ͼ���ļ�:";
                }
                : text {
                    label = "��͸����ͼ�ļ�:";
                }
            }
            : column {
                :  edit_box {
                    key = "plant_name";
                    edit_width = 14;
                }
                : edit_box {
                    key = "plant_image";
                    edit_width = 14;
                }
                : edit_box {
                    key = "plant_opac";
                    edit_width = 14;
                }
            }
            : column {
                spacer_1;
                spacer_1;
                : button {
                    label = "�����ļ�(F)...";
                    key = "ld_find_image";
                    mnemonic = "F";
                }
                : button {
                    label = "�����ļ�(N)...";
                    key = "ld_find_opac";
                    mnemonic = "N";
                }
            }
        }
        spacer_1;
        ok_cancel_help_errtile;
    }
}

landscape_lib_confirm : dialog {
    label = "�޸��侰��";
    initial_focus = "save";
    : column {
        : text {
            label = "��ǰ�侰�����޸ġ�";
            alignment = centered;
        }
        : row {
            : button {
                label = "�����޸�(S)...";
                is_default = true;
                mnemonic = "S";
                key = "save";
            }
            : button {
                label = "�����޸�(D)";
                mnemonic = "D";
                key = "discard";
            }
            : button {
                label = "ȡ������(C)";
                is_cancel = true;
                mnemonic = "C";
                key = "cancel";
            }
        }
    }
}

landscape_edit : dialog {
    key = "dialog";
    : column {
        key = "topkey";
        : row {
            : column {
                : text {
                    key = "lib_name";
                    width = 25;
                    fixed_width = true;
                }
                : list_box {
                    key = "plant_list";
                    width = 25;
                }
            }
            : column {
                finish_preview;
             }
        }
        : row {
            : boxed_column {
                label = "����ͼ��";
                : radio_column {
                    key = "align_radio";
                    fixed_height = true;
                    : radio_button {
                        key = "align_single";
                        label = "����(S)";
                        mnemonic = "S";
                    }
                    : radio_button {
                        key = "align_cross";
                        label = "��Խ����(C)";
                        mnemonic = "C";
                    }
               }
               spacer_1;
               : toggle {
                 key = "align_toggle";
                 label = "�������(&V)";
               }
            }
            : column {
                spacer_0;
                : edit_box_4 {
                    label = "�߶�(&H):";
                    key = "height_txt";
                    fixed_width = true;
                    value = "30";
                }
                : slider {
                    label = "�߶�";
                    key = "height_sld";
                    min_value = 1;
                    max_value = 100;
                    value = "30";
                }
                : button {
                    label = "λ��(&P) <";
                    key = "plant_placement";
                }
            }
        }
    ok_cancel_help_errtile;
    }
}

//***************************************************************************
// Support for Fog/Depth cue
//***************************************************************************
fog : dialog {
    label = "��/�������";
    : row {
        : toggle {
           label = "������(&E)";
           key = "fog_enable";
        }
        : toggle {
           label = "������(&B)";
           key = "fog_background";
        }
    }
    : column {
        key = "values_to_edit";
        : row {
            : boxed_column {
                color_system;
                color_system_set;
                light_color_panel;
            }
        }
        : boxed_row {
            : column {
                : text {
                    label = "������:      ";
                    key = "near_dist_txt";
                    width = 22;
                    fixed_width = true;
                }
                : text {
                    label = "Զ����:";
                    key = "far_dist_txt";
                }
            }
            : column {
                : edit_box_4 {
                    key = "near_dist_txt_ed";
                    value = "0";
                }
                : edit_box_4 {
                    key = "far_dist_txt_ed";
                    value = "100";
                }
            }
            : column {
                : slider_0_1_fixed { key = "near_dist_sld"; }
                : slider_0_1_fixed { key = "far_dist_sld"; }
            }
        }
        : boxed_row {
            : column {
                : text {
                    label = "�������ٷ���:";
                    key = "near_pct_txt";
                    width = 22;
                    fixed_width = true;
                }
                : text {
                    label = "Զ�����ٷ���:";
                    key = "far_pct_txt";
               }
            }
            : column {
                : edit_box_4 {
                    key = "near_pct_txt_ed";
                    value = "0";
                }
                : edit_box_4 {
                    key = "far_pct_txt_ed";
                    value = "100";
                }
            }
            : column {
                : slider_0_1_fixed { key = "near_pct_sld"; }
                : slider_0_1_fixed { key = "far_pct_sld"; }
            }
        }
    }
    spacer_1;
    ok_cancel_help_errtile;
}
