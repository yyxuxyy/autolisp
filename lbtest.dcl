/*������ListDCL @ fsxm.bokee.com������*/

lbtest:dialog {
    height = 50 ;
    label = "block lists" ;
    width = 30 ;
    :list_box {
        alignment = centered ;
        allow_accept = true ;
        height = 12 ;
        key = "blocks" ;
        label = "choose the block:" ;
    }
    :button {
        alignment = centered ;
        is_default = true ;
        key = "accept" ;
        label = "ok" ;
    }
}
