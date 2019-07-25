*&---------------------------------------------------------------------*
*& MIT License
*&
*& Copyright (c) 2016 FI LAB. All rights reserved.
*&
*& Permission is hereby granted, free of charge, to any person obtaining
*& a copy of this software and associated documentation files (the "Software"),
*& to deal in the Software without restriction, including without limitation
*& the rights to use, copy, modify, merge, publish, distribute, sublicense,
*& and/or sell copies of the Software, and to permit persons to whom the
*& Software is furnished to do so, subject to the following conditions:
*&
*& The above copyright notice and this permission notice shall be included
*& in all copies or substantial portions of the Software.
*&
*& THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
*& OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*& FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*& AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*& LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*& OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
*& IN THE SOFTWARE.
*&---------------------------------------------------------------------*
class ZCL_FLIFM_PROCESS_TB definition
  public
  inheriting from ZCL_FLIFM_PROCESS
  create public .

public section.


    TYPES:
      BEGIN OF tys_tb_ttb_fields,
        bcfamt TYPE zif_flifm_definitions=>ty_amt_calc,
        dtamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        dbamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        cbamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        ctamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        balamt TYPE zif_flifm_definitions=>ty_amt_calc,
      END OF tys_tb_ttb_fields .
    TYPES:
      BEGIN OF tys_tb_ttb,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_tb_ttb_fields.
    TYPES: END OF tys_tb_ttb .
    TYPES:
      BEGIN OF tys_tb_ttb_temp,
        hkont  TYPE hkont,
        waers  TYPE waers,
        obamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        ycbamt TYPE zif_flifm_definitions=>ty_amt_calc,
        ctamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        dtamt  TYPE zif_flifm_definitions=>ty_amt_calc,
      END OF tys_tb_ttb_temp .
    TYPES:
      BEGIN OF tys_tb_ttb_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_tb_ttb_fields.
    TYPES: END OF tys_tb_ttb_fsv .
    TYPES:
      BEGIN OF tys_tb_ttb_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_tb_ttb.
    TYPES: END OF tys_tb_ttb_display .
    TYPES:
      tyt_tb_ttb TYPE STANDARD TABLE OF tys_tb_ttb WITH DEFAULT KEY .
    TYPES:
      tyt_tb_ttb_fsv TYPE STANDARD TABLE OF tys_tb_ttb_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_tb_ttb_temp TYPE STANDARD TABLE OF tys_tb_ttb_temp WITH DEFAULT KEY .
    TYPES:
      tyt_tb_ttb_display TYPE STANDARD TABLE OF tys_tb_ttb_display WITH DEFAULT KEY .
    TYPES:
      BEGIN OF tys_tb_trend_fields,
        amt00 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt01 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt02 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt03 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt04 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt05 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt06 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt07 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt08 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt09 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt10 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt11 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt12 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt13 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt14 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt15 TYPE zif_flifm_definitions=>ty_amt_calc,
        amt16 TYPE zif_flifm_definitions=>ty_amt_calc,
      END OF tys_tb_trend_fields .
    TYPES:
      BEGIN OF tys_tb_trend,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_tb_trend_fields.
    TYPES: END OF tys_tb_trend .
    TYPES:
      BEGIN OF tys_tb_trend_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_tb_trend_fields.
    TYPES: END OF tys_tb_trend_fsv .
    TYPES:
      BEGIN OF tys_tb_trend_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_trend.
    TYPES: END OF tys_tb_trend_display .
    TYPES:
      tyt_tb_trend TYPE STANDARD TABLE OF tys_tb_trend WITH DEFAULT KEY .
    TYPES:
      tyt_tb_trend_fsv TYPE STANDARD TABLE OF tys_tb_trend_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_tb_trend_display TYPE STANDARD TABLE OF tys_tb_trend_display WITH DEFAULT KEY .
    TYPES:
      BEGIN OF tys_tb_rptb_fields,
        begamt TYPE zif_flifm_definitions=>ty_amt_calc,
        dtamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        ctamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        balamt TYPE zif_flifm_definitions=>ty_amt_calc,
      END OF tys_tb_rptb_fields .
    TYPES:
      BEGIN OF tys_tb_rptb,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_tb_rptb_fields.
    TYPES: END OF tys_tb_rptb .
    TYPES:
      BEGIN OF tys_tb_rptb_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_tb_rptb_fields.
    TYPES: END OF tys_tb_rptb_fsv .
    TYPES:
      BEGIN OF tys_tb_rptb_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_tb_rptb.
    TYPES: END OF tys_tb_rptb_display .
    TYPES:
      tyt_tb_rptb TYPE STANDARD TABLE OF tys_tb_rptb WITH DEFAULT KEY .
    TYPES:
      tyt_tb_rptb_fsv TYPE STANDARD TABLE OF tys_tb_rptb_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_tb_rptb_display TYPE STANDARD TABLE OF tys_tb_rptb_display WITH DEFAULT KEY .

  methods BUILD_DATA
    redefinition .
  methods SET_TOP_LINE_COLOR
    redefinition .
  methods ZIF_FLIFM_PROCESS~ADD_NOT_ASSIGNED_LINES
    redefinition .
  methods CREATE_DATA
    redefinition .
protected section.
  PRIVATE SECTION.

    CONSTANTS mc_tb_ttb_tabname TYPE tabname VALUE 'ZFLIFMS_TB_TTB' ##NO_TEXT.
    CONSTANTS mc_tb_trend_tabname TYPE tabname VALUE 'ZFLIFMS_TREND' ##NO_TEXT.
    CONSTANTS mc_tb_rptb_tabname TYPE tabname VALUE 'ZFLIFMS_TB_RPTB' ##NO_TEXT.

    METHODS _build_tb_ttb_data
      IMPORTING
        !it_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot
      RAISING
        zcx_flifm_exception .
    METHODS _build_tb_trend_data
      IMPORTING
        !it_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot
      RAISING
        zcx_flifm_exception .
    METHODS _build_tb_rptb_data
      IMPORTING
        !it_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot
      RAISING
        zcx_flifm_exception .
ENDCLASS.



CLASS ZCL_FLIFM_PROCESS_TB IMPLEMENTATION.


  METHOD build_data.

    create_data( ).

    build_fsv_data( mo_fetch->get_ifm_node_tab_tb( ) ).
    build_total_data( ).

  ENDMETHOD.


  method CREATE_DATA.

    DATA: lt_gl_tot TYPE zcl_flifm_fetch=>tyt_gl_tot,
          lr_bukrs  TYPE RANGE OF t001-bukrs.

    DATA: lv_type_name TYPE string.

    DATA: lx_error TYPE REF TO cx_sy_create_data_error.

    DATA: lv_msg TYPE string.

    TRY.

        CONCATENATE 'TYT_' mv_menu INTO lv_type_name.
        CREATE DATA gr_data TYPE (lv_type_name).

        CLEAR lv_type_name.
        CONCATENATE 'TYT_' mv_menu '_FSV' INTO lv_type_name.
        CREATE DATA gr_data_fsv    TYPE (lv_type_name).
        CREATE DATA gr_data_na_fsv TYPE (lv_type_name).

        CLEAR lv_type_name.
        CONCATENATE 'TYT_' mv_menu '_DISPLAY' INTO lv_type_name.
        CREATE DATA gr_data_display      TYPE (lv_type_name).
        CREATE DATA gr_data_display_line TYPE LINE OF (lv_type_name).

      CATCH cx_sy_create_data_error INTO lx_error.
        lv_msg = lx_error->get_text( ).
        zcx_flifm_exception=>raise_msg( lv_msg ).

    ENDTRY.

    lt_gl_tot = mo_fetch->get_gl_tot( ).

    CASE mv_menu.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_ttb.

        _build_tb_ttb_data( lt_gl_tot ).

        gv_strname = mc_tb_ttb_tabname.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_trend.

        _build_tb_trend_data( lt_gl_tot ).

        gv_strname = mc_tb_trend_tabname.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_rptb.

        _build_tb_rptb_data( lt_gl_tot ).

        gv_strname = mc_tb_rptb_tabname.

      WHEN OTHERS.
    ENDCASE.

  endmethod.


  METHOD set_top_line_color.


    FIELD-SYMBOLS: <lv_dtamt> TYPE any,
                   <lv_dbamt> TYPE any,
                   <lv_cbamt> TYPE any,
                   <lv_ctamt> TYPE any.

    FIELD-SYMBOLS: <lv_amt00> TYPE any.

    CASE mv_menu.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_ttb.

        ASSIGN COMPONENT 'DTAMT' OF STRUCTURE is_data TO <lv_dtamt>.
        ASSIGN COMPONENT 'DBAMT' OF STRUCTURE is_data TO <lv_dbamt>.
        ASSIGN COMPONENT 'CBAMT' OF STRUCTURE is_data TO <lv_cbamt>.
        ASSIGN COMPONENT 'CTAMT' OF STRUCTURE is_data TO <lv_ctamt>.

        IF ( <lv_dtamt> = <lv_ctamt> ) AND ( <lv_dbamt> = <lv_cbamt> ).
          mv_top_color = mc_tree_line_color-green.
        ELSE.
          mv_top_color = mc_tree_line_color-red.
        ENDIF.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_trend.

        ASSIGN COMPONENT 'AMT00' OF STRUCTURE is_data TO <lv_amt00>.

        IF <lv_amt00> = 0.
          mv_top_color = mc_tree_line_color-green.
        ELSE.
          mv_top_color = mc_tree_line_color-red.
        ENDIF.

      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_rptb.

        ASSIGN COMPONENT 'DTAMT' OF STRUCTURE is_data TO <lv_dtamt>.
        ASSIGN COMPONENT 'CTAMT' OF STRUCTURE is_data TO <lv_ctamt>.

        IF ( <lv_dtamt> = <lv_ctamt> ).
          mv_top_color = mc_tree_line_color-green.
        ELSE.
          mv_top_color = mc_tree_line_color-red.
        ENDIF.

    ENDCASE.


  ENDMETHOD.


  METHOD zif_flifm_process~add_not_assigned_lines.

    DATA: lv_end_id         TYPE seu_id,
          lv_child_id       TYPE seu_id,
          lv_line           TYPE i,
          lv_parent         TYPE seu_name,
          lv_cnt            TYPE i,
          lv_where          TYPE string,
          lv_data_type_name TYPE string.

    DATA: lt_skat TYPE zif_flifm_definitions=>tyt_skat,
          ls_skat LIKE LINE OF lt_skat.

    DATA: lr_data        TYPE REF TO data,
          lr_data_fsv    TYPE REF TO data,
          lr_data_na_fsv TYPE REF TO data.

    FIELD-SYMBOLS: <lt_na_fsv>    TYPE table,
                   <ls_na_fsv>    TYPE data,
                   <lv_na_hkont>  TYPE any,
                   <lv_na_txt50>  TYPE any,
                   <lv_na_text>   TYPE any,
                   <lv_na_id>     TYPE any,
                   <lv_na_type>   TYPE any,
                   <lv_na_name>   TYPE any,
                   <lv_na_tlevel> TYPE any,
                   <lv_na_parent> TYPE any,
                   <lv_na_child>  TYPE any.

    FIELD-SYMBOLS: <lt_fsv>    TYPE table,
                   <ls_fsv>    TYPE data,
                   <lv_fsv_id> TYPE any.

    FIELD-SYMBOLS: <lt_data>  TYPE table,
                   <ls_data>  TYPE data,
                   <lv_hkont> TYPE any,
                   <lv_txt50> TYPE any.

*// Assign table
    ASSIGN gr_data_fsv->* TO <lt_fsv>.
    CREATE DATA lr_data_fsv LIKE LINE OF <lt_fsv>.
    ASSIGN lr_data_fsv->* TO <ls_fsv>.

    ASSIGN gr_data->* TO <lt_data>.
    CREATE DATA lr_data LIKE LINE OF <lt_data>.
    ASSIGN lr_data->* TO <ls_data>.

    ASSIGN gr_data_na_fsv->* TO <lt_na_fsv>.
    CREATE DATA lr_data_na_fsv LIKE LINE OF <lt_na_fsv>.
    ASSIGN lr_data_na_fsv->* TO <ls_na_fsv>.

    lt_skat = mo_fetch->get_skat( ).

    lv_line = lines( <lt_fsv> ).
    READ TABLE <lt_fsv> INTO <ls_fsv> INDEX lv_line.

    ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_fsv> TO <lv_fsv_id>.
    lv_end_id = <lv_fsv_id>.

    lv_where = 'hkont NOT IN ir_hkont'.

*// Accounts with zero balance
    DATA: lv_zero TYPE c.

    lv_zero = zcl_flifm_selection=>get_para_zero( ).
    IF lv_zero IS INITIAL.
      delete_zero_balance_add_lines( EXPORTING
                            it_fields = it_fields
                           CHANGING
                             ct_table = <lt_data> ).
    ENDIF.

    LOOP AT <lt_data> ASSIGNING <ls_data> WHERE (lv_where).

      CLEAR: <ls_na_fsv>.
      MOVE-CORRESPONDING <ls_data> TO <ls_na_fsv>.

      ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_na_fsv> TO <lv_na_hkont>.
      CLEAR <lv_na_hkont>."Needed to make additional total line.

      IF lv_cnt = 0.
        lv_end_id = lv_end_id + 1.
        lv_parent = lv_end_id.
        lv_child_id = lv_end_id + 1.
        lv_cnt = 1.
      ENDIF.

      ASSIGN COMPONENT 'ID'     OF STRUCTURE <ls_na_fsv> TO <lv_na_id>.
      ASSIGN COMPONENT 'TYPE'   OF STRUCTURE <ls_na_fsv> TO <lv_na_type>.
      ASSIGN COMPONENT 'NAME'   OF STRUCTURE <ls_na_fsv> TO <lv_na_name>.
      ASSIGN COMPONENT 'TLEVEL' OF STRUCTURE <ls_na_fsv> TO <lv_na_tlevel>.
      ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_na_fsv> TO <lv_na_parent>.
      ASSIGN COMPONENT 'CHILD'  OF STRUCTURE <ls_na_fsv> TO <lv_na_child>.
      ASSIGN COMPONENT 'TEXT'   OF STRUCTURE <ls_na_fsv> TO <lv_na_text>.

      <lv_na_id>     = lv_parent.
      <lv_na_type>   = zif_flifm_definitions=>c_node_type-position.
      <lv_na_name>   = zif_flifm_definitions=>c_add_line_type-not_assigned.
      <lv_na_tlevel> = 2.
      <lv_na_parent> = 1.
      <lv_na_child>  = lv_child_id.
      <lv_na_text>   = zcl_flifm_i18n=>get_instance( )->not_assigned.

      COLLECT <ls_na_fsv> INTO <lt_na_fsv>.

      "Total Child
      ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_data> TO <lv_hkont>.
      ASSIGN COMPONENT 'TXT50' OF STRUCTURE <ls_na_fsv> TO <lv_na_txt50>.

      CLEAR ls_skat.
      READ TABLE lt_skat INTO ls_skat WITH TABLE KEY saknr = <lv_hkont>.
      IF sy-subrc = 0.
        <lv_na_txt50> = ls_skat-txt50.
      ENDIF.

      <lv_na_hkont> = <lv_hkont>.
      CONCATENATE <lv_hkont> <lv_na_txt50> INTO <lv_na_text> SEPARATED BY space.

      lv_end_id = lv_end_id + 1.
      <lv_na_id> = lv_end_id.
      <lv_na_type> = zif_flifm_definitions=>c_node_type-gl.
      <lv_na_tlevel> = 3.
      <lv_na_parent> = lv_parent.
      CLEAR <lv_na_child>.

      COLLECT <ls_na_fsv> INTO <lt_na_fsv>.

    ENDLOOP.

    CHECK <lt_na_fsv> IS NOT INITIAL.

    IF <lt_na_fsv> IS NOT INITIAL.
      APPEND LINES OF <lt_na_fsv> TO <lt_fsv>.
    ENDIF.

*// Recalculate the total sum
*// Get fields

    READ TABLE <lt_fsv> ASSIGNING <ls_fsv> INDEX 1.
    READ TABLE <lt_na_fsv> ASSIGNING <ls_na_fsv> INDEX 1.

    recalculate_total_sum( EXPORTING
                            is_data   = <ls_na_fsv>
                           CHANGING
                             cs_data = <ls_fsv> ).

  ENDMETHOD.


  METHOD _build_tb_rptb_data.


    DATA: ls_tb_rptb TYPE tys_tb_rptb.

    DATA: ls_gl_tot LIKE LINE OF it_gl_tot.

    DATA: lv_gjahr       TYPE gjahr,
          lv_monat       TYPE monat,
          lv_inter_waers TYPE waers,
          lv_tcurr       TYPE waers.

    DATA: lr_bukrs TYPE tyr_bukrs.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company.

    DATA lv_ukurs TYPE zflifme_ukurs.

    FIELD-SYMBOLS: <lt_tb_rptb> TYPE table.
    ASSIGN gr_data->* TO <lt_tb_rptb>.

    lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
    lv_monat = zcl_flifm_selection=>get_monat( ).
    lv_inter_waers = mo_fetch->get_inter_waers( ).
    lr_bukrs = get_select_company( ).
    lv_tcurr = get_to_currency( ).

    LOOP AT it_gl_tot INTO ls_gl_tot WHERE gjahr = lv_gjahr
                                       AND monat = lv_monat
                                       AND bukrs IN lr_bukrs.
      CLEAR ls_tb_rptb.

      ls_tb_rptb-hkont = ls_gl_tot-hkont.

      lv_ukurs = mo_fetch->get_tcurr( iv_gjahr = lv_gjahr
                                      iv_monat = lv_monat
                                      iv_fcurr = ls_gl_tot-waers
                                      iv_tcurr = lv_tcurr ).
      " Beginning amount
      ls_tb_rptb-begamt = ( ls_gl_tot-ycbamt - ls_gl_tot-mbamt ) * lv_ukurs.

      " Balance amount
      ls_tb_rptb-balamt = ls_gl_tot-ycbamt * lv_ukurs.

      " Credit total
      ls_tb_rptb-ctamt = ls_gl_tot-cbamt * lv_ukurs * -1.

      " Debit total
      ls_tb_rptb-dtamt = ls_gl_tot-dbamt * lv_ukurs.

      ls_tb_rptb-waers = lv_inter_waers.

      COLLECT : ls_tb_rptb INTO <lt_tb_rptb>.

    ENDLOOP.

  ENDMETHOD.


  METHOD _build_tb_trend_data.


    DATA: ls_tb_trend TYPE tys_tb_trend.

    DATA: ls_gl_tot LIKE LINE OF it_gl_tot.

    DATA: lv_gjahr       TYPE gjahr,
          lv_monat       TYPE monat,
          lv_inter_waers TYPE waers,
          lv_tcurr       TYPE waers.

    DATA: lr_bukrs TYPE tyr_bukrs.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company.

    DATA lv_ukurs TYPE zflifme_ukurs.

    DATA lv_monat_amt TYPE string.

    FIELD-SYMBOLS: <lv_monat_amt> TYPE any.

    FIELD-SYMBOLS: <lt_tb_trend> TYPE table.
    ASSIGN gr_data->* TO <lt_tb_trend>.

    lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
    lv_monat = zcl_flifm_selection=>get_monat( ).
    lv_inter_waers = mo_fetch->get_inter_waers( ).
    lr_bukrs = get_select_company( ).
    lv_tcurr = get_to_currency( ).

    LOOP AT it_gl_tot INTO ls_gl_tot WHERE gjahr = lv_gjahr
                                       AND bukrs IN lr_bukrs.

      CLEAR ls_tb_trend.
      ls_tb_trend-hkont = ls_gl_tot-hkont.

      lv_ukurs = mo_fetch->get_tcurr( iv_gjahr = lv_gjahr
                                      iv_monat = lv_monat
                                      iv_fcurr = ls_gl_tot-waers
                                      iv_tcurr = lv_tcurr ).

      IF ls_gl_tot-monat = lv_monat.
        ls_tb_trend-amt00 = ls_gl_tot-ycbamt * lv_ukurs.
      ENDIF.

      CONCATENATE 'LS_TB_TREND-AMT' ls_gl_tot-monat INTO lv_monat_amt.
      ASSIGN (lv_monat_amt) TO <lv_monat_amt>.
      <lv_monat_amt> = ls_gl_tot-ycbamt * lv_ukurs.

      ls_tb_trend-waers = lv_tcurr.

      COLLECT ls_tb_trend INTO <lt_tb_trend>.

      UNASSIGN <lv_monat_amt>.

    ENDLOOP.

  ENDMETHOD.


  METHOD _build_tb_ttb_data.


    DATA: ls_tb_ttb      TYPE tys_tb_ttb,
          lt_tb_ttb_temp TYPE tyt_tb_ttb_temp,
          ls_tb_ttb_temp LIKE LINE OF lt_tb_ttb_temp.

    DATA: ls_gl_tot LIKE LINE OF it_gl_tot.

    DATA: lv_gjahr       TYPE gjahr,
          lv_monat       TYPE monat,
          lv_inter_waers TYPE waers,
          lv_tcurr       TYPE waers.

    DATA: lr_bukrs TYPE tyr_bukrs.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company.

    DATA lv_ukurs TYPE zflifme_ukurs.

    FIELD-SYMBOLS: <lt_tb_ttb> TYPE table.
    ASSIGN gr_data->* TO <lt_tb_ttb>.

    lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
    lv_monat = zcl_flifm_selection=>get_monat( ).
    lv_inter_waers = mo_fetch->get_inter_waers( ).
    lr_bukrs = get_select_company( ).
    lv_tcurr = get_to_currency( ).

    LOOP AT it_gl_tot INTO ls_gl_tot WHERE gjahr = lv_gjahr
                                       AND monat = lv_monat
                                       AND bukrs IN lr_bukrs.

      ls_tb_ttb-hkont = ls_tb_ttb_temp-hkont = ls_gl_tot-hkont.

      lv_ukurs = mo_fetch->get_tcurr( iv_gjahr = lv_gjahr
                                         iv_monat = lv_monat
                                         iv_fcurr = ls_gl_tot-waers
                                         iv_tcurr = lv_tcurr ).

      " Balance carried forward amount
      ls_tb_ttb-bcfamt =

      " Opening balance amount
      ls_tb_ttb_temp-obamt = ls_gl_tot-obamt * lv_ukurs.

      " Balance amount
      ls_tb_ttb-balamt =

      " Yearly Cumulative Balance amount
      ls_tb_ttb_temp-ycbamt = ls_gl_tot-ycbamt * lv_ukurs.

      " Credit total amount
      ls_tb_ttb_temp-ctamt = ls_gl_tot-ctamt * lv_ukurs.

      " Debit total amount
      ls_tb_ttb_temp-dtamt = ls_gl_tot-dtamt * lv_ukurs.

      ls_tb_ttb-waers = ls_tb_ttb_temp-waers = lv_inter_waers.

      COLLECT: ls_tb_ttb INTO <lt_tb_ttb>,
               ls_tb_ttb_temp INTO lt_tb_ttb_temp.

      CLEAR: ls_gl_tot, ls_tb_ttb, ls_tb_ttb_temp.

    ENDLOOP.

    DELETE lt_tb_ttb_temp WHERE obamt = 0 AND ycbamt = 0 AND ctamt = 0 AND dtamt = 0.

    CLEAR: ls_tb_ttb, ls_tb_ttb_temp.

    SORT lt_tb_ttb_temp BY hkont.

    LOOP AT lt_tb_ttb_temp INTO ls_tb_ttb_temp.

      ls_tb_ttb-hkont = ls_tb_ttb_temp-hkont.
      ls_tb_ttb-waers = ls_tb_ttb_temp-waers.

      "Credit Total / Credit multiply by -1
      ls_tb_ttb-ctamt = ls_tb_ttb_temp-ctamt * -1.

      "Debit Total
      ls_tb_ttb-dtamt = ls_tb_ttb_temp-dtamt.

      "If the yearly cumulative balance is greater than zero
      IF ls_tb_ttb_temp-ycbamt > 0.
        "Debit Balance
        ls_tb_ttb-dbamt = ls_tb_ttb_temp-ycbamt.
      ELSE.
        "Credit Balance / Credit multiply by -1
        ls_tb_ttb-cbamt = ls_tb_ttb_temp-ycbamt * -1.
      ENDIF.

      COLLECT ls_tb_ttb INTO <lt_tb_ttb>.
      CLEAR: ls_tb_ttb, ls_tb_ttb_temp.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
