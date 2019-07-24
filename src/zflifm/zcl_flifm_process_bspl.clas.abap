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
CLASS zcl_flifm_process_bspl DEFINITION
  PUBLIC
  INHERITING FROM zcl_flifm_process
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS add_net_profit_lines
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF tys_rp_cysp_fields,
        rpamt   TYPE zif_flifm_definitions=>ty_amt_calc,
        rpper   TYPE zif_flifm_definitions=>ty_per_calc,
        cyspamt TYPE zif_flifm_definitions=>ty_amt_calc,
        cyspper TYPE zif_flifm_definitions=>ty_per_calc,
        idamt   TYPE zif_flifm_definitions=>ty_amt_calc,
        idper   TYPE zif_flifm_definitions=>ty_per_calc,
      END OF tys_rp_cysp_fields .
    TYPES:
      BEGIN OF tys_rp_cysp,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_rp_cysp_fields.
    TYPES : END OF tys_rp_cysp .
    TYPES:
      BEGIN OF tys_rp_cysp_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_rp_cysp_fields.
    TYPES: END OF tys_rp_cysp_fsv .
    TYPES:
      BEGIN OF tys_rp_cysp_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_rp_cysp.
    TYPES: END OF tys_rp_cysp_display .
    TYPES:
      BEGIN OF tys_rp_cysp_np,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_rp_cysp_fields.
    TYPES : END OF tys_rp_cysp_np .
    TYPES:
      BEGIN OF tys_rp_cysp_np_popup,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        txt50 TYPE txt50,
        waers TYPE waers.
        INCLUDE TYPE tys_rp_cysp_fields.
    TYPES : END OF tys_rp_cysp_np_popup .
    TYPES:
      BEGIN OF tys_try_tcy_fields,
        tryamt TYPE zif_flifm_definitions=>ty_amt_calc,
        tryper TYPE zif_flifm_definitions=>ty_per_calc,
        tcyamt TYPE zif_flifm_definitions=>ty_amt_calc,
        tcyper TYPE zif_flifm_definitions=>ty_per_calc,
        idamt  TYPE zif_flifm_definitions=>ty_amt_calc,
        idper  TYPE zif_flifm_definitions=>ty_per_calc,
      END OF tys_try_tcy_fields .
    TYPES:
      BEGIN OF tys_try_tcy,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_try_tcy_fields.
    TYPES: END OF tys_try_tcy .
    TYPES:
      BEGIN OF tys_try_tcy_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_try_tcy_fields.
    TYPES: END OF tys_try_tcy_fsv .
    TYPES:
      BEGIN OF tys_try_tcy_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_try_tcy.
    TYPES: END OF tys_try_tcy_display .
    TYPES:
      BEGIN OF tys_try_tcy_np,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_try_tcy_fields.
    TYPES : END OF tys_try_tcy_np .
    TYPES:
      BEGIN OF tys_try_tcy_np_popup,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        txt50 TYPE txt50,
        waers TYPE waers.
        INCLUDE TYPE tys_try_tcy_fields.
    TYPES : END OF tys_try_tcy_np_popup .
    TYPES:
      BEGIN OF tys_trend_fields,
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
      END OF tys_trend_fields .
    TYPES:
      BEGIN OF tys_ry_trend,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES: END OF tys_ry_trend .
    TYPES:
      BEGIN OF tys_ry_trend_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES: END OF tys_ry_trend_fsv .
    TYPES:
      BEGIN OF tys_ry_trend_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_trend.
    TYPES: END OF tys_ry_trend_display .
    TYPES:
      BEGIN OF tys_ry_trend_np,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES : END OF tys_ry_trend_np .
    TYPES:
      BEGIN OF tys_ry_trend_np_popup,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        txt50 TYPE txt50,
        waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES : END OF tys_ry_trend_np_popup .
    TYPES:
      BEGIN OF tys_cy_trend,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES: END OF tys_cy_trend .
    TYPES:
      BEGIN OF tys_cy_trend_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES: END OF tys_cy_trend_fsv .
    TYPES:
      BEGIN OF tys_cy_trend_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_trend.
    TYPES: END OF tys_cy_trend_display .
    TYPES:
      BEGIN OF tys_cy_trend_np,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES : END OF tys_cy_trend_np .
    TYPES:
      BEGIN OF tys_cy_trend_np_popup,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        txt50 TYPE txt50,
        waers TYPE waers.
        INCLUDE TYPE tys_trend_fields.
    TYPES : END OF tys_cy_trend_np_popup .
    TYPES:
      BEGIN OF tys_cy_rp_ry_fields,
        tcyamt    TYPE zif_flifm_definitions=>ty_amt_calc,
        tcyper    TYPE zif_flifm_definitions=>ty_per_calc,
        rpamt     TYPE zif_flifm_definitions=>ty_amt_calc,
        rpper     TYPE zif_flifm_definitions=>ty_per_calc,
        idcyrpamt TYPE zif_flifm_definitions=>ty_amt_calc,
        idcyrpper TYPE zif_flifm_definitions=>ty_per_calc,
        tryamt    TYPE zif_flifm_definitions=>ty_amt_calc,
        tryper    TYPE zif_flifm_definitions=>ty_per_calc,
        idrpryamt TYPE zif_flifm_definitions=>ty_amt_calc,
        idrpryper TYPE zif_flifm_definitions=>ty_per_calc,
      END OF tys_cy_rp_ry_fields .
    TYPES:
      BEGIN OF tys_cy_rp_ry,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_cy_rp_ry_fields.
    TYPES: END OF tys_cy_rp_ry .
    TYPES:
      BEGIN OF tys_cy_rp_ry_fsv.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE tys_cy_rp_ry_fields.
    TYPES: END OF tys_cy_rp_ry_fsv .
    TYPES:
      BEGIN OF tys_cy_rp_ry_display.
        INCLUDE TYPE zif_flifm_definitions=>tys_node_tab.
    TYPES: hkont TYPE hkont,
           txt50 TYPE txt50,
           waers TYPE waers.
        INCLUDE TYPE zflifms_bs_tcy_rp_try.
    TYPES: END OF tys_cy_rp_ry_display .
    TYPES:
      BEGIN OF tys_cy_rp_ry_np,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        waers TYPE waers.
        INCLUDE TYPE tys_cy_rp_ry_fields.
    TYPES : END OF tys_cy_rp_ry_np .
    TYPES:
      BEGIN OF tys_cy_rp_ry_np_popup,
        komok TYPE t030-komok,
        konts TYPE t030-konts,
        hkont TYPE hkont,
        txt50 TYPE txt50,
        waers TYPE waers.
        INCLUDE TYPE tys_cy_rp_ry_fields.
    TYPES : END OF tys_cy_rp_ry_np_popup .
    TYPES:
      tyt_rp_cysp          TYPE STANDARD TABLE OF tys_rp_cysp WITH DEFAULT KEY .
    TYPES:
      tyt_rp_cysp_fsv      TYPE STANDARD TABLE OF tys_rp_cysp_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_rp_cysp_display  TYPE STANDARD TABLE OF tys_rp_cysp_display WITH DEFAULT KEY .
    TYPES:
      tyt_rp_cysp_np       TYPE STANDARD TABLE OF tys_rp_cysp_np WITH DEFAULT KEY .
    TYPES:
      tyt_rp_cysp_np_popup TYPE STANDARD TABLE OF tys_rp_cysp_np_popup WITH DEFAULT KEY .
    TYPES:
      tyt_try_tcy          TYPE STANDARD TABLE OF tys_try_tcy WITH DEFAULT KEY .
    TYPES:
      tyt_try_tcy_fsv      TYPE STANDARD TABLE OF tys_try_tcy_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_try_tcy_display  TYPE STANDARD TABLE OF tys_try_tcy_display WITH DEFAULT KEY .
    TYPES:
      tyt_try_tcy_np       TYPE STANDARD TABLE OF tys_try_tcy_np WITH DEFAULT KEY .
    TYPES:
      tyt_try_tcy_np_popup TYPE STANDARD TABLE OF tys_try_tcy_np_popup WITH DEFAULT KEY .
    TYPES:
      tyt_ry_trend          TYPE STANDARD TABLE OF tys_ry_trend WITH DEFAULT KEY .
    TYPES:
      tyt_ry_trend_fsv      TYPE STANDARD TABLE OF tys_ry_trend_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_ry_trend_display  TYPE STANDARD TABLE OF tys_ry_trend_display WITH DEFAULT KEY .
    TYPES:
      tyt_ry_trend_np       TYPE STANDARD TABLE OF tys_ry_trend_np WITH DEFAULT KEY .
    TYPES:
      tyt_ry_trend_np_popup TYPE STANDARD TABLE OF tys_ry_trend_np_popup WITH DEFAULT KEY .
    TYPES:
      tyt_cy_trend          TYPE STANDARD TABLE OF tys_cy_trend WITH DEFAULT KEY .
    TYPES:
      tyt_cy_trend_fsv      TYPE STANDARD TABLE OF tys_cy_trend_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_cy_trend_display  TYPE STANDARD TABLE OF tys_cy_trend_display WITH DEFAULT KEY .
    TYPES:
      tyt_cy_trend_np       TYPE STANDARD TABLE OF tys_cy_trend_np WITH DEFAULT KEY .
    TYPES:
      tyt_cy_trend_np_popup TYPE STANDARD TABLE OF tys_cy_trend_np_popup WITH DEFAULT KEY .
    TYPES:
      tyt_cy_rp_ry          TYPE STANDARD TABLE OF tys_cy_rp_ry WITH DEFAULT KEY .
    TYPES:
      tyt_cy_rp_ry_fsv      TYPE STANDARD TABLE OF tys_cy_rp_ry_fsv WITH DEFAULT KEY .
    TYPES:
      tyt_cy_rp_ry_display  TYPE STANDARD TABLE OF tys_cy_rp_ry_display WITH DEFAULT KEY .
    TYPES:
      tyt_cy_rp_ry_np       TYPE STANDARD TABLE OF tys_cy_rp_ry_np WITH DEFAULT KEY .
    TYPES:
      tyt_cy_rp_ry_np_popup TYPE STANDARD TABLE OF tys_cy_rp_ry_np_popup WITH DEFAULT KEY .

    CONSTANTS mc_rp_cysp_tabname TYPE tabname VALUE 'ZFLIFMS_RP_CYSP' ##NO_TEXT.
    CONSTANTS mc_try_tcy_tabname TYPE tabname VALUE 'ZFLIFMS_TRY_TCY' ##NO_TEXT.
    CONSTANTS mc_trend_tabname TYPE tabname VALUE 'ZFLIFMS_TREND' ##NO_TEXT.
    CONSTANTS mc_cy_rp_ry_tabname TYPE tabname VALUE 'ZFLIFMS_BS_TCY_RP_TRY' ##NO_TEXT.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FLIFM_PROCESS_BSPL IMPLEMENTATION.


  METHOD add_net_profit_lines.


    DATA: lv_end_id   TYPE seu_id,
          lv_child_id TYPE seu_id,
          lv_line     TYPE i,
          lv_parent   TYPE seu_name,
          lv_cnt      TYPE i.

    DATA: lt_skat TYPE zif_flifm_definitions=>tyt_skat,
          ls_skat LIKE LINE OF lt_skat.

    DATA: lr_data_fsv         TYPE REF TO data,
          lr_data_np          TYPE REF TO data,
          lr_data_np_fsv      TYPE REF TO data,
          lr_popup_data_temp  TYPE REF TO data,
          lrs_popup_data_temp TYPE REF TO data,
          lr_popup_data_np    TYPE REF TO data.

    DATA: lv_sub_menu TYPE zif_flifm_definitions=>ty_flifm_menu_type.

    DATA lx_error TYPE REF TO cx_sy_create_data_error.

    FIELD-SYMBOLS: <lt_fsv> TYPE table,
                   <ls_fsv> TYPE data,
                   <lv_id>  TYPE any.

    FIELD-SYMBOLS: <lt_np>       TYPE table,
                   <ls_np>       TYPE data,
                   <lv_np_hkont> TYPE any,
                   <lv_np_komok> TYPE any,
                   <lv_np_konts> TYPE any.

    FIELD-SYMBOLS: <lt_np_fsv>        TYPE table,
                   <ls_np_fsv>        TYPE data,
                   <lv_np_fsv_hkont>  TYPE any,
                   <lv_np_fsv_txt50>  TYPE any,
                   <lv_np_fsv_text>   TYPE any,
                   <lv_np_fsv_id>     TYPE any,
                   <lv_np_fsv_type>   TYPE any,
                   <lv_np_fsv_name>   TYPE any,
                   <lv_np_fsv_tlevel> TYPE any,
                   <lv_np_fsv_parent> TYPE any,
                   <lv_np_fsv_child>  TYPE any.

    FIELD-SYMBOLS: <lt_popup_temp>       TYPE table,
                   <ls_popup_temp>       TYPE data,
                   <lv_popup_temp_komok> TYPE any,
                   <lv_popup_temp_konts> TYPE any,
                   <lv_popup_temp_hkont> TYPE any,
                   <lv_popup_temp_txt50> TYPE any.

    FIELD-SYMBOLS: <lt_popup_np>       TYPE table,
                   <ls_popup_np>       TYPE data,
                   <lv_popup_np_komok> TYPE any,
                   <lv_popup_np_konts> TYPE any,
                   <lv_popup_np_hkont> TYPE any,
                   <lv_popup_np_txt50> TYPE any.

    ASSIGN gr_data_fsv->* TO <lt_fsv>.
    CREATE DATA lr_data_fsv LIKE LINE OF <lt_fsv>.
    ASSIGN lr_data_fsv->* TO <ls_fsv>.

    IF iv_ytd_check IS INITIAL.

      ASSIGN gr_data_np->* TO <lt_np>.
      CREATE DATA lr_data_np LIKE LINE OF <lt_np>.
      ASSIGN lr_data_np->* TO <ls_np>.

      ASSIGN gr_data_np_fsv->* TO <lt_np_fsv>.
      CREATE DATA lr_data_np_fsv LIKE LINE OF <lt_np_fsv>.
      ASSIGN lr_data_np_fsv->* TO <ls_np_fsv>.

    ELSE.

      ASSIGN gr_data_np_ytd->* TO <lt_np>.
      CREATE DATA lr_data_np LIKE LINE OF <lt_np>.
      ASSIGN lr_data_np->* TO <ls_np>.

      ASSIGN gr_data_np_ytd_fsv->* TO <lt_np_fsv>.
      CREATE DATA lr_data_np_fsv LIKE LINE OF <lt_np_fsv>.
      ASSIGN lr_data_np_fsv->* TO <ls_np_fsv>.

    ENDIF.

    TRY.

        DATA: lv_type_name TYPE string.

        lv_sub_menu = zcl_flifm_utils=>split_menu( iv_menu = mv_menu
                                                   iv_sub = abap_true ).

        CONCATENATE 'TYT_' lv_sub_menu '_NP_POPUP' INTO lv_type_name.

        CREATE DATA lr_popup_data_temp TYPE (lv_type_name).
        ASSIGN lr_popup_data_temp->* TO <lt_popup_temp>.
        CREATE DATA lrs_popup_data_temp LIKE LINE OF <lt_popup_temp>.
        ASSIGN lrs_popup_data_temp->* TO <ls_popup_temp>.

        CLEAR lv_type_name.

        CONCATENATE gv_strname '_NP_POPUP' INTO lv_type_name.

        IF iv_ytd_check IS INITIAL.
          CREATE DATA gr_popup_data_np TYPE TABLE OF (lv_type_name).
          ASSIGN gr_popup_data_np->* TO <lt_popup_np>.
          CREATE DATA lr_popup_data_np LIKE LINE OF <lt_popup_np>.
          ASSIGN lr_popup_data_np->* TO <ls_popup_np>.
        ELSE.
          CREATE DATA gr_popup_data_np_ytd TYPE TABLE OF (lv_type_name).
          ASSIGN gr_popup_data_np_ytd->* TO <lt_popup_np>.
          CREATE DATA lr_popup_data_np LIKE LINE OF <lt_popup_np>.
          ASSIGN lr_popup_data_np->* TO <ls_popup_np>.
        ENDIF.

      CATCH cx_sy_create_data_error INTO lx_error.

        zcx_flifm_exception=>raise_msg( lx_error->get_text( ) ).

    ENDTRY.

    lt_skat = mo_fetch->get_skat( ).

    lv_line = lines( <lt_fsv> ).
    READ TABLE <lt_fsv> INTO <ls_fsv> INDEX lv_line.

    ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_fsv> TO <lv_id>.
    lv_end_id = <lv_id>.

    LOOP AT <lt_np> ASSIGNING <ls_np>.

      CLEAR: <ls_np_fsv>.
      MOVE-CORRESPONDING <ls_np> TO <ls_np_fsv>.

      ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_hkont>.
      CLEAR <lv_np_fsv_hkont>.

      IF lv_cnt = 0.
        lv_end_id = lv_end_id + 1.
        lv_parent = lv_end_id.
        lv_child_id = lv_end_id + 1.
        lv_cnt = 1.
      ENDIF.

      ASSIGN COMPONENT 'ID'     OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_id>.
      ASSIGN COMPONENT 'TYPE'   OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_type>.
      ASSIGN COMPONENT 'NAME'   OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_name>.
      ASSIGN COMPONENT 'TLEVEL' OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_tlevel>.
      ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_parent>.
      ASSIGN COMPONENT 'CHILD'  OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_child>.
      ASSIGN COMPONENT 'TEXT'   OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_text>.

      <lv_np_fsv_id>     = lv_parent.
      <lv_np_fsv_type>   = zif_flifm_definitions=>c_node_type-position.

      IF iv_ytd_check IS INITIAL.
        <lv_np_fsv_name>   = zif_flifm_definitions=>c_add_line_type-np_tot.
      ELSE.
        <lv_np_fsv_name>   = zif_flifm_definitions=>c_add_line_type-np_tot_mc.
      ENDIF.

      <lv_np_fsv_tlevel> = 2.
      <lv_np_fsv_parent> = 1.
      <lv_np_fsv_child>  = lv_child_id.

      IF iv_ytd_check IS INITIAL.
        <lv_np_fsv_text>   = zcl_flifm_i18n=>get_instance( )->calc_net_profit.
      ELSE.
        <lv_np_fsv_text>   = zcl_flifm_i18n=>get_instance( )->calc_net_profit_ytd.
      ENDIF.

      COLLECT <ls_np_fsv> INTO <lt_np_fsv>.

*// Net Profit Child
      ASSIGN COMPONENT 'KOMOK' OF STRUCTURE <ls_np> TO <lv_np_komok>.
      ASSIGN COMPONENT 'KONTS' OF STRUCTURE <ls_np> TO <lv_np_konts>.

      CONCATENATE <lv_np_komok> <lv_np_konts> INTO <lv_np_fsv_text> SEPARATED BY space.

      <lv_np_fsv_type>   = zif_flifm_definitions=>c_node_type-gl.
      <lv_np_fsv_tlevel> = 3.
      <lv_np_fsv_parent> = lv_parent.
      CLEAR <lv_np_fsv_id>.
      CLEAR <lv_np_fsv_child>.

      <lv_np_fsv_hkont> = <lv_np_konts>.

      ASSIGN COMPONENT 'TXT50' OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_txt50>.

      CLEAR ls_skat.
      READ TABLE lt_skat INTO ls_skat WITH TABLE KEY saknr = <lv_np_fsv_hkont>.
      IF sy-subrc = 0.
        <lv_np_fsv_txt50> = ls_skat-txt50.
      ELSE.
        CLEAR <lv_np_fsv_txt50>.
      ENDIF.

      COLLECT <ls_np_fsv> INTO <lt_np_fsv>.

*// Popup lines
      ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_np> TO <lv_np_hkont>.

      CLEAR ls_skat.
      READ TABLE lt_skat INTO ls_skat WITH TABLE KEY saknr = <lv_np_hkont>.
      IF sy-subrc = 0.
        <lv_np_fsv_txt50> = ls_skat-txt50.
      ELSE.
        CLEAR <lv_np_fsv_txt50>.
      ENDIF.

      <lv_np_fsv_hkont> = <lv_np_hkont>.

      CLEAR <ls_popup_temp>.
      MOVE-CORRESPONDING <ls_np_fsv> TO <ls_popup_temp>.

      ASSIGN COMPONENT 'KOMOK' OF STRUCTURE <ls_popup_temp> TO <lv_popup_temp_komok>.
      ASSIGN COMPONENT 'KONTS' OF STRUCTURE <ls_popup_temp> TO <lv_popup_temp_konts>.

      <lv_popup_temp_komok> = <lv_np_komok>.
      <lv_popup_temp_konts> = <lv_np_konts>.

      COLLECT <ls_popup_temp> INTO <lt_popup_temp>.

      ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_popup_temp> TO <lv_popup_temp_hkont>.
      ASSIGN COMPONENT 'TXT50' OF STRUCTURE <ls_popup_temp> TO <lv_popup_temp_txt50>.

      CLEAR ls_skat.
      READ TABLE lt_skat INTO ls_skat WITH TABLE KEY saknr = <lv_popup_temp_konts>.
      IF sy-subrc = 0.
        <lv_popup_temp_txt50> = ls_skat-txt50.
      ELSE.
        CLEAR <lv_popup_temp_txt50>.
      ENDIF.

      <lv_popup_temp_hkont> = <lv_popup_temp_konts>.

      COLLECT <ls_popup_temp> INTO <lt_popup_temp>.

    ENDLOOP.

    CHECK <lt_np_fsv> IS NOT INITIAL.

    LOOP AT <lt_np_fsv> ASSIGNING <ls_np_fsv> WHERE ('ID IS INITIAL').
      ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_np_fsv> TO <lv_np_fsv_id>.
      lv_end_id          = lv_end_id + 1.
      <lv_np_fsv_id>     = lv_end_id.
    ENDLOOP.

    SORT <lt_np_fsv> BY ('ID').

*// Calculate ratio, rate
    calc_ratio_rate( CHANGING ct_table = <lt_np_fsv> ).

    IF <lt_np_fsv> IS NOT INITIAL.
      APPEND LINES OF <lt_np_fsv> TO <lt_fsv>.
    ENDIF.

*// Accounts with zero balance
    DATA: lv_zero TYPE c.

    lv_zero = zcl_flifm_selection=>get_para_zero( ).
    IF lv_zero IS INITIAL.
      delete_zero_balance_add_lines( EXPORTING
                            it_fields = it_fields
                           CHANGING
                             ct_table = <lt_popup_temp> ).
    ENDIF.

    READ TABLE <lt_np_fsv> ASSIGNING <ls_np_fsv> INDEX 1.

*// Calculate ratio, rate
    calc_ratio_rate( EXPORTING is_data = <ls_np_fsv>
                     CHANGING ct_table = <lt_popup_temp> ).

*// Build popup data
    build_net_profit_popup( iv_ytd_check = iv_ytd_check
                            it_table = <lt_popup_temp> ).

*// Recalculate the total sum
    IF iv_ytd_check IS INITIAL.

      CHECK mv_menu <> zif_flifm_definitions=>c_flifm_menu_type-bs_ry_trend
       AND mv_menu <> zif_flifm_definitions=>c_flifm_menu_type-bs_cy_trend.

      READ TABLE <lt_fsv> ASSIGNING <ls_fsv> INDEX 1.
      READ TABLE <lt_np_fsv> ASSIGNING <ls_np_fsv> INDEX 1.

      recalculate_total_sum( EXPORTING
                              is_data  = <ls_np_fsv>
                             CHANGING
                               cs_data = <ls_fsv> ).
    ELSE.

      IF mv_menu = zif_flifm_definitions=>c_flifm_menu_type-bs_ry_trend
        OR mv_menu = zif_flifm_definitions=>c_flifm_menu_type-bs_cy_trend.

        READ TABLE <lt_fsv> ASSIGNING <ls_fsv> INDEX 1.
        READ TABLE <lt_np_fsv> ASSIGNING <ls_np_fsv> INDEX 1.

        recalculate_total_sum( EXPORTING
                                is_data  = <ls_np_fsv>
                               CHANGING
                                 cs_data = <ls_fsv> ).
      ENDIF.

    ENDIF.


  ENDMETHOD.
ENDCLASS.
