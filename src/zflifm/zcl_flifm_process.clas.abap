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
class ZCL_FLIFM_PROCESS definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_FLIFM_PROCESS .

  aliases ADD_NET_PROFIT_LINES
    for ZIF_FLIFM_PROCESS~ADD_NET_PROFIT_LINES .
  aliases ADD_NOT_ASSIGNED_LINES
    for ZIF_FLIFM_PROCESS~ADD_NOT_ASSIGNED_LINES .
  aliases CALC_RATIO_RATE
    for ZIF_FLIFM_PROCESS~CALC_RATIO_RATE .
  aliases TYT_DFIES
    for ZIF_FLIFM_PROCESS~TYT_DFIES .

  data GR_DATA type ref to DATA .
  data GR_DATA_FSV type ref to DATA .
  data GR_DATA_NA_FSV type ref to DATA .
  data GR_DATA_NP type ref to DATA .
  data GR_DATA_NP_FSV type ref to DATA .
  data GR_DATA_NP_YTD type ref to DATA .
  data GR_DATA_NP_YTD_FSV type ref to DATA .
  data GR_DATA_DISPLAY type ref to DATA .
  data GR_DATA_DISPLAY_LINE type ref to DATA .
  data GR_POPUP_DATA type ref to DATA .
  data GR_POPUP_DATA_NP type ref to DATA .
  data GR_POPUP_DATA_NP_YTD type ref to DATA .
  data GV_STRNAME type TABNAME .

  methods CONSTRUCTOR
    importing
      !IV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE
      !IV_COMPANY type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_COMPANY_TYPE
      !IV_ACTION type ZIF_FLIFM_DEFINITIONS=>TY_ACTION .
  methods BUILD_DATA
  abstract
    raising
      ZCX_FLIFM_EXCEPTION .
  methods CREATE_DATA
  abstract
    raising
      ZCX_FLIFM_EXCEPTION .
  methods SET_TOP_LINE_COLOR
  abstract
    importing
      !IS_DATA type DATA .
  methods BUILD_TREE
    importing
      !IO_ALV_TREE type ref to CL_GUI_ALV_TREE
    returning
      value(RT_EXP) type LVC_T_NKEY
    raising
      ZCX_FLIFM_EXCEPTION .
  methods GET_FSV_AMT_DATA
    importing
      !IT_NODE_TAB type ZIF_FLIFM_DEFINITIONS=>TYT_NODE_TAB optional
    exporting
      !ET_TABLE type TABLE
      !ET_HKONT type ZIF_FLIFM_PROCESS=>TYR_HKONT .
protected section.

  types:
    tyr_bukrs TYPE RANGE OF t001-bukrs .

  data MO_FETCH type ref to ZCL_FLIFM_FETCH .
  data MV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE .
  data MV_COMPANY type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_COMPANY_TYPE .
  data MV_ACTION type ZIF_FLIFM_DEFINITIONS=>TY_ACTION .
  data MV_TOP_COLOR type I .
  constants:
    BEGIN OF mc_tree_line_color,
        yellow   TYPE i VALUE cl_gui_column_tree=>style_emphasized, "C300
        green    TYPE i VALUE cl_gui_column_tree=>style_emphasized_positive, "C500
        red      TYPE i VALUE cl_gui_column_tree=>style_emphasized_negative, "C610
        sky_blue TYPE i VALUE cl_gui_column_tree=>style_emphasized_a, "C100
      END OF mc_tree_line_color .

  methods GET_SELECT_COMPANY
    returning
      value(RR_BUKRS) type TYR_BUKRS .
  methods GET_TO_CURRENCY
    returning
      value(RV_TCURR) type WAERS .
  methods BUILD_FSV_DATA
    importing
      !IT_NODE_TAB type ZIF_FLIFM_DEFINITIONS=>TYT_NODE_TAB
    raising
      ZCX_FLIFM_EXCEPTION .
  methods BUILD_FSV_AMT_DATA
    importing
      !IT_NODE_TAB type ZIF_FLIFM_DEFINITIONS=>TYT_NODE_TAB
    exporting
      !ET_TABLE type TABLE
      !ET_HKONT type ZIF_FLIFM_PROCESS=>TYR_HKONT
    raising
      ZCX_FLIFM_EXCEPTION .
  methods MOVE_AMT
    importing
      !IS_DATA type DATA
      !IT_FIELDS type TYT_DFIES
    changing
      !CS_DATA type DATA
      !CT_DATA type STANDARD TABLE optional .
  methods BUILD_TOTAL_DATA .
  methods DELETE_ZERO_BALANCE
    importing
      !IT_FIELDS type TYT_DFIES
    changing
      !CT_TABLE type TABLE .
  methods DELETE_ZERO_BALANCE_ADD_LINES
    importing
      !IT_FIELDS type TYT_DFIES
    changing
      !CT_TABLE type TABLE .
  methods RECALCULATE_TOTAL_SUM
    importing
      !IS_DATA type DATA
    changing
      !CS_DATA type DATA
    raising
      ZCX_FLIFM_EXCEPTION .
  methods CHANGE_NUMBER_TO_CHAR
    importing
      !IT_FIELDS type TYT_DFIES optional
    changing
      !CS_DATA type DATA .
  methods MOVE_SIGN_FRONT
    importing
      !IV_WAERS type WAERS optional
    changing
      !CV_DATA type CLIKE .
  methods ADD_TREE_NODES
    importing
      !IO_ALV_TREE type ref to CL_GUI_ALV_TREE
      !IS_DATA type DATA
      !IV_TOP_COLOR type I optional
      !IV_NKEY type LVC_NKEY optional
    returning
      value(RV_CUR_NODE) type LVC_NKEY
    raising
      ZCX_FLIFM_EXCEPTION .
  methods BUILD_NET_PROFIT_POPUP
    importing
      !IV_YTD_CHECK type ABAP_BOOL optional
      !IT_TABLE type STANDARD TABLE
    raising
      ZCX_FLIFM_EXCEPTION .
private section.

  methods _BUILD_FSV_LIST_POPUP
    importing
      !IV_NKEY type LVC_NKEY
      !IV_COLOR type ZIF_FLIFM_DEFINITIONS=>TY_ROW_COLOR
      !IS_DATA type DATA
    raising
      ZCX_FLIFM_EXCEPTION .
ENDCLASS.



CLASS ZCL_FLIFM_PROCESS IMPLEMENTATION.


  METHOD add_tree_nodes.

    DATA : ls_node_layout TYPE lvc_s_layn,
           lv_row_color   TYPE zif_flifm_definitions=>ty_row_color,
           lv_node_text   TYPE lvc_value.

    FIELD-SYMBOLS: <ls_data>   TYPE data,
                   <lv_type>   TYPE any,
                   <lv_name>   TYPE any,
                   <lv_parent> TYPE any,
                   <lv_text>   TYPE any.

    ASSIGN is_data->* TO <ls_data>.

    ASSIGN COMPONENT 'TYPE'   OF STRUCTURE <ls_data> TO <lv_type>.
    ASSIGN COMPONENT 'NAME'   OF STRUCTURE <ls_data> TO <lv_name>.
    ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_data> TO <lv_parent>.
    ASSIGN COMPONENT 'TEXT'   OF STRUCTURE <ls_data> TO <lv_text>.

    lv_node_text = <lv_text>.

    " Top line color is red or green.
    IF <lv_parent> = 0.
      ls_node_layout-style = iv_top_color.
      IF iv_top_color = mc_tree_line_color-green.
        lv_row_color = zif_flifm_definitions=>c_row_color-green.
      ELSE.
        lv_row_color = zif_flifm_definitions=>c_row_color-red.
      ENDIF.
    ENDIF.

    " Level 2 color is yellow.
    IF <lv_parent> = 1 AND ( <lv_name> <> zif_flifm_definitions=>c_add_line_type-tot
                             AND <lv_name> <> zif_flifm_definitions=>c_add_line_type-not_assigned
                             AND <lv_name> <> zif_flifm_definitions=>c_add_line_type-np_tot ).
      ls_node_layout-style = mc_tree_line_color-yellow.
      lv_row_color = zif_flifm_definitions=>c_row_color-yellow.
    ENDIF.

    " Add lines color.
    IF <lv_type> = zif_flifm_definitions=>c_node_type-position AND
       <lv_name> = zif_flifm_definitions=>c_add_line_type-tot.

      ls_node_layout-n_image = icon_sum.
      ls_node_layout-style =  mc_tree_line_color-sky_blue.
      lv_row_color = zif_flifm_definitions=>c_row_color-sky_blue.

    ELSEIF <lv_type> = zif_flifm_definitions=>c_node_type-position AND
           <lv_name> = zif_flifm_definitions=>c_add_line_type-not_assigned.

      ls_node_layout-n_image = icon_assign.
      ls_node_layout-exp_image = icon_unassign.
      ls_node_layout-style = mc_tree_line_color-yellow.
      lv_row_color = zif_flifm_definitions=>c_row_color-yellow.

    ELSEIF <lv_type> = zif_flifm_definitions=>c_node_type-position AND
           ( <lv_name> = zif_flifm_definitions=>c_add_line_type-np_tot OR
             <lv_name> = zif_flifm_definitions=>c_add_line_type-np_tot_mc ).
      ls_node_layout-n_image = icon_calculation.
      ls_node_layout-exp_image = icon_calculation.

      CASE zcl_flifm_utils=>split_menu( mv_menu ).
        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl.

          IF <lv_name> = zif_flifm_definitions=>c_add_line_type-np_tot.
            ls_node_layout-style = mc_tree_line_color-yellow.
            lv_row_color = zif_flifm_definitions=>c_row_color-yellow.
          ELSE.
            ls_node_layout-style = mc_tree_line_color-sky_blue.
            lv_row_color = zif_flifm_definitions=>c_row_color-sky_blue.
          ENDIF.

        WHEN zif_flifm_definitions=>c_flifm_menu_type-bs.

          IF mv_menu = zif_flifm_definitions=>c_flifm_menu_type-bs_ry_trend OR
            mv_menu = zif_flifm_definitions=>c_flifm_menu_type-bs_cy_trend.

            IF <lv_name> = zif_flifm_definitions=>c_add_line_type-np_tot.
              ls_node_layout-style = mc_tree_line_color-sky_blue.
              lv_row_color = zif_flifm_definitions=>c_row_color-sky_blue.
            ELSE.
              ls_node_layout-style = mc_tree_line_color-yellow.
              lv_row_color = zif_flifm_definitions=>c_row_color-yellow.
            ENDIF.

          ELSE.

            IF <lv_name> = zif_flifm_definitions=>c_add_line_type-np_tot.
              ls_node_layout-style = mc_tree_line_color-yellow.
              lv_row_color = zif_flifm_definitions=>c_row_color-yellow.
            ENDIF.

          ENDIF.

      ENDCASE.

    ENDIF.

    CALL METHOD io_alv_tree->add_node
      EXPORTING
        i_relat_node_key = iv_nkey
        i_relationship   = cl_gui_column_tree=>relat_last_child
        is_outtab_line   = <ls_data>
        is_node_layout   = ls_node_layout
        i_node_text      = |{ <lv_text> }|
      IMPORTING
        e_new_node_key   = rv_cur_node.

*// Build alv popup
    _build_fsv_list_popup( iv_nkey  = iv_nkey
                           iv_color = lv_row_color
                           is_data  = is_data ).


  ENDMETHOD.


  METHOD build_fsv_amt_data.


    DATA: lt_node_tab_sub TYPE zif_flifm_definitions=>tyt_node_tab.

    DATA: lr_data     TYPE REF TO data,
          lr_data_fsv TYPE REF TO data.

    DATA: lr_hkont  TYPE zif_flifm_process=>tyr_hkont,
          lrs_hkont LIKE LINE OF lr_hkont.

    DATA lv_id TYPE seu_id.

    DATA lv_subrc TYPE sy-subrc.

    DATA: lt_skat TYPE zif_flifm_definitions=>tyt_skat,
          ls_skat LIKE LINE OF lt_skat.

    DATA: lt_fields TYPE tyt_dfies,
          ls_fields LIKE LINE OF lt_fields.

    DATA lv_tcurr TYPE waers.

    FIELD-SYMBOLS: <lt_fsv>   TYPE table,
                   <ls_fsv>   TYPE data,
                   <lv_id>    TYPE any,
                   <lv_name>  TYPE any,
                   <lv_waers> TYPE any.

    FIELD-SYMBOLS: <lt_data>  TYPE table,
                   <ls_data>  TYPE data,
                   <lv_hkont> TYPE any,
                   <lv_txt50> TYPE any.

    FIELD-SYMBOLS: <ls_node_tab>     LIKE LINE OF it_node_tab,
                   <ls_node_tab_sub> LIKE LINE OF lt_node_tab_sub.

*// Define
    DEFINE _define_range_hkont.
      &1-sign = 'I'.
      &1-option = 'EQ'.
      &1-low = &2.
      APPEND &1 TO &3.
      CLEAR &1.
    END-OF-DEFINITION.

    lt_skat = mo_fetch->get_skat( ).

    lt_node_tab_sub = it_node_tab.

*// Get Currency
    lv_tcurr = get_to_currency( ).

*// Get fields
    lt_fields = zcl_flifm_utils=>get_fieldinfo( gv_strname ).

*// Assign table
    ASSIGN gr_data_fsv->* TO <lt_fsv>.
    CREATE DATA lr_data_fsv LIKE LINE OF <lt_fsv>.
    ASSIGN lr_data_fsv->* TO <ls_fsv>.

    ASSIGN gr_data->* TO <lt_data>.
    CREATE DATA lr_data LIKE LINE OF <lt_data>.
    ASSIGN lr_data->* TO <ls_data>.

    SORT <lt_data> BY ('HKONT').

    LOOP AT it_node_tab ASSIGNING <ls_node_tab>.

      CLEAR: lv_id, <ls_fsv>.

      lv_id = <ls_node_tab>-parent.

      MOVE-CORRESPONDING <ls_node_tab> TO <ls_fsv>.

      ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_fsv> TO <lv_name>.

      READ TABLE <lt_data> ASSIGNING <ls_data> WITH KEY ('HKONT') = <lv_name> BINARY SEARCH.

      lv_subrc = sy-subrc.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_data> TO <lv_hkont>.
        MOVE-CORRESPONDING <ls_data> TO <ls_fsv>.
        _define_range_hkont: lrs_hkont <lv_hkont> lr_hkont.

        CLEAR ls_skat.

        READ TABLE lt_skat INTO ls_skat WITH TABLE KEY saknr = <lv_hkont>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT 'TXT50' OF STRUCTURE <ls_fsv> TO <lv_txt50>.
          <lv_txt50> = ls_skat-txt50.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_fsv> TO <lv_waers>.
      <lv_waers> = lv_tcurr.

      COLLECT <ls_fsv> INTO <lt_fsv>.

      CHECK lv_subrc = 0.

      WHILE lv_id IS NOT INITIAL.

        LOOP AT lt_node_tab_sub ASSIGNING <ls_node_tab_sub> WHERE id = lv_id.

          CLEAR: <ls_fsv>.

          MOVE-CORRESPONDING <ls_node_tab_sub> TO <ls_fsv>.

          move_amt( EXPORTING is_data   = <ls_data>
                              it_fields = lt_fields
                    CHANGING cs_data = <ls_fsv> ).

          ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_fsv> TO <lv_waers>.
          <lv_waers> = lv_tcurr.

          COLLECT <ls_fsv> INTO <lt_fsv>.
          lv_id = <ls_node_tab_sub>-parent.

        ENDLOOP.

      ENDWHILE.

    ENDLOOP.

    et_table = <lt_fsv>.
    et_hkont = lr_hkont.

  ENDMETHOD.


  METHOD build_fsv_data.


    DATA: lt_node_tab_sub TYPE zif_flifm_definitions=>tyt_node_tab.

    DATA: lv_fav_tabname TYPE tabname,
          lv_tabname     TYPE tabname.

    DATA: lr_data     TYPE REF TO data,
          lr_data_fsv TYPE REF TO data.

    DATA: lr_hkont  TYPE zif_flifm_process=>tyr_hkont,
          lrs_hkont LIKE LINE OF lr_hkont.

    DATA: lt_skat TYPE zif_flifm_definitions=>tyt_skat,
          ls_skat LIKE LINE OF lt_skat.

    DATA lv_id TYPE seu_id.

    DATA lv_subrc TYPE sy-subrc.

    DATA: lt_fields TYPE tyt_dfies,
          ls_fields LIKE LINE OF lt_fields.

    DATA: lv_field TYPE fieldname.

    FIELD-SYMBOLS: <lt_fsv>  TYPE table,
                   <ls_fsv>  TYPE data,
                   <lv_id>   TYPE any,
                   <lv_name> TYPE any.

    FIELD-SYMBOLS: <ls_node_tab>     LIKE LINE OF it_node_tab,
                   <ls_node_tab_sub> LIKE LINE OF lt_node_tab_sub.

*// Define
    DEFINE _define_range_hkont.
      &1-sign = 'I'.
      &1-option = 'EQ'.
      &1-low = &2.
      APPEND &1 TO &3.
      CLEAR &1.
    END-OF-DEFINITION.

    lt_skat = mo_fetch->get_skat( ).

    lt_node_tab_sub = it_node_tab.

*// Get fields
    lt_fields = zcl_flifm_utils=>get_fieldinfo( gv_strname ).

*// Assign table
    ASSIGN gr_data_fsv->* TO <lt_fsv>.
    CREATE DATA lr_data_fsv LIKE LINE OF <lt_fsv>.
    ASSIGN lr_data_fsv->* TO <ls_fsv>.

    build_fsv_amt_data( EXPORTING it_node_tab = it_node_tab
                        IMPORTING et_table = <lt_fsv>
                                  et_hkont = lr_hkont ).

*// Accounts with zero balance
    DATA: lv_zero TYPE c.

    lv_zero = zcl_flifm_selection=>get_para_zero( ).
    IF lv_zero IS INITIAL.
      delete_zero_balance( EXPORTING
                            it_fields = lt_fields
                           CHANGING
                             ct_table = <lt_fsv> ).
    ENDIF.

*// PL, BS
    calc_ratio_rate( CHANGING ct_table = <lt_fsv> ).

*// PL, BS
    add_net_profit_lines( it_fields = lt_fields ).
    add_net_profit_lines( it_fields = lt_fields iv_ytd_check = abap_true ).

*// TB
    add_not_assigned_lines( it_fields = lt_fields ir_hkont = lr_hkont ).

*// Setting top line color
    READ TABLE <lt_fsv> ASSIGNING <ls_fsv> INDEX 1.
    set_top_line_color( <ls_fsv> ).

    CLEAR: lr_data, lr_data_fsv.


  ENDMETHOD.


  METHOD build_net_profit_popup.


    DATA: lr_popup_data_temp TYPE REF TO data,
          lr_popup_data_np   TYPE REF TO data.

    DATA: lv_type_name TYPE string.

    DATA: lt_fields TYPE tyt_dfies.

    DATA: lt_node_tab_tb TYPE zif_flifm_definitions=>tyt_node_tab.

    FIELD-SYMBOLS: <ls_popup_temp> TYPE data.

    FIELD-SYMBOLS: <lt_popup_np>       TYPE table,
                   <ls_popup_np>       TYPE data,
                   <lv_popup_np_komok> TYPE any,
                   <lv_popup_np_konts> TYPE any,
                   <lv_popup_np_hkont> TYPE any,
                   <lv_row_color>      TYPE any,
                   <lv_sort_key>       TYPE any.

    CONSTANTS: c_sort_key_sum TYPE c VALUE '2',
               c_sort_key_na  TYPE c VALUE '1'.

    CREATE DATA lr_popup_data_temp LIKE LINE OF it_table.
    ASSIGN lr_popup_data_temp->* TO <ls_popup_temp>.

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

*// Get FSV Node
    lt_node_tab_tb = mo_fetch->get_ifm_node_tab_tb( ).

*// Get fields
    lt_fields = zcl_flifm_utils=>get_fieldinfo( gv_strname ).

    LOOP AT it_table ASSIGNING <ls_popup_temp>.

      CLEAR: <ls_popup_np>.
      MOVE-CORRESPONDING <ls_popup_temp> TO <ls_popup_np>.

      change_number_to_char( EXPORTING
                              it_fields = lt_fields
                             CHANGING
                               cs_data = lr_popup_data_np ).

      ASSIGN COMPONENT 'KONTS' OF STRUCTURE <ls_popup_np> TO <lv_popup_np_konts>.
      ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_popup_np> TO <lv_popup_np_hkont>.

      ASSIGN COMPONENT 'ROW_COLOR' OF STRUCTURE <ls_popup_np> TO <lv_row_color>.
      ASSIGN COMPONENT 'SORT_KEY' OF STRUCTURE <ls_popup_np> TO <lv_sort_key>.
      IF <lv_popup_np_konts> = <lv_popup_np_hkont>.
        <lv_row_color> = zif_flifm_definitions=>c_row_color-yellow.
        <lv_sort_key> = c_sort_key_sum.
      ELSE.
        READ TABLE lt_node_tab_tb TRANSPORTING NO FIELDS WITH KEY name = <lv_popup_np_hkont>.
        IF sy-subrc <> 0.
          <lv_row_color> = zif_flifm_definitions=>c_row_color-red.
          <lv_sort_key> = c_sort_key_na.
        ENDIF.
      ENDIF.

      APPEND <ls_popup_np> TO <lt_popup_np>.

      UNASSIGN: <lv_popup_np_konts>, <lv_popup_np_hkont>, <lv_row_color>.

    ENDLOOP.

    SORT <lt_popup_np> BY ('SORT_KEY') ('KOMOK') ('KONTS') ('HKONT').


  ENDMETHOD.


  METHOD build_total_data.


*// Layout data
    DATA: lt_layout       TYPE zif_flifm_definitions=>tyt_layout,
          ls_layout       LIKE LINE OF lt_layout,
          ls_saved_layout TYPE zif_flifm_definitions=>ty_saved_layout.

    DATA: lt_data_fsv TYPE REF TO data,
          lt_data_tot TYPE REF TO data,
          ls_data_tot TYPE REF TO data.

    DATA: lv_split_menu TYPE zif_flifm_definitions=>ty_flifm_menu_type.

*// Fsv tree data
    FIELD-SYMBOLS: <lt_fsv>    TYPE table,
                   <ls_fsv>    TYPE data,
                   <lv_id>     TYPE any,
                   <lv_name>   TYPE any,
                   <lv_parent> TYPE any,
                   <lv_child>  TYPE any,
                   <lv_text>   TYPE any,
                   <lt_tot>    TYPE table,
                   <ls_tot>    TYPE data,
                   <lv_tot_id> TYPE any.

    FIELD-SYMBOLS: <ms_display> TYPE data.

    ASSIGN gr_data_fsv->* TO <lt_fsv>.
    CREATE DATA lt_data_fsv LIKE LINE OF <lt_fsv>.
    ASSIGN lt_data_fsv->* TO <ls_fsv>.

    "Use Total
    CREATE DATA lt_data_tot LIKE <lt_fsv>.
    ASSIGN lt_data_tot->* TO <lt_tot>.

    CREATE DATA ls_data_tot LIKE LINE OF <lt_tot>.
    ASSIGN ls_data_tot->* TO <ls_tot>.

    lt_layout = zcl_flifm_fetch=>get_instance( )->get_layout( ).
    ls_saved_layout = zcl_flifm_fetch=>get_instance( )->get_saved_layout( ).

    lv_split_menu = zcl_flifm_utils=>split_menu( mv_menu ).

    LOOP AT lt_layout INTO ls_layout WHERE item = lv_split_menu.

      READ TABLE <lt_fsv> ASSIGNING <ls_fsv> WITH KEY ('NAME')  = ls_layout-name.

      IF sy-subrc = 0.
        CLEAR <ls_tot>.
        MOVE-CORRESPONDING <ls_fsv> TO <ls_tot>.

        ASSIGN COMPONENT 'ID'    OF STRUCTURE <ls_tot> TO <lv_id>.
        ASSIGN COMPONENT 'NAME'  OF STRUCTURE <ls_tot> TO <lv_name>.
        ASSIGN COMPONENT 'CHILD' OF STRUCTURE <ls_tot> TO <lv_child>.
        ASSIGN COMPONENT 'TEXT'  OF STRUCTURE <ls_tot> TO <lv_text>.

        CLEAR: <lv_id>, <lv_name>, <lv_child>, <lv_text>.

        COLLECT <ls_tot> INTO <lt_tot>.
      ENDIF.

    ENDLOOP.

    CHECK <lt_tot> IS NOT INITIAL.

    DATA: lv_name TYPE seu_name,
          lv_text TYPE seu_text.

    CASE lv_split_menu.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb.
        lv_name = ls_saved_layout-pl.
        lv_text = zcl_flifm_i18n=>get_instance( )->fsv_tot_i18n-tb.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl.
        lv_name = ls_saved_layout-pl.
        lv_text = zcl_flifm_i18n=>get_instance( )->fsv_tot_i18n-pl.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-bs.
        lv_name = ls_saved_layout-liab_equity.
        lv_text = zcl_flifm_i18n=>get_instance( )->fsv_tot_i18n-bs.
      WHEN OTHERS.

    ENDCASE.

    LOOP AT <lt_tot> ASSIGNING <ls_tot>.
      READ TABLE <lt_fsv> ASSIGNING <ls_fsv> WITH KEY ('NAME')  = lv_name.

      IF sy-subrc = 0.

        sy-tabix = sy-tabix + 1.
        LOOP AT <lt_fsv> ASSIGNING <ls_fsv> FROM sy-tabix.

          ASSIGN COMPONENT 'ID'     OF STRUCTURE <ls_fsv> TO <lv_id>.
          ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_fsv> TO <lv_parent>.
          ASSIGN COMPONENT 'ID'     OF STRUCTURE <ls_tot> TO <lv_tot_id>.
          ASSIGN COMPONENT 'NAME'   OF STRUCTURE <ls_tot> TO <lv_name>.
          ASSIGN COMPONENT 'TEXT'   OF STRUCTURE <ls_tot> TO <lv_text>.

          IF <lv_parent> = 1.

            <lv_tot_id> = <lv_id> - 1.
            <lv_name>   = zif_flifm_definitions=>c_add_line_type-tot.
            <lv_text>   = lv_text.
            APPEND <ls_tot> TO <lt_fsv>.

            EXIT.

          ENDIF.

          IF sy-tabix = lines( <lt_fsv> ).

            <lv_tot_id> = <lv_id> + 1.
            <lv_name>   = zif_flifm_definitions=>c_add_line_type-tot.
            <lv_text>   = lv_text.
            APPEND <ls_tot> TO <lt_fsv>.

            EXIT.

          ENDIF.

        ENDLOOP.

        IF sy-subrc <> 0.
          ASSIGN COMPONENT 'ID'     OF STRUCTURE <ls_fsv> TO <lv_id>.
          ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_fsv> TO <lv_parent>.
          ASSIGN COMPONENT 'ID'     OF STRUCTURE <ls_tot> TO <lv_tot_id>.
          ASSIGN COMPONENT 'NAME'   OF STRUCTURE <ls_tot> TO <lv_name>.
          ASSIGN COMPONENT 'TEXT'   OF STRUCTURE <ls_tot> TO <lv_text>.
          <lv_tot_id> = <lv_id> + 1.
          <lv_name>   = zif_flifm_definitions=>c_add_line_type-tot.
          <lv_text>   = lv_text.
          APPEND <ls_tot> TO <lt_fsv>.
        ENDIF.

      ENDIF.

    ENDLOOP.

    SORT <lt_fsv> BY ('ID').


  ENDMETHOD.


  METHOD build_tree.


    DATA: lv_fav_tabname TYPE tabname.

    DATA: lv_cur_node  TYPE lvc_nkey,
          lv_top_color TYPE i.

    DATA: lt_data     TYPE REF TO data,
          lt_data_fsv TYPE REF TO data.

    DATA: lv_level TYPE seu_level.

    DATA: lt_fields TYPE tyt_dfies.

    FIELD-SYMBOLS: <lt_fsv>       TYPE table,
                   <ls_fsv>       TYPE data,
                   <ls_data>      TYPE data,
                   <lv_parent>    TYPE any,
                   <lv_text>      TYPE any,
                   <lv_saknr>     TYPE any,
                   <lv_nkey>      TYPE any,
                   <lv_tlevel>    TYPE any,
                   <lv_data_nkey> TYPE any.

    FIELD-SYMBOLS: <ms_display> TYPE data.

    lv_level = zcl_flifm_selection=>get_level( ).

*// Get fields
    lt_fields = zcl_flifm_utils=>get_fieldinfo( gv_strname ).

    CHECK gr_data_fsv IS NOT INITIAL.

    ASSIGN gr_data_fsv->* TO <lt_fsv>.
    CREATE DATA lt_data_fsv LIKE LINE OF <lt_fsv>.
    ASSIGN lt_data_fsv->* TO <ls_fsv>.

    CREATE DATA lt_data LIKE LINE OF <lt_fsv>.
    ASSIGN lt_data->* TO <ls_data>.

    ASSIGN gr_data_display_line->* TO <ms_display>.

    LOOP AT <lt_fsv> ASSIGNING <ls_fsv>.

      ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_fsv> TO <lv_parent>.
      IF <lv_parent> = 0.
        ASSIGN COMPONENT 'TEXT' OF STRUCTURE <ls_fsv> TO <lv_text>.

        <lv_text> = zcl_flifm_i18n=>get_instance( )->get_menu_description( mv_menu ).

      ENDIF.

      MOVE-CORRESPONDING <ls_fsv> TO <ms_display>.

      change_number_to_char( EXPORTING
                              it_fields = lt_fields
                             CHANGING
                               cs_data = gr_data_display_line ).

      READ TABLE <lt_fsv> ASSIGNING <ls_data> WITH KEY ('ID') = <lv_parent>.

      IF sy-subrc = 0.

        ASSIGN COMPONENT 'NKEY' OF STRUCTURE <ls_data> TO <lv_data_nkey>.

        lv_cur_node = add_tree_nodes( io_alv_tree = io_alv_tree
                                      is_data = gr_data_display_line
                                      iv_nkey = <lv_data_nkey> ).

        "Expand Level
        ASSIGN COMPONENT 'TLEVEL' OF STRUCTURE <ls_fsv> TO <lv_tlevel>.
        IF lv_level >= <lv_tlevel>.
          COLLECT <lv_data_nkey> INTO rt_exp.
        ENDIF.

      ELSE.

        lv_cur_node = add_tree_nodes( io_alv_tree = io_alv_tree
                                      is_data = gr_data_display_line
                                      iv_top_color = mv_top_color ).

      ENDIF.

      ASSIGN COMPONENT 'NKEY' OF STRUCTURE <ls_fsv> TO <lv_nkey>.
      <lv_nkey> = lv_cur_node.

      CLEAR <ms_display>.

    ENDLOOP.

    CLEAR: gr_data_fsv, gr_data_display_line.


  ENDMETHOD.


  METHOD change_number_to_char.


    DATA: ls_fields LIKE LINE OF it_fields.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_waers> TYPE any,
                   <lv_data>  TYPE any.

    ASSIGN cs_data->* TO <ls_data>.

    ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_data> TO <lv_waers>.
    IF <lv_waers> IS INITIAL.
      <lv_waers> = get_to_currency( ).
    ENDIF.

    LOOP AT it_fields INTO ls_fields.

      ASSIGN COMPONENT ls_fields-fieldname OF STRUCTURE <ls_data> TO <lv_data>.

      IF ls_fields-fieldname CP '*PER'.
        move_sign_front( CHANGING cv_data = <lv_data> ).
      ELSE.
        move_sign_front( EXPORTING
                          iv_waers = <lv_waers>
                         CHANGING
                           cv_data = <lv_data> ).
      ENDIF.

      UNASSIGN <lv_data>.

    ENDLOOP.


  ENDMETHOD.


  METHOD constructor.

    mv_menu = iv_menu.
    mv_company = iv_company.
    mv_action  = iv_action.

    mo_fetch = zcl_flifm_fetch=>get_instance( ).

  ENDMETHOD.


  METHOD delete_zero_balance.


    DATA: lr_id  TYPE RANGE OF seu_id,
          lrs_id LIKE LINE OF lr_id.

    DATA : lv_id     TYPE seu_id,
           lv_parent TYPE seu_id.

    DATA: lv_lines TYPE i.

    DATA: ls_fields LIKE LINE OF it_fields.

    DATA: lv_del_where  TYPE string,
          lv_loop_where TYPE string.

    DATA: lt_data TYPE REF TO data,
          ls_data TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data>       TYPE table,
                   <ls_data>       TYPE data,
                   <ls_data_while> TYPE data,
                   <lv_id>         TYPE any,
                   <lv_parent>     TYPE any.

    CREATE DATA lt_data LIKE ct_table.
    ASSIGN lt_data->* TO <lt_data>.
    <lt_data> = ct_table.

    CREATE DATA ls_data LIKE LINE OF ct_table.
    ASSIGN ls_data->* TO <ls_data>.
    ASSIGN ls_data->* TO <ls_data_while>.

*// Define
    DEFINE _define_range_id.
      &1-sign = 'I'.
      &1-option = 'EQ'.
      &1-low = &2.
      APPEND &1 TO &3.
      CLEAR &1.
    END-OF-DEFINITION.

    lv_lines = lines( it_fields ).

    lv_loop_where = |( TYPE = 'G' ) AND ( |.

    LOOP AT it_fields INTO ls_fields.

      IF lv_lines = sy-tabix.
        lv_loop_where = lv_loop_where && |{ ls_fields-fieldname } <> 0 )|.
      ELSE.
        lv_loop_where = lv_loop_where && |{ ls_fields-fieldname } <> 0 OR |.
      ENDIF.

    ENDLOOP.

    LOOP AT <lt_data> ASSIGNING <ls_data> WHERE (lv_loop_where).

      ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_data> TO <lv_id>.
      _define_range_id: lrs_id <lv_id> lr_id.

      lv_id = <lv_id>.

      ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_data> TO <lv_parent>.
      lv_parent = <lv_parent>.

      WHILE lv_id <> 1.

        READ TABLE <lt_data> ASSIGNING <ls_data_while> WITH KEY ('ID') = lv_parent.

        IF sy-subrc = 0.
          _define_range_id: lrs_id <lv_id> lr_id.
          ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_data_while> TO <lv_id>.
          ASSIGN COMPONENT 'PARENT' OF STRUCTURE <ls_data_while> TO <lv_parent>.
          lv_id = <lv_id>.
          lv_parent = <lv_parent>.
        ELSE.
          EXIT.
        ENDIF.

      ENDWHILE.

    ENDLOOP.

    IF lr_id IS INITIAL.

      lv_del_where = |ID <> 1 AND TLEVEL <> 2 |.

      LOOP AT it_fields INTO ls_fields.
        lv_del_where = lv_del_where && |AND { ls_fields-fieldname } = 0 |.
      ENDLOOP.

      DELETE <lt_data> WHERE (lv_del_where).

    ELSE.
      SORT lr_id.
      DELETE ADJACENT DUPLICATES FROM lr_id.
      DELETE <lt_data> WHERE ('ID <> 1 AND TLEVEL <> 2 AND ID NOT IN LR_ID').
    ENDIF.

    ct_table = <lt_data>.


  ENDMETHOD.


  METHOD delete_zero_balance_add_lines.

    DATA: lv_del_where  TYPE string.

    DATA: ls_fields LIKE LINE OF it_fields.

    DATA: lt_data TYPE REF TO data,
          ls_data TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE table,
                   <ls_data> TYPE data.

    CREATE DATA lt_data LIKE ct_table.
    ASSIGN lt_data->* TO <lt_data>.
    <lt_data> = ct_table.

    CREATE DATA ls_data LIKE LINE OF ct_table.
    ASSIGN ls_data->* TO <ls_data>.

    LOOP AT it_fields INTO ls_fields.

      IF sy-tabix = 1.
        lv_del_where = |{ ls_fields-fieldname } = 0|.
      ELSE.
        lv_del_where = lv_del_where && | AND { ls_fields-fieldname } = 0 |.
      ENDIF.

    ENDLOOP.

    DELETE <lt_data> WHERE (lv_del_where).

    ct_table = <lt_data>.

  ENDMETHOD.


  METHOD get_fsv_amt_data.

    create_data( ).

    DATA lv_menu TYPE zif_flifm_definitions=>ty_flifm_menu_type.
    DATA lt_node_tab TYPE zif_flifm_definitions=>tyt_node_tab.

    lv_menu = zcl_flifm_utils=>split_menu( iv_menu = mv_menu ).

    CASE lv_menu.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-bs.
        lt_node_tab = mo_fetch->get_ifm_node_tab_bs( ).
      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl.
        lt_node_tab = mo_fetch->get_ifm_node_tab_pl( ).
      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb.
        lt_node_tab = mo_fetch->get_ifm_node_tab_tb( ).
      WHEN OTHERS.

    ENDCASE.

    build_fsv_amt_data(
      EXPORTING
        it_node_tab = lt_node_tab
      IMPORTING
        et_table = et_table ).

  ENDMETHOD.


  METHOD get_select_company.


    DATA: lrs_bukrs LIKE LINE OF rr_bukrs.

*// Define
    DEFINE _define_range_bukrs.
      &1-sign = 'I'.
      &1-option = 'EQ'.
      &1-low = &2.
      APPEND &1 TO &3.
      CLEAR &1.
    END-OF-DEFINITION.

    IF mv_company IS NOT INITIAL.
      _define_range_bukrs: lrs_bukrs mv_company rr_bukrs.
    ENDIF.


  ENDMETHOD.


  METHOD get_to_currency.


    DATA: lv_inter_waers TYPE waers.

    lv_inter_waers = mo_fetch->get_inter_waers( ).

    CASE mv_action.
      WHEN zif_flifm_definitions=>c_action-ic.

        rv_tcurr = lv_inter_waers.

      WHEN zif_flifm_definitions=>c_action-lc.

        DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
              ls_company LIKE LINE OF lt_company.

        lt_company = mo_fetch->get_company( ).

        READ TABLE lt_company INTO ls_company WITH TABLE KEY bukrs = mv_company.

        IF sy-subrc = 0.
          rv_tcurr = ls_company-waers.
        ENDIF.

      WHEN OTHERS.
        rv_tcurr = lv_inter_waers.
    ENDCASE.


  ENDMETHOD.


  METHOD move_amt.


    DATA: ls_fields LIKE LINE OF it_fields.

    FIELD-SYMBOLS: <lv_cs> TYPE any,
                   <lv_is> TYPE any.

    LOOP AT it_fields INTO ls_fields.

      ASSIGN COMPONENT ls_fields-fieldname OF STRUCTURE cs_data TO <lv_cs>.

      ASSIGN COMPONENT ls_fields-fieldname OF STRUCTURE is_data TO <lv_is>.

      <lv_cs> = <lv_is>.

    ENDLOOP.


  ENDMETHOD.


  METHOD move_sign_front.

    zcl_flifm_utils=>move_number_to_char( EXPORTING
                                            iv_waers = iv_waers
                                          CHANGING
                                            cv_data = cv_data ).

    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
      CHANGING
        value = cv_data.

    IF iv_waers IS INITIAL.
      CONCATENATE cv_data '%' INTO cv_data SEPARATED BY space.
    ENDIF.


  ENDMETHOD.


  METHOD recalculate_total_sum.


    DATA: lt_fields TYPE zif_flifm_definitions=>tyt_dfies,
          ls_fields LIKE LINE OF lt_fields.

    FIELD-SYMBOLS: <lv_cs> TYPE any,
                   <lv_is> TYPE any.

    lt_fields = zcl_flifm_utils=>get_fieldinfo( gv_strname ).

    LOOP AT lt_fields INTO ls_fields.

      ASSIGN COMPONENT ls_fields-fieldname OF STRUCTURE cs_data TO <lv_cs>.

      ASSIGN COMPONENT ls_fields-fieldname OF STRUCTURE is_data TO <lv_is>.

      CASE zcl_flifm_utils=>split_menu( mv_menu ).
        WHEN zif_flifm_definitions=>c_flifm_menu_type-tb.
          <lv_cs> = <lv_cs> + <lv_is>.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl.
          <lv_cs> = <lv_cs> - <lv_is>.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-bs.
          <lv_cs> = <lv_cs> - <lv_is>.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD zif_flifm_process~add_net_profit_lines.
  ENDMETHOD.


  METHOD zif_flifm_process~add_not_assigned_lines.
  ENDMETHOD.


  METHOD zif_flifm_process~calc_ratio_rate.
  ENDMETHOD.


  METHOD _build_fsv_list_popup.


    DATA lv_tabname TYPE tabname.

    DATA lr_data TYPE REF TO data.

    DATA lv_tlevel TYPE seu_level.

    DATA lx_error TYPE REF TO cx_sy_create_data_error.

    FIELD-SYMBOLS: <lt_popup_data> TYPE table,
                   <ls_popup_data> TYPE data,
                   <ls_nkey>       TYPE any,
                   <lv_tlevel>     TYPE any,
                   <lv_text>       TYPE any,
                   <lv_row_color>  TYPE any.

    FIELD-SYMBOLS: <is_data> TYPE data.

    ASSIGN is_data->* TO <is_data>.

    IF gr_popup_data IS INITIAL.

      CASE mv_menu.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_ttb.
          lv_tabname = 'ZFLIFMS_TB_TTB_POPUP'.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_trend.
          lv_tabname = 'ZFLIFMS_TREND_POPUP'.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-tb_rptb.
          lv_tabname = 'ZFLIFMS_TB_RPTB_POPUP'.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_rp_cysp OR
             zif_flifm_definitions=>c_flifm_menu_type-bs_rp_cysp.
          lv_tabname = 'ZFLIFMS_RP_CYSP_POPUP'.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_try_tcy OR
             zif_flifm_definitions=>c_flifm_menu_type-bs_try_tcy.
          lv_tabname = 'ZFLIFMS_TRY_TCY_POPUP'.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl_ry_trend OR
             zif_flifm_definitions=>c_flifm_menu_type-pl_cy_trend OR
             zif_flifm_definitions=>c_flifm_menu_type-bs_ry_trend OR
             zif_flifm_definitions=>c_flifm_menu_type-bs_cy_trend.
          lv_tabname = 'ZFLIFMS_TREND_POPUP'.
        WHEN zif_flifm_definitions=>c_flifm_menu_type-bs_cy_rp_ry.
          lv_tabname = 'ZFLIFMS_BS_TCY_RP_TRY_POPUP'.
        WHEN OTHERS.
          zcx_flifm_exception=>raise_msg( |No popup tabnmae. Error from _build_fsv_list_popup| ).
      ENDCASE.

      TRY.
          CREATE DATA gr_popup_data TYPE TABLE OF (lv_tabname).
        CATCH cx_sy_create_data_error INTO lx_error.

          zcx_flifm_exception=>raise_msg( lx_error->get_text( ) ).

      ENDTRY.

    ENDIF.

    ASSIGN gr_popup_data->* TO <lt_popup_data>.

    CREATE DATA lr_data LIKE LINE OF <lt_popup_data>.
    ASSIGN lr_data->* TO <ls_popup_data>.

    MOVE-CORRESPONDING <is_data> TO <ls_popup_data>.

    ASSIGN COMPONENT 'NKEY'      OF STRUCTURE <ls_popup_data> TO <ls_nkey>.
    ASSIGN COMPONENT 'TLEVEL'    OF STRUCTURE <ls_popup_data> TO <lv_tlevel>.
    ASSIGN COMPONENT 'ROW_COLOR' OF STRUCTURE <ls_popup_data> TO <lv_row_color>.
    ASSIGN COMPONENT 'TEXT'      OF STRUCTURE <ls_popup_data> TO <lv_text>.

    <ls_nkey> = iv_nkey.
    <lv_row_color> = iv_color.

    CONSTANTS: lv_space TYPE c LENGTH 1 VALUE ` `.

    lv_tlevel = <lv_tlevel> - 1.
    DO lv_tlevel TIMES.
*-> `  `: no-break space / Dec: 160 / Hex: 0xA0
*-- You can input Alt + 255
      CONCATENATE zif_flifm_definitions=>c_no_break_space <lv_text> INTO <lv_text>.
    ENDDO.

    APPEND <ls_popup_data> TO <lt_popup_data>.


  ENDMETHOD.
ENDCLASS.
