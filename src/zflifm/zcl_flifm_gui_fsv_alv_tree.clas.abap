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
class ZCL_FLIFM_GUI_FSV_ALV_TREE definition
  public
  inheriting from ZCL_FLIFM_GUI
  create public .

public section.

  methods ZIF_FLIFM_GUI~RENDER
    redefinition .
protected section.
private section.

  class-data MO_FSV_ALV_TREE type ref to ZCL_FLIFM_GUI_FSV_ALV_TREE .
  data MT_LIST_COMMENTARY type SLIS_T_LISTHEADER .

  methods _INITIALIZE_ALV_TREE
    raising
      ZCX_FLIFM_EXCEPTION .
  methods _BUILD_ALV_TREE_HEADER
    returning
      value(RS_ALV_TREE_HEADER) type TREEV_HHDR .
  methods _BUILD_ALV_TREE_EXCLUDING
    returning
      value(RT_EXCLUDING) type UI_FUNCTIONS .
  methods _BUILD_ALV_TREE_COMMENT .
  methods _CREATE_ALV_TREE
    raising
      ZCX_FLIFM_EXCEPTION .
  methods _CREATE_FIELDCAT
    returning
      value(RT_FCAT) type LVC_T_FCAT
    raising
      ZCX_FLIFM_EXCEPTION .
  methods _CHANGE_TOOLBAR .
  methods _ADD_BUTTON
    importing
      !IO_TOOLBAR type ref to CL_GUI_TOOLBAR
      !IV_FCODE type UI_FUNC
      !IV_ICON type C
      !IV_TEXT type TEXT40 optional
      !IV_TYPE type TB_BTYPE
      value(IV_QUICKINFO) type ICONQUICK optional .
  methods _REGISTER_EVENTS .
  methods _UPDATE_TREE
    importing
      !IT_EXP type LVC_T_NKEY .
  methods _CALL_ACCOUNT_BALANCE
    importing
      !IV_GJAHR type GJAHR
      !IS_DATA type ANY .
  methods _ON_TREE_FUNCTION_SELECTED
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods _ON_TREE_NODE_DOUBLE_CLICK
    for event NODE_DOUBLE_CLICK of CL_GUI_ALV_TREE
    importing
      !NODE_KEY
      !SENDER .
  methods _ON_TREE_ITEM_DOUBLE_CLICK
    for event ITEM_DOUBLE_CLICK of CL_GUI_ALV_TREE
    importing
      !FIELDNAME
      !NODE_KEY .
ENDCLASS.



CLASS ZCL_FLIFM_GUI_FSV_ALV_TREE IMPLEMENTATION.


  METHOD zif_flifm_gui~render.

    DATA: lt_exp TYPE lvc_t_nkey.

    zif_flifm_gui~clean_up( ).

    CLEAR ms_route_data.
    ms_route_data-menu    = iv_menu.
    ms_route_data-company = iv_company.
    ms_route_data-action  = iv_action.

    IF mo_process IS BOUND.
      CLEAR mo_process.
    ENDIF.

    mo_process = zcl_flifm_process_factory=>create( iv_menu    = ms_route_data-menu
                                                    iv_company = ms_route_data-company
                                                    iv_action  = ms_route_data-action ).

    mo_process->build_data( ).

    _initialize_alv_tree( ).

    lt_exp = mo_process->build_tree( mo_alv_tree ).

    _change_toolbar( ).

    _register_events( ).

    _update_tree( lt_exp ).

  ENDMETHOD.


  METHOD _add_button.


    IF iv_quickinfo IS INITIAL.
      iv_quickinfo = iv_text.
    ENDIF.

    io_toolbar->add_button( fcode     = iv_fcode
                            icon      = iv_icon
                            butn_type = iv_type
                            text      = iv_text
                            quickinfo = iv_quickinfo ).


  ENDMETHOD.


  METHOD _build_alv_tree_comment.


    DATA: ls_line TYPE slis_listheader.

    CLEAR: ls_line, mt_list_commentary.
    ls_line-typ  = 'S'.
    ls_line-info = 'IFM'.                                   "#EC NOTEXT
    APPEND ls_line TO mt_list_commentary.


  ENDMETHOD.


  METHOD _build_alv_tree_excluding.


    FIELD-SYMBOLS: <ls_excluding> LIKE LINE OF rt_excluding.

    APPEND INITIAL LINE TO rt_excluding ASSIGNING <ls_excluding>.
    <ls_excluding> = mo_alv_tree->mc_fc_calculate.


  ENDMETHOD.


  METHOD _build_alv_tree_header.


    rs_alv_tree_header-heading = zcl_flifm_i18n=>get_instance( )->item_account.
    rs_alv_tree_header-tooltip = zcl_flifm_i18n=>get_instance( )->item_account.
    rs_alv_tree_header-width = 60.
    rs_alv_tree_header-width_pix = space.


  ENDMETHOD.


  METHOD _call_account_balance.


    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company,
          lt_waers   TYPE TABLE OF waers WITH DEFAULT KEY,
          ls_waers   LIKE LINE OF lt_waers.

    DATA: ls_sender   TYPE rstirec,              " Sender report
          ls_receiver TYPE rstirec,              " Receiver report
          lt_sel      TYPE STANDARD TABLE OF rstisel WITH DEFAULT KEY, " Selection values
          ls_sel      LIKE LINE OF lt_sel,
          lt_fields   TYPE STANDARD TABLE OF rstifields WITH DEFAULT KEY, " Selection fields
          ls_fields   LIKE LINE OF lt_fields.

    FIELD-SYMBOLS: <lv_hkont> TYPE any.

*// Define: Selection fields values
    DEFINE _define_field.
      ls_sel-field    = &1.
      ls_sel-sign     = 'I'.
      ls_sel-option   = 'EQ'.
      ls_sel-low      = &2.
      APPEND ls_sel TO lt_sel.
    END-OF-DEFINITION.

    ASSIGN COMPONENT 'HKONT' OF STRUCTURE is_data TO <lv_hkont>.

    CHECK <lv_hkont> IS NOT INITIAL.

    IF ms_route_data-company IS NOT INITIAL.
      _define_field 'RBUKRS' ms_route_data-company.
    ELSE.
      " If the currency of the company code is the same, the inquiry is possible.
      lt_company = mo_fetch->get_company( ).
      LOOP AT lt_company INTO ls_company.
        _define_field 'RBUKRS' ls_company-bukrs.
        ls_waers = ls_company-waers.
        COLLECT ls_waers INTO lt_waers.
      ENDLOOP.

      IF lines( lt_waers ) > 1.
        MESSAGE s022(fagl_account_balance) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

    ENDIF.

    CLEAR ls_sender. " This report is sender
    ls_sender-rtool = 'RT'.
    ls_sender-rappl = space.
    ls_sender-rsubc = space.
    ls_sender-ronam = sy-repid.

    "Receiver (from S_ALR_87013019 définition)
    CLEAR ls_receiver. " Define receiver (recherche); RONAM syntax depends on RTOOL/RAPPL
    ls_receiver-rtool = 'RT'.
    ls_receiver-ronam = 'FAGL_ACCOUNT_BALANCE'.

    CLEAR: ls_fields.
    ls_fields-field    = 'RLDNR'.
    ls_fields-rollname = 'FAGL_RLDNR'.
    ls_fields-domname  = 'RLDNR'.
    ls_fields-memoryid = 'GLN_FLEX'.
    APPEND ls_fields TO lt_fields.

    CLEAR ls_fields.
    ls_fields-field    = 'RYEAR'.
    ls_fields-rollname = 'GJAHR'.
    ls_fields-domname  = 'GJAHR'.
    ls_fields-memoryid = 'GJR'.
    APPEND ls_fields TO lt_fields.

    CLEAR ls_fields.
    ls_fields-field    = 'RACCT'.
    ls_fields-rollname = 'RACCT'.
    ls_fields-domname  = 'SAKNR'.
    ls_fields-memoryid = 'ACC'.
    APPEND ls_fields TO lt_fields.

    CLEAR ls_fields.
    ls_fields-field    = 'RBUKRS'.
    ls_fields-rollname = 'BUKRS'.
    ls_fields-domname  = 'BUKRS'.
    ls_fields-memoryid = 'BUK'.
    APPEND ls_fields TO lt_fields.

    CLEAR ls_fields.
    ls_fields-field    = 'RBUSA'.
    ls_fields-rollname = 'GSBER'.
    ls_fields-domname  = 'GSBER'.
    ls_fields-memoryid = 'GSB'.
    APPEND ls_fields TO lt_fields.

    "FKBER
    CLEAR ls_fields.
    ls_fields-field    = 'RFAREA'.
    ls_fields-rollname = 'FKBER'.
    ls_fields-domname  = 'FKBER'.
    ls_fields-memoryid = 'FBE'.
    APPEND ls_fields TO lt_fields.

    CLEAR ls_sel.
    _define_field 'RLDNR' '0L'.
    _define_field 'RYEAR' iv_gjahr.
    _define_field 'RACCT' <lv_hkont>.

    CALL FUNCTION 'RSTI_APPL_STACK_INITIALIZE'
      EXPORTING
        e_tool = ls_receiver-rtool
        e_onam = ls_receiver-ronam.

    CALL FUNCTION 'RSTI_SELECTION_EXPORT'
      TABLES
        it_sel    = lt_sel
        it_fields = lt_fields.

    SUBMIT fagl_account_balance AND RETURN.


  ENDMETHOD.


  METHOD _change_toolbar.


    DATA: lo_toolbar TYPE REF TO cl_gui_toolbar.

    CALL METHOD mo_alv_tree->get_toolbar_object
      IMPORTING
        er_toolbar = lo_toolbar.

    CHECK NOT lo_toolbar IS INITIAL.

    "Add standard button to toolbar(for Expand tree)
    _add_button( io_toolbar = : lo_toolbar
                 iv_fcode = ''
                 iv_icon  = ''
                 iv_type  = cntb_btype_sep ),
                 lo_toolbar
                 iv_fcode = zif_flifm_definitions=>c_action-expd_all
                 iv_icon  = icon_expand_all
                 iv_type  = cntb_btype_button
                 iv_text  = |{ zcl_flifm_i18n=>get_instance( )->expand_all }| ),
                 lo_toolbar
                 iv_fcode = zif_flifm_definitions=>c_action-cole_all
                 iv_icon  = icon_collapse_all
                 iv_type  = cntb_btype_button
                 iv_text  = |{ zcl_flifm_i18n=>get_instance( )->collapse_all }| ),
                 lo_toolbar
                 iv_fcode = ''
                 iv_icon  = ''
                 iv_type  = cntb_btype_sep ),
                 lo_toolbar
                 iv_fcode = zif_flifm_definitions=>c_action-fsv_list_popup
                 iv_icon  = icon_list
                 iv_type  = cntb_btype_button
                 iv_text  = |{ zcl_flifm_i18n=>get_instance( )->fsv_popup }| ),
                 lo_toolbar
                 iv_fcode = zif_flifm_definitions=>c_action-fsv_download_excel
                 iv_icon  = icon_export
                 iv_type  = cntb_btype_button
                 iv_text  = |{ zcl_flifm_i18n=>get_instance( )->fsv_excel }| ).

    SET HANDLER _on_tree_function_selected FOR lo_toolbar.


  ENDMETHOD.


  METHOD _create_alv_tree.

*// Create tree control
    CREATE OBJECT mo_alv_tree
      EXPORTING
        parent                      = mo_splitter_right->get_container( row = 1 column = 1 )
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_html_header              = 'X' "Delete Header
        no_toolbar                  = ' '
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_sy_msg( ).
    ENDIF.

  ENDMETHOD.


  METHOD _create_fieldcat.


    DATA: lt_dd03t TYPE zcl_flifm_fetch=>tyt_dd03t,
          ls_dd03t LIKE LINE OF lt_dd03t.

    FIELD-SYMBOLS: <ls_fieldcat> TYPE lvc_s_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_bypassing_buffer     = 'X'
        i_structure_name       = mo_process->gv_strname
      CHANGING
        ct_fieldcat            = rt_fcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_t100( iv_msgno = 001
                                       iv_msgv1 = 'LVC_FIELDCATALOG_MERGE'
                                       iv_msgv2 = |{ sy-subrc }| ).
    ENDIF.

    lt_dd03t = mo_fetch->get_dd03t( ).

    LOOP AT rt_fcat ASSIGNING <ls_fieldcat>.

      IF <ls_fieldcat>-fieldname CP 'AMT*'.
        <ls_fieldcat>-just = 'R'.
        IF <ls_fieldcat>-fieldname+3(2) > zcl_flifm_selection=>get_to_period( ).
          <ls_fieldcat>-no_out = abap_true.
        ENDIF.
      ENDIF.

      READ TABLE lt_dd03t INTO ls_dd03t
        WITH KEY tabname = mo_process->gv_strname
                 fieldname = <ls_fieldcat>-fieldname
       BINARY SEARCH.

      IF sy-subrc = 0.
        <ls_fieldcat>-coltext = ls_dd03t-ddtext.
        <ls_fieldcat>-just = 'R'.
      ENDIF.

      IF zcl_flifm_utils=>split_menu( ms_route_data-menu ) = zif_flifm_definitions=>c_flifm_menu_type-bs
        AND <ls_fieldcat>-fieldname = 'AMT00'.
        <ls_fieldcat>-coltext = zcl_flifm_i18n=>get_instance( )->balance_cf.
      ELSEIF zcl_flifm_utils=>split_menu( ms_route_data-menu ) = zif_flifm_definitions=>c_flifm_menu_type-pl
        AND <ls_fieldcat>-fieldname = 'AMT00'.
        <ls_fieldcat>-coltext = zcl_flifm_i18n=>get_instance( )->ytd.
      ENDIF.

    ENDLOOP.

    APPEND INITIAL LINE TO rt_fcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'HKONT'.
    <ls_fieldcat>-ref_table = 'SKA1'.
    <ls_fieldcat>-ref_field = 'SAKNR'.
    <ls_fieldcat>-no_out    = abap_true.

    APPEND INITIAL LINE TO rt_fcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'TXT50'.
    <ls_fieldcat>-ref_table = 'SKAT'.
    <ls_fieldcat>-ref_field = 'TXT50'.
    <ls_fieldcat>-outputlen = 20.
    <ls_fieldcat>-col_pos   = '0'.
    <ls_fieldcat>-no_out    = abap_true.

    APPEND INITIAL LINE TO rt_fcat ASSIGNING <ls_fieldcat>.
    <ls_fieldcat>-fieldname = 'WAERS'.
    <ls_fieldcat>-ref_table = 'T001'.
    <ls_fieldcat>-ref_field = 'WAERS'.
    <ls_fieldcat>-no_out    = abap_true.


  ENDMETHOD.


  METHOD _initialize_alv_tree.


    DATA: lt_fcat TYPE lvc_t_fcat.

    DATA: ls_disvariant TYPE disvariant.

    DATA: ls_alv_tree_header TYPE treev_hhdr.

    DATA: lt_excluding TYPE ui_functions.

    FIELD-SYMBOLS : <lt_outtab> TYPE STANDARD TABLE.

    _create_alv_tree( ).
    ls_alv_tree_header = _build_alv_tree_header( ).
    lt_excluding = _build_alv_tree_excluding( ).

    ASSIGN mo_process->gr_data_display->* TO <lt_outtab>.

    lt_fcat = _create_fieldcat( ).

    ls_disvariant-report   = sy-repid.
    ls_disvariant-handle   = 'IFM'.
    ls_disvariant-username = sy-uname.

    mo_alv_tree->set_table_for_first_display(
      EXPORTING
        is_variant          = ls_disvariant
        is_hierarchy_header = ls_alv_tree_header
        i_save               = 'A'
       it_toolbar_excluding = lt_excluding
      CHANGING
        it_outtab            = <lt_outtab> "must be empty table
        it_fieldcatalog      = lt_fcat ).


  ENDMETHOD.


  METHOD _on_tree_function_selected.


    DATA lx_exception TYPE REF TO zcx_flifm_exception.

    DATA lt_expd_keys TYPE lvc_t_nkey.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    TRY.
        CASE fcode.
          WHEN zif_flifm_definitions=>c_action-expd_all.
            DATA: lv_node_key TYPE tv_nodekey.
            DATA: lt_children TYPE lvc_t_nkey.

            lv_node_key = 1.

            mo_alv_tree->get_children( EXPORTING i_node_key = lv_node_key
                                       IMPORTING et_children = lt_children ).

            CHECK lt_children IS NOT INITIAL.

            mo_alv_tree->expand_node( i_node_key = lv_node_key
                                      i_expand_subtree = 'X' ).

            mo_alv_tree->column_optimize( i_start_column = '&Hierarchy' ).

          WHEN zif_flifm_definitions=>c_action-cole_all.
            mo_alv_tree->collapse_all_nodes( ).

          WHEN zif_flifm_definitions=>c_action-fsv_list_popup.

            CALL METHOD mo_alv_tree->get_expanded_nodes
              CHANGING
                ct_expanded_nodes = lt_expd_keys.

            ASSIGN mo_process->gr_popup_data->* TO <lt_data>.

            CHECK <lt_data> IS ASSIGNED.
            zcl_flifm_popups=>get_instance( )->show_fsv_list_popup(
                                                EXPORTING iv_menu        = ms_route_data-menu
                                                          it_nkey        = lt_expd_keys
                                                          it_popup_table = <lt_data> ).

          WHEN zif_flifm_definitions=>c_action-fsv_download_excel.

            CALL METHOD mo_alv_tree->get_expanded_nodes
              CHANGING
                ct_expanded_nodes = lt_expd_keys.

            ASSIGN mo_process->gr_popup_data->* TO <lt_data>.

            CHECK <lt_data> IS ASSIGNED.
            zcl_flifm_excel=>get_instance( )->download_fsv_list_excel(
                                                EXPORTING
                                                  iv_menu        = ms_route_data-menu
                                                  it_nkey        = lt_expd_keys
                                                  it_popup_table = <lt_data> ).

        ENDCASE.

      CATCH zcx_flifm_exception INTO lx_exception.
        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.


  ENDMETHOD.


  METHOD _on_tree_item_double_click.


    DATA: lt_t030 TYPE zif_flifm_definitions=>tyt_t030.

    DATA: lv_gjahr TYPE gjahr.

    FIELD-SYMBOLS : <lt_display> TYPE STANDARD TABLE,
                    <ls_display> TYPE any,
                    <lv_name>    TYPE any.

    lt_t030 =  zcl_flifm_fetch=>get_instance( )->get_t030( ).

    ASSIGN mo_process->gr_data_display->* TO <lt_display>.

    READ TABLE <lt_display> ASSIGNING <ls_display> INDEX node_key.

    ASSIGN COMPONENT 'NAME' OF STRUCTURE <ls_display> TO <lv_name>.

    CASE <lv_name>.
      WHEN zif_flifm_definitions=>c_add_line_type-np_tot OR
           zif_flifm_definitions=>c_add_line_type-np_tot_mc.

        DATA: lv_komok TYPE komok,
              lv_konts TYPE saknr.

        FIELD-SYMBOLS: <lv_text> TYPE any.

        ASSIGN COMPONENT 'TEXT' OF STRUCTURE <ls_display> TO <lv_text>.

        SPLIT <lv_text> AT space INTO: lv_komok lv_konts.

        IF lv_komok IS INITIAL.
          MESSAGE s013.
          RETURN.
        ELSE.
          READ TABLE lt_t030 TRANSPORTING NO FIELDS WITH KEY komok = lv_komok.

          IF sy-subrc <> 0.
            MESSAGE s013.
            RETURN.
          ENDIF.
        ENDIF.

        IF <lv_name> = zif_flifm_definitions=>c_add_line_type-np_tot.
          ASSIGN mo_process->gr_popup_data_np->* TO <lt_display>.
        ELSE.
          ASSIGN mo_process->gr_popup_data_np_ytd->* TO <lt_display>.
        ENDIF.

        IF zcl_flifm_utils=>split_menu( ms_route_data-menu ) = zif_flifm_definitions=>c_flifm_menu_type-bs AND fieldname = 'AMT00'.

          IF ms_route_data-menu = zif_flifm_definitions=>c_flifm_menu_type-bs_ry_trend.
            lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
          ELSE.
            lv_gjahr = zcl_flifm_selection=>get_cmp_gjahr( ).
          ENDIF.

          _call_account_balance( iv_gjahr = lv_gjahr
                               is_data  = <ls_display> ).

        ELSE.

          zcl_flifm_popups=>get_instance( )->show_net_profit_list_popup( EXPORTING
                                                                          iv_menu        = ms_route_data-menu
                                                                          iv_name        = <lv_name>
                                                                          iv_fieldname   = fieldname
                                                                          iv_komok       = lv_komok
                                                                          it_popup_table = <lt_display> ).
        ENDIF.

      WHEN OTHERS.

        IF ms_route_data-menu = zif_flifm_definitions=>c_flifm_menu_type-pl_cy_trend OR
           ms_route_data-menu = zif_flifm_definitions=>c_flifm_menu_type-bs_cy_trend OR
           fieldname = 'CYSPAMT' OR fieldname = 'TCYAMT'.
          lv_gjahr = zcl_flifm_selection=>get_cmp_gjahr( ).
        ELSE.
          lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
        ENDIF.

        _call_account_balance( iv_gjahr = lv_gjahr
                               is_data  = <ls_display> ).

    ENDCASE.


  ENDMETHOD.


  METHOD _on_tree_node_double_click.


    DATA: lt_children TYPE lvc_t_nkey.

*// First check if the node is a leaf, i.e. can not be expanded
    CALL METHOD sender->get_children
      EXPORTING
        i_node_key  = node_key
      IMPORTING
        et_children = lt_children.

    IF NOT lt_children IS INITIAL.

      CALL METHOD sender->expand_node
        EXPORTING
          i_node_key    = node_key
          i_level_count = 2.
    ENDIF.


  ENDMETHOD.


  METHOD _register_events.


    DATA: lt_event TYPE cntl_simple_events,
          ls_event TYPE cntl_simple_event.

*// Register additional events for your own purposes:
    CALL METHOD mo_alv_tree->get_registered_events
      IMPORTING
        events = lt_event.

    ls_event-eventid  = cl_gui_column_tree=>eventid_node_double_click.
    APPEND ls_event TO lt_event.

    CLEAR ls_event.
    ls_event-eventid  = cl_gui_column_tree=>eventid_item_double_click.
    ls_event-appl_event = ' '.
    APPEND ls_event TO lt_event.

    ls_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
    APPEND ls_event TO lt_event.

*// Register event
    CALL METHOD mo_alv_tree->set_registered_events
      EXPORTING
        events                    = lt_event
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

*// assign event handlers in the application class to each desired event
    SET HANDLER _on_tree_node_double_click FOR mo_alv_tree.
    SET HANDLER _on_tree_item_double_click FOR mo_alv_tree.


  ENDMETHOD.


  METHOD _update_tree.

    CALL METHOD mo_alv_tree->frontend_update.

    CALL METHOD mo_alv_tree->expand_nodes
      EXPORTING
        it_node_key             = it_exp
      EXCEPTIONS
        failed                  = 1
        cntl_system_error       = 2
        error_in_node_key_table = 3
        dp_error                = 4
        node_not_found          = 5
        OTHERS                  = 6.

    CALL METHOD mo_alv_tree->column_optimize
      EXPORTING
        i_start_column = '&Hierarchy'.

  ENDMETHOD.
ENDCLASS.
