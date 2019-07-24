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
CLASS zcl_flifm_popups DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_flifm_popups .

    ALIASES show_fsv_list_popup
      FOR zif_flifm_popups~show_fsv_list_popup .
    ALIASES show_net_profit_list_popup
      FOR zif_flifm_popups~show_net_profit_list_popup .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ri_popups) TYPE REF TO zif_flifm_popups .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF tys_objs,
        cont_obj TYPE REF TO cl_gui_dialogbox_container,
        grid_obj TYPE REF TO cl_gui_alv_grid,
      END OF tys_objs .
  types:
    tyt_objs TYPE STANDARD TABLE OF tys_objs WITH DEFAULT KEY .

  class-data MO_POPUPS type ref to ZIF_FLIFM_POPUPS .
  class-data MT_OBJS type TYT_OBJS .

  methods _CREATE_DIALOG_CONTAINER_SHOW
    importing
      !IV_POPUP_TITLE type STRING
    changing
      !CT_FIELDCAT type LVC_T_FCAT
      !CT_OUTTAB type STANDARD TABLE
    raising
      ZCX_FLIFM_EXCEPTION .
  methods _ON_DIALOGBOX_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
  methods _BUILD_ALV_LAYOUT
    returning
      value(RS_LAYOUT) type LVC_S_LAYO .
  methods _CLEAR_POPUP
    changing
      !CS_OBJS type TYS_OBJS
    raising
      ZCX_FLIFM_EXCEPTION .
ENDCLASS.



CLASS ZCL_FLIFM_POPUPS IMPLEMENTATION.


  METHOD get_instance.


    IF mo_popups IS NOT BOUND.
      CREATE OBJECT mo_popups TYPE zcl_flifm_popups.
    ENDIF.

    ri_popups = mo_popups.


  ENDMETHOD.


  METHOD zif_flifm_popups~show_fsv_list_popup.

    DATA: lx_exception TYPE REF TO zcx_flifm_exception,
          lv_error     TYPE string.

    DATA: lv_title       TYPE string,
          lv_popup_title TYPE string.

    DATA: lv_menu_desc     TYPE zif_flifm_definitions=>ty_flifm_menu_desc,
          lv_sub_menu_desc TYPE zif_flifm_definitions=>ty_flifm_menu_desc.

    DATA: lt_data TYPE REF TO data.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    CREATE DATA lt_data LIKE it_popup_table.
    ASSIGN lt_data->* TO <lt_table>.

    zcl_flifm_event_data_provider=>filtering_data( EXPORTING it_nkey = it_nkey
                                                             it_table = it_popup_table
                                                   IMPORTING et_table = <lt_table> ).

*// Create fieldcatalog
    DATA lt_fieldcat TYPE lvc_t_fcat.

    FIELD-SYMBOLS: <ls_fieldcat> TYPE lvc_s_fcat.

    lt_fieldcat = zcl_flifm_event_data_provider=>create_event_fieldcat( it_popup_table ).

    IF iv_menu IS NOT INITIAL.

      CASE zcl_flifm_utils=>split_menu( iv_menu ).
        WHEN zif_flifm_definitions=>c_flifm_menu_type-bs.

          READ TABLE lt_fieldcat ASSIGNING <ls_fieldcat> WITH KEY fieldname = 'AMT00'.
          IF sy-subrc = 0.
            <ls_fieldcat>-coltext = zcl_flifm_i18n=>get_instance( )->balance_cf.
          ENDIF.

        WHEN zif_flifm_definitions=>c_flifm_menu_type-pl.

          READ TABLE lt_fieldcat ASSIGNING <ls_fieldcat> WITH KEY fieldname = 'AMT00'.
          IF sy-subrc = 0.
            <ls_fieldcat>-coltext = zcl_flifm_i18n=>get_instance( )->ytd.
          ENDIF.

      ENDCASE.

    ENDIF.

    lv_menu_desc = zcl_flifm_i18n=>get_instance( )->get_menu_description( zcl_flifm_utils=>split_menu( iv_menu ) ).

    lv_sub_menu_desc = zcl_flifm_i18n=>get_instance( )->get_menu_description( iv_menu ).

    CONCATENATE lv_menu_desc zif_flifm_definitions=>c_slash lv_sub_menu_desc INTO lv_popup_title SEPARATED BY space.

    TRY.
        _create_dialog_container_show( EXPORTING
                                        iv_popup_title = |{ lv_popup_title }|
                                       CHANGING
                                        ct_fieldcat = lt_fieldcat
                                        ct_outtab   = <lt_table> ).
      CATCH zcx_flifm_exception INTO lx_exception.

        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.


  ENDMETHOD.


  METHOD zif_flifm_popups~show_net_profit_list_popup.

    DATA: lx_exception TYPE REF TO zcx_flifm_exception.

    DATA: lv_title       TYPE string,
          lv_popup_title TYPE string.

    DATA: lv_menu_desc     TYPE zif_flifm_definitions=>ty_flifm_menu_desc,
          lv_sub_menu_desc TYPE zif_flifm_definitions=>ty_flifm_menu_desc.

    DATA: lt_data TYPE REF TO data.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    CREATE DATA lt_data LIKE it_popup_table.
    ASSIGN lt_data->* TO <lt_table>.

    <lt_table> = it_popup_table.

    DELETE <lt_table> WHERE ('KOMOK <> IV_KOMOK').

*// Create fieldcatalog
    DATA lt_fieldcat TYPE lvc_t_fcat.

    lt_fieldcat = zcl_flifm_event_data_provider=>create_event_fieldcat( iv_fieldname = iv_fieldname
                                                                        it_table     = it_popup_table ).

    lv_menu_desc = zcl_flifm_i18n=>get_instance( )->get_menu_description( zcl_flifm_utils=>split_menu( iv_menu ) ).

    lv_sub_menu_desc = zcl_flifm_i18n=>get_instance( )->get_menu_description( iv_menu ).

    CLEAR lv_title.
    CASE iv_name.
      WHEN zif_flifm_definitions=>c_add_line_type-np_tot.
        lv_title = zcl_flifm_i18n=>get_instance( )->calc_net_profit.
        CONCATENATE lv_menu_desc zif_flifm_definitions=>c_slash lv_sub_menu_desc
          zif_flifm_definitions=>c_slash lv_title INTO lv_popup_title SEPARATED BY space.

      WHEN zif_flifm_definitions=>c_add_line_type-np_tot_mc.
        lv_title = zcl_flifm_i18n=>get_instance( )->calc_net_profit_ytd.
        CONCATENATE lv_menu_desc zif_flifm_definitions=>c_slash lv_sub_menu_desc
          zif_flifm_definitions=>c_slash lv_title INTO lv_popup_title SEPARATED BY space.

    ENDCASE.

    TRY.
        _create_dialog_container_show( EXPORTING
                                        iv_popup_title = |{ lv_popup_title }|
                                       CHANGING
                                        ct_fieldcat = lt_fieldcat
                                        ct_outtab   = <lt_table> ).

      CATCH zcx_flifm_exception INTO lx_exception.

        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD _build_alv_layout.


    rs_layout-sel_mode   = 'D'.
    rs_layout-detailinit = 'X'.
    rs_layout-no_toolbar = ' '.
    rs_layout-no_rowmark = 'X'.
    rs_layout-info_fname = 'ROW_COLOR'.


  ENDMETHOD.


  METHOD _clear_popup.


    cs_objs-grid_obj->finalize( ).
    cs_objs-grid_obj->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_sy_msg( ).
    ENDIF.

    CLEAR cs_objs-grid_obj.

    cs_objs-cont_obj->finalize( ).
    cs_objs-cont_obj->free(
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_sy_msg( ).
    ENDIF.

    CLEAR cs_objs-grid_obj.


  ENDMETHOD.


  METHOD _create_dialog_container_show.


*// Create container, alv.
    DATA: lv_error TYPE string.

    DATA ls_objs LIKE LINE OF mt_objs.
    DATA lo_dialog_container TYPE REF TO cl_gui_dialogbox_container.
    DATA lo_alv_grid_popup   TYPE REF TO cl_gui_alv_grid.

    CREATE OBJECT lo_dialog_container
      EXPORTING
*       parent                      =
        width                       = 1300
        height                      = 300
*       style                       =
*       repid                       =
*       dynnr                       =
        lifetime                    = cl_gui_control=>lifetime_imode
        top                         = 50
        left                        = 60
        caption                     = |{ iv_popup_title }|
*       no_autodef_progid_dynnr     =
*       metric                      = 0
*       name                        =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        event_already_registered    = 6
        error_regist_event          = 7
        OTHERS                      = 8.

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_sy_msg( ).
    ENDIF.

    ls_objs-cont_obj = lo_dialog_container.

*// Set event
    SET HANDLER _on_dialogbox_close FOR lo_dialog_container.

*// Create cl_gui_alv_grid
    CREATE OBJECT lo_alv_grid_popup
      EXPORTING
        i_shellstyle      = cl_gui_control=>ws_child
        i_lifetime        = cl_gui_alv_grid=>lifetime_imode
        i_parent          = lo_dialog_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_sy_msg( ).
    ENDIF.

    ls_objs-grid_obj = lo_alv_grid_popup.
    APPEND ls_objs TO mt_objs.

*// Build layout
    DATA: ls_layout TYPE lvc_s_layo.

    ls_layout = _build_alv_layout( ).

*// Display
    lo_alv_grid_popup->set_table_for_first_display(
      EXPORTING
        is_layout                     = ls_layout
      CHANGING
        it_outtab                     = ct_outtab
        it_fieldcatalog               = ct_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).

    IF sy-subrc <> 0.
      zcx_flifm_exception=>raise_sy_msg( ).
    ENDIF.

    CALL METHOD lo_alv_grid_popup->set_focus
      EXPORTING
        control = lo_alv_grid_popup.


  ENDMETHOD.


  METHOD _on_dialogbox_close.

    DATA lx_exception TYPE REF TO zcx_flifm_exception.

    DATA: ls_objs LIKE LINE OF mt_objs.
    FIELD-SYMBOLS: <ls_objs> LIKE LINE OF mt_objs.

    READ TABLE mt_objs ASSIGNING <ls_objs> WITH KEY cont_obj = sender.

    TRY.
        _clear_popup( CHANGING cs_objs = <ls_objs> ).
      CATCH zcx_flifm_exception INTO lx_exception.
        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    DELETE mt_objs WHERE cont_obj = sender.

  ENDMETHOD.
ENDCLASS.
