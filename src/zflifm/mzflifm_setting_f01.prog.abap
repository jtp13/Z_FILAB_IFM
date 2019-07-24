*&---------------------------------------------------------------------*
*&  Include           MZFLIFM_SETTING_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  SELECT SINGLE *
    FROM zflifmt_setting.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*& Form SET_INITIAL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_initial_data .

*// Routes
  DATA: lt_routes TYPE TABLE OF zflifmt_routes.

  FIELD-SYMBOLS: <ls_routes> LIKE LINE OF lt_routes.

  SELECT COUNT(*) UP TO 1 ROWS
    FROM zflifmt_routes.

  IF sy-subrc <> 0.
    APPEND INITIAL LINE TO lt_routes ASSIGNING <ls_routes>.
    <ls_routes>-route = zif_flifm_definitions=>c_routes-gui_fsv_alv_tree.
    <ls_routes>-text = zif_flifm_definitions=>c_routes_desc-gui_fsv_alv_tree.

    MODIFY zflifmt_routes FROM TABLE lt_routes.
  ENDIF.

*// Menu type / Menu Description / Menu
  DATA: lt_menu_type  TYPE TABLE OF zflifmt_mtype,
        lt_menu_typet TYPE TABLE OF zflifmt_mtypet,
        lt_menu       TYPE TABLE OF zflifmt_menu.

  DATA ls_flifm_menu_desc TYPE zif_flifm_definitions=>tys_flifm_menu_desc_i18n.

  DATA lv_menu(100) TYPE c.
  DATA lv_route(100) TYPE c.
  DATA lv_text TYPE text30.
  DATA lv_cnt TYPE seu_id.

  FIELD-SYMBOLS: <ls_menu_type>  LIKE LINE OF lt_menu_type,
                 <ls_menu_typet> LIKE LINE OF lt_menu_typet,
                 <ls_menu>       LIKE LINE OF lt_menu.

  FIELD-SYMBOLS : <lv_menu> TYPE any.
  FIELD-SYMBOLS : <lv_text> TYPE any.
  FIELD-SYMBOLS : <lv_route> TYPE any.

  SELECT COUNT(*) UP TO 1 ROWS
    FROM zflifmt_menu.

  IF sy-subrc <> 0.

    ls_flifm_menu_desc = zcl_flifm_i18n=>get_instance( )->get_initial_menu_desc( ).

    DO.

      UNASSIGN: <lv_menu>, <lv_text>.
      lv_cnt = lv_cnt + 1.
      CONCATENATE 'ZIF_FLIFM_DEFINITIONS=>C_FLIFM_MENU-M' lv_cnt  INTO lv_menu.
      ASSIGN  (lv_menu)  TO  <lv_menu>.
      IF <lv_menu> IS ASSIGNED.

        APPEND INITIAL LINE TO lt_menu_type ASSIGNING <ls_menu_type>.
        <ls_menu_type>-menu_type = <lv_menu>.

        CONCATENATE 'LS_FLIFM_MENU_DESC-' <lv_menu> INTO lv_text.
        ASSIGN  (lv_text)  TO  <lv_text>.

        IF <lv_text> IS ASSIGNED.

          APPEND INITIAL LINE TO lt_menu_typet ASSIGNING <ls_menu_typet>.
          <ls_menu_typet>-spras = sy-langu.
          <ls_menu_typet>-menu_type = <lv_menu>.
          <ls_menu_typet>-descr = <lv_text>.

        ENDIF.

        APPEND INITIAL LINE TO lt_menu ASSIGNING <ls_menu>.
        <ls_menu>-id = lv_cnt.
        <ls_menu>-menu_type = <lv_menu>.

        IF <lv_menu> = zcl_flifm_utils=>split_menu( <lv_menu> ).
          CONCATENATE 'ZIF_FLIFM_DEFINITIONS=>C_MENU_ROUTES-' <lv_menu> INTO lv_route.
          ASSIGN  (lv_route)  TO  <lv_route>.
          IF <lv_route> IS ASSIGNED.
            <ls_menu>-route = <lv_route>.
          ENDIF.
          <ls_menu>-expand = abap_true.
        ELSE.
          <ls_menu>-parent = zcl_flifm_utils=>split_menu( <lv_menu> ).
        ENDIF.

      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

    MODIFY zflifmt_mtype FROM TABLE lt_menu_type.
    MODIFY zflifmt_mtypet FROM TABLE lt_menu_typet.
    MODIFY zflifmt_menu FROM TABLE lt_menu.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_data .

  zflifmt_setting-ernam = sy-uname.
  zflifmt_setting-erdat = sy-datum.
  zflifmt_setting-erzet = sy-uzeit.

  MODIFY zflifmt_setting FROM zflifmt_setting.

  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000 WITH 'Settings saved'.
    CLEAR : gv_init.
  ELSE.
    ROLLBACK WORK.
    MESSAGE e000 WITH 'Save failed'.
  ENDIF.

ENDFORM.
