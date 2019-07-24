*&---------------------------------------------------------------------*
*& Include          ZFLIFM_IFM_MAIN_CLASS
*&---------------------------------------------------------------------*

CLASS zcl_flifm_main DEFINITION
  INHERITING FROM zcl_flifm_screen
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        !iv_dynnr TYPE sy-dynnr
      RAISING
        zcx_flifm_exception.

    METHODS pbo REDEFINITION.
    METHODS pai REDEFINITION.
    METHODS render REDEFINITION.
    METHODS set_title REDEFINITION.

    EVENTS:
      user_command
        EXPORTING
          VALUE(iv_ucomm) TYPE sy-ucomm.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_docking_container_left TYPE REF TO cl_gui_docking_container .
    DATA mo_docking_container_right TYPE REF TO cl_gui_docking_container .
    DATA mo_splitter_left TYPE REF TO cl_gui_splitter_container .
    DATA mo_splitter_right TYPE REF TO cl_gui_splitter_container .
    DATA mo_simple_tree_menu TYPE REF TO cl_gui_simple_tree .
    DATA mo_simple_tree_company TYPE REF TO cl_gui_simple_tree .

    DATA mt_menu TYPE TABLE OF zflifmt_menu.

    DATA mv_route TYPE zif_flifm_definitions=>ty_route.

    METHODS _create_container_left .
    METHODS _create_container_right .
    METHODS _create_simple_tree_menu .
    METHODS _append_node_tree_menu
      CHANGING
        !ct_node TYPE zif_flifm_definitions=>tyt_node .
    METHODS _expand_node_tree_menu.
    METHODS _create_simple_tree_company .
    METHODS _on_user_command
          FOR EVENT user_command OF zcl_flifm_main
      IMPORTING
          !iv_ucomm .
    METHODS _on_menu_node_double_click
          FOR EVENT node_double_click OF cl_gui_simple_tree
      IMPORTING
          !node_key .
    METHODS _on_company_node_double_click
          FOR EVENT node_double_click OF cl_gui_simple_tree
      IMPORTING
          !node_key .
ENDCLASS.

CLASS zcl_flifm_main IMPLEMENTATION.

  METHOD constructor.

    super->constructor( iv_dynnr ).

    DATA ls_menu LIKE LINE OF mt_menu.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_menu
      FROM zflifmt_menu.

    READ TABLE mt_menu INTO ls_menu WITH KEY id = 1.

    IF sy-subrc = 0 AND ls_menu-route IS NOT INITIAL.
      ms_main_data-route = mv_route = ls_menu-route.
    ELSE.
      zcx_flifm_exception=>raise_t100( iv_msgno = 015 ).
    ENDIF.

    READ TABLE mt_menu INTO ls_menu WITH KEY id = 2.

    IF sy-subrc = 0 AND ls_menu-menu_type IS NOT INITIAL.
      ms_main_data-menu = ls_menu-menu_type.
    ELSE.
      zcx_flifm_exception=>raise_t100( iv_msgno = 015 ).
    ENDIF.

    ms_main_data-re_render = abap_true.

    SET HANDLER _on_user_command FOR me.

    CALL SCREEN iv_dynnr.

  ENDMETHOD.


  METHOD pai.

    DATA: lv_rc TYPE i.

    CALL METHOD cl_gui_cfw=>dispatch
      IMPORTING
        return_code = lv_rc.

    IF lv_rc <> cl_gui_cfw=>rc_noevent.
      EXIT.
    ENDIF.

    RAISE EVENT user_command EXPORTING iv_ucomm = iv_fcode.

  ENDMETHOD.

  METHOD pbo.

    DATA lt_ex TYPE status_excl_fcode_tt.
    DATA ls_title TYPE tys_title.

    IF ms_main_data-re_render = abap_true.

      set_exclude(
        EXPORTING
          iv_fcode = ''
        CHANGING ct_ex = lt_ex ).
      ls_title = set_title( ).

      SET PF-STATUS 'STATUS_0100' EXCLUDING lt_ex .
      SET TITLEBAR  'TITLE_0100'  WITH ls_title-t1 ls_title-t2 ls_title-t3.

    ENDIF.

    render( ).
    gui_router( mo_splitter_right ).

  ENDMETHOD.

  METHOD render.

    _create_container_left( ).
    _create_container_right( ).

  ENDMETHOD.

  METHOD set_title.


    DATA: lv_repid       TYPE sy-repid,
          lv_butxt       TYPE butxt,
          lv_title       TYPE string,
          lv_gjahr       TYPE gjahr,
          lv_monat       TYPE monat,
          lv_cgjahr      TYPE gjahr,
          lv_inter_waers TYPE waers,
          lv_fcurr       TYPE fcurr_curr,
          lv_tcurr       TYPE tcurr_curr,
          lv_delimiter   TYPE c LENGTH 2.

    DATA: lt_textpool TYPE STANDARD TABLE OF textpool,
          ls_textpool LIKE LINE OF lt_textpool.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company,
          lv_lines   TYPE i.

    lt_company = mo_fetch->get_company( ).

    lv_lines = lines( lt_company ).

    IF lv_lines EQ 1.
      LOOP AT lt_company INTO ls_company. ENDLOOP.
      lv_butxt = ls_company-butxt.
    ELSEIF ms_main_data-company IS NOT INITIAL.
      READ TABLE lt_company INTO ls_company WITH TABLE KEY bukrs = ms_main_data-company.
      lv_butxt = ls_company-butxt.
    ELSE.
      lv_title = zcl_flifm_i18n=>get_instance( )->flifm_title.
    ENDIF.

    CONCATENATE lv_butxt lv_title INTO rs_title-t1 SEPARATED BY space.

    lv_gjahr = zcl_flifm_selection=>get_gjahr( ).
    lv_monat = zcl_flifm_selection=>get_monat( ).
    lv_cgjahr = zcl_flifm_selection=>get_cmp_gjahr( ).
    CONCATENATE lv_gjahr zif_flifm_definitions=>c_slash lv_monat zif_flifm_definitions=>c_hyphen lv_cgjahr INTO rs_title-t2  SEPARATED BY space.

    lv_inter_waers = mo_fetch->get_inter_waers( ).

    CASE ms_main_data-action.
      WHEN zif_flifm_definitions=>c_action-ic.
        IF ms_main_data-company IS INITIAL.
          lv_fcurr = lv_inter_waers.
        ELSE.
          lv_delimiter = zif_flifm_definitions=>c_arrow.
          lv_fcurr = ls_company-waers.
          lv_tcurr = lv_inter_waers.
        ENDIF.
      WHEN zif_flifm_definitions=>c_action-lc.
        lv_fcurr = ls_company-waers.
      WHEN OTHERS.
        lv_fcurr = lv_inter_waers.
    ENDCASE.

    CONCATENATE lv_fcurr lv_delimiter lv_tcurr INTO rs_title-t3 SEPARATED BY space.


  ENDMETHOD.

  METHOD _append_node_tree_menu.

    TYPES: BEGIN OF ty_menu,
             id        TYPE seu_id,
             menu_type TYPE zflifme_menu_type,
             parent    TYPE zflifme_menu_type,
             descr     TYPE text30,
           END OF ty_menu.

    DATA: ls_node  TYPE mtreesnode,
*          lv_menu  TYPE zflifme_menu_type,
          lv_spras TYPE spras.

    DATA: lt_menu TYPE STANDARD TABLE OF ty_menu WITH DEFAULT KEY,
          ls_menu LIKE LINE OF lt_menu.

    SELECT COUNT( * ) UP TO 1 ROWS
      FROM zflifmt_mtypet
      WHERE spras = sy-langu.

    IF sy-subrc = 0.
      lv_spras = sy-langu.
    ELSE.
      lv_spras = zif_flifm_definitions=>c_default_langu.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_menu
     FROM zflifmt_menu AS a INNER JOIN zflifmt_mtypet AS b
      ON a~menu_type = b~menu_type
    WHERE b~spras = lv_spras
     ORDER BY id.

    LOOP AT lt_menu INTO ls_menu.

      CLEAR: ls_node.

      IF ls_menu-parent IS INITIAL.
*        lv_menu          = ls_menu-menu_type.
        ls_node-node_key = ls_menu-menu_type.
        ls_node-isfolder = 'X'.
        ls_node-text     = ls_menu-descr.
      ELSE.
        ls_node-node_key = ls_menu-menu_type.
        ls_node-relatkey = ls_menu-parent.
        ls_node-text     = ls_menu-descr.

        IF ls_node-node_key = ms_main_data-menu.
          ls_node-n_image = zif_flifm_definitions=>c_tree_menu_icon_display.
        ELSE.
          ls_node-n_image = zif_flifm_definitions=>c_tree_menu_icon.
        ENDIF.
        ls_node-exp_image = zif_flifm_definitions=>c_tree_menu_icon.
      ENDIF.

      APPEND ls_node TO ct_node.

    ENDLOOP.

  ENDMETHOD.

  METHOD _expand_node_tree_menu.
    DATA: ls_menu LIKE LINE OF mt_menu.

    LOOP AT mt_menu INTO ls_menu WHERE expand = 'X'.

      mo_simple_tree_menu->expand_node( node_key = ls_menu-menu_type ).

    ENDLOOP.

  ENDMETHOD.

  METHOD _create_container_left.


    IF mo_docking_container_left IS NOT BOUND.

*// Left Tree Menu
      CREATE OBJECT mo_docking_container_left
        EXPORTING
          dynnr = sy-dynnr
          repid = sy-repid
          side  = mo_docking_container_left->dock_at_left
          ratio = 15.

      CREATE OBJECT mo_splitter_left
        EXPORTING
          parent  = mo_docking_container_left
          rows    = 2
          columns = 1.

      mo_splitter_left->set_border( border = cl_gui_cfw=>false ).
      mo_splitter_left->set_column_mode( mode = mo_splitter_left->mode_absolute ).

      _create_simple_tree_menu( ). "Menu Tree
      _create_simple_tree_company( ). "Company Code List

    ENDIF.


  ENDMETHOD.

  METHOD _create_container_right.


    IF mo_docking_container_right IS NOT BOUND.

*// Right Tree ALV
      CREATE OBJECT mo_docking_container_right
        EXPORTING
          dynnr     = sy-dynnr
          repid     = sy-repid
          side      = mo_docking_container_right->dock_at_left
          style     = cl_gui_control=>ws_child
          extension = 3000.

      CREATE OBJECT mo_splitter_right
        EXPORTING
          parent  = mo_docking_container_right
          rows    = 1
          columns = 1.

      mo_splitter_right->set_border( border = cl_gui_cfw=>false ).
      mo_splitter_right->set_column_mode( mode = mo_splitter_right->mode_absolute ).

    ENDIF.


  ENDMETHOD.

  METHOD _create_simple_tree_company.


    DATA: lt_node TYPE zif_flifm_definitions=>tyt_node,
          ls_node TYPE mtreesnode.

    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company.

    CREATE OBJECT mo_simple_tree_company
      EXPORTING
        lifetime            = mo_simple_tree_menu->lifetime_imode
        parent              = mo_splitter_left->get_container( row = 2 column = 1 )
        node_selection_mode = mo_simple_tree_menu->node_sel_mode_single.

    mo_splitter_left->set_row_height( id = 2 height = 80 ).

    lt_company = mo_fetch->get_company( ).

    LOOP AT lt_company INTO ls_company.

      CLEAR ls_node.
      ls_node-node_key+0(4) = ls_company-bukrs.

      CONCATENATE ls_company-bukrs zif_flifm_definitions=>c_hyphen ls_company-butxt
        INTO ls_node-text SEPARATED BY space.

      ls_node-n_image = zif_flifm_definitions=>c_tree_company_icon.
      ls_node-exp_image = zif_flifm_definitions=>c_tree_company_icon.

      APPEND ls_node TO lt_node.

    ENDLOOP.

    mo_simple_tree_company->add_nodes(
      table_structure_name = 'MTREESNODE'
      node_table           = lt_node ).

*// Regist Event
    DATA: lt_events TYPE cntl_simple_events,
          ls_events TYPE cntl_simple_event.

    CLEAR lt_events.
    ls_events-eventid = cl_gui_column_tree=>eventid_node_double_click.
    ls_events-appl_event = 'X'.                    " Excute Pai
    APPEND ls_events TO lt_events.

    CALL METHOD mo_simple_tree_company->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

    SET HANDLER _on_company_node_double_click FOR mo_simple_tree_company.


  ENDMETHOD.

  METHOD _create_simple_tree_menu.


    DATA: lt_node TYPE zif_flifm_definitions=>tyt_node.

    CREATE OBJECT mo_simple_tree_menu
      EXPORTING
        lifetime            = mo_simple_tree_menu->lifetime_imode
        parent              = mo_splitter_left->get_container( row = 1 column = 1 )
        node_selection_mode = mo_simple_tree_menu->node_sel_mode_single.

    mo_splitter_left->set_row_height( id = 1 height = 50 ).

*// Add nodes
    _append_node_tree_menu( CHANGING ct_node = lt_node ).

    CALL METHOD mo_simple_tree_menu->add_nodes
      EXPORTING
        table_structure_name = 'MTREESNODE'
        node_table           = lt_node.

*// Regist Event
    DATA: lt_events TYPE cntl_simple_events,
          ls_events TYPE cntl_simple_event.

    CLEAR lt_events.
    ls_events-eventid = cl_gui_column_tree=>eventid_node_double_click.
    ls_events-appl_event = 'X'.                    " Excute Pai
    APPEND ls_events TO lt_events.

    CALL METHOD mo_simple_tree_menu->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

    SET HANDLER _on_menu_node_double_click FOR mo_simple_tree_menu.

*// Select Menu & Expand Node
    mo_simple_tree_menu->set_selected_node( node_key = ms_main_data-menu ).
    _expand_node_tree_menu( ).
*    mo_simple_tree_menu->expand_node( node_key = zif_flifm_definitions=>c_flifm_menu_type-pl ).
*    mo_simple_tree_menu->expand_node( node_key = zif_flifm_definitions=>c_flifm_menu_type-bs ).

  ENDMETHOD.

  METHOD _on_company_node_double_click.


    DATA: lv_p_company TYPE tv_nodekey,
          lv_image     TYPE tv_image.

    lv_p_company = ms_main_data-company.

    IF ms_main_data-prev_route IS INITIAL.
      ms_main_data-route = mv_route. " zif_flifm_definitions=>c_routes-gui_fsv_alv_tree.
    ELSE.
      ms_main_data-route = ms_main_data-prev_route.
    ENDIF.

    ms_main_data-re_render = abap_true.
    ms_main_data-company   = node_key.
    ms_main_data-action    = zif_flifm_definitions=>c_action-lc.


    IF NOT lv_p_company IS INITIAL.
      lv_image = zif_flifm_definitions=>c_tree_company_icon.

      mo_simple_tree_company->node_set_n_image( node_key = lv_p_company
                                                n_image  = lv_image ).
    ENDIF.

    lv_image = zif_flifm_definitions=>c_tree_company_icon_display.

    mo_simple_tree_company->node_set_n_image( node_key = node_key
                                              n_image  = lv_image ).


  ENDMETHOD.

  METHOD _on_menu_node_double_click.


    DATA: lv_p_menu TYPE tv_nodekey,
          lv_image  TYPE tv_image.

    DATA ls_menu LIKE LINE OF mt_menu.

    IF node_key+2(1) IS INITIAL.
      MESSAGE e012.
      RETURN.
    ENDIF.

*    IF node_key+2 = zif_flifm_definitions=>c_flifm_addon_menu.
*    ELSE.
*      ms_main_data-route = zif_flifm_definitions=>c_routes-gui_fsv_alv_tree.
*    ENDIF.

    READ TABLE mt_menu INTO ls_menu WITH KEY menu_type = zcl_flifm_utils=>split_menu( node_key ).

    ms_main_data-route = ls_menu-route.

    lv_p_menu = ms_main_data-menu.

    ms_main_data-re_render = abap_true.
    ms_main_data-menu      = node_key.
    ms_main_data-action    = zif_flifm_definitions=>c_action-ic.

    lv_image = zif_flifm_definitions=>c_tree_menu_icon.

    mo_simple_tree_menu->node_set_n_image( node_key = lv_p_menu
                                           n_image  = lv_image ).

    lv_image = zif_flifm_definitions=>c_tree_menu_icon_display.

    mo_simple_tree_menu->node_set_n_image( node_key = node_key
                                           n_image  = lv_image ).

*// Company Menu
    IF NOT ms_main_data-company IS INITIAL.
      lv_image = zif_flifm_definitions=>c_tree_company_icon.

      mo_simple_tree_company->node_set_n_image( node_key = ms_main_data-company
                                                n_image  = lv_image ).

      CLEAR ms_main_data-company.
    ENDIF.


  ENDMETHOD.

  METHOD _on_user_command.

    DATA lx_exception TYPE REF TO zcx_flifm_exception.

    DATA ls_menu LIKE LINE OF mt_menu.

    TRY.

        CASE iv_ucomm.
          WHEN 'BACK' OR 'EXIT' OR 'CANC'.

            SET SCREEN 0. LEAVE SCREEN.

          WHEN zif_flifm_definitions=>c_action-ic OR zif_flifm_definitions=>c_action-lc.

            IF iv_ucomm = zif_flifm_definitions=>c_action-lc AND ms_main_data-company IS INITIAL.
              zcx_flifm_exception=>raise_msg( 'Select Company' ).
            ENDIF.

            ms_main_data-re_render = abap_true.

            READ TABLE mt_menu INTO ls_menu WITH KEY menu_type = zcl_flifm_utils=>split_menu( ms_main_data-menu ).

            ms_main_data-route = ls_menu-route. "zif_flifm_definitions=>c_routes-gui_fsv_alv_tree.
            ms_main_data-action    = iv_ucomm.

          WHEN zif_flifm_definitions=>c_action-go_filab OR
               zif_flifm_definitions=>c_action-go_help.

            ms_main_data-re_render = abap_false.
            ms_main_data-route = zif_flifm_definitions=>c_routes-filab_services.
            ms_main_data-action = iv_ucomm.

          WHEN OTHERS.

        ENDCASE.

      CATCH zcx_flifm_exception INTO lx_exception.
        ms_main_data-re_render = abap_false.
        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
