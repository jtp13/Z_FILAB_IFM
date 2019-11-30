*&---------------------------------------------------------------------*
*& Include          ZFLIFM_IFMR_SCREEN_CLASS
*&---------------------------------------------------------------------*
CLASS zcl_flifm_screen DEFINITION
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF tys_main_data,
        re_render  TYPE abap_bool,
        route      TYPE zif_flifm_definitions=>ty_route,
        prev_route TYPE zif_flifm_definitions=>ty_route,
        menu       TYPE zif_flifm_definitions=>ty_flifm_menu_type,
        company    TYPE zif_flifm_definitions=>ty_flifm_company_type,
        action     TYPE zif_flifm_definitions=>ty_action,
      END OF tys_main_data .
    TYPES:
      BEGIN OF tys_title,
        t1      TYPE repti,
        t2(100) TYPE c,
        t3(100) TYPE c,
      END OF tys_title .

    METHODS constructor
      IMPORTING
        !iv_dynnr TYPE sy-dynnr
      EXCEPTIONS
        zcx_flifm_exception.

    METHODS pbo
        ABSTRACT .

    METHODS pai
          ABSTRACT
      IMPORTING
        !iv_fcode TYPE sy-ucomm .

    METHODS render
        ABSTRACT .

    METHODS set_title
          ABSTRACT
      RETURNING
        VALUE(rs_title) TYPE tys_title .

  PROTECTED SECTION.

    DATA ms_main_data TYPE tys_main_data .
    DATA mo_fetch TYPE REF TO zcl_flifm_fetch .
    DATA mo_router TYPE REF TO zif_flifm_router .

    METHODS get_exclude
      RETURNING
        VALUE(rt_ex) TYPE status_excl_fcode_tt .

    METHODS set_exclude
      IMPORTING
        !iv_fcode TYPE fcode
      CHANGING
        !ct_ex    TYPE status_excl_fcode_tt .

    METHODS gui_router
      IMPORTING
        !io_parent TYPE REF TO cl_gui_container OPTIONAL .

  PRIVATE SECTION.
    DATA _mv_screen TYPE sy-dynnr .

ENDCLASS.

CLASS zcl_flifm_screen IMPLEMENTATION.

  METHOD constructor.
    _mv_screen = iv_dynnr.

    IF mo_router IS INITIAL.
      CREATE OBJECT mo_router TYPE zcl_flifm_router.
    ENDIF.

    IF mo_fetch IS INITIAL.
      mo_fetch =  zcl_flifm_fetch=>get_instance( ).
    ENDIF.

  ENDMETHOD.

  METHOD get_exclude.

*// Change later...
    SELECT COUNT( * ) UP TO 1 ROWS
      FROM seoclass
      WHERE clsname = 'ZCL_FLIFM_AWS_QUICKSIGHT'.

    IF sy-subrc <> 0.
      set_exclude( EXPORTING
                    iv_fcode = 'AWS_QS'
                   CHANGING
                     ct_ex = rt_ex ).
    ENDIF.

  ENDMETHOD.

  METHOD set_exclude.

    APPEND iv_fcode TO ct_ex.

  ENDMETHOD.

  METHOD gui_router.

    DATA: lx_exception TYPE REF TO zcx_flifm_exception.

    CHECK ms_main_data-route IS NOT INITIAL.

    TRY.

        mo_router->on_routing(
          iv_route   = ms_main_data-route
          iv_menu    = ms_main_data-menu
          iv_company = ms_main_data-company
          iv_action  = ms_main_data-action
          io_parent  = io_parent
        ).

        ms_main_data-prev_route = ms_main_data-route.

        CLEAR: ms_main_data-route, ms_main_data-action.
        ms_main_data-re_render = abap_false.

      CATCH zcx_flifm_exception INTO lx_exception.
        ms_main_data-re_render = abap_false.
        MESSAGE lx_exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
