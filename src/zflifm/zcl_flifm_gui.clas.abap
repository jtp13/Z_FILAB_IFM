class ZCL_FLIFM_GUI definition
  public
  create public .

public section.

  interfaces ZIF_FLIFM_GUI .

  aliases RENDER
    for ZIF_FLIFM_GUI~RENDER .

  methods CONSTRUCTOR
    importing
      !IV_ROUTE type ZIF_FLIFM_DEFINITIONS=>TY_ROUTE
      !IO_PARENT type ref to CL_GUI_SPLITTER_CONTAINER .
  class-methods GET_GUI
    importing
      !IV_ROUTE type ZIF_FLIFM_DEFINITIONS=>TY_ROUTE
      !IO_PARENT type ref to CL_GUI_CONTAINER
    returning
      value(RO_GUI) type ref to ZCL_FLIFM_GUI .
protected section.

  aliases MO_ALV_GRID
    for ZIF_FLIFM_GUI~MO_ALV_GRID .
  aliases MO_ALV_TREE
    for ZIF_FLIFM_GUI~MO_ALV_TREE .
  aliases MO_FETCH
    for ZIF_FLIFM_GUI~MO_FETCH .
  aliases MO_PROCESS
    for ZIF_FLIFM_GUI~MO_PROCESS .
  aliases MO_SPLITTER_RIGHT
    for ZIF_FLIFM_GUI~MO_SPLITTER_RIGHT .
  aliases MS_ROUTE_DATA
    for ZIF_FLIFM_GUI~MS_ROUTE_DATA .
PRIVATE SECTION.

  TYPES:
    BEGIN OF tys_gui,
      gui TYPE REF TO zcl_flifm_gui,
    END OF tys_gui .
  TYPES:
    tyt_gui TYPE STANDARD TABLE OF tys_gui WITH EMPTY KEY .

  DATA route TYPE zif_flifm_definitions=>ty_route .
  CLASS-DATA mt_gui TYPE tyt_gui .
ENDCLASS.



CLASS ZCL_FLIFM_GUI IMPLEMENTATION.


  METHOD constructor.

    route = iv_route.

    mo_fetch = zcl_flifm_fetch=>get_instance( ).

    mo_splitter_right = io_parent.

  ENDMETHOD.


  METHOD get_gui.

    DATA ls_gui TYPE tys_gui.

    DATA lv_object_type TYPE seoclsname.

    DATA: lo_splitter_container TYPE REF TO cl_gui_splitter_container.

    lo_splitter_container ?= io_parent.

    READ TABLE mt_gui INTO ls_gui WITH KEY gui->route = iv_route.

    IF sy-subrc <> 0.
      CONCATENATE zif_flifm_definitions=>c_flifm_class_name iv_route INTO lv_object_type SEPARATED BY '_'.

      TRANSLATE lv_object_type TO UPPER CASE.

      TRY.
          CREATE OBJECT ls_gui-gui TYPE (lv_object_type)
            EXPORTING
              iv_route = iv_route
              io_parent = lo_splitter_container.

        CATCH cx_sy_dyn_call_illegal_class.
          zcx_flifm_exception=>raise_t100( iv_msgno = 002 iv_msgv1 = |'{ iv_route }'| ).
      ENDTRY.

      APPEND ls_gui TO mt_gui.

    ENDIF.

    ro_gui ?= ls_gui-gui.

  ENDMETHOD.


  METHOD zif_flifm_gui~clean_up.

    IF mo_alv_tree IS BOUND.

      mo_alv_tree->finalize( ).
      mo_alv_tree->free( ).

      CLEAR: mo_alv_tree.

    ENDIF.

    IF mo_alv_grid IS BOUND.

      mo_alv_grid->finalize( ).
      mo_alv_grid->free( ).

      CLEAR: mo_alv_grid.

    ENDIF.

  ENDMETHOD.


  method ZIF_FLIFM_GUI~RENDER.
  endmethod.
ENDCLASS.
