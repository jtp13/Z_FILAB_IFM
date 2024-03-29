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
CLASS zcl_flifm_router DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_flifm_router .

    ALIASES on_routing
      FOR zif_flifm_router~on_routing .
  PROTECTED SECTION.
private section.

  methods _ROUTES
    importing
      !IS_ROUTE_DATA type ZIF_FLIFM_DEFINITIONS=>TYS_ROUTE_DATA
    raising
      ZCX_FLIFM_EXCEPTION .
  methods _FILAB_SERVICE
    importing
      !IS_ROUTE_DATA type ZIF_FLIFM_DEFINITIONS=>TYS_ROUTE_DATA
    raising
      ZCX_FLIFM_EXCEPTION .
ENDCLASS.



CLASS ZCL_FLIFM_ROUTER IMPLEMENTATION.


  METHOD on_routing.

    DATA: ls_route_data TYPE zif_flifm_definitions=>tys_route_data.

    ls_route_data-route   = iv_route.
    ls_route_data-menu    = iv_menu.
    ls_route_data-company = iv_company.
    ls_route_data-action  = iv_action.
    ls_route_data-parent  = io_parent.

    CASE ls_route_data-route.
      WHEN zif_flifm_definitions=>c_routes-filab_services.

        _filab_service(
          EXPORTING
            is_route_data = ls_route_data ).

      WHEN OTHERS.

        SELECT COUNT(*) UP TO 1 ROWS
          FROM zflifmt_routes
          WHERE route = ls_route_data-route.

        IF sy-subrc = 0.
          _routes(
            EXPORTING
              is_route_data = ls_route_data ).
        ELSE.
          zcx_flifm_exception=>raise_t100( iv_msgno = 016 iv_msgv1 = |'{ ls_route_data-route }'| ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD _filab_service.

    CASE is_route_data-action.
      WHEN zif_flifm_definitions=>c_action-go_filab.
        zcl_flifm_services_filab=>open_filab_homepage( ).
      WHEN zif_flifm_definitions=>c_action-go_help.
        zcl_flifm_services_filab=>open_filab_help( ).
      WHEN OTHERS.
        zcx_flifm_exception=>raise_t100( iv_msgno = 004 iv_msgv1 = |{ is_route_data-action }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD _routes.

    zcl_flifm_gui=>get_gui( EXPORTING
                              iv_route = is_route_data-route
                              io_parent = is_route_data-parent )->render( iv_menu    = is_route_data-menu
                                                                          iv_company = is_route_data-company
                                                                          iv_action  = is_route_data-action ).

  ENDMETHOD.
ENDCLASS.
