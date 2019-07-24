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
CLASS zcl_flifm_process_factory DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create
      IMPORTING
        !iv_menu          TYPE zif_flifm_definitions=>ty_flifm_menu_type
        !iv_company       TYPE zif_flifm_definitions=>ty_flifm_company_type
        !iv_action        TYPE zif_flifm_definitions=>ty_action
      RETURNING
        VALUE(ro_process) TYPE REF TO zcl_flifm_process
      RAISING
        zcx_flifm_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FLIFM_PROCESS_FACTORY IMPLEMENTATION.


  METHOD create.

    CASE zcl_flifm_utils=>split_menu( iv_menu ).
      WHEN zif_flifm_definitions=>c_flifm_menu_type-tb.
        CREATE OBJECT ro_process TYPE zcl_flifm_process_tb
          EXPORTING
            iv_menu    = iv_menu
            iv_company = iv_company
            iv_action  = iv_action.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-pl.
        CREATE OBJECT ro_process TYPE zcl_flifm_process_pl
          EXPORTING
            iv_menu    = iv_menu
            iv_company = iv_company
            iv_action  = iv_action.
      WHEN zif_flifm_definitions=>c_flifm_menu_type-bs.
        CREATE OBJECT ro_process TYPE zcl_flifm_process_bs
          EXPORTING
            iv_menu    = iv_menu
            iv_company = iv_company
            iv_action  = iv_action.

      WHEN OTHERS.
        zcx_flifm_exception=>raise_t100( iv_msgno = 003 iv_msgv1 = |{ iv_menu }| ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
