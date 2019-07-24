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
CLASS zcl_flifm_selection DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tyr_bukrs TYPE RANGE OF t001-bukrs .

    CLASS-METHODS initialize
      EXPORTING
        !ev_gjahr     TYPE gjahr
        !ev_monat     TYPE monat
        !ev_cmp_gjahr TYPE gjahr .
    CLASS-METHODS validate_input
      IMPORTING
        !iv_monat TYPE monat
      RAISING
        zcx_flifm_exception .
    CLASS-METHODS set_data
      IMPORTING
        !ir_bukrs     TYPE tyr_bukrs OPTIONAL
        !iv_gjahr     TYPE gjahr OPTIONAL
        !iv_monat     TYPE monat OPTIONAL
        !iv_cmp_gjahr TYPE gjahr OPTIONAL
        !iv_level     TYPE seu_level OPTIONAL
        !iv_ktopl     TYPE ktopl OPTIONAL
        !iv_versn     TYPE versn_011 OPTIONAL
        !iv_waers     TYPE waers OPTIONAL
        !iv_to_period TYPE zflifme_to_period OPTIONAL
        !iv_zero      TYPE c OPTIONAL
        !iv_parallel  TYPE c OPTIONAL .
    CLASS-METHODS get_bukrs
      RETURNING
        VALUE(rr_bukrs) TYPE tyr_bukrs .
    CLASS-METHODS get_versn
      RETURNING
        VALUE(rv_versn) TYPE versn_011 .
    CLASS-METHODS get_parallel
      RETURNING
        VALUE(rv_parallel) TYPE char1 .
    CLASS-METHODS get_cmp_gjahr
      RETURNING
        VALUE(rv_cmp_gjahr) TYPE gjahr .
    CLASS-METHODS get_gjahr
      RETURNING
        VALUE(rv_gjahr) TYPE gjahr .
    CLASS-METHODS get_monat
      RETURNING
        VALUE(rv_monat) TYPE monat .
    CLASS-METHODS get_level
      RETURNING
        VALUE(rv_level) TYPE seu_level .
    CLASS-METHODS get_ktopl
      RETURNING
        VALUE(rv_ktopl) TYPE ktopl .
    CLASS-METHODS get_waers
      RETURNING
        VALUE(rv_waers) TYPE waers .
    CLASS-METHODS get_to_period
      RETURNING
        VALUE(rv_to_period) TYPE zflifme_to_period .
    CLASS-METHODS get_para_zero
      RETURNING
        VALUE(rv_zero) TYPE char1 .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mr_bukrs TYPE tyr_bukrs .
    CLASS-DATA mv_gjahr TYPE gjahr .
    CLASS-DATA mv_monat TYPE monat .
    CLASS-DATA mv_cmp_gjahr TYPE gjahr .
    CLASS-DATA mv_level TYPE seu_level .
    CLASS-DATA mv_versn TYPE versn_011 .
    CLASS-DATA mv_waers TYPE waers .
    CLASS-DATA mv_to_period TYPE zflifme_to_period .
    CLASS-DATA mv_zero TYPE c .
    CLASS-DATA mv_parallel TYPE c .
    CLASS-DATA mv_ktopl TYPE ktopl .

    METHODS _validate_company
      RAISING
        zcx_flifm_exception .
ENDCLASS.



CLASS ZCL_FLIFM_SELECTION IMPLEMENTATION.


  METHOD get_bukrs.

    rr_bukrs = mr_bukrs.

  ENDMETHOD.


  METHOD get_cmp_gjahr.

    rv_cmp_gjahr = mv_cmp_gjahr.

  ENDMETHOD.


  METHOD get_gjahr.

    rv_gjahr = mv_gjahr.

  ENDMETHOD.


  METHOD get_ktopl.

    rv_ktopl = mv_ktopl.

  ENDMETHOD.


  METHOD get_level.

    rv_level = mv_level.

  ENDMETHOD.


  METHOD get_monat.

    rv_monat = mv_monat.

  ENDMETHOD.


  METHOD get_parallel.

    rv_parallel = mv_parallel.

  ENDMETHOD.


  METHOD get_para_zero.

    rv_zero = mv_zero.

  ENDMETHOD.


  METHOD get_to_period.

    rv_to_period = mv_to_period.

  ENDMETHOD.


  METHOD get_versn.

    rv_versn = mv_versn.

  ENDMETHOD.


  METHOD get_waers.

    rv_waers = mv_waers.

  ENDMETHOD.


  METHOD initialize.

    ev_gjahr     = sy-datum(4).
    ev_monat     = sy-datum+4(2).
    ev_cmp_gjahr = ev_gjahr - 1.

  ENDMETHOD.


  METHOD set_data.


    IF ir_bukrs IS NOT INITIAL.
      mr_bukrs    = ir_bukrs.
    ENDIF.

    IF iv_gjahr IS NOT INITIAL.
      mv_gjahr    = iv_gjahr.
    ENDIF.

    IF iv_monat IS NOT INITIAL.
      mv_monat    = iv_monat.
    ENDIF.

    IF iv_cmp_gjahr IS NOT INITIAL.
      mv_cmp_gjahr = iv_cmp_gjahr.
    ENDIF.

    IF iv_level IS NOT INITIAL.
      mv_level    = iv_level.
    ENDIF.

    IF iv_zero IS NOT INITIAL.
      mv_zero     = iv_zero.
    ENDIF.

    IF iv_versn IS NOT INITIAL.
      mv_versn    = iv_versn.
    ENDIF.

    IF iv_waers IS NOT INITIAL.
      mv_waers    = iv_waers.
    ENDIF.

    IF iv_to_period IS NOT INITIAL.
      mv_to_period    = iv_to_period.
    ENDIF.

    IF iv_parallel IS NOT INITIAL.
      mv_parallel = iv_parallel.
    ENDIF.

    IF iv_ktopl IS NOT INITIAL.
      mv_ktopl    = iv_ktopl.
    ENDIF.


  ENDMETHOD.


  METHOD validate_input.

    IF iv_monat = '00'.
      MESSAGE e014.
    ENDIF.

    IF iv_monat > '16'.
      MESSAGE e014.
    ENDIF.

  ENDMETHOD.


  METHOD _validate_company.

  ENDMETHOD.
ENDCLASS.
