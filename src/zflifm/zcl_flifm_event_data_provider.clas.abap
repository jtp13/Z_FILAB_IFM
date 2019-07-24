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

CLASS zcl_flifm_event_data_provider DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS filtering_data
      IMPORTING
        !it_nkey        TYPE lvc_t_nkey
        !it_table       TYPE STANDARD TABLE
      EXPORTING
        VALUE(et_table) TYPE STANDARD TABLE .
    CLASS-METHODS create_event_fieldcat
      IMPORTING
        !iv_fieldname            TYPE lvc_fname OPTIONAL
        !it_table                TYPE STANDARD TABLE
      RETURNING
        VALUE(rt_fieldcat_table) TYPE lvc_t_fcat .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_FLIFM_EVENT_DATA_PROVIDER IMPLEMENTATION.


  METHOD create_event_fieldcat.


    DATA mo_table_descr TYPE REF TO cl_abap_tabledescr.
    DATA lo_struct_descr TYPE REF TO cl_abap_structdescr.
    DATA lv_relative_name TYPE tabname.

    mo_table_descr ?= cl_abap_tabledescr=>describe_by_data( it_table ).
    lo_struct_descr ?= mo_table_descr->get_table_line_type( ).
    lv_relative_name = lo_struct_descr->get_relative_name( ).

    DATA: lt_dd03t TYPE STANDARD TABLE OF dd03t WITH DEFAULT KEY,
          ls_dd03t LIKE LINE OF lt_dd03t.

    FIELD-SYMBOLS: <ls_fieldcat> TYPE lvc_s_fcat.

    lt_dd03t = zcl_flifm_fetch=>get_instance( )->get_dd03t( ).

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_bypassing_buffer = 'X'
        i_structure_name   = lv_relative_name
      CHANGING
        ct_fieldcat        = rt_fieldcat_table.

    LOOP AT rt_fieldcat_table ASSIGNING <ls_fieldcat>.

      CASE <ls_fieldcat>-fieldname.
        WHEN 'ID'.
          <ls_fieldcat>-key        = abap_true.
          <ls_fieldcat>-fix_column = abap_true.
          <ls_fieldcat>-no_out     = abap_true.
        WHEN 'TLEVEL'.
          <ls_fieldcat>-key        = abap_true.
          <ls_fieldcat>-fix_column = abap_true.
        WHEN 'TEXT'.
          <ls_fieldcat>-key        = abap_true.
          <ls_fieldcat>-fix_column = abap_true.
          <ls_fieldcat>-col_opt    = abap_true.
          <ls_fieldcat>-scrtext_l  = zcl_flifm_i18n=>get_instance( )->item_account.
          <ls_fieldcat>-scrtext_m  = zcl_flifm_i18n=>get_instance( )->item_account.
          <ls_fieldcat>-scrtext_s  = zcl_flifm_i18n=>get_instance( )->item_account.
          <ls_fieldcat>-coltext    = zcl_flifm_i18n=>get_instance( )->item_account.
        WHEN 'TXT50'.
          <ls_fieldcat>-col_opt = abap_true.
        WHEN 'NKEY'.
          <ls_fieldcat>-no_out = abap_true.
        WHEN 'ROW_COLOR'.
          <ls_fieldcat>-no_out = abap_true.
        WHEN 'SORT_KEY'.
          <ls_fieldcat>-no_out = abap_true.
        WHEN OTHERS.
          IF <ls_fieldcat>-scrtext_l IS INITIAL.
            READ TABLE lt_dd03t INTO ls_dd03t WITH KEY tabname   = lv_relative_name
                                                       fieldname = <ls_fieldcat>-fieldname BINARY SEARCH.
            IF sy-subrc = 0.
              <ls_fieldcat>-just = 'R'.
              <ls_fieldcat>-scrtext_l = ls_dd03t-ddtext.
              <ls_fieldcat>-scrtext_m = ls_dd03t-ddtext.
              <ls_fieldcat>-scrtext_s = ls_dd03t-ddtext.
              <ls_fieldcat>-coltext   = ls_dd03t-ddtext.
              <ls_fieldcat>-outputlen  = 18.
            ENDIF.

            CASE <ls_fieldcat>-fieldname.
              WHEN 'KOMOK'.
                <ls_fieldcat>-just = 'C'.
              WHEN 'KONTS'.
                <ls_fieldcat>-just = 'L'.
            ENDCASE.
          ENDIF.
      ENDCASE.

      IF <ls_fieldcat>-fieldname CP 'AMT*' AND iv_fieldname CP 'AMT*'.
        IF <ls_fieldcat>-fieldname <> iv_fieldname.
          <ls_fieldcat>-no_out = abap_true.
        ENDIF.
      ENDIF.

      IF iv_fieldname IS INITIAL AND <ls_fieldcat>-fieldname CP 'AMT*'.
        IF <ls_fieldcat>-fieldname+3(2) > zcl_flifm_selection=>get_to_period( ).
          <ls_fieldcat>-no_out = abap_true.
        ENDIF.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.


  METHOD filtering_data.


    DATA: ls_nkey LIKE LINE OF it_nkey,
          lr_keys TYPE RANGE OF lvc_nkey,
          lrs_key LIKE LINE OF lr_keys.

    et_table = it_table.

*// Show only what is displayed on the screen.
    lrs_key(3) = 'IEQ'.
    lrs_key-low = ' '.
    APPEND lrs_key TO lr_keys.

    LOOP AT it_nkey INTO ls_nkey.
      lrs_key(3) = 'IEQ'.
      lrs_key-low = ls_nkey.
      APPEND lrs_key TO lr_keys.
    ENDLOOP.

    DELETE et_table WHERE ('NKEY NOT IN LR_KEYS').


  ENDMETHOD.
ENDCLASS.
