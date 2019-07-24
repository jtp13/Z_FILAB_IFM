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
CLASS zcl_flifm_excel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_flifm_excel .

    ALIASES download_fsv_list_excel
      FOR zif_flifm_excel~download_fsv_list_excel .

    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_excel) TYPE REF TO zif_flifm_excel .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_excel TYPE REF TO zcl_flifm_excel .
    DATA mo_excel TYPE ole2_object .
    DATA mo_workbooks TYPE ole2_object .
    DATA mo_workbook TYPE ole2_object .
    DATA mo_worksheets TYPE ole2_object .
    DATA mo_sheet TYPE ole2_object .
    DATA mo_cells TYPE ole2_object .
    DATA mo_to_cells TYPE ole2_object .
    DATA mo_font TYPE ole2_object .
    DATA mo_interior TYPE ole2_object .
    DATA mo_range TYPE ole2_object .
    DATA mo_borders TYPE ole2_object .
    DATA mo_columns TYPE ole2_object .
    DATA mv_row TYPE i .
    DATA mv_column TYPE i .

    METHODS _init_excel
      IMPORTING
        !iv_menu_desc TYPE zif_flifm_definitions=>ty_flifm_menu_desc
      RAISING
        zcx_flifm_exception .
    METHODS _set_title_excel
      IMPORTING
        !iv_cell  TYPE i
        !iv_title TYPE string
      RAISING
        zcx_flifm_exception .
    METHODS _fill_cell
      IMPORTING
        !iv_row    TYPE i
        !iv_column TYPE i
        !iv_bold   TYPE i
        !iv_size   TYPE i
        !iv_color  TYPE char4 OPTIONAL
        !iv_value  TYPE string
      RAISING
        zcx_flifm_exception .
    METHODS _get_company
      RETURNING
        VALUE(rv_butxt) TYPE butxt
      RAISING
        zcx_flifm_exception .
    METHODS _set_header_excel
      IMPORTING
        !it_fieldcat TYPE lvc_t_fcat
      RAISING
        zcx_flifm_exception .
    METHODS _set_row_excel
      IMPORTING
        !it_table    TYPE STANDARD TABLE
        !it_fieldcat TYPE lvc_t_fcat
      RAISING
        zcx_flifm_exception .
    METHODS _set_format_excel
      RAISING
        zcx_flifm_exception .
    METHODS _clear_excel_object .
ENDCLASS.



CLASS ZCL_FLIFM_EXCEL IMPLEMENTATION.


  METHOD get_instance.


    IF go_excel IS NOT BOUND.
      CREATE OBJECT go_excel.
    ENDIF.

    ro_excel = go_excel.


  ENDMETHOD.


  METHOD zif_flifm_excel~download_fsv_list_excel.

    DATA: lv_cell TYPE i.
    DATA: lv_rc TYPE i.

    DATA: lv_menu_desc     TYPE zif_flifm_definitions=>ty_flifm_menu_desc,
          lv_sub_menu_desc TYPE zif_flifm_definitions=>ty_flifm_menu_desc.

    DATA: lt_data TYPE REF TO data.
    FIELD-SYMBOLS: <lt_table> TYPE STANDARD TABLE.

    zcl_flifm_progress=>show( iv_text = |Starting Excel...| ).

    CREATE DATA lt_data LIKE it_popup_table.
    ASSIGN lt_data->* TO <lt_table>.

    zcl_flifm_event_data_provider=>filtering_data( EXPORTING it_nkey  = it_nkey
                                                             it_table = it_popup_table
                                                   IMPORTING et_table = <lt_table> ).

    DATA: lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat LIKE LINE OF lt_fieldcat.

    lt_fieldcat = zcl_flifm_event_data_provider=>create_event_fieldcat( it_popup_table ).

    lv_menu_desc = zcl_flifm_i18n=>get_instance( )->get_menu_description( zcl_flifm_utils=>split_menu( iv_menu ) ).

    _init_excel( lv_menu_desc ).

    zcl_flifm_progress=>show( iv_text = |Fill cells from list...| ).

    ADD 1 TO mv_row.

    LOOP AT lt_fieldcat INTO ls_fieldcat WHERE no_out IS INITIAL.
      IF ls_fieldcat-fieldname CP 'AMT*'.
        IF ls_fieldcat-fieldname+3(2) <= zcl_flifm_selection=>get_to_period( ).
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      ADD 1 TO lv_cell.
    ENDLOOP.
    ADD 1 TO lv_cell.

    _set_title_excel( EXPORTING iv_cell  = lv_cell
                                iv_title = |{ sy-title }| ).

    ADD 2 TO mv_row.

    lv_sub_menu_desc = zcl_flifm_i18n=>get_instance( )->get_menu_description( iv_menu ).

    _set_title_excel( EXPORTING iv_cell  = lv_cell
                                iv_title = |{ lv_sub_menu_desc }| ).

    ADD 1 TO mv_row.
    ADD 1 TO mv_column.

    _set_header_excel( lt_fieldcat ).

    _set_row_excel( EXPORTING it_table = <lt_table>
                              it_fieldcat = lt_fieldcat ).

    _set_format_excel( ).

    _clear_excel_object( ).

  ENDMETHOD.


  METHOD _clear_excel_object.

    FREE OBJECT:
      mo_excel,
      mo_workbooks,
      mo_workbook,
      mo_worksheets,
      mo_cells,
      mo_to_cells,
      mo_font,
      mo_interior,
      mo_range,
      mo_borders,
      mo_columns.

    CLEAR: mv_row, mv_column.

  ENDMETHOD.


  METHOD _fill_cell.


    CALL METHOD OF
        mo_excel
        'CELLS'  = mo_cells
      EXPORTING
        #1       = iv_row
        #2       = iv_column.

    SET PROPERTY OF mo_cells 'VALUE' = iv_value.

    GET PROPERTY OF mo_cells 'FONT' = mo_font.

    SET PROPERTY OF mo_font 'SIZE' = iv_size.

    SET PROPERTY OF mo_font 'BOLD' = iv_bold.

    CASE iv_color.
      WHEN zif_flifm_definitions=>c_row_color-yellow.
        GET PROPERTY OF mo_cells 'Interior' = mo_interior.
        SET PROPERTY OF mo_interior 'ColorIndex' = 36.
      WHEN zif_flifm_definitions=>c_row_color-green.
        GET PROPERTY OF mo_cells 'Interior' = mo_interior.
        SET PROPERTY OF mo_interior 'ColorIndex' = 35.
      WHEN zif_flifm_definitions=>c_row_color-red.
        GET PROPERTY OF mo_cells 'Interior' = mo_interior.
        SET PROPERTY OF mo_interior 'ColorIndex' = 3.
      WHEN zif_flifm_definitions=>c_row_color-sky_blue.
        GET PROPERTY OF mo_cells 'Interior' = mo_interior.
        SET PROPERTY OF mo_interior 'ColorIndex' = 20.
    ENDCASE.


  ENDMETHOD.


  METHOD _get_company.


    DATA: lt_company TYPE zcl_flifm_fetch=>tyt_company,
          ls_company LIKE LINE OF lt_company,
          lv_lines   TYPE i.

*//Display company
    lt_company = zcl_flifm_fetch=>get_instance( )->get_company( ).

    lv_lines = lines( lt_company ).

    IF lv_lines EQ 1.
      LOOP AT lt_company INTO ls_company. ENDLOOP.
      rv_butxt = ls_company-butxt.
    ELSE.
      rv_butxt = zcl_flifm_i18n=>get_instance( )->flifm_title.
    ENDIF.


  ENDMETHOD.


  METHOD _init_excel.


*// Create OLE object for excel application.
    CREATE OBJECT mo_excel 'EXCEL.APPLICATION'.
    IF sy-subrc <> 0.
      _clear_excel_object( ).

      zcx_flifm_exception=>raise_msg( |Can't open Excel Application| ).
    ENDIF.

*// The excel sheet is not visible to the user while data transfer.
    SET PROPERTY OF mo_excel 'Visible' = 0.

*// Get the control
    CALL METHOD OF
      mo_excel
        'Workbooks' = mo_workbooks.

*// Create a new workbook
    CALL METHOD OF
      mo_workbooks
        'Add' = mo_workbook.

*// Create sheets
    CALL METHOD OF
      mo_excel
        'Worksheets' = mo_worksheets.

*// Set sheet name
    CALL METHOD OF
      mo_worksheets
        'Add' = mo_sheet.

    SET PROPERTY OF mo_sheet 'Name' = iv_menu_desc.


  ENDMETHOD.


  METHOD _set_format_excel.

    "Start table cell
    CALL METHOD OF
        mo_excel
        'Cells'  = mo_cells
      EXPORTING
        #1       = 4
        #2       = 2.

    "End table cell
    CALL METHOD OF
        mo_excel
        'Cells'  = mo_to_cells
      EXPORTING
        #1       = mv_row
        #2       = mv_column.

    CALL METHOD OF
        mo_excel
        'Range'  = mo_range
      EXPORTING
        #1       = mo_cells
        #2       = mo_to_cells.

    CALL METHOD OF
      mo_range
        'Borders' = mo_borders.

    SET PROPERTY OF mo_borders 'LINESTYLE' = 1.

*// Autofit
    CALL METHOD OF
      mo_excel
        'COLUMNS' = mo_columns.

    CALL METHOD OF
      mo_columns
      'AUTOFIT'.

*// Start excel
    SET PROPERTY OF mo_excel  'Visible' = 1.


  ENDMETHOD.


  METHOD _set_header_excel.

    DATA: ls_fieldcat LIKE LINE OF it_fieldcat.

    LOOP AT it_fieldcat INTO ls_fieldcat WHERE no_out IS INITIAL.
      IF ls_fieldcat-fieldname CP 'AMT*'.
        IF ls_fieldcat-fieldname+3(2) <= zcl_flifm_selection=>get_to_period( ).
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
      ADD 1 TO mv_column.
      _fill_cell( EXPORTING iv_row    = mv_row
                            iv_column = mv_column
                            iv_bold   = 1
                            iv_size   = 15
                            iv_value  = |{ ls_fieldcat-scrtext_l }| ).

    ENDLOOP.

  ENDMETHOD.


  METHOD _set_row_excel.

    DATA: lt_data TYPE REF TO data.

    DATA: ls_fieldcat LIKE LINE OF it_fieldcat.

    FIELD-SYMBOLS: <ls_table> TYPE any,
                   <lv_value> TYPE any,
                   <lv_color> TYPE any.

    CREATE DATA lt_data LIKE it_table.
    ASSIGN lt_data->* TO <ls_table>.

    LOOP AT it_table ASSIGNING <ls_table>.

      ADD 1 TO mv_row.
      CLEAR mv_column.
      ADD 2 TO mv_column.

      ASSIGN COMPONENT 'ROW_COLOR' OF STRUCTURE <ls_table> TO <lv_color>.

      ASSIGN COMPONENT 'TLEVEL' OF STRUCTURE <ls_table> TO <lv_value>.
      _fill_cell( EXPORTING iv_row    = mv_row
                            iv_column = mv_column
                            iv_bold   = 1
                            iv_size   = 10
                            iv_color  = <lv_color>
                            iv_value  = |{ <lv_value> }| ).

      ADD 1 TO mv_column.
      ASSIGN COMPONENT 'TEXT' OF STRUCTURE <ls_table> TO <lv_value>.
      _fill_cell( EXPORTING iv_row    = mv_row
                            iv_column = mv_column
                            iv_bold   = 1
                            iv_size   = 10
                            iv_color  = <lv_color>
                            iv_value  = |{ <lv_value> }| ).

      ADD 1 TO mv_column.
      ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_table> TO <lv_value>.
      _fill_cell( EXPORTING iv_row    = mv_row
                            iv_column = mv_column
                            iv_bold   = 1
                            iv_size   = 10
                            iv_color  = <lv_color>
                            iv_value  = |{ <lv_value> }| ).

      ADD 1 TO mv_column.
      ASSIGN COMPONENT 'TXT50' OF STRUCTURE <ls_table> TO <lv_value>.
      _fill_cell( EXPORTING iv_row    = mv_row
                            iv_column = mv_column
                            iv_bold   = 1
                            iv_size   = 10
                            iv_color  = <lv_color>
                            iv_value  = |{ <lv_value> }| ).

      LOOP AT it_fieldcat INTO ls_fieldcat WHERE fieldname CP '*AMT*' OR fieldname CP '*PER'.
        IF ls_fieldcat-fieldname CP 'AMT*'.
          IF ls_fieldcat-fieldname+3(2) <= zcl_flifm_selection=>get_to_period( ).
          ELSE.
            CONTINUE.
          ENDIF.
        ENDIF.
        ADD 1 TO mv_column.
        ASSIGN COMPONENT ls_fieldcat-fieldname OF STRUCTURE <ls_table> TO <lv_value>.
        _fill_cell( EXPORTING iv_row  = mv_row
                              iv_column = mv_column
                              iv_bold   = 1
                              iv_size   = 10
                              iv_color  = <lv_color>
                              iv_value  = |{ <lv_value> }| ).
      ENDLOOP.

      ADD 1 TO mv_column.
      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_table> TO <lv_value>.
      _fill_cell( EXPORTING iv_row = mv_row
                            iv_column = mv_column
                            iv_bold   = 1
                            iv_size   = 10
                            iv_color  = <lv_color>
                            iv_value  = |{ <lv_value> }| ).

    ENDLOOP.


  ENDMETHOD.


  METHOD _set_title_excel.

    _fill_cell( EXPORTING iv_row    = mv_row
                          iv_column = 2
                          iv_bold   = 1
                          iv_size   = 17
                          iv_value  = iv_title ).

    CALL METHOD OF
        mo_excel
        'CELLS'  = mo_to_cells
      EXPORTING
        #1       = mv_row
        #2       = iv_cell.

    CALL METHOD OF
        mo_excel
        'Range'  = mo_range
      EXPORTING
        #1       = mo_cells
        #2       = mo_to_cells.

    CALL METHOD OF
      mo_range
      'SELECT'.

    CALL METHOD OF
      mo_range
      'MERGE'.

  ENDMETHOD.
ENDCLASS.
