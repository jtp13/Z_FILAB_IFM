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
class ZCL_FLIFM_I18N definition
  public
  create public .

public section.

  data FLIFM_TITLE type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data BLOCK1 type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data BLOCK2 type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data BLOCK3 type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data REPORTING_YEAR type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data REPORTING_PERIOD type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data COMPANY_CODE type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data COMPARISON_YEAR type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data TREE_DISPLAY_LEVEL type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data NOT_ASSIGNED type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data CALC_NET_PROFIT type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data CALC_NET_PROFIT_YTD type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data EXPAND_ALL type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data COLLAPSE_ALL type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data FSV_POPUP type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data FSV_EXCEL type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data ITEM_ACCOUNT type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data BALANCE_CF type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data YTD type ZIF_FLIFM_DEFINITIONS=>TY_I18N .
  data:
    BEGIN OF fsv_tot_i18n,
        tb TYPE seu_text VALUE 'TB(Total)',
        pl TYPE seu_text VALUE 'PL(Total)',
        bs TYPE seu_text VALUE 'BS(Total)',
      END OF fsv_tot_i18n .

  class-methods GET_INSTANCE
    returning
      value(RO_TEXT) type ref to ZCL_FLIFM_I18N .
  methods CONSTRUCTOR .
  methods GET_MENU_DESCRIPTION
    importing
      !IV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE
    returning
      value(RV_DESCR) type TEXT30 .
  methods GET_INITIAL_MENU_DESC
    returning
      value(RS_DESC) type ZIF_FLIFM_DEFINITIONS=>TYS_FLIFM_MENU_DESC_I18N .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF tys_flifm_menu_desc,
             menu_type TYPE zif_flifm_definitions=>ty_flifm_menu_type,
             descr     TYPE text30,
           END OF tys_flifm_menu_desc .
  types:
    tyt_flifm_menu_desc TYPE HASHED TABLE OF tys_flifm_menu_desc WITH UNIQUE KEY menu_type .

  class-data GO_FLIFM_TEXT type ref to ZCL_FLIFM_I18N .
  data MT_FLIFM_MENU_DESC type TYT_FLIFM_MENU_DESC .
ENDCLASS.



CLASS ZCL_FLIFM_I18N IMPLEMENTATION.


  METHOD constructor.

    flifm_title         = 'FI LAB. IFM Report'(t01).
    block1              = 'Select Option'(t02).
    block2              = 'Comparison Option'(t03).
    block3              = 'Etc Option'(t04).
    reporting_year      = 'Reporting Year'(t05).
    reporting_period    = 'Reporting Period'(t06).
    company_code        = 'Company Code'(t07).
    comparison_year     = 'Comparison Year'(t08).
    tree_display_level  = 'Tree Display Level'(t09).
    not_assigned        = 'Not Assigned'(t10).
    calc_net_profit     = 'Calculated Net Profit'(t11).
    calc_net_profit_ytd = 'Calculated Net Profit(Cumulation)'(t12).
    expand_all          = 'Expand All'(t13).
    collapse_all        = 'Collapse All'(t14).
    fsv_popup           = 'Drag & Copy'(t15).
    fsv_excel           = 'Download Excel'(t16).
    item_account        = 'Item/Account'(t17).
    balance_cf          = 'Balance Carryforward'(t18).
    ytd                 = 'YTD'(t19).

    fsv_tot_i18n-tb = 'TB(Total)'(t20).
    fsv_tot_i18n-pl = 'PL(Total)'(t21).
    fsv_tot_i18n-bs = 'BS(Total)'(t22).

**// Menu Description
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_flifm_menu_desc
*      FROM zflifmt_mtypet
*      WHERE spras = sy-langu.
*
*    IF sy-subrc <> 0.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_flifm_menu_desc
*        FROM zflifmt_mtypet
*        WHERE spras = zif_flifm_definitions=>c_default_langu.
*    ENDIF.

  ENDMETHOD.


  METHOD get_initial_menu_desc.

    rs_desc-tb          = 'Trial Balance'(d01).
    rs_desc-tb_ttb      = 'Total Trial Balance'(d02).
    rs_desc-tb_trend    = 'Trial Balance Trend'(d03).
    rs_desc-tb_rptb     = 'T/B of Reporting Period'(d04).
    rs_desc-pl          = 'Profit and Loss'(d05).
    rs_desc-pl_rp_cysp  = 'Rpt Period vs CY Same Period'(d06).
    rs_desc-pl_try_tcy  = 'Tot of RY vs Tot of CY'(d07).
    rs_desc-pl_ry_trend = 'Reporting Year Monthly Trend'(d08).
    rs_desc-pl_cy_trend = 'Comparison Year Monthly Trend'(d09).
    rs_desc-bs          = 'Balance Sheet'(d10).
    rs_desc-bs_cy_rp_ry = 'Tot of CY vs RP vs Tot of RY'(d11).
    rs_desc-bs_try_tcy  = 'Tot of RY vs Tot of CY'(d12).
    rs_desc-bs_rp_cysp  = 'Rpt Period vs CY Same Period'(d13).
    rs_desc-bs_ry_trend = 'Reporting Year Monthly Trend'(d14).
    rs_desc-bs_cy_trend = 'Comparison Year Monthly Trend'(d15).

  ENDMETHOD.


  METHOD get_instance.


    IF go_flifm_text IS NOT BOUND.
      CREATE OBJECT go_flifm_text.
    ENDIF.

    ro_text = go_flifm_text.


  ENDMETHOD.


  METHOD get_menu_description.

    DATA ls_flifm_menu_desc LIKE LINE OF mt_flifm_menu_desc.

    IF mt_flifm_menu_desc IS INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_flifm_menu_desc
        FROM zflifmt_mtypet
        WHERE spras = sy-langu.

      IF sy-subrc <> 0.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_flifm_menu_desc
          FROM zflifmt_mtypet
          WHERE spras = zif_flifm_definitions=>c_default_langu.
      ENDIF.
    ENDIF.

    READ TABLE mt_flifm_menu_desc INTO ls_flifm_menu_desc WITH TABLE KEY menu_type = iv_menu.

    IF sy-subrc = 0.
      rv_descr = ls_flifm_menu_desc-descr.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
