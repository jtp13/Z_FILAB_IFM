INTERFACE zif_flifm_definitions
  PUBLIC .


  TYPES ty_i18n TYPE itex132 .
  TYPES ty_amt_calc TYPE zflifme_amt_calc .
  TYPES:
    ty_per_calc    TYPE p LENGTH 10 DECIMALS 2 .
  TYPES:
    ty_amt_display TYPE c LENGTH 25 .
  TYPES:
    ty_per_display TYPE c LENGTH 15 .
  TYPES:
    BEGIN OF tys_skat,
      saknr TYPE saknr,
      txt50 TYPE txt50,
    END OF tys_skat .
  TYPES:
    tyt_skat TYPE HASHED TABLE OF tys_skat WITH UNIQUE KEY saknr .
  TYPES:
    tyt_dfies TYPE STANDARD TABLE OF dfies WITH DEFAULT KEY .
  TYPES:
    tyt_t030 TYPE STANDARD TABLE OF t030 WITH DEFAULT KEY .
  TYPES ty_flifm_menu_type TYPE zflifme_menu_type.
  TYPES ty_flifm_company_type TYPE tv_nodekey .
  TYPES ty_flifm_menu_desc TYPE text30 .
  TYPES:
    BEGIN OF tys_flifm_menu_desc_i18n,
      tb          TYPE ty_flifm_menu_desc,
      tb_ttb      TYPE ty_flifm_menu_desc,
      tb_trend    TYPE ty_flifm_menu_desc,
      tb_rptb     TYPE ty_flifm_menu_desc,
      pl          TYPE ty_flifm_menu_desc,
      pl_rp_cysp  TYPE ty_flifm_menu_desc,
      pl_try_tcy  TYPE ty_flifm_menu_desc,
      pl_ry_trend TYPE ty_flifm_menu_desc,
      pl_cy_trend TYPE ty_flifm_menu_desc,
      bs          TYPE ty_flifm_menu_desc,
      bs_cy_rp_ry TYPE ty_flifm_menu_desc,
      bs_try_tcy  TYPE ty_flifm_menu_desc,
      bs_rp_cysp  TYPE ty_flifm_menu_desc,
      bs_ry_trend TYPE ty_flifm_menu_desc,
      bs_cy_trend TYPE ty_flifm_menu_desc,
    END OF tys_flifm_menu_desc_i18n .
  TYPES:
    BEGIN OF tys_layout,
      item    TYPE tv_nodekey,
      name    TYPE seu_name,
      formula TYPE c LENGTH 50,
    END OF tys_layout .
  TYPES:
    tyt_layout TYPE STANDARD TABLE OF tys_layout WITH DEFAULT KEY .
  TYPES ty_saved_layout TYPE zflifmt_layout .
  TYPES:
    tyt_saved_layout TYPE STANDARD TABLE OF ty_saved_layout WITH DEFAULT KEY .
  TYPES:
    tyt_node TYPE STANDARD TABLE OF mtreesnode WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF tys_node_tab,
      id     TYPE snodetext-id,
      type   TYPE snodetext-type,
      name   TYPE snodetext-name,
      tlevel TYPE snodetext-tlevel,
      parent TYPE snodetext-parent,
      child  TYPE snodetext-child,
      text   TYPE snodetext-text,
      nkey   TYPE lvc_nkey, "Used when creating tree
      p_id   TYPE snodetext-id, "Used for ration calculation
    END OF tys_node_tab .
  TYPES:
    tyt_node_tab TYPE STANDARD TABLE OF tys_node_tab WITH DEFAULT KEY .
  TYPES:
    BEGIN OF tys_gl_tot,
      gjahr  TYPE gjahr,
      monat  TYPE monat,
      bukrs  TYPE bukrs,
      waers  TYPE waers,
      hkont  TYPE hkont,
      komok  TYPE komok,
      konts  TYPE saknr,
      obamt  TYPE zflifme_obamt,
      mbamt  TYPE zflifme_mbamt,
      mcbamt TYPE zflifme_mcbamt,
      ycbamt TYPE zflifme_ycbamt,
      dobamt TYPE zflifme_dobamt,
      dbamt  TYPE zflifme_dbamt,
      dtamt  TYPE zflifme_dtamt,
      cobamt TYPE zflifme_cobamt,
      cbamt  TYPE zflifme_cbamt,
      ctamt  TYPE zflifme_ctamt,
    END OF tys_gl_tot .
  TYPES ty_action TYPE sy-ucomm .
  TYPES ty_route TYPE zflifme_route .
  TYPES:
    BEGIN OF tys_route_data,
      route   TYPE ty_route,
      menu    TYPE ty_flifm_menu_type,
      company TYPE ty_flifm_company_type,
      action  TYPE ty_action,
      parent  TYPE REF TO cl_gui_container, "cl_gui_splitter_container,
    END OF tys_route_data .
  TYPES ty_row_color TYPE char4 .

  CONSTANTS c_default_langu TYPE spras VALUE 'E'.
  CONSTANTS c_slash TYPE c VALUE '/' ##NO_TEXT.
  CONSTANTS c_hyphen TYPE c VALUE '-' ##NO_TEXT.
  CONSTANTS c_underscore TYPE c VALUE '_'.
  CONSTANTS:
    c_arrow  TYPE c LENGTH 2 VALUE '->' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_flifm_menu,
      m000001  TYPE ty_flifm_menu_type VALUE 'TB',
      m000002  TYPE ty_flifm_menu_type VALUE 'TB_TTB',
      m000003  TYPE ty_flifm_menu_type VALUE 'TB_TREND',
      m000004  TYPE ty_flifm_menu_type VALUE 'TB_RPTB',
      m000005  TYPE ty_flifm_menu_type VALUE 'PL',
      m000006  TYPE ty_flifm_menu_type VALUE 'PL_RP_CYSP',
      m000007  TYPE ty_flifm_menu_type VALUE 'PL_TRY_TCY',
      m000008  TYPE ty_flifm_menu_type VALUE 'PL_RY_TREND',
      m000009  TYPE ty_flifm_menu_type VALUE 'PL_CY_TREND',
      m000010 TYPE ty_flifm_menu_type VALUE 'BS',
      m000011 TYPE ty_flifm_menu_type VALUE 'BS_CY_RP_RY',
      m000012 TYPE ty_flifm_menu_type VALUE 'BS_TRY_TCY',
      m000013 TYPE ty_flifm_menu_type VALUE 'BS_RP_CYSP',
      m000014 TYPE ty_flifm_menu_type VALUE 'BS_RY_TREND',
      m000015 TYPE ty_flifm_menu_type VALUE 'BS_CY_TREND',
    END OF c_flifm_menu .
  CONSTANTS:
    BEGIN OF c_flifm_menu_type,
      tb          TYPE ty_flifm_menu_type VALUE 'TB',
      tb_ttb      TYPE ty_flifm_menu_type VALUE 'TB_TTB',
      tb_trend    TYPE ty_flifm_menu_type VALUE 'TB_TREND',
      tb_rptb     TYPE ty_flifm_menu_type VALUE 'TB_RPTB',
      pl          TYPE ty_flifm_menu_type VALUE 'PL',
      pl_rp_cysp  TYPE ty_flifm_menu_type VALUE 'PL_RP_CYSP',
      pl_try_tcy  TYPE ty_flifm_menu_type VALUE 'PL_TRY_TCY',
      pl_ry_trend TYPE ty_flifm_menu_type VALUE 'PL_RY_TREND',
      pl_cy_trend TYPE ty_flifm_menu_type VALUE 'PL_CY_TREND',
      bs          TYPE ty_flifm_menu_type VALUE 'BS',
      bs_cy_rp_ry TYPE ty_flifm_menu_type VALUE 'BS_CY_RP_RY',
      bs_try_tcy  TYPE ty_flifm_menu_type VALUE 'BS_TRY_TCY',
      bs_rp_cysp  TYPE ty_flifm_menu_type VALUE 'BS_RP_CYSP',
      bs_ry_trend TYPE ty_flifm_menu_type VALUE 'BS_RY_TREND',
      bs_cy_trend TYPE ty_flifm_menu_type VALUE 'BS_CY_TREND',
    END OF c_flifm_menu_type .
  CONSTANTS:
    BEGIN OF c_menu_routes,
      tb  TYPE ty_route VALUE 'gui_fsv_alv_tree',
      pl  TYPE ty_route VALUE 'gui_fsv_alv_tree',
      bs  TYPE ty_route VALUE 'gui_fsv_alv_tree',
    END OF c_menu_routes.
  CONSTANTS:
    BEGIN OF c_node_type,
      root        TYPE seu_type VALUE 'R',
      position    TYPE seu_type VALUE 'P',
      acct_interv TYPE seu_type VALUE 'A',
      gl          TYPE seu_type VALUE 'G',
    END OF c_node_type .
  CONSTANTS:
    BEGIN OF c_add_line_type,
      tot          TYPE seu_name VALUE 'TOT', "Total
      np_tot       TYPE seu_name VALUE 'NP_TOT', "Net Profit Total
      np_tot_mc    TYPE seu_name VALUE 'NP_TOT_MC', "For monthly cumulation, Net Profit Total
      not_assigned TYPE seu_name VALUE 'NA', "Not Assigned
    END OF c_add_line_type .
  CONSTANTS c_tree_menu_icon TYPE iconname VALUE icon_wd_tree ##NO_TEXT.
  CONSTANTS c_tree_menu_icon_display TYPE iconname VALUE icon_system_mark ##NO_TEXT.
  CONSTANTS c_tree_company_icon TYPE iconname VALUE icon_company_code ##NO_TEXT.
  CONSTANTS c_tree_company_icon_display TYPE iconname VALUE icon_system_mark ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_action,
      ic                 TYPE ty_action VALUE 'IC', "Integrated Currency
      lc                 TYPE ty_action VALUE 'LC', "Local Currency
      go_filab           TYPE ty_action VALUE 'GO_FILAB',
      go_help            TYPE ty_action VALUE 'GO_HELP',
      expd_all           TYPE ty_action VALUE 'EXPD_ALL', "Expand All
      cole_all           TYPE ty_action VALUE 'COLE_ALL', "Collapse All
      fsv_list_popup     TYPE ty_action VALUE 'FSV_LIST_POPUP',
      fsv_download_excel TYPE ty_action VALUE 'FSV_DOWNLOAD_EXCEL',
    END OF c_action .
  CONSTANTS:
    BEGIN OF c_ui,
      gui TYPE string VALUE 'gui',
      ui5 TYPE string VALUE 'ui5',
    END OF c_ui .
  CONSTANTS c_services TYPE string VALUE 'services' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF c_routes,
      gui_fsv_alv_tree TYPE ty_route VALUE 'gui_fsv_alv_tree',
      filab_services   TYPE ty_route VALUE 'filab_services',
    END OF c_routes .
  CONSTANTS:
    BEGIN OF c_routes_desc,
      gui_fsv_alv_tree TYPE text256 VALUE 'IFM Financial Statement ALV Tree',
      filab_services   TYPE text256 VALUE 'FI LAB. Service',
    END OF c_routes_desc.
  CONSTANTS:
    BEGIN OF c_row_color,
      sky_blue TYPE ty_row_color VALUE 'C100',
      yellow   TYPE ty_row_color VALUE 'C300',
      green    TYPE ty_row_color VALUE 'C500',
      red      TYPE ty_row_color VALUE 'C610',
    END OF c_row_color .
ENDINTERFACE.
