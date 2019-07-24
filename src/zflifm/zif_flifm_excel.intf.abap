interface ZIF_FLIFM_EXCEL
  public .


  methods DOWNLOAD_FSV_LIST_EXCEL
    importing
      !IV_MENU type ZIF_FLIFM_DEFINITIONS=>TY_FLIFM_MENU_TYPE
      !IT_NKEY type LVC_T_NKEY
      !IT_POPUP_TABLE type STANDARD TABLE
    raising
      ZCX_FLIFM_EXCEPTION .
endinterface.
